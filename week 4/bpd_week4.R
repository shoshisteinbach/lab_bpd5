###############################################################################
#  מעבדה בסטטיסטיקה - קבוצת BPD5 - שבוע 4
#  התאמת עקומות גדילה ל-BPD על נתוני האימון (train.csv משבוע 3):
#  fractional polynomials לתוחלת ולסטיית התקן, בהנחת התפלגות נורמלית.
#
#  קובץ אחד לכל העבודה. כל משימה יושבת בסעיף משלה, כדי שאפשר יהיה לעבוד
#  עליה בזמנים שונים בגיט בלי להתנגש:
#    0. הגדרות
#    1. הנתונים
#    2. מנוע ה-fractional polynomials (משותף, מוכן)
#    3. מודלים קיימים וממשק משותף של מודל (מוכן)
#    4. משימה 1 - מודל התוחלת
#    5. משימה 2 - מודל סטיית התקן
#       (בסוף הסעיף מורכב new_model מהמודלים של משימות 1 ו-2)
#    6. משימה 3 - גרף האחוזונים
#    7. משימה 4 - הצלחת המודל מול המודלים הקיימים
#
#  איך המשימות לא תלויות זו בזו:
#    - new_model מוגדר מראש כמודל זמני (סעיף 3), ולכן משימות 3 ו-4 רצות גם
#      לפני שמשימות 1 ו-2 הסתיימו. כשהן מסתיימות, new_model מוחלף במודל הסופי
#      ומספיק להריץ מחדש את הקובץ.
#    - משימה 1 מציבה את mean_fit, משימה 2 מציבה את sd_fit. אין צורך לגעת
#      בסעיפים של אחרות.
#
#  להריץ מתיקיית השורש של הפרויקט. R בסיסי בלבד.
###############################################################################

## ======================== 0. הגדרות =========================================

TRAIN_PATH <- file.path("week 3", "train.csv")
TEST_PATH  <- file.path("week 3", "test.csv")
OUT_DIR    <- file.path("week 4", "output")

POWERS      <- c(-2, -1, -0.5, 0, 0.5, 1, 2, 3)   # החזקה 0 = LOG
MAX_DEGREE  <- 2                                  # FP1 ו-FP2
PERCENTILES <- c(0.03, 0.05, 0.10, 0.95, 0.975)
K_FOLDS     <- 5

set.seed(2026)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

save_table <- function(df, name) {
  write.csv(df, file.path(OUT_DIR, paste0(name, ".csv")), row.names = FALSE)
}

# גרפים: אותו עיצוב כמו בשבוע 3 (10x6 אינץ', כותרת מודגשת, צבעים מתאימים גם
# לעיוורי צבעים). אותם גוונים כמו בגרפים של המצגת, ולכל צבע משמעות קבועה:
# כתום = המודל החדש או המודל שנבחר, כחול = Hadlock ומודל קיים,
# ירוק = Kurmanavicius, אפור-כחלחל = חלופה שנשקלה, אפור = הנתונים.
COL_NEW  <- "#C2510F"
COL_OLD  <- "#0B6AA8"
COL_OLD2 <- "#009E73"
COL_ALT  <- "#4F606C"
COL_BULK <- "grey30"
alpha_col <- function(col, a) adjustcolor(col, alpha.f = a)

# עברית בגרפים: מכשיר הגרפים של R לא מסדר טקסט מימין לשמאל, ולכן heb() הופכת
# את המילים והאותיות לסדר החזותי. מספרים ושמות באנגלית (BPD) נשארים כמו שהם.
# לעטוף בה כל טקסט עברי בגרף. ביטוי באנגלית בתוך משפט עברי (למשל p < 0.001)
# לכתוב עם רווח קשיח ( ) בין האיברים.
heb <- function(x) {
  fix_word <- function(w) {
    if (!grepl("[א-ת]", w)) return(w)
    runs <- regmatches(w, gregexpr("[A-Za-z0-9.%|]+|[^A-Za-z0-9.%|]+", w))[[1]]
    is_ltr <- grepl("^[A-Za-z0-9.%|]", runs)
    runs[!is_ltr] <- vapply(runs[!is_ltr], function(r)
      chartr("()[]", ")(][", paste(rev(strsplit(r, "")[[1]]), collapse = "")), "")
    paste(rev(runs), collapse = "")
  }
  one <- function(s) paste(rev(vapply(strsplit(s, " ", fixed = TRUE)[[1]], fix_word, "")),
                           collapse = " ")
  vapply(x, one, "", USE.NAMES = FALSE)
}

# pointsize: גודל הגופן. הגרפים שנכנסים למצגת מוקטנים בה לכ-7.6 אינץ', ולכן מקבלים
# PLOT_PT_SLIDE, כדי שהטקסט בהם ייראה בגודל הטקסט בשאר הגרפים במצגת
PLOT_PT_SLIDE <- 16
save_plot <- function(name, draw, width = 10, height = 6, pointsize = 12) {
  png(file.path(OUT_DIR, paste0(name, ".png")), width = width, height = height,
      units = "in", res = 150, pointsize = pointsize)
  tryCatch(draw(), finally = dev.off())
  invisible(NULL)
}

# עזרים לגרפים לפי שבוע: תווית כל שבועיים, שנתה קטנה כל שבוע, וקווי רקע עדינים
week_axis <- function() {
  axis(1, at = seq(14, 40, 2))
  axis(1, at = 14:40, labels = FALSE, tcl = -0.25)
}
soft_grid <- function(y_at) abline(v = seq(14, 40, 2), h = y_at, col = "grey92")

## ======================== 1. הנתונים =========================================
# קובץ האימון והבדיקה משבוע 3 (אחרי תיקון יחידות, ניקוי חריגים והסרת מדידות
# סמוכות). המדד: bpd במ"מ. גיל ההריון: pregweek בשבועות עם שברים.
# ההתאמה נעשית על קובץ האימון בלבד. קובץ הבדיקה לבדיקה משלימה.
# preg_id מזהה הריון: לאותו הריון כמה מדידות תלויות, ולכן בחלוקות לקפלים
# (cross-validation) מחלקים לפי preg_id ולא לפי מדידה.

train <- read.csv(TRAIN_PATH, stringsAsFactors = FALSE)
test  <- read.csv(TEST_PATH,  stringsAsFactors = FALSE)
cat("Train:", nrow(train), "scans |", length(unique(train$preg_id)), "pregnancies\n")
cat("Test: ", nrow(test),  "scans |", length(unique(test$preg_id)),  "pregnancies\n")

## ======================== 2. מנוע fractional polynomials (משותף) =============
# FP ממעלה 1: x^p. FP ממעלה 2: x^p1 + x^p2, ואם p1 = p2 אז x^p ו-x^p * log(x).
# החזקה 0 היא log(x). x מחולק ב-10 (שבועות / 10) לייצוב מספרי. העקומות לא משתנות.
# האלגוריתם: כל צירוף חזקות הוא מודל רגרסיה ליניארית רגילה (OLS).

fp_basis <- function(x, powers) {
  if (length(powers) == 0) return(NULL)
  z <- x / 10
  powers <- sort(powers)
  cols <- vector("list", length(powers))
  for (i in seq_along(powers)) {
    cols[[i]] <- if (i > 1 && powers[i] == powers[i - 1]) cols[[i - 1]] * log(z)
                 else if (powers[i] == 0) log(z) else z^powers[i]
  }
  do.call(cbind, cols)
}

# התאמת מודל אחד. מחזירה את המקדמים, קריטריונים (AIC, BIC בהנחת נורמליות),
# ופונקציה predict(xnew) שמחשבת את הערך המותאם לגיל הריון חדש.
fit_fp <- function(y, x, powers) {
  powers <- sort(powers)
  X <- cbind(rep(1, length(x)), fp_basis(x, powers))
  fit <- lm.fit(X, y)
  n <- length(y); k <- ncol(X); rss <- sum(fit$residuals^2)
  loglik <- -n / 2 * (log(2 * pi) + log(rss / n) + 1)
  list(powers = powers, coef = fit$coefficients, n = n, k = k, rss = rss,
       aic = -2 * loglik + 2 * (k + 1), bic = -2 * loglik + log(n) * (k + 1),
       predict = function(xnew) as.vector(cbind(rep(1, length(xnew)), fp_basis(xnew, powers)) %*% fit$coefficients))
}

fp_label <- function(powers) {
  if (length(powers) == 0) return("constant")
  paste0("(", paste(sort(powers), collapse = ", "), ")")
}

# כל צירופי החזקות עד MAX_DEGREE (עם חזרות). עם 8 חזקות: 8 + 36 = 44 מודלים.
fp_candidates <- function(powers = POWERS, max_degree = MAX_DEGREE) {
  out <- lapply(powers, function(p) p)
  if (max_degree >= 2) {
    g <- expand.grid(p1 = powers, p2 = powers)
    g <- g[g$p1 <= g$p2, ]
    out <- c(out, lapply(seq_len(nrow(g)), function(i) c(g$p1[i], g$p2[i])))
  }
  out
}

# טבלת כל המועמדים עם הקריטריונים (RSS, AIC, BIC) לתגובה y לפי גיל x.
fp_table <- function(y, x, cands = fp_candidates()) {
  do.call(rbind, lapply(cands, function(p) {
    f <- fit_fp(y, x, p)
    data.frame(model = fp_label(p), degree = length(p), params = f$k,
               rss = f$rss, aic = f$aic, bic = f$bic, stringsAsFactors = FALSE)
  }))
}

# תחזיות מחוץ למדגם (K-fold cross-validation) למודל אחד. הקפלים נבנים לפי
# קבוצה (groups = preg_id), כדי שמדידות של אותו הריון לא יהיו גם באימון וגם
# בבדיקה של אותו קפל. מחזירה וקטור של תחזית לכל מדידה, מקפל שלא כלל אותה.
cv_predict <- function(y, x, groups, powers, K = K_FOLDS, seed = 2026) {
  set.seed(seed)
  ug <- unique(groups)
  fold <- sample(rep(seq_len(K), length.out = length(ug)))[match(groups, ug)]
  pred <- numeric(length(y))
  for (k in seq_len(K)) {
    tr <- fold != k
    pred[!tr] <- fit_fp(y[tr], x[tr], powers)$predict(x[!tr])
  }
  pred
}

## ======================== 3. מודלים קיימים וממשק משותף =======================
# מודל = שם + פונקציית תוחלת + פונקציית סטיית תקן, שתיהן של גיל הריון בשבועות.
# כל הסעיפים הבאים עובדים רק מול הממשק הזה, ולכן מודל קיים ומודל חדש
# מטופלים באותו אופן.
#   model$mean(w)               תוחלת BPD במ"מ
#   model$sd(w)                 סטיית תקן במ"מ
#   model_percentile(model, w, p)   האחוזון p (למשל 0.05)
#   model_z(model, w, y)        ציון תקן של מדידה

make_model <- function(name, mean_fun, sd_fun) list(name = name, mean = mean_fun, sd = sd_fun)
model_percentile <- function(model, w, p) model$mean(w) + qnorm(p) * model$sd(w)
model_z <- function(model, w, y) (y - model$mean(w)) / model$sd(w)

# הדלוק (Hadlock 1984): BPD בס"מ = -3.08 + 0.41*w - 0.000061*w^3, סטיית תקן 0.3 ס"מ.
had_model <- make_model("Hadlock",
  function(w) 10 * (-3.08 + 0.41 * w - 0.000061 * w^3),
  function(w) rep(3, length(w)))

# Kurmanavicius (1999): הנוסחאות מקובץ ההקדמה (Intro1.pdf), במ"מ.
kur_model <- make_model("Kurmanavicius",
  function(w) -28.04 + 4.179 * w - 0.0006414 * w^3,
  function(w) 1.648 + 0.06531 * w)

# מודל זמני, כדי שמשימות 3 ו-4 יוכלו לרוץ לפני שמשימות 1 ו-2 הסתיימו.
# תוחלת: FP2 עם חזקות (1, 2). סטיית תקן: רגרסיה של הערך המוחלט של השאריות
# על גיל (חזקה 1), כפול sqrt(pi/2): התוחלת של |נורמלי| היא sd * sqrt(2/pi).
# אל תשתמשו בו בתוצאות הסופיות: הוא מוחלף בסוף סעיף 5.
prov_mean <- fit_fp(train$bpd, train$pregweek, c(1, 2))
prov_sd   <- fit_fp(abs(train$bpd - prov_mean$predict(train$pregweek)), train$pregweek, 1)
new_model <- make_model("Provisional model (to be replaced)",
                        prov_mean$predict,
                        function(w) sqrt(pi / 2) * prov_sd$predict(w))

## ======================== 4. משימה 1 - מודל התוחלת ===========================
# מטרה: רגרסיה של BPD על גיל הריון עם fractional polynomials (חזקות POWERS,
# כולל LOG), בחירת המודל לפי קריטריון אחד, ומודל סופי לתוחלת.
# מה להגיש: איך בחרתם את המשתנים (הסבר מפורש), המודל הסופי (חזקות ומקדמים
# ומשוואה), טבלת המועמדים עם הקריטריון, ובדיקה שהשאריות סבירות.
# התוצאה: mean_fit (האובייקט שמחזירה fit_fp). משימה 2 משתמשת בו לחישוב שאריות.
#
# כלים מוכנים: fp_candidates(), fp_table(y, x), fit_fp(y, x, powers),
#              cv_predict(y, x, groups, powers).

# א. כל 44 המועמדים
cands <- fp_candidates()

# ב. קריטריון הבחירה: cross-validation, שגיאת חיזוי מחוץ למדגם (5 קפלים לפי
# הריון). אותם קפלים לכל המועמדים.
cv_rmse <- sapply(cands, function(p) {
  pred <- cv_predict(train$bpd, train$pregweek, train$preg_id, p)
  sqrt(mean((train$bpd - pred)^2))
})
mean_table <- data.frame(model = sapply(cands, fp_label), degree = lengths(cands),
                         cv_rmse = cv_rmse)

# ג. הבחירה: המועמד עם שגיאת החיזוי הנמוכה ביותר מחוץ למדגם
mean_powers <- cands[[which.min(mean_table$cv_rmse)]]
mean_table <- mean_table[order(mean_table$cv_rmse), ]
save_table(mean_table, "04_mean_candidates")
cat("\nTop mean models by CV RMSE:\n")
print(head(mean_table, 8), digits = 7)

# ד. המודל הסופי: החזקות שנבחרו, מותאמות על כל קובץ האימון
mean_fit <- fit_fp(train$bpd, train$pregweek, mean_powers)
mean_coef <- data.frame(term = c("intercept", paste0("(w/10)^", mean_fit$powers)),
                        coef = as.vector(mean_fit$coef))
save_table(mean_coef, "04_mean_final_model")
cat("\nFinal mean model:\n")
print(mean_coef, digits = 7)

# ה. בדיקת שאריות: הממוצע בכל שבוע צריך להיות קרוב ל-0
mean_resid <- train$bpd - mean_fit$predict(train$pregweek)
resid_by_week <- data.frame(week = sort(unique(train$week)),
                            n = as.vector(table(train$week)),
                            mean_resid = as.vector(tapply(mean_resid, train$week, mean)))
save_table(resid_by_week, "04_mean_residuals_by_week")
cat("\nMean residual by week:\n")
print(resid_by_week, digits = 3)

# גרף המודל הסופי: ענן המדידות, הממוצע בפועל בכל שבוע והעקומה שנבחרה
save_plot("04_mean_fit", function() {
  weeks <- seq(14, 40, 0.1)
  plot(train$pregweek, train$bpd, pch = 16, cex = 0.25, col = alpha_col(COL_BULK, 0.12),
       xlim = c(14, 40), ylim = c(10, 110), xaxt = "n", yaxt = "n",
       xlab = heb("שבוע הריון"), ylab = heb("BPD (מ\"מ)"),
       main = heb("מודל התוחלת הסופי מול הנתונים: חזקות (2,3)"),
       panel.first = soft_grid(seq(10, 110, 10)))
  week_axis()
  axis(2, at = seq(10, 110, 10), las = 1)
  points(tapply(train$pregweek, train$week, mean), tapply(train$bpd, train$week, mean),
         pch = 21, bg = "white", col = COL_BULK, cex = 1.1)
  lines(weeks, mean_fit$predict(weeks), col = COL_NEW, lwd = 3)
  legend("topleft", bty = "n", pch = c(16, 21, NA), lty = c(NA, NA, 1), lwd = c(NA, NA, 3),
         pt.bg = c(NA, "white", NA), col = c(alpha_col(COL_BULK, 0.5), COL_BULK, COL_NEW),
         legend = heb(c("מדידה בודדת", "ממוצע המדידות בשבוע", "העקומה שנבחרה")))
  text(40, 18, expression("BPD = 3.727 + 0.15329" ~ w^2 ~ "- 0.002448" ~ w^3), adj = 1, col = COL_NEW, font = 2)
}, pointsize = PLOT_PT_SLIDE)

save_plot("04_mean_residuals_by_week", function() {
  lim <- 1.25
  plot(resid_by_week$week, resid_by_week$mean_resid, type = "n", xlim = c(14, 40),
       ylim = c(-lim, lim), xaxt = "n", yaxt = "n",
       xlab = heb("שבוע הריון"), ylab = heb("ממוצע השאריות (מ\"מ)"),
       main = heb("העקומה רחוקה מהנתונים בעיקר בשבועות 14–25 ומתקרבת אליהם בהמשך"),
       panel.first = soft_grid(seq(-1.25, 1.25, 0.25)))
  week_axis()
  axis(2, at = seq(-1.25, 1.25, 0.25), las = 1)
  abline(h = 0, col = "grey40", lty = 2)
  lines(resid_by_week$week, resid_by_week$mean_resid, col = COL_NEW, lwd = 2)
  points(resid_by_week$week, resid_by_week$mean_resid, pch = 16, col = COL_NEW, cex = 1.1)
  marked <- resid_by_week[resid_by_week$week %in% c(14, 19, 23, 30, 33), ]
  text(marked$week, marked$mean_resid, sprintf("%+.2f", marked$mean_resid), font = 2,
       pos = ifelse(marked$mean_resid > 0, 3, 1))
  text(40, 1.12, heb("מעל האפס = המדידות גבוהות מהעקומה"), adj = 1, col = "grey30")
  text(40, -1.12, heb("מתחת לאפס = המדידות נמוכות מהעקומה"), adj = 1, col = "grey30")
})

## ======================== 5. משימה 2 - מודל סטיית התקן ======================
# מטרה: מודל לסטיית התקן של BPD כפונקציה של גיל הריון, גם הוא עם fractional
# polynomials. השיטה המקובלת: רגרסיה של |השארית| על גיל, וסטיית התקן היא
# sqrt(pi/2) כפול הערך המותאם. השאריות מגיעות מהמודל של משימה 1
# (עד שהוא מוכן משתמשים בתוחלת הזמנית).
# מה להגיש: איך בחרתם את המשתנים (אותו קריטריון של משימה 1, או נימוק לקריטריון
# אחר), המודל הסופי, ובדיקה שציוני התקן z = (y - תוחלת) / sd מתפלגים כ-N(0,1)
# (ממוצע 0, סטיית תקן 1, ללא תלות בגיל), כי האחוזונים נשענים על הנחה זו.
# התוצאה: sd_fit (האובייקט שמחזירה fit_fp, על |השארית|).

mean_fun_now <- function() if (!is.null(mean_fit)) mean_fit$predict else prov_mean$predict
resid_now <- train$bpd - mean_fun_now()(train$pregweek)

# א. המועמדים: סטיית תקן קבועה (ללא חזקות) ועוד 44 המודלים
sd_cands <- c(list(numeric(0)), cands)

# ב. אותה חלוקה לקבוצות לפי הריון כמו ב-cv_predict
set.seed(2026)
ug <- unique(train$preg_id)
fold <- sample(rep(seq_len(K_FOLDS), length.out = length(ug)))[match(train$preg_id, ug)]

# ג. קריטריון הבחירה: קנס האחוזונים מההרצאה, על הריונות מוסתרים. בכל סיבוב
# מתאימים תוחלת וסטיית תקן על 4 קבוצות, ובודקים על החמישית כמה מדידות
# נמצאות מתחת לכל עקומה. הקנס בכל אחוזון הוא המרחק מהאחוז הרצוי, מחולק
# ב-sqrt(p(1-p)). ממוצע על השבועות (ובהרצאה סכום: אותו דירוג) ועל האחוזונים.
cv_mu_sd <- function(sd_powers) {
  mu <- s <- numeric(nrow(train))
  for (k in seq_len(K_FOLDS)) {
    tr <- fold != k
    m <- fit_fp(train$bpd[tr], train$pregweek[tr], mean_powers)
    abs_res <- abs(train$bpd[tr] - m$predict(train$pregweek[tr]))
    sf <- fit_fp(abs_res, train$pregweek[tr], sd_powers)
    mu[!tr] <- m$predict(train$pregweek[!tr])
    s[!tr]  <- sqrt(pi / 2) * sf$predict(train$pregweek[!tr])
  }
  list(mu = mu, s = s)
}

cv_percentile_penalty <- function(sd_powers) {
  o <- cv_mu_sd(sd_powers)
  mean(sapply(PERCENTILES, function(p) {
    below <- train$bpd < o$mu + qnorm(p) * o$s
    mean(abs(tapply(below, train$week, mean) - p)) / sqrt(p * (1 - p))
  }))
}

sd_table <- data.frame(model = sapply(sd_cands, fp_label), degree = lengths(sd_cands),
                       penalty = sapply(sd_cands, cv_percentile_penalty))
sd_table$pct_above_best <- 100 * (sd_table$penalty / min(sd_table$penalty) - 1)
sd_table <- sd_table[order(sd_table$penalty), ]
save_table(sd_table, "05_sd_candidates")
cat("\nTop SD models by percentile penalty:\n")
print(head(sd_table, 8), digits = 4)
cat("\nConstant SD:\n")
print(sd_table[sd_table$model == "constant", ], digits = 4)

# ד. הבחירה: קו ישר. הציון שלו גבוה בכ-2% מהטוב ביותר, והוא הרבה יותר פשוט
# (הנימוקים ב-README, סעיף 6)
sd_powers <- 1

# ה. המודל הסופי: מותאם על כל קובץ האימון
sd_fit <- fit_fp(abs(resid_now), train$pregweek, sd_powers)
sd_coef <- data.frame(term = c("intercept", paste0("(w/10)^", sd_fit$powers)),
                      coef_sd = sqrt(pi / 2) * as.vector(sd_fit$coef))
save_table(sd_coef, "05_sd_final_model")
cat("\nFinal SD model (coefficients after multiplying by sqrt(pi/2)):\n")
print(sd_coef, digits = 7)

# ה2. האם יש התאמת יתר? אותם מדדים פעם על הנתונים שעליהם המודל הותאם, ופעם
# על הריונות מוסתרים (cross-validation). אם הם כמעט זהים, אין התאמת יתר.
cv_final <- cv_mu_sd(sd_powers)
mu_in <- mean_fit$predict(train$pregweek)
s_in  <- sqrt(pi / 2) * sd_fit$predict(train$pregweek)
cv_check <- data.frame(
  measure = c("mean_rmse_mm", paste0("pct_below_P", 100 * PERCENTILES)),
  fitted_data = c(sqrt(mean(resid_now^2)),
                  sapply(PERCENTILES, function(p) 100 * mean(train$bpd < mu_in + qnorm(p) * s_in))),
  hidden_pregnancies = c(mean_table$cv_rmse[1],
                         sapply(PERCENTILES, function(p) 100 * mean(train$bpd < cv_final$mu + qnorm(p) * cv_final$s))))
save_table(cv_check, "05_cv_check")
cat("\nIn-sample vs cross-validation:\n")
print(cv_check, digits = 5)

# ו. בדיקת ההנחה: ציוני Z צריכים להתפלג N(0,1), בלי תלות בשבוע
z_scores <- resid_now / (sqrt(pi / 2) * sd_fit$predict(train$pregweek))
z_summary <- data.frame(mean_z = mean(z_scores), sd_z = sd(z_scores),
                        pct_within_1645 = 100 * mean(abs(z_scores) < 1.645))
save_table(z_summary, "05_z_summary")
z_by_week <- data.frame(week = sort(unique(train$week)),
                        n = as.vector(table(train$week)),
                        mean_z = as.vector(tapply(z_scores, train$week, mean)),
                        sd_z = as.vector(tapply(z_scores, train$week, sd)),
                        pct_within_1645 = as.vector(tapply(abs(z_scores) < 1.645, train$week, mean)) * 100)
save_table(z_by_week, "05_z_by_week")
cat("\nZ scores overall:\n")
print(z_summary, digits = 4)
cat("\nZ scores by week:\n")
print(z_by_week, digits = 3)

# גרף המודל הסופי: סטיית התקן לפי שבוע, מול שתי החלופות שנשקלו
save_plot("05_sd_fit", function() {
  weeks <- seq(14, 40, 0.1)
  alt_33 <- fit_fp(abs(resid_now), train$pregweek, c(3, 3))
  const  <- sqrt(pi / 2) * mean(abs(resid_now))
  plot(tapply(train$pregweek, train$week, mean),
       tapply(sqrt(pi / 2) * abs(resid_now), train$week, mean),
       pch = 21, bg = "white", col = COL_BULK, cex = 1.1,
       xlim = c(14, 40), ylim = c(2.25, 4), xaxt = "n", yaxt = "n",
       xlab = heb("שבוע הריון"), ylab = heb("סטיית תקן (מ\"מ)"),
       main = heb("סטיית התקן גדלה עם ההריון, וקו ישר מתאר זאת היטב"),
       panel.first = soft_grid(seq(2.25, 4, 0.25)))
  week_axis()
  axis(2, at = seq(2.25, 4, 0.25), las = 1)
  abline(h = const, col = "grey50", lwd = 2, lty = 3)
  lines(weeks, sqrt(pi / 2) * alt_33$predict(weeks), col = COL_ALT, lwd = 2, lty = 2)
  lines(weeks, sqrt(pi / 2) * sd_fit$predict(weeks), col = COL_NEW, lwd = 3)
  ends <- c(14, 40)
  end_sd <- sqrt(pi / 2) * sd_fit$predict(ends)
  text(ends, end_sd, sprintf("%.2f", end_sd), col = COL_NEW, font = 2, pos = 3)
  legend("topleft", bty = "n", pch = c(21, NA, NA, NA), lty = c(NA, 1, 2, 3), lwd = c(NA, 3, 2, 2),
         pt.bg = c("white", NA, NA, NA), col = c(COL_BULK, COL_NEW, COL_ALT, "grey50"),
         legend = c(heb(c("אומדן לכל שבוע", "קו ישר, הנבחר")),
                    heb("החזקות (3,3)"), heb("סטיית תקן קבועה")))
}, pointsize = PLOT_PT_SLIDE)

# בדיקת ההנחה: אזור האחוזונים שלנו מסומן בקווים אנכיים
save_plot("05_z_qq", function() {
  qqnorm(z_scores, pch = 16, cex = 0.3, col = COL_BULK, las = 1,
         xlim = c(-4.5, 4.5), ylim = c(-6, 6), xaxt = "n", yaxt = "n",
         xlab = heb("ערכים צפויים בהתפלגות נורמלית"), ylab = heb("ציוני Z בפועל"),
         main = heb("ציוני Z נורמליים באזור האחוזונים, והזנבות כבדים מעט יותר"))
  axis(1, at = -4:4)
  axis(2, at = seq(-6, 6, 2), las = 1)
  abline(0, 1, col = COL_NEW, lwd = 2)
  abline(v = qnorm(c(0.03, 0.975)), col = "grey40", lty = 2)
  text(0, 5.3, heb("בין הקווים: האחוזונים 3 עד 97.5"), col = "grey30")
  legend("bottomright", bty = "n", pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2),
         col = c(COL_BULK, COL_NEW), legend = heb(c("ציוני Z בפועל", "התאמה מושלמת לנורמלית")))
}, pointsize = PLOT_PT_SLIDE)

# אותה בדיקה בנפרד לכל תקופה בהריון
save_plot("05_z_qq_by_period", function() {
  op <- par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3, 1))
  on.exit(par(op))
  periods <- list("14–20" = 14:20, "21–25" = 21:25, "26–32" = 26:32, "33–39" = 33:39)
  for (nm in names(periods)) {
    qqnorm(z_scores[train$week %in% periods[[nm]]], pch = 16, cex = 0.3, col = COL_BULK, las = 1,
           xlim = c(-4.5, 4.5), ylim = c(-6, 6),
           xlab = heb("ערכים צפויים"), ylab = heb("ציוני Z בפועל"),
           main = heb(paste("שבועות", nm)))
    abline(0, 1, col = COL_NEW, lwd = 2)
  }
})

save_plot("05_z_by_week", function() {
  plot(z_by_week$week, z_by_week$sd_z, type = "n", xlim = c(14, 40),
       ylim = c(-0.5, 1.5), xaxt = "n", yaxt = "n",
       xlab = heb("שבוע הריון"), ylab = heb("ציון Z"),
       main = heb("ציוני Z לפי שבוע: הממוצע גולש בשבועות 14–24 והפיזור גדול בשבועות 23–25"),
       panel.first = {
         soft_grid(seq(-0.5, 1.5, 0.25))
         rect(22.5, -1, 25.5, 2, col = "grey90", border = NA)
       })
  week_axis()
  axis(2, at = seq(-0.5, 1.5, 0.25), las = 1)
  abline(h = c(0, 1), col = "grey40", lty = 2)
  lines(z_by_week$week, z_by_week$sd_z, col = COL_ALT, lwd = 2)
  points(z_by_week$week, z_by_week$sd_z, pch = 15, col = COL_ALT, cex = 1.1)
  lines(z_by_week$week, z_by_week$mean_z, col = COL_NEW, lwd = 2)
  points(z_by_week$week, z_by_week$mean_z, pch = 16, col = COL_NEW, cex = 1.1)
  mean_marks <- z_by_week[z_by_week$week %in% c(14, 19, 23), ]
  text(mean_marks$week, mean_marks$mean_z, sprintf("%+.2f", mean_marks$mean_z), font = 2,
       pos = ifelse(mean_marks$mean_z > 0, 3, 1))
  sd_mark <- z_by_week[which.max(z_by_week$sd_z), ]
  text(sd_mark$week, sd_mark$sd_z, sprintf("%.2f", sd_mark$sd_z), font = 2, pos = 3)
  legend("topright", bty = "n", pch = c(15, 16), lwd = 2, col = c(COL_ALT, COL_NEW),
         legend = heb(c("סטיית התקן של Z – רצוי 1", "ממוצע Z – רצוי 0")))
})

# הבדיקה השלישית מהמאמר: כ-90% מהציונים צריכים להיות בין 1.645- ל-1.645
save_plot("05_z_within_by_week", function() {
  plot(z_by_week$week, z_by_week$pct_within_1645, type = "n", xlim = c(14, 40),
       ylim = c(80, 95), xaxt = "n", yaxt = "n",
       xlab = heb("שבוע הריון"), ylab = heb("אחוז הציונים בטווח"),
       main = heb("אחוז ציוני Z בטווח ±1.645 לפי שבוע, הרצוי הוא 90%"),
       panel.first = {
         soft_grid(seq(80, 95, 2.5))
         rect(22.5, 70, 25.5, 100, col = "grey90", border = NA)
       })
  week_axis()
  axis(2, at = seq(80, 95, 2.5), las = 1)
  abline(h = 90, col = "grey40", lty = 2)
  lines(z_by_week$week, z_by_week$pct_within_1645, col = COL_NEW, lwd = 2)
  points(z_by_week$week, z_by_week$pct_within_1645, pch = 16, col = COL_NEW, cex = 1.1)
  low <- z_by_week[which.min(z_by_week$pct_within_1645), ]
  text(low$week, low$pct_within_1645, sprintf("%.1f%%", low$pct_within_1645), font = 2, pos = 1)
  text(24, 94, heb("שבועות 23–25"), col = "grey30")
})

# הרכבת המודל החדש מהמודלים שנבחרו. אם אחד מהם עדיין לא מוכן, נשאר הזמני שלו.
if (!is.null(mean_fit) || !is.null(sd_fit)) {
  new_model <- make_model("Fractional polynomials",
    mean_fun_now(),
    if (!is.null(sd_fit)) function(w) sqrt(pi / 2) * sd_fit$predict(w)
    else function(w) sqrt(pi / 2) * prov_sd$predict(w))
}

## ======================== 6. משימה 3 - גרף האחוזונים =========================
# מטרה: בעזרת התוחלת וסטיית התקן, לחשב ולצייר גרף של האחוזונים 3, 5, 10, 95
# ו-97.5 כפונקציה של גיל ההריון.
# מה להגיש: גרף אחד (כותרת, צירים ומקרא בעברית, שנתות כל שבועיים, טווח שבועות
# 14-40) וטבלת ערכי האחוזונים לכל שבוע שלם (CSV). אפשר להוסיף את ענן הנתונים.
# כלים מוכנים: model_percentile(new_model, w, PERCENTILES).

percentile_weeks <- 14:40
percentile_labels <- paste0("P", sub("\\.0$", "", format(100 * PERCENTILES, trim = TRUE)))
percentile_values <- sapply(PERCENTILES, function(p)
  model_percentile(new_model, percentile_weeks, p))
percentile_table <- data.frame(week = percentile_weeks, percentile_values,
                               check.names = FALSE)
names(percentile_table)[-1] <- percentile_labels
save_table(percentile_table, "06_percentiles_by_week")

save_plot("06_percentile_curves", function() {
  w <- seq(14, 40, by = 0.1)
  curves <- sapply(PERCENTILES, function(p) model_percentile(new_model, w, p))
  # כל העקומות הן של המודל החדש (כתום). האחוזונים הנמוכים והגבוהים מובחנים
  # בסוג הקו: מלא לקיצוני (P3, P97.5), מקווקו ל-P5 ו-P95, מנוקד ל-P10.
  ltys <- c(1, 2, 3, 2, 1)
  plot(train$pregweek, train$bpd, pch = 16, cex = 0.22,
       col = alpha_col(COL_BULK, 0.12), xlim = c(14, 40), ylim = c(10, 110),
       xaxt = "n", yaxt = "n", xlab = heb("שבוע הריון"),
       ylab = heb("BPD (מ\"מ)"), main = heb("עקומות האחוזונים של המודל החדש"),
       panel.first = soft_grid(seq(10, 110, 10)))
  week_axis()
  axis(2, at = seq(10, 110, 10), las = 1)
  for (j in seq_along(PERCENTILES)) lines(w, curves[, j], col = COL_NEW, lwd = 2.5, lty = ltys[j])
  legend("topleft", bty = "n", lwd = 2.5, lty = rev(ltys), col = COL_NEW, seg.len = 3,
         legend = rev(percentile_labels))
}, pointsize = PLOT_PT_SLIDE)

## ======================== 7. משימה 4 - הצלחת המודל מול הקיימים ===============
# מטרה: לתאר את הצלחת המודל החדש לעומת המודלים הקיימים (had_model, kur_model)
# על קובץ האימון. הרעיון כמו בשבוע 2: אם העקומה מתאימה, אחוז המדידות
# שחורגות מכל אחוזון צריך להיות שווה לאחוזון הנקוב (3, 5, 10, 95, 97.5).
# מה להגיש: טבלה והשוואה (שיעור החריגות בפועל מול הנקוב, לכל מודל ולכל אחוזון,
# עם רווח סמך), גרף, ומסקנה איזה מודל מתאים יותר ובאיזה שבועות. כמו שנאמר
# בהרצאה, הבדיקה נעשית על קובץ האימון בלבד, וקובץ הבדיקה לא משמש כאן.
# כלים מוכנים: model_z(model, w, y), model_percentile(model, w, p).

models_compare <- list(new_model, had_model, kur_model)
model_names <- vapply(models_compare, `[[`, "", "name")
names(models_compare) <- model_names
weeks_compare <- sort(unique(train$week))
preg_ids <- unique(train$preg_id)
preg_index <- match(train$preg_id, preg_ids)
week_index <- match(train$week, weeks_compare)
n_preg <- length(preg_ids)
n_week <- length(weeks_compare)

# דגלים: האם המדידה נמצאת מתחת לאחוזון הנתון? כל מטריצה היא n מדידות × 5 אחוזונים.
comparison_flags <- lapply(models_compare, function(model) {
  sapply(PERCENTILES, function(p)
    train$bpd < model_percentile(model, train$pregweek, p))
})

# רווחי סמך באחוזונים של bootstrap אשכולות לפי הריון. בכל חזרה נדגמים
# הריונות עם החזרה; כל המדידות מהריון שנדגם מקבלות את אותו משקל bootstrap.
# העמודה הראשונה מסכמת את כל המדידות, והיתר מסכמות לפי שבוע.
BOOT_REPS <- 1000
set.seed(2026)
n_metrics <- length(models_compare) * length(PERCENTILES)
boot_rates <- array(NA_real_, dim = c(BOOT_REPS, n_metrics, n_week + 1))
for (b in seq_len(BOOT_REPS)) {
  multiplicity <- tabulate(sample.int(n_preg, n_preg, replace = TRUE), nbins = n_preg)
  row_weights <- multiplicity[preg_index]
  weekly_denominator <- as.vector(rowsum(row_weights, week_index))
  metric <- 0L
  for (model_name in model_names) {
    flags <- comparison_flags[[model_name]] * 1
    total_rates <- colSums(flags * row_weights) / sum(row_weights)
    weekly_numerator <- rowsum(flags * row_weights, week_index)
    weekly_rates <- t(weekly_numerator / weekly_denominator)
    for (j in seq_along(PERCENTILES)) {
      metric <- metric + 1L
      boot_rates[b, metric, ] <- c(total_rates[j], weekly_rates[j, ])
    }
  }
}

comparison_rows <- vector("list", n_metrics * (n_week + 1))
ri <- 0L
metric <- 0L
for (model_name in model_names) {
  flags <- comparison_flags[[model_name]]
  for (j in seq_along(PERCENTILES)) {
    metric <- metric + 1L
    observed <- c(mean(flags[, j]),
      as.vector(tapply(flags[, j], train$week, mean)))
    cis <- t(apply(boot_rates[, metric, ], 2, quantile,
                   probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE))
    for (g in seq_len(n_week + 1)) {
      ri <- ri + 1L
      is_all <- g == 1L
      use <- if (is_all) rep(TRUE, nrow(train)) else train$week == weeks_compare[g - 1L]
      nominal <- 100 * PERCENTILES[j]
      comparison_rows[[ri]] <- data.frame(
        model = model_name,
        week = if (is_all) "All" else as.character(weeks_compare[g - 1L]),
        percentile = nominal, expected_pct = nominal,
        observed_pct = 100 * observed[g],
        ci_low_pct = 100 * cis[g, 1], ci_high_pct = 100 * cis[g, 2],
        n_scans = sum(use), difference_pp = 100 * observed[g] - nominal,
        stringsAsFactors = FALSE)
    }
  }
}
comparison_table <- do.call(rbind, comparison_rows)
comparison_table$week_order <- 0L
is_week_row <- comparison_table$week != "All"
comparison_table$week_order[is_week_row] <- as.integer(comparison_table$week[is_week_row])
comparison_table <- comparison_table[order(comparison_table$percentile,
  comparison_table$model, comparison_table$week_order), ]
comparison_table$week_order <- NULL
save_table(comparison_table, "07_percentile_coverage")

# גרף כיול: לכל אחוזון מוצגים שיעור החריגה בפועל, רווח הסמך, וקו השיעור הנקוב.
save_plot("07_percentile_coverage", function() {
  op <- par(mfrow = c(2, 3), mar = c(4.2, 4.2, 3, 1))
  on.exit(par(op))
  model_cols <- c("Fractional polynomials" = COL_NEW,
                  "Hadlock" = COL_OLD, "Kurmanavicius" = COL_OLD2)
  for (p in 100 * PERCENTILES) {
    d <- comparison_table[comparison_table$percentile == p & comparison_table$week != "All", ]
    d$week_num <- as.integer(d$week)
    pad <- max(2, p * 0.15)
    ylim <- c(max(0, min(d$ci_low_pct, p) - pad),
              min(100, max(d$ci_high_pct, p) + pad))
    plot(NA, xlim = c(13, 40), ylim = ylim, xaxt = "n",
         xlab = heb("שבוע הריון"), ylab = heb("אחוז המדידות מתחת לעקומה"),
         main = paste0("P", format(p, trim = TRUE)))
    week_axis()
    abline(h = p, lty = 2, col = "grey40")
    for (mn in model_names) {
      z <- d[d$model == mn, ]
      lines(z$week_num, z$observed_pct, type = "b", pch = 16, cex = 0.6,
            col = model_cols[mn])
      nonzero_ci <- z$ci_high_pct > z$ci_low_pct
      if (any(nonzero_ci)) arrows(z$week_num[nonzero_ci], z$ci_low_pct[nonzero_ci],
        z$week_num[nonzero_ci], z$ci_high_pct[nonzero_ci],
        angle = 90, code = 3, length = 0.025, col = model_cols[mn])
    }
  }
  plot.new()
  legend("center", bty = "n", col = model_cols, lty = 1, pch = 16,
         legend = names(model_cols), cex = 0.9)
})
