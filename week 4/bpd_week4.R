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
# לעיוורי צבעים). אפור = הנתונים, כתום = המודל החדש, כחול = מודל קיים.
COL_NEW  <- "#D55E00"
COL_OLD  <- "#0072B2"
COL_OLD2 <- "#009E73"
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

save_plot <- function(name, draw, width = 10, height = 6) {
  png(file.path(OUT_DIR, paste0(name, ".png")), width = width, height = height,
      units = "in", res = 150)
  tryCatch(draw(), finally = dev.off())
  invisible(NULL)
}

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
# החזקה 0 היא log(x). x מחולק ב-10 (שבועות / 10) לייצוב מספרי, כמו במאמר.
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
  X <- cbind(1, fp_basis(x, powers))
  fit <- lm.fit(X, y)
  n <- length(y); k <- ncol(X); rss <- sum(fit$residuals^2)
  loglik <- -n / 2 * (log(2 * pi) + log(rss / n) + 1)
  list(powers = powers, coef = fit$coefficients, n = n, k = k, rss = rss,
       aic = -2 * loglik + 2 * (k + 1), bic = -2 * loglik + log(n) * (k + 1),
       predict = function(xnew) as.vector(cbind(1, fp_basis(xnew, powers)) %*% fit$coefficients))
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

mean_fit <- NULL

# TODO: בחירת המודל, mean_fit <- fit_fp(train$bpd, train$pregweek, <החזקות שנבחרו>)

## ======================== 5. משימה 2 - מודל סטיית התקן ======================
# מטרה: מודל לסטיית התקן של BPD כפונקציה של גיל הריון, גם הוא עם fractional
# polynomials. השיטה המקובלת: רגרסיה של |השארית| על גיל, וסטיית התקן היא
# sqrt(pi/2) כפול הערך המותאם. השאריות מגיעות מהמודל של משימה 1
# (עד שהוא מוכן משתמשים בתוחלת הזמנית).
# מה להגיש: איך בחרתם את המשתנים (אותו קריטריון של משימה 1, או נימוק לקריטריון
# אחר), המודל הסופי, ובדיקה שציוני התקן z = (y - תוחלת) / sd מתפלגים כ-N(0,1)
# (ממוצע 0, סטיית תקן 1, ללא תלות בגיל), כי האחוזונים נשענים על הנחה זו.
# התוצאה: sd_fit (האובייקט שמחזירה fit_fp, על |השארית|).

sd_fit <- NULL

mean_fun_now <- function() if (!is.null(mean_fit)) mean_fit$predict else prov_mean$predict
# resid_now <- train$bpd - mean_fun_now()(train$pregweek)

# TODO: בחירת המודל, sd_fit <- fit_fp(abs(resid_now), train$pregweek, <החזקות>)

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

# TODO: חישוב האחוזונים והגרף (save_plot, heb)

## ======================== 7. משימה 4 - הצלחת המודל מול הקיימים ===============
# מטרה: לתאר את הצלחת המודל החדש לעומת המודלים הקיימים (had_model, kur_model)
# על קובץ האימון. הרעיון כמו בשבוע 2: אם העקומה מתאימה, אחוז המדידות
# שחורגות מכל אחוזון צריך להיות שווה לאחוזון הנקוב (3, 5, 10, 95, 97.5).
# מה להגיש: טבלה והשוואה (שיעור החריגות בפועל מול הנקוב, לכל מודל ולכל אחוזון,
# עם רווח סמך), גרף, ומסקנה איזה מודל מתאים יותר ובאיזה שבועות. אפשר להוסיף
# בדיקה על קובץ הבדיקה (test) כבדיקה משלימה.
# כלים מוכנים: model_z(model, w, y), model_percentile(model, w, p).

# TODO: חישוב השיעורים, הטבלה והגרף
