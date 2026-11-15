###############################################################################
#  מעבדה בסטטיסטיקה - קבוצת BPD5 - שבוע 5
#  התאמת עקומות גדילה ל-BPD על נתוני האימון (train.csv משבוע 3) ברגרסיה
#  גמישה (cubic splines), לתוחלת ולסטיית התקן, בהנחת התפלגות נורמלית.
#
#  קובץ אחד לכל העבודה, סעיף לכל שלב:
#    0. הגדרות
#    1. הנתונים
#    2. מודלים להשוואה וממשק משותף (מוכן): Hadlock, Kurmanavicius, שבוע 4
#    3. מנוע הספליין (משותף לתוחלת ולסטיית התקן)
#    4. מודל התוחלת
#    5. מודל סטיית התקן (בסוף הסעיף מורכב new_model)
#    6. האחוזונים
#    7. טיב ההתאמה מול המודלים הקיימים
#
#  להריץ מתיקיית השורש של הפרויקט: Rscript "week 5/bpd_week5.R". R בסיסי בלבד.
###############################################################################

## ======================== 0. הגדרות =========================================

TRAIN_PATH <- file.path("week 3", "train.csv")
OUT_DIR    <- file.path("week 5", "output")

PERCENTILES <- c(0.03, 0.05, 0.10, 0.95, 0.975)
K_FOLDS     <- 5

set.seed(2026)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

save_table <- function(df, name) {
  write.csv(df, file.path(OUT_DIR, paste0(name, ".csv")), row.names = FALSE)
}

# גרפים: אותו עיצוב כמו בשבועות 3 ו-4. אותם גוונים כמו בגרפים של המצגת, ולכל
# צבע משמעות קבועה: כתום = המודל החדש או המודל שנבחר, כחול = Hadlock ומודל
# קיים, ירוק = Kurmanavicius, אפור-כחלחל = חלופה שנשקלה, אפור = הנתונים.
COL_NEW  <- "#C2510F"
COL_OLD  <- "#0B6AA8"
COL_OLD2 <- "#009E73"
COL_ALT  <- "#4F606C"
COL_BULK <- "grey30"
alpha_col <- function(col, a) adjustcolor(col, alpha.f = a)

# עברית בגרפים: מכשיר הגרפים של R לא מסדר טקסט מימין לשמאל, ולכן heb() הופכת
# את המילים והאותיות לסדר החזותי. מספרים ושמות באנגלית (BPD) נשארים כמו שהם.
# לעטוף בה כל טקסט עברי בגרף. ביטוי באנגלית בתוך משפט עברי (למשל p < 0.001)
# לכתוב עם רווח קשיח ( ) בין האיברים.
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
# קובץ האימון משבוע 3 (אחרי תיקון יחידות, ניקוי חריגים והסרת מדידות סמוכות).
# המדד: bpd במ"מ. גיל ההריון: pregweek בשבועות עם שברים. week: שבוע שלם.
# קובץ הבדיקה לא נטען: הוא שמור לשלב ההשוואה בין השיטות.

train <- read.csv(TRAIN_PATH, stringsAsFactors = FALSE)
cat("Train:", nrow(train), "scans |", length(unique(train$preg_id)), "pregnancies\n")

# חלוקה קבועה של ההריונות ל-K_FOLDS קבוצות, כמו בשבוע 4 (אותו seed ואותה
# שיטה, ולכן אותן קבוצות). כל המדידות של הריון נמצאות באותה קבוצה. לשימוש
# בכל הגרלה של cross-validation בשבוע הזה.
set.seed(2026)
ug <- unique(train$preg_id)
fold <- sample(rep(seq_len(K_FOLDS), length.out = length(ug)))[match(train$preg_id, ug)]

## ======================== 2. מודלים להשוואה וממשק משותף ======================
# מודל = שם + פונקציית תוחלת + פונקציית סטיית תקן, שתיהן של גיל הריון בשבועות.
#   model$mean(w)                   תוחלת BPD במ"מ
#   model$sd(w)                     סטיית תקן במ"מ
#   model_percentile(model, w, p)   האחוזון p (למשל 0.05)
#   model_z(model, w, y)            ציון תקן של מדידה

make_model <- function(name, mean_fun, sd_fun) list(name = name, mean = mean_fun, sd = sd_fun)
model_percentile <- function(model, w, p) model$mean(w) + qnorm(p) * model$sd(w)
model_z <- function(model, w, y) (y - model$mean(w)) / model$sd(w)

# הדלוק (Hadlock 1984): BPD בס"מ = -3.08 + 0.41*w - 0.000061*w^3, סטיית תקן 0.3 ס"מ.
# זו השיטה שבה משתמשים היום בישראל (Intro1.pdf), ולכן היא מופיעה בכל גרף של המודל.
had_model <- make_model("Hadlock",
  function(w) 10 * (-3.08 + 0.41 * w - 0.000061 * w^3),
  function(w) rep(3, length(w)))

# Kurmanavicius (1999): הנוסחאות מקובץ ההקדמה (Intro1.pdf), במ"מ.
kur_model <- make_model("Kurmanavicius",
  function(w) -28.04 + 4.179 * w - 0.0006414 * w^3,
  function(w) 1.648 + 0.06531 * w)

# המודל הסופי של שבוע 4 (fractional polynomials), מותאם מחדש על אותו קובץ אימון:
# תוחלת עם w^2 ו-w^3, וסטיית תקן = sqrt(pi/2) כפול קו ישר של |השארית| על w.
fp_mean_fit <- lm(bpd ~ I(pregweek^2) + I(pregweek^3), data = train)
fp_sd_fit   <- lm(abs(residuals(fp_mean_fit)) ~ pregweek, data = train)
fp_model <- make_model("Fractional polynomials (week 4)",
  function(w) as.vector(predict(fp_mean_fit, data.frame(pregweek = w))),
  function(w) sqrt(pi / 2) * as.vector(predict(fp_sd_fit, data.frame(pregweek = w))))
cat("\nWeek 4 model - mean:", round(coef(fp_mean_fit), 6),
    "| sd:", round(sqrt(pi / 2) * coef(fp_sd_fit), 4), "\n")

## ======================== 3. מנוע הספליין (משותף) ===========================
# מטרה: הכלים להתאמת ספליין (README, החלטה 1), כך שאותו מנוע ישמש גם לתוחלת
# וגם לסטיית התקן. סוג הספליין ואופן הבנייה: החלטות 1ב ו-2.
# מה צריך להיות כאן: התאמה של מודל אחד, פונקציית predict(w), תחזיות מחוץ
# למדגם עם fold, ודרך לכתוב את המודל הסופי כנוסחה.

# א. הנימוק להחלטה 1ב (ספליין רגיל או טבעי): קצב הגדילה של ה-BPD הממוצע משבוע
# לשבוע. ספליין טבעי מאלץ את העקומה להיות ישרה בקצוות, כלומר קצב גדילה שלא
# משתנה שם. הטבלה בודקת אם זה מתאים לנתונים בתחילת ההריון ובסופו.
# הגדילה לשבוע = ההפרש בין הממוצעים של שבועות סמוכים, חלקי ההפרש בגיל הממוצע
# שלהם. המרווח בין הגילים הממוצעים אינו בדיוק שבוע. בשבוע 39 הוא ארוך יותר,
# כי שבוע 39 כולל את יום 280, לפי ההגדרה של week.
growth_by_week <- data.frame(week = sort(unique(train$week)),
                             n = as.vector(table(train$week)),
                             mean_age = as.vector(tapply(train$pregweek, train$week, mean)),
                             mean_bpd = as.vector(tapply(train$bpd, train$week, mean)))
growth_by_week$growth <- c(NA, diff(growth_by_week$mean_bpd) / diff(growth_by_week$mean_age))
save_table(growth_by_week, "03_growth_by_week")
cat("\nMean BPD and weekly growth:\n")
print(growth_by_week, digits = 4)
# טווח הנתונים: האם העקומות (14-40) דורשות חיזוי מחוץ לנתונים?
cat("pregweek range:", range(train$pregweek), "| scans at day 280:", sum(train$pregweek == 40), "\n")

# ב. המנוע: בסיס ידני של חזקות קטומות, כמו בשיעור (README, החלטה 2).
# ספליין קובי רגיל:  b0 + b1*w + b2*w^2 + b3*w^3 + סכום על הקשרים g_k*(w - t_k)^3+
# ספליין קובי טבעי:  b0 + b1*w + סכום על הקשרים q_k*B_k(w), כאשר
#   B_t(w) = (w-t)^3+ - (b-t)/(b-a)*(w-a)^3+ - (t-a)/(b-a)*(w-b)^3+
#   ו-a, b הן נקודות הקצה: מחוץ להן העקומה ישרה. a ו-b = 14 ו-40, טווח הנתונים.
# (x)^3+ פירושו x^3 כש-x חיובי, ו-0 אחרת.
# בזמן ההתאמה השבוע מחולק ב-10, כמו בשבוע 4, כדי שהעמודות לא יהיו בסדרי
# גודל שונים מאוד. העקומה לא משתנה, והמקדמים מומרים חזרה לשבועות בנוסחה.
SPLINE_BOUNDS <- c(14, 40)

tpow3 <- function(z, t) pmax(z - t, 0)^3

# עמודות הבסיס (בלי חותך), ב-z = w / 10
spline_basis <- function(x, knots, natural = FALSE, bounds = SPLINE_BOUNDS) {
  z <- x / 10; tk <- knots / 10; a <- bounds[1] / 10; b <- bounds[2] / 10
  cols <- function(f) matrix(vapply(tk, f, numeric(length(z))), nrow = length(z))
  if (!natural) return(cbind(z, z^2, z^3, cols(function(t) tpow3(z, t))))
  cbind(z, cols(function(t) tpow3(z, t) - (b - t) / (b - a) * tpow3(z, a) -
                              (t - a) / (b - a) * tpow3(z, b)))
}

# המודל כנוסחה בשבועות: טבלת איברים ומקדמים. בספליין טבעי פותחים את B_t,
# ולכן מופיעים גם איברים קטומים בנקודות הקצה a ו-b.
spline_terms <- function(coef_z, knots, natural = FALSE, bounds = SPLINE_BOUNDS) {
  if (!natural) {
    return(data.frame(term = c("1", "w", "w^2", "w^3", sprintf("(w-%g)^3+", knots)),
                      power = c(0:3, rep(3, length(knots))), knot = c(rep(NA, 4), knots),
                      coef = coef_z / 10^c(0:3, rep(3, length(knots)))))
  }
  a <- bounds[1]; b <- bounds[2]; q <- coef_z[-(1:2)] / 1000
  data.frame(term = c("1", "w", sprintf("(w-%g)^3+", c(knots, a, b))),
             power = c(0, 1, rep(3, length(knots) + 2)), knot = c(NA, NA, knots, a, b),
             coef = c(coef_z[1], coef_z[2] / 10, q,
                      -sum(q * (b - knots)) / (b - a), -sum(q * (knots - a)) / (b - a)))
}

# חישוב ערך המודל מתוך טבלת האיברים (הנוסחה שתירשם במצגת)
eval_terms <- function(terms, w) {
  as.vector(sapply(seq_len(nrow(terms)), function(i) {
    base <- if (is.na(terms$knot[i])) w else pmax(w - terms$knot[i], 0)
    base^terms$power[i]
  }, simplify = "matrix") %*% terms$coef)
}

# התאמת מודל אחד. מחזירה את הקשרים, מספר המקדמים, RSS, הנוסחה בשבועות
# (terms), ופונקציה predict(xnew).
fit_spline <- function(y, x, knots = numeric(0), natural = FALSE) {
  knots <- sort(knots)
  X <- cbind(1, spline_basis(x, knots, natural))
  fit <- lm.fit(X, y)
  coef_z <- fit$coefficients
  list(knots = knots, natural = natural, k = ncol(X), rss = sum(fit$residuals^2),
       terms = spline_terms(coef_z, knots, natural),
       predict = function(xnew) as.vector(cbind(1, spline_basis(xnew, knots, natural)) %*% coef_z))
}

spline_label <- function(knots, natural = FALSE) {
  paste0(if (natural) "natural" else "cubic", " | knots: ",
         if (length(knots) == 0) "none" else paste(sort(knots), collapse = ", "))
}

# הנוסחה כטקסט, לפלטים ולמצגת. 5 ספרות משמעותיות: עם 4 הנוסחה המעוגלת סוטה
# מהעקומה בעד 0.05 מ"מ, ועם 5 בעד 0.008 מ"מ (README, המודל הסופי לתוחלת).
spline_formula <- function(fit, digits = 5) {
  s <- fit$terms$coef
  mag <- formatC(abs(s), digits = digits, format = "fg")
  parts <- ifelse(fit$terms$term == "1", mag, paste0(mag, "*", fit$terms$term))
  out <- paste0(ifelse(s < 0, "- ", "+ "), parts, collapse = " ")
  sub("^- ", "-", sub("^\\+ ", "", out))
}

# תחזיות מחוץ למדגם לפי חלוקה להריונות (ברירת המחדל: fold מסעיף 1).
# y ו-x חייבים להיות בסדר השורות של train. מטריצת הבסיס נבנית פעם אחת,
# ובכל סיבוב מתאימים על השורות של 4 הקבוצות וחוזים את החמישית.
cv_predict_spline <- function(y, x, knots = numeric(0), natural = FALSE, folds = fold) {
  X <- cbind(1, spline_basis(x, sort(knots), natural))
  pred <- numeric(length(y))
  for (k in seq_len(K_FOLDS)) {
    tr <- folds != k
    pred[!tr] <- X[!tr, , drop = FALSE] %*% .lm.fit(X[tr, , drop = FALSE], y[tr])$coefficients
  }
  pred
}

# ג. בדיקות שהמנוע נכון. הקשרים 20 ו-30 הם דוגמה לבדיקה בלבד, לא הבחירה.
check_knots <- c(20, 30)
w_in  <- seq(14, 40, 0.25)
w_out <- c(10, 11, 12, 42, 43, 44)
cub <- fit_spline(train$bpd, train$pregweek, check_knots)
nat <- fit_spline(train$bpd, train$pregweek, check_knots, natural = TRUE)
second_diff <- function(f, w) max(abs(f(w[3]) - 2 * f(w[2]) + f(w[1])),
                                  abs(f(w[6]) - 2 * f(w[5]) + f(w[4])))
engine_checks <- data.frame(check = c(
    "cubic, no knots = cubic polynomial (lm)",
    "natural, no knots = straight line (lm)",
    "cubic: formula in weeks = fitted curve",
    "natural: formula in weeks = fitted curve",
    "natural: straight line outside 14-40 (2nd difference)",
    "cubic: number of coefficients = 4 + knots",
    "natural: number of coefficients = 2 + knots"),
  value = c(
    max(abs(fit_spline(train$bpd, train$pregweek)$predict(w_in) -
            predict(lm(bpd ~ pregweek + I(pregweek^2) + I(pregweek^3), train), data.frame(pregweek = w_in)))),
    max(abs(fit_spline(train$bpd, train$pregweek, natural = TRUE)$predict(w_in) -
            predict(lm(bpd ~ pregweek, train), data.frame(pregweek = w_in)))),
    max(abs(eval_terms(cub$terms, w_in) - cub$predict(w_in))),
    max(abs(eval_terms(nat$terms, w_in) - nat$predict(w_in))),
    second_diff(nat$predict, w_out),
    cub$k - (4 + length(check_knots)),
    nat$k - (2 + length(check_knots))))
engine_checks$passed <- abs(engine_checks$value) < 1e-6
save_table(engine_checks, "03_engine_checks")
cat("\nSpline engine checks:\n")
print(engine_checks, digits = 3)
stopifnot(all(engine_checks$passed))

## ======================== 4. מודל התוחלת =====================================
# מטרה: מודל גמיש לתוחלת ה-BPD כפונקציה של גיל ההריון.
# מה להגיש: המועמדים שנשקלו (כמה knots ואיפה), הקריטריון והבחירה, המודל
# הסופי כנוסחה, שאריות לפי שבוע, וגרף המודל מול הנתונים עם Hadlock.
# התוצאה: mean_fit.

# א. המועמדים (README, החלטה 3): 2 או 3 קשרים, בשבועות שלמים בין 16 ל-38,
# עם לפחות שבועיים בין קשרים סמוכים.
KNOT_GRID   <- 16:38
KNOT_COUNTS <- 2:3
MIN_GAP     <- 2
knot_sets <- unlist(lapply(KNOT_COUNTS, function(K)
  Filter(function(t) all(diff(t) >= MIN_GAP), combn(KNOT_GRID, K, simplify = FALSE))),
  recursive = FALSE)
cat("\nKnot sets searched:", length(knot_sets), "\n")

# שיטות המיקום מהשיעור, מעוגלות לשבוע שלם. הן חלק מהחיפוש, ומסומנות בטבלה
# כדי שיהיה ברור איפה דורגה כל אחת.
round_week <- function(x) floor(x + 0.5)
ref_sets <- rbind(
  data.frame(method = "quantiles", knots = sapply(KNOT_COUNTS, function(K)
    paste(round_week(quantile(train$pregweek, (1:K) / (K + 1), names = FALSE)), collapse = ", "))),
  data.frame(method = "equal width", knots = sapply(KNOT_COUNTS, function(K)
    paste(round_week(SPLINE_BOUNDS[1] + diff(SPLINE_BOUNDS) * (1:K) / (K + 1)), collapse = ", "))),
  data.frame(method = c("clinical: weeks 20, 30", "clinical: routine scans 22, 32"),
             knots = c("20, 30", "22, 32")))

# ב. הציונים (README, החלטה 4), על הריונות מוסתרים:
#   bias      ההטיה השבועית: ממוצע השאריות בכל שבוע, ושורש ממוצע הריבועים על
#             פני השבועות. כל שבוע באותו משקל. זה קריטריון הבחירה.
#   bias_se   טעות התקן שלו: סטיית התקן של אותו מדד בין 5 הקבוצות, חלקי שורש 5.
#   max_bias  ההטיה בשבוע שבו העקומה הכי רחוקה מהנתונים (כמו בשבוע 4).
#   rmse      השגיאה לכל מדידה, כמו בשבוע 4. מדווח להשוואה, לא לבחירה.
weekly_bias <- function(res, week) sqrt(mean(tapply(res, week, mean)^2))
score_cv <- function(pred, folds = fold) {
  res <- train$bpd - pred
  per_fold <- sapply(seq_len(K_FOLDS), function(k) weekly_bias(res[folds == k], train$week[folds == k]))
  c(bias = weekly_bias(res, train$week), bias_se = sd(per_fold) / sqrt(K_FOLDS),
    max_bias = max(abs(tapply(res, train$week, mean))), rmse = sqrt(mean(res^2)))
}
score_set <- function(knots, natural = FALSE, folds = fold)
  score_cv(cv_predict_spline(train$bpd, train$pregweek, knots, natural, folds), folds)

mean_table <- do.call(rbind, lapply(c(FALSE, TRUE), function(nat) {
  sc <- t(sapply(knot_sets, score_set, natural = nat))
  data.frame(type = if (nat) "natural" else "cubic",
             knots = sapply(knot_sets, paste, collapse = ", "),
             n_knots = lengths(knot_sets),
             coefs = lengths(knot_sets) + if (nat) 2 else 4, sc)
}))
mean_table$method <- ref_sets$method[match(mean_table$knots, ref_sets$knots)]
mean_table$method[is.na(mean_table$method)] <- "search"

# נקודות ייחוס, לא מועמדות: ספליין בלי קשרים (פולינום מדרגה 3) והמודל של שבוע 4
fp_cv <- numeric(nrow(train))
for (k in seq_len(K_FOLDS)) {
  tr <- fold != k
  fp_cv[!tr] <- predict(lm(bpd ~ I(pregweek^2) + I(pregweek^3), train[tr, ]), train[!tr, ])
}
mean_refs <- data.frame(type = c("cubic, no knots", "week 4: FP (2,3)"), knots = "",
                        n_knots = 0, coefs = c(4, 3),
                        rbind(score_set(numeric(0)), score_cv(fp_cv)),
                        method = "reference")

# ג. הבחירה, בכלל שנקבע מראש (one-standard-error rule): מבין הספליינים הקוביים,
# המודל עם הכי מעט קשרים שהציון שלו לא גבוה מהציון הטוב ביותר ועוד טעות תקן
# אחת. בין מודלים עם אותו מספר קשרים: הציון הנמוך ביותר.
cubic_rows <- mean_table[mean_table$type == "cubic", ]
best_row   <- cubic_rows[which.min(cubic_rows$bias), ]
threshold  <- best_row$bias + best_row$bias_se
eligible   <- cubic_rows[cubic_rows$bias <= threshold, ]
eligible   <- eligible[eligible$n_knots == min(eligible$n_knots), ]
chosen_row <- eligible[which.min(eligible$bias), ]
mean_knots <- as.numeric(strsplit(chosen_row$knots, ", ")[[1]])
mean_table$chosen <- mean_table$type == "cubic" & mean_table$knots == chosen_row$knots
mean_refs$chosen <- FALSE

mean_table <- mean_table[order(mean_table$bias), ]
mean_table$rank <- ave(mean_table$bias, mean_table$type, FUN = seq_along)
mean_refs$rank <- NA
save_table(rbind(mean_refs, mean_table), "04_mean_candidates")

cat("\nBest cubic:", best_row$knots, "| bias", round(best_row$bias, 4),
    "| SE", round(best_row$bias_se, 4), "| threshold", round(threshold, 4), "\n")
cat("Chosen:", chosen_row$knots, "| bias", round(chosen_row$bias, 4), "\n")
cat("\nTop cubic candidates by weekly bias:\n")
print(head(mean_table[mean_table$type == "cubic", ], 10), digits = 4, row.names = FALSE)
cat("\nPlacement methods from class:\n")
print(mean_table[mean_table$method != "search" & mean_table$type == "cubic", ], digits = 4, row.names = FALSE)
cat("\nBest natural:\n")
print(head(mean_table[mean_table$type == "natural", ], 3), digits = 4, row.names = FALSE)
cat("\nReferences:\n")
print(mean_refs, digits = 4, row.names = FALSE)

# ד. יציבות הבחירה: אותו חישוב עם 5 חלוקות אקראיות אחרות להריונות, על 20
# המועמדים הקוביים המובילים. אם בכל חלוקה נבחר משהו אחר, הבחירה לא באמת
# נקבעה על ידי הנתונים.
top_sets <- head(mean_table[mean_table$type == "cubic", ], 20)
stability <- do.call(rbind, lapply(2027:2031, function(s) {
  set.seed(s)
  f <- sample(rep(seq_len(K_FOLDS), length.out = length(ug)))[match(train$preg_id, ug)]
  sc <- t(sapply(top_sets$knots, function(kn) score_set(as.numeric(strsplit(kn, ", ")[[1]]), folds = f)))
  sc <- data.frame(knots = top_sets$knots, n_knots = top_sets$n_knots, sc)
  b <- sc[which.min(sc$bias), ]
  el <- sc[sc$bias <= b$bias + b$bias_se, ]
  el <- el[el$n_knots == min(el$n_knots), ]
  data.frame(seed = s, best = b$knots, chosen_by_rule = el$knots[which.min(el$bias)],
             main_choice_bias = sc$bias[sc$knots == chosen_row$knots],
             best_bias = b$bias)
}))
save_table(stability, "04_mean_stability")
cat("\nStability across other splits:\n")
print(stability, digits = 4, row.names = FALSE)

# ה. המודל הסופי: הקשרים שנבחרו, מותאמים על כל קובץ האימון
mean_fit <- fit_spline(train$bpd, train$pregweek, mean_knots)
save_table(mean_fit$terms[, c("term", "coef")], "04_mean_final_model")
cat("\nFinal mean model:\n", spline_formula(mean_fit), "\n")
stopifnot(all(diff(mean_fit$predict(seq(14, 40, 0.01))) > 0))   # עולה בכל הטווח

# ו. שאריות לפי שבוע, לצד השאריות של המודל משבוע 4 ושל Hadlock
week_resid <- function(f) as.vector(tapply(train$bpd - f(train$pregweek), train$week, mean))
resid_by_week <- data.frame(week = sort(unique(train$week)), n = as.vector(table(train$week)),
                            mean_resid = week_resid(mean_fit$predict),
                            mean_resid_week4 = week_resid(fp_model$mean),
                            mean_resid_hadlock = week_resid(had_model$mean))
save_table(resid_by_week, "04_mean_residuals_by_week")
cat("\nMean residual by week:\n")
print(resid_by_week, digits = 3, row.names = FALSE)

# ז. קצב הגדילה לפי העקומות, מול קצב הגדילה בנתונים (סעיף 3א). כל עקומה
# מחושבת בגיל הממוצע של כל שבוע, כמו הממוצעים של הנתונים, ובאותה חלוקה.
week_age <- growth_by_week$mean_age
curve_growth <- function(f) c(NA, diff(f(week_age)) / diff(week_age))
growth_compare <- data.frame(week = growth_by_week$week, data = growth_by_week$growth,
                             spline = curve_growth(mean_fit$predict),
                             week4 = curve_growth(fp_model$mean),
                             hadlock = curve_growth(had_model$mean))
save_table(growth_compare, "04_mean_growth")
cat("\nWeekly growth (mm): data vs curves:\n")
print(growth_compare, digits = 3, row.names = FALSE)

# ח. גרפים. בכל גרף: המודל החדש (כתום), Hadlock (כחול), המודל של שבוע 4
# (אפור-כחלחל, README החלטה 6), והקשרים כקווים אנכיים מנוקדים.
mark_knots <- function(y_lab) {
  abline(v = mean_knots, col = alpha_col(COL_NEW, 0.5), lty = 3, lwd = 1.5)
  text(mean_knots, y_lab, mean_knots, col = COL_NEW, font = 2, cex = 0.85)
}
curve_legend <- heb(c("ספליין: המודל החדש", "המודל של שבוע 4", "Hadlock"))
curve_cols <- c(COL_NEW, COL_ALT, COL_OLD)

save_plot("04_mean_fit", function() {
  weeks <- seq(14, 40, 0.1)
  plot(train$pregweek, train$bpd, pch = 16, cex = 0.25, col = alpha_col(COL_BULK, 0.12),
       xlim = c(14, 40), ylim = c(10, 110), xaxt = "n", yaxt = "n",
       xlab = heb("שבוע הריון"), ylab = heb("BPD (מ\"מ)"),
       main = heb("מודל התוחלת: ספליין עם שלושה קשרים"),
       panel.first = soft_grid(seq(10, 110, 10)))
  week_axis()
  axis(2, at = seq(10, 110, 10), las = 1)
  mark_knots(108)
  points(week_age, tapply(train$bpd, train$week, mean), pch = 21, bg = "white", col = COL_BULK, cex = 1.1)
  lines(weeks, had_model$mean(weeks), col = COL_OLD, lwd = 2)
  lines(weeks, fp_model$mean(weeks), col = COL_ALT, lwd = 2, lty = 2)
  lines(weeks, mean_fit$predict(weeks), col = COL_NEW, lwd = 3)
  legend("bottomright", bty = "n", pch = c(21, rep(NA, 3)), lty = c(NA, 1, 2, 1),
         lwd = c(NA, 3, 2, 2), pt.bg = c("white", NA, NA, NA), col = c(COL_BULK, curve_cols),
         legend = c(heb("ממוצע המדידות בשבוע"), curve_legend))
}, pointsize = PLOT_PT_SLIDE)

save_plot("04_mean_residuals_by_week", function() {
  plot(NA, xlim = c(14, 40), ylim = c(-2, 1.25), xaxt = "n", yaxt = "n",
       xlab = heb("שבוע הריון"), ylab = heb("ממוצע השאריות (מ\"מ)"),
       main = heb("הגל של שבוע 4 נעלם: העקומה החדשה קרובה לממוצע בכל שבוע"),
       panel.first = soft_grid(seq(-2, 1.25, 0.25)))
  week_axis()
  axis(2, at = seq(-2, 1, 0.5), las = 1)
  abline(h = 0, col = "grey40", lty = 2)
  mark_knots(1.2)
  lines(resid_by_week$week, resid_by_week$mean_resid_hadlock, col = COL_OLD, lwd = 2, type = "o", pch = 16, cex = 0.7)
  lines(resid_by_week$week, resid_by_week$mean_resid_week4, col = COL_ALT, lwd = 2, lty = 2,
        type = "o", pch = 16, cex = 0.7)
  lines(resid_by_week$week, resid_by_week$mean_resid, col = COL_NEW, lwd = 3, type = "o", pch = 16)
  legend("bottomleft", bty = "n", lty = c(1, 2, 1), lwd = c(3, 2, 2), pch = 16,
         col = curve_cols, legend = curve_legend)
  text(40, 0.95, heb("מעל האפס = המדידות גבוהות מהעקומה"), adj = 1, col = "grey30")
  text(40, -1.85, heb("מתחת לאפס = המדידות נמוכות מהעקומה"), adj = 1, col = "grey30")
}, pointsize = PLOT_PT_SLIDE)

save_plot("04_mean_growth", function() {
  g <- growth_compare[-1, ]
  plot(NA, xlim = c(14, 40), ylim = c(0.5, 4), xaxt = "n", yaxt = "n",
       xlab = heb("שבוע הריון"), ylab = heb("גדילה לשבוע (מ\"מ)"),
       main = heb("העקומה החדשה עוקבת אחרי האטת הגדילה שבנתונים בשבועות 21–22"),
       panel.first = soft_grid(seq(0.5, 4, 0.5)))
  week_axis()
  axis(2, at = seq(0.5, 4, 0.5), las = 1)
  mark_knots(3.95)
  lines(g$week, g$hadlock, col = COL_OLD, lwd = 2)
  lines(g$week, g$week4, col = COL_ALT, lwd = 2, lty = 2)
  lines(g$week, g$spline, col = COL_NEW, lwd = 3)
  points(g$week, g$data, pch = 21, bg = "white", col = COL_BULK, cex = 1.1)
  legend("bottomleft", bty = "n", pch = c(21, rep(NA, 3)), lty = c(NA, 1, 2, 1),
         lwd = c(NA, 3, 2, 2), pt.bg = c("white", NA, NA, NA), col = c(COL_BULK, curve_cols),
         legend = c(heb("בנתונים: לפי ממוצעי השבועות"), curve_legend))
}, pointsize = PLOT_PT_SLIDE)

## ======================== 5. מודל סטיית התקן =================================
# מטרה: מודל לסטיית התקן, על |השארית| של מודל התוחלת, כמו בשבוע 4.
# ההחלטה (README, החלטה 5): קו ישר, כמו בשבוע 4. בלי חיפוש קשרים.
# מה להגיש: המודל הסופי כנוסחה, בדיקת ציוני Z לפי שבוע, וגרף סטיית התקן עם
# Hadlock ועם המודל של שבוע 4.
# התוצאה: sd_fit, ובסוף הסעיף new_model.

# א. סטיית התקן = sqrt(pi/2) כפול קו ישר של |השארית| על השבוע: בהתפלגות
# נורמלית התוחלת של |שארית| היא סטיית התקן כפול sqrt(2/pi), כ-0.8.
mean_resid_all <- train$bpd - mean_fit$predict(train$pregweek)
sd_fit <- lm(abs(mean_resid_all) ~ pregweek, data = train)
sd_fun <- function(w) sqrt(pi / 2) * as.vector(predict(sd_fit, data.frame(pregweek = w)))
sd_coef <- data.frame(term = c("1", "w"), coef = sqrt(pi / 2) * as.vector(coef(sd_fit)))
save_table(sd_coef, "05_sd_final_model")
cat("\nFinal SD model: SD =", signif(sd_coef$coef[1], 5), "+", signif(sd_coef$coef[2], 5), "* w\n")
stopifnot(all(sd_fun(seq(14, 40, 0.1)) > 0))   # חיובית בכל הטווח

new_model <- make_model("Spline", mean_fit$predict, sd_fun)

# ב. בדיקת ההנחה: ציוני Z = (מדידה - תוחלת) / סטיית תקן. אם המודל טוב, בכל
# שבוע הממוצע שלהם קרוב ל-0, סטיית התקן קרובה ל-1, וכ-90% מהם בין
# -1.645 ל-1.645. לצד המודל החדש: המודל של שבוע 4 ו-Hadlock.
z_models <- list(spline = new_model, week4 = fp_model, hadlock = had_model)
z_all <- lapply(z_models, function(m) model_z(m, train$pregweek, train$bpd))
z_summary <- data.frame(model = names(z_all),
                        mean_z = sapply(z_all, mean), sd_z = sapply(z_all, sd),
                        pct_within_1.645 = sapply(z_all, function(z) 100 * mean(abs(z) <= qnorm(0.95))))
save_table(z_summary, "05_z_summary")
cat("\nZ scores, all training data:\n")
print(z_summary, digits = 3, row.names = FALSE)

z_by_week <- data.frame(week = sort(unique(train$week)), n = as.vector(table(train$week)))
for (m in names(z_all)) {
  z_by_week[[paste0("mean_z_", m)]] <- as.vector(tapply(z_all[[m]], train$week, mean))
  z_by_week[[paste0("sd_z_", m)]]   <- as.vector(tapply(z_all[[m]], train$week, sd))
  z_by_week[[paste0("pct_within_", m)]] <-
    as.vector(tapply(abs(z_all[[m]]) <= qnorm(0.95), train$week, mean)) * 100
}
save_table(z_by_week, "05_z_by_week")
cat("\nZ scores by week:\n")
print(z_by_week[, c("week", "n", "mean_z_spline", "sd_z_spline", "pct_within_spline",
                    "sd_z_week4")], digits = 3, row.names = FALSE)

# ג. גרפים
# סטיית התקן לפי שבוע: אומדן לכל שבוע בנפרד (sqrt(pi/2) כפול ממוצע |השארית|)
# מול הקו הישר, המודל של שבוע 4 ו-Hadlock (סטיית תקן קבועה, 3 מ"מ).
sd_by_week <- sqrt(pi / 2) * as.vector(tapply(abs(mean_resid_all), train$week, mean))
save_plot("05_sd_fit", function() {
  weeks <- seq(14, 40, 0.1)
  plot(NA, xlim = c(14, 40), ylim = c(2, 4), xaxt = "n", yaxt = "n",
       xlab = heb("שבוע הריון"), ylab = heb("סטיית התקן של BPD (מ\"מ)"),
       main = heb("הקו הישר מחמיץ את הפיזור הגדול בשבועות 23–25"),
       panel.first = soft_grid(seq(2, 4, 0.25)))
  week_axis()
  axis(2, at = seq(2, 4, 0.5), las = 1)
  lines(weeks, had_model$sd(weeks), col = COL_OLD, lwd = 2)
  lines(weeks, fp_model$sd(weeks), col = COL_ALT, lwd = 2, lty = 2)
  lines(weeks, sd_fun(weeks), col = COL_NEW, lwd = 3)
  points(week_age, sd_by_week, pch = 21, bg = "white", col = COL_BULK, cex = 1.1)
  legend("topleft", bty = "n", pch = c(21, rep(NA, 3)), lty = c(NA, 1, 2, 1),
         lwd = c(NA, 3, 2, 2), pt.bg = c("white", NA, NA, NA), col = c(COL_BULK, curve_cols),
         legend = c(heb("אומדן לכל שבוע בנפרד"), heb("קו ישר: המודל החדש"),
                    heb("המודל של שבוע 4"), "Hadlock"))
}, pointsize = PLOT_PT_SLIDE)

save_plot("05_z_by_week", function() {
  op <- par(mfrow = c(1, 2), mar = c(4.2, 4.2, 3, 1))
  on.exit(par(op))
  panel <- function(prefix, target, ylim, title, ylab) {
    plot(NA, xlim = c(14, 40), ylim = ylim, xaxt = "n", las = 1,
         xlab = heb("שבוע הריון"), ylab = heb(ylab), main = heb(title))
    week_axis()
    abline(h = target, col = "grey40", lty = 2)
    lines(z_by_week$week, z_by_week[[paste0(prefix, "hadlock")]], col = COL_OLD, lwd = 2, type = "o", pch = 16, cex = 0.6)
    lines(z_by_week$week, z_by_week[[paste0(prefix, "week4")]], col = COL_ALT, lwd = 2, lty = 2, type = "o", pch = 16, cex = 0.6)
    lines(z_by_week$week, z_by_week[[paste0(prefix, "spline")]], col = COL_NEW, lwd = 3, type = "o", pch = 16, cex = 0.8)
  }
  panel("mean_z_", 0, c(-0.8, 0.5), "ממוצע ציוני Z לפי שבוע", "ממוצע Z")
  legend("bottomleft", bty = "n", lty = c(1, 2, 1), lwd = c(3, 2, 2), pch = 16,
         col = curve_cols, legend = curve_legend, cex = 0.85)
  panel("sd_z_", 1, c(0.7, 1.3), "סטיית התקן של ציוני Z לפי שבוע", "סטיית התקן של Z")
})

## ======================== 6. האחוזונים =======================================
# מטרה: האחוזונים 3, 5, 10, 95 ו-97.5 של המודל החדש לכל שבוע 14-40.
# מה להגיש: טבלה לכל שבוע שלם, וגרף העקומות מעל הנתונים, עם עקומות Hadlock.

## ======================== 7. טיב ההתאמה מול המודלים הקיימים ==================
# מטרה: אחוז המדידות מתחת לכל עקומה, בפועל מול הנקוב, בכלל קובץ האימון ולפי
# שבוע: המודל החדש מול Hadlock, Kurmanavicius והמודל של שבוע 4.
# מה להגיש: טבלה עם רווחי סמך (bootstrap לפי הריון, כמו בשבוע 4), גרף ומסקנה.
