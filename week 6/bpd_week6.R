###############################################################################
# מעבדה בסטטיסטיקה – קבוצת BPD5 – שבוע 6
# אחוזוני BPD לכל שבוע בנפרד, ובחלון נע של שבעה ימים סביב כל יום.
#
# שלבי העבודה (סעיף בקוד לכל שלב, לפי הסדר):
#   0. הכנה: הגדרות, כלי עזר, טעינה ובדיקת הנתונים, חלוקה ל-5 קבוצות הריונות
#   1. גודל הקבוצות: מדידות, הריונות וערכים חוזרים בכל שבוע ובכל חלון
#   2. אחוזונים שבועיים בהנחת נורמליות (ממוצע ושונות של אותו שבוע)
#   3. אחוזונים שבועיים אמפיריים (בלי הנחה על ההתפלגות)
#   4. השוואת השיטות השבועיות: אחוז המדידות מתחת לסף
#   5. ערכים חוזרים: האם קיים סף שבדיוק P% מהמדידות מתחתיו, וכמה קרוב אפשר להגיע
#   6. אחוזונים יומיים בחלון של 3 ימים לפני ואחרי כל יום (kernel אחיד)
#   7. השוואת השיטות היומיות, וערכים חוזרים בחלונות
#   8. שבועי מול יומי: סיכום ארבע השיטות, ואחוז מתחת לסף לפי היום בשבוע
#   9. גרפים
#
# כל השוואה נעשית פעמיים: על כל קובץ האימון (in-sample), ועל הריונות מוסתרים
# (cross-validation לפי הריון, אותן 5 קבוצות כמו בשבועות 4–5).
# הרצה מתיקיית השורש: Rscript "week 6/bpd_week6.R". R בסיסי בלבד.
###############################################################################

## ======================== 0. הכנה ובדיקת נתונים ============================
TRAIN_PATH  <- file.path("week 3", "train.csv")
OUT_DIR     <- file.path("week 6", "output")
WEEK5_DIR   <- file.path("week 5", "output")
PERCENTILES <- c(0.03, 0.05, 0.10, 0.95, 0.975)   # אותם אחוזונים כמו בשבועות 4–5
PCT         <- 100 * PERCENTILES
WINDOW_DAYS <- 3                                   # היום עצמו ועוד 3 ימים לכל צד
K_FOLDS     <- 5
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

save_table <- function(df, name) {
  write.csv(df, file.path(OUT_DIR, paste0(name, ".csv")), row.names = FALSE)
}

# צבעים: משמעות קבועה כמו בשבועות 4–5. בשבוע הזה:
# כתום = השיטה הנורמלית, אפור-כחלחל = השיטה האמפירית (החלופה), אפור = הנתונים
# והיעד. קו מלא = חלון יומי, קו מקווקו = שבוע שלם.
COL_NEW  <- "#C2510F"
COL_OLD  <- "#0B6AA8"
COL_OLD2 <- "#009E73"
COL_ALT  <- "#4F606C"
COL_BULK <- "grey30"
alpha_col <- function(col, a) adjustcolor(col, alpha.f = a)

# עברית בגרפים: מכשיר הגרפים של R לא מסדר טקסט מימין לשמאל, ולכן heb() הופכת
# את המילים והאותיות לסדר החזותי. מספרים ושמות באנגלית נשארים כמו שהם.
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

# גרפים שנכנסים למצגת מקבלים PLOT_PT_SLIDE, כמו בשבוע 5
PLOT_PT_SLIDE <- 16
save_plot <- function(name, draw, width = 10, height = 6, pointsize = 12) {
  png(file.path(OUT_DIR, paste0(name, ".png")), width = width, height = height,
      units = "in", res = 150, pointsize = pointsize)
  tryCatch(draw(), finally = dev.off())
  invisible(NULL)
}
week_axis <- function() {
  axis(1, at = seq(14, 40, 2))
  axis(1, at = 14:40, labels = FALSE, tcl = -0.25)
}

# טעינה. קובץ הבדיקה לא נטען: הוא שמור לשלב ההשוואה הסופי.
dat <- read.csv(TRAIN_PATH, stringsAsFactors = FALSE)
required <- c("bpd", "week", "pregage_days", "preg_id")
missing_cols <- setdiff(required, names(dat))
if (length(missing_cols)) stop("חסרות עמודות: ", paste(missing_cols, collapse = ", "))
if (anyNA(dat[required])) stop("יש ערכים חסרים בעמודות הנדרשות")

WEEKS <- sort(unique(dat$week))                                   # 14–39
DAYS  <- seq(min(dat$pregage_days), max(dat$pregage_days))        # 98–280
if (!all(DAYS %in% dat$pregage_days)) stop("יש ימים בלי מדידות")
# היום בתוך השבוע: 0 = היום הראשון של השבוע. יום 280 הוא היום השמיני של שבוע 39.
dat$day_in_week <- dat$pregage_days - 7 * dat$week

n_preg <- length(unique(dat$preg_id))
data_check <- data.frame(
  check = c("measurements", "pregnancies", "measurements_per_pregnancy", "missing_values",
            "weeks", "days", "bpd_whole_mm_pct", "min_measurements_per_day",
            "median_measurements_per_day", "measurements_day_280"),
  value = c(nrow(dat), n_preg, round(nrow(dat) / n_preg, 2), 0,
            paste(range(WEEKS), collapse = "-"), paste(range(DAYS), collapse = "-"),
            round(100 * mean(dat$bpd == round(dat$bpd)), 1),
            min(table(dat$pregage_days)), median(table(dat$pregage_days)),
            sum(dat$pregage_days == 280)))
save_table(data_check, "00_data_check")
cat("Loaded", nrow(dat), "scans from", n_preg, "pregnancies |",
    data_check$value[data_check$check == "bpd_whole_mm_pct"], "% of BPD values are whole mm\n")

# חלוקה קבועה של ההריונות ל-5 קבוצות, זהה לשבועות 4–5 (אותו seed ואותה שיטה).
# כל המדידות של הריון נמצאות באותה קבוצה.
set.seed(2026)
ug <- unique(dat$preg_id)
fold <- sample(rep(seq_len(K_FOLDS), length.out = length(ug)))[match(dat$preg_id, ug)]

## ======================== עזרי חישוב (משותפים לכל השלבים) ===================
# ערכי BPD של כל קבוצה: לפי שבוע שלם, או בחלון של WINDOW_DAYS ימים לכל צד של יום
values_by_week   <- function(d) lapply(WEEKS, function(w) d$bpd[d$week == w])
values_by_window <- function(d) lapply(DAYS, function(c) d$bpd[abs(d$pregage_days - c) <= WINDOW_DAYS])

# ספים לכל קבוצה ולכל אחוזון, בשתי השיטות:
#   נורמלי:  ממוצע + qnorm(p) * סטיית תקן (שונות מדגמית, מכנה n-1)
#   אמפירי: הערך הקטן ביותר שלפחות p מהמדידות קטנות ממנו או שוות לו
#            (quantile type 1). זה תמיד ערך BPD שנמדד בפועל.
calc_thresholds <- function(values, ids) {
  do.call(rbind, lapply(seq_along(ids), function(i) {
    z <- values[[i]]
    data.frame(id = ids[i], percentile = PCT, n = length(z), mean = mean(z),
               variance = var(z), sd = sd(z),
               normal = mean(z) + qnorm(PERCENTILES) * sd(z),
               empirical = quantile(z, PERCENTILES, type = 1, names = FALSE))
  }))
}

# הסף שחל על כל מדידה: מטריצה עם שורה לכל מדידה ועמודה לכל אחוזון.
# obs_id הוא השבוע של המדידה, או היום שלה (מרכז החלון).
thr_matrix <- function(tab, method, obs_id) {
  sapply(PCT, function(p) {
    t <- tab[tab$percentile == p, ]
    t[[method]][match(obs_id, t$id)]
  })
}

# ספים להריונות מוסתרים: לכל קבוצת הריונות מחשבים את הספים מארבע הקבוצות
# האחרות, ומחילים אותם על ההריונות שהוסתרו. כך בודקים איך הסף עובד על הריון
# חדש, ולא על המדידות שמהן הוא חושב.
cv_thresholds <- function(values_fun, ids, obs_col) {
  out <- list(normal    = matrix(NA_real_, nrow(dat), length(PCT)),
              empirical = matrix(NA_real_, nrow(dat), length(PCT)))
  for (k in seq_len(K_FOLDS)) {
    tab <- calc_thresholds(values_fun(dat[fold != k, ]), ids)
    for (m in names(out)) out[[m]][fold == k, ] <- thr_matrix(tab, m, dat[[obs_col]][fold == k])
  }
  out
}

# אחוז המדידות מתחת לסף (<), שוות לו (=) ועד-וכולל (<=), לכל שבוע ולכל אחוזון
coverage_by_week <- function(thr, method, evaluation) {
  do.call(rbind, lapply(seq_along(PCT), function(j) {
    t <- thr[, j]
    data.frame(method = method, evaluation = evaluation, percentile = PCT[j], week = WEEKS,
               n = as.vector(table(dat$week)),
               below_pct = 100 * as.vector(tapply(dat$bpd < t, dat$week, mean)),
               equal_pct = 100 * as.vector(tapply(dat$bpd == t, dat$week, mean)),
               at_or_below_pct = 100 * as.vector(tapply(dat$bpd <= t, dat$week, mean)))
  }))
}

# סיכום לכל אחוזון: כל המדידות יחד, הסטייה השבועית מהיעד (ממוצעת ומקסימלית),
# וקנס האחוזונים מההרצאה, בדיוק כמו בשבועות 4–5:
# ממוצע על פני השבועות של |אחוז מתחת − p|, חלקי שורש p(1−p).
summarize_coverage <- function(thr, method, evaluation) {
  do.call(rbind, lapply(seq_along(PCT), function(j) {
    t <- thr[, j]; p <- PERCENTILES[j]
    wk <- tapply(dat$bpd < t, dat$week, mean)
    data.frame(method = method, evaluation = evaluation, percentile = PCT[j],
               below_pct = 100 * mean(dat$bpd < t), equal_pct = 100 * mean(dat$bpd == t),
               at_or_below_pct = 100 * mean(dat$bpd <= t),
               mean_abs_weekly_error_pp = 100 * mean(abs(wk - p)),
               max_abs_weekly_error_pp = 100 * max(abs(wk - p)),
               penalty = mean(abs(wk - p)) / sqrt(p * (1 - p)))
  }))
}

# האם קיים סף כלשהו שבדיוק p מהמדידות מתחתיו, וכמה קרוב אפשר להגיע?
# סף תמיד נופל בין שני ערכי BPD שנמדדו, ולכן אחוז המדידות מתחתיו יכול לקבל רק
# ערכים מסוימים: 0, ואחר כך הסכום המצטבר של מספר המדידות בכל ערך. כשערך אחד
# חוזר הרבה פעמים, יש "קפיצה" גדולה בין שני ערכים אפשריים סמוכים.
# האפשרות הטובה ביותר היא תמיד אחת משתיים: מתחת לערך שבו נופל האחוזון
# (= הסף האמפירי), או מעליו.
closest_reachable <- function(z, p) {
  n <- length(z)
  counts <- table(z)
  vals <- as.numeric(names(counts))
  reach <- c(0, cumsum(counts)) / n          # אחוז המדידות שקטנות מ-vals[i]
  i <- which.min(abs(reach - p))
  tie_value <- quantile(z, p, type = 1, names = FALSE)
  data.frame(n = n, target_n = n * p,
             target_n_whole = abs(n * p - round(n * p)) < 1e-9,
             tie_value = tie_value, tie_block_n = sum(z == tie_value),
             tie_block_pct = 100 * mean(z == tie_value),
             below_tie_pct = 100 * mean(z < tie_value),
             at_or_below_tie_pct = 100 * mean(z <= tie_value),
             best_threshold = c(vals, Inf)[i],            # "מתחת לערך הזה"
             best_reachable_pct = 100 * reach[i],
             best_gap_pp = 100 * abs(reach[i] - p),
             gap_without_ties_pp = 100 * abs(round(n * p) / n - p),
             exact_possible = abs(reach[i] - p) < 1e-9)
  }

# סיכום בדיקת הדיוק לכל אחוזון, על פני הקבוצות (שבועות או חלונות)
summarize_exactness <- function(tab, group_label) {
  do.call(rbind, lapply(PCT, function(p) {
    z <- tab[tab$percentile == p, ]
    data.frame(groups = group_label, percentile = p, n_groups = nrow(z),
               groups_target_n_whole = sum(z$target_n_whole),
               groups_exact_possible = sum(z$exact_possible),
               median_tie_block_pct = median(z$tie_block_pct),
               max_tie_block_pct = max(z$tie_block_pct),
               median_best_gap_pp = median(z$best_gap_pp),
               max_best_gap_pp = max(z$best_gap_pp),
               median_gap_without_ties_pp = median(z$gap_without_ties_pp))
  }))
}

## ======================== 1. גודל הקבוצות ===================================
# לפני שמפרשים אחוזון: כמה מדידות וכמה הריונות יש בכל קבוצה, וכמה ערכים שונים.
# top_value_pct = אחוז המדידות שנמצאות בערך ה-BPD השכיח ביותר בקבוצה.
group_sizes <- function(values, ids, preg_lists, label) {
  data.frame(group = label, id = ids, n = lengths(values),
             n_pregnancies = vapply(preg_lists, function(x) length(unique(x)), 0L),
             n_unique_bpd = vapply(values, function(z) length(unique(z)), 0L),
             top_value_pct = vapply(values, function(z) 100 * max(table(z)) / length(z), 0))
}
weekly_sizes <- group_sizes(values_by_week(dat), WEEKS,
                            lapply(WEEKS, function(w) dat$preg_id[dat$week == w]), "week")
save_table(weekly_sizes, "01_weekly_group_sizes")

window_sizes <- group_sizes(values_by_window(dat), DAYS,
  lapply(DAYS, function(c) dat$preg_id[abs(dat$pregage_days - c) <= WINDOW_DAYS]), "day")
# חלון מלא = כל שבעת הימים בתוך טווח הנתונים. בקצוות (98–100, 278–280) החלון חסר.
window_sizes$full_window <- DAYS - WINDOW_DAYS >= min(DAYS) & DAYS + WINDOW_DAYS <= max(DAYS)
save_table(window_sizes, "01_daily_window_group_sizes")

## ======================== 2. אחוזונים שבועיים – נורמליים ===================
# calc_thresholds מחשבת את שתי השיטות. כאן נשמרת השיטה הנורמלית.
weekly_tab <- calc_thresholds(values_by_week(dat), WEEKS)
names(weekly_tab)[names(weekly_tab) == "id"] <- "week"
save_table(weekly_tab[, c("week", "percentile", "n", "mean", "variance", "sd", "normal")],
           "02_weekly_normal")

## ======================== 3. אחוזונים שבועיים – אמפיריים ===================
weekly_emp_out <- weekly_tab[, c("week", "percentile", "n", "empirical", "normal")]
weekly_emp_out$empirical_minus_normal <- weekly_emp_out$empirical - weekly_emp_out$normal
save_table(weekly_emp_out, "03_weekly_empirical")

## ======================== 4. השוואת השיטות השבועיות =========================
# הסף של כל מדידה הוא הסף של השבוע שלה. בודקים פעמיים:
# (א) על כל קובץ האימון – אותן מדידות שמהן חושבו הספים.
# (ב) על הריונות מוסתרים – הספים חושבו בלי ההריון של המדידה.
names(weekly_tab)[names(weekly_tab) == "week"] <- "id"
thr_in <- list(week_normal    = thr_matrix(weekly_tab, "normal", dat$week),
               week_empirical = thr_matrix(weekly_tab, "empirical", dat$week))
cv_w <- cv_thresholds(values_by_week, WEEKS, "week")
thr_cv <- list(week_normal = cv_w$normal, week_empirical = cv_w$empirical)

weekly_cov <- rbind(
  do.call(rbind, lapply(names(thr_in), function(m) coverage_by_week(thr_in[[m]], m, "in_sample"))),
  do.call(rbind, lapply(names(thr_cv), function(m) coverage_by_week(thr_cv[[m]], m, "cv"))))
save_table(weekly_cov, "04_weekly_coverage_by_week")

weekly_summary <- rbind(
  do.call(rbind, lapply(names(thr_in), function(m) summarize_coverage(thr_in[[m]], m, "in_sample"))),
  do.call(rbind, lapply(names(thr_cv), function(m) summarize_coverage(thr_cv[[m]], m, "cv"))))
save_table(weekly_summary, "04_weekly_coverage_summary")
cat("\nWeekly methods, % below threshold:\n")
print(weekly_summary[, c("method", "evaluation", "percentile", "below_pct",
                         "at_or_below_pct", "mean_abs_weekly_error_pp")], digits = 3, row.names = FALSE)

## ======================== 5. ערכים חוזרים ודיוק – שבועות ====================
# לכל שבוע ואחוזון: באיזה ערך BPD נופל האחוזון, כמה מדידות יש בערך הזה, האם קיים
# סף שבדיוק P% מתחתיו, ומה האחוז הקרוב ביותר ליעד שאפשר להשיג.
# gap_without_ties_pp = הפער שהיה נשאר גם בלי ערכים חוזרים (רק כי n*P לא שלם).
weekly_vals <- values_by_week(dat)
weekly_exact <- do.call(rbind, lapply(seq_along(WEEKS), function(i)
  do.call(rbind, lapply(PERCENTILES, function(p)
    cbind(week = WEEKS[i], percentile = 100 * p, closest_reachable(weekly_vals[[i]], p))))))
save_table(weekly_exact, "05_weekly_ties_exactness")
weekly_exact_summary <- summarize_exactness(weekly_exact, "week")
save_table(weekly_exact_summary, "05_weekly_ties_summary")
cat("\nWeekly ties: how close can any threshold get?\n")
print(weekly_exact_summary, digits = 3, row.names = FALSE)

## ======================== 6. אחוזונים יומיים בחלון +/- 3 ימים ===============
# לכל יום d: כל המדידות שגילן בין d−3 ל-d+3 (שבעה ימים, משקל שווה לכולן).
# החלונות של ימים סמוכים חופפים כמעט לגמרי, ולכן העקומה היומית חלקה יותר.
daily_tab <- calc_thresholds(values_by_window(dat), DAYS)
daily_out <- daily_tab
names(daily_out)[names(daily_out) == "id"] <- "day"
daily_out$full_window <- window_sizes$full_window[match(daily_out$day, DAYS)]
save_table(daily_out, "06_daily_window_thresholds")

## ======================== 7. השוואת השיטות היומיות =========================
# כל מדידה מקבלת את הסף של היום שלה (מרכז החלון = גיל ההריון שלה בימים).
# כך כל מדידה נספרת פעם אחת, וההשוואה לשיטה השבועית הוגנת.
thr_in$day_normal    <- thr_matrix(daily_tab, "normal", dat$pregage_days)
thr_in$day_empirical <- thr_matrix(daily_tab, "empirical", dat$pregage_days)
cv_d <- cv_thresholds(values_by_window, DAYS, "pregage_days")
thr_cv$day_normal    <- cv_d$normal
thr_cv$day_empirical <- cv_d$empirical

day_methods <- c("day_normal", "day_empirical")
daily_cov <- rbind(
  do.call(rbind, lapply(day_methods, function(m) coverage_by_week(thr_in[[m]], m, "in_sample"))),
  do.call(rbind, lapply(day_methods, function(m) coverage_by_week(thr_cv[[m]], m, "cv"))))
save_table(daily_cov, "07_daily_coverage_by_week")

# ערכים חוזרים בכל חלון: אותה בדיקה כמו בשבועות
window_vals <- values_by_window(dat)
daily_exact <- do.call(rbind, lapply(seq_along(DAYS), function(i)
  do.call(rbind, lapply(PERCENTILES, function(p)
    cbind(day = DAYS[i], percentile = 100 * p, closest_reachable(window_vals[[i]], p))))))
save_table(daily_exact, "07_daily_ties_exactness")
daily_exact_summary <- summarize_exactness(daily_exact, "day_window")
save_table(daily_exact_summary, "07_daily_ties_summary")

## ======================== 8. שבועי מול יומי ================================
# (א) סיכום ארבע השיטות, על כל קובץ האימון ועל הריונות מוסתרים
methods_summary <- rbind(
  do.call(rbind, lapply(names(thr_in), function(m) summarize_coverage(thr_in[[m]], m, "in_sample"))),
  do.call(rbind, lapply(names(thr_cv), function(m) summarize_coverage(thr_cv[[m]], m, "cv"))))
save_table(methods_summary, "08_methods_summary")

# קנס האחוזונים הכולל (ממוצע על חמשת האחוזונים), ולצידו מודל הספליין של שבוע 5
# כנקודת ייחוס. ערכי שבוע 5 נקראים מהפלטים שלו, לא מחושבים מחדש.
penalty <- aggregate(penalty ~ method + evaluation, data = methods_summary, FUN = mean)
# טעות התקן של הקנס, כמו בשבוע 5: סטיית התקן של הקנס בין 5 הקבוצות, חלקי שורש 5.
# רק להריונות מוסתרים, כי שם כל קבוצה היא בדיקה נפרדת.
penalty_on <- function(thr, idx) mean(sapply(seq_along(PCT), function(j) {
  p <- PERCENTILES[j]
  wk <- tapply((dat$bpd < thr[, j])[idx], dat$week[idx], mean)
  mean(abs(wk - p)) / sqrt(p * (1 - p))
}))
penalty$penalty_se <- NA_real_
for (m in names(thr_cv)) {
  per_fold <- sapply(seq_len(K_FOLDS), function(k) penalty_on(thr_cv[[m]], fold == k))
  penalty$penalty_se[penalty$method == m & penalty$evaluation == "cv"] <- sd(per_fold) / sqrt(K_FOLDS)
}
w5_cv <- read.csv(file.path(WEEK5_DIR, "05_sd_candidates.csv"))
w5_in <- read.csv(file.path(WEEK5_DIR, "07_penalty_all.csv"))
penalty <- rbind(penalty, data.frame(
  method = "week5_spline_reference", evaluation = c("in_sample", "cv"),
  penalty = c(w5_in$penalty_all_data[w5_in$model == "spline"], w5_cv$penalty[w5_cv$chosen]),
  penalty_se = c(NA, w5_cv$penalty_se[w5_cv$chosen])))
penalty <- penalty[order(penalty$evaluation, penalty$penalty), ]
save_table(penalty, "08_penalty")
cat("\nPercentile penalty (lower = better):\n")
print(penalty, digits = 3, row.names = FALSE)

# (ב) אחוז מתחת לסף לפי היום בתוך השבוע (0 = היום הראשון). אם החלוקה לשבועות
# מטה את הספים, נראה שבתחילת השבוע יותר מדידות מתחת לסף ובסופו פחות.
# יום 280 (היום השמיני של שבוע 39) לא נכלל בטבלה הזו בלבד.
in_dw <- dat$day_in_week <= 6
by_day_in_week <- function(thr_list, evaluation) {
  do.call(rbind, lapply(names(thr_list), function(m)
    do.call(rbind, lapply(seq_along(PCT), function(j) {
      below <- (dat$bpd < thr_list[[m]][, j])[in_dw]
      data.frame(method = m, evaluation = evaluation, percentile = PCT[j], day_in_week = 0:6,
                 n = as.vector(table(dat$day_in_week[in_dw])),
                 below_pct = 100 * as.vector(tapply(below, dat$day_in_week[in_dw], mean)))
    }))))
}
dw <- rbind(by_day_in_week(thr_in, "in_sample"), by_day_in_week(thr_cv, "cv"))
save_table(dw, "08_coverage_by_day_in_week")
# סיכום: הפער בין היום הראשון לאחרון בשבוע, לכל שיטה ואחוזון
dw_summary <- do.call(rbind, lapply(split(dw, list(dw$method, dw$evaluation, dw$percentile)), function(z)
  data.frame(method = z$method[1], evaluation = z$evaluation[1], percentile = z$percentile[1],
             first_day_pct = z$below_pct[z$day_in_week == 0],
             last_day_pct = z$below_pct[z$day_in_week == 6],
             first_minus_last_pp = z$below_pct[z$day_in_week == 0] - z$below_pct[z$day_in_week == 6])))
dw_summary <- dw_summary[order(dw_summary$evaluation, dw_summary$percentile, dw_summary$method), ]
save_table(dw_summary, "08_day_in_week_summary")
cat("\nFirst vs last day of the week, % below threshold (cv):\n")
print(dw_summary[dw_summary$evaluation == "cv", ], digits = 3, row.names = FALSE)

## ======================== 9. גרפים ==========================================
names(weekly_tab)[names(weekly_tab) == "id"] <- "week"
METHOD_COL <- c(week_normal = COL_NEW, day_normal = COL_NEW,
                week_empirical = COL_ALT, day_empirical = COL_ALT)
METHOD_LTY <- c(week_normal = 2, day_normal = 1, week_empirical = 2, day_empirical = 1)
METHOD_LAB <- c(week_normal = "שבועי נורמלי", week_empirical = "שבועי אמפירי",
                day_normal = "חלון יומי נורמלי", day_empirical = "חלון יומי אמפירי")

# 9א. האחוזונים השבועיים בשתי השיטות
save_plot("09_weekly_percentiles", function() {
  par(mar = c(4.2, 4.2, 3, 4))
  plot(NA, xlim = c(14, 39), ylim = range(weekly_tab[, c("normal", "empirical")]), xaxt = "n",
       las = 1, xlab = heb("שבוע הריון"), ylab = heb("BPD במ\"מ"),
       main = heb("אחוזוני BPD לכל שבוע בנפרד"))
  week_axis()
  abline(v = seq(14, 40, 2), h = pretty(weekly_tab$normal), col = "grey92")
  for (p in PCT) {
    z <- weekly_tab[weekly_tab$percentile == p, ]
    lines(z$week, z$normal, col = COL_NEW, lwd = 2)
    points(z$week, z$empirical, col = COL_ALT, pch = 16, cex = 0.7)
  }
  # תוויות האחוזונים בצד ימין, מורחקות זו מזו כדי שלא יעלו זו על זו
  y_end <- weekly_tab$normal[weekly_tab$week == max(WEEKS)]
  y_lab <- c(y_end[2] + c(-3.8, 0, 3.8), y_end[4] - 1.2, y_end[5] + 2.6)
  mtext(paste0("P", PCT), side = 4, at = y_lab, las = 1, line = 0.4, cex = 0.75)
  legend("topleft", bty = "n", col = c(COL_NEW, COL_ALT), lwd = c(2, NA), pch = c(NA, 16),
         legend = heb(c("נורמלי: ממוצע ושונות של השבוע", "אמפירי: ערך שנמדד בפועל")))
}, pointsize = PLOT_PT_SLIDE)

# 9ב. אחוז המדידות מתחת לסף, לכל שבוע, על כל קובץ האימון. בשיטה האמפירית
# הקו האנכי מראה את הערכים החוזרים: מתחתיתו (<) ועד ראשו (<=) כולן מדידות
# ששוות בדיוק לסף. אם הקו האפור עובר דרכו, שום סף לא יפגע בדיוק ביעד.
save_plot("10_weekly_coverage_ties", function() {
  op <- par(mfrow = c(2, 3), mar = c(4.2, 4.2, 3, 1)); on.exit(par(op))
  for (p in PCT) {
    n <- weekly_cov[weekly_cov$percentile == p & weekly_cov$evaluation == "in_sample" &
                    weekly_cov$method == "week_normal", ]
    e <- weekly_cov[weekly_cov$percentile == p & weekly_cov$evaluation == "in_sample" &
                    weekly_cov$method == "week_empirical", ]
    plot(NA, xlim = c(14, 39), ylim = range(c(n$below_pct, e$below_pct, e$at_or_below_pct)),
         xaxt = "n", las = 1, xlab = heb("שבוע הריון"), ylab = heb("אחוז מתחת לסף"),
         main = paste0("P", p))
    week_axis()
    abline(h = p, col = "grey40", lty = 2)
    segments(e$week, e$below_pct, e$week, e$at_or_below_pct, col = alpha_col(COL_ALT, 0.7), lwd = 3)
    points(e$week, e$below_pct, col = COL_ALT, pch = 16, cex = 0.7)
    lines(n$week, n$below_pct, col = COL_NEW, lwd = 2, type = "o", pch = 16, cex = 0.6)
  }
  plot.new()
  legend("center", bty = "n", col = c(COL_NEW, COL_ALT, alpha_col(COL_ALT, 0.7), "grey40"),
         lwd = c(2, NA, 3, 1), lty = c(1, NA, 1, 2), pch = c(16, 16, NA, NA),
         legend = heb(c("נורמלי", "אמפירי: מתחת לסף", "אמפירי: מדידות ששוות לסף", "היעד")))
  text(0.5, 0.1, heb("על כל קובץ האימון"), col = "grey30")
}, width = 12, height = 7.5)

# 9ג. אותו דבר על הריונות מוסתרים, ארבע השיטות
save_plot("11_cv_coverage_by_week", function() {
  op <- par(mfrow = c(2, 3), mar = c(4.2, 4.2, 3, 1)); on.exit(par(op))
  all_cov <- rbind(weekly_cov, daily_cov)
  all_cov <- all_cov[all_cov$evaluation == "cv", ]
  for (p in PCT) {
    z <- all_cov[all_cov$percentile == p, ]
    plot(NA, xlim = c(14, 39), ylim = range(c(z$below_pct, p)), xaxt = "n", las = 1,
         xlab = heb("שבוע הריון"), ylab = heb("אחוז מתחת לסף"), main = paste0("P", p))
    week_axis()
    abline(h = p, col = "grey40", lty = 2)
    for (m in names(METHOD_COL)) {
      s <- z[z$method == m, ]
      lines(s$week, s$below_pct, col = METHOD_COL[m], lty = METHOD_LTY[m], lwd = 2)
    }
  }
  plot.new()
  legend("center", bty = "n", col = c(METHOD_COL, "grey40"), lty = c(METHOD_LTY, 2), lwd = c(rep(2, 4), 1),
         legend = c(heb(METHOD_LAB[names(METHOD_COL)]), heb("היעד")))
  text(0.5, 0.1, heb("על הריונות מוסתרים"), col = "grey30")
}, width = 12, height = 7.5)

# 9ד. לפי היום בתוך השבוע, הריונות מוסתרים: האם השיטה השבועית מטה את הספים?
save_plot("12_coverage_by_day_in_week", function() {
  op <- par(mfrow = c(2, 3), mar = c(4.2, 4.2, 3, 1)); on.exit(par(op))
  z_all <- dw[dw$evaluation == "cv", ]
  for (p in PCT) {
    z <- z_all[z_all$percentile == p, ]
    plot(NA, xlim = c(0, 6), ylim = range(c(z$below_pct, p)), las = 1,
         xlab = heb("היום בתוך השבוע"), ylab = heb("אחוז מתחת לסף"), main = paste0("P", p))
    abline(h = p, col = "grey40", lty = 2)
    for (m in names(METHOD_COL)) {
      s <- z[z$method == m, ]
      lines(s$day_in_week, s$below_pct, col = METHOD_COL[m], lty = METHOD_LTY[m], lwd = 2,
            type = "o", pch = 16, cex = 0.6)
    }
  }
  plot.new()
  legend("center", bty = "n", col = c(METHOD_COL, "grey40"), lty = c(METHOD_LTY, 2), lwd = c(rep(2, 4), 1),
         legend = c(heb(METHOD_LAB[names(METHOD_COL)]), heb("היעד")))
  text(0.5, 0.1, heb("0 הוא היום הראשון בשבוע"), col = "grey30")
}, width = 12, height = 7.5, pointsize = PLOT_PT_SLIDE * 0.85)

# 9ה. זום על שבועות 20–24: הסף השבועי קבוע לאורך השבוע וקופץ בין שבועות,
# והסף היומי עוקב אחרי הגדילה בתוך השבוע. שיטה נורמלית בשניהם.
save_plot("13_daily_vs_weekly_zoom", function() {
  zoom_days <- 140:174                          # שבועות 20–24
  z <- dat[dat$pregage_days %in% zoom_days, ]
  set.seed(1)                                   # לפיזור הנקודות בגרף בלבד
  dn <- daily_tab[daily_tab$id %in% zoom_days, ]
  plot(NA, xlim = range(zoom_days) / 7 + c(0, 1 / 7), ylim = range(dn$normal) + c(-2, 2),
       las = 1, xlab = heb("גיל הריון בשבועות"), ylab = heb("BPD במ\"מ"),
       main = heb("סף שבועי מול סף יומי: שבועות 20 עד 24"))
  points(z$pregage_days / 7 + runif(nrow(z), -0.06, 0.06), z$bpd + runif(nrow(z), -0.3, 0.3),
         pch = 16, cex = 0.3, col = alpha_col("grey40", 0.15))
  for (p in PCT) {
    d <- dn[dn$percentile == p, ]
    lines(d$id / 7, d$normal, col = COL_NEW, lwd = 2.2)
    w <- weekly_tab[weekly_tab$percentile == p & weekly_tab$week %in% 20:24, ]
    segments(w$week, w$normal, w$week + 1, w$normal, col = COL_NEW, lwd = 2.2, lty = 2)
  }
  legend("topleft", bty = "n", col = c(COL_NEW, COL_NEW, "grey40"), lwd = c(2.2, 2.2, NA),
         lty = c(1, 2, NA), pch = c(NA, NA, 16),
         legend = heb(c("חלון יומי נורמלי", "שבועי נורמלי", "מדידות")))
}, pointsize = PLOT_PT_SLIDE)

cat("\nDone. Outputs in", OUT_DIR, "\n")
