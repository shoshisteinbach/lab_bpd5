###############################################################################
# מעבדה בסטטיסטיקה – קבוצת BPD5 – שבוע 6
# אחוזוני BPD לפי שבוע בנפרד, ובחלונות יומיים של +/- 3 ימים.
#
# שלבי העבודה:
#   0. הכנה ובדיקת נתונים
#   1. גודל הקבוצות
#   2. אחוזונים שבועיים בהנחת נורמליות
#   3. אחוזונים שבועיים אמפיריים
#   4. כיסוי והשוואת השיטות השבועיות
#   5. קשרים ויכולת להגיע בדיוק לאחוזון היעד
#   6. אחוזונים יומיים בחלון +/- 3 ימים
#   7. כיסוי וקשרים בחלונות היומיים
#   8. גרפים וסיכום
#
# הרצה מתיקיית השורש: Rscript "week 6/bpd_week6.R". R בסיסי בלבד.
###############################################################################

## ======================== 0. הכנה ובדיקת נתונים ============================
TRAIN_PATH <- file.path("week 3", "train.csv")
OUT_DIR <- file.path("week 6", "output")
P <- c(0.03, 0.05, 0.10, 0.90, 0.95, 0.975)
WINDOW_DAYS <- 3L
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

save_table <- function(x, name) {
  write.csv(x, file.path(OUT_DIR, paste0(name, ".csv")), row.names = FALSE)
}

# אותם צבעים וכלי גרפים כמו בשבועות 4–5.
COL_NEW  <- "#C2510F"
COL_OLD  <- "#0B6AA8"
COL_OLD2 <- "#009E73"
COL_ALT  <- "#4F606C"
COL_BULK <- "grey30"
alpha_col <- function(col, a) adjustcolor(col, alpha.f = a)

# מכשיר הגרפים של R אינו מסדר עברית מימין לשמאל; heb() מסדרת טקסט לתצוגה.
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
soft_grid <- function(y_at) abline(v = seq(14, 40, 2), h = y_at, col = "grey92")

if (!file.exists(TRAIN_PATH)) stop("לא נמצא קובץ האימון: ", TRAIN_PATH)
dat <- read.csv(TRAIN_PATH, stringsAsFactors = FALSE)
required <- c("bpd", "week", "pregage_days", "preg_id")
missing_cols <- setdiff(required, names(dat))
if (length(missing_cols)) stop("חסרות עמודות: ", paste(missing_cols, collapse = ", "))
dat$bpd <- suppressWarnings(as.numeric(dat$bpd))
dat$week <- suppressWarnings(as.integer(dat$week))
dat$pregage_days <- suppressWarnings(as.numeric(dat$pregage_days))
usable <- is.finite(dat$bpd) & is.finite(dat$week) & is.finite(dat$pregage_days)
data_check <- data.frame(
  input_rows = nrow(dat), usable_rows = sum(usable), excluded_rows = sum(!usable),
  missing_bpd = sum(!is.finite(dat$bpd)), missing_week = sum(!is.finite(dat$week)),
  missing_day = sum(!is.finite(dat$pregage_days)),
  weeks_min = min(dat$week[usable]), weeks_max = max(dat$week[usable]),
  day_min = min(dat$pregage_days[usable]), day_max = max(dat$pregage_days[usable]),
  unique_pregnancies = length(unique(dat$preg_id[usable]))
)
save_table(data_check, "00_data_check")
dat <- dat[usable, , drop = FALSE]
cat("Loaded", nrow(dat), "usable scans from", length(unique(dat$preg_id)), "pregnancies\n")

## ======================== עזרי חישוב ========================================
fmt_p <- function(p) {
  pct <- 100 * p
  label <- vapply(pct, function(value) {
    if (abs(value - round(value)) < 1e-10) as.character(round(value))
    else format(value, trim = TRUE, scientific = FALSE)
  }, "")
  paste0("P", label)
}
summarize_groups <- function(x, group, group_name) {
  ids <- sort(unique(group))
  do.call(rbind, lapply(ids, function(g) {
    z <- x[group == g]
    data.frame(group = g, n = length(z), n_unique = length(unique(z)),
               mean = mean(z), variance = if (length(z) > 1) var(z) else NA_real_,
               sd = if (length(z) > 1) sd(z) else NA_real_,
               min = min(z), max = max(z), stringsAsFactors = FALSE)
  }))
}
make_thresholds <- function(groups, values, pvals, group_col, method) {
  out <- lapply(seq_along(groups), function(i) {
    z <- values[[i]]
    if (!length(z)) return(NULL)
    mu <- mean(z); s <- if (length(z) > 1) sd(z) else NA_real_
    data.frame(group = groups[i], percentile = pvals, p = pvals,
      threshold = if (method == "normal") mu + qnorm(pvals) * s else
        as.numeric(quantile(z, probs = pvals, type = 1, names = FALSE)),
      n = length(z), mean = mu, variance = if (length(z) > 1) var(z) else NA_real_,
      sd = s, method = method,
      group_type = group_col, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}
measure_coverage <- function(thresholds, group_values, group_ids) {
  out <- lapply(seq_len(nrow(thresholds)), function(i) {
    r <- thresholds[i, ]
    z <- group_values[[match(r$group, group_ids)]]
    if (is.null(z) || !length(z)) return(NULL)
    t <- r$threshold
    data.frame(group = r$group, group_type = r$group_type,
      method = r$method, percentile = r$percentile, target_pct = 100 * r$p,
      n = length(z), threshold = t,
      below_n = sum(z < t), equal_n = sum(z == t), at_or_below_n = sum(z <= t),
      below_pct = 100 * mean(z < t), equal_pct = 100 * mean(z == t),
      at_or_below_pct = 100 * mean(z <= t),
      exact_below = abs(mean(z < t) - r$p) < 1e-12,
      target_bracketed = mean(z < t) <= r$p && mean(z <= t) >= r$p,
      stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}
exact_cutoff_check <- function(z, p) {
  n <- length(z)
  wanted <- n * p
  k <- round(wanted)
  integer_target <- abs(wanted - k) < 1e-10
  ordered <- sort(z)
  boundary_tied <- integer_target && k > 0 && k < n && ordered[k] == ordered[k + 1]
  possible <- integer_target && !boundary_tied
  example_threshold <- NA_real_
  if (possible) {
    if (k == 0) example_threshold <- ordered[1]
    else if (k == n) example_threshold <- max(ordered) + max(1, abs(max(ordered))) * 1e-10
    else example_threshold <- (ordered[k] + ordered[k + 1]) / 2
  }
  data.frame(target_below_n = wanted, nearest_integer_n = k,
             target_count_integer = integer_target, boundary_tied = boundary_tied,
             possible_exact_threshold = possible,
             example_exact_threshold = example_threshold)
}

## ======================== 1. גודל הקבוצות ===================================
weekly_ids <- sort(unique(dat$week))
weekly_values <- lapply(weekly_ids, function(w) dat$bpd[dat$week == w])
weekly_sizes <- summarize_groups(dat$bpd, dat$week, "week")
names(weekly_sizes)[names(weekly_sizes) == "group"] <- "week"
save_table(weekly_sizes, "00_group_sizes")

## ======================== 2. אחוזונים שבועיים – נורמליים ===================
weekly_normal <- make_thresholds(weekly_ids, weekly_values, P, "week", "normal")
names(weekly_normal)[names(weekly_normal) == "group"] <- "week"
weekly_normal_out <- weekly_normal
weekly_normal_out$group_type <- NULL
save_table(weekly_normal_out, "01_weekly_thresholds")

## ======================== 3. אחוזונים שבועיים – אמפיריים ===================
weekly_empirical <- make_thresholds(weekly_ids, weekly_values, P, "week", "empirical")
names(weekly_empirical)[names(weekly_empirical) == "group"] <- "week"
weekly_empirical_out <- weekly_empirical
weekly_empirical_out$group_type <- NULL
save_table(weekly_empirical_out, "02_weekly_empirical")

## ======================== 4. כיסוי והשוואה שבועית ===========================
# המדידה היא in-sample: אותם נתונים משמשים לאמידת הסף ולחישוב הכיסוי.
weekly_normal_cov <- measure_coverage(transform(weekly_normal, group = week),
                                     weekly_values, weekly_ids)
weekly_emp_cov <- measure_coverage(transform(weekly_empirical, group = week),
                                   weekly_values, weekly_ids)
weekly_cov <- rbind(weekly_normal_cov, weekly_emp_cov)
weekly_cov$week <- weekly_cov$group
weekly_cov_out <- weekly_cov
weekly_cov_out$group <- NULL
weekly_cov_out$group_type <- NULL
save_table(weekly_cov_out, "03_weekly_coverage")
weekly_comparison <- aggregate(cbind(below_n, equal_n, at_or_below_n, n) ~
  method + percentile, data = weekly_cov, FUN = sum)
weekly_comparison$target_pct <- 100 * weekly_comparison$percentile
weekly_comparison$below_pct <- 100 * weekly_comparison$below_n / weekly_comparison$n
weekly_comparison$equal_pct <- 100 * weekly_comparison$equal_n / weekly_comparison$n
weekly_comparison$at_or_below_pct <- 100 * weekly_comparison$at_or_below_n / weekly_comparison$n
weekly_abs_error <- aggregate(abs(below_pct - target_pct) ~ method + percentile,
  data = weekly_cov, FUN = mean)
names(weekly_abs_error)[3] <- "mean_abs_weekly_error_pp"
weekly_comparison <- merge(weekly_comparison, weekly_abs_error,
                           by = c("method", "percentile"), sort = FALSE)
weekly_exact_summary <- aggregate(exact_below ~ method + percentile,
  data = weekly_cov, FUN = sum)
weekly_comparison$weeks_exact_at_computed_threshold <- weekly_exact_summary$exact_below[
  match(paste(weekly_comparison$method, weekly_comparison$percentile),
        paste(weekly_exact_summary$method, weekly_exact_summary$percentile))]

## ======================== 5. קשרים ודיוק ====================================
# target_bracketed בודק אם P נמצא בין הכיסוי מתחת לסף לבין הכיסוי עד-כולל.
# אפשרות אמיתית לסף חד נבדקת בנפרד: n*P חייב להיות שלם, ובגבול אסור שיהיה tie.
weekly_exact <- weekly_cov[, c("week", "method", "percentile", "target_pct", "n",
  "threshold", "below_n", "equal_n", "at_or_below_n", "below_pct", "equal_pct",
  "at_or_below_pct", "exact_below", "target_bracketed")]
feasibility <- do.call(rbind, lapply(seq_len(nrow(weekly_exact)), function(i) {
  z <- dat$bpd[dat$week == weekly_exact$week[i]]
  exact_cutoff_check(z, weekly_exact$percentile[i])
}))
weekly_exact <- cbind(weekly_exact, feasibility)
save_table(weekly_exact, "05_ties_and_exactness")
weekly_feasible <- weekly_exact[!duplicated(weekly_exact[c("week", "percentile")]), ]
weekly_possible_summary <- aggregate(possible_exact_threshold ~ percentile,
  data = weekly_feasible, FUN = sum)
names(weekly_possible_summary)[2] <- "weeks_with_some_exact_threshold"
weekly_comparison <- merge(weekly_comparison, weekly_possible_summary,
                           by = "percentile", sort = FALSE)
save_table(weekly_comparison, "04_weekly_comparison")

## ======================== 6. אחוזונים יומיים בחלון +/- 3 ימים ===============
# משתמשים בכל הימים שבהם יש לפחות תצפית. לא משייכים לפי week בעיבוד הזה.
day_ids <- seq.int(floor(min(dat$pregage_days)), ceiling(max(dat$pregage_days)))
daily_values <- lapply(day_ids, function(d) {
  dat$bpd[abs(dat$pregage_days - d) <= WINDOW_DAYS]
})
daily_sizes <- data.frame(day = day_ids, n = lengths(daily_values),
                          n_unique = vapply(daily_values, function(z) length(unique(z)), 0L))
save_table(daily_sizes, "06_daily_window_group_sizes")
keep_day <- lengths(daily_values) > 0
day_ids_used <- day_ids[keep_day]
daily_values_used <- daily_values[keep_day]
daily_normal <- make_thresholds(day_ids_used, daily_values_used, P, "day", "normal")
daily_empirical <- make_thresholds(day_ids_used, daily_values_used, P, "day", "empirical")
daily_thresholds <- rbind(daily_normal, daily_empirical)
names(daily_thresholds)[names(daily_thresholds) == "group"] <- "day"
daily_thresholds_out <- daily_thresholds
daily_thresholds_out$group_type <- NULL
save_table(daily_thresholds_out, "06_daily_window_thresholds")

## ======================== 7. כיסוי וקשרים בחלונות ==========================
dn <- measure_coverage(daily_normal, daily_values_used, day_ids_used)
de <- measure_coverage(daily_empirical, daily_values_used, day_ids_used)
daily_cov <- rbind(dn, de)
daily_cov$day <- daily_cov$group
daily_cov$window_days <- WINDOW_DAYS
daily_cov_out <- daily_cov
daily_cov_out$group <- NULL
daily_cov_out$group_type <- NULL
save_table(daily_cov_out, "07_daily_window_coverage")
daily_exact <- daily_cov[, c("day", "window_days", "method", "percentile", "target_pct",
  "n", "threshold", "below_n", "equal_n", "at_or_below_n", "below_pct",
  "equal_pct", "at_or_below_pct", "exact_below", "target_bracketed")]
daily_feasibility <- do.call(rbind, lapply(seq_len(nrow(daily_exact)), function(i) {
  z <- daily_values_used[[match(daily_exact$day[i], day_ids_used)]]
  exact_cutoff_check(z, daily_exact$percentile[i])
}))
daily_exact <- cbind(daily_exact, daily_feasibility)
save_table(daily_exact, "08_daily_window_exactness")
daily_comparison <- aggregate(cbind(below_pct, equal_pct, at_or_below_pct, n,
  exact_below) ~ method + percentile, data = daily_cov, FUN = mean)
names(daily_comparison)[3:7] <- c("mean_below_pct", "mean_equal_pct",
  "mean_at_or_below_pct", "mean_window_n", "fraction_days_exact_at_computed_threshold")
daily_comparison$target_pct <- 100 * daily_comparison$percentile
daily_comparison$mean_abs_daily_error_pp <- vapply(seq_len(nrow(daily_comparison)), function(i) {
  z <- daily_cov[daily_cov$method == daily_comparison$method[i] &
                 daily_cov$percentile == daily_comparison$percentile[i], ]
  mean(abs(z$below_pct - z$target_pct))
}, 0.0)
daily_feasible_days <- daily_exact[!duplicated(daily_exact[c("day", "percentile")]), ]
possible_summary <- aggregate(possible_exact_threshold ~ percentile,
  data = daily_feasible_days, FUN = sum)
names(possible_summary)[2] <- "days_with_some_exact_threshold"
daily_comparison <- merge(daily_comparison, possible_summary, by = "percentile", sort = FALSE)
save_table(daily_comparison, "08_daily_window_comparison")

## ======================== 8. גרפים ==========================================
cols <- c("#C2510F", "#0B6AA8", "#009E73", "#4F606C", "#7A5195", "#EF5675")
save_plot("09_weekly_percentiles", function() {
  y <- range(c(weekly_normal$threshold, weekly_empirical$threshold), finite = TRUE)
  plot(NA, xlim = range(weekly_ids), ylim = y, xlab = heb("שבוע הריון"),
       ylab = heb("BPD (מ\"מ)"), main = heb("אחוזוני BPD לפי שבוע: נורמלי ואמפירי"),
       xaxt = "n")
  week_axis()
  soft_grid(pretty(y))
  box()
  for (j in seq_along(P)) {
    a <- weekly_normal[weekly_normal$p == P[j], ]; b <- weekly_empirical[weekly_empirical$p == P[j], ]
    lines(a$week, a$threshold, col = cols[j], lty = 1, lwd = 2.5)
    lines(b$week, b$threshold, col = cols[j], lty = 2, lwd = 2.5)
  }
  legend_labels <- as.vector(rbind(paste("נורמלי", fmt_p(P)), paste("אמפירי", fmt_p(P))))
  legend("topleft", legend = heb(legend_labels), col = rep(cols, each = 2),
         lty = rep(c(1, 2), length(P)), lwd = 2.5, cex = 0.8, bty = "n", ncol = 2)
})

save_plot("10_daily_window_percentiles", function() {
  day_weeks <- day_ids_used / 7
  y <- range(daily_thresholds$threshold, finite = TRUE)
  plot(NA, xlim = range(day_weeks), ylim = y,
       xlab = heb("גיל הריון (שבועות; מרכז החלון)"), ylab = heb("BPD (מ\"מ)"),
       main = heb("אחוזוני BPD מקומיים: חלון של שבעה ימים"))
  abline(h = pretty(y), col = "grey92")
  abline(v = seq(floor(min(day_weeks)), ceiling(max(day_weeks)), by = 2), col = "grey92")
  for (j in seq_along(P)) {
    a <- daily_normal[daily_normal$p == P[j], ]; b <- daily_empirical[daily_empirical$p == P[j], ]
    lines(a$group / 7, a$threshold, col = cols[j], lty = 1, lwd = 2.5)
    lines(b$group / 7, b$threshold, col = cols[j], lty = 2, lwd = 2.5)
  }
  legend_labels <- as.vector(rbind(paste("נורמלי", fmt_p(P)), paste("אמפירי", fmt_p(P))))
  legend("topleft", legend = heb(legend_labels), col = rep(cols, each = 2),
         lty = rep(c(1, 2), length(P)), lwd = 2.5, cex = 0.8, bty = "n", ncol = 2)
})

cat("\nFinished. Review output tables, especially group sizes and in-sample coverage.\n")
