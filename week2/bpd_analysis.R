###############################################################################
#  מעבדה בסטטיסטיקה - קבוצת BPD - שבוע 2
#  האם עקומות הייחוס הקיימות (הדלוק = Hadlock, ו-Kurmanavicius) מתאימות
#  לנתוני ה-BPD הישראליים?
#
#  השלבים:
#    1. קריאת הנתונים וספירת ערכים חסרים
#    2. הגדרת הריונות
#    3. הגבלה לשבוע 40 ותיאור המדגם (נשים, הריונות, מדידות)
#    4. תיקון יחידות מידה
#    5. הסרת מדידות חסרות וניקוי חריגים (שיטת שבוע 1)
#    6. עקומות הייחוס
#    7. ציון z ואחוזון לכל מדידה
#    8. הבדיקה הסטטיסטית: בדיקה אחת לכל הריון + מבחן בינומי לכל סף
#
#  R בסיסי בלבד - אין חבילות להתקין.
#  הטבלאות נשמרות כ-CSV והגרפים כ-PNG בתיקייה "bpd_output".
###############################################################################

## ======================== הגדרות ============================================

DATA_PATH  <- "rawdata.csv"
OUT_DIR    <- file.path(dirname(DATA_PATH), "bpd_output")

MAX_WEEK     <- 40           # הגבלה לגיל הריון <= 40 שבועות
NEW_PREG_GAP <- 100          # ימים בין תאריכי לידה משוערים שמגדירים הריון חדש
BPD_RANGE    <- c(10, 120)   # ניקוי, כלל 1: טווח BPD סביר במ"מ
ROBUST_Z_MAX <- 5            # ניקוי, כלל 2: z חסין (חציון/MAD) בתוך שבוע
ALPHA        <- 0.05

set.seed(2026)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

## ======================== פונקציות עזר ======================================

section <- function(title) {
  cat("\n", strrep("=", 78), "\n  ", title, "\n", strrep("=", 78), "\n", sep = "")
}
sub_section <- function(title) cat("\n--- ", title, " ---\n", sep = "")

# מצייר גרף על המסך (בהרצה אינטראקטיבית) ושומר אותו כ-PNG.
# אם חלון המסך קטן מדי, הגרף על המסך מדולג עם הודעה והקובץ נשמר בכל זאת.
make_plot <- function(name, draw, width = 10, height = 6) {
  file <- file.path(OUT_DIR, paste0(name, ".png"))
  png(file, width = width, height = height, units = "in", res = 150)
  tryCatch(draw(), finally = dev.off())

  if (interactive()) {
    ok <- tryCatch({ draw(); TRUE }, error = function(e) FALSE)
    if (!ok) {
      cat("  [Plot '", name, "' is too large for the Plots pane - enlarge the pane ",
          "or open the saved file: ", file, "]\n", sep = "")
      if (dev.cur() > 1) try(dev.off(), silent = TRUE)
    }
  }
  invisible(NULL)
}

save_table <- function(df, name) {
  write.csv(df, file.path(OUT_DIR, paste0(name, ".csv")), row.names = FALSE)
}

pct <- function(x, digits = 2) round(100 * x, digits)

# צבעים (מתאימים גם לעיוורי צבעים)
COL_HAD <- "#D55E00"   # Hadlock
COL_KUR <- "#0072B2"   # Kurmanavicius
alpha_col <- function(col, a) adjustcolor(col, alpha.f = a)

## ======================== 1. קריאה וערכים חסרים ==============================
section("1. Reading the data and missing values")

raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"))
cat("Rows:", nrow(raw), "  Columns:", ncol(raw), "\n")
str(raw)

sub_section("Missing values and zeros per variable (raw)")
missing_tbl <- data.frame(
  variable = names(raw),
  n_NA     = sapply(raw, function(x) sum(is.na(x))),
  pct_NA   = sapply(raw, function(x) pct(mean(is.na(x)))),
  n_zero   = sapply(raw, function(x) if (is.numeric(x)) sum(x == 0, na.rm = TRUE) else NA)
)
print(missing_tbl, row.names = FALSE)
save_table(missing_tbl, "01_missing_values_raw")

dat <- raw

## ======================== 2. הגדרת הריונות ===================================
section("2. Defining pregnancies")
# snumber מזהה את האם. preg_num חסר ברוב השורות, ולכן הריון מוגדר כך:
# אותה אם + תאריכי לידה משוערים במרחק של עד NEW_PREG_GAP ימים זה מזה
# (תאריך הלידה המשוער יכול להתעדכן מעט בין בדיקות של אותו הריון).
cat("preg_num missing in", pct(mean(is.na(dat$preg_num))), "% of rows -> not usable as an ID\n")

dat <- dat[order(dat$snumber, dat$expected, dat$check_d_sas), ]
new_preg <- c(TRUE, diff(dat$snumber) != 0 | diff(dat$expected) > NEW_PREG_GAP)
dat$preg_id <- cumsum(new_preg)

## ======================== 3. הגבלה לשבוע 40 ותיאור המדגם =====================
section(paste0("3. Restricting to GA <= ", MAX_WEEK, " weeks and describing the sample"))
in_range <- dat$pregweek <= MAX_WEEK
cat("Scans removed (GA > ", MAX_WEEK, "): ", sum(!in_range), "\n", sep = "")
dat <- dat[in_range, ]
dat$week <- pmin(floor(dat$pregweek), 39)     # שבוע שלם; יום 280 מצורף לשבוע 39

# מספר נשים, הריונות ומדידות, והתפלגות ההריונות לאישה והמדידות להריון
describe_sample <- function(df, label) {
  sub_section(paste("Sample description -", label))
  n_w <- length(unique(df$snumber)); n_p <- length(unique(df$preg_id))
  cat("Scans:", nrow(df), " | Women:", n_w, " | Pregnancies:", n_p, "\n")
  cat("Mean scans per pregnancy:", round(nrow(df) / n_p, 2),
      " | Mean pregnancies per woman:", round(n_p / n_w, 2), "\n")

  ppw <- tapply(df$preg_id, df$snumber, function(x) length(unique(x)))
  spp <- table(df$preg_id)
  t1 <- table(cut(ppw, c(0, 1, 2, 3, Inf), labels = c("1", "2", "3", "4+")))
  t2 <- table(cut(as.numeric(spp), c(0, 1, 2, 3, 4, 5, Inf), labels = c("1", "2", "3", "4", "5", "6+")))
  cat("\nPregnancies per woman:\n");  print(rbind(n = t1, pct = pct(prop.table(t1), 1)))
  cat("\nScans per pregnancy:\n");    print(rbind(n = t2, pct = pct(prop.table(t2), 1)))
  invisible(list(women = n_w, pregnancies = n_p, scans = nrow(df)))
}
desc_restricted <- describe_sample(dat, "after restricting to week 40")

## ======================== 4. יחידות מידה =====================================
section("4. Units of measurement")
# רוב הנתונים הם במ"מ (BPD בערך 25-100 מ"מ בין שבוע 14 ל-40).
# יש פערים ברורים בין קבוצות היחידות:
#   bpd < 15         -> נרשם בס"מ                  (x 10)
#   150 <= bpd < 1500 -> חסרה נקודה עשרונית        (/ 10)   למשל 924 -> 92.4
#   bpd >= 1500       -> חסרות שתי ספרות עשרוניות  (/ 100)  למשל 5152 -> 51.52
bpd_factor <- with(dat, ifelse(is.na(bpd), NA,
                        ifelse(bpd < 15, 10,
                        ifelse(bpd >= 1500, 0.01,
                        ifelse(bpd >= 150, 0.1, 1)))))

dat$bpd_raw <- dat$bpd
dat$bpd     <- dat$bpd_raw * bpd_factor

sub_section("BPD unit correction factor applied")
print(table(factor = bpd_factor, useNA = "ifany"))
save_table(as.data.frame(table(factor = bpd_factor, useNA = "ifany")), "02_unit_corrections")

unit_group <- factor(bpd_factor, levels = c(1, 10, 0.1, 0.01),
                     labels = c("mm (no change)", "cm (x10)", "missing decimal (/10)", "x100 (/100)"))
make_plot("01_bpd_raw_units", function() {
  cols <- c(alpha_col("grey30", 0.15), COL_HAD, COL_KUR, "purple")
  ord  <- order(unit_group != "mm (no change)")
  plot(dat$pregweek[ord], dat$bpd_raw[ord], log = "y", pch = 16, cex = 0.4,
       col = cols[as.integer(unit_group[ord])], ylim = c(1, 60000),
       xlab = "Gestational age (weeks)", ylab = "BPD as recorded (log scale)",
       main = "Raw BPD values: several units are mixed in the file")
  legend("topleft", legend = levels(unit_group), col = c("grey30", COL_HAD, COL_KUR, "purple"),
         pch = 16, bty = "n", title = "Detected unit", ncol = 2, cex = 0.85)
})

## ======================== 5. מדידות חסרות וניקוי חריגים ======================
section("5. Missing BPD and cleaning recording errors (week-1 method)")
cat("Scans with missing BPD are removed (no imputation):", sum(is.na(dat$bpd)), "\n")
dat <- dat[!is.na(dat$bpd), ]
n_before_cleaning <- nrow(dat)

# אנחנו לא מנקים ביחס לעקומות Hadlock/Kurmanavicius - זה היה מניח את מה שאנחנו
# רוצים לבדוק, והיה גוזם בדיוק את הזנבות שאנחנו חוקרים.
# שיטת הניקוי היא זו של שבוע 1 (week1/02_clean.R), בשני כללים:
#   כלל 1 - הסרת ערכים מחוץ לטווח הביולוגי BPD_RANGE (10-120 מ"מ)
#   כלל 2 - הסרת ערכים בודדים שרחוקים מאוד מעמיתיהם באותו שבוע:
#           z חסין = (bpd - חציון השבוע) / (1.4826 * MAD של השבוע), |z| > 5
#           החציון וה-MAD לא מושפעים מהחריגים עצמם, וסף 5 שמרני מאוד,
#           כך שרק שגיאות ברורות מוסרות.
# הכללים מופעלים אחרי תיקון היחידות (סעיף 4).

# כלל 1: טווח ביולוגי
rule1 <- dat$bpd < BPD_RANGE[1] | dat$bpd > BPD_RANGE[2]

# כלל 2: z חסין בתוך כל שבוע (החציון וה-MAD מחושבים רק על מה שעבר את כלל 1)
ok1   <- dat[!rule1, ]
med_w <- tapply(ok1$bpd, ok1$week, median)
mad_w <- tapply(ok1$bpd, ok1$week, mad)          # mad() כבר כולל את המקדם 1.4826
rz    <- (dat$bpd - med_w[as.character(dat$week)]) / mad_w[as.character(dat$week)]
rule2 <- !rule1 & !is.na(rz) & abs(rz) > ROBUST_Z_MAX

flag_any <- rule1 | rule2

clean_tbl <- data.frame(
  rule = c(paste0("Rule 1: BPD outside ", BPD_RANGE[1], "-", BPD_RANGE[2], " mm"),
           paste0("Rule 2: |robust z| > ", ROBUST_Z_MAX, " within week"),
           "Removed by at least one rule"),
  n   = c(sum(rule1), sum(rule2), sum(flag_any)),
  pct = pct(c(mean(rule1), mean(rule2), mean(flag_any)), 3)
)
print(clean_tbl, row.names = FALSE)
save_table(clean_tbl, "03_cleaning_rules")

make_plot("02_cleaning", function() {
  reason <- ifelse(rule1, 1, ifelse(rule2, 2, 0))
  plot(dat$pregweek[reason == 0], dat$bpd[reason == 0], pch = 16, cex = 0.35,
       col = alpha_col("grey30", 0.12), ylim = range(c(0, dat$bpd), na.rm = TRUE),
       xlab = "Gestational age (weeks)", ylab = "BPD (mm), units corrected",
       main = "Scans removed as recording errors")
  cols <- c(COL_HAD, COL_KUR)
  for (k in 1:2) points(dat$pregweek[reason == k], dat$bpd[reason == k], pch = 16, cex = 0.7, col = cols[k])
  legend("topleft", bty = "n", pch = 16, col = cols,
         legend = c("rule 1: outside 10-120 mm", "rule 2: robust z > 5"))
})

dat <- dat[!flag_any, ]
desc_final <- describe_sample(dat, "FINAL analysis file")

flow_tbl <- data.frame(
  step  = c("Raw file", "After restricting to week 40", "After removing missing BPD",
            "Final (after cleaning)"),
  scans = c(nrow(raw), desc_restricted$scans, n_before_cleaning, nrow(dat)))
sub_section("Data flow"); print(flow_tbl, row.names = FALSE)
save_table(flow_tbl, "04_data_flow")

## ======================== 6. עקומות הייחוס ===================================
section("6. The existing reference curves")
# המודל של כל עקומת ייחוס:  BPD | w  ~  N( mean(w), sd(w)^2 )
# האחוזון p:                q_w(p) = mean(w) + sd(w) * qnorm(p)
#   (qnorm = ההופכית של פונקציית ההתפלגות הנורמלית; במצגות כתוב pnorm,
#    אבל פונקציית הקוונטילים היא qnorm)
kur_mean <- function(w) -28.04 + 4.179 * w - 0.0006414 * w^3      # מ"מ
kur_sd   <- function(w) 1.648 + 0.06531 * w                          # מ"מ
had_mean <- function(w) 10 * (-3.08 + 0.41 * w - 0.000061 * w^3)    # ס"מ -> מ"מ
had_sd   <- function(w) rep(3.0, length(w))                          # 0.30 ס"מ = 3 מ"מ

wk <- seq(16, 40, by = 4)
ref_tbl <- data.frame(week = wk,
                      hadlock_mean = round(had_mean(wk), 1), hadlock_sd = had_sd(wk),
                      kurman_mean = round(kur_mean(wk), 1),  kurman_sd = round(kur_sd(wk), 2),
                      data_median = round(sapply(wk, function(k) median(dat$bpd[abs(dat$pregweek - k) <= 0.5])), 1))
# (data_median = חציון ה-BPD של הבדיקות בטווח של +-0.5 שבוע מאותו שבוע)
print(ref_tbl, row.names = FALSE)
save_table(ref_tbl, "05_reference_curves")

make_plot("03_data_vs_reference_curves", function() {
  ww <- seq(14, MAX_WEEK, by = 0.1)
  idx <- sample(nrow(dat), min(25000, nrow(dat)))
  plot(dat$pregweek[idx], dat$bpd[idx], pch = 16, cex = 0.3, col = alpha_col("grey30", 0.12),
       xlab = "Gestational age (weeks)", ylab = "BPD (mm)",
       main = "Israeli data vs. existing curves (3rd, 50th, 97.5th percentiles)")
  for (p in c(0.03, 0.5, 0.975)) {
    lty <- if (p == 0.5) 1 else 2
    lines(ww, had_mean(ww) + had_sd(ww) * qnorm(p), col = COL_HAD, lwd = 2, lty = lty)
    lines(ww, kur_mean(ww) + kur_sd(ww) * qnorm(p), col = COL_KUR, lwd = 2, lty = lty)
  }
  legend("topleft", bty = "n", lwd = 2, col = c(COL_HAD, COL_KUR, "black", "black"), lty = c(1, 1, 1, 2),
         legend = c("Hadlock", "Kurmanavicius", "50th percentile", "3rd / 97.5th percentile"))
})

## ======================== 7. ציון z ואחוזון לכל מדידה ========================
section("7. Standardising: z-scores and percentiles")
# אם העקומה נכונה:  z = (bpd - mean(w)) / sd(w) ~ N(0,1)  בכל שבוע,
# ולכן אפשר לאחד את כל הבדיקות, ו-  u = pnorm(z) ~ Uniform(0,1).
# "bpd מתחת לעקומת האחוזון ה-p"  <=>  u < p.
methods <- c(Hadlock = "had", Kurmanavicius = "kur")
dat$z_had <- (dat$bpd - had_mean(dat$pregweek)) / had_sd(dat$pregweek)
dat$z_kur <- (dat$bpd - kur_mean(dat$pregweek)) / kur_sd(dat$pregweek)
dat$u_had <- pnorm(dat$z_had)
dat$u_kur <- pnorm(dat$z_kur)

# ספי הסיווג: שלושה של "קטן" ושניים של "גדול"
thr <- data.frame(label = c("< 3%", "< 5%", "< 10%", "> 95%", "> 97.5%"),
                  cut   = c(0.03, 0.05, 0.10, 0.95, 0.975),
                  side  = c("low", "low", "low", "high", "high"))
thr$nominal <- ifelse(thr$side == "low", thr$cut, 1 - thr$cut)   # השיעור הצפוי של חריגים

## ======================== 8. הבדיקה הסטטיסטית ================================
section("8. Test for each method and threshold")
# השאלה: אם העקומה מתאימה, אחוז המדידות החריגות בכל סף שווה לאחוז הנקוב.
#   H0: P(מדידה מסווגת כחריגה) = השיעור הנקוב   H1: השיעור שונה
#
# בדיקות של אותו הריון אינן בלתי תלויות, ולכן המבחן הבינומי נעשה על
# בדיקה אחת שנבחרה אקראית מכל הריון.
one_per_preg <- function(df) {
  idx <- split(seq_len(nrow(df)), df$preg_id)
  unname(vapply(idx, function(i) if (length(i) == 1) i else sample(i, 1), integer(1)))
}
set.seed(2026)
d1 <- dat[one_per_preg(dat), ]
n  <- nrow(d1)
cat("Independent sample: one random scan per pregnancy, n =", n, "\n")

rows <- list()
for (m in names(methods)) {
  u <- d1[[paste0("u_", methods[m])]]
  for (i in seq_len(nrow(thr))) {
    flagged <- if (thr$side[i] == "low") u < thr$cut[i] else u > thr$cut[i]
    x  <- sum(flagged)
    bt <- binom.test(x, n, thr$nominal[i])       # מבחן בינומי מדויק, עם רווח סמך 95%
    rows[[length(rows) + 1]] <- data.frame(
      method = m, threshold = thr$label[i],
      nominal_pct = pct(thr$nominal[i]), n_flagged = x, n = n,
      observed_pct = pct(x / n),
      ci95_low = pct(bt$conf.int[1]), ci95_high = pct(bt$conf.int[2]),
      ratio_obs_to_nominal = round((x / n) / thr$nominal[i], 2),
      excess_per_1000_scans = round(1000 * (x / n - thr$nominal[i]), 1),
      p_value = signif(bt$p.value, 3))
  }
}
res <- do.call(rbind, rows)
# Bonferroni: 10 מבחנים -> כל ערך p מוכפל ב-10 ומושווה ל-ALPHA
res$p_bonferroni <- signif(p.adjust(res$p_value, "bonferroni"), 3)
res$reject_H0    <- res$p_bonferroni < ALPHA
print(res, row.names = FALSE)
save_table(res, "06_threshold_tests")

# האם העקומה מוסטת (ממוצע z) או שהפיזור שלה לא נכון (סטיית התקן של z)?
# אם העקומה נכונה: ממוצע z = 0 וסטיית תקן של z = 1
sub_section("Location and spread of z (one scan per pregnancy)")
z_tbl <- do.call(rbind, lapply(names(methods), function(m) {
  z  <- d1[[paste0("z_", methods[m])]]
  tt <- t.test(z)
  data.frame(method = m, mean_z = round(mean(z), 3),
             ci95_low = round(tt$conf.int[1], 3), ci95_high = round(tt$conf.int[2], 3),
             sd_z = round(sd(z), 3))
}))
print(z_tbl, row.names = FALSE)
save_table(z_tbl, "07_location_spread_z")

make_plot("04_summary_forest", function() {
  op <- par(mar = c(4.5, 10, 3, 2)); on.exit(par(op))
  y <- rev(seq_len(nrow(res)) + rep(c(0, 1), each = nrow(thr)))
  cols <- ifelse(res$method == "Hadlock", COL_HAD, COL_KUR)
  rat_lo <- res$ci95_low / res$nominal_pct; rat_hi <- res$ci95_high / res$nominal_pct
  plot(res$ratio_obs_to_nominal, y, xlim = range(0, rat_lo, rat_hi, 2), pch = 16, col = cols,
       yaxt = "n", ylab = "", xlab = "Observed / nominal share of 'abnormal' scans (1 = perfect)",
       main = "How many scans each curve flags, relative to what it promises")
  segments(rat_lo, y, rat_hi, y, col = cols, lwd = 2)
  abline(v = 1, lty = 2, lwd = 2)
  axis(2, at = y, labels = paste(res$method, res$threshold), las = 1, cex.axis = 0.8)
}, width = 10, height = 6)

cat("\nAll tables (CSV) and plots (PNG) were saved to:\n  ", normalizePath(OUT_DIR), "\n")
