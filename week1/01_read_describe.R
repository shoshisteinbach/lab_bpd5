# =============================================================================
# שבוע 1 - שלב 1: קריאת הנתונים ותיאורם (מדד BPD - קוטר ראש)
# Week 1 - Step 1: read the data and describe it (BPD only)
#
# לפי הנחיות המטלה משתמשים רק בגיל ההריון (pregweek) ובמדד הביומטרי (bpd).
# בשלב זה לא מסירים דבר - רק מתארים ומסמנים ערכים חשודים.
# הרצה: Rscript week1/01_read_describe.R  (מתוך תיקיית lab)
# =============================================================================

library(ggplot2)

dir.create("week1/output", showWarnings = FALSE, recursive = TRUE)

# --- 1. קריאת הקובץ ובחירת שתי העמודות ---------------------------------------
raw <- read.csv("rawdata.csv")
cat("Raw file:", nrow(raw), "rows x", ncol(raw), "columns\n")

d <- raw[, c("pregweek", "bpd")]

# --- 2. ערכים חסרים ------------------------------------------------------------
cat("\nMissing values:\n")
print(colSums(is.na(d)))
d <- d[complete.cases(d), ]
cat("Rows with both pregweek and bpd:", nrow(d), "\n")

# --- 3. סטטיסטיקה תיאורית -----------------------------------------------------
cat("\nSummary:\n")
print(summary(d))

cat("\nBPD quantiles (mm):\n")
print(quantile(d$bpd, c(0, .001, .005, .01, .05, .5, .95, .99, .995, .999, 1)))

# --- 4. מספר תצפיות לכל שבוע הריון שלם ----------------------------------------
d$week <- floor(d$pregweek)
n_week <- as.data.frame(table(week = d$week))
cat("\nObservations per completed week:\n")
print(n_week, row.names = FALSE)
write.csv(n_week, "week1/output/01_n_per_week.csv", row.names = FALSE)

# --- 5. ערכים חשודים ----------------------------------------------------------
# קוטר ראש של עובר בשבועות 14-44 נע בערך בין 20 ל-110 מ"מ.
# ערכים מתחת ל-10 נראים כמו רישום בס"מ, ומעל 120 כמו ספרה מיותרת.
cat("\nSuspicious values:\n")
cat("  bpd < 10 mm  :", sum(d$bpd < 10), "\n")
cat("  10 <= bpd < 15:", sum(d$bpd >= 10 & d$bpd < 15), "\n")
cat("  bpd > 120 mm :", sum(d$bpd > 120), "\n")
cat("\n  10 largest :", sort(d$bpd, decreasing = TRUE)[1:10], "\n")
cat("  10 smallest:", sort(d$bpd)[1:10], "\n")

# בדיקה: אם מכפילים את הערכים הקטנים ב-10 (ס"מ -> מ"מ), האם הם דומים לחציון של אותו שבוע?
# יחס קרוב ל-1 אומר שהערך כנראה נרשם בס"מ.
ok <- d[d$bpd >= 10 & d$bpd <= 120, ]
week_median <- tapply(ok$bpd, ok$week, median)

small <- d[d$bpd < 10, ]
ratio_small <- small$bpd * 10 / week_median[as.character(small$week)]
cat("\n  Values < 10: (bpd x 10) / week median:\n")
print(summary(ratio_small))

big <- d[d$bpd > 120, ]
ratio_big <- big$bpd / 10 / week_median[as.character(big$week)]
cat("\n  Values > 120: (bpd / 10) / week median:\n")
print(summary(ratio_big))

# --- 6. גרפים -----------------------------------------------------------------
# גרף פיזור של כל הנתונים (סקלה לוגריתמית כדי לראות גם את הערכים הקיצוניים)
p1 <- ggplot(d, aes(pregweek, bpd)) +
  geom_point(alpha = 0.08, size = 0.6) +
  scale_y_log10() +
  labs(title = "BPD vs gestational age - all raw data (log scale)",
       x = "Gestational age (weeks)", y = "BPD (mm, log scale)") +
  theme_minimal()
ggsave("week1/output/01_scatter_raw_log.png", p1, width = 8, height = 5, dpi = 150)

# גרף פיזור בטווח הסביר בלבד (10-120 מ"מ)
p2 <- ggplot(d[d$bpd >= 10 & d$bpd <= 120, ], aes(pregweek, bpd)) +
  geom_point(alpha = 0.05, size = 0.6) +
  labs(title = "BPD vs gestational age (10-120 mm only)",
       x = "Gestational age (weeks)", y = "BPD (mm)") +
  theme_minimal()
ggsave("week1/output/01_scatter_zoom.png", p2, width = 8, height = 5, dpi = 150)

# היסטוגרמה של מספר התצפיות לפי שבוע
p3 <- ggplot(d, aes(week)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Number of scans per completed gestational week",
       x = "Gestational week", y = "Count") +
  theme_minimal()
ggsave("week1/output/01_n_per_week.png", p3, width = 8, height = 4, dpi = 150)

cat("\nPlots saved to week1/output/\n")
