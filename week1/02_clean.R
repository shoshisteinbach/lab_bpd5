# =============================================================================
# שבוע 1 - שלב 2: ניקוי ערכים חריגים (מדד BPD)
# Week 1 - Step 2: outlier cleaning (BPD)
#
# החלטה (אפשרות ד): שומרים שתי גרסאות של הנתונים -
#   raw   - כל הנתונים כפי שהם (מלבד ערכים חסרים)
#   clean - לאחר הסרת שגיאות רישום
# בהמשך נחשב אחוזונים על שתיהן כדי להראות את השפעת הניקוי.
#
# כללי הניקוי:
#   כלל 1 - הסרת ערכים מחוץ לטווח הביולוגי 10-120 מ"מ
#           (ערכים < 10 נרשמו בס"מ, ערכים > 120 עם ספרה מיותרת - ראו שלב 1)
#   כלל 2 - הסרת ערכים בודדים שרחוקים מאוד מעמיתיהם באותו שבוע:
#           z חסין = (bpd - חציון השבוע) / (1.4826 * MAD של השבוע), |z| > 5
#           חציון ו-MAD לא מושפעים מהחריגים עצמם, וסף 5 שמרני מאוד
#           (בהתפלגות נורמלית - כ-1 ל-3.5 מיליון), כך שרק שגיאות ברורות מוסרות.
# הרצה: Rscript week1/02_clean.R  (מתוך תיקיית lab)
# =============================================================================

library(ggplot2)

raw <- read.csv("rawdata.csv")[, c("pregweek", "bpd")]
raw <- raw[complete.cases(raw), ]
raw$week <- floor(raw$pregweek)
cat("Raw (non-missing) rows:", nrow(raw), "\n")

# --- כלל 1: טווח ביולוגי ------------------------------------------------------
raw$rule1 <- raw$bpd < 10 | raw$bpd > 120
cat("\nRule 1 - outside 10-120 mm:", sum(raw$rule1),
    sprintf("(%.2f%%)", 100 * mean(raw$rule1)), "\n")
cat("   of which < 10 :", sum(raw$bpd < 10), "\n")
cat("   of which > 120:", sum(raw$bpd > 120), "\n")

# --- כלל 2: z חסין בתוך כל שבוע (מחושב רק על מה שעבר את כלל 1) -----------------
ok1 <- raw[!raw$rule1, ]
med <- tapply(ok1$bpd, ok1$week, median)
mad_ <- tapply(ok1$bpd, ok1$week, mad)          # mad() כבר כולל את המקדם 1.4826
raw$robust_z <- (raw$bpd - med[as.character(raw$week)]) / mad_[as.character(raw$week)]
raw$rule2 <- !raw$rule1 & abs(raw$robust_z) > 5
cat("\nRule 2 - |robust z| > 5 within week:", sum(raw$rule2),
    sprintf("(%.2f%%)", 100 * mean(raw$rule2)), "\n")
cat("   below (small heads):", sum(raw$rule2 & raw$robust_z < 0), "\n")
cat("   above (large heads):", sum(raw$rule2 & raw$robust_z > 0), "\n")

cat("\nPer-week median and robust SD (after rule 1):\n")
print(data.frame(week = names(med), n = as.vector(table(ok1$week)),
                 median = as.vector(med), robust_sd = round(as.vector(mad_), 2)),
      row.names = FALSE)

cat("\nValues removed by rule 2 (week, bpd, robust z):\n")
r2 <- raw[raw$rule2, c("pregweek", "bpd", "robust_z")]
r2 <- r2[order(r2$pregweek), ]
r2$robust_z <- round(r2$robust_z, 1)
r2$pregweek <- round(r2$pregweek, 1)
print(r2, row.names = FALSE)

# --- סיכום ושמירה -------------------------------------------------------------
raw$status <- ifelse(raw$rule1, "Removed: outside 10-120 mm",
               ifelse(raw$rule2, "Removed: |robust z| > 5", "Kept"))
clean <- raw[raw$status == "Kept", c("pregweek", "week", "bpd")]

summary_tab <- as.data.frame(table(status = raw$status))
summary_tab$percent <- round(100 * summary_tab$Freq / nrow(raw), 2)
cat("\nCleaning summary:\n")
print(summary_tab, row.names = FALSE)
cat("\nClean rows:", nrow(clean), "\n")

cat("\nClean data per-week counts (weeks with < 100 obs flagged):\n")
nw <- table(clean$week)
print(nw[nw < 100])

write.csv(raw[, c("pregweek", "week", "bpd")], "week1/output/bpd_raw.csv", row.names = FALSE)
write.csv(clean, "week1/output/bpd_clean.csv", row.names = FALSE)
write.csv(summary_tab, "week1/output/02_cleaning_summary.csv", row.names = FALSE)

# --- גרף: מה הוסר -------------------------------------------------------------
plot_d <- raw
plot_d$status <- factor(plot_d$status, levels = c("Kept", "Removed: outside 10-120 mm",
                                                  "Removed: |robust z| > 5"))
p <- ggplot(plot_d, aes(pregweek, bpd, colour = status)) +
  geom_point(data = plot_d[plot_d$status == "Kept", ], alpha = 0.05, size = 0.6) +
  geom_point(data = plot_d[plot_d$status != "Kept", ], alpha = 0.7, size = 1.2) +
  scale_y_log10() +
  scale_colour_manual(values = c("grey30", "darkorange", "firebrick")) +
  labs(title = "BPD cleaning: kept vs removed observations (log scale)",
       x = "Gestational age (weeks)", y = "BPD (mm, log scale)", colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("week1/output/02_cleaning.png", p, width = 8, height = 5.5, dpi = 150)

# זום על הטווח הסביר - רק כלל 2
p2 <- ggplot(plot_d[!raw$rule1, ], aes(pregweek, bpd, colour = status)) +
  geom_point(data = plot_d[!raw$rule1 & !raw$rule2, ], alpha = 0.05, size = 0.6) +
  geom_point(data = plot_d[raw$rule2, ], size = 1.5) +
  scale_colour_manual(values = c("Kept" = "grey30", "Removed: |robust z| > 5" = "firebrick")) +
  labs(title = "Rule 2 (within-week robust z > 5), 10-120 mm range",
       x = "Gestational age (weeks)", y = "BPD (mm)", colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("week1/output/02_cleaning_rule2.png", p2, width = 8, height = 5.5, dpi = 150)

cat("\nSaved: bpd_raw.csv, bpd_clean.csv, 02_cleaning_summary.csv, 02_cleaning*.png\n")
