# =============================================================================
# שבוע 1 - שלב 3: חישוב האחוזונים 3, 5, 10, 95, 97.5 (מדד BPD)
# Week 1 - Step 3: percentiles of BPD by gestational age
#
# טווח: שבועות 14-42 (בשבועות 43-44 יש רק 12 בדיקות - אי אפשר לחשב שם אחוזונים)
#
# השיטה - אחוזונים אמפיריים: לכל שבוע שלם לוקחים את כל הבדיקות של אותו שבוע
#          ומחשבים את האחוזונים ישירות מהנתונים, בלי הנחה על צורת ההתפלגות.
#          עושים זאת על הנתונים הגולמיים ועל הנתונים הנקיים, כדי לראות כמה הניקוי משנה.
#
# הרצה: Rscript week1/03_percentiles.R  (מתוך תיקיית lab)
# =============================================================================

library(ggplot2)

probs <- c(0.03, 0.05, 0.10, 0.95, 0.975)
labels <- c("P3", "P5", "P10", "P95", "P97.5")
weeks <- 14:42

raw   <- read.csv("week1/output/bpd_raw.csv")
clean <- read.csv("week1/output/bpd_clean.csv")
raw   <- raw[raw$week %in% weeks, ]
clean <- clean[clean$week %in% weeks, ]

# -----------------------------------------------------------------------------
# אחוזונים אמפיריים לכל שבוע
# -----------------------------------------------------------------------------
empirical <- function(data) {
  result <- data.frame(week = weeks)
  result$n <- sapply(weeks, function(w) sum(data$week == w))
  for (i in seq_along(probs)) {
    result[[labels[i]]] <- sapply(weeks, function(w)
      quantile(data$bpd[data$week == w], probs[i]))
  }
  result
}

emp_raw   <- empirical(raw)
emp_clean <- empirical(clean)

cat("=== Empirical percentiles - CLEAN data (mm) ===\n")
print(emp_clean, row.names = FALSE)

cat("\n=== Empirical percentiles - RAW data (mm) ===\n")
print(emp_raw, row.names = FALSE)

cat("\n=== Difference RAW minus CLEAN (mm) ===\n")
diff_tab <- emp_raw
diff_tab[, labels] <- round(emp_raw[, labels] - emp_clean[, labels], 1)
print(diff_tab[, c("week", labels)], row.names = FALSE)

write.csv(emp_clean, "week1/output/03_empirical_clean.csv", row.names = FALSE)
write.csv(emp_raw,   "week1/output/03_empirical_raw.csv",   row.names = FALSE)

# -----------------------------------------------------------------------------
# גרפים
# -----------------------------------------------------------------------------
emp_long <- do.call(rbind, lapply(labels, function(l) data.frame(
  week = emp_clean$week + 0.5, percentile = l, value = emp_clean[[l]])))
emp_long$percentile <- factor(emp_long$percentile, levels = labels)

colours <- c(P3 = "#B2182B", P5 = "#EF8A62", P10 = "#F4A582",
             P95 = "#67A9CF", "P97.5" = "#2166AC")

# גרף 1: הנתונים + האחוזונים האמפיריים (מצוירים באמצע כל שבוע ומחוברים בקו)
p1 <- ggplot() +
  geom_point(data = clean, aes(pregweek, bpd), alpha = 0.03, size = 0.5) +
  geom_line(data = emp_long, aes(week, value, colour = percentile), linewidth = 0.9) +
  geom_point(data = emp_long, aes(week, value, colour = percentile), size = 1.6) +
  scale_colour_manual(values = colours) +
  scale_x_continuous(breaks = seq(14, 42, 2)) +
  labs(title = "BPD empirical percentiles by gestational week (clean data)",
       x = "Gestational age (weeks)", y = "BPD (mm)", colour = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("week1/output/03_percentile_curves.png", p1, width = 9, height = 6, dpi = 150)

# גרף 2: אחוזונים אמפיריים - גולמי מול נקי
emp_both <- rbind(
  data.frame(emp_long, data = "Clean"),
  do.call(rbind, lapply(labels, function(l) data.frame(
    week = emp_raw$week + 0.5, percentile = factor(l, levels = labels),
    value = emp_raw[[l]], data = "Raw"))))
p2 <- ggplot(emp_both, aes(week, value, colour = percentile, linetype = data)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = colours) +
  scale_x_continuous(breaks = seq(14, 42, 2)) +
  labs(title = "Empirical percentiles: raw data vs clean data",
       x = "Gestational age (weeks)", y = "BPD (mm)", colour = NULL, linetype = NULL) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("week1/output/03_raw_vs_clean.png", p2, width = 9, height = 6, dpi = 150)

cat("\nSaved tables and plots to week1/output/\n")
