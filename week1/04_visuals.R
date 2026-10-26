# =============================================================================
# שבוע 1 - שלב 4: גרפים מעוצבים להצגה למתייעץ (מדד BPD)
# Week 1 - Step 4: presentation-quality charts
#
# כל הגרפים באותו סגנון: רקע לבן, קווי רשת עדינים, טקסט בעברית,
# צבעים קבועים - כתום לאחוזונים הנמוכים (עובר קטן), כחול לגבוהים (עובר גדול).
# בגרפים אין כותרת - הכותרת תופיע בשקף עצמו.
# הרצה: Rscript week1/04_visuals.R  (מתוך תיקיית lab)
# =============================================================================

library(ggplot2)

dir.create("week1/output/visuals", showWarnings = FALSE)

# --- הגדרות עיצוב משותפות ---------------------------------------------------
FONT  <- "Arial Hebrew"
INK   <- "#0b0b0b"   # טקסט ראשי
INK2  <- "#52514e"   # טקסט משני
MUTED <- "#898781"   # צירים
GRID  <- "#e1e0d9"   # קווי רשת
POINT <- "#b9b8b0"   # נקודות הנתונים
ORANGE <- "#eb6834"  # אחוזונים נמוכים
BLUE   <- "#2a78d6"  # אחוזונים גבוהים

# צבע לכל אחוזון: ככל שהאחוזון קיצוני יותר - הצבע כהה יותר
pct_colours <- c(P3 = "#c4471a", P5 = ORANGE, P10 = "#f3a27f",
                 P95 = "#6da7ec", "P97.5" = "#1c5cab")
pct_labels  <- c(P3 = "אחוזון 3", P5 = "אחוזון 5", P10 = "אחוזון 10",
                 P95 = "אחוזון 95", "P97.5" = "אחוזון 97.5")

theme_deck <- function() {
  theme_minimal(base_family = FONT, base_size = 15) +
    theme(
      text = element_text(colour = INK2),
      axis.text = element_text(colour = MUTED, size = 13),
      axis.title = element_text(colour = INK2, size = 14),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.line.x = element_line(colour = "#c3c2b7", linewidth = 0.4),
      legend.position = "top",
      legend.justification = "right",
      legend.text = element_text(colour = INK2, size = 13),
      legend.title = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      plot.margin = margin(12, 16, 12, 12)
    )
}

save_chart <- function(p, name, width = 11, height = 6) {
  ggsave(file.path("week1/output/visuals", name), p, width = width, height = height,
         dpi = 200, device = ragg::agg_png, bg = "white")
}

x_weeks <- scale_x_continuous(breaks = seq(14, 42, 2), limits = c(14, 43),
                              expand = expansion(mult = c(0.01, 0.01)))
lab_week <- "גיל הריון (שבועות)"
lab_bpd  <- "קוטר ראש BPD (מ\"מ)"

# --- נתונים -----------------------------------------------------------------
labels <- c("P3", "P5", "P10", "P95", "P97.5")

raw   <- read.csv("week1/output/bpd_raw.csv")
clean <- read.csv("week1/output/bpd_clean.csv")
clean <- clean[clean$week %in% 14:42, ]
emp_clean <- read.csv("week1/output/03_empirical_clean.csv")
emp_raw   <- read.csv("week1/output/03_empirical_raw.csv")

# האחוזונים האמפיריים משלב 3, מצוירים באמצע כל שבוע (למשל שבוע 20 = 20.5)
emp_long <- do.call(rbind, lapply(labels, function(l)
  data.frame(pregweek = emp_clean$week + 0.5, percentile = l, bpd = emp_clean[[l]])))
emp_long$percentile <- factor(emp_long$percentile, levels = rev(labels))

# =============================================================================
# גרף 1: עקומות האחוזונים - הגרף המרכזי למתייעץ
# =============================================================================
band <- data.frame(pregweek = emp_clean$week + 0.5,
                   lo = emp_clean$P10, hi = emp_clean$P95)   # בין אחוזון 10 לאחוזון 95
p1 <- ggplot() +
  geom_ribbon(data = band, aes(pregweek, ymin = lo, ymax = hi), fill = "#f1f0ec") +
  geom_point(data = clean, aes(pregweek, bpd), colour = POINT, alpha = 0.06, size = 0.35) +
  geom_line(data = emp_long, aes(pregweek, bpd, colour = percentile), linewidth = 1.1) +
  geom_point(data = emp_long, aes(pregweek, bpd, colour = percentile), size = 1.8) +
  scale_colour_manual(values = pct_colours, labels = pct_labels) +
  x_weeks + labs(x = lab_week, y = lab_bpd) +
  theme_deck()
save_chart(p1, "04_growth_chart.png")

# =============================================================================
# גרף 2: מה מצאנו בנתונים הגולמיים - שלושה "פסים"
# =============================================================================
raw$type <- "תקין"
raw$type[raw$bpd < 10]  <- "נרשם בס\"מ במקום במ\"מ"
raw$type[raw$bpd > 120] <- "ספרה מיותרת"
raw$type <- factor(raw$type, levels = c("תקין", "ספרה מיותרת", "נרשם בס\"מ במקום במ\"מ"))
p3 <- ggplot(raw, aes(pregweek, bpd, colour = type)) +
  geom_point(data = raw[raw$type == "תקין", ], alpha = 0.05, size = 0.4) +
  geom_point(data = raw[raw$type != "תקין", ], alpha = 0.55, size = 1) +
  scale_colour_manual(values = c(POINT, BLUE, ORANGE)) +
  scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_x_continuous(breaks = seq(14, 44, 2)) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(x = lab_week, y = "קוטר ראש BPD (מ\"מ, סקאלה לוגריתמית)") +
  theme_deck()
save_chart(p3, "04_raw_errors.png")

# =============================================================================
# גרף 3: השפעת הניקוי על אחוזון 3 - כמה מ"מ "יורדים" בלי ניקוי
# =============================================================================
diff <- data.frame(week = emp_clean$week, d = emp_raw$P3 - emp_clean$P3)
p4 <- ggplot(diff, aes(week, d)) +
  geom_col(fill = ORANGE, width = 0.75) +
  geom_hline(yintercept = 0, colour = "#c3c2b7", linewidth = 0.4) +
  scale_x_continuous(breaks = seq(14, 42, 2)) +
  scale_y_continuous(breaks = seq(-3, 0, 1), limits = c(-3.2, 0.2)) +
  labs(x = lab_week, y = "הפרש באחוזון 3 (מ\"מ)\nגולמי פחות נקי") +
  theme_deck()
save_chart(p4, "04_cleaning_effect_P3.png", height = 4.5)

# =============================================================================
# גרף 4: כמה בדיקות יש בכל שבוע
# =============================================================================
n_week <- as.data.frame(table(week = floor(raw$pregweek)))
n_week$week <- as.numeric(as.character(n_week$week))
n_week$used <- ifelse(n_week$week <= 42, "נכלל בחישוב (14-42)", "לא נכלל (מעט מדי בדיקות)")
p5 <- ggplot(n_week, aes(week, Freq, fill = used)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = c("נכלל בחישוב (14-42)" = BLUE,
                               "לא נכלל (מעט מדי בדיקות)" = "#c3c2b7")) +
  scale_x_continuous(breaks = seq(14, 44, 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = lab_week, y = "מספר בדיקות") +
  theme_deck()
save_chart(p5, "04_n_per_week.png", height = 4.5)

cat("Saved charts to week1/output/visuals/\n")
