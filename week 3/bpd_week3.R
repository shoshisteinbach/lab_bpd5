###############################################################################
#  מעבדה בסטטיסטיקה - קבוצת BPD - שבוע 3
#  ניקוי חריגים, מדידות סמוכות, הטיית דגימה, וחלוקה לקובץ אימון ובדיקה
#
#  קובץ אחד לכל העבודה. כל חלק יושב בסעיף משלו, כדי שאפשר יהיה לעבוד
#  עליו בזמנים שונים בגיט בלי להתנגש:
#    0. הגדרות
#    1. קריאה, הריונות, שבוע 40 ותיקון יחידות   (מוכן - מבוסס על שבוע 2)
#    2. חריגים                                   (חלק א)
#    3. מדידות סמוכות                            (חלק ב)
#    4. הטיית דגימה                              (חלק ג)
#    5. אימון מול בדיקה                          (חלק ד)
#    6. יצירת הקבצים הסופיים: train.csv ו-test.csv
#
#  עקרונות עבודה:
#    - עובדים רק בסעיף שלכם. סעיפים 2 ו-3 מוסיפים עמודת סימון (TRUE = להסיר)
#      ולא מוחקים שורות בעצמם; ההסרה בפועל נעשית בסעיף 6.
#    - להריץ מתיקיית השורש של הפרויקט (שם נמצא rawdata.csv).
#    - R בסיסי בלבד. טבלאות ל-CSV וגרפים ל-PNG בתיקייה "week 3/output".
###############################################################################

## ======================== 0. הגדרות =========================================

DATA_PATH <- "rawdata.csv"
OUT_DIR   <- file.path("week 3", "output")

MAX_WEEK     <- 40           # הגבלה לגיל הריון <= 40 שבועות
NEW_PREG_GAP <- 100          # ימים בין תאריכי לידה משוערים שמגדירים הריון חדש
TRAIN_YEARS  <- 2000:2007    # קובץ האימון
TEST_FROM    <- 2008         # קובץ הבדיקה: משנה זו והלאה

set.seed(2026)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

save_table <- function(df, name) {
  write.csv(df, file.path(OUT_DIR, paste0(name, ".csv")), row.names = FALSE)
}

# גרפים: אותו עיצוב כמו בשבוע 2 (10x6 אינץ', כותרת מודגשת, צבעים מתאימים
# גם לעיוורי צבעים). כתום = מה שהוסר / סומן, כחול = חריג שנשאר, אפור = שאר הנתונים.
COL_REMOVE <- "#D55E00"
COL_KEEP   <- "#0072B2"
COL_BULK   <- "grey30"
alpha_col  <- function(col, a) adjustcolor(col, alpha.f = a)

# עברית בגרפים: מכשיר הגרפים של R לא מסדר טקסט מימין לשמאל, ולכן heb() הופכת
# את המילים והאותיות לסדר החזותי. מספרים ושמות באנגלית (BPD) נשארים כמו שהם.
# להשתמש בה על כל טקסט עברי בגרף: main, xlab, ylab, legend וכו'.
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

## ======================== 1. הנתונים: קריאה, הריונות, יחידות =================
# אותם שלבים כמו בשבוע 2 (week2/bpd_analysis.R, שלבים 1-4)

raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"))
dat <- raw

# הגדרת הריונות: אותה אם + תאריכי לידה משוערים במרחק של עד NEW_PREG_GAP ימים
dat <- dat[order(dat$snumber, dat$expected, dat$check_d_sas), ]
new_preg <- c(TRUE, diff(dat$snumber) != 0 | diff(dat$expected) > NEW_PREG_GAP)
dat$preg_id <- cumsum(new_preg)

# הגבלה לשבוע 40
dat <- dat[dat$pregweek <= MAX_WEEK, ]
dat$week <- pmin(floor(dat$pregweek), 39)     # שבוע שלם; יום 280 מצורף לשבוע 39

# יחידות מידה (bpd_raw = הערך המקורי, bpd = במ"מ)
#   bpd < 15          -> נרשם בס"מ                  (x 10)
#   150 <= bpd < 1500 -> חסרה נקודה עשרונית        (/ 10)
#   bpd >= 1500       -> חסרות שתי ספרות עשרוניות  (/ 100)
bpd_factor <- with(dat, ifelse(is.na(bpd), NA,
                        ifelse(bpd < 15, 10,
                        ifelse(bpd >= 1500, 0.01,
                        ifelse(bpd >= 150, 0.1, 1)))))
dat$bpd_raw <- dat$bpd
dat$bpd     <- dat$bpd_raw * bpd_factor

# הסרת מדידות ללא BPD
dat <- dat[!is.na(dat$bpd), ]

# מכאן והלאה dat הוא "קובץ הבסיס": עד שבוע 40, יחידות מתוקנות, בלי ניקוי חריגים
cat("Base data:", nrow(dat), "scans |", length(unique(dat$preg_id)),
    "pregnancies |", length(unique(dat$snumber)), "women\n")

## ======================== 2. חריגים (חלק א) =================================
# מטרה: לסמן רק תצפיות החשודות כטעות הקלדה או טעות אחרת, שאינן נתון אמיתי.
# לספור כמה תצפיות נראות חריגות, ולבדוק אם הן מתרכזות בשבועות מסוימים או
# פזורות לאורך כל ההריון. אפשר שהשיטה תהיה שונה מזו שבכיתה.
#
# התוצאה: העמודה dat$outlier (TRUE = להסיר).
#
# השיטה (פירוט הנימוקים ב-week 3/README.md):
#   חריג = BPD רחוק מהחציון של השבוע שלו (סטייה חסינה, חציון/MAD).
#   אבל חריג איננו בהכרח טעות: עובר עם ראש קטן או גדול באמת הוא נתון אמיתי.
#   לכן בודקים גם עקביות מול היקף הראש: היחס head_cir/bpd יציב מאוד בתוך
#   כל שבוע, ו-BPD שגוי "שובר" אותו. מסירים רק תצפית שחורגת בשתי הבדיקות.

OUT_COUNT_Z  <- 3     # "נראה חריג" לצורך הספירה
OUT_REMOVE_Z <- 5     # סף ההסרה (כמו בשבוע 1)
RATIO_Z_MAX  <- 5     # אי-עקביות: סטייה חסינה של היחס head_cir/bpd

robust_z <- function(x, group) {
  med <- ave(x, group, FUN = function(v) median(v, na.rm = TRUE))
  mad <- ave(x, group, FUN = function(v) mad(v, na.rm = TRUE))
  (x - med) / mad
}

# יחידות של היקף הראש: ערך קטן מ-50 לא יכול להיות מ"מ (כבר בשבוע 14 הוא כ-90),
# ולכן נרשם בס"מ (x 10). משתמשים בו רק לבדיקת העקביות.
dat$hc_mm <- ifelse(dat$head_cir < 50, dat$head_cir * 10, dat$head_cir)

dat$bpd_rz   <- robust_z(dat$bpd, dat$week)
dat$ratio_rz <- robust_z(dat$hc_mm / dat$bpd, dat$week)

out_looks    <- abs(dat$bpd_rz) > OUT_COUNT_Z
out_extreme  <- abs(dat$bpd_rz) > OUT_REMOVE_Z
inconsistent <- !is.na(dat$ratio_rz) & abs(dat$ratio_rz) > RATIO_Z_MAX

dat$outlier <- out_extreme & inconsistent

# ספירה מדורגת
tier_tbl <- data.frame(
  tier = c("looks like outlier (|z|>3)", "  of them consistent with head circumference",
           "  of them inconsistent", "    extreme (|z|>5) and inconsistent = REMOVED",
           "  extreme (|z|>5) but consistent = kept"),
  n    = c(sum(out_looks), sum(out_looks & !inconsistent), sum(out_looks & inconsistent),
           sum(dat$outlier), sum(out_extreme & !inconsistent)))
tier_tbl$pct_of_scans <- round(100 * tier_tbl$n / nrow(dat), 2)
print(tier_tbl, row.names = FALSE)
cat("Removed outliers: low", sum(dat$outlier & dat$bpd_rz < 0),
    "| high", sum(dat$outlier & dat$bpd_rz > 0),
    "| head_cir missing (cannot be removed):", sum(is.na(dat$ratio_rz)), "\n")
save_table(tier_tbl, "02_outlier_tiers")

# פיזור לפי שבוע
wk_tbl <- data.frame(
  week       = sort(unique(dat$week)),
  scans      = as.vector(table(dat$week)),
  looks      = as.vector(tapply(out_looks, dat$week, sum)),
  removed    = as.vector(tapply(dat$outlier, dat$week, sum)))
wk_tbl$pct_looks   <- round(100 * wk_tbl$looks / wk_tbl$scans, 1)
wk_tbl$pct_removed <- round(100 * wk_tbl$removed / wk_tbl$scans, 2)
save_table(wk_tbl, "02_outliers_by_week")

# גרף 1: אחוז החריגים לפי שבוע (עמודות מוערמות: כתום = הוסר, כחול = נשאר)
save_plot("02_outliers_by_week", function() {
  m <- rbind(removed = wk_tbl$pct_removed, kept = wk_tbl$pct_looks - wk_tbl$pct_removed)
  mid <- barplot(m, col = c(COL_REMOVE, COL_KEEP), border = NA, ylim = c(0, 4),
                 las = 1, xlab = heb("גיל הריון (שבועות)"),
                 ylab = heb("אחוז מהמדידות באותו שבוע"),
                 main = heb("חריגים לפי גיל הריון: אחוז המדידות שנראות חריגות בכל שבוע"))
  axis(1, at = mid, labels = wk_tbl$week, cex.axis = 0.9)
  mtext(heb("נראית חריגה = סטיית ה-BPD מהחציון של אותו שבוע גדולה מ-3 (סטייה חסינה)"),
        side = 3, line = 0.3, cex = 0.85)
  legend("topleft", bty = "n", pch = 15, pt.cex = 1.5, col = c(COL_REMOVE, COL_KEEP),
         legend = heb(c("הוסרו כטעות: קיצוניות ולא עקביות עם היקף הראש",
                        "נשארו: עקביות עם היקף הראש או לא קיצוניות מאוד")))
})

# גרף 2: איך נראים החריגים - BPD מול גיל הריון
save_plot("02_outliers_scatter", function() {
  plot(dat$pregweek[!out_looks], dat$bpd[!out_looks], pch = 16, cex = 0.35,
       col = alpha_col(COL_BULK, 0.15), xlim = c(14, 40), ylim = c(0, 120), las = 1,
       xaxt = "n", xlab = heb("גיל הריון (שבועות)"),
       ylab = heb("BPD במ\"מ, לאחר תיקון יחידות"),
       main = heb("BPD לפי גיל הריון: מדידות שנראות חריגות ומדידות שהוסרו כטעות"))
  axis(1, at = seq(14, 40, 2))
  points(dat$pregweek[out_looks & !dat$outlier], dat$bpd[out_looks & !dat$outlier],
         pch = 16, cex = 0.5, col = alpha_col(COL_KEEP, 0.7))
  points(dat$pregweek[dat$outlier], dat$bpd[dat$outlier], pch = 16, cex = 0.9, col = COL_REMOVE)
  legend("topleft", bty = "n", pch = 16, col = c(COL_BULK, COL_KEEP, COL_REMOVE),
         legend = heb(c("מדידות רגילות",
                        paste0("נראות חריגות אבל נשארו: ", sum(out_looks & !dat$outlier)),
                        paste0("הוסרו כטעות: ", sum(dat$outlier)))))
})

## ======================== 3. מדידות סמוכות (חלק ב) ==========================
# מטרה: לאתר מדידות של אותו הריון (preg_id) באותו יום או בימים סמוכים
# (check_d_sas), לבדוק עד כמה הערכים שונים זה מזה, ולקבוע איזו מדידה נשארת.
# במצגת: כמה מקרים כאלה היו ואיזה כלל עבדנו לפיו.
#
# התוצאה: העמודה dat$near_dup (TRUE = המדידה להסרה).
#
# הכלל (פירוט הנימוקים ב-week 3/README.md):
#   סמוך = עד NEAR_DAYS ימים מהמדידה הראשונה בקבוצה, באותו הריון. הקבוצה נמדדת
#   מהמדידה הראשונה ולא בשרשרת, כדי שמדידות בימים 0, 3, 6, 9 לא יהפכו לקבוצה אחת.
#   נשארת המדידה הראשונה. אם בתאריך הראשון יש כמה מדידות (אין שעה), נשארת זו
#   שהיחס head_cir/bpd שלה הכי קרוב לחציון של השבוע.
#   הבדיקה רצה רק על מדידות שלא הוסרו כחריגים בסעיף 2.

NEAR_DAYS <- 3     # ה-BPD גדל בכ-0.3 מ"מ ביום, כלומר פחות ממחצית רעש המדידה (כ-2.4 מ"מ)

dat$near_dup <- FALSE
idx <- which(!dat$outlier)
idx <- idx[order(dat$preg_id[idx], dat$check_d_sas[idx], abs(dat$ratio_rz[idx]))]

near_group <- integer(length(idx))     # מספר קבוצה לכל מדידה (לפי הסדר ב-idx)
g <- 0; anchor_day <- -Inf; anchor_preg <- -1
for (i in seq_along(idx)) {
  r <- idx[i]
  if (dat$preg_id[r] != anchor_preg || dat$check_d_sas[r] - anchor_day > NEAR_DAYS) {
    g <- g + 1; anchor_day <- dat$check_d_sas[r]; anchor_preg <- dat$preg_id[r]
  }
  near_group[i] <- g
}
# בכל קבוצה הראשונה ב-idx נשארת (תאריך מוקדם, ובאותו יום: היחס העקבי ביותר)
dat$near_dup[idx] <- duplicated(near_group)

# הפער בין מדידות עוקבות של אותו הריון, ועד כמה ה-BPD שונה
o    <- idx[order(dat$preg_id[idx], dat$check_d_sas[idx])]
same <- c(FALSE, diff(dat$preg_id[o]) == 0)
gap  <- ifelse(same, c(NA, diff(dat$check_d_sas[o])), NA)
dbpd <- ifelse(same, c(NA, diff(dat$bpd[o])), NA)

gap_tbl <- data.frame(gap_days = 0:NEAR_DAYS)
gap_tbl$pairs        <- sapply(gap_tbl$gap_days, function(x) sum(gap == x, na.rm = TRUE))
gap_tbl$median_abs_d <- sapply(gap_tbl$gap_days, function(x) median(abs(dbpd[which(gap == x)])))
gap_tbl$p90_abs_d    <- sapply(gap_tbl$gap_days, function(x) quantile(abs(dbpd[which(gap == x)]), 0.9))
gap_tbl$pct_identical <- sapply(gap_tbl$gap_days, function(x) round(100 * mean(dbpd[which(gap == x)] == 0), 1))
print(gap_tbl, row.names = FALSE)
save_table(gap_tbl, "03_near_by_gap")

grp_size <- table(near_group)
near_tbl <- data.frame(
  what = c("groups of adjacent scans", "pregnancies involved", "scans removed",
           "same-day scans fully identical (all measures)"),
  n    = c(sum(grp_size > 1),
           length(unique(dat$preg_id[dat$near_dup])),
           sum(dat$near_dup),
           sum(duplicated(dat[idx, c("preg_id", "check_d_sas", "bpd", "hc_mm", "femur")]))))
print(near_tbl, row.names = FALSE)
save_table(near_tbl, "03_near_summary")

# גרף 1: כמה ימים עוברים בין מדידות עוקבות של אותו הריון (0-28)
save_plot("03_near_gap_days", function() {
  cnt <- sapply(0:28, function(x) sum(gap == x, na.rm = TRUE))
  mid <- barplot(cnt, col = ifelse(0:28 <= NEAR_DAYS, COL_REMOVE, COL_BULK), border = NA, las = 1,
                 xlab = heb("ימים בין שתי מדידות עוקבות של אותו הריון"),
                 ylab = heb("מספר זוגות מדידות"),
                 main = heb("הפער בימים בין מדידות עוקבות של אותו הריון"))
  axis(1, at = mid, labels = 0:28, cex.axis = 0.8)
  legend("topleft", bty = "n", pch = 15, pt.cex = 1.5, col = c(COL_REMOVE, COL_BULK),
         legend = heb(c(paste0("סמוכות: עד ", NEAR_DAYS, " ימים - נשארת רק הראשונה"),
                        "מדידות נפרדות")))
})

# גרף 2: עד כמה ה-BPD שונה בין שתי מדידות סמוכות
save_plot("03_near_bpd_pairs", function() {
  k <- which(gap <= NEAR_DAYS)
  b1 <- dat$bpd[o][k - 1]; b2 <- dat$bpd[o][k]
  plot(b1, b2, pch = 16, cex = 0.7, col = alpha_col(COL_KEEP, 0.6), las = 1,
       xlab = heb("BPD במדידה הראשונה (מ\"מ)"),
       ylab = heb("BPD במדידה הסמוכה (מ\"מ)"),
       main = heb(paste0("מדידות סמוכות (עד ", NEAR_DAYS, " ימים): BPD במדידה הראשונה מול הסמוכה")))
  abline(0, 1, col = COL_REMOVE, lwd = 1.5)
  legend("topleft", bty = "n", lty = 1, col = COL_REMOVE, lwd = 1.5,
         legend = heb(c("קו שוויון: אותו ערך בשתי המדידות")))
})

## ======================== 4. הטיית דגימה (חלק ג) ============================
# מטרה: לבדוק אם נשים חזרו לבדיקות מאוחרות בעקבות ממצא חריג מוקדם
# (הטיית דגימה בשבועות המאוחרים), על המדד BPD. שיטה שונה מזו שבכיתה.
# מומלץ להריץ אחרי סעיפים 2-3, על הנתונים אחרי ההסרות:
#   clean <- dat[!dat$outlier & !dat$near_dup, ]
#
# הסעיף לא משנה את dat; הוא רק מפיק תוצאות.

# TODO: השאלה, השיטה, התוצאה וגרף

## ======================== 5. אימון מול בדיקה (חלק ד) ========================
# מטרה: לבדוק שהמשתנה BPD מתנהג בצורה דומה בקובץ האימון (2000-2007)
# ובקובץ הבדיקה (2008 והלאה), למשל ממוצע או חציון לפי שבוע.
# מומלץ להריץ אחרי סעיפים 2-3.
#
# הסעיף לא משנה את dat; הוא רק מפיק תוצאות.

# TODO: ההשוואה בין הקבצים וגרף

## ======================== 6. הקבצים הסופיים ================================
# הסרת הסימונים מסעיפים 2-3 וחלוקה לפי שנה.
# להחליט כצוות: האם גם קובץ הבדיקה עובר ניקוי חריגים ומדידות סמוכות
# (כרגע כן, אותו ניקוי לשני הקבצים).

clean <- dat[!dat$outlier & !dat$near_dup, ]
train <- clean[clean$year %in% TRAIN_YEARS, ]
test  <- clean[clean$year >= TEST_FROM, ]

write.csv(train, file.path("week 3", "train.csv"), row.names = FALSE)
write.csv(test,  file.path("week 3", "test.csv"),  row.names = FALSE)

cat("Removed: ", sum(dat$outlier), " outliers, ", sum(dat$near_dup),
    " near-duplicate scans\n", sep = "")
cat("Train:", nrow(train), "scans | Test:", nrow(test), "scans |",
    "years outside both:", sum(!(clean$year %in% TRAIN_YEARS) & clean$year < TEST_FROM), "scans\n")
