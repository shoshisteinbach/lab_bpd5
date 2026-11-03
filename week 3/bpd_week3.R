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
# איך מזהים: לכל מדידה יש preg_id (סעיף 1: אותה אישה + תאריך לידה משוער
# בהפרש של עד 100 ימים). ממיינים לפי הריון ותאריך בדיקה, ולכל מדידה מחשבים
# כמה ימים עברו מהמדידה הקודמת באותו הריון (check_d_sas הוא תאריך בימים).
#
# מה מצאנו לפני ההחלטה (אחרי הסרת 53 החריגים):
#   - פערים של 0 / עד 1 / עד 3 / עד 7 ימים: 86 / 122 / 271 / 1,099 זוגות.
#   - הביקורים בקצב שבועי (שיאים ב-7, 14, 21, 28 ימים); 0-3 ימים חורגים מהקצב.
#   - אחרי הפחתת הגידול הצפוי, ההפרש בין מדידות עוקבות הוא כ-2.4 מ"מ (סטיית
#     תקן) בכל פער, מ-0 ימים ועד 28. זה רעש המדידה; אין שבירה סטטיסטית.
#   - באותו יום: 29 זוגות זהים בכל המדדים (הקלדה כפולה), ו-8 זוגות שבהם כל
#     המדדים גדולים יחד במדידה אחת (חשד לתאומים). אין שעה לבדיקה.
#
# כללי ההכרעה שנבחרו (פירוט הנימוקים ב-week 3/README.md, סעיף 6):
#   החלטה 1 - סמוך = עד NEAR_DAYS = 3 ימים מהמדידה הראשונה בקבוצה.
#     למה: הראש גדל בכ-0.3 מ"מ ביום, כלומר כ-0.9 מ"מ ב-3 ימים, פחות ממחצית
#     רעש המדידה. 7 ימים נדחה: זה מרווח ביקורים רגיל, והיה מוחק מעקב תקין.
#   החלטה 2 - נשארת המדידה הראשונה. באותו יום (אין שעה) נשארת זו שהיחס
#     head_cir/bpd שלה הכי קרוב לחציון של השבוע.
#     למה: ערך שנמדד באמת (לא ממוצע), והמדידה השנייה עלולה להיות בדיקה
#     חוזרת בעקבות ממצא, כלומר הטיית דגימה.
#   החלטה 3 - זוגות שנראים כתאומים מטופלים כמו כל זוג.
#     למה: אין משתנה תאומים לאימות, ומדובר ב-8 זוגות בלבד.
#   פרט טכני - הקבוצה נמדדת מהמדידה הראשונה ולא בשרשרת, כדי שמדידות בימים
#     0, 3, 6, 9 לא יהפכו לקבוצה אחת (ימים 0+3 וימים 6+9 הן שתי קבוצות).
#   הבדיקה רצה רק על מדידות שלא הוסרו כחריגים, כדי שחריג שנמחק לא יגרום
#   למחיקה של מדידה תקינה שצמודה אליו.
#
# תוצאה: 266 קבוצות ב-258 הריונות, 267 מדידות הוסרו (0.47%).

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

# התפלגות ההבדל המוחלט ב-BPD בין מדידות סמוכות (עד NEAR_DAYS ימים)
near_k  <- which(gap <= NEAR_DAYS)
diff_tbl <- as.data.frame(table(cut(abs(dbpd[near_k]), c(-Inf, 0, 1, 2, 3, 5, Inf),
                                    labels = c("0 (identical)", "0-1", "1-2", "2-3", "3-5", ">5"))))
names(diff_tbl) <- c("abs_bpd_diff_mm", "pairs")
diff_tbl$pct <- round(100 * diff_tbl$pairs / sum(diff_tbl$pairs), 1)
print(diff_tbl, row.names = FALSE)
save_table(diff_tbl, "03_near_diff_dist")

# חשד לתאומים: באותו יום, BPD שונה ב-3 מ"מ לפחות, והיקף הראש (8 מ"מ לפחות) ועצם
# הירך זזים באותו כיוון
k0 <- which(gap == 0)
d_b <- diff(dat$bpd[o])[k0 - 1]; d_h <- diff(dat$hc_mm[o])[k0 - 1]; d_f <- diff(dat$femur[o])[k0 - 1]
twin_like <- abs(d_b) >= 3 & abs(d_h) >= 8 & sign(d_b) == sign(d_h) & sign(d_b) == sign(d_f)

grp_size <- table(near_group)
near_tbl <- data.frame(
  what = c("groups of adjacent scans", "pregnancies involved", "scans removed",
           "same-day scans fully identical (all measures)", "same-day twin-like pairs"),
  n    = c(sum(grp_size > 1),
           length(unique(dat$preg_id[dat$near_dup])),
           sum(dat$near_dup),
           sum(duplicated(dat[idx, c("preg_id", "check_d_sas", "bpd", "hc_mm", "femur")])),
           sum(twin_like, na.rm = TRUE)))
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
# שאלת המחקר:
# האם הסיכוי למדידת BPD נוספת בתוך ארבעה שבועות שונה בין הריונות
# שבהם מדידת הבסיס נמוכה, מרכזית או גבוהה ביחס להתפלגות BPD באותו שבוע?
#
# שיטה:
#   1. עובדים על נתונים נקיים: מסירים outlier ו-near_dup.
#   2. יחידת הניתוח היא הריון (preg_id), לא מדידה.
#   3. לכל הריון בוחרים את המדידה הראשונה בשבועות 20-24.
#   4. בתוך כל שבוע בסיס מחשבים P10 ו-P90 של BPD במדידות הבסיס.
#   5. מחלקים לשלוש קבוצות: נמוך (<P10), מרכזי (P10 עד P90),
#      גבוה (>P90).
#   6. בודקים אם קיימת מדידה נוספת באותו הריון במהלך 28 הימים
#      שלאחר מדידת הבסיס.
#   7. משווים את שיעור החזרה בין שלוש הקבוצות בטבלת 3x2 ובמבחן
#      Chi-square. אם התדירויות הצפויות קטנות, משתמשים ב-Fisher.
#
# חשוב:
#   - מוציאים הריונות שאין להם חלון מעקב מלא של 28 ימים עד סוף הנתונים.
#   - מוציאים גם הריון שבו תאריך הלידה המשוער (expected) חל בתוך חלון
#     המעקב או לפניו. זהו סינון שמרני לפי תאריך משוער, לא תאריך לידה בפועל.
#   - "לא חזר" פירושו שלא נמצאה מדידה נוספת בקובץ בתוך 28 יום;
#     אין בכך הוכחה שלא נעשתה בדיקה מחוץ למאגר.
#   - הבדיקה מזהה קשר אפשרי למנגנון מעקב/בחירה, ולא מוכיחה סיבתיות.
#   - יש לוודא שהשיטה שונה מהשיטה שנלמדה בכיתה, כנדרש במטלה.
#
# התוצאות נשמרות ב-week 3/output. הסעיף אינו משנה את dat.

clean <- dat[!dat$outlier & !dat$near_dup, ]

# 4.1 מדידת בסיס: המדידה הראשונה בשבועות 20-24 בכל הריון
baseline_pool <- clean[clean$pregweek >= 20 & clean$pregweek < 25, ]
baseline_pool <- baseline_pool[
  order(baseline_pool$preg_id, baseline_pool$check_d_sas), ]

first_idx <- !duplicated(baseline_pool$preg_id)
baseline <- baseline_pool[first_idx, ]

# 4.2 חלון מעקב מלא: 28 ימים עד סוף תקופת הנתונים
FOLLOWUP_DAYS <- 28
last_data_day <- max(dat$check_d_sas, na.rm = TRUE)

has_full_calendar_window <- baseline$check_d_sas + FOLLOWUP_DAYS <= last_data_day

# סינון שמרני נוסף: תאריך הלידה המשוער צריך להיות אחרי סוף חלון המעקב.
# אם expected חסר, לא ניתן לוודא את התנאי ולכן מוציאים את ההריון.
has_full_pregnancy_window <- !is.na(baseline$expected) &
  baseline$expected > baseline$check_d_sas + FOLLOWUP_DAYS

eligible <- baseline[has_full_calendar_window & has_full_pregnancy_window, ]

cat("Sampling-bias baseline:", nrow(baseline), "pregnancies |",
    "excluded: incomplete calendar window",
    sum(!has_full_calendar_window), "|",
    "excluded: expected delivery within follow-up window",
    sum(has_full_calendar_window & !has_full_pregnancy_window), "|",
    "eligible:", nrow(eligible), "\n")

# 4.3 ספי P10 ו-P90 בתוך שבוע הבסיס (על מדידות הבסיס בלבד)
week_cutoffs <- do.call(rbind, lapply(sort(unique(eligible$week)), function(w) {
  x <- eligible$bpd[eligible$week == w]
  data.frame(
    week = w,
    n_baseline = length(x),
    p10 = as.numeric(quantile(x, 0.10, na.rm = TRUE, names = FALSE)),
    p90 = as.numeric(quantile(x, 0.90, na.rm = TRUE, names = FALSE))
  )
}))

eligible$p10 <- week_cutoffs$p10[match(eligible$week, week_cutoffs$week)]
eligible$p90 <- week_cutoffs$p90[match(eligible$week, week_cutoffs$week)]

eligible$bpd_group <- ifelse(
  eligible$bpd < eligible$p10, "Low (<P10)",
  ifelse(eligible$bpd > eligible$p90, "High (>P90)", "Middle (P10-P90)")
)
eligible$bpd_group <- factor(
  eligible$bpd_group,
  levels = c("Low (<P10)", "Middle (P10-P90)", "High (>P90)")
)

# 4.4 האם קיימת מדידה נוספת בתוך 28 יום אחרי מדידת הבסיס?
followup_by_preg <- split(clean, clean$preg_id)

eligible$repeat_scan <- vapply(seq_len(nrow(eligible)), function(i) {
  p <- followup_by_preg[[as.character(eligible$preg_id[i])]]
  any(p$check_d_sas > eligible$check_d_sas[i] &
      p$check_d_sas <= eligible$check_d_sas[i] + FOLLOWUP_DAYS)
}, logical(1))

# 4.5 טבלת שיעורי חזרה לפי קבוצת BPD
sampling_tbl <- as.data.frame(
  table(eligible$bpd_group, eligible$repeat_scan),
  stringsAsFactors = FALSE
)
names(sampling_tbl) <- c("bpd_group", "repeat_scan", "pregnancies")
sampling_tbl$repeat_scan <- sampling_tbl$repeat_scan == "TRUE"

group_summary <- do.call(rbind, lapply(levels(eligible$bpd_group), function(g) {
  x <- eligible[eligible$bpd_group == g, ]
  data.frame(
    bpd_group = g,
    pregnancies = nrow(x),
    returned = sum(x$repeat_scan),
    return_rate_pct = round(100 * mean(x$repeat_scan), 1)
  )
}))
print(group_summary, row.names = FALSE)
save_table(group_summary, "04_sampling_bias_return_rates")
save_table(sampling_tbl, "04_sampling_bias_contingency")
save_table(week_cutoffs, "04_sampling_bias_week_cutoffs")

# 4.6 מבחן קשר: Chi-square; אם יש תאים צפויים קטנים, Fisher exact
contingency <- table(eligible$bpd_group, eligible$repeat_scan)
chi <- suppressWarnings(chisq.test(contingency, correct = FALSE))

if (any(chi$expected < 5)) {
  test_name <- "Fisher exact test"
  test_result <- fisher.test(contingency)
} else {
  test_name <- "Chi-square test"
  test_result <- chi
}

test_tbl <- data.frame(
  test = test_name,
  statistic = if (test_name == "Chi-square test") unname(test_result$statistic) else NA_real_,
  p_value = test_result$p.value,
  n = sum(contingency)
)
print(test_tbl, row.names = FALSE)
save_table(test_tbl, "04_sampling_bias_test")

# 4.7 גרף: שיעור ההריונות עם מדידת מעקב בכל קבוצת BPD
save_plot("04_sampling_bias_return_rates", function() {
  bar <- barplot(
    group_summary$return_rate_pct,
    # heb() לא מטפלת בשבירת שורה ובפסיקים, ולכן כל שורה נבנית בנפרד
    names.arg = paste0(heb(c("נמוך - מתחת ל-P10", "מרכזי", "גבוה - מעל P90")),
                       "\n", "n = ", group_summary$pregnancies),
    col = c(COL_REMOVE, COL_BULK, COL_KEEP),
    # הציר מקוצר לטווח הערכים (כ-8%-14%), כדי שההבדל בין הקבוצות ייראה במצגת
    border = NA, ylim = c(0, ceiling(max(group_summary$return_rate_pct) / 2) * 2 + 2), las = 1,
    ylab = heb("הריונות עם מדידת מעקב בתוך 4 שבועות (%)"),
    xlab = heb("קבוצת BPD במדידת הבסיס, שבועות 20-24"),
    main = heb("שיעור החזרה למדידה לפי קבוצת BPD")
  )
  overall <- 100 * mean(eligible$repeat_scan)
  abline(h = overall, lty = 2, col = "grey50")
  text(par("usr")[2], overall, heb(paste0("כלל ההריונות: ", round(overall, 1), "%")),
       adj = c(1, -0.4), cex = 0.85, col = "grey30")
  mtext(paste0("Chi-square: p = ", signif(test_result$p.value, 2)),
        side = 3, line = 0.3, cex = 0.85)
  text(bar, group_summary$return_rate_pct,
       labels = paste0(group_summary$return_rate_pct, "%"),
       pos = 3, cex = 1)
})

## ======================== 5. אימון מול בדיקה (חלק ד) ========================
# מטרה: לבדוק שהמשתנה BPD מתנהג בצורה דומה בקובץ האימון (2000-2007)
# ובקובץ הבדיקה (2008 והלאה), למשל ממוצע או חציון לפי שבוע.
# רץ על הנתונים אחרי ההסרות של סעיפים 2-3.
#
# השיטה (פירוט הנימוקים ב-week 3/README.md, סעיף 8):
#   1. סטנדרטיזציה לפי קובץ האימון בלבד: בכל שבוע מחשבים חציון ו-MAD של
#      BPD מהאימון, ומתקננים בעזרתם את שני הקבצים. אם כל קובץ היה מתוקנן לפי
#      עצמו, שניהם היו ממורכזים סביב 0 מעצם החישוב, והבדל אמיתי היה נעלם.
#      התקנון לפי שבוע מנטרל גם הבדל בהרכב השבועות בין הקבצים.
#   2. עקומות התפלגות (צפיפות) של הערך המתוקנן באימון מול בדיקה, בלי חלוקה
#      לטרימסטרים.
#   3. שיעור חריגה: אחוז המדידות מתחת לאחוזון 10 ומעל אחוזון 90 של האימון
#      (בכל שבוע). אם הקבצים דומים, בבדיקה צפויים כ-10% בכל צד.
#   4. מדווחים גודל אפקט, ולא רק ערך p (עם כ-57,000 מדידות כל הבדל זניח מובהק).
#      כל מדד בודק היבט אחר של ההבדל בין ההתפלגויות:
#        - הזזה (מיקום): ההבדל בחציון ובממוצע, בערך המתוקנן ובמ"מ, ו-d של Cohen,
#          שהוא ההבדל ביחידות של סטיית תקן (0.2 = אפקט קטן).
#        - פיזור: יחס ה-MAD. MAD ולא סטיית תקן, כי הוא חסין לזנבות.
#        - צורה: הסטטיסטי D של Kolmogorov-Smirnov, ההבדל המקסימלי בין
#          ההתפלגויות המצטברות. KS רגיש לכל סוג של הבדל ולא מניח נורמליות.
#          ערך ה-p מדווח כתוספת בלבד, והוא קירוב בגלל ערכים זהים (BPD שלם).
#        - שיעורי החריגה: המדד המעשי, כמה נשים יסומנו מתחת לאחוזון 10 של האימון.
#      ההנחה: התצפיות בלתי תלויות (לא בוחרים מדידה אחת לכל הריון), אף שיש
#      כמה מדידות לכל הריון.
#
# הסעיף לא משנה את dat; הוא רק מפיק תוצאות.

# החלוקה לאימון ובדיקה היא לפי הריון ולא לפי מדידה: כל הריון עובר בשלמותו לקובץ
# של השנה של המדידה הראשונה שלו (מבין המדידות שנשארו). אחרת 469 הריונות שעוברים
# מסוף 2007 ל-2008 היו מפוצלים בין הקבצים, וקובץ הבדיקה לא היה בלתי תלוי באימון.
# אותה חלוקה משמשת גם בסעיף 6.
keep <- !dat$outlier & !dat$near_dup
first_day <- ave(ifelse(keep, dat$check_d_sas, Inf), dat$preg_id, FUN = min)
dat$split_year <- ave(ifelse(keep & dat$check_d_sas == first_day, dat$year, Inf), dat$preg_id, FUN = min)
dat$set <- ifelse(dat$split_year %in% TRAIN_YEARS, "train",
                  ifelse(dat$split_year >= TEST_FROM & is.finite(dat$split_year), "test", NA))
cat("Scans moved by whole-pregnancy split:",
    sum(keep & dat$set == "train" & dat$year >= TEST_FROM), "into train |",
    sum(keep & dat$set == "test" & dat$year %in% TRAIN_YEARS), "into test\n")

cmp <- dat[keep & !is.na(dat$set), ]
cmp$set <- factor(cmp$set, levels = c("train", "test"))
is_tr <- cmp$set == "train"

# 5.1 ערכי ייחוס מהאימון בלבד, בכל שבוע
ref <- data.frame(week = sort(unique(cmp$week[is_tr])))
ref$med <- tapply(cmp$bpd[is_tr], cmp$week[is_tr], median)[as.character(ref$week)]
ref$mad <- tapply(cmp$bpd[is_tr], cmp$week[is_tr], mad)[as.character(ref$week)]
ref$p10 <- tapply(cmp$bpd[is_tr], cmp$week[is_tr], quantile, 0.10)[as.character(ref$week)]
ref$p90 <- tapply(cmp$bpd[is_tr], cmp$week[is_tr], quantile, 0.90)[as.character(ref$week)]
m <- match(cmp$week, ref$week)
cmp$z      <- (cmp$bpd - ref$med[m]) / ref$mad[m]
cmp$dev_mm <- cmp$bpd - ref$med[m]
cmp$below  <- cmp$bpd < ref$p10[m]
cmp$above  <- cmp$bpd > ref$p90[m]

# 5.2 גודל אפקט ושיעורי חריגה (רווח סמך 95% בהנחת תצפיות בלתי תלויות)
prop_ci <- function(x) { ci <- prop.test(sum(x), length(x))$conf.int; round(100 * ci, 1) }
ks <- suppressWarnings(ks.test(cmp$z[is_tr], cmp$z[!is_tr]))
effect_tbl <- do.call(rbind, lapply(levels(cmp$set), function(s) {
  x <- cmp[cmp$set == s, ]
  data.frame(set = s, scans = nrow(x),
             median_z = round(median(x$z), 3), median_dev_mm = round(median(x$dev_mm), 2),
             mean_z = round(mean(x$z), 3), mean_dev_mm = round(mean(x$dev_mm), 2),
             mad_z = round(mad(x$z), 3),
             pct_below_p10 = round(100 * mean(x$below), 1),
             ci_below = paste(prop_ci(x$below), collapse = "-"),
             pct_above_p90 = round(100 * mean(x$above), 1),
             ci_above = paste(prop_ci(x$above), collapse = "-"))
}))
effect_tbl$spread_ratio_vs_train <- round(effect_tbl$mad_z / effect_tbl$mad_z[1], 3)
# d של Cohen: הבדל הממוצעים חלקי סטיית התקן המשותפת (בערך המתוקנן)
sd_pooled <- sqrt(((sum(is_tr) - 1) * var(cmp$z[is_tr]) + (sum(!is_tr) - 1) * var(cmp$z[!is_tr])) /
                  (nrow(cmp) - 2))
effect_tbl$cohen_d_vs_train <- round(c(0, (mean(cmp$z[!is_tr]) - mean(cmp$z[is_tr])) / sd_pooled), 3)
print(effect_tbl, row.names = FALSE)
cat("Kolmogorov-Smirnov: D =", round(unname(ks$statistic), 3), "| p =", signif(ks$p.value, 3), "\n")
save_table(effect_tbl, "05_train_test_effect")
save_table(data.frame(test = "Kolmogorov-Smirnov (z, train vs test)",
                      D = unname(ks$statistic), p_value = ks$p.value,
                      n_train = sum(is_tr), n_test = sum(!is_tr)), "05_train_test_ks")

# שיעורי חריגה של קובץ הבדיקה בכל שבוע, והרכב השבועות בשני הקבצים
wk_cmp <- data.frame(week = ref$week,
  train_scans = as.vector(table(factor(cmp$week[is_tr], levels = ref$week))),
  test_scans  = as.vector(table(factor(cmp$week[!is_tr], levels = ref$week))))
wk_cmp$train_pct_of_file <- round(100 * wk_cmp$train_scans / sum(wk_cmp$train_scans), 1)
wk_cmp$test_pct_of_file  <- round(100 * wk_cmp$test_scans / sum(wk_cmp$test_scans), 1)
wk_cmp$test_pct_below_p10 <- round(100 * tapply(cmp$below[!is_tr], factor(cmp$week[!is_tr], levels = ref$week), mean), 1)
wk_cmp$test_pct_above_p90 <- round(100 * tapply(cmp$above[!is_tr], factor(cmp$week[!is_tr], levels = ref$week), mean), 1)
save_table(wk_cmp, "05_train_test_by_week")

# 5.3 גרף: עקומות ההתפלגות של הערך המתוקנן, אימון מול בדיקה
save_plot("05_train_test_density", function() {
  # רוחב החלקה משותף ורחב: כ-93% מערכי ה-BPD שלמים, ולכן הערך המתוקנן "קופץ" במדרגות.
  # רוחב אוטומטי שונה לכל קובץ היה מייצר גלים שונים ונראה כמו הבדל שאינו קיים.
  DENS_BW <- 0.3
  d_tr <- density(cmp$z[is_tr], bw = DENS_BW, from = -5, to = 5, n = 512)
  d_te <- density(cmp$z[!is_tr], bw = DENS_BW, from = -5, to = 5, n = 512)
  plot(d_tr, col = COL_BULK, lwd = 2.5, las = 1, xlim = c(-5, 5),
       ylim = c(0, max(d_tr$y, d_te$y) * 1.15), zero.line = FALSE,
       xlab = heb("BPD מתוקנן לפי החציון וה-MAD של קובץ האימון באותו שבוע"),
       ylab = heb("צפיפות"),
       main = heb("התפלגות ה-BPD אחרי סטנדרטיזציה לפי קובץ האימון"))
  lines(d_te, col = COL_REMOVE, lwd = 2.5, lty = 2)
  abline(v = 0, col = "grey70")
  te <- effect_tbl[effect_tbl$set == "test", ]
  mtext(paste0("mean shift = ", te$mean_z, " (", te$mean_dev_mm, " mm) | Cohen's d = ",
               te$cohen_d_vs_train, " | spread ratio = ", te$spread_ratio_vs_train,
               " | KS D = ", round(unname(ks$statistic), 3)),
        side = 3, line = 0.3, cex = 0.85)
  legend("topleft", bty = "n", lwd = 2.5, lty = c(1, 2), col = c(COL_BULK, COL_REMOVE),
         legend = heb(c("אימון 2000-2007", "בדיקה 2008 ואילך")))
  legend("topright", bty = "n", legend = c(
    paste0("n: train ", sum(is_tr), " | test ", sum(!is_tr)),
    heb("מתחת לאחוזון 10 של האימון:"),
    paste0("train ", effect_tbl$pct_below_p10[1], "% | test ", te$pct_below_p10, "%"),
    heb("מעל אחוזון 90 של האימון:"),
    paste0("train ", effect_tbl$pct_above_p90[1], "% | test ", te$pct_above_p90, "%")))
})

## ======================== 6. הקבצים הסופיים ================================
# הסרת הסימונים מסעיפים 2-3 וחלוקה לפי שנה.
# החלטות (פירוט ב-week 3/README.md, סעיף 9):
#   - שני הקבצים עוברים אותו ניקוי (חריגים ומדידות סמוכות): טעויות הקלדה היו
#     מעוותות את הערכת העקומה על קובץ הבדיקה, וסעיף 5 הראה שהקבצים דומים.
#   - עמודות העבודה מוסרות: bpd_rz ו-ratio_rz (חושבו על כל קובץ הבסיס ואין להן
#     משמעות בהמשך), outlier ו-near_dup (תמיד FALSE אחרי ההסרה). hc_mm נשאר.

#   - כל הריון עובר בשלמותו לקובץ של השנה של המדידה הראשונה שלו (העמודה set,
#     שנבנית בסעיף 5), כדי שאף הריון לא יופיע בשני הקבצים.

DROP_COLS <- c("bpd_rz", "ratio_rz", "outlier", "near_dup", "split_year", "set")

clean <- dat[!dat$outlier & !dat$near_dup, ]
clean <- clean[order(clean$preg_id, clean$check_d_sas), ]
train <- clean[clean$set %in% "train", setdiff(names(clean), DROP_COLS)]
test  <- clean[clean$set %in% "test",  setdiff(names(clean), DROP_COLS)]

final_tbl <- data.frame(
  file         = c("train.csv", "test.csv"),
  years        = c(paste(range(train$year), collapse = "-"), paste(range(test$year), collapse = "-")),
  scans        = c(nrow(train), nrow(test)),
  pregnancies  = c(length(unique(train$preg_id)), length(unique(test$preg_id))),
  women        = c(length(unique(train$snumber)), length(unique(test$snumber))),
  weeks        = c(paste(range(train$week), collapse = "-"), paste(range(test$week), collapse = "-")),
  columns      = ncol(train))
print(final_tbl, row.names = FALSE)
cat("Pregnancies split between train and test:",
    length(intersect(train$preg_id, test$preg_id)), "\n")
save_table(final_tbl, "06_final_files")

write.csv(train, file.path("week 3", "train.csv"), row.names = FALSE)
write.csv(test,  file.path("week 3", "test.csv"),  row.names = FALSE)

cat("Removed: ", sum(dat$outlier), " outliers, ", sum(dat$near_dup),
    " near-duplicate scans\n", sep = "")
cat("Train:", nrow(train), "scans | Test:", nrow(test), "scans |",
    "in neither file:", sum(is.na(clean$set)), "scans\n")
