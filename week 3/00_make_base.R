###############################################################################
#  מעבדה בסטטיסטיקה - קבוצת BPD - שבוע 3
#  יצירת קובץ הבסיס המשותף: base_data.csv
#
#  אותם שלבים כמו בשבוע 2 (week2/bpd_analysis.R, שלבים 1-4):
#    1. קריאת הנתונים
#    2. הגדרת הריונות
#    3. הגבלה לשבוע 40
#    4. תיקון יחידות מידה
#  + הסרת מדידות ללא BPD.
#  בלי ניקוי חריגים ובלי הסרת מדידות סמוכות - אלה החלקים של שבוע 3.
#
#  להריץ מתיקיית השורש של הפרויקט (שם נמצא rawdata.csv).
###############################################################################

DATA_PATH <- "rawdata.csv"
OUT_FILE  <- file.path("week3", "base_data.csv")

MAX_WEEK     <- 40     # הגבלה לגיל הריון <= 40 שבועות
NEW_PREG_GAP <- 100    # ימים בין תאריכי לידה משוערים שמגדירים הריון חדש

## 1. קריאה
raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE, na.strings = c("", "NA"))
dat <- raw

## 2. הגדרת הריונות: אותה אם + תאריכי לידה משוערים במרחק של עד NEW_PREG_GAP ימים
dat <- dat[order(dat$snumber, dat$expected, dat$check_d_sas), ]
new_preg <- c(TRUE, diff(dat$snumber) != 0 | diff(dat$expected) > NEW_PREG_GAP)
dat$preg_id <- cumsum(new_preg)

## 3. הגבלה לשבוע 40
dat <- dat[dat$pregweek <= MAX_WEEK, ]
dat$week <- pmin(floor(dat$pregweek), 39)     # שבוע שלם; יום 280 מצורף לשבוע 39

## 4. יחידות מידה (bpd_raw = הערך המקורי, bpd = במ"מ)
#   bpd < 15          -> נרשם בס"מ                  (x 10)
#   150 <= bpd < 1500 -> חסרה נקודה עשרונית        (/ 10)
#   bpd >= 1500       -> חסרות שתי ספרות עשרוניות  (/ 100)
bpd_factor <- with(dat, ifelse(is.na(bpd), NA,
                        ifelse(bpd < 15, 10,
                        ifelse(bpd >= 1500, 0.01,
                        ifelse(bpd >= 150, 0.1, 1)))))
dat$bpd_raw <- dat$bpd
dat$bpd     <- dat$bpd_raw * bpd_factor

## הסרת מדידות ללא BPD
dat <- dat[!is.na(dat$bpd), ]

write.csv(dat, OUT_FILE, row.names = FALSE)
cat("Saved", OUT_FILE, "|", nrow(dat), "scans |",
    length(unique(dat$preg_id)), "pregnancies |",
    length(unique(dat$snumber)), "women\n")
