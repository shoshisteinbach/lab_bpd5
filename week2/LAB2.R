library(dplyr)

original_data <- read.csv("/Users/danielpery/Lab/rawdata.csv") %>% filter(pregweek<=40)
clean_data <- original_data %>% select(pregweek,bpd) %>% filter(pregweek<=40)
num_women <- n_distinct(original_data$snumber)
num_preg <- n_distinct(original_data$snumber, original_data$preg_num)
count_women <- sum(is.na(original_data$snumber))
count_pregnancies <- original_data %>%                         ## take the data and save the final result back into "data"
  arrange(snumber, expected) %>%         ## sort rows by woman, then by due date, so each woman's scans are in order
  group_by(snumber) %>%                  ## from here on, calculations are done separately for each woman
  mutate(                                ## add new columns (same number of rows as before)
    new_preg = is.na(lag(expected)) |    ## TRUE if this is the woman's first row (no previous row to compare to)...
      (expected - lag(expected)) > 100,  ## ...OR if her due date jumped by more than 100 days since the previous row
    ## lag(expected) = the "expected" value from the row just above, within the same woman
    preg_seq = cumsum(new_preg)          ## running count of the TRUEs: 1 for her 1st pregnancy, 2 when a new one starts, etc.
  ) %>%
  ungroup() %>%                          ## remove the grouping, so later commands work on the whole dataset again
  mutate(preg_id = paste(snumber, preg_seq, sep = "_"))  ## build a unique pregnancy ID, e.g. "22064485_1", "22064485_2"

n_distinct(count_pregnancies$preg_id)                 ## count how many different pregnancy IDs exist = number of pregnancies

## data clean - remove all records in the low and upper curve that aren't in correct measurement sizes

library(dplyr)                                     ## load dplyr

cutoff <- sqrt(10)                                 ## ≈ 3.16: halfway (on log scale) between the main curve (x1) and the bands (x10, x0.1)

data_clean <- original_data %>%                             ## start from "data", save the result as "data_clean"
  filter(pregweek <= 40, !is.na(bpd)) %>%          ## keep scans up to week 40 that have a BPD value
  mutate(week = floor(pregweek)) %>%               ## whole week, to compare scans of the same age
  group_by(week) %>%                               ## calculate separately for each week
  mutate(bpd_ratio = bpd / median(bpd)) %>%        ## this BPD divided by the week's typical BPD (1 = on the main curve)
  ungroup() %>%                                    ## back to the whole table
  filter(bpd_ratio > 1 / cutoff,                   ## drop points more than ~3x BELOW typical (the lower band)...
         bpd_ratio < cutoff)                       ## ...and more than ~3x ABOVE typical (the upper band)

nrow(original_data %>% filter(pregweek <= 40, !is.na(bpd))) - nrow(data_clean)   ## number of outliers removed (expect 1,040)
data_for_test <- data_clean %>% select(pregweek,bpd)
head(data_for_test)