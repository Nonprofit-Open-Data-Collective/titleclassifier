# 00-check-equivalence.R
# Check that the unique-title shortcut (_steps.R) gives the same TitleTxt7 and
# title.standard as the full pipeline, on the 2023 demo data (4,000 rows), and
# time steps 1-6 on a sample of panel titles.
#   Rscript data-raw/partvii-validation/00-check-equivalence.R

suppressMessages(devtools::load_all(quiet = TRUE))
source("data-raw/partvii-validation/_config.R")
source("data-raw/partvii-validation/_steps.R")
library(data.table)

sc <- get_googlesheets_status_codes(); xw <- get_googlesheets_title_xwalk()
demo <- fread("data-raw/demo/partvii-demo-2023.csv", colClasses = "character")

# full pipeline, steps 1-7
utils::capture.output(full <- demo |> as.data.frame() |> standardize_df() |> remove_dates() |> standardize_conj() |>
  split_titles() |> standardize_spelling() |> gen_status_codes(gs_status_codes = sc) |>
  standardize_titles(gs_title_xwalk = xw))
full <- as.data.table(full)[, .(TITLE_RAW = fifelse(is.na(TITLE_RAW), "", TITLE_RAW), TitleTxt7, title.standard)]

# shortcut: steps 1-6 per distinct raw title, step 7 per (title x step-7 inputs)
d <- copy(demo)
d[, TITLE_RAW := fifelse(is.na(F9_07_COMP_DTK_TITLE), "", F9_07_COMP_DTK_TITLE)]
utils::capture.output(s <- standardize_df(as.data.frame(demo)))
d[, `:=`(OFFICER_X = s$F9_07_COMP_DTK_POS_OFF_X, PAY_GT0 = s$TOT.COMP > 0, HRS_GT40 = s$TOT.HOURS > 40)]
raw <- unique(d$TITLE_RAW)
t16 <- as.data.table(steps_1_6(raw, sc))
k <- merge(d[, .(TITLE_RAW, OFFICER_X, PAY_GT0, HRS_GT40)], t16, by = "TITLE_RAW", allow.cartesian = TRUE)
short <- as.data.table(step_7(as.data.frame(k), xw))[, .(TITLE_RAW, TitleTxt7, title.standard)]

key <- function(x) x[, .N, by = .(TITLE_RAW, TitleTxt7, title.standard)][order(TITLE_RAW, TitleTxt7, title.standard)]
a <- key(full); b <- key(short)
cat(sprintf("full pipeline rows %d, shortcut rows %d\n", nrow(full), nrow(short)))
cat("identical (title, TitleTxt7, title.standard) counts: ", isTRUE(all.equal(a, b, check.attributes = FALSE)), "\n")
if (!isTRUE(all.equal(a, b, check.attributes = FALSE))) print(fsetdiff(rbind(a, b), fintersect(a, b))[1:20])

# timing on panel titles
tr <- arrow::read_parquet(file.path(pv_data, "titles-raw.parquet"))
set.seed(1); smp <- sample(tr$TITLE_RAW, 20000)
t0 <- Sys.time(); x <- steps_1_6(smp, sc); el <- as.numeric(Sys.time() - t0, units = "secs")
cat(sprintf("steps 1-6: %d titles in %.0f s (%.1f ms/title, one core)\n", length(smp), el, 1000 * el / length(smp)))
