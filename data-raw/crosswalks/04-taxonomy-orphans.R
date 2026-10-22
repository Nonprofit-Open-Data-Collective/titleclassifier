# 04-taxonomy-orphans.R
# Remove taxonomy rows that no title can reach: standards that no row of
# title-standardization.csv or title-patterns.csv maps to. Most are left over
# from merging a title into a broader standard (e.g. CFO is now a variant of
# CHIEF FINANCIAL OFFICER). Logs each removed row in taxonomy-orphans-log.csv
# and rebuilds the package data.
#
#   Rscript data-raw/crosswalks/04-taxonomy-orphans.R

suppressMessages(library(data.table))
source("data-raw/crosswalks/_crosswalk-io.R")
tx <- read_xwalk("title-taxonomy")
xw <- read_xwalk("title-standardization")
pt <- read_xwalk("title-patterns")

orphan <- tx[!title.standard %in% c(xw$title.standard, pt$title.standard)]
message(sprintf("taxonomy: %d rows, %d unreachable", nrow(tx), nrow(orphan)))
if (!nrow(orphan)) quit(save = "no")

log <- copy(orphan)
log[, now.variant.of := xw$title.standard[match(title.standard, xw$title.variant)]]
log[is.na(now.variant.of), now.variant.of := ""]
log[, removed := as.character(Sys.Date())]
lp <- file.path(xwalk_dir, "taxonomy-orphans-log.csv")
fwrite(if (file.exists(lp)) rbind(read_xwalk("taxonomy-orphans-log"), log) else log, lp, quote = TRUE)

write_xwalk(tx[!title.standard %in% orphan$title.standard], "title-taxonomy")
rebuild_xwalks()
