# 27-ceo-rerun.R
# Re-run the working-tree package (steps 01-09) on the 750 filings of the CEO /
# board label set (24-ceo-sample.R, FINDINGS.md F-031, F-032) and save the
# output for scoring:
#
#   Rscript data-raw/partvii-validation/27-ceo-rerun.R <tag>
#   CLASSIFIED=<PARTVII_DIR>/ceo-sample/classified-<tag>.rds Rscript data-raw/partvii-validation/26-ceo-score.R
#
# The tag names the variant being tested (e.g. "main", "board-flags").

source("tests/regression/regression-helpers.R"); tc_load_package()
suppressMessages(library(data.table))
tag <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(tag)) tag <- "rerun"
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
OUT <- file.path(PV, "ceo-sample")
r <- as.data.frame(readRDS(file.path(OUT, "raw.rds")))
r[] <- lapply(r, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
xw <- list(status = status.codes, xwalk = title.xwalk, taxonomy = title.taxonomy)
invisible(capture.output(out <- tc_run_pipeline(r, xw)))
f <- file.path(OUT, sprintf("classified-%s.rds", tag))
saveRDS(out, f)
cat("wrote", f, "\n")
