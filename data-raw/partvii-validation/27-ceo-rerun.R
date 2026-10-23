# 27-ceo-rerun.R
# Re-run the working-tree package (steps 01-09) on the 750 filings of the CEO /
# board label set (24-ceo-sample.R, FINDINGS.md F-031, F-032) and save the
# output for scoring:
#
#   Rscript data-raw/partvii-validation/27-ceo-rerun.R <tag>
#   CLASSIFIED=<PARTVII_DIR>/ceo-sample/classified-<tag>.rds Rscript data-raw/partvii-validation/26-ceo-score.R
#
# The tag names the variant being tested (e.g. "main", "board-flags").
# CEO_SET=ceo-holdout reruns the held-out set instead. CODES=1 joins the BMF
# NTEE code and 501(c) subsection (PARTVII_DIR/bmf/bmf-ntee.rds) to the rows,
# so that step 09 takes the org type from the codes rather than the name.

source("tests/regression/regression-helpers.R"); tc_load_package()
suppressMessages(library(data.table))
tag <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(tag)) tag <- "rerun"
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
SET <- Sys.getenv("CEO_SET", "ceo-sample")
OUT <- file.path(PV, SET)
r <- as.data.frame(readRDS(file.path(OUT, "raw.rds")))
if (Sys.getenv("CODES") == "1") {
  bmf <- unique(as.data.table(readRDS(file.path(PV, "bmf", "bmf-ntee.rds")))[, .(EIN2, ntee_code_clean, subsection_code)], by = "EIN2")
  i <- match(r$EIN2, bmf$EIN2)   # keep the row order: ties in pay are broken by it
  r$ntee_code_clean <- bmf$ntee_code_clean[i]; r$subsection_code <- bmf$subsection_code[i]
}
r[] <- lapply(r, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
xw <- list(status = status.codes, xwalk = title.xwalk, taxonomy = title.taxonomy)
invisible(capture.output(out <- tc_run_pipeline(r, xw)))
f <- file.path(OUT, sprintf("classified-%s.rds", tag))
saveRDS(out, f)
cat("wrote", f, "\n")
