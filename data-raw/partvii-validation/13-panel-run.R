# 13-panel-run.R
# Run steps 01-09 on the 2019-2023 calibration sample (12-panel-sample.R,
# FINDINGS.md F-025). For each year:
# - pull the sampled filings' Part VII rows from the efile_v2_3 Parquet table;
# - run the working-tree pipeline with the package crosswalks;
# - save the result, with the sample's design columns, to
#   PARTVII_DIR/sample/classified-<year>.rds.
# A year that is already done is skipped, so the script can be re-run after an
# interruption.
#
#   Rscript data-raw/partvii-validation/13-panel-run.R

source("tests/regression/regression-helpers.R"); tc_load_package()
suppressMessages({ library(arrow); library(data.table) })
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
smp <- readRDS(file.path(PV, "sample", "panel-sample-2019-2023.rds"))
xw  <- list(status = status.codes, xwalk = title.xwalk, taxonomy = title.taxonomy)

for (y in sort(unique(smp$year))) {
  f <- file.path(PV, "sample", sprintf("classified-%d.rds", y))
  if (file.exists(f)) { message(y, ": done already"); next }
  s   <- smp[year == y]
  raw <- as.data.frame(read_parquet(file.path(PV, "efile", sprintf("F9-P07-T01-COMPENSATION-%d.parquet", y))))
  raw <- raw[raw$OBJECTID %in% s$OBJECTID, ]
  raw[] <- lapply(raw, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  t0 <- Sys.time()
  invisible(capture.output(out <- tc_run_pipeline(raw, xw)))
  out <- as.data.table(out)
  out[s, on = c(object.id = "OBJECTID"),
      `:=`(EIN2 = i.EIN2, design = i.design, stratum = i.stratum, weight = i.weight,
           size = i.size, paid_bin = i.paid_bin, return_type = i.RETURN_TYPE)]
  saveRDS(out, f)
  message(sprintf("%d: %d filings, %d Part VII rows -> %d person-title rows in %.1f min", y,
                  nrow(s), nrow(raw), nrow(out), as.numeric(Sys.time() - t0, units = "mins")))
}
