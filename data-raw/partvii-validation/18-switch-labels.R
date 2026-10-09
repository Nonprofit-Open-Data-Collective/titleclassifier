# 18-switch-labels.R
# A 990-EZ test set from organizations that switch between 990-EZ and the full
# 990 in consecutive years (FINDINGS.md F-029).
#
# A person who appears in both years carries a label for their 990-EZ year:
# the role step 09 gives them in the full-990 year, where the trustee, officer
# and key-employee boxes are available. The label assumes the role did not
# change across the switch. The strict subset therefore keeps people whose
# standardized title is the same in both years.
#
# Steps:
# 1. Pairs: consecutive tax years (2019-2023) where an organization's form
#    changes, from the Part I tables (one filing per organization-year, the
#    latest).
#    - Organizations in the calibration sample are excluded, so none trained
#      the no-checkbox model (16-nobox-model.R).
#    - Sample: 3,000 990-EZ -> 990 pairs and 1,500 990 -> 990-EZ pairs, seed 2026.
# 2. Run steps 01-09 on both filings of each pair.
# 3. Link each organization's people across its two years with synthid (as in
#    15-panel-synthid.R).
# 4. Score each person's 990-EZ-year role against the label:
#    - step 09 (the rules);
#    - the no-checkbox model.
#
# Writes PARTVII_DIR/sample/switch-labels.rds and
# panel-results/switch-*.csv.
#
#   Rscript data-raw/partvii-validation/18-switch-labels.R [workers]

source("tests/regression/regression-helpers.R"); tc_load_package()
suppressMessages({ library(data.table); library(arrow); library(xgboost); library(future); library(furrr); library(peopleparser) })
source("data-raw/partvii-validation/_nobox-features.R")
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
SYN <- normalizePath(Sys.getenv("SYNTHID_DIR", "../synthid"), winslash = "/")
RES <- "data-raw/partvii-validation/panel-results"
args <- commandArgs(trailingOnly = TRUE)
NW   <- if (length(args)) as.integer(args[1]) else max(1L, parallel::detectCores(logical = FALSE) - 1L)
years <- 2019:2023

# ---- 1. pairs ---------------------------------------------------------------
fr <- rbindlist(lapply(years, function(y) as.data.table(read_parquet(
  file.path(PV, "efile", sprintf("F9-P01-T00-SUMMARY-%d.parquet", y)),
  col_select = c("EIN2", "OBJECTID", "RETURN_TYPE", "RETURN_TIME_STAMP", "F9_01_EXP_TOT_CY")))[, year := y]))
setorder(fr, EIN2, year, -RETURN_TIME_STAMP); fr <- fr[!duplicated(fr[, .(EIN2, year)])]
fr[, exp := suppressWarnings(as.numeric(F9_01_EXP_TOT_CY))]
fr[, size := as.character(cut(exp, c(-Inf, 0.5, 150000, 1e6, 1e7, 1e8, Inf), right = FALSE,
                              labels = c("$0", "$1-150k", "$150k-1m", "$1m-10m", "$10m-100m", "$100m+")))]
fr[is.na(exp), size := "missing"]
train_orgs <- unique(readRDS(file.path(PV, "sample", "panel-sample-2019-2023.rds"))$EIN2)
pr <- merge(fr[, .(EIN2, year, f0 = RETURN_TYPE, o0 = OBJECTID)],
            fr[, .(EIN2, year = year - 1L, f1 = RETURN_TYPE, o1 = OBJECTID)], by = c("EIN2", "year"))
pr <- pr[f0 != f1 & !EIN2 %in% train_orgs]
set.seed(2026)
up   <- pr[f0 == "990EZ"][, .SD[sample(.N, 1)], by = EIN2][sample(.N, min(.N, 3000))]
down <- pr[f0 == "990" & !EIN2 %in% up$EIN2][, .SD[sample(.N, 1)], by = EIN2][sample(.N, min(.N, 1500))]
pairs <- rbind(up[, dir := "EZ -> 990"], down[, dir := "990 -> EZ"])
message(sprintf("switch pairs: %d (%d EZ -> 990, %d 990 -> EZ)", nrow(pairs), nrow(up), nrow(down)))

# ---- 2. steps 01-09 on both filings ---------------------------------------------
want <- rbind(pairs[, .(OBJECTID = o0, year)], pairs[, .(OBJECTID = o1, year = year + 1L)])
xw <- list(status = status.codes, xwalk = title.xwalk, taxonomy = title.taxonomy, patterns = title.patterns)
cl <- rbindlist(lapply(sort(unique(want$year)), function(y) {
  raw <- as.data.frame(read_parquet(file.path(PV, "efile", sprintf("F9-P07-T01-COMPENSATION-%d.parquet", y))))
  raw <- raw[raw$OBJECTID %in% want[year == y, OBJECTID], ]
  raw[] <- lapply(raw, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
  invisible(capture.output(o <- as.data.table(tc_run_pipeline(raw, xw))))
  o[, year := y]
}), fill = TRUE)
cl[fr, on = c(object.id = "OBJECTID"), `:=`(EIN2 = i.EIN2, size = i.size, return_type = i.RETURN_TYPE)]
message(sprintf("classified %d person-title rows in %d filings", nrow(cl), uniqueN(cl$object.id)))
saveRDS(cl, file.path(PV, "sample", "switch-classified.rds"))   # used by 19-nobox-model-v2.R

# ---- 3. link people across the two years (synthid) ------------------------------
p <- cl[role.primary == TRUE & !is.na(dtk.name) & nzchar(trimws(dtk.name))]
parsed <- peopleparser::parse_names(p$dtk.name)
pn <- data.table(ein = p$EIN2, org.name = p$org.name, taxyr = as.character(p$year), name = parsed$name,
                 OBJECTID = p$object.id, TABLE_ID = p$person.id, salutation = parsed$salutation,
                 first_name = parsed$first_name, middle_name = parsed$middle_name, last_name = parsed$last_name,
                 suffix = parsed$suffix, gender = parsed$gender, title.standard = p$title.standard)
batches <- split(unique(pn$ein), ceiling(seq_along(unique(pn$ein)) / 100))
plan(multisession, workers = NW)
lk <- rbindlist(future_map(batches, function(b) {
  Sys.setenv(OMP_NUM_THREADS = "1"); data.table::setDTthreads(1L)
  suppressMessages(devtools::load_all(SYN, quiet = TRUE, export_all = FALSE))
  data.table::as.data.table(synthid::link_panel(as.data.frame(pn[ein %in% b])))
}, .options = furrr_options(seed = TRUE, globals = c("pn", "SYN"), packages = c("data.table", "devtools"))), fill = TRUE)
plan(sequential)
p[lk, on = c(object.id = "OBJECTID", person.id = "TABLE_ID"), EMP_ID := i.EMP_ID]

# ---- 4. labels and scores ---------------------------------------------------------
f <- features(cl)[role.primary == TRUE]
fit <- xgb.load(file.path(PV, "sample", "nobox-model.json"))
prob <- predict(fit, xgb.DMatrix(mm(f))); if (is.null(dim(prob))) prob <- matrix(prob, ncol = length(classes), byrow = TRUE)
f[, model := classes[max.col(prob, ties.method = "first")]]
p[f, on = c("object.id", "person.id"), model := i.model]

ez  <- p[return_type == "990EZ" & !is.na(EMP_ID)]
ful <- p[return_type == "990" & !is.na(EMP_ID), .(EMP_ID, label = role.final, title_990 = title.standard,
                                                  board_990 = role.board)]
ful <- ful[!duplicated(EMP_ID)]
lab <- merge(ez[, .(EIN2, object.id, person.id, EMP_ID, year, size, title.raw, title.standard, tot.comp, tot.hours,
                    step09 = role.final, model)], ful, by = "EMP_ID")
lab[, same_title := !is.na(title.standard) & title.standard == title_990]
pairs[, `:=`(ez_obj = fifelse(f0 == "990EZ", o0, o1))]
lab[pairs, on = c(object.id = "ez_obj"), dir := i.dir]
saveRDS(lab, file.path(PV, "sample", "switch-labels.rds"))

sc <- function(x) x[, .(people = .N, step09 = round(100 * mean(step09 == label), 1), model = round(100 * mean(model == label), 1))]
res <- rbind(cbind(subset = "all linked", sc(lab)), cbind(subset = "same title (strict)", sc(lab[same_title == TRUE])),
             cbind(subset = "strict, non-board label", sc(lab[same_title == TRUE & label != "BOARD"])),
             cbind(subset = "strict, paid in EZ year", sc(lab[same_title == TRUE & as.numeric(tot.comp) > 0])))
res_dir  <- lab[same_title == TRUE, .(people = .N, step09 = round(100 * mean(step09 == label), 1), model = round(100 * mean(model == label), 1)), by = dir]
res_size <- lab[same_title == TRUE, .(people = .N, step09 = round(100 * mean(step09 == label), 1), model = round(100 * mean(model == label), 1)), by = size][order(size)]
fwrite(res, file.path(RES, "switch-scores.csv")); fwrite(res_size, file.path(RES, "switch-by-size.csv"))
cat(sprintf("\n990-EZ-year people linked to the full-990 year: %d (%d organizations)\n", nrow(lab), uniqueN(lab$EIN2)))
cat("agreement with the full-990-year role (%):\n"); print(res); print(res_dir); print(res_size)
pr2 <- function(col) t(sapply(classes, function(cl) { s <- lab[same_title == TRUE]; pp <- s[[col]] == cl; t <- s$label == cl
  c(precision = round(sum(pp & t) / max(sum(pp), 1), 2), recall = round(sum(pp & t) / max(sum(t), 1), 2), labeled = sum(t)) }))
cat("\nstrict set, by role -- model:\n"); print(pr2("model")); cat("step 09:\n"); print(pr2("step09"))
