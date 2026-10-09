# 17-nobox-gold.R
# Score the no-checkbox model (16-nobox-model.R) against the labeled set
# (gold-check/gold-v2.csv, 316 people from 2010-12 filings; FINDINGS.md F-028).
#
# The labeled people's filings are run through steps 01-09. Expense size comes
# from the 2010-2012 Part I tables (efile_v2_3, in PARTVII_DIR/efile). Each
# person then gets three roles:
# - model: the no-box model, which never sees the checkboxes;
# - step09: step 09 as it runs, using the boxes on a full 990;
# - step09_nobox: step 09 with the boxes hidden, as if on a 990-EZ.
# Each is compared with the label. DUAL accepts CEO, OFFICER or BOARD;
# UNSURE and NONE are not scored.
#
#   Rscript data-raw/partvii-validation/17-nobox-gold.R

source("tests/regression/regression-helpers.R"); tc_load_package()
suppressMessages({ library(data.table); library(xgboost); library(arrow) })
PV <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
S  <- Sys.getenv("SYNTHID_DEV", "../synthid/dev")
D  <- "data-raw/partvii-validation/gold-check"
source("data-raw/partvii-validation/_nobox-features.R")   # features(), mm(), classes
fit  <- xgb.load(file.path(PV, "sample", "nobox-model.json"))

raw <- readRDS(file.path(S, "data", "slice_2010_2012_1000eins.rds"))
raw[] <- lapply(raw, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
xw  <- list(status = status.codes, xwalk = title.xwalk, taxonomy = title.taxonomy, patterns = title.patterns)
invisible(capture.output(out <- as.data.table(tc_run_pipeline(raw, xw))))

# expense size and form, as in 12-panel-sample.R
p1 <- rbindlist(lapply(2010:2012, function(y) as.data.table(read_parquet(
  file.path(PV, "efile", sprintf("F9-P01-T00-SUMMARY-%d.parquet", y)), col_select = c("OBJECTID", "F9_01_EXP_TOT_CY", "RETURN_TYPE")))))
p1 <- p1[!duplicated(OBJECTID)]
out[p1, on = c(object.id = "OBJECTID"), `:=`(exp = suppressWarnings(as.numeric(i.F9_01_EXP_TOT_CY)), return_type = i.RETURN_TYPE)]
out[, size := as.character(cut(exp, c(-Inf, 0.5, 150000, 1e6, 1e7, 1e8, Inf), right = FALSE,
                               labels = c("$0", "$1-150k", "$150k-1m", "$1m-10m", "$10m-100m", "$100m+")))]
out[is.na(exp), size := "missing"]
out[is.na(return_type), return_type := formtype]

# the three roles
f <- features(out)
f$model <- classes[max.col({ pr <- predict(fit, xgb.DMatrix(mm(f))); if (is.null(dim(pr))) matrix(pr, ncol = length(classes), byrow = TRUE) else pr }, ties.method = "first")]
hid <- copy(out)
for (b in c("dtk.indiv.trustee.x", "dtk.inst.trustee.x", "dtk.officer.x", "dtk.key.empl.x", "dtk.high.comp.x")) set(hid, j = b, value = 0)
hid[, formtype := "990EZ"]
hid <- as.data.table(resolve_roles(as.data.frame(hid[, !c("role.position", "role.final", "role.board", "role.ceo", "role.source", "role.primary", "org.leadership")])))
f[hid, on = c("object.id", "person.id", "title.order"), step09_nobox := i.role.final]

# match the labeled people (filing + raw title + pay), as 09-gold-check.R does
k <- function(o, t, c) paste(o, toupper(trimws(t)), round(as.numeric(c)))
f[, .k := k(object.id, title.raw, tot.comp)]
v <- fread(file.path(D, "gold-v2.csv"), colClasses = "character", na.strings = NULL)
v[, .k := k(OBJECTID, title.raw, comp)]
one <- function(col) vapply(v$.k, function(key) { r <- unique(f[[col]][f$.k == key]); if (length(r) == 1) r else NA_character_ }, "")
v[, `:=`(model = one("model"), step09 = one("role.final"), step09_nobox = one("step09_nobox"))]
hit <- function(lab, pr) ifelse(lab == "DUAL", pr %in% c("CEO", "OFFICER", "BOARD"), lab == pr)
e <- v[!role %in% c("UNSURE", "NONE") & !is.na(model) & !is.na(step09)]
e[, `:=`(h_model = hit(role, model), h_step09 = hit(role, step09), h_nobox = hit(role, step09_nobox))]

s <- function(x) x[, .(people = .N, model = round(100 * mean(h_model), 1), step09 = round(100 * mean(h_step09), 1),
                       step09_nobox = round(100 * mean(h_nobox), 1))]
res <- rbind(cbind(group = "all", s(e)), cbind(group = "full 990", s(e[formtype == "990"])),
             cbind(group = "990-EZ", s(e[formtype == "990EZ"])), cbind(group = "august set", s(e[set == "august"])),
             cbind(group = "new set", s(e[set == "new"])))
cat("agreement with the labels (%): model without boxes | step 09 (boxes where present) | step 09 boxes hidden\n")
print(res)
fwrite(res, "data-raw/partvii-validation/panel-results/nobox-gold.csv")
pr2 <- function(col) t(sapply(classes, function(cl) { p <- e[[col]] == cl; t <- e$role == cl
  c(precision = round(sum(p & t) / sum(p), 2), recall = round(sum(p & t) / sum(t), 2)) }))
cat("\nmodel by role:\n"); print(pr2("model")); cat("\nstep 09 by role:\n"); print(pr2("step09"))
cat("\n990-EZ: label vs model\n"); print(e[formtype == "990EZ", table(label = role, model = model)])
cat("\nwhere model and step 09 disagree, who matches the label:\n")
print(e[model != step09, .N, by = .(model_right = h_model, step09_right = h_step09)][order(-N)])
