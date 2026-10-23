# 25-ceo-cases.R
# For the CEO / board sample (24-ceo-sample.R, FINDINGS.md F-031):
# 1. run steps 01-09 of the current package on the 750 filings and keep the
#    output, which the labelers never see (PARTVII_DIR/ceo-sample/classified.rds);
# 2. write blind case files for the two labeling agents, one organization per
#    section with all five years of Part VII, in batches of 6 organizations
#    (ceo-sample/cases/batch-NN.md), and the row key that links each labeled
#    row back to the filing (ceo-sample/row-key.csv).
#
#   Rscript data-raw/partvii-validation/25-ceo-cases.R

source("tests/regression/regression-helpers.R"); tc_load_package()
suppressMessages(library(data.table))
PV   <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
OUT  <- file.path(PV, "ceo-sample")
REPO <- "data-raw/partvii-validation/ceo-sample"; dir.create(file.path(REPO, "cases"), showWarnings = FALSE, recursive = TRUE)
raw  <- readRDS(file.path(OUT, "raw.rds")); ctx <- readRDS(file.path(OUT, "orgs.rds"))
orgs <- ctx$orgs; fil <- as.data.table(ctx$filings)
# one Part I row per filing (a few filings have more than one)
fil <- fil[order(OBJECTID, is.na(F9_01_EXP_TOT_CY), is.na(F9_01_ACT_GVRN_ACT_MISSION))][!duplicated(OBJECTID)]
num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }
box <- function(x) ifelse(toupper(trimws(as.character(x))) %in% c("X", "1", "TRUE"), "Y", "-")

# ---- 1. classify (hidden from the labelers) ---------------------------------------------
r <- as.data.frame(raw)
r[] <- lapply(r, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
xw <- list(status = status.codes, xwalk = title.xwalk, taxonomy = title.taxonomy)
invisible(capture.output(out <- tc_run_pipeline(r, xw)))
saveRDS(out, file.path(OUT, "classified.rds"))

# ---- 2. row key and case files --------------------------------------------------------------
raw <- as.data.table(raw)
raw[fil, on = "OBJECTID", `:=`(org_id = i.org_id, form = i.form)]
raw[, `:=`(pay = num(F9_07_COMP_DTK_COMP_ORG), pay_rltd = num(F9_07_COMP_DTK_COMP_RLTD), pay_oth = num(F9_07_COMP_DTK_COMP_OTH),
           hrs = num(F9_07_COMP_DTK_AVE_HOUR_WEEK), hrs_rltd = num(F9_07_COMP_DTK_AVE_HOUR_WEEK_RL),
           person = fifelse(nzchar(fcoalesce(F9_07_COMP_DTK_NAME_PERS, "")), F9_07_COMP_DTK_NAME_PERS,
                            paste(fcoalesce(F9_07_COMP_DTK_NAME_ORG_L1, ""), fcoalesce(F9_07_COMP_DTK_NAME_ORG_L2, ""))))]
setorder(raw, org_id, year, -pay, -hrs)
raw[, row_id := sprintf("%s-%d-%02d", org_id, year, seq_len(.N)), by = .(org_id, year)]
key <- raw[, .(row_id, org_id, year, OBJECTID, EIN2, form, person, title_raw = F9_07_COMP_DTK_TITLE, pay, hrs)]
fwrite(key, file.path(REPO, "row-key.csv"))

money <- function(x) formatC(round(x), format = "d", big.mark = ",")
cell  <- function(x) gsub("\\|", "/", gsub("[\r\n]+", " ", trimws(fcoalesce(as.character(x), ""))))
case_md <- function(o) {
  g <- orgs[org_id == o]; f <- fil[org_id == o][order(year)]
  mission <- cell(tail(na.omit(f$F9_01_ACT_GVRN_ACT_MISSION[nzchar(fcoalesce(f$F9_01_ACT_GVRN_ACT_MISSION, ""))]), 1))
  sub <- if (is.na(g$subsection)) "unknown" else paste0("501(c)(", g$subsection, ")")
  lines <- c(sprintf("## %s  -  %s", o, cell(g$name)), "",
             sprintf("NTEE %s; %s.%s", fcoalesce(g$ntee, "unknown"), sub, if (length(mission) && nzchar(mission)) paste0(" Mission: ", mission) else ""), "")
  for (y in f$year) {
    fy <- f[year == y]; x <- raw[org_id == o & year == y]
    lines <- c(lines, sprintf("### %s  -  tax year %d, form %s; total expenses $%s; employees %s", o, y, fy$form,
                              if (is.na(fy$F9_01_EXP_TOT_CY)) "n/a" else money(num(fy$F9_01_EXP_TOT_CY)),
                              if (is.na(fy$F9_01_ACT_GVRN_EMPL_TOT)) "n/a" else fy$F9_01_ACT_GVRN_EMPL_TOT), "",
               "| row_id | name | title (raw) | pay | related pay | other | h/wk | rel h/wk | T | I | O | K | H | F |",
               "|---|---|---|---|---|---|---|---|---|---|---|---|---|---|",
               x[, sprintf("| %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s | %s |", row_id, cell(person), cell(F9_07_COMP_DTK_TITLE),
                           money(pay), money(pay_rltd), money(pay_oth), hrs, hrs_rltd,
                           if (form[1] == "990EZ") "-" else box(F9_07_COMP_DTK_POS_INDIV_TRUST_X), if (form[1] == "990EZ") "-" else box(F9_07_COMP_DTK_POS_INST_TRUST_X),
                           if (form[1] == "990EZ") "-" else box(F9_07_COMP_DTK_POS_OFF_X), if (form[1] == "990EZ") "-" else box(F9_07_COMP_DTK_POS_KEY_EMPL_X),
                           if (form[1] == "990EZ") "-" else box(F9_07_COMP_DTK_POS_HIGH_COMP_X), if (form[1] == "990EZ") "-" else box(F9_07_COMP_DTK_POS_FORMER_X))], "")
  }
  lines
}
ids <- sort(unique(orgs$org_id)); set.seed(7); ids <- sample(ids)   # mix the strata across batches
batches <- split(ids, ceiling(seq_along(ids) / 6))
for (b in seq_along(batches)) {
  o <- sort(batches[[b]]); n <- raw[org_id %in% o, .N]
  md <- c(sprintf("# CEO and board labeling: batch %02d (%d organizations, %d rows)", b, length(o), n), "",
          "Label every row following `../LABELING-GUIDE-ceo.md`. Columns T, I, O, K, H, F are the Part VII boxes:",
          "individual trustee, institutional trustee, officer, key employee, highly compensated, former. 990-EZ filings have no boxes (`-`).",
          "Pay is reportable compensation from this organization; related pay is from related organizations.", "",
          unlist(lapply(o, case_md)))
  writeLines(md, file.path(REPO, "cases", sprintf("batch-%02d.md", b)), useBytes = TRUE)
}
cat(sprintf("classified %d person-title rows; wrote %d batches; %d rows to label\n", nrow(out), length(batches), nrow(key)))
print(raw[, .(orgs = uniqueN(org_id), rows = .N), by = .(batch = match(org_id, unlist(batches)) %/% 6.0001 + 1)][order(batch)])
