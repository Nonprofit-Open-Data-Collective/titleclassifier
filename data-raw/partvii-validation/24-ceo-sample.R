# 24-ceo-sample.R
# Draw the CEO / board labeling sample (FINDINGS.md F-031): 150 organizations
# that filed a 990 or 990-EZ with Part VII rows in each of 2019-2023, in 14
# strata built for the CEO problem (data-dev notes in F-031):
#
#   the org types named for the CEO thesaurus - museums, universities, lodges,
#   ministries, orchestras, fire departments, veterinary associations;
#   the types where step 09 finds the CEO worst - unions (501(c)(5)), trade
#   associations (c6), clubs and co-ops (c7, c12), benefit trusts and
#   cemeteries (c9, c13), veterans posts (c19);
#   multi-CEO systems - hospitals, charter schools, senior centers whose
#   Part VII lists two or more chief-executive titles;
#   and ordinary charities as a control.
#
# Org type comes from the NCCS unified BMF (NTEE code, 501(c) subsection;
# panel990::bmf_retrieve(), cached in PARTVII_DIR/bmf) and the org name.
# 10 organizations per stratum and 20 controls; seed 2026. Within a stratum
# half are drawn from organizations that pay someone $25k+ for 20+ hours a week
# in at least one year, half from the rest that pay anyone. Filings with more
# than 60 Part VII rows (100 for multi-CEO systems) are left out so a case
# stays readable.
#
# Writes PARTVII_DIR/ceo-sample/: sample.csv (one row per organization),
# raw.rds (the Part VII rows of the 750 filings) and orgs.rds (BMF and Part I
# context). The list of organizations is also kept in the repository,
# ceo-sample/sample.csv.
#
#   Rscript data-raw/partvii-validation/24-ceo-sample.R
#
# CEO_SET=ceo-holdout draws the held-out check of the CEO thesaurus (F-033):
# 8 organizations in each of the 8 strata the thesaurus covers, none of them in
# the first sample, seed 2027, with the 2021 and 2023 filings only. It writes
# to PARTVII_DIR/ceo-holdout and ceo-holdout/ instead.

suppressMessages({ library(arrow); library(data.table) })
PV   <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
SET  <- Sys.getenv("CEO_SET", "ceo-sample"); holdout <- SET != "ceo-sample"
OUT  <- file.path(PV, SET); dir.create(OUT, showWarnings = FALSE)
REPO <- file.path("data-raw/partvii-validation", SET); dir.create(REPO, showWarnings = FALSE)
years <- 2019:2023
keep_years <- if (holdout) c(2021L, 2023L) else years   # the filings written for labeling
num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }

# ---- 1. one filing per organization and year -------------------------------------
cols <- c("EIN2", "OBJECTID", "RETURN_TYPE", "RETURN_TIME_STAMP", "RETURN_AMENDED_X", "ORG_NAME_L1", "F9_07_COMP_DTK_TITLE",
          "F9_07_COMP_DTK_COMP_ORG", "F9_07_COMP_DTK_AVE_HOUR_WEEK")
fl <- rbindlist(lapply(years, function(y) {
  x <- as.data.table(read_parquet(file.path(PV, "efile", sprintf("F9-P07-T01-COMPENSATION-%d.parquet", y)), col_select = all_of(cols)))
  x[, `:=`(comp = num(F9_07_COMP_DTK_COMP_ORG), hrs = num(F9_07_COMP_DTK_AVE_HOUR_WEEK), title = toupper(F9_07_COMP_DTK_TITLE))]
  x[, .(year = y, rows = .N, paid = sum(comp > 0), exec = any(comp >= 25000 & hrs >= 20),
        ceo_titles = sum(grepl("\\bCEO\\b|CHIEF EXECUTIVE|EXECUTIVE DIRECTOR|HEAD OF SCHOOL|\\bPRINCIPAL\\b|\\bPRESIDENT\\s*(&|AND|/)\\s*CEO", title)),
        name = ORG_NAME_L1[1], type = RETURN_TYPE[1], stamp = RETURN_TIME_STAMP[1]), by = .(EIN2, OBJECTID)]
}))
setorder(fl, EIN2, year, -stamp)
fl <- fl[!duplicated(fl[, .(EIN2, year)])]            # the latest filing of each year
org <- fl[, .(n_years = .N, max_rows = max(rows), any_paid = any(paid > 0), exec = any(exec), multi_ceo = any(ceo_titles >= 2),
              name = name[.N], forms = paste(sort(unique(type)), collapse = "/")), by = EIN2]
org <- org[n_years == length(years) & any_paid]
cat(sprintf("organizations with Part VII rows in all of %d-%d and anyone paid: %d\n", min(years), max(years), nrow(org)))

# ---- 2. org type from the BMF and the name -----------------------------------------------
bmf <- readRDS(file.path(PV, "bmf", "bmf-ntee.rds"))
bmf <- unique(as.data.table(bmf), by = "EIN2")
org[bmf, on = "EIN2", `:=`(ntee = i.ntee_code_clean, subsection = as.integer(i.subsection_code))]
org[ntee == "UNDEFINED", ntee := NA]
nm <- toupper(org$name)
n3 <- substr(fcoalesce(org$ntee, ""), 1, 3); n1 <- substr(n3, 1, 1)
sub <- fcoalesce(org$subsection, -1L)
support <- "FOUNDATION|AUXILIARY|ALUMNI|FRIENDS OF|RELIEF|BENEVOLENT|BOOSTER|SCHOLARSHIP|EARLY|CHILD"
org[, stratum := fcase(
  grepl("VETERINAR", nm) & (sub == 6L | grepl("ASSOCIATION|SOCIETY|ACADEMY|COLLEGE OF|ASSN", nm)), "veterinary association",
  n3 == "A69" | grepl("ORCHESTRA|SYMPHON|PHILHARMONIC", nm),                     "orchestra",
  substr(n3, 1, 2) == "A5" | grepl("\\bMUSEUM", nm),                             "museum",
  n3 == "M24" | grepl("FIRE (DEPT|DEPARTMENT|COMPANY|CO|DISTRICT|PROTECTION)\\b|VOLUNTEER FIRE|FIREMEN|FIRE RESCUE|HOSE CO", nm) &
    !grepl(support, nm),                                                         "fire department",
  # two-year colleges, colleges and universities (B41-B43); B40 and B50 only
  # when named a college, university or seminary (not "college of physicians")
  !grepl(support, nm) & (n3 %in% c("B41", "B42", "B43") |
    n3 %in% c("B40", "B50", "") & sub == 3 & grepl("\\bUNIVERSITY\\b|\\bCOLLEGE\\b(?! OF)|\\bSEMINARY\\b", nm, perl = TRUE)), "university",
  (n3 %in% c("E20", "E21", "E22", "E24", "B29", "P75") | sub == 14) & multi_ceo, "multi-CEO system",
  sub %in% c(8L, 10L),                                                           "lodge",
  sub == 19L | sub == 23L,                                                       "veterans post",
  # 501(c)(5) also covers farm and horticulture groups (NTEE K): unions only
  sub == 5L & (n1 == "J" | n1 == "" & grepl("UNION|LOCAL|BROTHERHOOD|WORKERS|FEDERATION|EMPLOYEES|TEAMSTERS|GUILD|COUNCIL|ASSOCIATION", nm)), "union",
  sub == 6L,                                                                     "trade association",
  sub %in% c(7L, 12L),                                                           "club or co-op",
  sub %in% c(9L, 13L),                                                           "benefit trust or cemetery",
  n1 == "X" | grepl("CHURCH|MINISTR|CONGREGATION|SYNAGOGUE|\\bTEMPLE\\b|PARISH|DIOCESE", nm), "ministry",
  sub == 3L,                                                                     "control charity",
  default = NA_character_)]
print(org[, .(orgs = .N, with_exec = sum(exec), max_rows_ok = sum(max_rows <= 60)), by = stratum][order(orgs)])

# ---- 3. draw ---------------------------------------------------------------------------
set.seed(if (holdout) 2027 else 2026)
th_strata <- c("veterans post", "lodge", "union", "club or co-op", "benefit trust or cemetery", "university", "ministry", "fire department")
if (holdout) {
  first <- fread("data-raw/partvii-validation/ceo-sample/sample.csv", colClasses = "character")
  org <- org[!EIN2 %in% first$EIN2 & stratum %in% th_strata]
}
cap <- function(s) if (s == "multi-CEO system") 100 else 60
take <- function(s, n) {
  pool <- org[stratum == s & max_rows <= cap(s)]
  a <- pool[exec == TRUE]; b <- pool[exec == FALSE]
  na <- min(nrow(a), ceiling(n / 2)); nb <- min(nrow(b), n - na); na <- min(nrow(a), n - nb)
  rbind(a[sample(.N, na)], b[sample(.N, nb)])
}
n_of <- function(s) if (holdout) 8 else if (s == "control charity") 20 else 10
smp <- rbindlist(lapply(sort(unique(na.omit(org$stratum))), function(s) take(s, n_of(s))))
smp[, org_id := sprintf(if (holdout) "H%03d" else "O%03d", seq_len(.N))]
cat(sprintf("\nsample: %d organizations\n", nrow(smp))); print(smp[, .(orgs = .N, exec = sum(exec)), by = stratum])

# ---- 4. the filings' Part VII rows and context ------------------------------------------
fls <- fl[smp[, .(EIN2, org_id, stratum)], on = "EIN2"][year %in% keep_years]
raw <- rbindlist(lapply(keep_years, function(y) {
  x <- as.data.table(read_parquet(file.path(PV, "efile", sprintf("F9-P07-T01-COMPENSATION-%d.parquet", y))))
  x[OBJECTID %in% fls[year == y, OBJECTID]][, year := y]
}), fill = TRUE)
p1 <- rbindlist(lapply(keep_years, function(y) {
  a <- as.data.table(read_parquet(file.path(PV, "efile", sprintf("F9-P01-T00-SUMMARY-%d.parquet", y)),
        col_select = c("OBJECTID", "F9_01_ACT_GVRN_ACT_MISSION", "F9_01_ACT_GVRN_EMPL_TOT", "F9_01_ACT_GVRN_VOL_TOT", "F9_01_EXP_TOT_CY")))
  a[OBJECTID %in% fls$OBJECTID]
}), fill = TRUE)
ez <- rbindlist(lapply(keep_years, function(y) {
  f <- file.path(PV, "efile", sprintf("F9-P01-T00-SUMMARY-EZ-%d.parquet", y))
  a <- as.data.table(read_parquet(f)); a <- a[OBJECTID %in% fls$OBJECTID]
  e <- grep("EXP_TOT", names(a), value = TRUE)[1]
  a[, .(OBJECTID, F9_01_EXP_TOT_CY = if (!is.na(e)) get(e) else NA)]
}), fill = TRUE)
ctx <- merge(fls[, .(org_id, EIN2, year, OBJECTID, form = type, rows)], rbind(p1, ez, fill = TRUE), by = "OBJECTID", all.x = TRUE)
orgs <- merge(smp[, .(org_id, EIN2, stratum, name, ntee, subsection, forms, exec, multi_ceo)],
              bmf[, .(EIN2, nteev2, nteev2_subsector, state = NULL)], by = "EIN2", all.x = TRUE)
saveRDS(raw, file.path(OUT, "raw.rds")); saveRDS(list(orgs = orgs, filings = ctx), file.path(OUT, "orgs.rds"))
fwrite(orgs[order(org_id), .(org_id, EIN2, stratum, name, ntee, subsection, forms, exec_paid = exec, multi_ceo_titles = multi_ceo)],
       file.path(REPO, "sample.csv"))
fwrite(orgs[order(org_id)], file.path(OUT, "sample.csv"))
cat(sprintf("filings: %d; Part VII rows: %d\n", uniqueN(raw$OBJECTID), nrow(raw)))
