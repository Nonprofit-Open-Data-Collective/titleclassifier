# 03-title-profiles.R
# One profile per standardized title (TitleTxt7, the title the crosswalk is
# matched on): how often it occurs, who holds it (Part VII checkboxes, pay,
# hours), the raw titles that produce it, where the crosswalk sends it, and
# automatic checks that flag a mapping worth inspecting.
#
# Writes to <PARTVII>/data:
#   title-profiles.parquet   one row per TitleTxt7
#   title-variants.parquet   TitleTxt7 x raw title (with the step-by-step trace)
#   title-examples.parquet   up to 5 example filings per TitleTxt7
#   Rscript data-raw/partvii-validation/03-title-profiles.R

source("data-raw/partvii-validation/_config.R")
suppressMessages({library(data.table); library(DBI)})
suppressMessages(devtools::load_all(quiet = TRUE))

con <- dbConnect(duckdb::duckdb(shared_home = FALSE))
dbExecute(con, sprintf("SET temp_directory = '%s'", file.path(pv_data, "duckdb_tmp")))
pc <- sprintf("read_parquet('%s/panel-classified/*/*.parquet', hive_partitioning = true)", pv_data)
dbExecute(con, sprintf("CREATE VIEW pc AS SELECT * FROM %s", pc))

# ---- profile per TitleTxt7 ------------------------------------------------------
prof <- as.data.table(dbGetQuery(con, "
  SELECT TitleTxt7,
         any_value(\"title.standard\") AS title_standard, any_value(\"title.match\") AS title_match,
         count(DISTINCT \"title.standard\") AS n_standards,
         count(*) AS n_rows, count(DISTINCT OBJECTID) AS n_filings, count(DISTINCT EIN2) AS n_orgs,
         min(TAX_YEAR) AS first_year, max(TAX_YEAR) AS last_year,
         avg((RETURN_TYPE = '990EZ')::INT) AS pct_ez,
         avg(OFFICER_X) AS pct_officer, avg(TRUST_INDIV_X) AS pct_trustee, avg(TRUST_INST_X) AS pct_trustee_inst,
         avg(KEY_EMPL_X) AS pct_key_empl, avg(HIGH_COMP_X) AS pct_high_comp, avg(FORMER_X) AS pct_former_box,
         avg((TOT_COMP > 0)::INT) AS pct_paid,
         median(TOT_COMP) FILTER (WHERE TOT_COMP > 0) AS med_pay_paid,
         median(TOT_HOURS) AS med_hours, avg((TOT_HOURS >= 35)::INT) AS pct_fulltime,
         avg((\"Num.Titles\" > 1)::INT) AS pct_second_title,
         avg(\"FORMER.X\"::INT) AS pct_former, avg(\"INTERIM.X\"::INT) AS pct_interim, avg(\"FOUNDER.X\"::INT) AS pct_founder,
         avg(\"EXOFFICIO.X\"::INT) AS pct_exofficio, avg(\"PARTIAL.X\"::INT) AS pct_partial, avg(\"CO.X\"::INT) AS pct_co,
         avg((TitleTxt6 IS DISTINCT FROM TitleTxt7)::INT) AS pct_step7_changed
  FROM pc GROUP BY TitleTxt7"))
yrs <- as.data.table(dbGetQuery(con, "SELECT TitleTxt7, TAX_YEAR, count(*) n FROM pc GROUP BY ALL"))
yrs <- yrs[order(TAX_YEAR), .(rows_by_year = paste0(TAX_YEAR, ":", n, collapse = " ")), by = TitleTxt7]
prof <- merge(prof, yrs, by = "TitleTxt7", all.x = TRUE)
prof[is.na(TitleTxt7), TitleTxt7 := ""]

# crosswalk + taxonomy context
xw <- as.data.table(get_googlesheets_title_xwalk())
tx <- as.data.table(get_googlesheets_title_taxonomy())
# a duplicated title.standard in the taxonomy would duplicate titles in the merge
# (COMPTROLLER has two rows; see FINDINGS.md F-001): keep the first row of each
tx_dups <- tx[duplicated(title.standard), unique(title.standard)]
if (length(tx_dups)) message("taxonomy rows duplicated for: ", paste(tx_dups, collapse = ", "), " (first row kept)")
tx <- tx[!duplicated(title.standard)]
# "mapped" = exact crosswalk match, "pattern" = head rule (title.patterns), "missing" = neither
prof[, crosswalk := fifelse(is.na(title_standard), "missing", fifelse(!is.na(title_match) & title_match == "pattern", "pattern", "mapped"))]
prof <- merge(prof, tx, by.x = "title_standard", by.y = "title.standard", all.x = TRUE, sort = FALSE)
flagcols <- intersect(c("emp", "ceo", "c.level", "dir.vp", "mgr", "spec", "board", "pres", "vp", "sec", "treas", "mem"), names(tx))
prof[, tax_flags := apply(.SD, 1, function(r) paste(flagcols[!is.na(r) & r == "X"], collapse = " ")), .SDcols = flagcols]

# ---- automatic checks -----------------------------------------------------------
# Each check names a reason a title is worth a look; none is a verdict.
is_board <- grepl("\\bboard\\b|\\bmem\\b", prof$tax_flags)
is_exec  <- grepl("\\bceo\\b|\\bc\\.level\\b", prof$tax_flags)
chk <- list(
  MISSING_FROM_CROSSWALK = prof$crosswalk == "missing",
  NO_TAXONOMY_ROW        = prof$crosswalk == "mapped" & is.na(prof$domain.category),
  BOARD_BUT_PAID_FT      = is_board & prof$pct_paid > 0.5 & prof$med_hours >= 35,
  EXEC_BUT_UNPAID_PT     = is_exec & prof$pct_paid < 0.1 & prof$med_hours < 10 & prof$pct_ez < 0.5,
  OFFICER_BOX_MISMATCH   = prof$crosswalk == "mapped" & grepl("\\bboard\\b", prof$tax_flags) & !grepl("pres|vp|sec|treas", prof$tax_flags) &
                           prof$pct_officer > 0.6 & prof$pct_ez < 0.5,
  STANDARD_WORDS_DIFFER  = prof$crosswalk == "mapped" & mapply(function(a, b) {
                             wa <- strsplit(a, "\\s+")[[1]]; wb <- strsplit(b, "\\s+")[[1]]
                             length(setdiff(wb, wa)) > 0 && length(intersect(wa, wb)) == 0 }, prof$TitleTxt7, prof$title_standard),
  CLEANING_RESIDUE       = grepl("[0-9&/#@()]|^\\W|\\W$|\\b(AND|OF|THE|TO|FOR)$|^(AND|OF|THE|TO|FOR)\\b", prof$TitleTxt7) |
                           nchar(prof$TitleTxt7) <= 2,
  EMPTY_TITLE            = prof$TitleTxt7 == "",
  STEP7_REWRITE          = prof$pct_step7_changed > 0
)
chk <- lapply(chk, function(x) !is.na(x) & x)
prof[, auto_checks := do.call(paste, c(lapply(names(chk), function(n) ifelse(chk[[n]], n, "")), sep = " "))]
prof[, auto_checks := trimws(gsub("\\s+", " ", auto_checks))]
# STANDARD_WORDS_DIFFER is informational (DIRECTOR -> BOARD MEMBER is right on the
# 990); the other checks send a mapped title to INSPECT
inspect_checks <- setdiff(names(chk), c("MISSING_FROM_CROSSWALK", "STANDARD_WORDS_DIFFER"))
prof[, inspect := Reduce(`|`, chk[inspect_checks])]

# ---- queue and ids ---------------------------------------------------------------
setorder(prof, -n_rows, TitleTxt7)
prof[, rank := .I]
prof[, cum_pct_rows := cumsum(n_rows) / sum(n_rows)]
prof[, id := sprintf("T%07d", rank)]
prof[, queue := fcase(crosswalk == "missing", "ADD", crosswalk == "pattern", "PATTERN", inspect, "INSPECT", default = "CONFIRM")]
# tiers by frequency (person-title rows): 1 = 1,000+ rows (about 93% of rows),
# 2 = 100-999, 3 = under 100 rows but in 5+ filings, 4 = the long tail
prof[, tier := fcase(n_rows >= 1000, 1L, n_rows >= 100, 2L, n_filings >= 5, 3L, default = 4L)]
arrow::write_parquet(prof, file.path(pv_data, "title-profiles.parquet"))

# ---- raw variants and step trace per TitleTxt7 ------------------------------------
k <- as.data.table(arrow::read_parquet(file.path(pv_data, "title-keys.parquet")))
k[is.na(TitleTxt7), TitleTxt7 := ""]
var <- k[, .(n_rows = sum(n_rows)), by = .(TitleTxt7, TITLE_RAW, TitleTxt3, TitleTxt5, TitleTxt6, Num.Titles)]
setorder(var, TitleTxt7, -n_rows)
arrow::write_parquet(var, file.path(pv_data, "title-variants.parquet"))

# ---- example filings ---------------------------------------------------------------
ex <- as.data.table(dbGetQuery(con, "
  SELECT TitleTxt7, TAX_YEAR, RETURN_TYPE, EIN2, ORG_NAME_L1, TITLE_RAW, TOT_HOURS, TOT_COMP,
         OFFICER_X, TRUST_INDIV_X, KEY_EMPL_X, HIGH_COMP_X, FORMER_X, URL
  FROM (SELECT *, row_number() OVER (PARTITION BY TitleTxt7 ORDER BY hash(OBJECTID || coalesce(TABLE_ID, '') || TitleTxt7)) AS r FROM pc)
  WHERE r <= 5"))
ex[is.na(TitleTxt7), TitleTxt7 := ""]
arrow::write_parquet(ex, file.path(pv_data, "title-examples.parquet"))
dbDisconnect(con, shutdown = TRUE)

s <- prof[, .(titles = .N, rows = sum(n_rows)), by = .(queue, tier)][order(tier, queue)]
s[, pct_rows := round(100 * rows / sum(rows), 2)]
print(s)
fwrite(s, file.path(pv_data, "queue-summary.csv"))
