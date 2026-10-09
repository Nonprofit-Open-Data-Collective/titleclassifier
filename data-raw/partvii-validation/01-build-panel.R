# 01-build-panel.R
# Build the Part VII Section A panel (all tax years) and the list of unique
# raw titles.
#
# Source: the efile_v2_3 tables F9-P07-T01-COMPENSATION-<year>.parquet
# (990 and 990-EZ officers, directors, trustees, key employees, TY2009-2024),
# built locally in EFILE_BUILD_SEPT_2026/EFILE_V2_3 and published at
# s3://nccs-efile/public/efile_v2_3/.
#
# Writes to <PARTVII>/data:
#   panel/TAX_YEAR=<yr>/*.parquet  one row per person-row of Part VII, with the
#                                  columns the classifier uses plus the step-7
#                                  inputs (officer flag, pay > 0, hours > 40)
#   titles-raw.parquet             one row per distinct raw title, with counts
#
#   Rscript data-raw/partvii-validation/01-build-panel.R

source("data-raw/partvii-validation/_config.R")
library(DBI)

con <- dbConnect(duckdb::duckdb(shared_home = FALSE))
dbExecute(con, sprintf("SET temp_directory = '%s'", file.path(pv_data, "duckdb_tmp")))
dbExecute(con, "SET memory_limit = '48GB'")
src <- sprintf("read_parquet('%s/F9-P07-T01-COMPENSATION-20*.parquet', union_by_name = true)", efile_dir)

# numeric fields as standardize_df() reads them: strip non-digits, NA -> 0
num <- function(x) sprintf("coalesce(try_cast(nullif(regexp_replace(%s, '[^0-9.]', '', 'g'), '') AS DOUBLE), 0)", x)
# checkboxes as to_boole(): X / TRUE / YES -> 1, else 0; NA on 990-EZ
box <- function(x) sprintf("CASE WHEN RETURN_TYPE = '990EZ' THEN NULL WHEN upper(trim(%s)) IN ('X', 'TRUE', 'YES', '1') THEN 1 ELSE 0 END", x)

panel_sql <- sprintf("
SELECT TAX_YEAR, RETURN_TYPE, EIN2, OBJECTID, URL, ORG_NAME_L1, TABLE_ID,
       F9_07_COMP_DTK_TITLE AS TITLE_RAW,
       F9_07_COMP_DTK_NAME_PERS AS NAME_PERS,
       F9_07_COMP_DTK_NAME_ORG_L1 AS NAME_ORG,
       %s + %s AS TOT_HOURS,
       %s + %s + %s + %s AS TOT_COMP,
       %s AS TRUST_INDIV_X, %s AS TRUST_INST_X, %s AS OFFICER_X,
       %s AS KEY_EMPL_X, %s AS HIGH_COMP_X, %s AS FORMER_X
FROM %s",
  num("F9_07_COMP_DTK_AVE_HOUR_WEEK"), num("F9_07_COMP_DTK_AVE_HOUR_WEEK_RL"),
  num("F9_07_COMP_DTK_COMP_ORG"), num("F9_07_COMP_DTK_EMPL_BEN"), num("F9_07_COMP_DTK_COMP_RLTD"), num("F9_07_COMP_DTK_COMP_OTH"),
  box("F9_07_COMP_DTK_POS_INDIV_TRUST_X"), box("F9_07_COMP_DTK_POS_INST_TRUST_X"), box("F9_07_COMP_DTK_POS_OFF_X"),
  box("F9_07_COMP_DTK_POS_KEY_EMPL_X"), box("F9_07_COMP_DTK_POS_HIGH_COMP_X"), box("F9_07_COMP_DTK_POS_FORMER_X"),
  src)

panel_dir <- file.path(pv_data, "panel")
unlink(panel_dir, recursive = TRUE)
t0 <- Sys.time()
dbExecute(con, sprintf("COPY (%s) TO '%s' (FORMAT parquet, PARTITION_BY (TAX_YEAR), COMPRESSION zstd)", panel_sql, panel_dir))
message("panel written in ", format(round(Sys.time() - t0)))

panel <- sprintf("read_parquet('%s/*/*.parquet', hive_partitioning = true)", panel_dir)
by_year <- dbGetQuery(con, sprintf("SELECT TAX_YEAR, RETURN_TYPE, count(*) AS rows, count(DISTINCT OBJECTID) AS filings,
                                    count(DISTINCT TITLE_RAW) AS titles FROM %s GROUP BY ALL ORDER BY ALL", panel))
data.table::fwrite(by_year, file.path(pv_data, "panel-summary.csv"))
print(by_year)

# distinct raw titles with their frequency (rows, filings, years)
dbExecute(con, sprintf("COPY (
  SELECT coalesce(TITLE_RAW, '') AS TITLE_RAW, count(*) AS n_rows, count(DISTINCT OBJECTID) AS n_filings,
         min(TAX_YEAR) AS first_year, max(TAX_YEAR) AS last_year
  FROM %s GROUP BY 1) TO '%s' (FORMAT parquet)", panel, file.path(pv_data, "titles-raw.parquet")))
message("distinct raw titles: ", dbGetQuery(con, sprintf("SELECT count(*) FROM '%s'", file.path(pv_data, "titles-raw.parquet")))[[1]])
dbDisconnect(con, shutdown = TRUE)
