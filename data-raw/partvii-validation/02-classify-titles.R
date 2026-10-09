# 02-classify-titles.R
# Run the title classifier on every Part VII title, TY2009-2024.
#
# Steps 1-6 run once per distinct raw title (in parallel, cached by chunk);
# step 7 and the crosswalk run once per (raw title x officer flag x pay > 0 x
# hours > 40). 00-check-equivalence.R shows this gives the same TitleTxt7 and
# title.standard as the full pipeline. Results are joined back to the panel.
#
# Writes to <PARTVII>/data:
#   title-steps.parquet       raw title -> split titles, TitleTxt2..6, status flags
#   title-keys.parquet        (raw title x step-7 inputs) -> TitleTxt7, title.standard, rows
#   panel-classified/         the panel with one row per person-title, adding
#                             TitleTxt6, TitleTxt7, title.standard, strata, Num.Titles
#   Rscript data-raw/partvii-validation/02-classify-titles.R [workers]

source("data-raw/partvii-validation/_config.R")
suppressMessages({library(data.table); library(DBI)})
args <- commandArgs(trailingOnly = TRUE)
workers <- if (length(args)) as.integer(args[1]) else 20L

suppressMessages(devtools::load_all(quiet = TRUE))
source(file.path(pv_repo, "_steps.R"))
sc <- get_googlesheets_status_codes(); xw <- get_googlesheets_title_xwalk()

# ---- steps 1-6 per distinct raw title -----------------------------------------
raw <- arrow::read_parquet(file.path(pv_data, "titles-raw.parquet"))$TITLE_RAW
chunk_dir <- file.path(pv_data, "chunks-steps-1-6")
dir.create(chunk_dir, showWarnings = FALSE)
size <- 5000L
chunks <- split(raw, ceiling(seq_along(raw) / size))
todo <- which(!file.exists(file.path(chunk_dir, sprintf("chunk-%04d.parquet", seq_along(chunks)))))
message(sprintf("%d raw titles, %d chunks, %d to run on %d workers", length(raw), length(chunks), length(todo), workers))

if (length(todo)) {
  cl <- parallel::makeCluster(workers)
  parallel::clusterExport(cl, c("chunk_dir", "sc", "pv_repo", "chunks"))
  invisible(parallel::clusterEvalQ(cl, {
    suppressMessages(devtools::load_all(quiet = TRUE))
    source(file.path(pv_repo, "_steps.R"))
  }))
  t0 <- Sys.time()
  invisible(parallel::clusterApplyLB(cl, todo, function(i) {
    out <- steps_1_6(chunks[[i]], sc)
    out$RID <- NULL
    arrow::write_parquet(out, file.path(chunk_dir, sprintf("chunk-%04d.parquet", i)))
    i
  }))
  parallel::stopCluster(cl)
  message("steps 1-6 done in ", format(round(Sys.time() - t0)))
}

con <- dbConnect(duckdb::duckdb(shared_home = FALSE))
dbExecute(con, sprintf("SET temp_directory = '%s'", file.path(pv_data, "duckdb_tmp")))
dbExecute(con, sprintf("COPY (SELECT * FROM read_parquet('%s/chunk-*.parquet', union_by_name = true)) TO '%s' (FORMAT parquet)",
                       chunk_dir, file.path(pv_data, "title-steps.parquet")))
n_raw <- dbGetQuery(con, sprintf("SELECT count(DISTINCT TITLE_RAW) FROM '%s'", file.path(pv_data, "title-steps.parquet")))[[1]]
stopifnot(n_raw == length(unique(raw)))

# ---- step 7 + crosswalk per (raw title x step-7 inputs) ------------------------
panel <- sprintf("read_parquet('%s/panel/*/*.parquet', hive_partitioning = true)", pv_data)
keys <- as.data.table(dbGetQuery(con, sprintf("
  SELECT coalesce(TITLE_RAW, '') AS TITLE_RAW, OFFICER_X, TOT_COMP > 0 AS PAY_GT0, TOT_HOURS > 40 AS HRS_GT40,
         count(*) AS n_rows
  FROM %s GROUP BY ALL", panel)))
steps <- as.data.table(arrow::read_parquet(file.path(pv_data, "title-steps.parquet")))
k <- merge(keys, steps, by = "TITLE_RAW", allow.cartesian = TRUE)
k <- as.data.table(step_7(as.data.frame(k), xw))
arrow::write_parquet(k, file.path(pv_data, "title-keys.parquet"))
message(sprintf("title keys: %d (raw title x step-7 inputs) -> %d person-title rows; %d distinct TitleTxt7; %.2f%% of rows unmatched",
                nrow(keys), sum(k$n_rows), uniqueN(k$TitleTxt7), 100 * k[is.na(title.standard), sum(n_rows)] / sum(k$n_rows)))

# ---- join back to the panel ------------------------------------------------------
duckdb::duckdb_register(con, "k", k[, .(TITLE_RAW, OFFICER_X, PAY_GT0, HRS_GT40, Num.Titles, TitleTxt6, TitleTxt7,
                                        title.standard, title.match, strata, DATE.X, FORMER.X, INTERIM.X, FOUNDER.X, FUTURE.X,
                                        OUTGOING.X, PARTIAL.X, AT.LARGE.X, EXOFFICIO.X, REGIONAL.X, CO.X, SCHED.O.X)])
out <- file.path(pv_data, "panel-classified")
unlink(out, recursive = TRUE)
dbExecute(con, sprintf("COPY (
  SELECT p.*, k.* EXCLUDE (TITLE_RAW, OFFICER_X, PAY_GT0, HRS_GT40)
  FROM %s p JOIN k ON coalesce(p.TITLE_RAW, '') = k.TITLE_RAW
   AND p.OFFICER_X IS NOT DISTINCT FROM k.OFFICER_X
   AND (p.TOT_COMP > 0) = k.PAY_GT0 AND (p.TOT_HOURS > 40) = k.HRS_GT40
) TO '%s' (FORMAT parquet, PARTITION_BY (TAX_YEAR), COMPRESSION zstd)", panel, out))
n <- dbGetQuery(con, sprintf("SELECT count(*) FROM read_parquet('%s/*/*.parquet')", out))[[1]]
message(sprintf("panel-classified: %d person-title rows (expected %d)", n, sum(k$n_rows)))
stopifnot(n == sum(k$n_rows))
dbDisconnect(con, shutdown = TRUE)
