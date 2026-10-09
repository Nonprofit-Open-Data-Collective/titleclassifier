# 12-panel-sample.R
# Draw the 2019-2023 sample for calibrating step 09 at scale (FINDINGS.md F-025).
#
# Source: NCCS efile_v2_3 parquet tables. The Part I summary table has total
# expenses for 990 and 990-EZ returns; the Part VII table has one row per
# person. Both are read from PARTVII_DIR/efile (default ~/Documents/PARTVII/efile),
# where they are downloaded with:
#   https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_3/{table}-{year}.parquet
#   for F9-P01-T00-SUMMARY and F9-P07-T01-COMPENSATION, 2019-2023.
#
# Unit: one filing per organization (EIN2) and tax year: the latest
# submission, so an amended return replaces the original.
#
# Strata:
#   - form: 990 or 990-EZ;
#   - size: total expenses (F9_01_EXP_TOT_CY) in six bins:
#     - $0, including the few negative values. These filers almost never pay
#       anyone (3% of 990s, 1% of EZs, against 16% and 15% at $1-150k), so they
#       get their own bin;
#     - $1-150k, $150k-1m, $1m-10m, $10m-100m, $100m+;
#     - 990-EZ returns with no expense figure are a separate "missing" bin;
#   - paid people on Part VII: 0, 1, 2-4, 5+.
#
# Design, 12,000 filings a year:
#   - panel: 6,000 organizations that file in all five years, stratified by
#     their 2019 cell and kept in every year (30,000 filings). This supports
#     the cross-time consistency check;
#   - fresh: 6,000 organizations a year, not in the panel, stratified by that
#     year's cell (30,000 filings).
# Allocation is proportional to the square root of each cell's population,
# with at least 25 per cell where the cell is that big. Each filing carries a
# weight (cell population / cell sample) so estimates can be re-weighted. Seed
# 2026.
#
# Writes PARTVII_DIR/sample/panel-sample-2019-2023.rds (the filings) and, in
# the repository, data-raw/partvii-validation/panel-sample-cells.csv (counts by cell).
#
#   Rscript data-raw/partvii-validation/12-panel-sample.R

suppressMessages({ library(arrow); library(data.table) })
PV <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
E  <- file.path(PV, "efile"); OUT <- file.path(PV, "sample"); dir.create(OUT, showWarnings = FALSE)
years <- 2019:2023
num <- function(x) suppressWarnings(as.numeric(x))

# ---- frame: one filing per organization-year -----------------------------------
fr <- rbindlist(lapply(years, function(y) {
  p1 <- as.data.table(read_parquet(file.path(E, sprintf("F9-P01-T00-SUMMARY-%d.parquet", y)),
        col_select = c("EIN2", "OBJECTID", "RETURN_TYPE", "RETURN_TIME_STAMP", "F9_01_EXP_TOT_CY")))
  p7 <- as.data.table(read_parquet(file.path(E, sprintf("F9-P07-T01-COMPENSATION-%d.parquet", y)),
        col_select = c("OBJECTID", "F9_07_COMP_DTK_COMP_ORG")))
  s7 <- p7[, .(people = .N, paid = sum(num(F9_07_COMP_DTK_COMP_ORG) > 0, na.rm = TRUE)), by = OBJECTID]
  merge(p1, s7, by = "OBJECTID", all.x = TRUE)[, year := y]
}))
fr[is.na(people), `:=`(people = 0L, paid = 0L)]
setorder(fr, EIN2, year, -RETURN_TIME_STAMP)
fr <- fr[!duplicated(fr[, .(EIN2, year)])]
fr[, exp := num(F9_01_EXP_TOT_CY)]
fr[, size := as.character(cut(exp, c(-Inf, 0.5, 150000, 1e6, 1e7, 1e8, Inf), right = FALSE,
                              labels = c("$0", "$1-150k", "$150k-1m", "$1m-10m", "$10m-100m", "$100m+")))]
fr[is.na(exp), size := "missing"]
fr[, paid_bin := fcase(paid == 0, "0", paid == 1, "1", paid <= 4, "2-4", default = "5+")]
fr[, cell := paste(RETURN_TYPE, size, paid_bin, sep = " | ")]

# ---- allocation ---------------------------------------------------------------
alloc <- function(pop, n, floor = 25) {          # sqrt-proportional, with a floor
  a <- pmin(pop, pmax(pmin(pop, floor), round(n * sqrt(pop) / sum(sqrt(pop)))))
  a
}
draw <- function(d, n, seed) {
  set.seed(seed)
  cells <- d[, .(pop = .N), by = cell][, n := alloc(pop, ..n)]
  s <- d[cells, on = "cell"][, .SD[sample(.N, n[1])], by = cell]
  s[, weight := pop / n][, c("pop", "n") := NULL]
  s
}

# panel: organizations in all five years, stratified by their 2019 cell
full <- fr[, .N, by = EIN2][N == length(years), EIN2]
pan  <- draw(fr[year == years[1] & EIN2 %in% full], 6000, 2026)
panel <- fr[EIN2 %in% pan$EIN2]
panel[pan, on = "EIN2", `:=`(stratum = i.cell, weight = i.weight)]
panel[, design := "panel"]

# fresh: each year, organizations outside the panel
fresh <- rbindlist(lapply(seq_along(years), function(i)
  draw(fr[year == years[i] & !EIN2 %in% pan$EIN2], 6000, 2026 + i)[, `:=`(stratum = cell, design = "fresh")]))

smp <- rbind(panel, fresh, fill = TRUE)
saveRDS(smp, file.path(OUT, "panel-sample-2019-2023.rds"))

cells <- smp[, .(filings = .N, orgs = uniqueN(EIN2), weighted = round(sum(weight))), by = .(design, RETURN_TYPE, size, paid_bin)]
setorder(cells, design, RETURN_TYPE, size, paid_bin)
fwrite(cells, "data-raw/partvii-validation/panel-sample-cells.csv")
cat(sprintf("frame: %d filings, %d organizations; %d file all 5 years\n", nrow(fr), uniqueN(fr$EIN2), length(full)))
cat(sprintf("sample: %d filings (%d panel organizations x 5 years + %d fresh filings), %d organizations\n",
            nrow(smp), uniqueN(panel$EIN2), nrow(fresh), uniqueN(smp$EIN2)))
print(smp[, .N, by = .(year, design)][order(year, design)])
print(smp[, .(filings = .N), by = .(RETURN_TYPE, size)][order(RETURN_TYPE, size)])
