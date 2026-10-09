# 21-performance.R
# The tables behind the Performance article (vignettes/performance.Rmd):
# 1. Coverage: how many title rows the crosswalk maps (exactly or by pattern),
#    and how many remain uncoded, in the 2019-2023 calibration sample
#    (13-panel-run.R), by form and size. Shares are unweighted sample shares;
#    the overall row also gives the share weighted to the population.
# 2. Accuracy on the labeled set (gold-v2, scored by 09-gold-check.R), by form,
#    labeled role, paid or unpaid, and size (Part I total expenses, joined from
#    the 2010-2012 summary tables).
#
# Run 09-gold-check.R first. Writes panel-results/perf-*.csv.
#
#   Rscript data-raw/partvii-validation/21-performance.R

suppressMessages({ library(data.table); library(arrow) })
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
RES <- "data-raw/partvii-validation/panel-results"
size_levels <- c("$0", "$1-150k", "$150k-1m", "$1m-10m", "$10m-100m", "$100m+", "missing")
size_of <- function(exp) { s <- as.character(cut(exp, c(-Inf, 0.5, 150000, 1e6, 1e7, 1e8, Inf), right = FALSE,
                                                 labels = size_levels[1:6])); s[is.na(exp)] <- "missing"; s }

# ---- 1. coverage --------------------------------------------------------------------
d <- rbindlist(lapply(list.files(file.path(PV, "sample"), "^classified-\\d{4}\\.rds$", full.names = TRUE), function(f)
  readRDS(f)[, .(object.id, person.id, return_type, size, weight, title.v7, title.standard, title.match,
                 domain.category, role.final, role.primary)]), fill = TRUE)
d[, match := fifelse(is.na(title.match), "uncoded", title.match)]
d[match != "uncoded" & domain.category == "non-job title", match := "non-job title"]
cov <- function(x) x[, .(title_rows = .N, exact = round(100 * mean(match == "exact"), 1),
                         pattern = round(100 * mean(match == "pattern"), 1),
                         non_job_title = round(100 * mean(match == "non-job title"), 1),
                         uncoded = round(100 * mean(match == "uncoded"), 1),
                         people_with_role = round(100 * mean(!is.na(role.final[role.primary])), 1))]
cov_all  <- rbind(cbind(group = "all (sample)", cov(d)),
                  cbind(group = "all (weighted)", d[, .(title_rows = .N,
                    exact = round(100 * weighted.mean(match == "exact", weight), 1),
                    pattern = round(100 * weighted.mean(match == "pattern", weight), 1),
                    non_job_title = round(100 * weighted.mean(match == "non-job title", weight), 1),
                    uncoded = round(100 * weighted.mean(match == "uncoded", weight), 1),
                    people_with_role = round(100 * mean(!is.na(role.final[role.primary])), 1))]))
cov_form <- d[, cov(.SD), by = .(form = return_type)]
cov_size <- d[, cov(.SD), by = .(form = return_type, size)][order(form, match(size, size_levels))]
unc <- d[match == "uncoded", .(rows = .N), by = .(title = title.v7)][order(-rows)]
unc[, share_of_uncoded := round(100 * rows / sum(rows), 2)]
fwrite(rbind(cov_all, cbind(group = cov_form$form, cov_form[, -1])), file.path(RES, "perf-coverage.csv"))
fwrite(cov_size, file.path(RES, "perf-coverage-by-size.csv"))
fwrite(head(unc, 25), file.path(RES, "perf-uncoded-top.csv"))
cat("coverage:\n"); print(cov_all); print(cov_form); print(cov_size)
cat(sprintf("\ndistinct uncoded titles: %d; the top 25 cover %.1f%% of uncoded rows\n",
            nrow(unc), sum(head(unc, 25)$share_of_uncoded)))

# ---- 2. accuracy on the labeled set ---------------------------------------------------
g <- fread("data-raw/partvii-validation/gold-check/gold-v2-results.csv", colClasses = "character", na.strings = c("", "NA"))
g <- g[!role %in% c("UNSURE", "NONE") & !is.na(role.final)]
g[, hit := as.logical(hit)]
p1 <- rbindlist(lapply(2010:2012, function(y) as.data.table(read_parquet(
  file.path(PV, "efile", sprintf("F9-P01-T00-SUMMARY-%d.parquet", y)), col_select = c("OBJECTID", "F9_01_EXP_TOT_CY")))))
p1 <- p1[!duplicated(OBJECTID)]
g[p1, on = "OBJECTID", exp := suppressWarnings(as.numeric(i.F9_01_EXP_TOT_CY))]
g[, size := size_of(exp)]
g[, paid := fifelse(as.numeric(comp) > 0, "paid", "unpaid")]
acc <- function(x, by) x[, .(people = .N, accuracy = round(100 * mean(hit), 1)), by = by]
out <- rbind(cbind(breakdown = "all", group = "all", acc(g, NULL)),
             cbind(breakdown = "form", setnames(acc(g, "formtype"), "formtype", "group")),
             cbind(breakdown = "labeled role", setnames(acc(g, "role"), "role", "group")[
               order(match(group, c("CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF", "BOARD", "DUAL")))]),
             cbind(breakdown = "pay", setnames(acc(g, "paid"), "paid", "group")),
             cbind(breakdown = "size", setnames(acc(g, "size"), "size", "group")[order(match(group, size_levels))]))
# precision: of the people step 09 put in a role, the share labeled that role
# (a DUAL label counts as correct for CEO, OFFICER or BOARD)
prec <- g[, .(predicted = .N, precision = round(100 * mean(hit), 1)), by = .(role = role.final)][
  order(match(role, c("CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF", "BOARD")))]
b <- g[role == "BOARD" & role.final == "BOARD"]
brd <- data.table(board_seats = nrow(b), right_board_role = round(100 * mean(b$board_role == b$role.board), 1))
fwrite(out, file.path(RES, "perf-gold.csv")); fwrite(prec, file.path(RES, "perf-gold-precision.csv"))
fwrite(brd, file.path(RES, "perf-gold-board-role.csv"))
cat("\nlabeled set:\n"); print(out); print(prec); print(brd)
cat("\nform x pay:\n"); print(acc(g, c("formtype", "paid")))
