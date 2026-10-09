# 05-collect-reviews.R
# Read the decisions recorded in the review files and write them to the
# repository:
#   data-raw/partvii-validation/review-decisions.csv  every reviewed title (latest decision wins)
#   data-raw/partvii-validation/crosswalk-changes.csv  rows to add to or change in the crosswalk
#   data-raw/partvii-validation/taxonomy-additions.csv new title.standard values to add to the taxonomy
#   data-raw/partvii-validation/cleaning-issues.csv    titles a cleaning step (02-07) gets wrong
# and print the problems that block applying them (06-apply-decisions.R).
#
#   Rscript data-raw/partvii-validation/05-collect-reviews.R

source("data-raw/partvii-validation/_config.R")
suppressMessages(library(data.table))
suppressMessages(devtools::load_all(quiet = TRUE))

statuses <- c("pending", "confirmed", "remap", "add", "add_new_standard", "fix_cleaning", "not_a_title", "ambiguous")
files <- list.files(file.path(pv_review, "titles"), "\\.md$", recursive = TRUE, full.names = TRUE)
# only files edited since they were written can hold a decision
stamp <- file.mtime(file.path(pv_review, "index.csv"))
files <- files[file.mtime(files) > stamp]
message(sprintf("%d review files edited since the index was written", length(files)))

read_front <- function(f) {
  l <- readLines(f, n = 60, warn = FALSE, encoding = "UTF-8")
  end <- which(l == "---")[2]
  y <- yaml::yaml.load(paste(l[2:(end - 1)], collapse = "\n"))
  r <- y$review
  data.table(id = y$id, title_v7 = y$title_v7, crosswalk = y$crosswalk, current_standard = y$current_standard,
             n_rows = y$n_rows, status = r$status %||% "pending", standard = r$standard %||% "",
             new_taxonomy = r$new_taxonomy %||% "", cleaning_issue = r$cleaning_issue %||% "",
             reviewer = r$reviewer %||% "", date = as.character(r$date %||% ""), notes = r$notes %||% "",
             file = sub(paste0(pv_review, "/"), "", f, fixed = TRUE))
}
`%||%` <- function(a, b) if (is.null(a)) b else a
new <- rbindlist(lapply(files, function(f) tryCatch(read_front(f), error = function(e) {
  message("  cannot read the YAML block of ", f, ": ", conditionMessage(e)); NULL })))
new <- if (nrow(new)) new[status != "pending"] else new

out <- file.path(pv_repo, "review-decisions.csv")
old <- if (file.exists(out)) fread(out, colClasses = "character") else NULL
dec <- rbindlist(list(old, new[, lapply(.SD, as.character)]), fill = TRUE)
dec <- dec[order(title_v7, date)][, .SD[.N], by = title_v7]
fwrite(dec[order(-as.numeric(n_rows))], out)
message(sprintf("%d decisions on file (%d new or updated this run)", nrow(dec), nrow(new)))
print(dec[, .N, by = status])

# checks that block applying a decision
tx <- as.data.table(get_googlesheets_title_taxonomy())
bad <- rbind(
  dec[!status %in% statuses, .(title_v7, problem = paste("unknown status", status))],
  dec[status %in% c("remap", "add", "add_new_standard") & standard == "", .(title_v7, problem = "no standard given")],
  dec[status %in% c("remap", "add") & standard != "" & !standard %in% tx$title.standard,
      .(title_v7, problem = paste0("'", standard, "' is not in the taxonomy (use add_new_standard)"))],
  dec[status == "add_new_standard" & new_taxonomy == "", .(title_v7, problem = "add_new_standard without new_taxonomy")],
  dec[status == "fix_cleaning" & cleaning_issue == "", .(title_v7, problem = "fix_cleaning without cleaning_issue")])
if (nrow(bad)) { message("decisions that need attention:"); print(bad) }

fwrite(dec[status %in% c("remap", "add", "add_new_standard") & standard != "",
           .(title.variant = title_v7, title.standard = standard, action = fifelse(status == "remap", "change", "add"),
             previous_standard = current_standard, id, n_rows, reviewer, date, notes)],
       file.path(pv_repo, "crosswalk-changes.csv"))
fwrite(unique(dec[status == "add_new_standard" & standard != "" & !standard %in% tx$title.standard,
                  .(title.standard = standard, new_taxonomy, first_variant = title_v7, reviewer, date)], by = "title.standard"),
       file.path(pv_repo, "taxonomy-additions.csv"))
fwrite(dec[status == "fix_cleaning", .(title_v7, id, n_rows, cleaning_issue, reviewer, date, notes)],
       file.path(pv_repo, "cleaning-issues.csv"))
