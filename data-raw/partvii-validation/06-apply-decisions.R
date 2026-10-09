# 06-apply-decisions.R
# Apply the reviewed crosswalk changes (crosswalk-changes.csv) and new taxonomy
# rows (taxonomy-additions.csv) to the bundled crosswalk snapshots in
# inst/extdata/crosswalks/, and log what was applied in applied-log.csv.
#
# The snapshots are copies of the Google Sheet; get_googlesheets_*(refresh =
# TRUE) overwrites them from the sheet, so the same rows must be entered in the
# sheet too (applied-log.csv lists them) before anyone refreshes.
#
# new_taxonomy is written "domain.category / domain.label / flags", e.g.
# "operations / finance / emp mgr", where flags are taxonomy flag columns.
#
#   Rscript data-raw/partvii-validation/06-apply-decisions.R          (dry run)
#   Rscript data-raw/partvii-validation/06-apply-decisions.R --apply

source("data-raw/partvii-validation/_config.R")
suppressMessages(library(data.table))
apply <- "--apply" %in% commandArgs(trailingOnly = TRUE)

xw_path <- "inst/extdata/crosswalks/xwalk-title-standardization.csv"
tx_path <- "inst/extdata/crosswalks/xwalk-title-taxonomy.csv"
xw <- fread(xw_path, colClasses = "character", na.strings = NULL)
tx <- fread(tx_path, colClasses = "character", na.strings = NULL)
ch <- fread(file.path(pv_repo, "crosswalk-changes.csv"), colClasses = "character", na.strings = NULL)
ta <- if (file.exists(f <- file.path(pv_repo, "taxonomy-additions.csv"))) fread(f, colClasses = "character", na.strings = NULL) else data.table()

# drafts (reviewer "Claude (draft)" or a script) are not applied until a person
# confirms them by putting their own name in `reviewer`
is_draft <- function(r) grepl("(draft)", r, fixed = TRUE)
if (any(is_draft(ch$reviewer))) message(sprintf("skipping %d unconfirmed draft decisions", sum(is_draft(ch$reviewer))))
ch <- ch[!is_draft(reviewer)]
if (nrow(ta)) ta <- ta[!is_draft(reviewer)]

# new taxonomy rows
if (nrow(ta)) {
  flagcols <- c("emp", "ceo", "c.level", "dir.vp", "mgr", "spec", "board", "pres", "vp", "sec", "treas", "mem")
  new_tx <- rbindlist(lapply(seq_len(nrow(ta)), function(i) {
    p <- trimws(strsplit(ta$new_taxonomy[i], "/", fixed = TRUE)[[1]])
    r <- as.list(setNames(rep("", ncol(tx)), names(tx)))
    r$title.standard <- ta$title.standard[i]; r$domain.category <- p[1]; r$domain.label <- if (length(p) > 1) p[2] else ""
    fl <- if (length(p) > 2) strsplit(p[3], "\\s+")[[1]] else character()
    bad <- setdiff(fl, flagcols); if (length(bad)) stop("unknown taxonomy flag(s) for ", ta$title.standard[i], ": ", paste(bad, collapse = ", "))
    for (x in fl) r[[x]] <- "X"
    as.data.table(r)
  }))
  new_tx <- new_tx[!title.standard %in% tx$title.standard]
} else new_tx <- tx[0]

# crosswalk rows: add new variants, change existing ones
add <- ch[action == "add" & !title.variant %in% xw$title.variant]
chg <- ch[title.variant %in% xw$title.variant]
missing_std <- setdiff(ch$title.standard, c(tx$title.standard, new_tx$title.standard))
if (length(missing_std)) stop("standards not in the taxonomy: ", paste(missing_std, collapse = ", "))
message(sprintf("crosswalk: %d variants to add, %d to change; taxonomy: %d new standards", nrow(add), nrow(chg), nrow(new_tx)))

if (!apply) { message("dry run; rerun with --apply to write the snapshots"); quit(save = "no") }

xw2 <- copy(xw)
if (nrow(chg)) xw2[chg, on = "title.variant", title.standard := i.title.standard]
xw2 <- rbind(xw2, add[, .(title.variant, title.standard, strata = "", strata.label = "")], fill = TRUE)
setorder(xw2, title.variant)
fwrite(xw2, xw_path, quote = TRUE)
if (nrow(new_tx)) fwrite(rbind(tx, new_tx)[order(title.standard)], tx_path, quote = TRUE)

log <- rbind(add[, .(table = "title-standardization", key = title.variant, old = "", new = title.standard, id, reviewer)],
             chg[, .(table = "title-standardization", key = title.variant, old = previous_standard, new = title.standard, id, reviewer)],
             new_tx[, .(table = "title-taxonomy", key = title.standard, old = "", new = "new standard", id = "", reviewer = "")])
log[, applied := as.character(Sys.Date())]
lp <- file.path(pv_repo, "applied-log.csv")
fwrite(if (file.exists(lp)) rbind(fread(lp, colClasses = "character"), log) else log, lp)
message("applied; rerun 02 and 03 to measure the effect, and enter the logged rows in the Google Sheet")
