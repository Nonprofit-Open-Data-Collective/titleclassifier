# 06-apply-decisions.R
# Apply the reviewed crosswalk changes (crosswalk-changes.csv) and new taxonomy
# rows (taxonomy-additions.csv) to the crosswalk tables in data-raw/crosswalks/,
# rebuild the package data from them, and log what was applied in
# applied-log.csv.
#
# new_taxonomy is written "domain.category / domain.label / flags", e.g.
# "operations / finance / emp mgr", where flags are taxonomy flag columns.
#
#   Rscript data-raw/partvii-validation/06-apply-decisions.R          (dry run)
#   Rscript data-raw/partvii-validation/06-apply-decisions.R --apply

source("data-raw/partvii-validation/_config.R")
suppressMessages(library(data.table))
apply <- "--apply" %in% commandArgs(trailingOnly = TRUE)

source("data-raw/crosswalks/_crosswalk-io.R")
xw <- read_xwalk("title-standardization")
tx <- read_xwalk("title-taxonomy")
ch <- fread(file.path(pv_repo, "crosswalk-changes.csv"), colClasses = "character", na.strings = NULL)
ta <- if (file.exists(f <- file.path(pv_repo, "taxonomy-additions.csv"))) fread(f, colClasses = "character", na.strings = NULL) else data.table()

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

if (!apply) { message("dry run; rerun with --apply to write the tables"); quit(save = "no") }

xw2 <- copy(xw)
if (nrow(chg)) xw2[chg, on = "title.variant", title.standard := i.title.standard]
xw2 <- rbind(xw2, add[, .(title.variant, title.standard, strata = "", strata.label = "")], fill = TRUE)
setorder(xw2, title.variant)
write_xwalk(xw2, "title-standardization")
if (nrow(new_tx)) write_xwalk(rbind(tx, new_tx)[order(title.standard)], "title-taxonomy")
rebuild_xwalks()

log <- rbind(add[, .(table = "title-standardization", key = title.variant, old = "", new = title.standard, id, reviewer)],
             chg[, .(table = "title-standardization", key = title.variant, old = previous_standard, new = title.standard, id, reviewer)],
             new_tx[, .(table = "title-taxonomy", key = title.standard, old = "", new = "new standard", id = "", reviewer = "")])
log[, applied := as.character(Sys.Date())]
lp <- file.path(pv_repo, "applied-log.csv")
fwrite(if (file.exists(lp)) rbind(fread(lp, colClasses = "character"), log) else log, lp)
message("applied and rebuilt data/; rerun 02 and 03 to measure the effect")
