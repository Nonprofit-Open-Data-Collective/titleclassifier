# 06-apply-decisions.R
# Apply the reviewed crosswalk changes (crosswalk-changes.csv) and new taxonomy
# rows (taxonomy-additions.csv) to the crosswalk tables in data-raw/crosswalks/,
# rebuild the package data from them, and log what was applied in
# applied-log.csv.
#
# new_taxonomy is written "domain.category / domain.label / level", e.g.
# "operations / finance / MANAGER", where level is an emp.level (CEO, OFFICER,
# MANAGER, PROFESSIONAL, STAFF) or a board.role (CHAIR, VICE CHAIR, SECRETARY,
# TREASURER, MEMBER), or is left out for a standard that is not a role.
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

# drafts (reviewer "Claude (draft)" or a script) are not applied until a person
# confirms them by putting their own name in `reviewer`
is_draft <- function(r) grepl("(draft)", r, fixed = TRUE)
if (any(is_draft(ch$reviewer))) message(sprintf("skipping %d unconfirmed draft decisions", sum(is_draft(ch$reviewer))))
ch <- ch[!is_draft(reviewer)]
if (nrow(ta)) ta <- ta[!is_draft(reviewer)]

# new taxonomy rows
if (nrow(ta)) {
  emp_levels  <- c("CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF")
  board_roles <- c("CHAIR", "VICE CHAIR", "SECRETARY", "TREASURER", "MEMBER")
  new_tx <- rbindlist(lapply(seq_len(nrow(ta)), function(i) {
    p <- trimws(strsplit(ta$new_taxonomy[i], "/", fixed = TRUE)[[1]])
    r <- as.list(setNames(rep("", ncol(tx)), names(tx)))
    r$title.standard <- ta$title.standard[i]; r$domain.category <- p[1]; r$domain.label <- if (length(p) > 1) p[2] else ""
    lv <- if (length(p) > 2) p[3] else ""
    if (lv %in% emp_levels) r$emp.level <- lv
    else if (lv %in% board_roles) r$board.role <- lv
    else if (nzchar(lv)) stop("unknown level for ", ta$title.standard[i], ": ", lv)
    as.data.table(r)
  }))
  new_tx <- new_tx[!title.standard %in% tx$title.standard]
  # the domain must be in the vocabulary (data-raw/crosswalks/domains.csv)
  off <- new_tx[!read_xwalk("domains"), on = .(domain.category, domain.label)]
  if (nrow(off)) stop("domain not in domains.csv for: ",
                      paste(off$title.standard, " (", off$domain.category, " / ", off$domain.label, ")", sep = "", collapse = ", "))
} else new_tx <- tx[0]

# policy P1 (DECISIONS.md): every variant on the placeholder standards moves to
# CHIEF OFFICER, not only the reviewed ones (once CHIEF OFFICER has a taxonomy row)
p1_from <- c("CHIEF _X_ OFFICER", "CHEIF _X_ OFFICER")
if ("CHIEF OFFICER" %in% c(tx$title.standard, new_tx$title.standard)) {
  p1 <- xw[title.standard %in% p1_from & !title.variant %in% ch$title.variant,
           .(title.variant, title.standard = "CHIEF OFFICER", action = "change", previous_standard = title.standard,
             id = "", n_rows = "", reviewer = "policy P1", date = as.character(Sys.Date()), notes = "")]
  if (nrow(p1)) message(sprintf("policy P1: %d more variants move from %s to CHIEF OFFICER", nrow(p1), paste(p1_from, collapse = " / ")))
  ch <- rbind(ch, p1, fill = TRUE)
}

# crosswalk rows: add new variants, change existing ones
add <- ch[action == "add" & !title.variant %in% xw$title.variant]
# only rows that change the standard (decisions applied in an earlier round are no-ops)
chg <- ch[title.variant %in% xw$title.variant & title.standard != xw$title.standard[match(title.variant, xw$title.variant)]]
missing_std <- setdiff(ch$title.standard, c(tx$title.standard, new_tx$title.standard))
if (length(missing_std)) stop("standards not in the taxonomy: ", paste(missing_std, collapse = ", "))
message(sprintf("crosswalk: %d variants to add, %d to change; taxonomy: %d new standards", nrow(add), nrow(chg), nrow(new_tx)))

if (!apply) { message("dry run; rerun with --apply to write the tables"); quit(save = "no") }

xw2 <- copy(xw)
if (nrow(chg)) xw2[chg, on = "title.variant", title.standard := i.title.standard]
xw2 <- rbind(xw2, add[, .(title.variant, title.standard, strata = "", strata.label = "", notes = "")], fill = TRUE)
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
