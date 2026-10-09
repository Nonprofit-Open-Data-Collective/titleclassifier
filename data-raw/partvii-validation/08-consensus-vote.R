# 08-consensus-vote.R
# Settle Claude's draft decisions by a three-vote consensus, so the crosswalk
# can be revised before a person reviews each title.
#
# Votes: the draft itself, plus two independent reviewers (agents A and B) who
# each vote agree / disagree on every draft and give their own decision when
# they disagree. Their votes are CSV parts in <PARTVII>/review/votes/<round>/A
# and .../B, with columns id, vote, status, standard, new_taxonomy, reason.
#
# Rule:
#   - at least one reviewer agrees           -> the draft stands (2 or 3 of 3 votes)
#   - both disagree with the same decision   -> their decision (2 of 3)
#   - both disagree with different decisions -> no majority: status ambiguous,
#                                               nothing is applied
# The result replaces the draft in each review file's `review:` block, with
# reviewer "Consensus vote (3 Claude reviewers)" so 06-apply-decisions.R
# applies it, and a note that a person has not reviewed it yet. All votes are
# kept in consensus-votes.csv for that later review.
#
#   Rscript data-raw/partvii-validation/08-consensus-vote.R round1

source("data-raw/partvii-validation/_config.R")
suppressMessages(library(data.table))
round_id <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(round_id)) round_id <- "round1"
vdir <- file.path(pv_review, "votes", round_id)

read_votes <- function(who) {
  f <- sort(list.files(file.path(vdir, who), "\\.csv$", full.names = TRUE))
  v <- rbindlist(lapply(f, fread, colClasses = "character", na.strings = NULL), fill = TRUE)
  v[, vote := tolower(trimws(vote))]
  for (c in c("status", "standard", "new_taxonomy", "reason")) if (!c %in% names(v)) v[, (c) := ""]
  v[, lapply(.SD, function(x) fifelse(is.na(x), "", trimws(x)))]
}
A <- read_votes("A"); B <- read_votes("B")
dec <- as.data.table(utils::read.csv(file.path(pv_repo, "review-decisions.csv"), colClasses = "character", na.strings = character(0), check.names = FALSE))
d <- dec[reviewer == "Claude (draft)"]
message(sprintf("drafts %d; votes A %d, B %d", nrow(d), nrow(A), nrow(B)))
for (x in list(A, B)) {
  stopifnot(!anyDuplicated(x$id), all(x$vote %in% c("agree", "disagree")))
  miss <- setdiff(d$id, x$id); if (length(miss)) stop("votes missing for ", length(miss), " drafts, e.g. ", miss[1])
}

v <- merge(d[, .(id, title_v7, n_rows, current_standard, draft_status = status, draft_standard = standard,
                 draft_new_taxonomy = new_taxonomy)],
           A[, .(id, a_vote = vote, a_status = status, a_standard = standard, a_new_taxonomy = new_taxonomy, a_reason = reason)], by = "id")
v <- merge(v, B[, .(id, b_vote = vote, b_status = status, b_standard = standard, b_new_taxonomy = new_taxonomy, b_reason = reason)], by = "id")
v[, outcome := fcase(
  a_vote == "agree" & b_vote == "agree", "draft (3 of 3)",
  a_vote == "agree" | b_vote == "agree", "draft (2 of 3)",
  a_status == b_status & toupper(a_standard) == toupper(b_standard), "reviewers (2 of 3)",
  default = "no majority")]
v[, `:=`(final_status = fcase(outcome %like% "^draft", draft_status, outcome %like% "^reviewers", a_status, default = "ambiguous"),
         final_standard = fcase(outcome %like% "^draft", draft_standard, outcome %like% "^reviewers", toupper(a_standard), default = ""),
         final_new_taxonomy = fcase(outcome %like% "^draft", draft_new_taxonomy,
                                    outcome %like% "^reviewers", fifelse(a_new_taxonomy != "", a_new_taxonomy, b_new_taxonomy), default = ""))]
v[final_status == "confirmed", final_standard := ""]
print(v[, .N, by = outcome][order(outcome)])
fwrite(v[order(id)], file.path(pv_repo, sprintf("consensus-votes-%s.csv", round_id)))

# write the consensus into the review files
idx <- fread(file.path(pv_review, "index.csv"), colClasses = "character", na.strings = NULL)
v <- merge(v, idx[, .(id, file)], by = "id")
q <- function(x) paste0('"', gsub('"', "'", ifelse(is.na(x), "", x)), '"')
for (i in seq_len(nrow(v))) {
  r <- v[i]; path <- file.path(pv_review, r$file)
  l <- readLines(path, warn = FALSE, encoding = "UTF-8")
  set <- function(key, val) { j <- grep(paste0("^  ", key, ":"), l)[1]; l[j] <<- paste0("  ", key, ": ", val) }
  old_notes <- sub('^  notes: "(.*)"$', "\\1", grep("^  notes:", l, value = TRUE)[1])
  vote_note <- sprintf("[%s; not yet reviewed by a person] draft %s %s; A %s%s; B %s%s.", r$outcome, r$draft_status, r$draft_standard,
                       r$a_vote, if (r$a_vote == "disagree") paste0(" -> ", r$a_status, " ", r$a_standard, " (", r$a_reason, ")") else "",
                       r$b_vote, if (r$b_vote == "disagree") paste0(" -> ", r$b_status, " ", r$b_standard, " (", r$b_reason, ")") else "")
  set("status", r$final_status); set("standard", q(r$final_standard)); set("new_taxonomy", q(r$final_new_taxonomy))
  set("reviewer", q(if (r$outcome == "no majority") "Consensus vote: no majority" else "Consensus vote (3 Claude reviewers)"))
  set("date", q(as.character(Sys.Date()))); set("notes", q(paste(vote_note, old_notes)))
  writeLines(l, path, useBytes = TRUE)
}
message("consensus written to ", nrow(v), " review files; run 05-collect-reviews.R next")
