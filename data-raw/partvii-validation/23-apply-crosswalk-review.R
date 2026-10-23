# 23-apply-crosswalk-review.R
# Apply the crosswalk review's decisions (22-crosswalk-review.R,
# crosswalk-review/review-table.csv and check-4-patterns.csv; FINDINGS.md
# F-030) to the tables in data-raw/crosswalks/, rebuild the package data, and
# log each change in crosswalk-review/applied-log.csv.
#
#   Rscript data-raw/partvii-validation/23-apply-crosswalk-review.R          (dry run)
#   Rscript data-raw/partvii-validation/23-apply-crosswalk-review.R --apply

source("data-raw/crosswalks/_crosswalk-io.R")
apply <- "--apply" %in% commandArgs(trailingOnly = TRUE)
OUT <- "data-raw/partvii-validation/crosswalk-review"
today <- "2026-10-22"

xw <- read_xwalk("title-standardization")
tx <- read_xwalk("title-taxonomy")
pt <- read_xwalk("title-patterns")
sc <- read_xwalk("status-codes")
rt <- fread(file.path(OUT, "review-table.csv"), colClasses = "character", na.strings = c("", "NA"))
p4 <- fread(file.path(OUT, "check-4-patterns.csv"), colClasses = "character", na.strings = c("", "NA"))

# ---- 1. new standard: ASSISTANT TREASURER, the treasurer's counterpart of
#         ASSISTANT SECRETARY (rank-word rule) ----------------------------------------
new_tx <- data.table(title.standard = "ASSISTANT TREASURER", domain.category = "operations", domain.label = "finance",
                     emp.level = "STAFF", board.role = "", soc.label = "Bookkeeping, Accounting, and Auditing Clerks",
                     major.group = "43", minor.group = "43-3000", broad.group = "43-3030", detailed.occupation = "43-3031")
new_tx <- new_tx[!title.standard %in% tx$title.standard]
tx2 <- rbind(tx, new_tx[, names(tx), with = FALSE])
setorder(tx2, title.standard)

# ---- 2. variants -----------------------------------------------------------------------
ch <- rt[decision %in% c("change", "add")]
for (j in c("new_strata", "new_strata_label")) set(ch, which(is.na(ch[[j]])), j, "")
bad <- setdiff(ch$new_standard, tx2$title.standard)
if (length(bad)) stop("review decisions target standards not in the taxonomy: ", paste(bad, collapse = ", "))
xw2 <- copy(xw)
i <- match(ch$title.variant, xw2$title.variant)
log <- data.table(title.variant = ch$title.variant, action = ifelse(is.na(i), "add", "change"),
                  previous_standard = xw2$title.standard[i],
                  previous_strata = paste(xw2$strata[i], xw2$strata.label[i], sep = " / "),
                  title.standard = ch$new_standard, strata = paste(ch$new_strata, ch$new_strata_label, sep = " / "),
                  check = ch$check, evidence = ch$evidence_detail, reason = ch$reason, date = today)
upd <- !is.na(i)
xw2[i[upd], `:=`(title.standard = ch$new_standard[upd], strata = ch$new_strata[upd], strata.label = ch$new_strata_label[upd])]
add <- ch[!upd, .(title.variant, title.standard = new_standard, strata = new_strata, strata.label = new_strata_label,
                  notes = paste("crosswalk review", today))]
xw2 <- rbind(xw2, add[, names(xw2), with = FALSE])
setorder(xw2, title.variant)
# drop changes that change nothing (a re-run)
log <- log[!(action == "change" & previous_standard == title.standard & previous_strata == strata)]

# ---- 3. pattern rules: drop the topic-prefix rules ---------------------------------------
drop <- p4[decision == "drop", .(position, head, title.standard = rule_standard)]
pt2 <- pt[!drop, on = .(position, head, title.standard)]
log <- rbind(log, drop[, .(title.variant = paste(position, head), action = "drop rule", previous_standard = title.standard,
                           previous_strata = "", title.standard = "", strata = "", check = "4 pattern rules",
                           evidence = paste("learned precision", p4$precision_learned[p4$decision == "drop"]),
                           reason = p4$reason[p4$decision == "drop"], date = today)][title.variant %in% paste(pt$position, pt$head)])

# ---- 4. status codes ----------------------------------------------------------------------
# PRESENT means current ("2019 - PRESENT"), not interim. NEW is handled in
# gen_status_codes(): it is a status only at the end of a title, so it no
# longer strips NEW from NEW YORK or NEW MEMBER COORDINATOR.
sc2 <- copy(sc)
sc2[status.variant == "PRESENT", `:=`(status.qualifier = "CURRENT", notes = "removed without a flag, like CURRENT (crosswalk review 2026-10-22)")]
sc2 <- sc2[!(status.variant == "NEW" & status.qualifier == "FUTURE")]
st_log <- data.table(title.variant = c("status: PRESENT", "status: NEW"), action = c("change", "remove"),
                     previous_standard = c("INTERIM", "FUTURE"), previous_strata = "", title.standard = c("CURRENT", ""),
                     strata = "", check = "6 status codes", evidence = "",
                     reason = c("PRESENT marks a current term (JAN-PRESENT), not an interim one",
                                "a trailing NEW is recoded as INCOMING in gen_status_codes(); elsewhere NEW is part of a name (NEW YORK, NEW MEMBER COORDINATOR)"),
                     date = today)
st_log <- st_log[c(any(sc$status.variant == "PRESENT" & sc$status.qualifier == "INTERIM"),
                   any(sc$status.variant == "NEW" & sc$status.qualifier == "FUTURE"))]
log <- rbind(log, st_log)

cat(sprintf("new standards: %d; variants changed: %d, added: %d; rules dropped: %d; status codes changed: %d\n",
            nrow(new_tx), sum(log$action == "change" & !grepl("^status", log$title.variant)), sum(log$action == "add"),
            sum(log$action == "drop rule"), nrow(st_log)))
print(log[, .N, by = .(check, action)])

if (apply) {
  write_xwalk(tx2, "title-taxonomy"); write_xwalk(xw2, "title-standardization")
  fwrite(pt2, xwalk_path("title-patterns"))   # this table is written unquoted (10-pattern-rules.R)
  write_xwalk(sc2, "status-codes")
  # crosswalk-changes.csv feeds 06-apply-decisions.R: its rows for the same
  # variants must agree, or a re-run of 06 would undo this review
  cp <- "data-raw/partvii-validation/crosswalk-changes.csv"
  cc <- as.data.table(utils::read.csv(cp, colClasses = "character", na.strings = character(0), check.names = FALSE))  # not fread: quoted variants
  k <- which(cc$title.variant %in% ch$title.variant)
  k <- k[cc$title.standard[k] != ch$new_standard[match(cc$title.variant[k], ch$title.variant)]]
  if (length(k)) {
    cc[k, `:=`(title.standard = ch$new_standard[match(title.variant, ch$title.variant)],
               notes = paste0(notes, " [superseded by the crosswalk review, ", today, ": see crosswalk-review/applied-log.csv]"))]
    fwrite(cc, cp)
    message(sprintf("crosswalk-changes.csv: %d rows superseded", length(k)))
  }
  lf <- file.path(OUT, "applied-log.csv")
  if (file.exists(lf)) log <- rbind(fread(lf, colClasses = "character", na.strings = NULL), log, fill = TRUE)
  fwrite(log, lf)
  rebuild_xwalks()
  cat("applied; package data rebuilt\n")
} else cat("dry run: add --apply to write the tables\n")
