# 22-crosswalk-review.R
# The crosswalk review (data-dev/CROSSWALK-ODDITIES.md, section 6; FINDINGS.md
# F-030). For each crosswalk variant seen in the 2019-2023 calibration sample
# (13-panel-run.R) it counts the people who hold it, the share who tick each
# Part VII box, and the share paid. Boxes are counted on full 990 returns only
# (990-EZ has none). It flags the cases of each check, decides each one, and
# writes crosswalk-review/:
#
#   check-1-board-jobs.csv     variants mapped to a BOARD standard whose words
#                              name a job: seat or job?
#   check-2-staff-seats.csv    variants mapped to a staff standard whose holders
#                              mostly tick only the trustee box and are unpaid
#   check-3-rank-words.csv     assistant / deputy / associate / vice / junior /
#                              senior / executive forms of the board offices
#                              and the C-suite
#   check-4-patterns.csv       the head rules (title.patterns) re-scored on the sample
#   check-5-uncoded.csv        the 200 most frequent uncoded titles
#   check-6-name-conflicts.csv variants spelled like a different standard
#   review-table.csv           one row per flagged case, with the decision and
#                              the reason; 23-apply-crosswalk-review.R applies it
#
# A decision is a rule (below) unless manual-decisions.csv overrides it.
# Run this before 23-apply-crosswalk-review.R: it reads the crosswalk as it
# stands, so a re-run after the apply shows the reviewed state.
#
#   Rscript data-raw/partvii-validation/22-crosswalk-review.R
#
# CLASSIFIED_DIR overrides the folder with classified-YYYY.rds
# (default PARTVII_DIR/sample).

source("tests/regression/regression-helpers.R"); tc_load_package()
source("data-raw/crosswalks/_crosswalk-io.R")
PV  <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
CL  <- Sys.getenv("CLASSIFIED_DIR", file.path(PV, "sample"))
OUT <- "data-raw/partvii-validation/crosswalk-review"; dir.create(OUT, showWarnings = FALSE)

d <- rbindlist(lapply(list.files(CL, "^classified-\\d{4}\\.rds$", full.names = TRUE), function(f)
  readRDS(f)[, .(object.id, person.id, return_type, title.v7, title.match, tot.comp, tot.hours,
                 tru = dtk.indiv.trustee.x %in% 1 | dtk.inst.trustee.x %in% 1, off = dtk.officer.x %in% 1,
                 key = dtk.key.empl.x %in% 1 | dtk.high.comp.x %in% 1)]), fill = TRUE)
d[, comp := fcoalesce(suppressWarnings(as.numeric(tot.comp)), 0)]
d[, paid := comp > 0]
d[, f990 := return_type == "990"]

xw <- read_xwalk("title-standardization"); tx <- read_xwalk("title-taxonomy")
xw[tx, on = "title.standard", `:=`(emp.level = i.emp.level, board.role = i.board.role)]
lev <- function(s) { i <- match(s, tx$title.standard); ifelse(nzchar(tx$board.role[i]), "BOARD", tx$emp.level[i]) }
emp_levels <- c("CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF")

pct <- function(x) if (length(x)) round(100 * mean(x)) else NA_real_
evidence_of <- function(r) r[, .(rows = .N, rows_990 = sum(f990), paid = pct(paid),
                                 median_pay = as.numeric(median(comp)),
                                 trustee_box = pct(tru[f990]), officer_box = pct(off[f990]), key_box = pct(key[f990]),
                                 trustee_only_unpaid = pct((tru & !off & !key & !paid)[f990]))]
ev <- d[, evidence_of(.SD), by = .(title.variant = title.v7)]
xe <- merge(xw, ev, by = "title.variant", all.x = TRUE)
xe[is.na(rows), `:=`(rows = 0L, rows_990 = 0L)]

# what the evidence says about the holders on full 990 returns:
# job  - many tick the key-employee box, or most are well paid and few are trustees
# seat - most tick the trustee box and few the key-employee box
# officer seat - most tick the officer box, few the key-employee box
evidence_says <- function(e) with(e, fcase(
  (rows_990 >= 10 & (key_box >= 40 | (trustee_box < 30 & median_pay >= 10000))) |
    (rows_990 >= 4 & key_box >= 50 & median_pay >= 10000),  "job",
  rows_990 < 10,                                            "too few",
  trustee_box >= 60 & key_box < 20,                         "seat",
  officer_box >= 60 & key_box < 20,                         "officer seat",
  default = "mixed"))
xe[, evidence := evidence_says(.SD)]
ev_text <- function(e) with(e, ifelse(rows_990 > 0,
  sprintf("%d full-990 holders: trustee box %s%%, officer %s%%, key employee %s%%; %s%% paid",
          rows_990, trustee_box, officer_box, key_box, paid),
  ifelse(rows > 0, sprintf("%d holders, all on 990-EZ; %s%% paid", rows, paid), "not in the sample")))

job_words <- paste0("\\b(", paste(c(
  "COORDINATOR", "COACH(ING)?", "ASSISTANT", "MANAGERS?", "ADMINISTRATOR", "PROGRAMS?", "YOUTH",
  "SUPERVISOR", "SPECIALIST", "STAFF", "CLERK", "ASSOCIATE", "CONSULTANT", "LIAISON", "ORGANIZER",
  "EDITOR", "PASTOR", "MINISTER", "TEACHER", "INSTRUCTOR", "COUNSEL", "ATTORNEY", "ACCOUNTANT",
  "BOOKKEEPER", "ANALYST", "ENGINEER", "TECHNICIAN", "NURSE", "PHYSICIAN", "DOCTOR", "ARCHITECT",
  "PLANNER", "AGENT", "REGISTRAR", "SCHEDULER", "TRAINER", "EMPLOYEE", "DIRECTOR OF", "CHIEF",
  "HEAD", "OFFICER", "EXECUTIVE", "REPRESENTATIVE", "STEWARD", "ADVISOR", "LEADER"), collapse = "|"), ")\\b")
keep_cols <- c("title.variant", "title.standard", "strata", "strata.label", "rows", "rows_990", "paid",
               "median_pay", "trustee_box", "officer_box", "key_box", "trustee_only_unpaid", "evidence")

# ---- check 1: board standards whose variant names a job -----------------------------
c1 <- xe[nzchar(board.role) & grepl(job_words, title.variant)]
# a variant with few holders is judged by its family: every board variant with
# the same job word and no seat word, pooled
seat_words <- "BOARD|TRUSTEE|MEMBER|CHAIR|PRESIDENT|SECRETARY|TREASURER|COMMITTEE"
families <- c("COORDINATOR", "COACH", "PHYSICIAN", "ATTORNEY|COUNSEL", "EDITOR", "ADMINISTRATOR", "^ASSISTANT",
              "CLERK", "STEWARD", "REPRESENTATIVE", "LIAISON", "YOUTH", "PROGRAMS", "INSTRUCTOR|TEACHER",
              "CONSULTANT", "ADVISOR", "^EXECUTIVE", "OFFICER$", "AGENT")
fam <- rbindlist(lapply(families, function(f) {
  v <- c1[grepl(f, title.variant) & !grepl(seat_words, title.variant), title.variant]
  cbind(family = f, evidence_of(d[title.v7 %in% v]))
}))
fam[, evidence := evidence_says(.SD)]
c1[, family := vapply(title.variant, function(t) { h <- families[vapply(families, grepl, NA, t)]; if (length(h)) h[1] else NA_character_ }, "")]
fwrite(c1[order(-rows), c(keep_cols, "family"), with = FALSE], file.path(OUT, "check-1-board-jobs.csv"))
fwrite(fam, file.path(OUT, "check-1-families.csv"))

# ---- check 2: staff standards whose holders look like board seats ---------------------
c2 <- xe[nzchar(emp.level) & rows_990 >= 10 & trustee_only_unpaid >= 50]
fwrite(c2[order(-rows), c(keep_cols, "emp.level"), with = FALSE], file.path(OUT, "check-2-staff-seats.csv"))

# ---- check 3: rank words on the board offices and the C-suite -------------------------
rank_re   <- "\\b(ASSISTANT|ASST|DEPUTY|ASSOCIATE|ASSOC|VICE|JUNIOR|SENIOR|SR|EXECUTIVE|EXEC)\\b"
office_re <- "\\b(SECRETARY|TREASURER|CLERK|ADJUTANT|CHAIR|CHAIRMAN|CHAIRPERSON|PRESIDENT|CEO|CFO|COO|EXECUTIVE DIRECTOR)$"
c3 <- xe[grepl(rank_re, title.variant) & grepl(office_re, title.variant) &
         !grepl("^(VICE (PRESIDENT|CHAIR|CHAIRMAN|CHAIRPERSON)|EXECUTIVE DIRECTOR)$", title.variant) &
         (nzchar(board.role) | emp.level %in% c("CEO", "OFFICER") | title.standard == "ASSISTANT SECRETARY")]
c3[, rank_word := sub(paste0("^.*?", rank_re, ".*$"), "\\1", title.variant, perl = TRUE)]
fwrite(c3[order(rank_word, -rows), c("rank_word", keep_cols, "emp.level", "board.role"), with = FALSE],
       file.path(OUT, "check-3-rank-words.csv"))

# ---- check 4: re-score the head rules on the sample -----------------------------------
# truth: the crosswalk standard of every sample title that has a crosswalk
# entry; a rule is scored on the titles whose head it is, by standard and by
# level (the emp.level or BOARD)
pat <- as.data.table(get_title_patterns())
st <- d[, .(rows = .N), by = .(title = title.v7)][title %in% xw$title.variant]
st[xw[!duplicated(title.variant)], on = c(title = "title.variant"), truth := i.title.standard]
h <- title_heads(st$title, xw$title.variant)
sc <- rbind(st[, .(position = "suffix", head = h$suffix, truth, rows)],
            st[, .(position = "prefix", head = h$prefix, truth, rows)])[!is.na(head)]
sc <- merge(sc, pat[, .(position, head, rule_standard = title.standard)], by = c("position", "head"))
c4 <- sc[, .(titles = .N, precision_sample = round(mean(truth == rule_standard), 3),
             level_precision_sample = round(mean(lev(truth) == lev(rule_standard)), 3)),
         by = .(position, head, rule_standard)]
c4 <- merge(pat[, .(position, head, rule_standard = title.standard, precision_learned = precision,
                    support_learned = support)], c4, all.x = TRUE, by = c("position", "head", "rule_standard"))
# uses: the uncoded sample titles each rule codes
pm <- d[title.match %in% "pattern", .(rows = .N), by = .(title = title.v7)]
hm <- title_heads(pm$title, xw$title.variant)
pm[, `:=`(suffix = hm$suffix, prefix = hm$prefix, std = match_title_patterns(title, pat, xw$title.variant))]
is_s <- match(paste("suffix", pm$suffix), paste(pat$position, pat$head))
pm[, rule := fifelse(!is.na(is_s) & pat$title.standard[is_s] %in% std, paste("suffix", suffix), paste("prefix", prefix))]
uses <- pm[, .(coded_titles = .N, coded_rows = sum(rows), examples = paste(head(title[order(-rows)], 4), collapse = "; ")), by = rule]
c4[, rule := paste(position, head)]
c4 <- merge(c4, uses, by = "rule", all.x = TRUE)
# topic heads: a prefix that names an activity, not a seat or a job
role_words <- "\\b(BOARD|MEMBERS?|TRUSTEES?|DIRECTORS?|CHAIR|CHAIRMAN|PRESIDENT|VICE|OFFICERS?|COUNCIL|COMMITTEE|GOVERNORS?|DELEGATE|ALT|ALTERNATE|EXOFFICIO|VOTING|HONORARY|ADVISORY|SECRETARY|TREASURER|CEO|CFO|REPRESENTATIVE|AFFILIATE|STUDENT|SERGEANT|NURSE|DEAN|MANAGER|CHIEF|ASSISTANT|SENIOR|ACTIVE|INDIVIDUAL|UNION|SCHOOL|REGIONAL|NUMBER)\\b|^(ALPHA|BETA|GAMMA|DELTA|EPSILON|IOTA|KAPPA|THETA)$"
c4[, topic_head := position == "prefix" & !grepl(role_words, head)]
c4[, decision := fifelse(topic_head & lev(rule_standard) == "BOARD" & precision_learned < 0.9, "drop", "keep")]
c4[, reason := fifelse(decision == "drop",
  "a topic prefix (an activity, not a seat or a job) with learned precision under 0.9: it codes jobs such as the examples as BOARD MEMBER",
  fifelse(topic_head & lev(rule_standard) == "BOARD",
          "a topic prefix with precision 0.9 or more: kept",
          "kept"))]
fwrite(c4[order(precision_learned)], file.path(OUT, "check-4-patterns.csv"))
# should a job-noun suffix outrank a topic prefix in match_title_patterns()?
# Of the crosswalk titles that have a topic prefix with a board rule and a
# suffix head mapped to an employee standard, how many are board titles? (Most
# are: 15 of 21 when written, so the selection rule was left as it is.)
xh <- title_heads(xw$title.variant, xw$title.variant)
g <- data.table(title = xw$title.variant, truth = xw$title.standard, suffix = xh$suffix, prefix = xh$prefix)
g <- g[!is.na(suffix) & !is.na(prefix)]
g[, suf_lvl := lev(xw$title.standard[match(suffix, xw$title.variant)])]
g <- merge(g, pat[position == "prefix", .(prefix = head, rule_standard = title.standard)], by = "prefix")
g <- g[suf_lvl %in% emp_levels & lev(rule_standard) == "BOARD" & !grepl(role_words, prefix) &
       !paste("suffix", suffix) %in% paste(pat$position, pat$head)]
guard_note <- sprintf("crosswalk titles with a topic-prefix board rule and a job-noun suffix: %d, of which %d (%.0f%%) map to a board standard",
                      nrow(g), sum(lev(g$truth) == "BOARD"), 100 * mean(lev(g$truth) == "BOARD"))

# ---- check 5: the most frequent uncoded titles ----------------------------------------
c5 <- d[is.na(title.match) & !is.na(title.v7) & nzchar(title.v7), evidence_of(.SD), by = .(title.variant = title.v7)][order(-rows)][1:200]
fwrite(c5, file.path(OUT, "check-5-uncoded.csv"))

# ---- check 6: variants spelled like a different standard --------------------------------
c6 <- xe[title.variant %in% tx$title.standard & title.variant != title.standard]
fwrite(c6[order(-rows), ..keep_cols], file.path(OUT, "check-6-name-conflicts.csv"))

# ---- decisions --------------------------------------------------------------------------
man <- fread(file.path(OUT, "manual-decisions.csv"), colClasses = "character", na.strings = NULL)
row <- function(check, e, decision, std, strata, label, reason)
  data.table(check = check, title.variant = e$title.variant, current_standard = e$title.standard,
             current_strata = paste(e$strata, e$strata.label, sep = " / "), evidence = e$evidence,
             evidence_detail = ev_text(e), decision = decision, new_standard = std, new_strata = strata,
             new_strata_label = label, reason = reason)

# check 1: keep the board standard unless the holders are staff
fam_note <- function(f) { i <- match(f, fam$family)
  ifelse(is.na(i), "", sprintf(" The %s family is pooled as %s (trustee box %s%%, key employee %s%%, %s%% paid).",
                               gsub("[$^]", "", f), fam$evidence[i], fam$trustee_box[i], fam$key_box[i], fam$paid[i])) }
r1 <- row("1 board standard, job word", c1, "keep", c1$title.standard, c1$strata, c1$strata.label,
  paste0(fcase(c1$evidence == "seat", "Held as a board seat; the job word names the seat's portfolio.",
               c1$evidence == "officer seat", "An officer post held unpaid or for a stipend; the board title lets step 09 count a token-paid officer as a seat.",
               c1$evidence == "mixed", "Mixed evidence; on full 990s the boxes settle each holder.",
               c1$evidence == "job", "Evidence of a job: see the manual decision.",
               default = "Too few holders to overturn the mapping, and no paid-occupation word."),
         fam_note(c1$family)))
# check 2: keep the staff standard unless overridden
r2 <- row("2 staff standard, seat evidence", c2, "keep", c2$title.standard, c2$strata, c2$strata.label,
          "An occupation title or mixed evidence: kept; the trustee box makes a holder BOARD in step 09.")
# check 3: the rank-word rule. An assistant, deputy, associate or vice
# secretary, treasurer or clerk is an officer post of its own (about 90% tick the
# officer box, under half the trustee box): it maps to ASSISTANT SECRETARY or
# ASSISTANT TREASURER, not to the office. A deputy, associate or assistant chair
# or president is a vice chair. Senior, executive and junior forms keep their office.
asst_re <- "^(BOARD )?(ASSISTANT|ASST|DEPUTY|ASSOCIATE|ASSOC|VICE)( TO)?( BOARD)? (RECORDING |CORRESPONDING |GENERAL )?(SECRETARY|TREASURER|CLERK|ADJUTANT|FINANCE|FINANCE OFFICER|SECRETARY TREASURER|TREASURER AND S)$|^(TREASURER|SECRETARY) ASSISTANT$|^ASST(SECRETARY|TREASURER)$"
c3[, asst := grepl(asst_re, title.variant) & title.standard %in% c("BOARD SECRETARY", "BOARD TREASURER", "ASSISTANT SECRETARY")]
c3[, asst_std := fifelse(grepl("TREASURER|FINANCE", title.variant), "ASSISTANT TREASURER", "ASSISTANT SECRETARY")]
c3[, asst_lab := tolower(sub("^(BOARD )?(ASSISTANT|ASST|DEPUTY|ASSOCIATE|ASSOC|VICE).*$", "\\2", title.variant))]
c3[asst_lab %in% c("asst", "vice") | grepl("^ASST|ASSISTANT$", title.variant), asst_lab := "assistant"]
c3[asst_lab == "assoc", asst_lab := "associate"]
r3 <- row("3 rank word", c3, ifelse(c3$asst & c3$asst_std != c3$title.standard | c3$asst & c3$strata != "subordinate", "change", "keep"),
          ifelse(c3$asst, c3$asst_std, c3$title.standard), ifelse(c3$asst, "subordinate", c3$strata),
          ifelse(c3$asst, c3$asst_lab, c3$strata.label),
          ifelse(c3$asst, "Rank-word rule: an assistant, deputy, associate or vice secretary or treasurer is an officer post of its own (ASSISTANT SECRETARY: trustee box 33%, officer 90%; ASSISTANT TREASURER: 41% and 89%), not the board office.",
                 "Rank-word rule: kept (a vice chair or president is BOARD VICE PRESIDENT; senior, executive and junior forms keep their office; the C-suite forms map to the same-name standard)."))
# check 4 goes to its own table (rules, not variants)
# check 5: add an entry only for a clear title
r5 <- data.table(check = "5 uncoded", title.variant = c5$title.variant, current_standard = NA_character_,
                 current_strata = NA_character_, evidence = evidence_says(c5), evidence_detail = ev_text(c5),
                 decision = "leave uncoded", new_standard = NA_character_, new_strata = NA_character_,
                 new_strata_label = NA_character_,
                 reason = "Not a title on its own: a place, an acronym, a fragment or a compound of names.")
# check 6: same-name conflicts. Deliberate: the bare board offices (step 09
# decides with pay and boxes), CHIEF, SUPERINTENDENT and the CIO / CAO duplicates
policy <- c("DIRECTOR", "PRESIDENT", "VICE PRESIDENT", "SECRETARY", "CHIEF", "SUPERINTENDENT",
            "CHIEF ADMINISTRATION OFFICER", "VICE PRESIDENT AND CIO", "ALTERNATE", "CLERK", "REGISTRAR",
            "FINANCE OFFICER", "EXECUTIVE")
r6 <- row("6 name conflict", c6, "keep", c6$title.standard, c6$strata, c6$strata.label,
  fcase(c6$title.variant %in% policy, "Deliberate: the bare title is a board office or a generic word; step 09 settles it with pay and boxes.",
        c6$evidence %in% c("seat", "officer seat"), "Held as a seat (a club or post office such as HISTORIAN, CHAPLAIN, SURGEON, WEBMASTER): kept.",
        default = "Kept: see the manual decision if changed."))

rt <- rbind(r1, r2, r3, r5, r6)
# manual decisions override the rules
i <- match(rt$title.variant, man$title.variant)
m <- !is.na(i)
rt[m, `:=`(decision = man$decision[i[m]], new_standard = man$title.standard[i[m]], new_strata = man$strata[i[m]],
           new_strata_label = man$strata.label[i[m]], reason = man$reason[i[m]])]
rt[m & decision == "add" & !is.na(current_standard), decision := "change"]
# manual decisions for variants no check flagged
extra <- man[!title.variant %in% rt$title.variant]
if (nrow(extra)) {
  e <- xe[match(extra$title.variant, title.variant)]
  e[is.na(title.variant), `:=`(title.variant = extra$title.variant[is.na(e$title.variant)], rows = 0L, rows_990 = 0L, evidence = "too few")]
  rt <- rbind(rt, data.table(check = "manual", title.variant = extra$title.variant, current_standard = e$title.standard,
                             current_strata = paste(e$strata, e$strata.label, sep = " / "), evidence = e$evidence,
                             evidence_detail = ev_text(e), decision = extra$decision, new_standard = extra$title.standard,
                             new_strata = extra$strata, new_strata_label = extra$strata.label, reason = extra$reason))
}
rt[decision == "keep", `:=`(new_standard = NA_character_, new_strata = NA_character_, new_strata_label = NA_character_)]
# one row per variant: a variant flagged by several checks takes the decision
# that changes it, and lists every check
rt[, prio := fcase(decision %in% c("change", "add"), 1L, default = 2L)]
rt[, checks := paste(unique(check), collapse = "; "), by = title.variant]
rt <- rt[order(prio)][!duplicated(title.variant)][, check := checks][, c("prio", "checks") := NULL]
rt <- rt[order(check, decision, title.variant)]
if (any(rt$evidence == "job" & rt$decision == "keep" & rt$check == "1 board standard, job word"))
  message("check 1 cases with job evidence still kept: ",
          paste(rt[evidence == "job" & decision == "keep" & check == "1 board standard, job word", title.variant], collapse = ", "))
fwrite(rt, file.path(OUT, "review-table.csv"))

cat(sprintf("check 1: %d board variants name a job (%d seen in the sample)\n", nrow(c1), sum(c1$rows > 0)))
print(c1[, .N, by = evidence])
cat(sprintf("check 2: %d staff variants held mostly as unpaid trustee-only seats\n", nrow(c2)))
cat(sprintf("check 3: %d rank-word variants; %d follow the assistant-office rule\n", nrow(c3), sum(c3$asst)))
cat(sprintf("check 4: %d head rules; %d dropped\n%s\n", nrow(c4), sum(c4$decision == "drop"), guard_note))
cat(sprintf("check 5: top 200 uncoded titles cover %d rows\n", sum(c5$rows)))
cat(sprintf("check 6: %d variants spelled like a different standard\n", nrow(c6)))
cat("\ndecisions:\n"); print(rt[, .N, by = .(check, decision)][order(check, decision)])
