# 10-pattern-rules.R
# Learn "head" rules from the titles the crosswalk already maps, for titles it
# does not (tiers 3-4: about 600,000 rare titles).
#
# A head is a crosswalk variant found at the end of a title (suffix head:
# FINANCE COMMITTEE MEMBER -> COMMITTEE MEMBER) or at its start (prefix head:
# BOARD MEMBER EMERITUS -> BOARD MEMBER; VICE PRESIDENT OF X -> VICE PRESIDENT),
# as whole words and never the whole title. For each head, the known titles
# that share it vote: the rule's standard is the most common standard among
# them, and its precision is the share of those known titles that map to it.
# A head becomes a rule when it has at least `min_support` known titles and a
# precision of at least `min_precision`. Precision is measured on titles the
# rule did not come from (each known title is matched only by its proper
# suffixes and prefixes), so it estimates out-of-sample accuracy.
#
# Writes data-raw/crosswalks/title-patterns.csv (position, head,
# title.standard, support, precision) and, in <PARTVII>/data,
# pattern-matches.parquet (the unmatched titles and the rule each one gets).
#   Rscript data-raw/partvii-validation/10-pattern-rules.R

source("data-raw/partvii-validation/_config.R")
suppressMessages(library(data.table))
suppressMessages(devtools::load_all(quiet = TRUE))   # title_heads(), the same head finder the pipeline uses
min_support <- 5L
min_precision <- 0.80

xw <- as.data.table(utils::read.csv("data-raw/crosswalks/title-standardization.csv", colClasses = "character",
                                    na.strings = character(0), check.names = FALSE))
xw <- xw[title.variant != "" & title.standard != ""]
std_of <- setNames(xw$title.standard, xw$title.variant)
p <- as.data.table(arrow::read_parquet(file.path(pv_data, "title-profiles.parquet")))[TitleTxt7 != ""]

# learn only from exact crosswalk matches ("mapped"), never from earlier pattern matches
known <- p[crosswalk == "mapped", .(title = TitleTxt7, standard = title_standard, n_rows)]
known <- cbind(known, title_heads(known$title, xw$title.variant))

# role level of a standard: the board role for board standards, else the employee level
tx <- as.data.table(utils::read.csv("data-raw/crosswalks/title-taxonomy.csv", colClasses = "character",
                                    na.strings = character(0), check.names = FALSE))
level_of <- setNames(fifelse(tx$board.role != "", paste("BOARD", tx$board.role), tx$emp.level), tx$title.standard)
known[, level := level_of[standard]]

learn <- function(pos) {
  k <- known[!is.na(get(pos)), .(head = get(pos), standard, level, n_rows)]
  r <- k[, .(n = .N, rows = sum(n_rows)), by = .(head, standard)][
    order(head, -n, -rows)][, .(majority = standard[1], support = sum(n), precision = n[1] / sum(n)), by = head]
  # level agreement with the head's own standard: how often known titles with
  # this head land on the same role level as the head itself
  k[, head_level := level_of[std_of[head]]]
  lv <- k[, .(level_share = mean(!is.na(level) & level == head_level)), by = head]
  r <- merge(r, lv, by = "head")
  r[, position := pos][]
}
rules <- rbind(learn("suffix"), learn("prefix"))
# a head qualifies by its majority standard (precision), or, when its known
# titles spread over specific standards of one role level, by mapping to the
# head's own generic standard (level agreement)
rules[, `:=`(by_majority = support >= min_support & precision >= min_precision,
             by_level = support >= min_support & level_share >= 0.85 & !is.na(std_of[head]))]
rules[, title.standard := fifelse(by_majority, majority, fifelse(by_level, std_of[head], NA_character_))]
rules[, rule := fifelse(by_majority, "majority", fifelse(by_level, "level", NA_character_))]
rules[, precision := fifelse(by_majority, precision, level_share)]
rules[, accepted := by_majority | by_level]

# context-dependent heads get no rule (F-017). In the blind checks of tier 3-4
# titles (DECISIONS.md, round 4), rare "VICE PRESIDENT OF X" titles were mostly
# volunteer board VPs, not staff VPs (60% same role level), "DIRECTOR OF X"
# titles were often board members with a portfolio (59%), and "... AND
# DIRECTOR" (14%), "... MANAGER" (57%) and "TREASURER ..." (67%) were as mixed.
# These titles stay unmatched; step 09 resolves the role from the checkboxes
# and pay.
context_dependent <- function(position, head, standard)
  standard %in% c("VICE PRESIDENT", "SENIOR VICE PRESIDENT", "EXECUTIVE VICE PRESIDENT", "DIRECTOR") |
  (position == "prefix" & head %in% c("VICE PRESIDENT", "VICE PRESIDENT OF", "TREASURER", "SECRETARY", "PRESIDENT",
                                      "DIRECTOR", "DIRECTOR OF", "SENIOR DIRECTOR")) |
  (position == "suffix" & head %in% c("MANAGER", "TREASURER", "SECRETARY", "PRESIDENT", "VICE PRESIDENT",
                                      "DIRECTOR", "AND DIRECTOR"))
rules[accepted == TRUE & context_dependent(position, head, title.standard), `:=`(accepted = FALSE, rule = "context-dependent")]
message(sprintf("heads: %d suffix, %d prefix; accepted %d suffix, %d prefix",
                rules[position == "suffix", .N], rules[position == "prefix", .N],
                rules[position == "suffix" & accepted, .N], rules[position == "prefix" & accepted, .N]))

# leave-one-out check on the known titles: what the accepted rules would give them
acc <- rules[accepted == TRUE]
pick <- function(d) {
  s <- acc[position == "suffix"][d, on = c(head = "suffix"), .(title.standard, precision)]
  r <- acc[position == "prefix"][d, on = c(head = "prefix"), .(title.standard, precision)]
  use_s <- !is.na(s$title.standard) & (is.na(r$title.standard) | s$precision >= r$precision)
  data.table(pattern_standard = fifelse(use_s, s$title.standard, r$title.standard),
             pattern_position = fifelse(use_s, "suffix", fifelse(is.na(r$title.standard), NA_character_, "prefix")),
             pattern_head = fifelse(use_s, d$suffix, fifelse(is.na(r$title.standard), NA_character_, d$prefix)))
}
known <- cbind(known, pick(known))
cv <- known[!is.na(pattern_standard)]
message(sprintf("leave-one-out on known titles: %d covered of %d multi-word known titles; same standard %.1f%% of titles (%.1f%% of rows); same role level %.1f%% of titles",
                nrow(cv), known[grepl(" ", title), .N], 100 * cv[, mean(pattern_standard == standard)],
                100 * cv[, sum(n_rows[pattern_standard == standard]) / sum(n_rows)],
                100 * cv[, mean(level_of[pattern_standard] == level, na.rm = TRUE)]))

# apply to the unmatched titles
un <- p[crosswalk %in% c("missing", "pattern"), .(title = TitleTxt7, tier, n_rows, n_filings, pct_officer, pct_trustee, pct_paid, med_hours, pct_ez)]
un <- cbind(un, title_heads(un$title, xw$title.variant))
un <- cbind(un, pick(un))
tot <- sum(p$n_rows)
message(sprintf("unmatched titles: %d (%.2f%% of rows); a rule matches %d titles, %.2f%% of all rows; still unmatched %.2f%% of rows",
                nrow(un), 100 * sum(un$n_rows) / tot, un[!is.na(pattern_standard), .N],
                100 * un[!is.na(pattern_standard), sum(n_rows)] / tot, 100 * un[is.na(pattern_standard), sum(n_rows)] / tot))
print(un[!is.na(pattern_standard), .(titles = .N, rows = sum(n_rows)), by = tier][order(tier)])

out <- rules[accepted == TRUE, .(position, head, title.standard, rule, support, precision = round(precision, 3))][order(position, head)]
fwrite(out, "data-raw/crosswalks/title-patterns.csv")
arrow::write_parquet(un, file.path(pv_data, "pattern-matches.parquet"))
arrow::write_parquet(known, file.path(pv_data, "pattern-loo.parquet"))
message("rules written: ", nrow(out))
