# 09-gold-check.R
# Score step 09 (resolve_roles) against the hand-labeled role sample (FINDINGS.md
# F-023). Runs the working-tree pipeline, steps 01-09, on the raw 2010-12 rows
# the sample was drawn from, matches each labeled person, and reports agreement.
#
# Inputs, from the synthid repository (set SYNTHID_DEV to move them):
#   data/slice_2010_2012_1000eins.rds  raw Part VII rows, 3,027 filings
#   gold_sample_labeled.csv            158 labeled people (gold_role)
# If gold-check/relabel-consensus.csv exists, its gold_role (both relabeling
# agents agreed, or the user decided a split) replaces the
# original label.
#
# Writes gold-check/gold-check-results.csv (one row per labeled person). When
# gold-check/gold-v2.csv exists (11-gold-v2-assemble.R), it also scores the v2
# label set (v2 vocabulary, board roles, by set and form) into gold-v2-results.csv.
#
#   Rscript data-raw/partvii-validation/09-gold-check.R

source("tests/regression/regression-helpers.R"); tc_load_package()
S   <- Sys.getenv("SYNTHID_DEV", "../synthid/dev")
out_dir <- "data-raw/partvii-validation/gold-check"

raw <- readRDS(file.path(S, "data", "slice_2010_2012_1000eins.rds"))
raw[] <- lapply(raw, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
xw  <- list(status = status.codes, xwalk = title.xwalk, taxonomy = title.taxonomy)
invisible(capture.output(out <- tc_run_pipeline(raw, xw)))

g <- utils::read.csv(file.path(S, "gold_sample_labeled.csv"), colClasses = "character")
cons <- file.path(out_dir, "relabel-consensus.csv")
if (file.exists(cons)) {
  rc <- utils::read.csv(cons, colClasses = "character")
  rc <- rc[nzchar(rc$gold_role), ]   # both agents agreed, or the user decided
  i <- match(paste(rc$OBJECTID, rc$title.raw, rc$comp), paste(g$OBJECTID, g$title.raw, g$comp))
  g$gold_role[i] <- rc$gold_role
  g$gold_source[i] <- ifelse(rc$decided_by == "both agents", "relabel consensus (2 agents)", rc$decided_by)
  message(sprintf("applied %d relabels from relabel-consensus.csv", length(i)))
}

# person ids are hashes that differ between runs: match on filing + raw title + pay,
# and keep a match only when every candidate person has the same resolved role
k <- function(o, t, c) paste(o, toupper(trimws(t)), round(as.numeric(c)))
out$.k <- k(out$object.id, out$title.raw, out$tot.comp)
g$.k   <- k(g$OBJECTID, g$title.raw, g$comp)
one <- function(col) vapply(g$.k, function(key) { r <- unique(out[[col]][out$.k == key]); if (length(r) == 1) r else NA_character_ }, "")
g$role.final <- one("role.final"); g$role.board <- one("role.board"); g$role.ceo <- one("role.ceo")

# gold roles -> the step 09 vocabulary. KEY_EMPLOYEE and STAFF accept
# PROFESSIONAL or STAFF; DUAL (board member and executive) accepts CEO,
# OFFICER or BOARD; UNSURE and NONE are not scored.
coarse <- c(CEO = "CEO", CO_CEO = "CEO", INTERIM_CEO = "CEO",
            BOARD_CHAIR = "BOARD", BOARD_OFFICER = "BOARD", BOARD_MEMBER = "BOARD",
            CFO = "OFFICER", COO = "OFFICER", C_LEVEL_OTHER = "OFFICER", OFFICER = "OFFICER",
            MANAGER = "MANAGER", KEY_EMPLOYEE = "STAFF", STAFF = "STAFF", DUAL = "DUAL")
g$gold <- unname(coarse[g$gold_role])
g$hit <- ifelse(g$gold == "STAFF", g$role.final %in% c("STAFF", "PROFESSIONAL"),
         ifelse(g$gold == "DUAL",  g$role.final %in% c("CEO", "OFFICER", "BOARD"),
                g$gold == g$role.final))
e <- g[!is.na(g$gold) & !is.na(g$role.final), ]

cat(sprintf("scored %d of %d labeled people (not scored: %d UNSURE/NONE, %d unmatched)\n",
            nrow(e), nrow(g), sum(is.na(g$gold)), sum(!is.na(g$gold) & is.na(g$role.final))))
cat(sprintf("agreement: %.1f%%\n\n", 100 * mean(e$hit)))
pr <- function(cls) { p <- e$role.final == cls; t <- e$gold == cls
  c(precision = round(sum(p & t) / sum(p), 2), recall = round(sum(p & t) / sum(t), 2), labeled = sum(t)) }
print(t(sapply(c("CEO", "BOARD", "OFFICER", "MANAGER"), pr)))
cat("\n"); print(table(gold = e$gold, step09 = e$role.final))

utils::write.csv(g[, c("bucket", "OBJECTID", "org.name", "title.raw", "comp", "hrs", "cb_tru", "cb_off",
                       "cb_key", "gold_role", "gold_source", "role.final", "role.board", "role.ceo", "hit")],
                 file.path(out_dir, "gold-check-results.csv"), row.names = FALSE)

# ---- v2: the refreshed and doubled label set (11-gold-v2-assemble.R) --------------
v2f <- file.path(out_dir, "gold-v2.csv")
if (file.exists(v2f)) {
  v <- utils::read.csv(v2f, colClasses = "character", na.strings = character(0))
  v$.k <- k(v$OBJECTID, v$title.raw, v$comp)
  vone <- function(col) vapply(v$.k, function(key) { r <- unique(out[[col]][out$.k == key]); if (length(r) == 1) r else NA_character_ }, "")
  v$role.final <- vone("role.final"); v$role.board <- vone("role.board")
  # DUAL (a board seat and an executive job) accepts CEO, OFFICER or BOARD
  v$hit <- ifelse(v$role == "DUAL", v$role.final %in% c("CEO", "OFFICER", "BOARD"), v$role == v$role.final)
  ev <- v[!v$role %in% c("UNSURE", "NONE") & !is.na(v$role.final), ]
  cat(sprintf("\n==== v2 label set: scored %d of %d people (%d UNSURE/NONE, %d unmatched) ====\n",
              nrow(ev), nrow(v), sum(v$role %in% c("UNSURE", "NONE")), sum(is.na(v$role.final))))
  cat(sprintf("agreement: %.1f%%\n", 100 * mean(ev$hit)))
  by <- function(f) { a <- tapply(ev$hit, f, mean); n <- tapply(ev$hit, f, length)
    print(data.frame(people = as.vector(n), agreement = paste0(round(100 * as.vector(a), 1), "%"), row.names = names(a))) }
  cat("\nby set:\n"); by(ev$set); cat("\nby form:\n"); by(ev$formtype)
  pr2 <- function(cls) { p <- ev$role.final == cls; t <- ev$role == cls
    c(precision = round(sum(p & t) / sum(p), 2), recall = round(sum(p & t) / sum(t), 2), labeled = sum(t)) }
  cat("\n"); print(t(sapply(c("CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF", "BOARD"), pr2)))
  b <- ev[ev$role == "BOARD" & ev$role.final == "BOARD", ]
  cat(sprintf("\nboard seats with the right board role: %d of %d (%.0f%%)\n", sum(b$board_role == b$role.board),
              nrow(b), 100 * mean(b$board_role == b$role.board)))
  cat("\n"); print(table(label = ev$role, step09 = ev$role.final))
  utils::write.csv(v[, setdiff(names(v), ".k")], file.path(out_dir, "gold-v2-results.csv"), row.names = FALSE)
}
