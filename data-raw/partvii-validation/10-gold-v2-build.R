# 10-gold-v2-build.R
# Build the v2 role-label set (FINDINGS.md F-024): refresh the 158 August labels
# to the current role standard and draw 160 new people, then write blind case
# files for the labeling agents. Run once; 11-gold-v2-assemble.R combines the
# agents' labels.
#
# Refresh. The August vocabulary maps onto the v2 one (gold-check/LABELING-GUIDE-v2.md)
# where the mapping is unambiguous. Examples:
#   - CEO -> CEO
#   - CFO -> OFFICER
#   - BOARD_CHAIR -> BOARD / CHAIR
#   - BOARD_OFFICER -> BOARD / VICE CHAIR, SECRETARY or TREASURER, read from the
#     title
# Cases that need judgment go to the agents:
#   - KEY_EMPLOYEE and STAFF (PROFESSIONAL or STAFF?)
#   - board officers whose title names no post
#   - DUAL with no board seat named
# The 22 relabels in relabel-consensus.csv are applied first. The title
# columns are recomputed with the current pipeline.
#
# New sample. 160 people from the same 2010-12 slice, stratified so every part
# of step 09's decision is covered. It includes 40 from 990-EZ filings, which
# the August sample lacked. There is at most one person per filing, and no
# filing that is already in the sample. Seed 2026.
#
#   Rscript data-raw/partvii-validation/10-gold-v2-build.R

source("tests/regression/regression-helpers.R"); tc_load_package()
S   <- Sys.getenv("SYNTHID_DEV", "../synthid/dev")
D   <- "data-raw/partvii-validation/gold-check"

raw <- readRDS(file.path(S, "data", "slice_2010_2012_1000eins.rds"))
raw[] <- lapply(raw, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x })
xw  <- list(status = status.codes, xwalk = title.xwalk, taxonomy = title.taxonomy)
invisible(capture.output(out <- tc_run_pipeline(raw, xw)))
k   <- function(o, t, c) paste(o, toupper(trimws(t)), round(as.numeric(c)))
out$.k <- k(out$object.id, out$title.raw, out$tot.comp)
pr  <- out[out$role.primary, ]

# ---- 1. refresh the August labels -------------------------------------------
g <- utils::read.csv(file.path(S, "gold_sample_labeled.csv"), colClasses = "character")
rc <- utils::read.csv(file.path(D, "relabel-consensus.csv"), colClasses = "character")
rc <- rc[nzchar(rc$gold_role), ]
i  <- match(paste(rc$OBJECTID, rc$title.raw, rc$comp), paste(g$OBJECTID, g$title.raw, g$comp))
g$gold_role[i] <- rc$gold_role
g$.k <- k(g$OBJECTID, g$title.raw, g$comp)
# two people were sampled twice, in two buckets: keep one row per person
g <- g[!duplicated(paste(g$OBJECTID, g$person.id)), ]

first_post <- function(t) {   # the first board post a title names
  t <- toupper(t)
  pos <- c(`VICE CHAIR` = regexpr("VICE|\\bVP\\b", t), SECRETARY = regexpr("SECRETARY|\\bSEC\\b", t),
           TREASURER = regexpr("TREASURER|\\bTREAS\\b", t), CHAIR = regexpr("CHAIR|PRESIDENT", t))
  pos <- pos[pos > 0]
  if (!length(pos)) return("")
  if ("VICE CHAIR" %in% names(pos) && "CHAIR" %in% names(pos)) pos <- pos[names(pos) != "CHAIR"]
  names(pos)[which.min(pos)]
}
map <- list(CEO = c("CEO", ""), CO_CEO = c("CEO", ""), INTERIM_CEO = c("CEO", ""),
            CFO = c("OFFICER", ""), COO = c("OFFICER", ""), C_LEVEL_OTHER = c("OFFICER", ""),
            OFFICER = c("OFFICER", ""), MANAGER = c("MANAGER", ""),
            BOARD_CHAIR = c("BOARD", "CHAIR"), BOARD_MEMBER = c("BOARD", "MEMBER"),
            UNSURE = c("UNSURE", ""), NONE = c("NONE", ""))
g$role <- ""; g$board_role <- ""; g$ceo_note <- ""; g$needs_label <- FALSE
for (j in seq_len(nrow(g))) {
  r <- g$gold_role[j]
  if (r %in% names(map)) { g$role[j] <- map[[r]][1]; g$board_role[j] <- map[[r]][2] }
  else if (r == "BOARD_OFFICER") { p <- first_post(g$title.raw[j]); g$role[j] <- "BOARD"; g$board_role[j] <- p
                                   g$needs_label[j] <- !nzchar(p) || p == "CHAIR" }
  else if (r == "DUAL") { g$role[j] <- "DUAL"
    n <- toupper(g$gold_notes[j])
    g$board_role[j] <- if (grepl("BOARD_CHAIR", n)) "CHAIR" else if (grepl("BOARD_MEMBER", n)) "MEMBER" else first_post(g$title.raw[j])
    g$needs_label[j] <- !nzchar(g$board_role[j]) }
  else if (r %in% c("KEY_EMPLOYEE", "STAFF")) g$needs_label[j] <- TRUE
  else stop("unmapped gold_role: ", r)
}
g$ceo_note[g$gold_role == "CO_CEO"] <- "co"; g$ceo_note[g$gold_role == "INTERIM_CEO"] <- "interim"
# current title columns
cur <- out[!duplicated(out$.k), c(".k", "title.v7", "title.standard")]
g$title.v7 <- cur$title.v7[match(g$.k, cur$.k)]; g$title.standard <- cur$title.standard[match(g$.k, cur$.k)]

# ---- 2. draw the new people ----------------------------------------------------
set.seed(2026)
used <- unique(g$OBJECTID)
pool <- pr[!pr$object.id %in% used, ]
pool$paid <- pool$tot.comp > 0
top <- function(d) d[ave(-d$tot.comp, d$object.id, FUN = rank, ties.method = "first") == 1, ]
f990 <- pool$formtype == "990"
strata <- list(
  designated_ceo     = list(n = 15, rows = f990 & pool$role.ceo %in% c("designated", "co-ceo", "interim")),
  imputed_ceo        = list(n = 20, rows = f990 & pool$role.ceo %in% "imputed"),
  board_governed_top = list(n = 15, rows = FALSE),   # set below
  board_title_officer_box = list(n = 20, rows = f990 & nzchar(pool$board.role) & pool$dtk.officer.x %in% 1),
  ambiguous_title_paid    = list(n = 20, rows = f990 & pool$paid &
                              pool$title.standard %in% c("VICE PRESIDENT", "DIRECTOR", "SECRETARY", "TREASURER",
                                                         "BOARD MEMBER", "BOARD PRESIDENT", "BOARD VICE PRESIDENT",
                                                         "BOARD SECRETARY", "BOARD TREASURER")),
  professional       = list(n = 10, rows = f990 & pool$role.final == "PROFESSIONAL"),
  staff_paid         = list(n = 10, rows = f990 & pool$role.final == "STAFF" & pool$paid),
  manager            = list(n = 10, rows = f990 & pool$role.final == "MANAGER"),
  ez_paid_top        = list(n = 20, rows = FALSE),   # set below
  ez_unpaid          = list(n = 20, rows = !f990 & !pool$paid)
)
# board-governed and 990-EZ paid strata take the top-paid person of a filing
tp <- top(pool[pool$paid, ])$.k
strata$board_governed_top$rows <- f990 & pool$org.leadership == "board_governed" & pool$.k %in% tp
strata$ez_paid_top$rows        <- !f990 & pool$paid & pool$.k %in% tp
new <- NULL; taken <- character(0)
for (s in names(strata)) {
  cand <- pool[strata[[s]]$rows & !pool$object.id %in% taken, ]
  cand <- cand[!duplicated(cand$object.id), ]
  pick <- cand[sample(nrow(cand), min(strata[[s]]$n, nrow(cand))), ]
  pick$stratum <- s; taken <- c(taken, pick$object.id); new <- rbind(new, pick)
}
new$case_id <- sprintf("N%03d", seq_len(nrow(new)))

# ---- 3. blind case files ----------------------------------------------------
refresh <- g[g$needs_label, ]
refresh$case_id <- sprintf("R%02d", seq_len(nrow(refresh)))
b <- function(x) ifelse(!is.na(x) & suppressWarnings(as.numeric(x)) %in% 1, "Y", "-")
render <- function(case_id, oid, title, comp, hrs, boxes) {
  f <- pr[pr$object.id == oid, ]; f <- f[order(-f$tot.comp, -f$tot.hours), ]
  c(sprintf("## %s  -  %s (%s, form %s)", case_id, f$org.name[1], f$taxyr[1], f$formtype[1]), "",
    sprintf("**Target:** `%s`  -  pay $%s, %s h/wk, boxes %s", title, format(round(as.numeric(comp)), big.mark = ","), hrs, boxes), "",
    sprintf("Everyone in the filing (%d people):", nrow(f)), "",
    "| title (raw) | pay | h/wk | T | O | K | H |", "|---|---|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s | %s | %s | %s |", gsub("|", "/", head(f$title.raw, 25), fixed = TRUE),
            format(round(head(f$tot.comp, 25)), big.mark = ","), head(f$tot.hours, 25),
            b(head(f$dtk.indiv.trustee.x, 25)), b(head(f$dtk.officer.x, 25)),
            b(head(f$dtk.key.empl.x, 25)), b(head(f$dtk.high.comp.x, 25))),
    if (nrow(f) > 25) sprintf("| ... %d more, all lower paid | | | | | | |", nrow(f) - 25), "")
}
boxes_new <- function(r) sprintf("T:%s O:%s K:%s H:%s", b(r$dtk.indiv.trustee.x), b(r$dtk.officer.x), b(r$dtk.key.empl.x), b(r$dtk.high.comp.x))
tf <- function(x) ifelse(x == "TRUE", "Y", "-")
L_ref <- unlist(lapply(seq_len(nrow(refresh)), function(i) { r <- refresh[i, ]
  render(r$case_id, r$OBJECTID, r$title.raw, r$comp, r$hrs,
         sprintf("T:%s O:%s K/H:%s", tf(r$cb_tru), tf(r$cb_off), tf(r$cb_key))) }))
L_new <- unlist(lapply(seq_len(nrow(new)), function(i) { r <- new[i, ]
  render(r$case_id, r$object.id, r$title.raw, r$tot.comp, r$tot.hours, boxes_new(r)) }))
head_md <- function(part, n) c(sprintf("# Role labeling, v2: %s (%d cases)", part, n), "",
  "Label each case's **target person** following `LABELING-GUIDE-v2.md`. Columns: case_id,",
  "role, board_role, ceo_note, confidence, notes. Boxes: T = trustee, O = officer, K = key",
  "employee, H = highly compensated; 990-EZ filings have no boxes. Filings are tax years 2010-12.", "")
half <- ceiling(nrow(new) / 2)
idx  <- cumsum(grepl("^## N", L_new))
writeLines(c(head_md("part 1", nrow(refresh) + half), L_ref, L_new[idx <= half]), file.path(D, "v2-cases-part1.md"))
writeLines(c(head_md("part 2", nrow(new) - half), L_new[idx > half]), file.path(D, "v2-cases-part2.md"))

# ---- 4. keys (not for the agents) ------------------------------------------
utils::write.csv(g[, c("bucket", "OBJECTID", "org.name", "taxyr", "title.raw", "title.v7", "title.standard",
                       "comp", "hrs", "cb_tru", "cb_off", "cb_key", "gold_role", "role", "board_role",
                       "ceo_note", "needs_label")], file.path(D, "v2-existing-mapped.csv"), row.names = FALSE)
utils::write.csv(data.frame(case_id = refresh$case_id, OBJECTID = refresh$OBJECTID, title.raw = refresh$title.raw,
                            comp = refresh$comp, old_gold_role = refresh$gold_role),
                 file.path(D, "v2-refresh-key.csv"), row.names = FALSE)
utils::write.csv(data.frame(case_id = new$case_id, stratum = new$stratum, OBJECTID = new$object.id,
                            org.name = new$org.name, taxyr = new$taxyr, formtype = new$formtype,
                            title.raw = new$title.raw, title.v7 = new$title.v7, title.standard = new$title.standard,
                            comp = new$tot.comp, hrs = new$tot.hours,
                            cb_tru = b(new$dtk.indiv.trustee.x), cb_off = b(new$dtk.officer.x),
                            cb_key = b(new$dtk.key.empl.x), cb_high = b(new$dtk.high.comp.x)),
                 file.path(D, "v2-new-key.csv"), row.names = FALSE)
cat(sprintf("existing: %d mapped, %d to the agents | new: %d people in %d strata\n",
            sum(!g$needs_label), nrow(refresh), nrow(new), length(unique(new$stratum))))
print(table(new$stratum))
