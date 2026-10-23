# 29-model-eval.R
# How well do Claude models of different sizes label the CEO / board panel,
# and what is the cheapest one before quality drops? (data-dev/NEXT-PLAN.md
# item 4, FINDINGS.md F-035.)
#
# Each model labeled the 25 blind batches of the F-031 panel (150
# organizations x 2019-2023, 750 filings) with the same guide and prompt as
# the original labelers. Labels are in PARTVII_DIR/model-eval/<model>/batch-NN.csv.
# They are scored against the consensus labels (ceo-sample/consensus.csv:
# rows both labelers agreed on, plus decided rows), and step 09 (the package)
# is scored the same way as a no-cost baseline.
#
# Measures:
# - rows returned and valid;
# - board seats (role BOARD or DUAL): precision, recall, and the right board
#   role for seats found;
# - role accuracy (a DUAL label or prediction matches CEO, OFFICER, BOARD or
#   DUAL);
# - CEOs: person-level precision and recall of the chief-executive flag, and
#   filing level, the exact set of chief executives (including none);
# - CEO transitions, on organizations' consecutive years: a transition is a
#   change in the set of chief executives (people matched by name). An
#   outgoing and an incoming CEO on the same return both count as the CEO:
#   that overlap records the transition and is scored as correct, not as an
#   extra CEO.
#
# Filing-level and transition measures use "clean" filings only: filings with
# no labeler disagreement, so the reference set of chief executives is
# complete.
#
#   Rscript data-raw/partvii-validation/29-model-eval.R [model ...]
#   (default: every folder under PARTVII_DIR/model-eval except input)

suppressMessages(library(data.table))
PV   <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
EV   <- file.path(PV, "model-eval")
REPO <- "data-raw/partvii-validation/ceo-sample"
RES  <- "data-raw/partvii-validation/panel-results"
CLS  <- Sys.getenv("CLASSIFIED", file.path(PV, "ceo-sample", "classified-chancellor-fix2.rds"))
roles  <- c("CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF", "BOARD", "DUAL", "UNSURE", "NONE")
args   <- commandArgs(trailingOnly = TRUE)
models <- if (length(args)) args else setdiff(basename(list.dirs(EV, recursive = FALSE)), "input")

G   <- fread(file.path(REPO, "consensus.csv"), colClasses = "character", na.strings = NULL)
G[, ceo := toupper(ceo) %in% c("TRUE", "Y")]
key <- fread(file.path(REPO, "row-key.csv"), colClasses = "character")
dis <- fread(file.path(REPO, "disagreements.csv"), colClasses = "character", na.strings = NULL)
dis <- dis[!nzchar(decision_role)]
dis[, org_id := sub("-.*", "", row_id)]
unclean <- unique(paste(dis$org_id, dis$year))
G[, filing := paste(org_id, year)]
G[, clean := !filing %in% unclean]
normname <- function(x) gsub("\\s+", " ", trimws(gsub("[^A-Z ]", " ", toupper(x))))
G[, pname := normname(person)]
nkey <- nrow(key)

read_model <- function(m) {
  fs <- list.files(file.path(EV, m), "^batch-[0-9]+[.]csv$", full.names = TRUE)
  if (!length(fs)) return(NULL)
  x <- rbindlist(lapply(fs, function(f) as.data.table(utils::read.csv(f, colClasses = "character", na.strings = character(0),
                                                                       strip.white = TRUE, check.names = FALSE))), fill = TRUE)
  names(x) <- tolower(trimws(names(x)))
  for (v in c("role", "board_role", "ceo", "ceo_note")) if (!v %in% names(x)) x[, (v) := ""]
  x[, `:=`(role = toupper(trimws(role)), board_role = toupper(trimws(board_role)),
           ceo = toupper(trimws(ceo)) %in% c("Y", "YES", "TRUE"), ceo_note = tolower(trimws(ceo_note)))]
  x[!duplicated(row_id), .(row_id, role, board_role, ceo, ceo_note, batches = length(fs))]
}

# step 09 (the package) as a baseline, matched as in 26-ceo-score.R
read_step09 <- function() {
  if (!file.exists(CLS)) return(NULL)
  cl <- as.data.table(readRDS(CLS))
  nk <- function(o, n, p, t = NULL) paste(sub("^OID-", "", o), toupper(gsub("\\s+", " ", trimws(n))), round(suppressWarnings(as.numeric(p))),
                                          if (is.null(t)) "" else toupper(gsub("\\s+", " ", trimws(t))))
  cl[, `:=`(k1 = nk(object.id, dtk.name, dtk.comp, title.raw), k2 = nk(object.id, dtk.name, dtk.comp))]
  one <- function(k) { p <- cl[role.primary == TRUE, .(role = role.final[1], board_role = role.board[1], n = uniqueN(person.id)), by = c(k)]
    setnames(p, k, "k"); p[n == 1] }
  p1 <- one("k1"); p2 <- one("k2")
  kk <- copy(key)
  kk[, `:=`(k1 = nk(OBJECTID, person, pay, title_raw), k2 = nk(OBJECTID, person, pay))]
  kk[p1, on = c(k1 = "k"), `:=`(role = i.role, board_role = i.board_role)]
  miss <- kk$k2[is.na(kk$role)]
  kk[is.na(role), c("role", "board_role") := p2[match(miss, p2$k), .(role, board_role)]]
  kk[is.na(board_role), board_role := ""]
  kk[!is.na(role), .(row_id, role, board_role, ceo = role == "CEO", ceo_note = "", batches = NA_integer_)]
}

score <- function(P, name) {
  S <- merge(G[!role %in% c("UNSURE", "NONE")], P, by = "row_id", all.x = TRUE, suffixes = c("", ".p"))
  returned <- nrow(P[row_id %in% key$row_id]); valid <- nrow(P[row_id %in% key$row_id & role %in% roles])
  S[is.na(role.p), `:=`(role.p = "MISSING", board_role.p = "", ceo.p = FALSE)]
  dualok <- c("CEO", "OFFICER", "BOARD", "DUAL")
  S[, hit := role == role.p | (role == "DUAL" & role.p %in% dualok) | (role.p == "DUAL" & role %in% dualok)]
  S[, `:=`(lb = role %in% c("BOARD", "DUAL"), pb = role.p %in% c("BOARD", "DUAL"))]
  br_ok <- function(lab, pred) lab == pred | (pred == "SECRETARY TREASURER" & lab %in% c("SECRETARY", "TREASURER")) |
                               (lab == "SECRETARY TREASURER" & pred %in% c("SECRETARY", "TREASURER"))
  rows <- S[, .(rows_scored = .N, missing = sum(role.p == "MISSING"), role_acc = 100 * mean(hit),
                board_prec = 100 * sum(lb & pb) / sum(pb), board_rec = 100 * sum(lb & pb) / sum(lb),
                board_role_right = 100 * mean(br_ok(board_role[lb & pb], board_role.p[lb & pb])),
                ceo_prec = 100 * sum(ceo & ceo.p) / sum(ceo.p), ceo_rec = 100 * sum(ceo & ceo.p) / sum(ceo))]
  # filing level, clean filings: the exact set of chief executives
  C <- S[clean == TRUE]
  F <- C[, .(lab = paste(sort(unique(pname[ceo])), collapse = ";"), pred = paste(sort(unique(pname[ceo.p])), collapse = ";"),
             lab_n = uniqueN(pname[ceo]), pred_n = uniqueN(pname[ceo.p])), by = .(org_id, year, stratum)]
  F[, right := lab == pred]
  fil <- F[, .(filings = .N, ceo_filing_right = 100 * mean(right),
               none_right = 100 * mean(right[lab_n == 0]), overlap_filings = sum(lab_n >= 2),
               overlap_right = if (any(lab_n >= 2)) 100 * mean(right[lab_n >= 2]) else NA_real_)]
  # transitions: consecutive clean years of the same organization
  setorder(F, org_id, year)
  F[, `:=`(prev_lab = shift(lab), prev_pred = shift(pred), prev_year = shift(as.integer(year))), by = org_id]
  T <- F[!is.na(prev_year) & as.integer(year) == prev_year + 1L]
  T[, `:=`(lab_t = lab != prev_lab, pred_t = pred != prev_pred)]
  tr <- T[, .(year_pairs = .N, transitions = sum(lab_t),
              trans_prec = 100 * sum(lab_t & pred_t) / max(sum(pred_t), 1),
              trans_rec = 100 * sum(lab_t & pred_t) / max(sum(lab_t), 1),
              trans_exact = 100 * mean(lab_t == pred_t & lab == pred & prev_lab == prev_pred))]
  out <- cbind(model = name, returned = returned, valid = valid, rows, fil, tr)
  num <- names(out)[vapply(out, is.numeric, TRUE)]
  out[, (num) := lapply(.SD, function(v) round(v, 1)), .SDcols = num]
  list(summary = out, filings = cbind(model = name, F), rows = cbind(model = name, S[, .(row_id, stratum, role, role.p, ceo, ceo.p, hit)]))
}

R <- list()
s9 <- read_step09(); if (!is.null(s9)) R[["step09"]] <- score(s9, "step 09 (package)")
for (m in models) { P <- read_model(m); if (!is.null(P)) R[[m]] <- score(P, m) }
summ <- rbindlist(lapply(R, `[[`, "summary"), fill = TRUE)
cat(sprintf("consensus rows %d (clean filings %d of 750); key rows %d\n", nrow(G), uniqueN(G[clean == TRUE, filing]), nkey))
print(summ)
by_str <- rbindlist(lapply(R, function(r) r$filings[, .(ceo_filing_right = round(100 * mean(right), 1), filings = .N), by = .(model, stratum)]))
print(dcast(by_str, stratum ~ model, value.var = "ceo_filing_right"))
fwrite(summ, file.path(RES, "model-eval-summary.csv"))
fwrite(dcast(by_str, stratum ~ model, value.var = "ceo_filing_right"), file.path(RES, "model-eval-by-stratum.csv"))
fwrite(rbindlist(lapply(R, `[[`, "filings"), fill = TRUE), file.path(EV, "model-eval-filings.csv"))
