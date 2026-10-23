# 26-ceo-score.R
# Combine the two labelers' work on the CEO / board sample (25-ceo-cases.R,
# FINDINGS.md F-031) and score step 09 against it.
#
# 1. Agreement between labelers A and B: role, board role, and who is each
#    filing's chief executive. Rows where both agree form the consensus set;
#    the rest go to ceo-sample/disagreements.csv for a person to decide.
#    Decisions written there (column `decision_role`, and optionally
#    `decision_board_role` and `decision_ceo`) are applied on the next run.
# 2. Score the package's output (PARTVII_DIR/ceo-sample/classified.rds) on the
#    consensus labels: person roles, board roles, and the filing's CEO, overall
#    and by stratum.
#
# Labels are read from PARTVII_DIR/ceo-sample/labels-{A,B}/batch-NN.csv and
# copied into the repository (ceo-sample/labels-A.csv, labels-B.csv).
#
#   Rscript data-raw/partvii-validation/26-ceo-score.R
#   CLASSIFIED=<path to another classified.rds> to score other output

suppressMessages(library(data.table))
PV   <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
SET  <- Sys.getenv("CEO_SET", "ceo-sample")   # or ceo-holdout (24-ceo-sample.R)
OUT  <- file.path(PV, SET)
REPO <- file.path("data-raw/partvii-validation", SET)
CLS  <- Sys.getenv("CLASSIFIED", file.path(OUT, "classified.rds"))
roles  <- c("CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF", "BOARD", "DUAL", "UNSURE", "NONE")
broles <- c("CHAIR", "VICE CHAIR", "SECRETARY", "SECRETARY TREASURER", "TREASURER", "MEMBER")

read_labels <- function(who) {
  fs <- list.files(file.path(OUT, paste0("labels-", who)), "^batch-[0-9]+[.]csv$", full.names = TRUE)
  x <- rbindlist(lapply(fs, function(f) as.data.table(utils::read.csv(f, colClasses = "character", na.strings = character(0),
                                                                       strip.white = TRUE))), fill = TRUE)
  x[, `:=`(role = toupper(role), board_role = toupper(board_role), ceo = toupper(ceo) %in% c("Y", "YES", "TRUE"))]
  bad <- x[!role %in% roles | !board_role %in% c(broles, "")]
  if (nrow(bad)) warning(who, ": ", nrow(bad), " rows with values outside the lists, e.g. ", bad$row_id[1])
  x[!duplicated(row_id)]
}
key <- fread(file.path(REPO, "row-key.csv"), colClasses = "character")
smp <- fread(file.path(REPO, "sample.csv"), colClasses = "character")
key[smp, on = "org_id", stratum := i.stratum]
A <- read_labels("A"); B <- read_labels("B")
fwrite(A, file.path(REPO, "labels-A.csv")); fwrite(B, file.path(REPO, "labels-B.csv"))
L <- merge(A, B, by = "row_id", suffixes = c(".A", ".B"))
L <- merge(key[, .(row_id, org_id, year, OBJECTID, form, person, title_raw, pay = as.numeric(pay), hrs = as.numeric(hrs), stratum)], L, by = "row_id")
cat(sprintf("rows: key %d, labeled by A %d, by B %d, by both %d\n", nrow(key), nrow(A), nrow(B), nrow(L)))

# ---- 1. agreement ------------------------------------------------------------------------
kappa <- function(a, b) { lv <- union(a, b); t <- table(factor(a, lv), factor(b, lv)); po <- sum(diag(t)) / sum(t)
  pe <- sum(rowSums(t) * colSums(t)) / sum(t)^2; (po - pe) / (1 - pe) }
L[, agree_role := role.A == role.B]
L[, agree_board := !role.A %in% c("BOARD", "DUAL") | board_role.A == board_role.B]
L[, agree_ceo := ceo.A == ceo.B]
L[, agree := agree_role & agree_board & agree_ceo]
fceo <- L[, .(A = paste(sort(row_id[ceo.A]), collapse = ";"), B = paste(sort(row_id[ceo.B]), collapse = ";")), by = .(org_id, year, stratum)]
fceo[, same := A == B]
agree_tab <- data.table(
  measure = c("rows: role", "rows: role (Cohen's kappa)", "board seats: board role", "rows: chief executive flag",
              "filings: same chief executive", "rows: all three"),
  value = round(c(100 * mean(L$agree_role), kappa(L$role.A, L$role.B), 100 * mean(L[role.A %in% c("BOARD", "DUAL") & role.B %in% c("BOARD", "DUAL"), agree_board]),
                  100 * mean(L$agree_ceo), 100 * mean(fceo$same), 100 * mean(L$agree)), 3))
cat("\n== agreement between labelers ==\n"); print(agree_tab)
cat("\nrole confusions (A x B):\n"); print(table(A = L$role.A, B = L$role.B))
fwrite(agree_tab, file.path(REPO, "agreement.csv"))

# ---- disagreements: for a person to decide ------------------------------------------------
dp <- file.path(REPO, "disagreements.csv")
old <- if (file.exists(dp)) fread(dp, colClasses = "character", na.strings = NULL) else NULL
dis <- L[agree == FALSE, .(row_id, stratum, org = smp$name[match(org_id, smp$org_id)], year, form, person, title_raw, pay, hrs,
                           A_role = role.A, A_board = board_role.A, A_ceo = ifelse(ceo.A, "Y", ""), A_notes = notes.A,
                           B_role = role.B, B_board = board_role.B, B_ceo = ifelse(ceo.B, "Y", ""), B_notes = notes.B,
                           decision_role = "", decision_board_role = "", decision_ceo = "", decision_notes = "")]
if (!is.null(old) && "decision_role" %in% names(old)) {
  k <- match(dis$row_id, old$row_id)
  for (c in c("decision_role", "decision_board_role", "decision_ceo", "decision_notes"))
    dis[!is.na(k), (c) := old[[c]][k[!is.na(k)]]]
}
fwrite(dis, dp)
cat(sprintf("\ndisagreements: %d rows in %d filings (%d decided)\n", nrow(dis), uniqueN(dis[, .(org, year)]), sum(nzchar(dis$decision_role))))

# ---- consensus labels --------------------------------------------------------------------
G <- L[, .(row_id, org_id, year, OBJECTID, person, title_raw, pay, hrs, stratum, role = role.A, board_role = board_role.A, ceo = ceo.A,
           source = "both labelers")][L$agree]
dec <- dis[nzchar(decision_role)]
if (nrow(dec)) G <- rbind(G, L[match(dec$row_id, row_id), .(row_id, org_id, year, OBJECTID, person, title_raw, pay, hrs, stratum,
                                                           role = toupper(dec$decision_role), board_role = toupper(dec$decision_board_role),
                                                           ceo = toupper(dec$decision_ceo) == "Y", source = "decided")])
fwrite(G, file.path(REPO, "consensus.csv"))

# ---- 2. score step 09 --------------------------------------------------------------------
if (!file.exists(CLS)) { cat("no classifier output to score\n"); quit(save = "no") }
cl <- as.data.table(readRDS(CLS))
# match each labeled row to its person in the output: filing + name + reportable
# pay (dtk.comp; tot.comp also counts other compensation) + raw title, and
# without the title when that alone identifies one person
nk <- function(o, n, p, t = NULL) paste(sub("^OID-", "", o), toupper(gsub("\\s+", " ", trimws(n))), round(suppressWarnings(as.numeric(p))),
                                        if (is.null(t)) "" else toupper(gsub("\\s+", " ", trimws(t))))
cl[, `:=`(k1 = nk(object.id, dtk.name, dtk.comp, title.raw), k2 = nk(object.id, dtk.name, dtk.comp))]
one <- function(k) { p <- cl[role.primary == TRUE, .(role.final = role.final[1], role.board = role.board[1], role.ceo = role.ceo[1],
                                                     n = uniqueN(person.id)), by = c(k)]; setnames(p, k, "k"); p[n == 1] }
p1 <- one("k1"); p2 <- one("k2")
G[, `:=`(k1 = nk(OBJECTID, person, pay, title_raw), k2 = nk(OBJECTID, person, pay))]
G[p1, on = c(k1 = "k"), `:=`(pred = i.role.final, pred_board = i.role.board, pred_ceo_kind = i.role.ceo)]
G[is.na(pred), c("pred", "pred_board", "pred_ceo_kind") := p2[match(G$k2[is.na(G$pred)], p2$k), .(role.final, role.board, role.ceo)]]
G[, pred_ceo := pred %in% "CEO"]
cat(sprintf("\nconsensus rows matched to the classifier: %d of %d\n", sum(!is.na(G$pred)), nrow(G)))
S <- G[!is.na(pred) & !role %in% c("UNSURE", "NONE")]
S[, hit := ifelse(role == "DUAL", pred %in% c("CEO", "OFFICER", "BOARD"), role == pred)]
S[, label_board := role %in% c("BOARD", "DUAL")]
# a secretary-treasurer (one board role since F-032) matches a label of either office
br_ok <- function(lab, pred) lab == pred | (pred %in% "SECRETARY TREASURER" & lab %in% c("SECRETARY", "TREASURER"))
acc <- function(x) x[, .(people = .N, role_accuracy = round(100 * mean(hit), 1),
                         board_seats = sum(label_board & pred == "BOARD"),
                         board_role_right = round(100 * mean(br_ok(board_role, pred_board)[label_board & pred == "BOARD"]), 1),
                         ceo_precision = round(100 * sum(pred_ceo & ceo) / sum(pred_ceo), 1),
                         ceo_recall = round(100 * sum(pred_ceo & ceo) / sum(ceo), 1))]
# filing level: the set of chief executives
F <- S[, .(lab = paste(sort(row_id[ceo]), collapse = ";"), pred = paste(sort(row_id[pred_ceo]), collapse = ";"),
           lab_n = sum(ceo), pred_n = sum(pred_ceo)), by = .(org_id, year, stratum)]
F[, outcome := fcase(lab == pred & lab_n == 0, "right: no CEO", lab == pred, "right CEO",
                     lab_n == 0, "CEO named, none labeled", pred_n == 0, "CEO missed",
                     pred_n > lab_n & mapply(function(l, p) all(strsplit(l, ";")[[1]] %in% strsplit(p, ";")[[1]]), lab, pred),
                     "extra CEO named", default = "wrong person")]
by_str <- merge(S[, acc(.SD), by = stratum],
                F[, .(filings = .N, ceo_right = round(100 * mean(lab == pred), 1)), by = stratum], by = "stratum")
all_row <- cbind(stratum = "ALL", acc(S), F[, .(filings = .N, ceo_right = round(100 * mean(lab == pred), 1))])
res <- rbind(all_row, by_str[order(ceo_right)])
cat("\n== step 09 against the consensus labels ==\n"); print(res)
cat("\nfiling CEO outcomes:\n"); print(dcast(F[, .N, by = .(stratum, outcome)], stratum ~ outcome, value.var = "N", fill = 0))
cat("\nrole confusions (label x step 09):\n"); print(table(label = S$role, step09 = S$pred))
fwrite(res, file.path(REPO, "score-by-stratum.csv")); fwrite(F, file.path(REPO, "score-filings.csv"))
fwrite(S[, .(row_id, stratum, person, title_raw, pay, hrs, role, board_role, ceo, pred, pred_board, pred_ceo, pred_ceo_kind, hit, source)],
       file.path(REPO, "score-rows.csv"))
