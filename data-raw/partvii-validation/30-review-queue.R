# 30-review-queue.R
# Build the label review queue for the CEO / board panel (NEXT-PLAN.md item 1,
# FINDINGS.md F-035). Uses agreement among the four model labelings
# (29-model-eval.R) to rank which rows a person should decide:
#
#   1. undecided: models split or agree on a third answer   (decide)
#   2. likely label error: the three stronger models all reject the consensus
#   3. undecided: the stronger models side with one labeler  (majority)
#   4. undecided: all three stronger models side with one labeler (confirm)
#   5. weakly confirmed: only one of the three stronger models agrees with the consensus
#
# Writes PARTVII_DIR/model-eval/review-queue.json: the cases, each with its
# filing (every person, boxes, pay, hours), the person's other years, the
# original labels, the four models' labels and notes, and step 09's role.
# The review page embeds this file.
#
#   Rscript data-raw/partvii-validation/30-review-queue.R

suppressMessages({ library(data.table); library(jsonlite) })
PV   <- Sys.getenv("PARTVII_DIR", file.path(Sys.getenv("USERPROFILE", "~"), "Documents", "PARTVII"))
EV   <- file.path(PV, "model-eval")
REPO <- "data-raw/partvii-validation/ceo-sample"
models <- c("haiku", "sonnet", "opus", "fable"); strong <- c("sonnet", "opus", "fable")

# ---- the case files: filing headers and every row with its boxes ----------------------
parse_batch <- function(f) {
  L <- readLines(f, warn = FALSE, encoding = "UTF-8")
  org <- NA; rows <- list(); fil <- list(); orgs <- list()
  for (i in seq_along(L)) {
    l <- L[i]
    if (startsWith(l, "## O")) {
      org <- sub("^## (O[0-9]+).*", "\\1", l)
      orgs[[org]] <- data.table(org_id = org, org_name = trimws(sub("^## O[0-9]+\\s+-\\s+", "", l)),
                                org_info = if (i + 2 <= length(L)) trimws(L[i + 2]) else "")
    } else if (startsWith(l, "### O")) {
      fil[[length(fil) + 1]] <- data.table(org_id = org, year = sub(".*tax year ([0-9]{4}).*", "\\1", l),
                                           filing_info = trimws(sub("^### O[0-9]+\\s+-\\s+", "", l)))
    } else if (grepl("^\\| O[0-9]+-[0-9]{4}-", l)) {
      v <- trimws(strsplit(sub("^\\|", "", sub("\\|\\s*$", "", l)), "|", fixed = TRUE)[[1]])
      rows[[length(rows) + 1]] <- as.list(setNames(v[1:14], c("row_id", "name", "title", "pay", "rel_pay", "other", "hrs",
                                                               "rel_hrs", "T", "I", "O", "K", "H", "F")))
    }
  }
  list(rows = rbindlist(rows), filings = rbindlist(fil), orgs = rbindlist(orgs))
}
P <- lapply(list.files(file.path(REPO, "cases"), "^batch-[0-9]+[.]md$", full.names = TRUE), parse_batch)
rows <- rbindlist(lapply(P, `[[`, "rows")); filings <- rbindlist(lapply(P, `[[`, "filings")); orgs <- rbindlist(lapply(P, `[[`, "orgs"))
rows[, `:=`(org_id = sub("-.*", "", row_id), year = sub("^O[0-9]+-([0-9]{4})-.*", "\\1", row_id))]
rows[, boxes := paste0(ifelse(T == "Y", "T", ""), ifelse(I == "Y", "I", ""), ifelse(O == "Y", "O", ""),
                       ifelse(K == "Y", "K", ""), ifelse(H == "Y", "H", ""), ifelse(F == "Y", "F", ""))]

# ---- labels -------------------------------------------------------------------------
lab <- function(role, board, ceo) paste0(role, ifelse(nzchar(board), paste0(" (", board, ")"), ""), ifelse(ceo, " +CEO", ""))
rd <- function(m) {
  x <- rbindlist(lapply(list.files(file.path(EV, m), "csv$", full.names = TRUE), function(f)
    as.data.table(read.csv(f, colClasses = "character", na.strings = character(0)))), fill = TRUE)
  x[, .(row_id, role = toupper(trimws(role)), board = toupper(trimws(board_role)), ceo = toupper(trimws(ceo)) == "Y",
        note = trimws(notes), model = m)]
}
M <- rbindlist(lapply(models, rd))
bk <- function(role, board) ifelse(role %in% c("BOARD", "DUAL"), board, "")
M[, key := paste(role, bk(role, board), ceo, sep = "|")]
# two labels agree when role, board office and the CEO flag match; a combined
# SECRETARY TREASURER matches SECRETARY or TREASURER, as the scorers count it
# (the F-031 labels predate the combined office)
same <- function(a, b) {
  pa <- tstrsplit(a, "|", fixed = TRUE); pb <- tstrsplit(b, "|", fixed = TRUE)
  st <- function(x, y) x == "SECRETARY TREASURER" & y %in% c("SECRETARY", "TREASURER")
  !is.na(a) & !is.na(b) & pa[[1]] == pb[[1]] & pa[[3]] == pb[[3]] &
    (pa[[2]] == pb[[2]] | st(pa[[2]], pb[[2]]) | st(pb[[2]], pa[[2]]))
}
W <- dcast(M, row_id ~ model, value.var = "key")
A <- fread(file.path(REPO, "labels-A.csv"), colClasses = "character", na.strings = NULL)
B <- fread(file.path(REPO, "labels-B.csv"), colClasses = "character", na.strings = NULL)
tf <- function(x) toupper(x) %in% c("TRUE", "Y")
A[, ceo := tf(ceo)]; B[, ceo := tf(ceo)]
G <- fread(file.path(REPO, "consensus.csv"), colClasses = "character", na.strings = NULL)
G[, ceo := tf(ceo)]
D <- fread(file.path(REPO, "disagreements.csv"), colClasses = "character", na.strings = NULL)[!nzchar(decision_role)]
S9 <- fread(file.path(REPO, "score-rows.csv"), colClasses = "character", na.strings = NULL)[, .(row_id, s9 = pred, s9_board = pred_board)]

vote <- function(dt, ref) rowSums(sapply(strong, function(m) same(dt[[m]], ref)))
# undecided rows
U <- merge(D[, .(row_id)], W, by = "row_id")
U[A, on = "row_id", A := paste(i.role, bk(i.role, i.board_role), i.ceo, sep = "|")][B, on = "row_id", B := paste(i.role, bk(i.role, i.board_role), i.ceo, sep = "|")]
U[, `:=`(vA = vote(.SD, A), vB = vote(.SD, B))]
U[, queue := fcase(vA == 3 | vB == 3, 4L,
                   (vA >= 2 & vA > vB) | (vB >= 2 & vB > vA), 3L,
                   default = 1L)]
U[, lean := fcase(vA > vB, "A", vB > vA, "B", default = "")]
# consensus rows
C <- merge(G[, .(row_id, ref = paste(role, bk(role, board_role), ceo, sep = "|"))], W, by = "row_id")
C[, all4 := rowSums(sapply(models, function(m) same(C[[m]], C$ref)))]
C[, s3 := vote(.SD, ref)]
C[, queue := fcase(s3 == 0, 2L, s3 == 1, 5L, default = NA_integer_)]
Q <- rbind(U[, .(row_id, queue, lean, kind = "undecided")], C[!is.na(queue), .(row_id, queue, lean = "", kind = "consensus")])

# ---- assemble cases ---------------------------------------------------------------------
smp <- fread(file.path(REPO, "sample.csv"), colClasses = "character")
key <- fread(file.path(REPO, "row-key.csv"), colClasses = "character")
labs <- function(id) {
  out <- list()
  a <- A[row_id == id]; b <- B[row_id == id]; g <- G[row_id == id]
  if (nrow(g)) out$reference <- list(label = lab(g$role, g$board_role, g$ceo), source = g$source)
  if (nrow(a)) out$A <- list(label = lab(a$role, a$board_role, a$ceo), note = a$notes)
  if (nrow(b)) out$B <- list(label = lab(b$role, b$board_role, b$ceo), note = b$notes)
  for (m in models) { x <- M[row_id == id & model == m]
    if (nrow(x)) out[[m]] <- list(label = lab(x$role, x$board, x$ceo), note = x$note) }
  s <- S9[row_id == id]
  if (nrow(s)) out$step09 <- list(label = lab(s$s9, ifelse(s$s9 == "BOARD", s$s9_board, ""), s$s9 == "CEO"))
  out
}
qn <- c("1" = "Models split", "2" = "Likely label error", "3" = "Models lean one way",
        "4" = "Models unanimous", "5" = "Weakly confirmed")
cases <- lapply(seq_len(nrow(Q)), function(i) {
  q <- Q[i]; r <- rows[row_id == q$row_id]; k <- key[row_id == q$row_id]
  pname <- toupper(gsub("\\s+", " ", trimws(r$name)))
  other <- rows[org_id == r$org_id & year != r$year & toupper(gsub("\\s+", " ", trimws(name))) == pname,
                .(year, title, pay, hrs, boxes)][order(year)]
  list(row_id = q$row_id, queue = q$queue, queue_name = unname(qn[as.character(q$queue)]), kind = q$kind, lean = q$lean,
       org_id = r$org_id, year = r$year, form = k$form,
       stratum = smp$stratum[match(r$org_id, smp$org_id)],
       person = r$name, title = r$title, pay = r$pay, hrs = r$hrs, boxes = r$boxes,
       labels = labs(q$row_id), other_years = other)
})
fk <- unique(Q[, .(org_id = sub("-.*", "", row_id), year = sub("^O[0-9]+-([0-9]{4})-.*", "\\1", row_id))])
filing_list <- lapply(seq_len(nrow(fk)), function(i) {
  f <- fk[i]; fr <- rows[org_id == f$org_id & year == f$year]
  fr[S9, on = "row_id", s9 := i.s9]
  fr[G, on = "row_id", ref := lab(i.role, i.board_role, i.ceo)]
  list(id = paste(f$org_id, f$year), org_id = f$org_id, year = f$year,
       info = filings[org_id == f$org_id & year == f$year, filing_info][1],
       people = fr[, .(row_id, name, title, pay, rel_pay, hrs, rel_hrs, boxes, s9 = fifelse(is.na(s9), "", s9),
                       ref = fifelse(is.na(ref), "", ref))])
})
names(filing_list) <- vapply(filing_list, `[[`, "", "id")
org_list <- lapply(split(orgs[org_id %in% fk$org_id], by = "org_id"), function(o) list(name = o$org_name, info = o$org_info))
out <- list(built = format(Sys.time(), "%Y-%m-%d"), counts = as.list(table(Q$queue)), queue_names = as.list(qn),
            cases = cases, filings = filing_list, orgs = org_list)
writeLines(toJSON(out, auto_unbox = TRUE, null = "null", na = "string"), file.path(EV, "review-queue.json"), useBytes = TRUE)
cat(sprintf("review queue: %d cases in %d filings\n", length(cases), length(filing_list))); print(table(Q$queue))

# ---- the review page: the template with the queue embedded ---------------------------
tpl <- paste(readLines("data-raw/partvii-validation/review-page/template.html", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
js  <- gsub("</", "<\\/", toJSON(out, auto_unbox = TRUE, null = "null", na = "string"), fixed = TRUE)
page <- file.path(EV, "review-page.html")
writeLines(enc2utf8(sub("/*__DATA__*/", js, tpl, fixed = TRUE)), page, useBytes = TRUE)
cat(sprintf("review page: %s (%.0f KB)\n", page, file.size(page) / 1024))
