# _nobox-features.R
# Features for the no-checkbox role model (FINDINGS.md F-028), shared by
# 16-nobox-model.R (training) and 17-nobox-gold.R (scoring) so both use the
# same definitions. Nothing here reads the Part VII checkboxes.
# Requires data.table.

classes <- c("BOARD", "CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF")

num <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }
kw <- c(president = "PRESIDENT", vice = "\\bVICE\\b|\\bVP\\b", chair = "CHAIR", secretary = "SECRETARY|\\bSEC\\b",
        treasurer = "TREASURER|\\bTREAS\\b", director = "DIRECTOR", trustee = "TRUSTEE", member = "MEMBER",
        board = "BOARD", executive = "EXECUTIVE|\\bEXEC\\b", ceo = "\\bCEO\\b", chief = "CHIEF", officer = "OFFICER",
        manager = "MANAGER|\\bMGR\\b", coordinator = "COORDINAT", administrator = "ADMINISTRAT", founder = "FOUNDER",
        assistant = "ASSISTANT|\\bASST\\b", deputy = "DEPUTY", senior = "SENIOR|\\bSR\\b", governor = "GOVERNOR",
        past = "PAST|FORMER|EMERITUS", ex_officio = "EX.?OFFICIO", at_large = "AT.?LARGE", physician = "PHYSICIAN|\\bMD\\b|DOCTOR|SURGEON",
        nurse = "NURSE|\\bRN\\b", counsel = "COUNSEL|ATTORNEY|LAWYER", professor = "PROFESSOR|FACULTY", coach = "COACH",
        pastor = "PASTOR|RECTOR|REVEREND|\\bREV\\b|CLERGY|MINISTER", principal = "PRINCIPAL|HEADMASTER|HEAD OF SCHOOL",
        dean = "\\bDEAN\\b", program = "PROGRAM", development = "DEVELOPMENT|FUNDRAIS|ADVANCEMENT", finance = "FINANC|CFO|CONTROLLER|COMPTROLLER")
features <- function(x) {
  x <- copy(x)
  t <- toupper(paste(x$title.raw, x$title.standard))
  for (k in names(kw)) set(x, j = paste0("kw_", k), value = as.integer(grepl(kw[[k]], t, perl = TRUE)))
  lv <- function(v, levels) { v[is.na(v) | !v %in% levels] <- "none"; factor(v, c(levels, "none")) }
  x[, `:=`(
    comp = log1p(pmax(num(tot.comp), 0)), hours = pmax(num(tot.hours), 0), paid = as.integer(num(tot.comp) > 0),
    lvl = lv(emp.level, classes[-1]), brd = lv(board.role, c("CHAIR", "VICE CHAIR", "SECRETARY", "TREASURER", "MEMBER")),
    dom = lv(domain.category, c("executive", "governance", "operations", "industry-specific", "non-job title")),
    soc = lv(substr(major.group, 1, 2), c("11", "13", "15", "17", "19", "21", "23", "25", "27", "29", "31", "33", "35", "37", "39", "41", "43", "47", "49", "51", "53")),
    match = lv(title.match, c("exact", "pattern")),
    sz = lv(size, c("$0", "$1-150k", "$150k-1m", "$1m-10m", "$10m-100m", "$100m+", "missing")),
    former = num(former.x), interim = num(interim.x), co = num(co.x), founder = num(founder.x), exoff = num(ex.officio.x),
    n_titles = num(title.count))]
  # place in the filing
  x[, `:=`(n_people = .N, n_paid = sum(paid), pay_rank = frank(-comp, ties.method = "min"),
           hrs_rank = frank(-hours, ties.method = "min"),
           pay_share = comp / max(max(comp), 1e-9), hrs_share = hours / max(max(hours), 1e-9),
           n_ceo_titles = sum(emp.level %in% "CEO"), n_board_titles = sum(nzchar(fcoalesce(board.role, ""))),
           n_exec_paid = sum(emp.level %in% c("CEO", "OFFICER") & paid == 1)), by = object.id]
  x[, `:=`(other_ceo_title = as.integer(n_ceo_titles - (emp.level %in% "CEO") > 0),
           same_title = .N), by = .(object.id, title.standard)]
  x[, `:=`(top_paid = as.integer(pay_rank == 1 & paid == 1), only_paid = as.integer(n_paid == 1 & paid == 1))]
  x
}
mm <- function(x) {
  cols <- c("comp", "hours", "paid", "former", "interim", "co", "founder", "exoff", "n_titles", "n_people", "n_paid",
            "pay_rank", "hrs_rank", "pay_share", "hrs_share", "n_ceo_titles", "n_board_titles", "n_exec_paid",
            "other_ceo_title", "same_title", "top_paid", "only_paid", grep("^kw_", names(x), value = TRUE))
  f <- stats::model.matrix(~ . - 1, data = as.data.frame(x[, c("lvl", "brd", "dom", "soc", "match", "sz")]),
                           contrasts.arg = lapply(x[, c("lvl", "brd", "dom", "soc", "match", "sz")], contrasts, contrasts = FALSE))
  cbind(as.matrix(x[, ..cols]), f)
}
