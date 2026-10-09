# 07-taxonomy-fixes.R
# Correct rows of the title-taxonomy table (data-raw/crosswalks/
# title-taxonomy.csv): role flags that are wrong regardless of context
# (F-012 to F-014) and SOC codes and labels (F-015). See FINDINGS.md.
#
# 06-apply-decisions.R adds crosswalk rows and new taxonomy rows; this script
# edits existing taxonomy cells, then rebuilds the package data. Every changed
# cell is logged in taxonomy-edits-log.csv. Edits check the old value first, so a second run changes nothing.
#
#   Rscript data-raw/partvii-validation/07-taxonomy-fixes.R          (dry run)
#   Rscript data-raw/partvii-validation/07-taxonomy-fixes.R --apply

source("data-raw/crosswalks/_crosswalk-io.R")
apply   <- "--apply" %in% commandArgs(trailingOnly = TRUE)
pv_repo <- "data-raw/partvii-validation"
soc_path <- "data-raw/standard-occupational-classifications/tidy-soc-codes.csv"

tx <- read_xwalk("title-taxonomy")
# the table no longer has the role flags this script edits (F-020); its fixes
# are in the table and in taxonomy-edits-log.csv
if ("emp.level" %in% names(tx)) { message("already applied; title-taxonomy.csv now uses emp.level and board.role"); quit(save = "no") }
codes <-c("major.group", "minor.group", "broad.group", "detailed.occupation")
log <- list()

# set one cell, recording the change; `from` guards against editing a row
# that is no longer what the finding describes
set_cell <- function(std, col, to, finding, from = NULL) {
  i <- which(tx$title.standard == std)
  if (length(i) != 1) stop("expected one taxonomy row for ", std, ", found ", length(i))
  old <- tx[[col]][i]
  if (identical(old, to)) return(invisible())
  if (!is.null(from) && !identical(old, from)) stop(std, " / ", col, ": expected '", from, "', found '", old, "'")
  set(tx, i, col, to)
  log[[length(log) + 1]] <<- data.table(title.standard = std, column = col, old = old, new = to, finding = finding)
}
set_flags <- function(std, on = character(), off = character(), finding) {
  for (f in on)  set_cell(std, f, "X", finding)
  for (f in off) set_cell(std, f, "",  finding)
}

# ---- F-012 CEO flag --------------------------------------------------------
# The top staff job: the instructions tab maps ED to ceo. EXECUTIVE DIRECTOR
# alone outnumbers CEO in the 2010-12 slice (673 vs 378 rows).
for (s in c("EXECUTIVE DIRECTOR", "GENERAL DIRECTOR")) set_flags(s, on = c("ceo", "c.level"), finding = "F-012")
# Deputies and seconds of the CEO are officers, not the CEO.
for (s in c("ASSISTANT CEO", "ASSOCIATE CEO", "DEPUTY CEO", "VICE CEO")) set_flags(s, on = "c.level", off = "ceo", finding = "F-012")
# Head of a hospital's medical staff: a physician, not the organization's CEO.
set_flags("PRESIDENT OF MEDICAL STAFF", on = "spec", off = "ceo", finding = "F-012")
# A department head, and an employee (emp was blank).
set_flags("POLITICAL DIRECTOR", on = c("emp", "dir.vp"), off = "ceo", finding = "F-012")

# ---- F-013 support and editorial titles filed as managers ------------------
for (s in c("EXECUTIVE ASSISTANT", "EXECUTIVE SECRETARY", "EDITOR", "ORGANIZER"))
  set_flags(s, on = "spec", off = "mgr", finding = "F-013")

# ---- F-014 employee levels without the emp flag ----------------------------
for (s in tx[emp == "" & (ceo == "X" | c.level == "X" | dir.vp == "X" | mgr == "X" | spec == "X"), title.standard])
  set_flags(s, on = "emp", finding = "F-014")

# ---- F-015 SOC codes and labels -------------------------------------------
for (col in codes) for (i in which(tx[[col]] != trimws(tx[[col]]))) set_cell(tx$title.standard[i], col, trimws(tx[[col]][i]), "F-015")
# placeholders ("board", "xxx", "missing") are not occupations
for (col in c("soc.label", codes)) for (i in which(tolower(tx[[col]]) %in% c("board", "xxx", "missing"))) set_cell(tx$title.standard[i], col, "", "F-015")
# 11-9051 is Food Service Managers; these rows mean Social and Community Service Managers
for (s in tx[detailed.occupation == "11-9051" & grepl("social and community", soc.label, ignore.case = TRUE), title.standard]) {
  set_cell(s, "broad.group", "11-9150", "F-015", from = "11-9050")
  set_cell(s, "detailed.occupation", "11-9151", "F-015", from = "11-9051")
}
# ARTIST: broad 27-1010 with a detailed code from another broad group; keep the broad group
set_cell("ARTIST", "detailed.occupation", "", "F-015", from = "27-2013")
# sports titles were all coded 27-2023 Umpires, Referees, and Other Sports Officials
set_cell("ATHLETE", "detailed.occupation", "27-2021", "F-015", from = "27-2023")
set_cell("SPORTS DIRECTOR", "detailed.occupation", "27-2022", "F-015", from = "27-2023")
for (s in c("SPORTS", "VICE PRESIDENT OF SOFTBALL")) set_cell(s, "detailed.occupation", "", "F-015", from = "27-2023")

# soc.label: the official 2018 title of the most detailed code given
soc <- fread(soc_path, encoding = "Latin-1")
strip <- function(x) sub("^SOC-", "", x)
official <- unique(rbind(
  soc[, .(code = substr(strip(MajorGroup), 1, 2), label = LEV1)],
  soc[, .(code = strip(MinorGroup),  label = LEV2)],
  soc[, .(code = strip(BroadGroup),  label = LEV3)],
  soc[, .(code = strip(DetailedOccupation), label = LEV4)]))
official <- official[!duplicated(code)]
deepest <- function(r) { for (col in rev(codes)) if (nzchar(r[[col]])) return(r[[col]]); "" }
for (i in seq_len(nrow(tx))) {
  code <- deepest(tx[i])
  if (!nzchar(code)) next
  lab <- official$label[match(code, official$code)]
  if (is.na(lab)) stop(tx$title.standard[i], ": SOC code ", code, " is not in ", soc_path)
  set_cell(tx$title.standard[i], "soc.label", lab, "F-015")
}

# ---- write -----------------------------------------------------------------
log <- rbindlist(log)
if (!nrow(log)) { message("nothing to change; the table already has these fixes"); quit(save = "no") }
message(sprintf("%d cells to change in %d rows: %s", nrow(log), uniqueN(log$title.standard),
                paste(names(table(log$finding)), table(log$finding), sep = " ", collapse = ", ")))
if (!apply) { print(log[finding != "F-015" | column != "soc.label"]); message("dry run; rerun with --apply to write the table"); quit(save = "no") }

write_xwalk(tx, "title-taxonomy")
rebuild_xwalks()
log[, applied := as.character(Sys.Date())]
lp <- file.path(pv_repo, "taxonomy-edits-log.csv")
fwrite(if (file.exists(lp)) rbind(fread(lp, colClasses = "character"), log) else log, lp)
message("applied and rebuilt data/")
