# build-crosswalks.R
# Build the package's crosswalk data objects from the CSV tables in this
# folder. Run it after every edit to a table, from the package root:
#
#   Rscript data-raw/crosswalks/build-crosswalks.R
#
# Writes data/status-codes.rda (status.codes), data/title-xwalk.rda
# (title.xwalk) and data/title-taxonomy.rda (title.taxonomy). Only the columns
# the pipeline uses go into the package; notes columns stay in the CSVs.
# tests/testthat/test-crosswalks.R checks that data/ matches these tables.

dir <- "data-raw/crosswalks"
read_table <- function(f, cols) {
  d <- utils::read.csv(file.path(dir, f), colClasses = "character",
                       na.strings = character(0), check.names = FALSE)
  missing <- setdiff(cols, names(d))
  if (length(missing)) stop(f, " is missing columns: ", paste(missing, collapse = ", "))
  d <- d[, cols, drop = FALSE]
  rownames(d) <- NULL
  d
}

status.codes <- read_table("status-codes.csv", c("status.variant", "status.qualifier"))

title.xwalk <- read_table("title-standardization.csv",
                          c("title.variant", "title.standard", "strata", "strata.label"))

title.taxonomy <- read_table("title-taxonomy.csv",
  c("title.standard", "domain.category", "domain.label", "soc.label",
    "major.group", "minor.group", "broad.group", "detailed.occupation",
    "emp.level", "board.role"))

# each title is an employee level or a board role (or neither: not a role)
emp_levels  <- c("CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF")
board_roles <- c("CHAIR", "VICE CHAIR", "SECRETARY", "TREASURER", "MEMBER")
lv <- title.taxonomy$emp.level; rl <- title.taxonomy$board.role
bad <- title.taxonomy$title.standard[!lv %in% c(emp_levels, "") | !rl %in% c(board_roles, "") | (nzchar(lv) & nzchar(rl))]
if (length(bad)) stop("invalid emp.level / board.role in title-taxonomy.csv: ", paste(bad, collapse = ", "))

# the legacy role flags ("X" or blank), derived so that the pipeline's output
# columns keep their meaning. dir.vp is retired and always blank.
flag <- function(cond) ifelse(cond, "X", "")
title.taxonomy <- within(title.taxonomy, {
  emp     <- flag(nzchar(lv))
  ceo     <- flag(lv == "CEO")
  c.level <- flag(lv %in% c("CEO", "OFFICER"))
  dir.vp  <- flag(FALSE)
  mgr     <- flag(lv == "MANAGER")
  spec    <- flag(lv %in% c("PROFESSIONAL", "STAFF"))
  board   <- flag(nzchar(rl))
  pres    <- flag(rl == "CHAIR")
  vp      <- flag(rl == "VICE CHAIR")
  sec     <- flag(rl == "SECRETARY")
  treas   <- flag(rl == "TREASURER")
  mem     <- flag(rl == "MEMBER")
})
title.taxonomy <- title.taxonomy[, c(
  "title.standard", "domain.category", "domain.label", "soc.label",
  "major.group", "minor.group", "broad.group", "detailed.occupation",
  "emp.level", "board.role",
  "emp", "ceo", "c.level", "dir.vp", "mgr", "spec",
  "board", "pres", "vp", "sec", "treas", "mem")]

# categorize_titles() merges on title.standard: a duplicate would return every
# holder of that title twice (FINDINGS.md F-001)
dup <- title.taxonomy$title.standard[duplicated(title.taxonomy$title.standard)]
if (length(dup)) stop("duplicate title.standard in title-taxonomy.csv: ", paste(unique(dup), collapse = ", "))

# head rules for titles the crosswalk does not list (10-pattern-rules.R)
title.patterns <- read_table("title-patterns.csv",
                             c("position", "head", "title.standard", "rule", "support", "precision"))
title.patterns$support   <- as.integer(title.patterns$support)
title.patterns$precision <- as.numeric(title.patterns$precision)
bad <- setdiff(title.patterns$title.standard, title.taxonomy$title.standard)
if (length(bad)) stop("title-patterns.csv targets standards not in the taxonomy: ", paste(bad, collapse = ", "))
if (!all(title.patterns$position %in% c("suffix", "prefix"))) stop("title-patterns.csv: position must be suffix or prefix")

save(status.codes,   file = "data/status-codes.rda",   compress = "xz")
save(title.patterns, file = "data/title-patterns.rda", compress = "xz")
save(title.xwalk,    file = "data/title-xwalk.rda",    compress = "xz")
save(title.taxonomy, file = "data/title-taxonomy.rda", compress = "xz")
message(sprintf("built status.codes (%d rows), title.xwalk (%d), title.taxonomy (%d), title.patterns (%d)",
                nrow(status.codes), nrow(title.xwalk), nrow(title.taxonomy), nrow(title.patterns)))
