# Consistency checks on the crosswalks (package data built from
# data-raw/crosswalks/). Findings referenced below are in
# data-raw/partvii-validation/FINDINGS.md.

tx <- get_title_taxonomy()
xw <- get_title_xwalk()

test_that("data/ matches the tables in data-raw/crosswalks/ (run build-crosswalks.R)", {
  dir <- test_path("..", "..", "data-raw", "crosswalks")
  skip_if_not(dir.exists(dir))
  rd <- function(f) utils::read.csv(file.path(dir, f), colClasses = "character",
                                    na.strings = character(0), check.names = FALSE)
  # the columns the table and the package data share (the data adds derived
  # role flags; the table keeps notes)
  same <- function(obj, f) {
    csv  <- rd(f)
    cols <- intersect(names(obj), names(csv))
    expect_identical(unname(as.list(obj[cols])), unname(as.list(csv[cols])), info = f)
  }
  same(get_status_codes(), "status-codes.csv")
  same(xw, "title-standardization.csv")
  same(tx, "title-taxonomy.csv")
})

emp_levels   <- c("ceo", "c.level", "dir.vp", "mgr", "spec")
board_levels <- c("pres", "vp", "sec", "treas", "mem")
soc_cols     <- c("major.group", "minor.group", "broad.group", "detailed.occupation")
is_x <- function(col) tx[[col]] == "X"

test_that("each title.standard has one taxonomy row (F-001)", {
  expect_equal(sum(duplicated(tx$title.standard)), 0)
})

test_that("every standard in the standardization crosswalk has a taxonomy row (F-002)", {
  expect_equal(setdiff(unique(xw$title.standard), tx$title.standard), character(0))
})

test_that("role flags are X or blank and levels imply their parent flag (F-014)", {
  for (col in c("emp", "board", emp_levels, board_levels))
    expect_true(all(tx[[col]] %in% c("", "X")), info = col)
  emp_level   <- Reduce(`|`, lapply(emp_levels, is_x))
  board_level <- Reduce(`|`, lapply(board_levels, is_x))
  expect_equal(tx$title.standard[emp_level & !is_x("emp")], character(0))
  expect_equal(tx$title.standard[board_level & !is_x("board")], character(0))
  expect_equal(tx$title.standard[is_x("emp") & is_x("board")], character(0))
})

test_that("each title has one employee level or one board role (F-020)", {
  expect_true(all(tx$emp.level %in% c("", "CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF")))
  expect_true(all(tx$board.role %in% c("", "CHAIR", "VICE CHAIR", "SECRETARY", "TREASURER", "MEMBER")))
  expect_equal(tx$title.standard[nzchar(tx$emp.level) & nzchar(tx$board.role)], character(0))
  # the legacy flags follow the two columns; dir.vp is retired
  expect_identical(is_x("emp"), nzchar(tx$emp.level))
  expect_identical(is_x("board"), nzchar(tx$board.role))
  expect_false(any(is_x("dir.vp")))
})

test_that("board titles use the five core board standards (F-016)", {
  core <- c("BOARD PRESIDENT", "BOARD VICE PRESIDENT", "BOARD SECRETARY", "BOARD TREASURER", "BOARD MEMBER")
  expect_setequal(tx$title.standard[nzchar(tx$board.role)], core)
})

test_that("only top-executive titles carry the ceo flag (F-012)", {
  ceo <- tx$title.standard[is_x("ceo")]
  expect_true(all(c("CEO", "EXECUTIVE DIRECTOR") %in% ceo))
  expect_false(any(grepl("^(ASSISTANT|ASSOCIATE|DEPUTY|VICE) CEO$", ceo)))
  expect_identical(ceo, tx$title.standard[tx$emp.level == "CEO"])
})

test_that("SOC codes are valid 2018 codes with a consistent hierarchy (F-015)", {
  soc_file <- test_path("..", "..", "data-raw", "standard-occupational-classifications", "tidy-soc-codes.csv")
  skip_if_not(file.exists(soc_file))
  soc <- utils::read.csv(soc_file, colClasses = "character", fileEncoding = "latin1")
  strip <- function(x) sub("^SOC-", "", x)
  valid <- list(major.group = unique(substr(strip(soc$MajorGroup), 1, 2)),
                minor.group = strip(soc$MinorGroup),
                broad.group = strip(soc$BroadGroup),
                detailed.occupation = strip(soc$DetailedOccupation))
  for (col in soc_cols) {
    x <- tx[[col]]
    bad <- tx$title.standard[nzchar(x) & !x %in% valid[[col]]]
    expect_equal(bad, character(0), info = col)
  }
  # each code starts with its parent's prefix (11 > 11-3000 > 11-3010 > 11-3012)
  nested <- function(child, parent, n) {
    a <- tx[[child]]; b <- tx[[parent]]
    tx$title.standard[nzchar(a) & nzchar(b) & substr(a, 1, n) != substr(b, 1, n)]
  }
  expect_equal(nested("minor.group", "major.group", 2), character(0))
  expect_equal(nested("broad.group", "minor.group", 4), character(0))
  expect_equal(nested("detailed.occupation", "broad.group", 6), character(0))
})
