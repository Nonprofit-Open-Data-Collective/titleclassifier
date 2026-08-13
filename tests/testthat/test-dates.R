# Step 02 - date detection and removal.

test_that("has_date flags date-bearing titles only", {
  expect_true (has_date("TREASURER (ENDED 3/16/23)"))
  expect_true (has_date("DIRECTOR AS OF OCT. 2023"))
  expect_false(has_date("PRESIDENT"))
  expect_false(has_date("VICE PRESIDENT OF FINANCE"))
})

test_that("has_date is vectorized", {
  expect_equal(has_date(c("PRESIDENT", "TRUSTEE (RESIGNED 1/24/23)")),
               c(FALSE, TRUE))
})

test_that("convert_ordinal spells out numeric ordinals", {
  expect_match(convert_ordinal("1ST VICE PRESIDENT"), "FIRST")
  expect_match(convert_ordinal("2ND LIEUTENANT GOVERNOR"), "SECOND")
})

test_that("convert_ordinal does not rewrite multi-digit ordinals (L1)", {
  expect_equal(convert_ordinal("21ST"), "21ST")
  expect_equal(convert_ordinal("1ST VICE PRESIDENT"), "FIRST VICE PRESIDENT")
})

test_that("a parenthesis glued to a word does not merge tokens (L2)", {
  expect_equal(remove_date("CFO(INTERIM)"), "CFO INTERIM")
})

test_that("remove_date strips the date fragment but keeps the role", {
  out <- remove_date("TREASURER (ENDED 3/16/23)")
  expect_match(out, "TREASURER")
  expect_false(grepl("3/16/23", out))
})

test_that("region-code substitution only hits standalone codes (M1)", {
  expect_true (grepl("REGION", remove_date("R-1")))
  # must NOT rewrite the trailing R of a word
  expect_false(grepl("REGION", remove_date("DIRECTOR-1")))
  expect_false(grepl("REGION", remove_date("MGR-2")))
})
