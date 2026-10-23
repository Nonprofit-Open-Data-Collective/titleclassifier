# Step 06: status words (crosswalk review, F-030).

status_of <- function(titles) {
  df <- data.frame(PERSONID = seq_along(titles), TitleTxt5 = titles, DATE.X = 0,
                   stringsAsFactors = FALSE)
  utils::capture.output(out <- gen_status_codes(df, gs_status_codes = get_status_codes()))
  as.data.frame(out)
}

test_that("a trailing NEW marks a new term, not a former one", {
  s <- status_of(c("TRUSTEE NEW", "NEW TRUSTEE"))
  expect_equal(s$TitleTxt6, c("TRUSTEE", "TRUSTEE"))
  expect_true(all(s$FUTURE.X == 1))
  expect_true(all(s$FORMER.X == 0))
})

test_that("NEW inside a name is kept and sets no flag", {
  s <- status_of(c("NEW MEMBER COORDINATOR", "DIRECTOR NEW YORK"))
  expect_equal(s$TitleTxt6[1], "NEW MEMBER COORDINATOR")
  expect_equal(s$TitleTxt6[2], "DIRECTOR REGIONAL")
  expect_true(all(s$FUTURE.X == 0))
})

test_that("PRESENT marks a current term: removed, with no interim flag", {
  s <- status_of("BOARD MEMBER PRESENT")
  expect_equal(s$TitleTxt6, "BOARD MEMBER")
  expect_equal(s$INTERIM.X, 0)
})
