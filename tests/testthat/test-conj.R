# Step 03 - conjunction / separator standardization.
# These pin the documented behavior: a real separator becomes "&", a compound
# subject is left intact.

test_that("standardize_and converts a separator but not a compound", {
  expect_equal(standardize_and("CEO AND BOARD PRESIDENT"), "CEO & BOARD PRESIDENT")
  expect_equal(standardize_and("VP OF FINANCE AND ADMINISTRATION"),
               "VP OF FINANCE AND ADMINISTRATION")
})

test_that("standardize_comma treats a comma as a separator", {
  expect_match(standardize_comma("SECRETARY, TREASURER"), "SECRETARY & TREASURER")
})

test_that("standardize_slash treats a slash as a separator", {
  expect_match(standardize_slash("SECRETARY/TREASURER"), "SECRETARY & TREASURER")
})

test_that("conjunction helpers are vectorized and length-preserving", {
  x <- c("CEO AND BOARD PRESIDENT", "SECRETARY, TREASURER", "DIRECTOR")
  expect_length(standardize_and(x), 3)
  expect_length(standardize_comma(x), 3)
})

test_that("standardize_of converts non-terminal FOR, keeps a trailing FOR (L4)", {
  expect_equal(standardize_of("VP FOR FINANCE"), "VP OF FINANCE")
  expect_equal(standardize_of("DIRECTOR FOR SALES FOR"), "DIRECTOR OF SALES FOR")
  expect_equal(standardize_of("SOMETHING FOR"), "SOMETHING FOR")
})

test_that("to_helper replaces TO only inside parentheses (L5)", {
  expect_equal(to_helper("MANAGER (TO 2020) TO BOARD"), "MANAGER (UNTIL 2020) TO BOARD")
})
