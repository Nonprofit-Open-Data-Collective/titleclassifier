# Fetch helpers (URL construction only; no network access in tests).

test_that("table file names follow the NCCS convention", {
  expect_equal(titleclassifier:::.table_filename(2023),
               "F9-P07-T01-COMPENSATION-2023.CSV")
  expect_equal(titleclassifier:::.table_filename(2020),
               "F9-P07-T01-COMPENSATION-2020.CSV")
})

test_that("table URLs are built from the default NCCS root", {
  u <- titleclassifier:::.table_url(2023)
  expect_true(startsWith(u, "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_0/"))
  expect_true(endsWith(u, "F9-P07-T01-COMPENSATION-2023.CSV"))
})

test_that("a custom root is honored", {
  u <- titleclassifier:::.table_url(2021, root = "https://example.org/data/")
  expect_equal(u, "https://example.org/data/F9-P07-T01-COMPENSATION-2021.CSV")
})

test_that("read_partvii errors clearly when given no files", {
  expect_error(read_partvii(character(0)), "no files")
})
