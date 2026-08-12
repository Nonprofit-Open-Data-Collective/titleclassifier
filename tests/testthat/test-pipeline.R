# End-to-end pipeline on the bundled demo data (offline: crosswalks are bundled).

test_that("the full pipeline runs and returns analysis-ready rows", {
  skip_if_not(exists("tinypartvii"))
  set.seed(1234)
  d <- dplyr::sample_n(tinypartvii, 150)

  out <- d |>
    standardize_df() |>
    remove_dates() |>
    standardize_conj() |>
    split_titles() |>
    standardize_spelling() |>
    gen_status_codes() |>
    standardize_titles() |>
    categorize_titles()

  expect_s3_class(out, "data.frame")
  expect_gt(nrow(out), 0)
  # splitting multi-title rows can only add rows
  expect_gte(nrow(out), 150)

  # key output columns are present
  for (col in c("title.raw", "title.standard", "title.v7",
                "tot.comp", "tot.hours", "object.id", "person.id"))
    expect_true(col %in% names(out), info = col)

  # status flags are 0/1 numeric
  expect_true(all(out$former.x %in% c(0, 1)))
})

test_that("standardize_df backfills missing checkbox columns", {
  d <- data.frame(
    OBJECTID = "OID-1", EIN2 = "EIN-1", RETURN_TYPE = "990", TAX_YEAR = "2023",
    URL = "u", ORG_NAME_L1 = "ORG", ORG_NAME_L2 = "",
    F9_07_COMP_DTK_NAME_PERS = "Jane Doe",
    F9_07_COMP_DTK_TITLE = "PRESIDENT",
    F9_07_COMP_DTK_NAME_ORG_L1 = "", F9_07_COMP_DTK_NAME_ORG_L2 = "",
    stringsAsFactors = FALSE
  )
  out <- standardize_df(d)
  expect_true("TOT.COMP" %in% names(out))
  expect_true("TOT.HOURS" %in% names(out))
  expect_equal(out$TITLE_RAW, "PRESIDENT")
})

test_that("replace_cfo promotes finance officers using the numeric officer flag (H1)", {
  # officer flag is numeric 1 after standardize_df (NA on 990EZ)
  expect_equal(replace_cfo("ACCOUNTANT", 40, 50000, 1), "CFO")
  expect_equal(replace_cfo("DIRECTOR OF FINANCE", 40, 50000, 1), "CFO")
  expect_equal(replace_cfo("FINANCE OFFICER", 41, 50000, 1), "CFO")
  # non-officers and unknown (NA) officer status are left unchanged (not NA)
  expect_equal(replace_cfo("ACCOUNTANT", 40, 50000, 0), "ACCOUNTANT")
  expect_equal(replace_cfo("ACCOUNTANT", 40, 50000, NA), "ACCOUNTANT")
})

test_that("bundled crosswalks load offline", {
  expect_s3_class(get_googlesheets_status_codes(), "data.frame")
  expect_true(all(c("title.variant", "title.standard") %in%
                    names(get_googlesheets_title_xwalk())))
})
