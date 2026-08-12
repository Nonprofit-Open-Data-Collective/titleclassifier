# Step 04 - multi-title splitting helpers.

test_that("identify_split_num counts titles separated by &", {
  expect_equal(identify_split_num("CFO & TREASURER & DIRECTOR"), 3)
  expect_equal(identify_split_num("CFO & TREASURER"), 2)
  expect_equal(identify_split_num("CFO"), 1)
})

test_that("identify_split_num only splits a whole-string secretary-treasurer (H2)", {
  expect_equal(identify_split_num("SECRETARY TREASURER"), 2)
  expect_equal(identify_split_num("SEC-TREAS"), 2)
  # must NOT misfire on unrelated SEC-/TREAS- titles
  expect_equal(identify_split_num("SECURITY DIRECTOR"), 1)
  expect_equal(identify_split_num("ASSISTANT TREASURER"), 1)
})

test_that("remove_first_split peels the first title", {
  expect_match(remove_first_split("CFO & TREASURER & DIRECTOR"), "TREASURER & DIRECTOR")
})

test_that("the FOUNDER split rule does not inject an empty title (M4)", {
  # a leading FOUNDER must not produce a leading "&" (which would split into "")
  expect_equal(apply_misc_split_rules("FOUNDER"), "FOUNDER")
  expect_false(grepl("^\\s*&", apply_misc_split_rules("FOUNDER")))
  expect_equal(apply_misc_split_rules("CEO FOUNDER"), "CEO & FOUNDER")
  # no empty fragments after splitting on "&"
  parts <- trimws(strsplit(apply_misc_split_rules("FOUNDER & CEO"), "&")[[1]])
  expect_false(any(parts == ""))
})

test_that("split_titles expands one multi-title row into several", {
  df <- data.frame(
    OBJECTID = "OID-1", EIN2 = "EIN-1", RETURN_TYPE = "990", TAX_YEAR = "2023",
    URL = "u", ORG_NAME_L1 = "ORG", ORG_NAME_L2 = "",
    F9_07_COMP_DTK_NAME_PERS = "Jane Doe",
    F9_07_COMP_DTK_TITLE = "SECRETARY & TREASURER",
    F9_07_COMP_DTK_NAME_ORG_L1 = "", F9_07_COMP_DTK_NAME_ORG_L2 = "",
    stringsAsFactors = FALSE
  )
  out <- df |>
    standardize_df() |>
    remove_dates() |>
    standardize_conj() |>
    split_titles()
  expect_gte(nrow(out), 2)
})
