# Step 05 - spelling / abbreviation normalization.

test_that("common abbreviations expand to canonical forms", {
  expect_equal(fix_spelling("VP"),    "VICE PRESIDENT")
  expect_equal(fix_spelling("SECY"),  "SECRETARY")
  expect_equal(fix_spelling("TRTEE"), "TRUSTEE")
})

test_that("unambiguous C-suite titles condense to abbreviations", {
  expect_equal(fix_spelling("CHIEF FINANCIAL OFFICER"), "CFO")
  expect_equal(fix_spelling("CHIEF EXEC OFFICER"),      "CEO")
})

test_that("spaced C-suite abbreviations condense, not clobbered by the chair rule (H3)", {
  expect_equal(fix_spelling("C E O"), "CEO")
  expect_equal(fix_spelling("C F O"), "CFO")
  expect_equal(fix_spelling("C O O"), "COO")
  # a genuinely standalone C still becomes CHAIR
  expect_equal(fix_chair("C"), "CHAIR")
  expect_equal(fix_chair("VICE C"), "VICE CHAIR")
})

test_that("executive-director abbreviations expand (ED -> CEO is a later crosswalk step)", {
  # NOTE: the README claims fix_spelling('EX DIR') == 'CEO', but the EXECUTIVE
  # DIRECTOR -> CEO mapping now happens in the crosswalk (step 07), not here.
  expect_equal(fix_spelling("EX DIR"), "EXECUTIVE DIRECTOR")
  expect_equal(fix_spelling("EXEC DIR"), "EXECUTIVE DIRECTOR")
})

test_that("an implicit 'of' is inserted before a recognized subject", {
  expect_equal(fix_spelling("VICE PRESIDENT FINANCE"), "VICE PRESIDENT OF FINANCE")
  # subjects not in the recognized list are left as-is (e.g. FACILITIES)
  expect_equal(fix_spelling("DIRECTOR FACILITIES"), "DIRECTOR FACILITIES")
})

test_that("fix_vice normalizes vice-president variants", {
  out <- fix_vice(c("EVP", "SVP"))
  expect_length(out, 2)
  expect_true(all(grepl("VICE PRESIDENT", out)))
})

test_that("VICE PRINCIPAL is not collapsed to VICE PRESIDENT (M5)", {
  expect_equal(fix_spelling("VICE PRINCIPAL"), "VICE PRINCIPAL")
  expect_equal(fix_spelling("VICE PRES"), "VICE PRESIDENT")
})

test_that("fix_governor maps GOV to GOVERNOR but leaves governance/government (M3)", {
  expect_equal(fix_spelling("GOV"), "GOVERNOR")
  expect_match(fix_spelling("GOVERNANCE COMMITTEE"), "GOVERNANCE")
  expect_match(fix_spelling("GOVERNMENT AFFAIRS DIRECTOR"), "GOVERNMENT")
})
