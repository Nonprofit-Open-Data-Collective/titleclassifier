test_that("title_heads finds the longest proper suffix and prefix heads", {
  v <- c("COMMITTEE MEMBER", "MEMBER", "BOARD MEMBER", "DIRECTOR OF")
  h <- title_heads(c("FINANCE COMMITTEE MEMBER", "BOARD MEMBER EMERITUS", "MEMBER", NA), v)
  expect_equal(h$suffix, c("COMMITTEE MEMBER", NA, NA, NA))
  expect_equal(h$prefix, c(NA, "BOARD MEMBER", NA, NA))
})

test_that("match_title_patterns uses the longest head only and prefers the more precise rule", {
  v <- c("COMMITTEE MEMBER", "MEMBER", "CHAIR", "DIRECTOR", "CFO")
  p <- data.frame(position = c("suffix", "suffix", "prefix"),
                  head = c("COMMITTEE MEMBER", "CHAIR", "CFO"),
                  title.standard = c("BOARD MEMBER", "BOARD MEMBER", "CHIEF FINANCIAL OFFICER"),
                  rule = "majority", support = 10L, precision = c(0.95, 0.9, 0.99),
                  stringsAsFactors = FALSE)
  got <- match_title_patterns(c("FINANCE COMMITTEE MEMBER", "GOLF CHAIR", "CFO AND COO",
                                "PROGRAM DIRECTOR", "MEMBER", "CFO CHAIR"), p, v)
  expect_equal(got, c("BOARD MEMBER", "BOARD MEMBER", "CHIEF FINANCIAL OFFICER",
                      NA, NA, "CHIEF FINANCIAL OFFICER"))
})

test_that("standardize_titles marks exact and pattern matches and leaves the rest NA", {
  xw <- data.frame(title.variant = c("TREASURER", "COMMITTEE MEMBER"),
                   title.standard = c("BOARD TREASURER", "BOARD MEMBER"),
                   strata = "", strata.label = "", stringsAsFactors = FALSE)
  p <- data.frame(position = "suffix", head = "COMMITTEE MEMBER", title.standard = "BOARD MEMBER",
                  rule = "majority", support = 10L, precision = 0.95, stringsAsFactors = FALSE)
  d <- data.frame(TitleTxt6 = c("TREASURER", "GOLF COMMITTEE MEMBER", "ZZZ"), TitleTxt3 = "",
                  TOT.HOURS = 0, TOT.COMP = 0, F9_07_COMP_DTK_POS_OFF_X = 0, stringsAsFactors = FALSE)
  invisible(capture.output(out <- standardize_titles(d, gs_title_xwalk = xw, title_patterns = p)))
  out <- out[order(out$TitleTxt7), ]
  expect_equal(out$title.match, c("pattern", "exact", NA))
  expect_equal(out$title.standard, c("BOARD MEMBER", "BOARD TREASURER", NA))
})
