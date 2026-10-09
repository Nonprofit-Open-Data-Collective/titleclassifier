# Run the classifier one unique raw title at a time.
#
# Steps 1-6 of the pipeline (standardize_df .. gen_status_codes) depend only on
# the title text: status flags are maxed within PERSONID, which is one input
# row, i.e. one raw title. Step 7 (basic_csuite_fixes) also reads the officer
# checkbox, whether total pay > 0 and whether weekly hours > 40, so it runs on
# (raw title x those three inputs). The crosswalk merge comes after it.
# tests: 00-check-equivalence.R compares this with the full pipeline.

# steps 1-6 for a vector of distinct raw titles; returns one row per split title
steps_1_6 <- function(raw, gs_status_codes) {
  df <- data.frame(RID = seq_along(raw), EIN2 = "EIN", OBJECTID = "OID", RETURN_TYPE = "990",
                   F9_07_COMP_DTK_TITLE = raw, F9_07_COMP_DTK_NAME_PERS = "", F9_07_COMP_DTK_NAME_ORG_L1 = "",
                   F9_07_COMP_DTK_NAME_ORG_L2 = "", stringsAsFactors = FALSE)
  utils::capture.output({
    d <- df |>
      standardize_df() |>
      remove_dates() |>
      standardize_conj() |>
      split_titles() |>
      standardize_spelling() |>
      gen_status_codes(gs_status_codes = gs_status_codes)
  })
  d <- as.data.frame(d)
  keep <- c("RID", "TITLE_RAW", "TitleTxt2", "TitleTxt3", "TitleTxt4", "TitleTxt5", "TitleTxt6", "Num.Titles",
            grep("\\.X$", names(d), value = TRUE), "PARTIAL")
  d <- d[, intersect(keep, names(d))]
  d$TITLE_RAW <- raw[d$RID]   # the input string itself, before standardize_df's clean-up
  d
}

# step 7 + crosswalk for rows of (step 1-6 output x step-7 inputs)
step_7 <- function(d, gs_title_xwalk) {
  d$TOT.HOURS <- ifelse(d$HRS_GT40, 41, 0)
  d$TOT.COMP <- ifelse(d$PAY_GT0, 1, 0)
  d$F9_07_COMP_DTK_POS_OFF_X <- d$OFFICER_X
  utils::capture.output(d <- basic_csuite_fixes(d))
  d$TOT.HOURS <- d$TOT.COMP <- d$F9_07_COMP_DTK_POS_OFF_X <- NULL
  x <- gs_title_xwalk[!duplicated(gs_title_xwalk$title.variant), ]
  i <- match(d$TitleTxt7, x$title.variant)
  d$title.standard <- x$title.standard[i]
  d$strata <- x$strata[i]
  d$strata.label <- x$strata.label[i]
  d
}
