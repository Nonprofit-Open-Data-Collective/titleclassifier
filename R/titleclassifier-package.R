#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
#' @importFrom utils capture.output head write.csv read.csv download.file
#' @importFrom datasets state.abb
#' @importFrom dplyr group_by ungroup mutate arrange filter rename bind_rows
#'   group_split sample_n n desc dense_rank
NULL

# Column names referenced inside dplyr non-standard-evaluation verbs and bare
# data-object names read from the package's lazy-loaded datasets. Declaring them
# here quiets the "no visible binding for global variable" R CMD check NOTE
# without changing behavior.
utils::globalVariables(c(
  # engineered / IRS column names
  "AT.LARGE.X", "CO.X", "DATE.X", "EIN2", "EXOFFICIO.X",
  "F9_07_COMP_DTK_AVE_HOUR_WEEK", "F9_07_COMP_DTK_AVE_HOUR_WEEK_RL",
  "F9_07_COMP_DTK_COMP_ORG", "F9_07_COMP_DTK_COMP_OTH", "F9_07_COMP_DTK_COMP_RLTD",
  "F9_07_COMP_DTK_EMPL_BEN", "F9_07_COMP_DTK_POS_FORMER_X",
  "F9_07_COMP_DTK_POS_HIGH_COMP_X", "F9_07_COMP_DTK_POS_INDIV_TRUST_X",
  "F9_07_COMP_DTK_POS_INST_TRUST_X", "F9_07_COMP_DTK_POS_KEY_EMPL_X",
  "F9_07_COMP_DTK_POS_OFF_X", "F9_07_COMP_DTK_TITLE",
  "FORMER.X", "FOUNDER.X", "FUTURE.X", "INTERIM.X", "Multiple.Titles", "NAME",
  "Num.Titles", "OBJECTID", "ORGNAME", "OUTGOING.X", "PARTIAL.X", "PERSONID",
  "REGIONAL.X", "RETURN_TYPE", "SCHED.O.X", "TAX_YEAR", "TITLE_RAW",
  "TOT.COMP", "TOT.COMP.TOT", "TOT.HOURS", "TOT.HOURS.TOT",
  "TitleTxt2", "TitleTxt3", "TitleTxt4", "TitleTxt5", "TitleTxt6", "TitleTxt7",
  "URL", "board", "board2", "c.level", "ceo", "dir.vp", "emp", "emp2",
  "hours.pct.of.max.incl.rltd", "mem", "mgr", "object.id", "pay.max",
  "pay.max.incl.rltd", "pay.pct.of.max.incl.rltd", "pay.tot", "pay.tot.incl.rltd",
  "person.id", "pres", "sec", "spec", "title.order", "title.variant",
  "tot.comp2", "tot.comp2.tot", "tot.hours2", "tot.titles", "treas", "variant", "vp",
  # lazy-loaded package data objects used by name
  "date.words", "number.words", "likely.subjects", "likely.titles",
  "possible.titles", "tinypartvii"
))
