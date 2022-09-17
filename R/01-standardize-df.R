#Step 1: Standardize Data Frame
#01-standardize-df.R
require(dplyr) #should we port this to somewhere else?


#' @title
#' Standardize the compensation table data frame (from 990 form, Part VII, Table 01)
#'
#' @description
#' `standardize_df` returns a processed data frame with renamed column titles
#' and cleaned up fields from the raw table extracted from PVII T01 build. Also,
#' this function removes duplicated lines if present. Both the input and output
#' are data frames. Ultimately this function provides takes in raw data and
#' returns a data frame with understandable information about DTK individuals.
#'
#' @export
standardize_df <- function( comp.data, ORG_NAME = "NAME", PersonNm = "F9_07_COMP_DTK_NAME_PERS" )
{

  d2 <-
    comp.data %>%
    rename(
      FilingId = OBJECT_ID, #numeric
      FilerEIN = EIN, #numeric
      FilerName = all_of(ORG_NAME),  #string (depends on the output from build_rdb_table)
      FormYr = TAXYR, #numeric
      FORMTYPE  = FORMTYPE, #990,990ez (no 990PF's for comp table)
      URL = URL, #string
      # PersonNm = all_of(name),
      # PersonNm = NAME.x, #string #if working off of jesse's
      TitleTxt   = F9_07_COMP_DTK_TITL, #string
      AvgHrs     = F9_07_COMP_DTK_AVE_HOUR_WEEK, #numeric
      TrustOrDir = F9_07_COMP_DTK_POS_INDIV_TRUST_X, #X or NA (should also consider institutional trustee)
      Officer    = F9_07_COMP_DTK_POS_OFF_X,  #X or NA
      RptCmpOrg  = F9_07_COMP_DTK_COMP_ORG,   #numeric
      RptCmpRltd = F9_07_COMP_DTK_COMP_RLTD,  #numeric
      OtherComp  = F9_07_COMP_DTK_COMP_OTH,  #numeric
      KeyEmpl    = F9_07_COMP_DTK_POS_KEY_EMPL_X, #X or NA
      HighComp   = F9_07_COMP_DTK_POS_HIGH_COMP_X, #X or NA (should only be 1)
      FmrOfficer = F9_07_COMP_DTK_POS_FORMER_X)   #X or NA (sparse)
  
  #converting all compensation fields to numeric if not already
  d2$RptCmpOrg  <- as.numeric( d2$RptCmpOrg )
  d2$RptCmpRltd <- as.numeric( d2$RptCmpRltd )
  d2$OtherComp  <- as.numeric( d2$OtherComp )
  
  #if compensation field is NA, translate that to 0
  d2$RptCmpOrg[  is.na(d2$RptCmpOrg)  ]  <- 0
  d2$RptCmpRltd[ is.na(d2$RptCmpRltd) ]  <- 0
  d2$OtherComp[  is.na(d2$OtherComp)  ]  <- 0
  
  #sum up all compensations for total comp column
  d2$TotalComp <- d2$RptCmpOrg + d2$RptCmpRltd + d2$OtherComp
  
  #converting average hours worked per week field to numeric if not already
  d2$AvgHrs <- as.numeric( d2$AvgHrs )
  d2$AvgHrs[ is.na(d2$AvgHrs) ]   <- 0
  
  #converting missing titles to an empty string
  d2$TitleTxt[ is.na(d2$TitleTxt) ] <- ""
  
  #pre_clean call
  d2$TitleTxt <- pre_clean(d2$TitleTxt)
  # d2$PersonNm <- pre_clean( d2$PersonNm )
  
  #converting empty checkboxes for title classification to empty string
  d2$TrustOrDir[ is.na(d2$TrustOrDir) ] <- ""
  d2$Officer[    is.na(d2$Officer)    ] <- ""
  d2$KeyEmpl[    is.na(d2$KeyEmpl)    ] <- ""
  d2$HighComp[   is.na(d2$HighComp)   ] <- ""
  d2$FmrOfficer[ is.na(d2$FmrOfficer) ] <- ""
  
  #removing duplicate rows
  d3 <- unique(d2)
  
  #returning a cleaned up dataset
  print("standardize df step complete")
  return( d3 )
  
}


#' @title
#' pre cleaning function
#'
#' @description
#' gets rid of meaningless punctuation (like periods), 
#' converts titles to uppercase form
#' 
#' @export
pre_clean <- function(title.text)
{
  TitleTxt <- title.text
  TitleTxt <- toupper(TitleTxt)
  TitleTxt <- gsub("\\.","",TitleTxt)
  return(TitleTxt)
}
