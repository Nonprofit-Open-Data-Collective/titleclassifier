#updated title classifier functions

require(dplyr)

#' @title
#' pre cleaning function
#'
#' @description
#' gets rid of meaningless punctuation (like periods), 
#' converts titles to uppercase
pre_clean <- function(title.text){
  TitleTxt <- title.text
  TitleTxt <- toupper(TitleTxt)
  TitleTxt <- gsub("\\.","",TitleTxt)
  return(TitleTxt)
}


#' @title
#' Format the compensation table data frame (from 990 form, Part VII, Table 01)
#'
#' @description
#' `format_comp_df` returns a processed data frame with renamed column titles
#' and cleaned up fields from the raw table extracted from PVII T01 build. Also,
#' this function removes duplicated lines if present. Both the input and output
#' are data frames. Ultimately this function provides takes in raw data and
#' returns a data frame with understandable information about DTK individuals.
#'
#' @export
format_comp_df <- function( comp.data ){
  ## ------------------------------------------------------------------------
  d2 <-
    comp.data %>%
    rename(
      FilingId = OBJECT_ID, #numeric
      FilerEIN = EIN, #numeric
      # FilerName1 = NAME,  #string
      # FilerName = NAME.x, #string #different if reading Jesse's file
      FormYr = TAXYR, #numeric
      FORMTYPE  = FORMTYPE, #990,990ez (no 990PF's for comp table)
      URL = URL, #string
      PersonNm = F9_07_PZ_DTK_NAME, #string
      TitleTxt = F9_07_PZ_DTK_TITLE, #string
      AvgHrs = F9_07_PZ_DTK_AVE_HOURS_WEEK, #numeric
      TrustOrDir = F9_07_PC_DTK_POS_TRUSTEE_INDIV, #X or NA (should also consider institutional trustee)
      Officer = F9_07_PC_DTK_POS_OFFICER,  #X or NA
      RptCmpOrg = F9_07_PZ_COMP_DIRECT,   #numeric
      RptCmpRltd = F9_07_PZ_COMP_RELATED,  #numeric
      OtherComp = F9_07_PZ_COMP_OTHER,  #numeric
      KeyEmpl = F9_07_PC_DTK_POS_KEY_EMPLOYEE, #X or NA
      HighComp = F9_07_PC_DTK_POS_HIGH_COMP_EMP, #X or NA (should only be 1)
      FmrOfficer = F9_07_PC_DTK_POS_FORMER)   #X or NA (sparse)
  
  #converting all compensation fields to numeric if not already
  d2$RptCmpOrg  <- as.numeric( d2$RptCmpOrg )
  d2$RptCmpRltd <- as.numeric( d2$RptCmpRltd )
  d2$OtherComp  <- as.numeric( d2$OtherComp )
  
  #if compensation field is NA, translate that to 0
  d2$RptCmpOrg[ is.na(d2$RptCmpOrg) ]   <- 0
  d2$RptCmpRltd[ is.na(d2$RptCmpRltd) ] <- 0
  d2$OtherComp[ is.na(d2$OtherComp) ]   <- 0
  
  #sum up all compensations for total comp column
  d2$TotalComp <- d2$RptCmpOrg + d2$RptCmpRltd + d2$OtherComp
  
  #converting average hours worked per week field to numeric if not already
  d2$AvgHrs <- as.numeric( d2$AvgHrs )
  d2$AvgHrs[ is.na(d2$AvgHrs) ]   <- 0
  
  #converting missing titles to an empty string
  d2$TitleTxt[ is.na(d2$TitleTxt) ] <- ""
  
  #pre_clean call
  d2$TitleTxt <- pre_clean(d2$TitleTxt)
  
  #converting empty checkboxes for title classification to empty string
  d2$TrustOrDir[ is.na(d2$TrustOrDir) ] <- ""
  d2$Officer[ is.na(d2$Officer) ] <- ""
  d2$KeyEmpl[ is.na(d2$KeyEmpl) ] <- ""
  d2$HighComp[ is.na(d2$HighComp) ] <- ""
  d2$FmrOfficer[ is.na(d2$FmrOfficer) ] <- ""
  
  #removing duplicate rows
  d3 <- unique(d2)
  
  #if working with Jesse's "title-test-run.csv" file
  # d3 <- unique(subset(d2,select = FilerEIN:NTMAJ12))
  
  #returning a cleaned up dataset
  return( d3 )
  
}


#' @title
#' clean dates wrapper function, takes in a data frame
#'
#' @description
#' cleans the dates by first identifying date, setting a flag if it finds one,
#' then removing the date if it is present
#' 
#' @export
clean_dates <- function(comp.data){
  comp.table <- comp.data
  
  comp.table$Date.Code <- 0
  for(i in 1:length(comp.table$TitleTxt)) {
    #asssume the table already formatted correctly
    title <- convert_ordinal(comp.table$TitleTxt[i])
    if(identify_date(title)){
      comp.table$Date.Code[i] <- 1
      comp.table$TitleTxt2[i] <- remove_date(title)
    }
    else{
      comp.table$TitleTxt2[i] <- title
    }
  }
  return(comp.table)
}


#' @title 
#' build split titles function
#'
#' @description 
#' basically doing the smae thing as build standard titles but on a split title
#' wrapper function
#' it will be applied on a data frame
#' 
#' simply splitting on "&"
#' assuming it's already cleaned well
#' 
#' runtime on 100,000 titles ~ 1 hour
#' 
#' @export
build_split_titles <- function(comp.data){
  time1 <- Sys.time()
  raw.titles <- comp.data$TitleTxt2
  pdf <- data.frame(matrix(ncol = ncol(comp.data)))
  colnames(pdf) <- names(comp.data)
  pdf$Num.Title <- 0
  pdf$TitleTxt3 <- NA
  k <- 1
  for(i in 1:length(raw.titles)){
    print(i)
    title <- raw.titles[i]
    if(!is.na(title)){
      title.list <- split_titles(title)
      for(j in 1:length(title.list)){
        subtitle <- title.list[j]
        
        if(length(subtitle) >= 1){
          if(!is.na(subtitle) & nchar(subtitle) > 0){
            subtitle <- apply_substitutes(subtitle)
            pdf[k,] <- comp.data[i,]
            pdf$TitleTxt3[k] <- subtitle
            pdf$Num.Title[k] <- j
            k <- k + 1
          }
        }
      }
    }
  }
  time2 <- Sys.time()
  print(paste0("RUNTIME: ", difftime(time2,time1, units = "mins"), " MINUTES"))
  return(pdf)
}







