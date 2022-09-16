#Step 2: Remove Dates

# 02-remove-dates.R
date.words <- 
  c( "JANUARY","JAN","FEBRUARY","FEB",
     "MARCH","MAR","APRIL","APR","MAY",
     "JUNE","JUN","JULY","JUL","AUGUST","AUG",
     "SEPTEMBER","SEPT","SEP","OCTOBER","OCT",
     "NOVEMBER","NOV","DECEMBER","DEC", "PARTIAL YEAR", "PARTIAL YR",
     "PART YEAR", "PART-YEAR", "YR", "YEAR",
     "PART YR","MO", "MOS", "MONTH", "MONTHS" )

#' @title
#' remove/clean dates wrapper function, takes in a data frame
#'
#' @description
#' cleans the dates by first identifying date, setting a flag if it finds one,
#' then removing the date if it is present
#' 
#' @export
remove_dates <- function( comp.data )
{
  title <- convert_ordinal( comp.data$TitleTxt )
  has.date <- identify_date( title )
  title <- remove_date( title )
  
  # 1 if date was removed from title, 0 otherwise
  comp.data$Date.x <- ifelse( has.date, 1, 0 )
  comp.data$TitleTxt2 <- title 
  
  print("remove dates step complete")
  return( comp.data )
}



#ordinal-numbers 
#' @title
#' convert ordinal numbers function
#'
#' @description
#' converts ordinal numbers (1st) to their alphabetic counterpart (first)
#'
#' @export
convert_ordinal <- function(TitleTxt){
  
  #substitute ordinal numbers
  TitleTxt <- gsub("1ST","FIRST",TitleTxt)
  TitleTxt <- gsub("2ND","SECOND",TitleTxt)
  TitleTxt <- gsub("3RD","THIRD",TitleTxt)
  TitleTxt <- gsub("4TH","FOURTH",TitleTxt)
  TitleTxt <- gsub("5TH","FIFTH",TitleTxt)
  TitleTxt <- gsub("6TH","SIXTH",TitleTxt)
  TitleTxt <- gsub("7TH","SEVENTH",TitleTxt)
  TitleTxt <- gsub("8TH","EIGHTH",TitleTxt)
  TitleTxt <- gsub("9TH","NINTH",TitleTxt)
  TitleTxt <- gsub("10TH","TENTH",TitleTxt)
  
  return(TitleTxt)
}


#assuming everything is uppercase
#' @title
#' identify dates
#'
#' @description
#' returns T/F if a string contains a date 
#'
#' @export
identify_date <- function(TitleTxt)
{
  #mm/dd/yyyy format
  format1 <- stringr::str_extract( TitleTxt, "\\d+/\\d+(/\\d+)*\\b" )
  
  #mm-dd-yyyy format
  format2 <- stringr::str_extract(TitleTxt,"\\d+-\\d+(-\\d+)*\\b")
  
  #date strings
  date <- paste0( "\\b", date.words, "\\b", collapse="|" )
  format3 <- grepl( date, TitleTxt )
  
  has.date <- !is.na(format1) | !is.na(format2) | format3
  
  return( has.date ) 
}



# remove dates from raw title text 
#' @title
#' remove dates
#'
#' @description
#' remove dates from raw title text
#'
#' @export
remove_date <- function(TitleTxt)
{
  #mm/dd/yyyy format
  TitleTxt <- gsub( "\\d+/\\d+(/\\d+)*\\b", "", TitleTxt )
  
  #mm-dd-yyyy format
  TitleTxt <- gsub( "\\d+-\\d+(-\\d+)*\\b", "", TitleTxt )
  
  #date strings
  date <- paste0( "\\b", date.words, "\\b", collapse="|" )
  
  TitleTxt <- gsub( date, "", TitleTxt )
  
  #remove miscellaneous digits still lying around
  TitleTxt <- gsub("\\d[A-Z]*\\s", " ", TitleTxt)
  TitleTxt <- gsub("\\d", " ", TitleTxt)
  
  #remove starting and leading spaces and excess spacing
  TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt)
  
  return(TitleTxt)
}



