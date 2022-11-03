#Step 2: Remove Dates

# 02-remove-dates.R
# date.words <- 
#   c( "JANUARY","JAN","FEBRUARY","FEB",
#      "MARCH","MAR","APRIL","APR","MAY",
#      "JUNE","JUN","JULY","JUL","AUGUST","AUG",
#      "SEPTEMBER","SEPT","SEP","OCTOBER","OCT",
#      "NOVEMBER","NOV","DECEMBER","DEC", "PARTIAL YEAR", "PARTIAL YR",
#      "PART YEAR", "PART-YEAR", "YR", "YEAR",
#      "PART YR","MO", "MOS", "MONTH", "MONTHS" )

#' @title
#' remove/clean dates wrapper function, takes in a data frame
#'
#' @description
#' cleans the dates by first identifying date, setting a flag if it finds one,
#' then removing the date if it is present
#' 
#' @export
remove_dates <- function( df, title="F9_07_COMP_DTK_TITLE" )
{
  x <- df[[ title ]]
  
  # replace 1st, 2nd, etc with first, second...
  x <- convert_ordinal( x )
  
  # flag cases that have dates: 
  # date.x = 1 if date was removed from title, 0 otherwise
  df$DATE.X <- identify_dates( x )
  
  df$TitleTxt2 <- remove_date( x )

  print("remove dates step complete")
  return( df )
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
#' returns boolean: 1 if a string contains a date 
#'
#' @export
identify_dates <- function(TitleTxt)
{
  
  # 'YY format: e.g. TRUSTEE (TO APR '19)
  format1 <- grepl( "'[[:digit:]]{2}\\b", TitleTxt )
  
  # YY-YY format: e.g. DIRECTOR (17-18)
  format2 <- grepl( "\\b[[:digit:]]{2}-[[:digit:]]{2}\\b", TitleTxt )
  
  #mm/dd/yyyy format
  format3 <- grepl( "\\d+/\\d+(/\\d+)*\\b", TitleTxt )
  
  #mm-dd-yyyy format
  format4 <- grepl( "\\d+-\\d+(-\\d+)*\\b", TitleTxt )
  
  #date strings
  date <- paste0( "\\b", date.words, "\\b", collapse="|" )
  format5 <- grepl( date, TitleTxt )
  
  has.date <- format1 | format2 | format3 | format4 | format5
  is.date <- ifelse( has.date, 1, 0 )
  
  return( is.date ) 
}



# remove dates from raw title text 
#' @title
#' remove date
#'
#' @description
#' removes date from raw title text
#'
#' @export
remove_date <- function(TitleTxt)
{
  ## \\d = digit
  ## \\s = space
  
  # 'YY format: e.g. TRUSTEE (TO APR '19)
  TitleTxt <- gsub( "'[[:digit:]]{2}\\b", "", TitleTxt )
  
  # YY-YY format: e.g. DIRECTOR (17-18)
  TitleTxt <- gsub( "\\b[[:digit:]]{2}-[[:digit:]]{2}\\b", "", TitleTxt )
  
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
  
  # clean up empty parentheses
  # "CFO ()"
  x <- gsub( "\\(\\s{0,3}\\)", "", x )
  x <- gsub( "\\b\\(", "", x )
  x <- gsub( "\\)\\b", "", x )
  x <- trimws(x)
  
  # clean up trailing hashes
  x <- gsub( "-$", "", x )
  x <- trimws(x)
  
  return(TitleTxt)
}



