#clean-dates

#assuming everything is uppercase
#' @title
#' identify dates
#'
#' @description
#' returns T/F if a string contains a date 
identify_date <- function(title.text){
  TitleTxt <- title.text
  #mm/dd/yyyy format
  k <- stringr::str_extract_all(TitleTxt,"\\d+/\\d+(/\\d+)*\\b")[[1]][1]
  if(!is.na(k)) {
    # print(k)
    return(TRUE)
  }
  
  #mm-dd-yyyy format
  k <- stringr::str_extract_all(TitleTxt,"\\d+-\\d+(-\\d+)*\\b")[[1]][1]
  if(!is.na(k)) {
    # print(k)
    return(TRUE)
  }
  
  #months
  month.words <- c("JANUARY","JAN","FEBRUARY","FEB",
                   "MARCH","MAR","APRIL","APR","MAY",
                   "JUNE","JUN","JULY","JUL","AUGUST","AUG",
                   "SEPTEMBER","SEPT","SEP","OCTOBER","OCT",
                   "NOVEMBER","NOV","DECEMBER","DEC", "PARTIAL YEAR", "PARTIAL YR",
                   "PART YEAR", "PART-YEAR", "YR", "YEAR",
                   "PART YR","MO", "MOS", "MONTH", "MONTHS")
  for(word in month.words){
    month <- paste0("\\b",word,"\\b")
    if(grepl(month, TitleTxt)) {
      # print(month)
      return(TRUE)
    }
  }
  return(FALSE)
}


# create a binary flag for cases that contain dates 
# gen_date_code <- function(title.text){
#   
# }

# remove dates from raw title text 
#' @title
#' remove dates
#'
#' @description
#' remove dates from raw title text
remove_date <- function(title.text){
  TitleTxt <- title.text
  #mm/dd/yyyy format
  k <- stringr::str_extract_all(TitleTxt,"\\d+/\\d+(/\\d+)*\\b")[[1]][1]
  if(!is.na(k)) {
    TitleTxt <- gsub(k,"",TitleTxt)
  }
  
  #mm-dd-yyyy format
  k <- stringr::str_extract_all(TitleTxt,"\\d+-\\d+(-\\d+)*\\b")[[1]][1]
  if(!is.na(k)) {
    TitleTxt <- gsub(k,"",TitleTxt)
  }
  
  #month words
  month.words <- c("JANUARY","JAN","FEBRUARY","FEB",
                   "MARCH","MAR","APRIL","APR","MAY",
                   "JUNE","JUN","JULY","JUL","AUGUST","AUG",
                   "SEPTEMBER","SEPT","SEP","OCTOBER","OCT",
                   "NOVEMBER","NOV","DECEMBER","DEC", "PARTIAL YEAR", "PARTIAL YR",
                   "PART YEAR", "PART YR","MO", "MOS", "MONTH", "MONTHS")
  for(word in month.words){
    month <- paste0("\\b",word,"\\b")
    TitleTxt <- gsub(month,"",TitleTxt)
  }
  
  #remove miscellaneous digits still lying around
  TitleTxt <- gsub("\\d[A-Z]*\\s", " ", TitleTxt)
  TitleTxt <- gsub("\\d", " ", TitleTxt)
  
  #remove starting and leading spaces and excess spacing
  TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt)
  
  return(TitleTxt)
  
}



#ordinal-numbers 
#' @title
#' convert ordinal numbers function
#'
#' @description
#' converts ordinal numbers (1st) to their alphabetic counterpart (first)
convert_ordinal <- function(title.text){
  TitleTxt <- title.text
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


