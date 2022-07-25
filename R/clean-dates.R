#clean-dates

#assuming everything is uppercase
#' @title
#' identify titles
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


#' @title
#' extract date function
#' EXPERIMENTAL/DEPRECATED
#'
#' @description
#' `extract_date` extracts the date information from an unfiltered title string,
#' if present. It works by first checking parentheticals for numbers, then
#' "XX/XX/XXXX" dates, then spelled out months. It extracts the nearby info using
#' a flexible RegEx matching scheme. The function takes in a title text string
#' and outputs a length 2 vector with the first element being the date string,
#' and the second being the number of detected dates in the string. If there is
#' no date present, the return vector has the first element NA and the second 0.
extract_date <- function(title.text){
  title <- toupper(title.text)
  title <- gsub("\\.","",title)
  returnDate <- c(NA,0)
  
  # Get what is inside the parentheses (only keep strings with #'s contained)
  # k <- stringr::str_extract_all(title, "\\([^()]+\\)")[[1]]
  # k <- substring(k, 2, nchar(k)-1)
  # if(length(k) != 0){
  #   if(!grepl("[[:digit:]]",k[1])) k <- character(0)
  # }
  
  #numerical dates
  # if(length(k) == 0){
    # "/" as a delineator
  k <- stringr::str_extract_all(title,"\\d+/\\d+(/\\d+)*\\b")[[1]]
  if(length(k) == 0){
    # "-" as a delineator
    k <- stringr::str_extract_all(title,"\\d+-\\d+(-\\d+)*\\b")[[1]]
  }
  # }
  
  #spelt out months
  if(length(k) == 0){
    month.words <- c("JANUARY","JAN","FEBRUARY","FEB",
                     "MARCH","MAR","APRIL","APR","MAY",
                     "JUNE","JUN","JULY","JUL","AUGUST","AUG",
                     "SEPTEMBER","SEPT","SEP","OCTOBER","OCT",
                     "NOVEMBER","NOV","DECEMBER","DEC", "PARTIAL YEAR", "PARTIAL YR",
                     "PART YEAR", "PART YR","MO", "MOS", "MONTH", "MONTHS")
    for(word in month.words){
      month <- paste0("\\b",word,"\\b")
      k <- stringr::str_extract_all(title,month)[[1]]
      if(length(k) != 0) break
    }
  }
  
  #formatting our return vector
  if(length(k) > 0) {
    returnDate[1] <- k[1]
    returnDate[2] <- length(k)
    # print(paste0(i," ", k[1]))
  }
  return(returnDate)
}


