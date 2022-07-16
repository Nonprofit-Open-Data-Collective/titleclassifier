#clean-dates
#' @title
#' clean dates wrapper function, takes in a data frame
#'
#' @description
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

#assuming everything is uppercase
#' @title
#' identify titles
#'
#' @description
#' returns T/F if a string contains a date 
#' 
#' @export
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
  month.words <- c("JAN","JANUARY","FEB","FEBRUARY",
                   "MAR","MARCH","APR","APRIL","MAY",
                   "JUN","JUNE","JUL","JULY","AUG", "AUGUST",
                   "SEP","SEPT","SEPTEMBER","OCT","OCTOBER",
                   "NOV","NOVEMBER","DEC","DECEMBER",
                   "YEAR","YR", "MO", "MOS", "MONTH", "MONTHS")
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
#' 
#' @export
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
                   "NOVEMBER","NOV","DECEMBER","DEC", 
                   "YEAR","YR", "MO", "MOS", "MONTH", "MONTHS")
  for(word in month.words){
    TitleTxt <- gsub(word,"",TitleTxt)
  }
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
#'
#' @export
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
#'
#'
#' @description
#' `extract_date` extracts the date information from an unfiltered title string,
#' if present. It works by first checking parentheticals for numbers, then
#' "XX/XX/XXXX" dates, then spelled out months. It extracts the nearby info using
#' a flexible RegEx matching scheme. The function takes in a title text string
#' and outputs a length 2 vector with the first element being the date string,
#' and the second being the number of detected dates in the string. If there is
#' no date present, the return vector has the first element NA and the second 0.
#'
#' @export
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
                     "NOVEMBER","NOV","DECEMBER","DEC", 
                     "YEAR","YR", "MO", "MOS", "MONTH", "MONTHS")
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

# i <- 1
# cotitles <- data.frame(matrix(ncol = 3))
# # comp.data <- read.csv("test-tables/refined-titles-UTD-2022-07-11.csv")
# for(title in comp.data$TitleTxt){
#   title <- pre_clean(title)
#   title <- convert_ordinal(title)
#   if(identify_date(title)) {
#     cotitles[i,1] <- title
#     cotitles[i,3] <- extract_date(title)[1]
#     cotitles[i,2] <- remove_date(title)
#     print(paste0(i," ",extract_date(title)[1]))
#     print(paste0(i, " ",remove_date(title)))
#     i <- i + 1
#   }
# }
# rm(i)
# colnames(cotitles) <- c("ORIGINAL", "FILTERED", "DATE")

