# conjunction cleaning


#' @title
#' standardize "and" function
#'
#' @description
#' detects whether “and” or & is a title separator
#' use & as separator, “and” as regular? 
#' assume our dates are already extracted
#'
#'& is separator, and is regular 
#' @export
standardize_and <- function(title.text){
  TitleTxt <- title.text
  and_true <- FALSE # "and" not used as separator in raw
  amp_true <- TRUE # & used as separator in raw
  possibles <- c("PRESIDENT", "CEO", "DIRECTOR\\b", "CHAIR",
                 "TRUSTEE\\b", "TREASURER", "SECRETARY", "OFFICER",
                 "COUNSEL","FOUNDER", "PUBLISHER","EDITOR", "MEMBER\\b",
                 "\\bCOO\\b", "CFO","MANAGEMENT", "CTO", "OFFICE ADMINISTRATION",
                 "CRO", "CURATOR", "TEACHER")
  if(grepl("\\bAND\\b",TitleTxt)){
    and_split <- unlist(strsplit(TitleTxt,"\\bAND\\b"))
    and_true <- TRUE
    for(i in 1:length(and_split)){
      testTitle <- standardize_titles(apply_cleaning(and_split[i]))
      titlePresent <- FALSE
      for(title in possibles){
        if(grepl(title,testTitle))
          titlePresent <- TRUE
      }
      and_true <- and_true & titlePresent
    }
  }
  
  if(grepl("&",TitleTxt)){
    amp_split <- unlist(strsplit(TitleTxt,"&"))
    amp_true <- TRUE
    for(i in 1:length(amp_split)){
      testTitle <- standardize_titles(apply_cleaning(amp_split[i]))
      titlePresent <- FALSE
      for(title in possibles){
        if(grepl(title,testTitle))
          titlePresent <- TRUE
      }
      amp_true <- amp_true & titlePresent
    }
  }
  
  if(and_true) TitleTxt <- gsub("\\bAND\\b","&",TitleTxt)
  if(!amp_true) TitleTxt <- gsub("&", " AND ", TitleTxt)
  
  return(TitleTxt)
}


#' @title
#' standardize "to" function
#'
#' @description
#' detects whether to is part of a date or not
#' if it is, replace with "UNTIL", if not, leave be
#' 
#' Note: we have already removed dates by this point so we 
#' cannot look for numbers as a means of detecting date
#' 
#' alternate way of standardize to is to check the date flag:
#' if we detected a date, then likely the "to" should be an until
#'
#' @export
standardize_to <- function(title.text){
  TitleTxt <- title.text
  #if "to" is in inside parentheses, almost certainly it was part of a date
  if(grepl("\\(",TitleTxt) & grepl("\\)",TitleTxt)){
    paren <- stringr::str_extract_all(TitleTxt, "\\([^()]+\\)")[[1]]
    if(length(paren) >= 1) {
      paren <- paren[1]
      paren <- substring(paren, 2, nchar(paren)-1)
      # print(paren)
      if(nchar(paren) > 0 & grepl("\\bTO\\b",paren)) 
        TitleTxt <- gsub("\\bTO\\b","UNTIL",TitleTxt)
    }
  }
  #if "to" is at the end of a title, then it's likely also a date extraction
  if(grepl("\\bTO$",TitleTxt)) 
    TitleTxt <- gsub("\\bTO\\b","UNTIL",TitleTxt)
  
  return(TitleTxt)
}


#' @title
#' standardize "of" usage function
#'
#' @description
#' detects whether “of” is a part of the title, or separator 
#' for example, TITLE OF TITLE is fine, “AS OF DATE” is not
#' 
#' if "of" is part of a date (and namely as of), then we replace with "SINCE"
#' for also gets mapped to of?
#'
#' @export
#' 
standardize_of <- function(title.text){
  TitleTxt <- title.text
  
  #we replace all the as of's with since
  TitleTxt <- gsub("\\bAS OF\\b", "SINCE", TitleTxt)
  if(grepl("\\bFOR\\b",TitleTxt) & !grepl("\\bFOR$",TitleTxt))
    TitleTxt <- gsub("\\bFOR\\b", "OF", TitleTxt)
  
  #replace vp- with vp of
  TitleTxt <- gsub("VP\\s*-","VP,", TitleTxt)
  return(TitleTxt)
  
}

#' @title
#' standardize comma function
#'
#' @description
#' distinguish if comma is for separator, extraneous, or of
#' we also deal with commas as "of"(e.g. VP, finance = VP of Finance)
#' 
#'
#' @export
#' 
standardize_comma <- function(title.text){
  TitleTxt <- title.text
  if(grepl(",",TitleTxt)){
    possibles <- c("PRESIDENT", "CEO", "DIRECTOR\\b", "CHAIR",
                   "TRUSTEE\\b", "TREASURER", "SECRETARY", "OFFICER",
                   "COUNSEL","FOUNDER", "PUBLISHER","EDITOR", "MEMBER\\b",
                   "\\bCOO\\b", "CFO","MANAGEMENT", "CTO", "OFFICE ADMINISTRATION",
                   "CRO", "CURATOR", "TEACHER")
    com_split <- unlist(strsplit(TitleTxt,","))
    com_true <- TRUE #comma used as a separator (defaulted to true)
    com_eq_of <- FALSE #comma used as of (i.e. vp, finance)
    for(i in 1:length(com_split)){
      testTitle <- standardize_titles(apply_cleaning(com_split[i]))
      titlePresent <- FALSE
      for(title in possibles){
        if(grepl(title,testTitle))
          titlePresent <- TRUE
        if((grepl("CHAIR",testTitle) | grepl("VICE PRESIDENT", testTitle) | 
            grepl("DIRECTOR", testTitle) | grepl("DEAN", testTitle) |
            grepl("TRUSTEE", testTitle)) & i == 1){
          com_eq_of <- TRUE
        }
      }
      com_true <- (com_true & titlePresent)
    }
    #comma as separator
    if(com_true) TitleTxt <- gsub(","," &",TitleTxt)
    
    #substitute first occurence of , --> "of"
    else if(com_eq_of) TitleTxt <- sub(",", " OF ", TitleTxt) 
    
    #extraneous comma is treated as "and"
    else TitleTxt <- gsub(",", " AND ", TitleTxt)
  }
  return(TitleTxt)
}

#' @title 
#' standardize separator function
#'
#' @description
#' standardize the separator for distinguishing titles
#' possible separators are / & ;
#' They all get mapped to &
#' 
#' @export
#' 
standardize_separator <- function(title.text){
  TitleTxt <- title.text
  standard_separator <- "&"
  alternate_separators <- c("/",";")
  for(separator in alternate_separators){
    TitleTxt <- gsub(separator,standard_separator,TitleTxt)
  }
  
  return(TitleTxt)
}



#' @title 
#' clean conjunctions function
#'
#' @description 
#' wrapper for cleaning conjunctions (all the standardizations come in here)
#' 
#' @export
#' 
clean_conjunctions <- function(title.text){
  TitleTxt <- title.text
  TitleTxt <- standardize_and(TitleTxt)
  TitleTxt <- standardize_to(TitleTxt)
  TitleTxt <- standardize_of(TitleTxt)
  TitleTxt <- standardize_comma(TitleTxt)
  TitleTxt <- standardize_separator(TitleTxt)
  
  return(TitleTxt)
}



#' @title 
#' split titles function
#'
#' @description 
#' basically doing the smae thing as build standard titles but on a split title
#' wrapper function
#' it will be applied on a data frame however (sorta like build_standard_titles
#' 
#' simply splitting on "&"
#' assuming it's already cleaned well, 
#' this function is to take the place of split_compound_titles
#' 
#' @export
#' 
split_titles <- function(comp.data){
  raw.titles <- comp.data$TitleTxt2
  pdf <- data.frame(matrix(ncol = ncol(comp.data)))
  colnames(pdf) <- names(comp.data)
  pdf$Num.Title <- 0
  pdf$Standardized.Title <- NA
  k <- 1
  for(i in 1:length(raw.titles)){
  # for(i in 1:1000){
    print(i)
    title <- raw.titles[i]
    title <- clean_conjunctions(title)
    if(!is.na(title)){
      title.list <- unlist(strsplit(title,"&"))
      
      for(j in 1:length(title.list)){
        subtitle <- title.list[j]
        subtitle <- apply_cleaning(subtitle)
        # print(subtitle)
        if(length(subtitle) >= 1){
          if(!is.na(subtitle) & nchar(subtitle) > 0){
            subtitle <- standardize_titles(subtitle)
            pdf[k,] <- comp.data[i,]
            pdf$Standardized.Title[k] <- subtitle
            pdf$Num.Title[k] <- j
            k <- k + 1
          }
        }
      }
    }
  }
  return(pdf)
}




# for(title in dates_filtered$TitleTxt){
#   TitleTxt <- toupper(title)
#   # if(grepl("\\bAND\\b",TitleTxt) & grepl("&",TitleTxt))
#   #   print(TitleTxt)
#   if(grepl(",",TitleTxt))
#     print(standardize_comma(TitleTxt))
# }