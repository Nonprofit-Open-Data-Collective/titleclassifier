#Step 3: Standardize Conjunctions

# 03-standardize-conj.R

#' @title 
#' split conjunctions function
#'
#' @description 
#' wrapper for cleaning conjunctions (all the standardizations come in here)
#'
#' @export
standardize_conj <- function(comp.data, title = "TitleTxt2"){
  TitleTxt <- comp.data[[ title ]]
  TitleTxt <- standardize_and(TitleTxt)
  TitleTxt <- standardize_to(TitleTxt)
  TitleTxt <- standardize_of(TitleTxt)
  TitleTxt <- standardize_comma(TitleTxt)
  TitleTxt <- standardize_slash(TitleTxt)
  TitleTxt <- standardize_and(TitleTxt) #repeated bc of possible standardization changes
  TitleTxt <- standardize_separator(TitleTxt)
  while(grepl("\\bAND AND\\b",TitleTxt))
    TitleTxt <- gsub("\\bAND AND\\b", "AND", TitleTxt)
  while(grepl("\\OF OF\\b",TitleTxt))
    TitleTxt <- gsub("\\bOF OF\\b", "OF", TitleTxt)
  while(grepl("\\TO TO\\b",TitleTxt))
    TitleTxt <- gsub("\\bTO TO\\b", "TO", TitleTxt)
  
  #"the" can safely be removed
  TitleTxt <- gsub("\\bTHE\\b", "", TitleTxt)
  
  comp.data$TitleTxt3 <- TitleTxt
  
  print("standardize conjunctions step complete")
  
  return(comp.data)
}





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
#'
#' @export
standardize_and <- function(TitleTxt){
  
  and_true <- FALSE # "and" not used as separator in raw
  amp_true <- TRUE # & used as separator in raw
  if(grepl("\\bAND\\b",TitleTxt)){
    and_split <- unlist(strsplit(TitleTxt,"\\bAND\\b"))
    and_true <- TRUE
    for(i in 1:length(and_split)){
      testTitle <- apply_substitutes(and_split[i])
      titlePresent <- FALSE
      for(title in likely.titles){
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
      testTitle <- apply_substitutes(amp_split[i])
      titlePresent <- FALSE
      for(title in likely.titles){
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
standardize_to <- function(TitleTxt){
  
  #if "to" is in inside parentheses, almost certainly it was part of a date
  if(grepl("\\(",TitleTxt) & grepl("\\)",TitleTxt)){
    paren <- stringr::str_extract_all(TitleTxt, "\\([^()]+\\)")[[1]]
    if(length(paren) >= 1) {
      paren <- paren[1]
      paren <- substring(paren, 2, nchar(paren)-1)
      if(nchar(paren) > 0 & grepl("\\bTO\\b",paren)) 
        TitleTxt <- gsub("\\bTO\\b","UNTIL",TitleTxt)
    }
  }
  
  #if "to" is at the end of a title, then it's likely also a date extraction
  TitleTxt <- gsub("\\bTO\\s*$","UNTIL",TitleTxt)
  
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
#' depending on context, we can add in "of" between vice president or director
#' and its subject
#'
#' @export
standardize_of <- function(TitleTxt){
  
  #we replace all the as of's with since
  TitleTxt <- gsub("\\bAS OF\\b", "SINCE", TitleTxt)
  #if nothing after "of", then remove entirely
  TitleTxt <- gsub("\\bOF$", "", TitleTxt) 
  if(grepl("\\bFOR\\b",TitleTxt) & !grepl("\\bFOR$",TitleTxt))
    TitleTxt <- gsub("\\bFOR\\b", "OF", TitleTxt)
  
  #replace vp- with vp,
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
#' @export
standardize_comma <- function(TitleTxt){
  
  if(grepl(",",TitleTxt)){
    com_split <- unlist(strsplit(TitleTxt,","))
    com_true <- TRUE #comma used as a separator (defaulted to true)
    com_eq_of <- FALSE #comma used as of (i.e. vp, finance)
    for(i in 1:length(com_split)){
      testTitle <- apply_substitutes(com_split[i])
      titlePresent <- FALSE
      for(title in likely.titles){
        if(grepl(title,testTitle))
          titlePresent <- TRUE
        if((grepl("CHAIR",testTitle) | grepl("VICE PRESIDENT", testTitle) | 
            grepl("DIRECTOR", testTitle) | grepl("DEAN", testTitle) |
            grepl("TRUSTEE", testTitle) | grepl("MANAGER", testTitle) |
            grepl("CEO", testTitle) | grepl("SECRETARY", testTitle)) & i == 1){
          com_eq_of <- TRUE
        }
      }
      com_true <- (com_true & titlePresent)
    }
    #comma as separator
    if(com_true) 
      TitleTxt <- gsub(","," &",TitleTxt)
    
    #substitute first occurence of , --> "of", (will be caught in fix_of)
    #"VP, Sales, marketing, and partnerships"
    else if(com_eq_of) 
      TitleTxt <- gsub(",", " ", TitleTxt) 
    
    #extraneous comma is treated as "and"
    else 
      TitleTxt <- gsub(",", " AND ", TitleTxt)
  }
  return(TitleTxt)
}

#' @title 
#' standardize slash function
#' 
#' @description 
#' distinguish if slash is for separator or "of"
#'
#' @export
standardize_slash <- function(TitleTxt){
  
  if(grepl("/",TitleTxt)){
    slash_split <- unlist(strsplit(TitleTxt,"/"))
    slash_true <- TRUE #slash used as a separator (defaulted to true)
    slash_eq_of <- FALSE #slash used as of (i.e. vp, finance)
    for(i in 1:length(slash_split)){
      testTitle <- apply_substitutes(slash_split[i])
      titlePresent <- FALSE
      # likely.titles <- readRDS("data/likely.titles.RDS")
      for(title in likely.titles){
        if(grepl(title,testTitle))
          titlePresent <- TRUE
        if((grepl("CHAIR",testTitle) | grepl("VICE PRESIDENT", testTitle) | 
            grepl("DIRECTOR", testTitle) | grepl("DEAN", testTitle) |
            grepl("TRUSTEE", testTitle) | grepl("MANAGER", testTitle) |
            grepl("CEO", testTitle) | grepl("SECRETARY", testTitle)) & i == 1)
          slash_eq_of <- TRUE
      }
      slash_true <- (slash_true & titlePresent)
    }
    #slash as separator
    if(slash_true) 
      TitleTxt <- gsub("/"," & ",TitleTxt)
    
    #substitute first occurrence of / --> "of" (to be fixed in fix_of)
    else if(slash_eq_of) 
      TitleTxt <- gsub("/", " ", TitleTxt) 
    
    #extraneous slash is treated as "AND"
    else 
      TitleTxt <- gsub("/", " AND ", TitleTxt)
  }
  return(TitleTxt)
}



#' @title 
#' standardize separator function
#'
#' @description
#' standardize the separator for distinguishing titles
#' possible separators are & ; and \ (rarely used but sometimes)
#' note: / and , already caught
#' They all get mapped to &
#'
#' @export
standardize_separator <- function(TitleTxt){
  
  standard_separator <- "&"
  alternate_separators <- c(";", "\\\\", "/")
  for(separator in alternate_separators){
    TitleTxt <- gsub(separator,standard_separator,TitleTxt)
  }
  
  return(TitleTxt)
}


