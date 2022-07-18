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
standardize_and <- function(title.text){
  TitleTxt <- title.text
  and_true <- FALSE # "and" not used as separator in raw
  amp_true <- TRUE # & used as separator in raw
  possibles <- c("PRESIDENT", "CEO", "DIRECTOR\\b", "CHAIR",
                 "TRUSTEE\\b", "TREASURER", "SECRETARY", "OFFICER",
                 "COUNSEL","FOUNDER", "PUBLISHER","EDITOR\\b", "MEMBER\\b",
                 "\\bCOO\\b", "CFO","MANAGER", "CTO", "ADMINISTRATOR",
                 "\\bCRO\\b", "CURATOR", "TEACHER", "CLERK", "CONTROLLER",
                 "LIAISON", "INSTRUCTOR", "GENERAL", "COMPTROLLER",
                 "HISTORIAN", "ADVISOR", "ARTISTIC","KEEPER",
                 "CHOREOGRAPHER", "EXECUTIVE\\s*$")
  if(grepl("\\bAND\\b",TitleTxt)){
    and_split <- unlist(strsplit(TitleTxt,"\\bAND\\b"))
    and_true <- TRUE
    for(i in 1:length(and_split)){
      testTitle <- apply_substitutes(and_split[i])
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
      testTitle <- apply_substitutes(amp_split[i])
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
#' depending on context, we can add in "of" between vice president or director
#' and its subject
#'
standardize_of <- function(title.text){
  TitleTxt <- title.text
  
  #we replace all the as of's with since
  TitleTxt <- gsub("\\bAS OF\\b", "SINCE", TitleTxt)
  #if nothing after "of", then remove entirely
  TitleTxt <- gsub("\\bOF$", "", TitleTxt) 
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
standardize_comma <- function(title.text){
  TitleTxt <- title.text
  if(grepl(",",TitleTxt)){
    possibles <- c("PRESIDENT", "CEO", "DIRECTOR\\b", "CHAIR",
                   "TRUSTEE\\b", "TREASURER", "SECRETARY", "OFFICER",
                   "COUNSEL","FOUNDER", "PUBLISHER","EDITOR\\b", "MEMBER\\b",
                   "\\bCOO\\b", "CFO","MANAGER", "CTO", "ADMINISTRATOR",
                   "\\bCRO\\b", "CURATOR", "TEACHER", "CLERK", "CONTROLLER",
                   "LIAISON", "INSTRUCTOR", "GENERAL", "COMPTROLLER",
                   "HISTORIAN", "ADVISOR", "ARTISTIC","KEEPER",
                   "CHOREOGRAPHER", "EXECUTIVE\\s*$")
    com_split <- unlist(strsplit(TitleTxt,","))
    com_true <- TRUE #comma used as a separator (defaulted to true)
    com_eq_of <- FALSE #comma used as of (i.e. vp, finance)
    for(i in 1:length(com_split)){
      testTitle <- apply_substitutes(com_split[i])
      titlePresent <- FALSE
      for(title in possibles){
        if(grepl(title,testTitle))
          titlePresent <- TRUE
        if((grepl("CHAIR",testTitle) | grepl("VICE PRESIDENT", testTitle) | 
            grepl("DIRECTOR", testTitle) | grepl("DEAN", testTitle) |
            grepl("TRUSTEE", testTitle) | grepl("MANAGER", testTitle)) & i == 1){
          com_eq_of <- TRUE
        }
      }
      com_true <- (com_true & titlePresent)
    }
    #comma as separator
    if(com_true) 
      TitleTxt <- gsub(","," &",TitleTxt)
    
    #substitute first occurence of , --> "of", (will be caught in fix_of)
    #"VP Sales, marketing, and partnerships"
    else if(com_eq_of) 
      TitleTxt <- sub(",", " ", TitleTxt) 
    
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
standardize_slash <- function(title.text){
  TitleTxt <- title.text
  if(grepl("/",TitleTxt)){
    possibles <- c("PRESIDENT", "CEO", "DIRECTOR\\b", "CHAIR",
                    "TRUSTEE\\b", "TREASURER", "SECRETARY", "OFFICER",
                    "COUNSEL","FOUNDER", "PUBLISHER","EDITOR\\b", "MEMBER\\b",
                    "\\bCOO\\b", "CFO","MANAGER", "CTO", "ADMINISTRATOR",
                    "\\bCRO\\b", "CURATOR", "TEACHER", "CLERK", "CONTROLLER",
                    "LIAISON", "INSTRUCTOR", "GENERAL", "COMPTROLLER",
                    "HISTORIAN", "ADVISOR", "ARTISTIC","KEEPER",
                    "CHOREOGRAPHER", "EXECUTIVE\\s*$")
    slash_split <- unlist(strsplit(TitleTxt,"/"))
    slash_true <- TRUE #slash used as a separator (defaulted to true)
    slash_eq_of <- FALSE #slash used as of (i.e. vp, finance)
    for(i in 1:length(slash_split)){
      testTitle <- apply_substitutes(slash_split[i])
      titlePresent <- FALSE
      for(title in possibles){
        if(grepl(title,testTitle))
          titlePresent <- TRUE
        if((grepl("CHAIR",testTitle) | grepl("VICE PRESIDENT", testTitle) | 
            grepl("DIRECTOR", testTitle) | grepl("DEAN", testTitle) |
            grepl("TRUSTEE", testTitle) | grepl("MANAGER", testTitle)) & i == 1)
          slash_eq_of <- TRUE
      }
      slash_true <- (slash_true & titlePresent)
    }
    #slash as separator
    if(slash_true) 
      TitleTxt <- gsub("/"," & ",TitleTxt)
    
    #substitute first occurrence of , --> "of"
    else if(slash_eq_of) 
      TitleTxt <- sub("/", " OF ", TitleTxt) 
    
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
standardize_separator <- function(title.text){
  TitleTxt <- title.text
  standard_separator <- "&"
  alternate_separators <- c(";", "\\\\")
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
clean_conjunctions <- function(title.text){
  TitleTxt <- title.text
  TitleTxt <- standardize_and(TitleTxt)
  TitleTxt <- standardize_to(TitleTxt)
  TitleTxt <- standardize_of(TitleTxt)
  TitleTxt <- standardize_comma(TitleTxt)
  TitleTxt <- standardize_slash(TitleTxt)
  TitleTxt <- standardize_separator(TitleTxt)
  TitleTxt <- gsub("\\bAND AND\\b", "AND", TitleTxt)
  TitleTxt <- gsub("\\bOF OF\\b", "OF", TitleTxt)
  TitleTxt <- gsub("\\bTO TO\\b", "TO", TitleTxt)
  
  #"the" can safely be removed
  TitleTxt <- gsub("\\bTHE\\b", "", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' split titles wrapper function
#' 
#' @description 
#' basically does the same as split compound titles, but is more standardized
#' only separator is &, removes punctuation too
#' 
#' @export
#' 
split_titles <- function(title.text){
  TitleTxt <- title.text
  TitleTxt <- clean_conjunctions(TitleTxt)
  title.list <- unlist(strsplit(TitleTxt,"&"))
  return.list <- c()
  for(i in 1:length(title.list)){
    #get rid of punctuation and numbers (except for apostrophe)
    title <- gsub("[^[:alnum:][:space:]'-]","",title.list[i])
    title <- gsub("\\d", "",title)
    title <- gsub("-", " ",title)
    
    title <- unmingle(title)
    if(!grepl("&", title)){
      if(length(title) > 0 && grepl("[A-Z]",title))
        return.list <- append(return.list,title)
    }
    else{
      if(grepl("\\bEX[A-Z]*\\b\\s*&\\s*DIR[A-Z]*\\b", title)){
        title <- gsub("\\bEX[A-Z]*\\b\\s*&\\s*DIR[A-Z]*\\b", "EXECUTIVE DIRECTOR", title)
        return.list <- append(return.list, title)
      }
      else{
        subtitles <- unlist(strsplit(title, "&"))
        for(st in subtitles){
          return.list <- append(return.list, st)
        }
      }
    }
  }
  return(return.list)
}


#' @title 
#' unmingle function
#' 
#' @description 
#' we will use hunspell
unmingle <- function(title.text){
  TitleTxt <- title.text
  
  title.list <- unlist(strsplit(TitleTxt," "))
  for(i in 1:length(title.list)){
    title <- title.list[i]
    if(length(title) >= 1 && !is.na(title) && nchar(title) > 0){
      if(nchar(title) > 10 & title != "INFORMATION" &
         title != "STEWARDSHIP" & title != "ORCHESTRATOR" &
         !grepl("\\bTRANSPORT",title) & !grepl("\\bREPRESENT", title)){
        unmingled.candidate <- hunspell::hunspell_suggest(title)[[1]][1]
        if(grepl("\\s", unmingled.candidate) && 
           regexpr("\\s", unmingled.candidate)[1] > 2){
          title.list[i] <- standardize_space(unmingled.candidate)
          
        }
      }
    }
  }
  TitleTxt <- paste(title.list,collapse = " ")
  
  return(TitleTxt)
}


#' @title 
#' standardize space function
#' 
#' @description 
#' only really used with unmingle (otherwise it's too volatile)
standardize_space <-function(title.text){
  TitleTxt <- title.text
  space_split <- unlist(strsplit(TitleTxt," "))
  possibles <- c("PRESIDENT", "CEO", "DIRECTOR\\b", "CHAIR",
                 "TRUSTEE\\b", "TREASURER", "SECRETARY", "OFFICER",
                 "COUNSEL","FOUNDER", "PUBLISHER","EDITOR\\b", "MEMBER\\b",
                 "\\bCOO\\b", "CFO","MANAGER", "CTO", "ADMINISTRATOR",
                 "\\bCRO\\b", "CURATOR", "TEACHER", "CLERK", "CONTROLLER",
                 "LIAISON", "INSTRUCTOR", "GENERAL", "COMPTROLLER",
                 "HISTORIAN", "ADVISOR", "ARTISTIC","KEEPER",
                 "CHOREOGRAPHER", "EXECUTIVE\\s*$")
  space_true <- TRUE #space used as a separator (defaulted to true)
  for(i in 1:length(space_split)){
    testTitle <- apply_substitutes(space_split[i])
    titlePresent <- FALSE
    for(title in possibles){
      if(grepl(title,testTitle))
        titlePresent <- TRUE
    }
    space_true <- (space_true & titlePresent)
  }
  #space as separator
  if(space_true) 
    TitleTxt <- gsub(" "," & ",TitleTxt)
  
  #other wise do nothing
  return(TitleTxt)
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
#' 
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
            subtitle <- fix_of(subtitle)
            subtitle <- spellcheck(subtitle) #slows things down, but is useful
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


#' @title 
#' fix "of" function
#' 
#' @description 
#' inserts an "of" in between the title and subject if not present and needed
#' e.g. would insert "of" in "VP Operations" --> "VP of Operations"
#' 
#' currently occurs after title standardization 
#' (but can potentially occur before too)
fix_of <- function(title.text){
  TitleTxt <- title.text
  # TitleTxt <- apply_substitutes(TitleTxt) #depending on order of op's
  titleMatch <- FALSE
  subjectMatch <- FALSE
  current.title <- NA
  titlePos <- 0
  if(!grepl("\\bOF\\b", TitleTxt)){
    possible.title.list <- c("VICE PRESIDENT", "DIRECTOR", "CHAIR",
                             "DEAN", "TRUSTEE", "CEO", "SECRETARY")
    possible.subject.list <- c("OPERATIONS", "FINANCE", "ADMINISTRATION",
                               "MANAGEMENT", "EXHIBIT", "PUBLICITY", 
                               "ACTIVITIES", "BUILDING", "EDUCATION",
                               "MARKETING", "STRATEGY", "SALES",
                               "ENSHRINEMENT", "ADVANCEMENT", "FUNDRAISING",
                               "COMMUNICATION", "PRODUCT", "CONSULTING",
                               "TECHNOLOGY", "DEVELOPMENT", "GROUNDS",
                               "\\bART", "MUSIC", "ENGINEERING",
                               "BUSINESS", "DESIGN","EXPERIENCE",
                               "CULTURE", "\\bLAB", "AFFAIR",
                               "BRANDING", "INTERNAL", "PUBLIC",
                               "EXTERNAL", "PROMOTION", "HUMAN RESOURCES",
                               "LEGAL", "RESEARCH", "TRANSPORTATION", 
                               "FELLOWSHIP", "FOUNDATION", "EVENT",
                               "COLLECTION", "SCHOOL", "ASSOCIATION")
    for(title in possible.title.list){
      if(grepl(title,TitleTxt)){
        titleMatch <- TRUE
        current.title <- title
        titlePos <- regexpr(title,TitleTxt)[1]
        break
      }
    }
    for(subject in possible.subject.list){
      if(grepl(subject,TitleTxt)){
        if(regexpr(subject,TitleTxt)[1] > titlePos){
          subjectMatch <- TRUE
          break
        }
      }
    }
  }
  if(titleMatch & subjectMatch){
    TitleTxt <- sub(current.title, paste0(current.title, " OF "), TitleTxt)
    TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt)
  }
  return(TitleTxt)
}

