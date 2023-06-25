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
  TitleTxt <- standardize_and(TitleTxt)
  
  for(i in 1:5){
  TitleTxt <- ifelse(grepl("\\bAND AND\\b",TitleTxt), 
                     gsub("\\bAND AND\\b", "AND", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(grepl("\\OF OF\\b",TitleTxt), 
                     gsub("\\bOF OF\\b", "OF", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(grepl("\\TO TO\\b",TitleTxt), 
                     gsub("\\bTO TO\\b", "OF", TitleTxt), TitleTxt)
  }
  
  #"the" can safely be removed
  TitleTxt <- gsub("\\bTHE\\b", "", TitleTxt)
  
  #remove all parentheticals too
  TitleTxt <- gsub("\\s*\\([^\\)]+\\)", "", TitleTxt)
  
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
  
  # "and" not used as separator in raw
  and_true <- ifelse(grepl("\\bAND\\b",TitleTxt),
                     unlist(lapply(TitleTxt, and_helper)),FALSE)
  
  # & used as separator in raw
  amp_true <- ifelse(grepl("&",TitleTxt), 
                     unlist(lapply(TitleTxt, amp_helper)),TRUE)
  
  TitleTxt <- ifelse(   and_true, gsub( "\\bAND\\b", "&", TitleTxt ), TitleTxt )
  TitleTxt <- ifelse( ! amp_true, gsub( "&", " AND ", TitleTxt ), TitleTxt )
  
  TitleTxt <- gsub( "&"," & ",TitleTxt ) 
  TitleTxt <- gsub( "  ", " ", TitleTxt ) # replace double space with single

  TitleTxt <- fix_double_and( TitleTxt )
  
  return(TitleTxt)
}

#' @title 
#' and standardization helper function
#' 
#' @description 
#' operates on a singular title level
#' splits it, checks that if both/all sub titles are valid titles, 
#' then returns a boolean whether and was used as a title separator
#' later lapply-ed for standardize_and usage
#' 
#' as opposed to & its default is false
#' 
#' @export
and_helper <- function(x){
  TitleTxt <- x
  and_true <- FALSE
  if(grepl("\\bAND\\b", TitleTxt)){
    and_split <- unlist(strsplit(TitleTxt,"\\bAND\\b"))
    and_true <- TRUE
    for(i in 1:length(and_split)){
      testTitle <- fix_spelling(and_split[i])
      titlePresent <- FALSE
      for(title in likely.titles){
        if(grepl(title,testTitle))
          titlePresent <- TRUE
      }
      and_true <- and_true & titlePresent
    }
  }
  return(and_true)
}


#' @title 
#' ampersand standardization helper function
#' 
#' @description 
#' operates on a singular title level
#' splits it, checks that if both/all sub titles are valid titles, 
#' then returns a boolean whether and was used as a title separator
#' later lapply-ed for standardize_and usage
#' 
#' as opposed to and, its default is true
#' 
#' @export
amp_helper <- function(x){
  TitleTxt <- x
  amp_true <- TRUE
  if(grepl("&", TitleTxt)){
    amp_split <- unlist(strsplit(TitleTxt,"&"))
    amp_true <- TRUE
    for(i in 1:length(amp_split)){
      testTitle <- fix_spelling(amp_split[i])
      titlePresent <- FALSE
      for(title in likely.titles){
        if(grepl(title,testTitle))
          titlePresent <- TRUE
      }
      amp_true <- amp_true & titlePresent
    }
  }
  return(amp_true)
}



#' @title 
#' double 'and' split rule
#' 
#' @description 
#' Identifies cases with the pattern "title AND word AND word"
#' and makes the substitution "title & word AND word".
#' 
#' @export
fix_double_and <- function(x)
{
  has.two <- grepl( " and [[:alpha:]]{1,} and ", x )
  replace_first <- function(x)
  { sub( " and ", " & ", x  ) }
  x <- ifelse( has.two, replace_first(x), x )
  return(x)
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
  TitleTxt <- ifelse(grepl("\\(",TitleTxt) & grepl("\\)",TitleTxt), 
                     unlist(lapply(TitleTxt, to_helper)),TitleTxt)
  
  #if "to" is at the end of a title, then it's likely also a date extraction
  TitleTxt <- gsub("\\bTO\\s*$","UNTIL",TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize to helper function
#' 
#' @description 
#' does what standardize to but on a singular level
#' checks for to in between parentheticals and replaces with until
#' 
#' @export
to_helper <- function(x){
  TitleTxt <- x
  if(grepl("\\(",TitleTxt) & grepl("\\)",TitleTxt)){
    paren <- stringr::str_extract_all(TitleTxt, "\\([^()]+\\)")[[1]]
    if(length(paren) >= 1) {
      paren <- paren[1]
      paren <- substring(paren, 2, nchar(paren)-1)
      if(nchar(paren) > 0 & grepl("\\bTO\\b",paren)) 
        TitleTxt <- gsub("\\bTO\\b","UNTIL",TitleTxt)
    }
  }
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
  TitleTxt <- ifelse(grepl("\\bFOR\\b",TitleTxt) & !grepl("\\bFOR$",TitleTxt),
                     gsub("\\bFOR\\b", "OF", TitleTxt), TitleTxt)
  
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
  type <- ifelse(grepl(",", TitleTxt), unlist(lapply(TitleTxt, comma_helper)),2)
  
  #comma as separator
  TitleTxt <- ifelse(type == 0, gsub(","," &",TitleTxt), TitleTxt) 
  
  #substitute first occurrence of , --> "of", (will be caught in fix_of)
  #"VP, Sales, marketing, and partnerships"
  TitleTxt <- ifelse(type == 1, gsub(",", " ", TitleTxt), TitleTxt)
  
  TitleTxt <- ifelse(type == 2, gsub(",", " AND ", TitleTxt), TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize comma helper function
#' 
#' @description 
#' returns 0,1,2 depending on whether a comma is used as a title separator,
#' word separator, or just extraneous
#' operates on the atomic vector level
#' 
#' @export
comma_helper <- function(x){
  TitleTxt <- x
  if(grepl(",",TitleTxt)){
    com_split <- unlist(strsplit(TitleTxt,","))
    com_true <- TRUE #comma used as a separator (defaulted to true)
    com_eq_of <- FALSE #comma used as of (i.e. vp, finance)
    for(i in 1:length(com_split)){
      testTitle <- fix_spelling(com_split[i])
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
    if(com_true) return(0)
    else if(com_eq_of) return(1)
  }
  else return(2)
  
}



#' @title 
#' standardize slash function
#' 
#' @description 
#' distinguish if slash is for separator or "of"
#'
#' @export
standardize_slash <- function(TitleTxt){
  slash_type = ifelse(grepl("/",TitleTxt), unlist(lapply(TitleTxt, slash_helper)), 2)
  #slash as separator
  TitleTxt <- ifelse(slash_type == 0, gsub("/"," &",TitleTxt), TitleTxt) 
  
  #substitute first occurrence of / --> "of" (to be fixed in fix_of)
  TitleTxt <- ifelse(slash_type == 1, gsub("/", " ", TitleTxt), TitleTxt)
  
  #extraneous slash is treated as "AND"
  TitleTxt <- ifelse(slash_type == 2, gsub("/", " AND ", TitleTxt), TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize slash helper function
#' 
#' @description 
#' returns 0,1,2 depending on whether a slash is used as a title separator,
#' word separator, or just extraneous
#' operates on the atomic vector level
#' 
#' @export
slash_helper <- function(x){
  TitleTxt <- x
  
  if(grepl("/",TitleTxt)){
    
    slash_split <- unlist(strsplit(TitleTxt,"/"))
    slash_true  <- TRUE   #slash used as a separator (defaulted to true)
    slash_eq_of <- FALSE   #slash used as of (i.e. vp, finance)
    for(i in 1:length(slash_split)){
      testTitle <- fix_spelling(slash_split[i])
      titlePresent <- FALSE
      for( title in likely.titles ){
        if( grepl( title, testTitle ) )
        {
          titlePresent <- TRUE
        }
        if( ( grepl( "CHAIR", testTitle )    |  grepl( "VICE PRESIDENT", testTitle ) | 
              grepl( "DIRECTOR", testTitle ) |  grepl( "DEAN", testTitle )  |
              grepl( "TRUSTEE", testTitle )  |  grepl( "MANAGER", testTitle )  |
              grepl( "CEO", testTitle )      |  grepl( "SECRETARY", testTitle ) ) & i == 1 )
          slash_eq_of <- TRUE
      }
      slash_true <- ( slash_true & titlePresent )
    }
    if(slash_true) return(0)
    else if(slash_eq_of) return(1)
  }
  return(2)
  
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
standardize_separator <- function( x )
{
  
  alt.separators <- c( ";", "\\\\", "/", " - ", " -", "- " )
  x <- gsub( paste( alt.separators, collapse="|" ), " & ", x )
  
  # standard_separator <- "&"
  # alternate_separators <- c( ";", "\\\\", "/", " - ", " -", "- " )
  # for(separator in alternate_separators){
  #   x <- gsub( separator, standard_separator, x )
  # }
  
  return(x)
}

#' @title 
#' misc split rules
#'
#' @description
#' Miscelaneous cases that should be split into
#' two titles but are not addressed by general rules.
#'
#' @export
fix_misc_splits <- function( x )
{
  x <- gsub( "^VICE PRESIDENT TREASURER$", "VICE PRESIDENT & TREASURER", x ) 
  x <- gsub( "\\bSECRETARY TREASURER\\b", "SECRETARY & TREASURER", x )
  x <- gsub( "^VICE PRESIDENT AND CIO$", "VICE PRESIDENT & CIO", x )
  x <- gsub( "^VICE PRESIDENT SECRETARY$", "VICE PRESIDENT & SECRETARY", x )
  x <- gsub( "^CFO AND MINISTRY MARKETING$", "CFO & MINISTRY MARKETING", x )
  x <- gsub( "^PRESIDENT AND REGIONAL", "PRESIDENT & REGIONAL", x )
  x <- gsub( "^PRESIDENT TREASURER$", "BOARD PRESIDENT & TREASURER", x )
  x <- gsub( "SENIOR VICE PRESIDENT GENERAL COUNSEL", "SENIOR VICE PRESIDENT & GENERAL COUNSEL", x )
  x <- gsub( "^TRUSTEE AND PHYSICIAN$", "TRUSTEE & PHYSICIAN", x )
  x <- gsub( "^VICE PRESIDENT DIRECTOR$", "VICE PRESIDENT & DIRECTOR", x )
  return(x)
}
