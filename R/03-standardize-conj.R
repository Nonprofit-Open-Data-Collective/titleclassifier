#Step 3: Standardize Conjunctions

# 03-standardize-conj.R

#' @title 
#' split conjunctions function
#'
#' @description 
#' wrapper for cleaning conjunctions (all the standardizations come in here)
#'
#' @export
standardize_conj <- function( comp.data, title="TitleTxt2" )
{
  TitleTxt <- comp.data[[ title ]]
  
  TitleTxt <- standardize_and(       TitleTxt  )
  TitleTxt <- standardize_to(        TitleTxt  )
  TitleTxt <- standardize_of(        TitleTxt  )
  TitleTxt <- standardize_comma(     TitleTxt  )
  TitleTxt <- standardize_slash(     TitleTxt  )
  TitleTxt <- standardize_and(       TitleTxt  ) #repeated bc of possible standardization changes
  TitleTxt <- standardize_separator( TitleTxt  )
  TitleTxt <- standardize_and(       TitleTxt  )
  TitleTxt <- txt_cleanup(           TitleTxt  )

  comp.data$TitleTxt3 <- TitleTxt
  cat( "✔ standardize conjunctions step complete\n" )
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
  TitleTxt <- fix_misc_splits( TitleTxt )
  
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
standardize_of <- function(TitleTxt)
{
  # we replace all the as of's with since
  TitleTxt <- gsub( "\\bAS OF\\b",  "SINCE", TitleTxt )
  TitleTxt <- gsub( ", SINCE$",    " SINCE", TitleTxt )
  
  # if nothing after "of", then remove entirely
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
standardize_comma <- function(TitleTxt)
{
  # make sure all commas are followed by space
  TitleTxt <- gsub( ",([A-Z])", ", \\1", TitleTxt )
  
  # replace state abbrev with REGION
  TitleTxt <- check_if_state( TitleTxt )
  
  # find most common 'of' cases
  TitleTxt <- replace_w_of_except( TitleTxt )
  
  type <- 
    ifelse( grepl(",", TitleTxt), 
            unlist( lapply( TitleTxt, comma_helper ) ), 2 )
  
  #comma as separator
  TitleTxt <- ifelse(type == 0, gsub(","," &",TitleTxt), TitleTxt) 
  
  #substitute first occurrence of , --> "of", (will be caught in fix_of)
  #"VP, Sales, marketing, and partnerships"
  TitleTxt <- ifelse( type == 1, gsub( ",", " ", TitleTxt), TitleTxt)
  TitleTxt <- ifelse( type == 2, gsub( ",", " AND ", TitleTxt), TitleTxt)
  
  TitleTxt <- gsub( "  ", " ", TitleTxt )
 
  return(TitleTxt)
}


# NEED TO ADD CHECK FOR REGION
#   DIRECTOR, CO
#   FAIRHOPE, AL
#   maybe.state <- 
#      ifelse( grepl( ", [A-Z]{2}$", x ), 
#              gsub( ".*, ([A-Z]{2})$", "\\1", x ), 
#              "" ) 


check_if_state <- function(x)
{
  maybe.state <- 
    ifelse( grepl( ", [A-Z]{2}$", x ), 
            gsub( ".*, ([A-Z]{2})$", "\\1", x ), 
            "" )
            
  is.state <- maybe.state %in% state.abb
  
  x <- 
      ifelse( is.state, 
              gsub( ", ([A-Z]{2})$", " OF REGION", x ), 
              x )
  return(x)
}

replace_w_of_except <- function(x)
{
  use.of.after <- 
    c( "\\bVP, ", "\\bVICE PRES, ",
       "\\bVICE PRESIDENT, ", 
       "\\bSVP, ", "\\bEVP, ",
       "\\bMANAGER, ", "MNGR, ", "\\bHEAD, ",
       "\\bCHAIR, ",
       "PROFESSOR, ", "\\bDEAN, " )

  use.and.before <- 
    c( "EXEC[A-Z]*\\b", "EX ", 
       "DIRECTOR", "DIR[A-Z]*\\b",
       "PRESIDENT", "CHIEF", "CHF",   
       "CEO", "CFO", "COO", "CAO", "CNO",
       "VP", "VICE", "EVP", "SVP",
       "GENERAL", "GEN[A-Z]*\\b",
       "TREASURER", "SECRETARY", 
       "[A-Z]* TRUSTEE",
       "ACTING" )

  # replace commas with '&'
  x <- replace_w_and( x, exp=use.and.before )
  
  # replace commas with 'of'
  x <- replace_w_of(  x, exp=use.of.after  )
  
  return( x )
} 

replace_w_of <- function( x, exp )
{
  exp <- gsub( "^", "(",   exp )
  exp <- gsub( ",", "),(", exp )
  exp <- gsub( "$", ")",   exp )
  for( i in exp )
  {
    x <- gsub( i, "\\1 OF\\2", x )
  }
  return(x)
}

# replace_w_and <- function( x, exp )
# {
#   exp <- gsub( "^", "(",   exp )
#   exp <- gsub( ",", "),(", exp )
#   exp <- gsub( "$", ")",   exp )
#   for( i in exp )
#   {
#    x <- gsub( i, "\\1 &\\2", x )
#   }
#   return(x)
# }


replace_w_and <- function( x, exp )
{
  exp <- gsub( "^", "([A-Z]),( ",   exp )
  exp <- gsub( "$", ")",   exp )
  for( i in exp )
  {
    x <- gsub( i, "\\1 &\\2", x )
  }
  return(x)
}


### CHECK THESE FOR COMMON OF TITLES
#
# HEAD,
# MANAGER,
# VC,
# VICE PRESIDENT,
# VICE PRES,
# VP,
# SVP,
# EVP,
# SECRETARY,
# CHAIR,
# PROFESSOR, 
# DIRECTOR,
# DIRECTOR/EVP,
# DIR,
# PRESIDENT,
# PRES,
# OFFICER,
# TRUSTEE,


  # # most common SOMETHING OF SOMETHING positions from standard titles
  # of.positions <- c("ACADEMICS", "ACQUISITIONS", "ADMINISTRATION", "ADVANCEMENT", 
  #   "ALUMNAE", "BUDGET", "BUSINESS", "COMMITTEE", "COMMUNICATIONS", 
  #   "COMMUNITY", "CORPORATE", "DEVELOPMENT", "ECONOMIC", "EDUCATION", 
  #   "ENGINEERING", "EVENTS", "EXTERNAL", "FACILITIES", "FINANCE", 
  #   "FRATERNITY", "FUNDRAISING", "GOVERNMENT", "HEALTH", "HOSPITALITY", 
  #   "HOUSING", "HUMAN", "INFORMATION", "INSTITUTIONAL", "LEGISLATIVE", 
  #   "LENDING", "MARKETING", "MEDICAL", "MEMBER", "MEMBERSHIP", "MEMBERSHIPS", 
  #   "NURSING", "OPERATIONS", "PHILANTHROPY", "PROGRAMMING", "PROGRAMS", 
  #   "PROJECTS", "PUBLIC", "REAL", "RESEARCH", "RISK", "SALES", "SOFTBALL", 
  #   "STAFF", "STANDARDS", "STRATEGY", "TECHNOLOGY", "UNIVERSITY", 
  #   "VOLUNTEERS")
  #  
  #    # DIRECTOR, PHARMACY
  #    # VP, FUND DEVELOPMENT
  #    # SENIOR VP, ASSISTED LIVING OPERATIONS
  # 
  # # word following comma
  # x <- TitleTxt
  # x <- gsub( ".*, ", "", x )
  # x <- gsub( " .*", "", x )
  # x.of <- gsub( ",", " OF ", TitleTxt )
  # TitleTxt[ x %in% of.positions ] <- x.of[ x %in% of.positions ]
  
  
  
  
  
  
  

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
#' replaces slash with '&'
#'
#' @export
standardize_slash <- function( TitleTxt ){
  TitleTxt <- sapply( TitleTxt, slash_helper, USE.NAMES=F )
  return(TitleTxt)
}

#' @title 
#' standardize slash helper function
#' 
#' @description 
#' checks for a slash and replaces it
#' with an ampersand; 
#' operates on single title
#' 
#' @export
slash_helper <- function( x ){
  
    # does not contain slash
    if( ! grepl( "/", x ) )
    { return(x) }

    # remove double slashes
    x <- gsub( "/ ?/", "/", x )

    x <- gsub( "/", " & ", x )
    x <- gsub( "  ", " ", x )
    return(x) 
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


txt_cleanup <- function(x)
{
  x <- gsub( "\\bAND AND\\b", "AND", x )
  x <- gsub( "\\bOF OF\\b", "OF", x )
  x <- gsub("\\bTO TO\\b", "OF", x )
  
  x <- gsub( "\\bAND AND\\b", "AND", x )
  x <- gsub( "\\bOF OF\\b", "OF", x )
  x <- gsub("\\bTO TO\\b", "OF", x )
  
  #"the" can safely be removed
  x <- gsub( "\\bTHE\\b", "", x )
  
  #remove all parentheticals too
  x <- gsub( "\\s*\\([^\\)]+\\)", "", x )
  
  #remove double spaces
  x <- gsub( " {2,}", " ", x )
  x <- trimws( x )
  
  return(x) 
}