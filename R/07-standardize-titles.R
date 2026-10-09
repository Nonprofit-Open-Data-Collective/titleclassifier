#Step 7:

# 07-standardize-titles.R

#' @title
#' standardize titles function
#'
#' @description
#' Maps multiple variants of a title onto the standard form
#' defined in the title-standardization crosswalk ([title.xwalk]).
#' 
#' @export
#' @param comp.data A Part VII compensation data frame.
#' @param officer Name of the officer-flag column.
#' @param gs_title_xwalk Optional title-standardization crosswalk; if `NULL`, loaded from the package data.
#' @param title_patterns Optional head rules for titles the crosswalk does not
#'   list; if `NULL`, loaded from the package data ([title.patterns]).
standardize_titles <- function(comp.data,
                               officer = "F9_07_COMP_DTK_POS_OFF_X",
                               gs_title_xwalk=NULL,
                               title_patterns=NULL)
{

  # load title standardization
  # crosswalk from the package data
  if( is.null(gs_title_xwalk) )
  { gs_title_xwalk <- get_title_xwalk() }
  if( is.null(title_patterns) )
  { title_patterns <- get_title_patterns() }

  comp.data <- basic_csuite_fixes( comp.data, officer = officer )

  comp.data <-
    merge( comp.data, gs_title_xwalk,
           by.x="TitleTxt7",
           by.y="title.variant",
           all.x=T )

  # titles the crosswalk does not list: fall back to a learned head rule
  # (title.patterns; Part VII review, tiers 3-4). title.match records how the
  # standard was found: "exact", "pattern", or NA when neither applies.
  comp.data$title.match <- ifelse( is.na( comp.data$title.standard ), NA_character_, "exact" )
  miss <- is.na( comp.data$title.standard )
  if( any( miss ) ) {
    pat <- match_title_patterns( comp.data$TitleTxt7[ miss ], title_patterns, gs_title_xwalk$title.variant )
    comp.data$title.standard[ miss ] <- pat
    comp.data$title.match[ which( miss )[ ! is.na( pat ) ] ] <- "pattern"
  }

  cat( "[OK] standardize titles step complete\n" )
  return(comp.data)
}


#' @title
#' title heads
#'
#' @description
#' The longest proper suffix and the longest proper prefix of each title that
#' is a crosswalk variant, as whole words: FINANCE COMMITTEE MEMBER has the
#' suffix head COMMITTEE MEMBER; VICE PRESIDENT OF X has the prefix head VICE
#' PRESIDENT. A title is never its own head.
#'
#' @export
#' @param titles A character vector of standardized titles (TitleTxt7).
#' @param variants The crosswalk's title variants.
#' @return A data frame with columns suffix and prefix (NA when none).
title_heads <- function( titles, variants )
{
  ut <- unique( titles[ ! is.na( titles ) & titles != "" ] )
  one <- function( t ) {
    w <- strsplit( t, " ", fixed = TRUE )[[1]]
    n <- length( w )
    if( n < 2 ) return( c( NA_character_, NA_character_ ) )
    suf <- vapply( 2:n,     function(k) paste( w[k:n], collapse = " " ), "" )
    pre <- vapply( (n-1):1, function(k) paste( w[1:k], collapse = " " ), "" )
    s <- suf[ suf %in% variants ]; p <- pre[ pre %in% variants ]
    c( if( length(s) ) s[1] else NA_character_, if( length(p) ) p[1] else NA_character_ )
  }
  h <- if( length( ut ) ) do.call( rbind, lapply( ut, one ) ) else matrix( character(0), 0, 2 )
  i <- match( titles, ut )
  data.frame( suffix = h[ i, 1 ], prefix = h[ i, 2 ], stringsAsFactors = FALSE )
}


#' @title
#' match titles to learned head rules
#'
#' @description
#' For titles the crosswalk does not list, returns the title.standard of the
#' rule for the title's longest suffix head or longest prefix head
#' ([title_heads()]); when both have a rule, the more precise one wins (the
#' suffix on a tie). A title whose longest head has no rule stays NA: rules
#' were learned and validated on longest heads only, and context-dependent
#' heads such as DIRECTOR or PRESIDENT deliberately have none (F-017).
#'
#' @export
#' @param titles A character vector of standardized titles (TitleTxt7).
#' @param patterns The head rules; defaults to [get_title_patterns()].
#' @param variants The crosswalk's title variants; defaults to the variants of [get_title_xwalk()].
#' @return A character vector of title.standard values (NA when no rule applies).
match_title_patterns <- function( titles,
                                  patterns = get_title_patterns(),
                                  variants = get_title_xwalk()$title.variant )
{
  out <- rep( NA_character_, length( titles ) )
  if( ! length( titles ) || is.null( patterns ) || ! nrow( patterns ) ) return( out )
  h <- title_heads( titles, variants )
  pk <- paste( patterns$position, patterns$head, sep = "|" )
  is <- match( paste( "suffix", h$suffix, sep = "|" ), pk )
  ip <- match( paste( "prefix", h$prefix, sep = "|" ), pk )
  is[ is.na( h$suffix ) ] <- NA; ip[ is.na( h$prefix ) ] <- NA
  ps <- patterns$precision[ is ]; pp <- patterns$precision[ ip ]
  use_s <- ! is.na( is ) & ( is.na( ip ) | ps >= pp )
  out[ use_s ] <- patterns$title.standard[ is[ use_s ] ]
  use_p <- ! use_s & ! is.na( ip )
  out[ use_p ] <- patterns$title.standard[ ip[ use_p ] ]
  out
}




#' @title  
#' basic c-suite fixes wrapper function
#' 
#' @description
#' applies conditional logic and creates some flags with helpful info
#'
#' @param comp.data A Part VII compensation data frame.
#' @param title Name of the title column to operate on.
#' @param hours Name of the weekly-hours column.
#' @param pay Name of the total-compensation column.
#' @param officer Name of the officer-flag column.
#'
#' @export
basic_csuite_fixes <-
  function(  comp.data, 
             title = "TitleTxt6", 
             hours = "TOT.HOURS",
             pay   = "TOT.COMP",
             officer = "F9_07_COMP_DTK_POS_OFF_X" )
{
  
  df <- comp.data
  
  TitleTxt      <-  df[[  title   ]]
  weekly.hours  <-  df[[  hours   ]]
  total.pay     <-  df[[  pay     ]]
  officer.flag  <-  df[[  officer ]]

  # flag cases with multiple titles
  df$Multiple.Titles <- ifelse( grepl( "&",  df$TitleTxt3 ), T, F ) 
  
  #ceo
  TitleTxt <- replace_ceo(TitleTxt, weekly.hours, total.pay)
  
  #cfo
  TitleTxt <- replace_cfo(TitleTxt, weekly.hours, total.pay, officer.flag)

  df$TitleTxt7 <- TitleTxt
  return(df)
}



#' @title 
#' replace ceo function
#' 
#' @description 
#' replaces all instances of titles that could be CEO
#' 
#' @export
#' @param TitleTxt A character vector of titles.
#' @param weekly.hours Numeric vector of average weekly hours.
#' @param total.pay Numeric vector of total compensation.
replace_ceo <- function( TitleTxt, weekly.hours, total.pay )
{
  # replace president with CEO if weekly hours > 10 and only singular title
  # TitleTxt <- 
  #   ifelse( TitleTxt == "PRESIDENT" & weekly.hours >= 10, 
  #           "CEO",  TitleTxt  )
  
  #replace chancellor with CEO if paid
  TitleTxt <- ifelse(TitleTxt == "CHANCELLOR" & total.pay > 0, 
                     "CEO", TitleTxt)
  TitleTxt <- ifelse(TitleTxt == "CHANCELLOR" & total.pay == 0, 
                     "BOARD PRESIDENT", TitleTxt)
  
  return(TitleTxt)
}



#' @title 
#' replace cfo function
#' 
#' @description 
#' replaces all instances of titles that could be CFO
#' 
#' @export
#' @param TitleTxt A character vector of titles.
#' @param weekly.hours Numeric vector of average weekly hours.
#' @param total.pay Numeric vector of total compensation.
#' @param officer.flag Officer checkbox flag vector.
replace_cfo <- function(TitleTxt, weekly.hours, total.pay, officer.flag)
{
  # standardize_df() converts the officer checkbox to numeric 0/1 (NA on 990EZ),
  # so compare against 1 -- not the string "X" -- and treat NA as "not an officer".
  is.officer <- ! is.na( officer.flag ) & officer.flag == 1

  #replace director of finance with CFO if officer flag
  TitleTxt <-ifelse(TitleTxt == "FINANCE DIRECTOR" |
                      TitleTxt == "HEAD OF FINANCE" |
                      TitleTxt == "DIRECTOR OF FINANCE AND OPERATIONS",
                    "DIRECTOR OF FINANCE", TitleTxt)

  TitleTxt <- ifelse(TitleTxt == "DIRECTOR OF FINANCE" & is.officer,
                     "CFO", TitleTxt)

  #finance officer
  TitleTxt <- ifelse(TitleTxt == "FINANCE OFFICER" & is.officer &
                       total.pay > 0 & weekly.hours > 40, "CFO", TitleTxt)

  #vp of finance (and operations)
  TitleTxt <- ifelse((TitleTxt == "VICE PRESIDENT OF FINANCE" |
                       TitleTxt == "VICE PRESIDENT OF FINANCE AND OPERATIONS") &
                       is.officer,
                     "CFO", TitleTxt)

  #accountant
  TitleTxt <- ifelse(TitleTxt == "ACCOUNTANT" & is.officer,
                     "CFO", TitleTxt)

  return(TitleTxt)
}



# CONDITIONAL LOGIC FOR CEO
#
#  candidate positions: 
#  - executive director
#  - president
#  - managing director
#  - general director
#  - museum director

# ceo title exists within org
# more than one person with title in org
# max pay rank for people with title is 1, only 
# max hour rank for people with title is 1
# count of titles %in% c(ceo,ed,managing director, general director)

###  ONLY ONE PAID EMPLOYEE
#
#    num.paid == 1
#    num.fte < 3
#    top pay has title in c(ceo,ed,managing director, general director)

###  NO ONE IS FULL TIME
#
# 

###  HAS INTERIM EXECS
#
# 

# if count < 3
# pay rank > 3
# no other CEO



# anyone with "chief" title? 

# deal with presidents last



