#Step 7:

# 07-standardize-titles.R

#' @title
#' standardize titles function
#'
#' @description
#' Maps multiple variants of a title onto the standard form
#' that is defined in the Google sheet:
#' \href{https://docs.google.com/spreadsheets/d/1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M/edit?usp=sharing}{Title standardization crosswalk}
#' 
#' @export
#' @param comp.data A Part VII compensation data frame.
#' @param officer Name of the officer-flag column.
#' @param gs_title_xwalk Optional title-standardization crosswalk; if `NULL`, loaded from the bundled snapshot.
standardize_titles <- function(comp.data,
                               officer = "F9_07_COMP_DTK_POS_OFF_X",
                               gs_title_xwalk=NULL)
{
  
  # load title standardization 
  # crosswalk from google sheets
  if( is.null(gs_title_xwalk) )
  { gs_title_xwalk <- get_googlesheets_title_xwalk() }
  
  comp.data <- basic_csuite_fixes( comp.data, officer = officer ) 
  
  comp.data <- 
    merge( comp.data, gs_title_xwalk, 
           by.x="TitleTxt7", 
           by.y="title.variant", 
           all.x=T )
  
  cat( "[OK] standardize titles step complete\n" )
  return(comp.data)
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



