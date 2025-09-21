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
standardize_titles <- function(comp.data, title = "TitleTxt6", 
                               hours = "TOT.HOURS", pay = "TOT.COMP",
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
  
  cat( "✔ standardize titles step complete\n" )
  return(comp.data)
}




#' @title  
#' basic c-suite fixes wrapper function
#' 
#' @description 
#' applies conditional logic and creates some flags with helpful info
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
replace_cfo <- function(TitleTxt, weekly.hours, total.pay, officer.flag)
{
  #replace director of finance with CFO if officer flag
  TitleTxt <-ifelse(TitleTxt == "FINANCE DIRECTOR" | 
                      TitleTxt == "HEAD OF FINANCE" | 
                      TitleTxt == "DIRECTOR OF FINANCE AND OPERATIONS", 
                    "DIRECTOR OF FINANCE", TitleTxt)
  
  TitleTxt <- ifelse(TitleTxt == "DIRECTOR OF FINANCE" & officer.flag == "X", 
                     "CFO", TitleTxt)
  
  #finance officer
  TitleTxt <- ifelse(TitleTxt == "FINANCE OFFICER" & officer.flag == "X" &
                       total.pay > 0 & weekly.hours > 40, "CFO", TitleTxt)
  
  #vp of finance (and operations)
  TitleTxt <- ifelse((TitleTxt == "VICE PRESIDENT OF FINANCE" | 
                       TitleTxt == "VICE PRESIDENT OF FINANCE AND OPERATIONS") &
                       officer.flag == "X",
                     "CFO", TitleTxt)
  
  #accountant
  TitleTxt <- ifelse(TitleTxt == "ACCOUNTANT" & officer.flag == "X",
                     "CFO", TitleTxt)
  
  return(TitleTxt)
}
