#Step 7:

# 07-standardize-titles.R

#' @title
#' standardize titles function
#'
#' @description
#' TODO not written yet, but we will generate the standardizations using the dynamic google sheets
#' still title manipulation, but now we need context of other codes
#' 
#' @export
standardize_titles <- function(comp.data, title = "TitleTxt6", 
                               hours = "TOT.HOURS", pay = "TOT.COMP",
                               officer = "F9_07_COMP_DTK_POS_OFF_X"){
  
  # MOVED TO SPELLING STEP
  # TitleTxt = comp.data[[title]]
  # TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  # TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  # comp.data[[title]] <- TitleTxt
  
  # manipulations with google sheets
  googlesheets4::gs4_deauth()
  df.standard <- googlesheets4::read_sheet( "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M", 
                                            sheet="title-standardization", range="A:C",
                                            col_types = "c" )  # c = character
  df.standard[ is.na( df.standard ) ] <- ""
  df.standard <- unique( df.standard )
  
  comp.data <- conditional_logic(comp.data, officer = officer)
  
  # comp.data <- merge( comp.data, df.standard, by.x=title, by.y="title.variant", all.x=T )
  comp.data <- merge( comp.data, df.standard, by.x="TitleTxt7", by.y="title.variant", all.x=T )
  
  print("standardize titles step complete")
  return(comp.data)
}


#' @title  
#' conditional logic wrapper function
#' 
#' @description 
#' applies conditional logic and creates flags with helpful info
#' 
#' @export
conditional_logic <- function(comp.data, title = "TitleTxt6", 
                              hours = "TOT.HOURS", pay = "TOT.COMP",
                              officer = "F9_07_COMP_DTK_POS_OFF_X"){
  
  df <- comp.data
  
  TitleTxt <- df[[title]]
  weekly.hours <- df[[hours]]
  total.pay <- df[[pay]]
  officer.flag <- df[[officer]]
  
  df$Multiple.Titles <- ifelse(grepl("&", df$TitleTxt3), T, F) 
  #flag for having multiple titles
  
  
  #ceo
  TitleTxt <- replace_ceo(TitleTxt, weekly.hours, total.pay)
  
  #cfo
  TitleTxt <- replace_cfo(TitleTxt, weekly.hours, total.pay, officer.flag)
  
  #board
  
 
  #necessarily a slow step?
  #set specific flags
  # orgs <- unique(df$OBJECT_ID)
  # for(i in 1:length(orgs)){
  #   
  # }
  
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
replace_ceo <- function(TitleTxt, weekly.hours, total.pay){
  
  #replace president with CEO if weekly hours > 10 and only singular title
  TitleTxt <- ifelse(TitleTxt == "PRESIDENT" & weekly.hours >= 10, 
                     "CEO", TitleTxt)
  
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
replace_cfo <- function(TitleTxt, weekly.hours, total.pay, officer.flag){
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

#' @title 
#' replace board function
#' 
#' @description 
#' replaces all instances of titles that could be a board position
#' including finance director and such
#' 
#' @export
replace_board <- function(TitleTxt){
}
