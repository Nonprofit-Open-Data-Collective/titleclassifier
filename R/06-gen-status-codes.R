#Step 6: Generate Status Codes

# 06-gen-status-codes.R



##########
##########   UPDATE THESE 
##########


#' @title generate status codes wrapper function
#' 
#' @description  
#' remove all quantifiers and qualifiers
#' we create flags for all of the qualifiers too
#' the qualifiers include: schedule o, at large, as needed, ex officio, co,
#' ordinal numbers, and role statuses (former, future, current, and interim)
#' 
#' roughly 5 minutes for 100,000 titles
#' 
#' TODO pull from the google sheets
#' 
#' @export
gen_status_codes <- function(comp.data, title="TitleTxt5"){
  
  TitleTxt <- comp.data[[title]]

  # no flag for current
  TitleTxt <- gsub( "\\bCURRENT\\b", "", TitleTxt )

  # edge cases - standardize first
  TitleTxt <- gsub( "\\bEX\\s", "FORMER", TitleTxt )
  TitleTxt <- gsub( "\\bEX$",   "FORMER", TitleTxt )
  TitleTxt <- gsub( "\\bEND$",  "FORMER", TitleTxt )
  TitleTxt <- gsub( "\\bNEW$",  "FORMER", TitleTxt )

  # flag but don't remove co- titles
  comp.data$CO.X <- grepl( "\\bCO-", TitleTxt )

  # remove numbers
  numbers <- paste0( "\\b", number.list, "\\b", collapse="|" )
  TitleTxt <- gsub( numbers, "", TitleTxt ) 
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  
  # use TitleTxt6 moving forward b/c
  # passing the df through flag steps
  title <- "TitleTxt6"
  comp.data[[title]] <- TitleTxt
  
  # load current status codes from google sheets
  googlesheets4::gs4_deauth()
  df.status <- googlesheets4::read_sheet( "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M", 
                                          sheet="status-codes", range="A:B",
                                          col_types = "c" )  # c = character

  # don't remove 'regional' because 
  # it changes the title meaning
  
  # drop ex-officio? 
  # sometimes is only title
  
  comp.data <- 
    comp.data %>% 
    flag_and_remove( title, df.status, s.code="FORMER"     )  %>% 
    flag_and_remove( title, df.status, s.code="FOUNDER"    )  %>%
    flag_and_remove( title, df.status, s.code="FUTURE"     )  %>%
    flag_and_remove( title, df.status, s.code="INTERIM"    )  %>%
    flag_and_remove( title, df.status, s.code="SCHED O" )  %>%  
    flag_and_remove( title, df.status, s.code="AT LARGE"   )  %>%  
    flag_and_keep(   title, df.status, s.code="REGIONAL"   )  %>%   
    flag_and_keep(   title, df.status, s.code="EX OFFICIO" )

  
  # sanity check: (we default to former 
  ## if both former and future checked it's likely 'from...until':
  ## code as FORMER
  comp.data$FUTURE.X[ comp.data$FUTURE.X == 1 & comp.data$FORMER.X == 1 ] <- 0 

  print("generate status codes step complete")

  return( comp.data )

}


number.list <- c("ONE", "TWO", "THREE", "FOUR", "FIVE",
                 "SIX", "SEVEN", "EIGHT", "NINE", "TEN",
                 "ELEVEN", "TWELVE", "THIRTEEN", "FOURTEEN",
                 "FIFTEEN", "SIXTEEN", "SEVENTEEN", "EIGHTEEN",
                 "NINETEEN", "TWENTY", "THIRTY", "FORTY","FIFTY",
                 "SIXTY", "SEVENTY", "EIGHTY", "NINETY",
                 "FIRST","SECOND","THIRD","FOURTH","FIFTH",
                 "SIXTH","SEVENTH","EIGHTH","NINETH","TENTH")


flag_and_remove <- function( df, title="TitleTxt6", df.status, s.code="FORMER" )
{
  # collapse all variants into regex OR statement 
  x <- df[[title]]
  v <- df.status$status.variant[ df.status$status.qualifier == s.code ]
  search.terms <- paste0( "\\b", v, "\\b", collapse="|" )
  # create a flag if there are any matches
  df[ paste0( gsub(" ",".",s.code), ".X" ) ] <- grepl( search.terms, x )
  # delete all variants
  x <- gsub( search.terms, "", x )
  # keep status code if it's the full title
  x[ trimws(x) == "" ] <- s.code
  df[[title]] <- x
  return( df )
}

flag_and_keep <- function( df, title="TitleTxt6", df.status, s.code )
{
  # collapse all variants into regex OR statement 
  x <- df[[title]]
  v <- df.status$status.variant[ df.status$status.qualifier == s.code ]
  search.terms <- paste0( "\\b", v, "\\b", collapse="|" )
  # create a flag if there are any matches
  df[ paste0( gsub(" ",".",s.code), ".X" ) ] <- grepl( search.terms, x )
  # replace variants with status code
  x <- gsub( search.terms, s.code, x )
  df[[title]] <- x
  return( df )
}




#' @title generate status codes wrapper function
#' 
#' @description  
#' remove all quantifiers and qualifiers
#' we create flags for all of the qualifiers too
#' the qualifiers include: schedule o, at large, as needed, ex officio, co,
#' ordinal numbers, and role statuses (former, future, current, and interim)
#' 
#' roughly 5 minutes for 100,000 titles
#' 
#' TODO pull from the google sheets
#' 
#' @export
gen_status_codes <- function(comp.data, title="TitleTxt5"){
  comp.table <- comp.data
  TitleTxt <- comp.data[[title]]
  comp.table <- categorize_miscellaneous(comp.table)
  comp.table <- categorize_qualifiers(comp.table)
  
  print("generate status codes step complete")
  
  return(comp.table)
}

#status codes


#' @title identify schedule o function
#'
#' @description 
#' identifies if schedule o is in the title text
#' 
#' @export
identify_sched_o <- function(TitleTxt){
  return(grepl("\\bSEE\\b.*",TitleTxt))
}


#' @title identify at large function
#'
#' @description 
#' identifies if at large is in the title text
#' 
#' @export
identify_at_large <- function(TitleTxt){
  at.large.1 <- grepl("\\bAT LARGE\\b",TitleTxt) #boolean flag
  at.large.2 <- grepl("\\bAT LG\\b",TitleTxt)    #variant
  at.large.present <- at.large.1 | at.large.2
  return(at.large.present)
}

#' @title identify as needed function
#'
#' @description 
#' identifies if "as needed" is in the title text
#' 
#' @export
identify_as_needed <- function(TitleTxt){
  return(grepl("\\bAS NEEDED\\b",TitleTxt))
}


#' @title identify ex officio function
#'
#' @description 
#' identifies if "ex officio" is in the title text
#' 
#' @export
identify_ex_officio <- function(TitleTxt){

  condition.01 <- grepl( "\\bEX-OFFICIO\\b", TitleTxt )  # contains ex-officio
  condition.02 <- TitleTxt == "EX-OFFICIO"  # is exactly ex-officio and no other ttles
  
  # needs to contain ex-officio but can't be the only title 
  is.ex.officio <- condition.01 & ! condition.02
  
  return( is.ex.officio )
}

#' @title identify co- function
#'
#' @description 
#' identifies if "co-" is in the title text
#' 
#' @export
identify_co <- function(TitleTxt){
  return(grepl("\\bCO-",TitleTxt))
}


#' @title filter regional words function
#' 
#' @description 
#' removes regional word text if present and creates flag
#' 
#' @export
filter_regional <- function(TitleTxt ){
  for(i in 1:length(regional.words)){
    word <- regional.words[i]
    if(word != "PA" & word != "AREA")
      TitleTxt <- gsub(paste0("\\b",word,"\\b"),"REGIONAL",
                       TitleTxt)
    else{
      if(word == "PA"){
        TitleTxt <- ifelse(grepl("^\\bPA\\b", TitleTxt), 
                           gsub("^\\bPA\\b","PAST", TitleTxt), 
                           TitleTxt)
        TitleTxt <- ifelse(!grepl("IMMED", TitleTxt), 
                           gsub(paste0("\\b",word,"\\b"),"REGIONAL", TitleTxt),
                           TitleTxt)
      }
      else if(word == "AREA"){
        TitleTxt <- ifelse(!grepl("REPRESENTATIVE",TitleTxt), 
                           gsub(paste0("\\b",word,"\\s[A-Z]+\\b$"),"REGIONAL", TitleTxt),
                           TitleTxt)
        TitleTxt <- gsub(paste0("\\b",word,"\\b"),"REGIONAL",
                         TitleTxt)
      }
    }
  }
  return(TitleTxt)
}



number.list <- c("ONE", "TWO", "THREE", "FOUR", "FIVE",
                 "SIX", "SEVEN", "EIGHT", "NINE", "TEN",
                 "ELEVEN", "TWELVE", "THIRTEEN", "FOURTEEN",
                 "FIFTEEN", "SIXTEEN", "SEVENTEEN", "EIGHTEEN",
                 "NINETEEN", "TWENTY", "THIRTY", "FORTY","FIFTY",
                 "SIXTY", "SEVENTY", "EIGHTY", "NINETY") 
ordinals <- c("FIRST","SECOND","THIRD","FOURTH","FIFTH",
              "SIXTH","SEVENTH","EIGHTH","NINETH","TENTH")


#' @title remove numbers helper function
#' 
#' @description 
#' removing common numbers from title text
#' 
#' @export
remove_numbers <- function(TitleTxt ){
  
  for(number in number.list){
    word <- paste0("\\b",number,"\\b")
    TitleTxt <- gsub(word," ", TitleTxt)
  }
  return(TitleTxt)
}


#' @title categorize miscellaneous wrapper function
#' 
#' @description 
#' categorizes, removes, and creates flags for:
#' schedule o, as needed, at large, ex officio, and co
#' also includes numericals and regionals
#' 
#' @export
categorize_miscellaneous <- function(comp.data, title = "TitleTxt5"){
  comp.table <- comp.data
  TitleTxt <- comp.table[[title]]
  
  comp.table$SCHED.O <- ifelse(identify_sched_o(TitleTxt), 1, 0)
  TitleTxt <- ifelse(identify_sched_o(TitleTxt), 
                     gsub("\\bSEE\\b.*", " ", TitleTxt), TitleTxt)
  
  comp.table$SCHED.O <- ifelse(identify_at_large(TitleTxt), 1, 0)
  TitleTxt <- ifelse(identify_at_large(TitleTxt), 
                     gsub("\\bAT LARGE\\b", " ", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(identify_at_large(TitleTxt), 
                     gsub("\\bAT LG\\b", " ", TitleTxt), TitleTxt)
  
  comp.table$AS.NEEDED <- ifelse(identify_as_needed(TitleTxt), 1, 0)
  TitleTxt <- ifelse(identify_as_needed(TitleTxt), 
                     gsub("\\bAS NEEDED\\b", " ", TitleTxt), TitleTxt)
  
  comp.table$EX.OFFICIO <- ifelse(identify_ex_officio(TitleTxt), 1, 0)
  TitleTxt <- ifelse(identify_ex_officio(TitleTxt), 
                     gsub("\\EX-OFFICIO\\b", " ", TitleTxt), TitleTxt)
  
  comp.table$CO <- ifelse(identify_co(TitleTxt), 1, 0)
  TitleTxt <- ifelse(identify_co(TitleTxt), 
                     gsub("\\bCO-", " ", TitleTxt), TitleTxt)
  
  #filter quantifiers (aka ordinals)
  for(ordinal in ordinals){
    TitleTxt <- ifelse(grepl(ordinal, TitleTxt), 
                       gsub(ordinal," ", TitleTxt), TitleTxt)
  }
  
  #filter regionals
  TitleTxt <- filter_regional(TitleTxt)
  comp.table$REGIONAL <- grepl("REGIONAL", TitleTxt)
  TitleTxt <- gsub("REGIONAL","", TitleTxt)
  
  #remove numbers
  TitleTxt <- remove_numbers(TitleTxt)
  
  comp.table$TitleTxt6 <- TitleTxt
  
  return(comp.table)
}


#' @title standardize qualifiers function
#' 
#' @description 
#' combining all mappings to standardize future, former, current, and interim
#' to their respective categories
#' Note: current is just treated as a regular title 
#' (currently, current is just thrown away with no flag, but that can be changed)
#' 
#' @export
standardize_qualifiers <- function(TitleTxt ){
  
  #alternate method (doing all mappings at once)
  for(i in 1:length(status.mapping$status.variant)){
    word <- status.mapping$status.variant[i]
    if(word != "EX" & word != "END" & word != "NEW")
      TitleTxt <- gsub(paste0("\\b",word,"\\b"),status.mapping$status.qualifier[i],
                       TitleTxt)
    else{
      TitleTxt <- gsub("\\bEX\\s","FORMER",TitleTxt)
      TitleTxt <- gsub("\\bEX$","FORMER",TitleTxt)
      TitleTxt <- gsub("\\bEND$","FORMER",TitleTxt)
      TitleTxt <- gsub("\\bNEW$","FORMER",TitleTxt)
    }
  }
  return(TitleTxt)
}

#' @title categorize qualifiers function
#' 
#' @description 
#' takes in a data frame and categorizes the role statuses
#' future, former, interim, current (current is default, no status for that)
#' 
#' @export
categorize_qualifiers <- function(comp.data, title = "TitleTxt6"){
  
  comp.table <- comp.data
  
  TitleTxt <- comp.table[[title]]
  
  TitleTxt <- standardize_qualifiers(TitleTxt)
  
  #FUTURE
  comp.table$FUTURE <- grepl("FUTURE",TitleTxt)
  TitleTxt <- gsub("FUTURE","", TitleTxt)
  
  #FORMER
  comp.table$FORMER <- grepl("FORMER",TitleTxt)
  TitleTxt <- gsub("FORMER","", TitleTxt)
  
  #INTERIM
  comp.table$INTERIM <- grepl("INTERIM",TitleTxt)
  TitleTxt <- gsub("INTERIM","", TitleTxt)
  
  #CURRENT (default) --> can create a flag for it, but why?
  TitleTxt <- gsub("CURRENT","", TitleTxt)
  
  #some sanity checks (we default to former if both former and future checked)
  comp.table$FUTURE <- ifelse(comp.table$FUTURE == 1 & comp.table$FORMER == 1, 
                              0, comp.table$FUTURE) #most likely from...until
  
  TitleTxt <- trimws( TitleTxt )
  comp.table$TitleTxt6 <- TitleTxt
  
  return(comp.table);
}


