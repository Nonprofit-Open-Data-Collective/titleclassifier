#status codes


#' @title 
#' filter schedule o function
#' 
#' @description 
#' add binary code for “see schedule o” comments and remove such text
#' also includes see attached and abbreviations of sched o
filter_sched_o <- function(title.text){
  TitleTxt <- title.text
  
  sched.o.present <- grepl("\\bSEE\\b.*",TitleTxt) #boolean flag
  
  if(sched.o.present){
    TitleTxt <- gsub("\\bSEE\\b.*", " ", TitleTxt)
  }
  
  returnVals <- c(TitleTxt, sched.o.present) #title text and binary flag
  
  return(returnVals)
}


#' @title 
#' filter at large function
#' 
#' 
#' @description 
#' removes at large text if present and creates flag
filter_at_large <- function(title.text){
  TitleTxt <- title.text
  at.large.present <- grepl("\\bAT LARGE\\b",TitleTxt) #boolean flag
  if(!at.large.present) 
    at.large.present <- grepl("\\bAT LG\\b",TitleTxt) #variant
  
  if(at.large.present){
    TitleTxt <- gsub("\\bAT LARGE\\b", " ", TitleTxt)
    TitleTxt <- gsub("\\bAT LG\\b", " ", TitleTxt)
  }
  
  returnVals <- c(TitleTxt, at.large.present) #title text and binary flag
  
  return(returnVals)
}


#' @title 
#' filter as needed function
#' 
#' 
#' @description 
#' removes as needed text if present and creates flag
filter_as_needed <- function(title.text){
  TitleTxt <- title.text
  as.needed.present <- grepl("\\bAS NEEDED\\b",TitleTxt) #boolean flag
  
  if(as.needed.present){
    TitleTxt <- gsub("\\bAS NEEDED\\b", " ", TitleTxt)
  }
  
  returnVals <- c(TitleTxt, as.needed.present) #title text and binary flag
  
  return(returnVals)
}


#' @title 
#' filter ex-officio function
#' 
#' 
#' @description 
#' removes ex-officio text if present and creates flag
filter_ex_officio <- function(title.text){
  TitleTxt <- title.text
  
  #if ex-officio is just the title
  if(!is.na(TitleTxt) && TitleTxt == "EX-OFFICIO"){
    returnVals <- c("EX-OFFICIO", FALSE)
    return(returnVals)
  }
  
  ex.officio.present <- grepl("\\bEX-OFFICIO\\b",TitleTxt) #boolean flag
  
  if(ex.officio.present){
    TitleTxt <- gsub("\\EX-OFFICIO\\b", " ", TitleTxt)
  }
  
  returnVals <- c(TitleTxt, ex.officio.present) #title text and binary flag
  return(returnVals)
}

#' @title 
#' filter co function
#' 
#' 
#' @description 
#' removes co- text if present and creates flag
#' co in this case means joint/together
filter_co <- function(title.text){
  TitleTxt <- title.text
  co.present <- grepl("\\bCO-",TitleTxt) #boolean flag
  
  if(co.present){
    TitleTxt <- gsub("\\bCO-", " ", TitleTxt)
  }
  
  returnVals <- c(TitleTxt, co.present) #title text and binary flag
  
  return(returnVals)
}

#' @title 
#' filter regional words function
#' 
#' 
#' @description 
#' removes regional word text if present and creates flag
filter_regional <- function(title.text){
  TitleTxt <- title.text
  regional.words <- readRDS("data/regional.words.RDS")
  for(i in 1:length(regional.words$REGIONS)){
    word <- regional.words$REGIONS[i]
    if(word != "PA" && word != "AREA")
      TitleTxt <- gsub(paste0("\\b",word,"\\b"),"REGIONAL",
                       TitleTxt)
    else{
      if(word == "PA"){
        if(grepl("^\\bPA\\b", TitleTxt))
          TitleTxt <- gsub("^\\bPA\\b","PAST",
                           TitleTxt)
        else if(!grepl("IMMED", TitleTxt))
        TitleTxt <- gsub(paste0("\\b",word,"\\b"),"REGIONAL",
                         TitleTxt)
      }
      else if(word == "AREA"){
        if(!grepl("REPRESENTATIVE",TitleTxt))
          TitleTxt <- gsub(paste0("\\b",word,"\\s[A-Z]+\\b$"),"REGIONAL",
                         TitleTxt)
        TitleTxt <- gsub(paste0("\\b",word,"\\b"),"REGIONAL",
                        TitleTxt)
      }
    }
  }
  return(TitleTxt)
}

#' @title 
#' remove numbers helper function
#' 
#' @description 
#' removing common numbers from title text
remove_numbers <- function(title.text){
  TitleTxt <- title.text
  number.list <- c("ONE", "TWO", "THREE", "FOUR", "FIVE",
                  "SIX", "SEVEN", "EIGHT", "NINE", "TEN",
                  "ELEVEN", "TWELVE", "THIRTEEN", "FOURTEEN",
                  "FIFTEEN", "SIXTEEN", "SEVENTEEN", "EIGHTEEN",
                  "NINETEEN", "TWENTY", "THIRTY", "FORTY","FIFTY",
                  "SIXTY", "SEVENTY", "EIGHTY", "NINETY") 
  for(number in number.list){
    word <- paste0("\\b",number,"\\b")
    TitleTxt <- gsub(word," ", TitleTxt)
  }
  return(TitleTxt)
}


#' @title 
#' categorize miscellaneous wrapper function
#' 
#' 
#' @description 
#' categorizes, removes, and creates flags for:
#' schedule o, as needed, at large, ex officio, and co
#' also includes numericals and regionals
categorize_miscellaneous <- function(comp.data){
  comp.table <- comp.data
  
  comp.table$TitleTxt4 <- NA
  
  #new flags
  comp.table$SCHED.O <- 0
  comp.table$AT.LARGE <- 0
  comp.table$AS.NEEDED <- 0
  comp.table$EX.OFFICIO <- 0
  comp.table$CO <- 0 #co-
  comp.table$QUANTIFIER <- ""
  comp.table$REGIONAL <- 0
  
  for(i in 1:length(comp.table$TitleTxt3)){
    TitleTxt <- comp.table$TitleTxt3[i]
    
    #schedule o
    sched.o.info <- filter_sched_o(TitleTxt)
    if(sched.o.info[2]) {
      comp.table$SCHED.O[i] <- 1 
      TitleTxt <- sched.o.info[1]
    }
    
    #filter at large
    at.large.info <- filter_at_large(TitleTxt)
    if(at.large.info[2]) {
      comp.table$AT.LARGE[i] <- 1 
      TitleTxt <- at.large.info[1]
    }
    
    #filter as needed
    as.needed.info <- filter_as_needed(TitleTxt)
    if(as.needed.info[2]) {
      comp.table$AS.NEEDED[i] <- 1 
      TitleTxt <- as.needed.info[1]
    }
    
    #filter ex officio
    ex.officio.info <- filter_ex_officio(TitleTxt)
    if(ex.officio.info[2]) {
      comp.table$EX.OFFICIO[i] <- 1 
      TitleTxt <- ex.officio.info[1]
    }
    
    #filter co
    co.info <- filter_co(TitleTxt)
    if(co.info[2]) {
      comp.table$CO[i] <- 1 
      TitleTxt <- co.info[1]
    }
    
    #filter quantifiers (aka ordinals)
    ordinals <- c("FIRST","SECOND","THIRD","FOURTH","FIFTH",
                  "SIXTH","SEVENTH","EIGHTH","NINETH","TENTH")
    for(ordinal in ordinals){
      if(grepl(ordinal, TitleTxt)){
        # if(grepl(paste0("^",ordinal), TitleTxt))
        comp.table$QUANTIFIER[i] <- ordinal
        TitleTxt <- gsub(ordinal," ", TitleTxt)
      }
    }
    
    #filter regions
    TitleTxt <- filter_regional(TitleTxt)
    if(grepl("REGIONAL",TitleTxt)){
      TitleTxt <- gsub("REGIONAL","", TitleTxt)
      comp.table$REGIONAL[i] <- 1
      possible.regional.list <- readRDS("data/possible.regional.list.RDS")
      for(title in possible.regional.list){
        if(grepl(title,TitleTxt)) {
          TitleTxt <- paste0("REGIONAL ", title)
          break
        }
      }
    }
    
    #remove numbers
    TitleTxt <- remove_numbers(TitleTxt)
    
    comp.table$TitleTxt4[i] <- TitleTxt
  }
  
  return(comp.table)
}

#' @title 
#' standardize qualifiers function
#' 
#' 
#' @description 
#' combining all mappings to standardize future, former, current, and interim
#' to their respective categories
#' Note: current is just treated as a regular title 
#' (currently, current is just thrown away with no flag, but that can be changed)
standardize_qualifiers <- function(title.text){
  TitleTxt <- title.text
  
  #method 1 (using helper functions)
  # TitleTxt <- standardize_former(TitleTxt)
  # TitleTxt <- standardize_future(TitleTxt)
  # TitleTxt <- standardize_interim(TitleTxt)
  # TitleTxt <- standardize_current(TitleTxt)
  
  #alternate method (doing all mappings at once)
  status.mapping <- readRDS("data/status.mapping.RDS")
  for(i in 1:length(status.mapping$VARIANT)){
    word <- status.mapping$VARIANT[i]
    if(word != "EX" && word != "END" && word != "NEW")
      TitleTxt <- gsub(paste0("\\b",word,"\\b"),status.mapping$CANONICAL[i],
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

#' @title 
#' categorize qualifiers function
#' 
#' @description 
#' takes in a data frame and categorizes the role statuses
#' future, former, interim, current (current is default, no status for that)
#' 
categorize_qualifiers <- function(comp.data){
  comp.table <- comp.data
  
  comp.table$FUTURE <- 0
  comp.table$FORMER <- 0
  comp.table$INTERIM <- 0
  for(i in 1:length(comp.table$TitleTxt4)){
    TitleTxt <- comp.table$TitleTxt4[i]
    
    TitleTxt <- standardize_qualifiers(TitleTxt)
    
    #FUTURE
    if(grepl("FUTURE",TitleTxt)){
      TitleTxt <- gsub("FUTURE","", TitleTxt)
      comp.table$FUTURE[i] <- 1
    }
    
    #FORMER
    if(grepl("FORMER",TitleTxt)){
      TitleTxt <- gsub("FORMER","", TitleTxt)
      comp.table$FORMER[i] <- 1
    }
    
    #INTERIM
    if(grepl("INTERIM", TitleTxt)){
      TitleTxt <- gsub("INTERIM","", TitleTxt)
      comp.table$INTERIM[i] <- 1
    }
    
    #CURRENT (default) --> can create a flag for it, but why?
    if(grepl("CURRENT", TitleTxt)){
      TitleTxt <- gsub("CURRENT","", TitleTxt)
    }
    
    #some sanity checks (we default to former if both former and future checked)
    if(comp.table$FUTURE[i] == 1 && comp.table$FORMER[i] == 1){
      comp.table$FUTURE[i] <- 0 #most likely = from until
    }
    
    
    #final cleaning steps:
    TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
    TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
    
    TitleTxt <- remove_trailing_conjunctions(TitleTxt)
    
    comp.table$TitleTxt4[i] <- TitleTxt
    
  }
  
  return(comp.table)
}




#' @title 
#' generate status codes wrapper function
#' 
#' @description  
#' phase 4: remove all quantifiers and qualifiers
#' we create flags for all of the qualifiers too
#' the qualifiers include: schedule o, at large, as needed, ex officio, co,
#' ordinal numbers, and role statuses (former, future, current, and interim)
#' 
#' roughly 5 minutes for 100,000 titles
#' @export
gen_status_codes <- function(comp.data){
  time1 <- Sys.time()
  comp.table <- comp.data
  
  comp.table <- categorize_miscellaneous(comp.table)
  comp.table <- categorize_qualifiers(comp.table)
  time2 <- Sys.time()
  print(paste0("RUNTIME (in min): ", difftime(time2, time1, units = "mins")))
  return(comp.table)
}
