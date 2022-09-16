#generate tables

# comp.table <- read.csv("test-tables/split/refined-titles-UTD-2022-07-31.csv")

#' @title 
#' get title dump function
#' 
#' @description 
#' generate all the titles that contain a given word and output a dataframe 
#' including the original raw title and its cleaned version
#' this function works for individual words as well as word lists
#' 
#' @export
get_title_dump <- function(comp.table, target.word){
  k <- 1
  cotitles <- data.frame(matrix(ncol = 2))
  for(i in 1:length(comp.table$TitleTxt)){
    title <- toupper(comp.table$TitleTxt[i])
    if(length(target.word) == 1){
      if(grepl(target.word,title)){
        cotitles[k,1] <- title
        cotitles[k,2] <- comp.table$TitleTxt7[i]
        k <- k + 1
      }
    }
    else{
      for(word in target.word){
        if(grepl(word,title)){
          cotitles[k,1] <- title
          cotitles[k,2] <- comp.table$TitleTxt4[i]
          k <- k + 1
        }
      }
    }
  }
  # cotitles <- unique(cotitles) #can be remove repetitive titles
  colnames(cotitles) <- c("TitleTxt", "Cleaned_Title")
  return(cotitles)
}



#' @title 
#' gen helpful tables function
#' 
#' @description 
#' generates and saves some helpful tables from a compensation table
#' including raw and cleaned titles including some specific string.
#' these list include titles with "co", "of", "to", ",", "/", "&", and "and".
#' they also include titles with ordinal numbers (1st, second, 3rd, etc.) and
#' c-suite titles (CEO, Chief Financial Officer).
#' 
#' The titles are saved into the /test-tables directory and have the file
#' naming convention of "titles with ~specific string~ ~date~". 
#' They are in csv format.
#' 
#' @export
gen_helpful_tables <- function(comp.table){
  co.titles <- get_title_dump(comp.table, "\\bCO\\b")
  of.titles <- get_title_dump(comp.table, "\\bOF\\b")
  to.titles <- get_title_dump(comp.table, "\\bTO\\b")
  comma.titles <- get_title_dump(comp.table, ",")
  slash.titles <- get_title_dump(comp.table, "/")
  ampersand.titles <- get_title_dump(comp.table, "&")
  and.titles <- get_title_dump(comp.table,"\\bAND\\b")

  ordinal.list <- c("1ST", "2ND", "3RD", "4TH", "5TH",
                    "6TH", "7TH", "8TH", "9TH", "10TH",
                    "FIRST", "SECOND", "THIRD", "FOURTH",
                    "FIFTH", "SIXTH", "SEVENTH", "EIGHTH",
                    "NINTH", "TENTH")
  ordinal.titles <- get_title_dump(comp.table,ordinal.list)

  c.suite.list <- c("\\bC[A-Z]O\\b", "\\bCHIEF\\s\\b[A-Z]+\\b\\sOFFICER\\b")
  c.suite.titles <- get_title_dump(comp.table,c.suite.list)

  write.csv(co.titles,paste0("test-tables/titles-with-CO-", Sys.Date(), ".csv"))
  write.csv(of.titles,paste0("test-tables/titles-with-OF-", Sys.Date(), ".csv"))
  write.csv(to.titles,paste0("test-tables/titles-with-TO-", Sys.Date(), ".csv"))
  write.csv(comma.titles,paste0("test-tables/titles-with-COMMA-", Sys.Date(), ".csv"))
  write.csv(slash.titles,paste0("test-tables/titles-with-SLASH-", Sys.Date(), ".csv"))
  write.csv(ampersand.titles,paste0("test-tables/titles-with-AMPERSAND-", Sys.Date(), ".csv"))
  write.csv(and.titles,paste0("test-tables/titles-with-AND-", Sys.Date(), ".csv"))
  write.csv(ordinal.titles, paste0("test-tables/titles-with-ordinal-numbers-", Sys.Date(), ".csv"))
  write.csv(c.suite.titles, paste0("test-tables/titles-with-c-suite-", Sys.Date(), ".csv"))
}


