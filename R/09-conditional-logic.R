#Step 9: Conditional Logic

# 09-conditional-logic.R

#' @title
#' conditional logic function
#'
#' @description
#' TODO formalize the conditional logic rules 
#' (have a couple of them already thought out in categorize titles, but need
#' to migrate them)
#' @export
conditional_logic <- function(comp.data){
  
  df <- comp.data
  
  #manipulations here
  
  df <- clean_up_ceos(df)
  
  print("conditional logic step complete")
  
  return(df)
}





#' @title 
#' clean up ceos function
#' 
#' @description 
#' removes all duplicate instances of ceos within a single org
#' 
#' @export
clean_up_ceos <- function(comp.data){
  df <- comp.data
  
  df = df[!duplicated(df[, c("ein", "dtk.name", "dtk.title", "title.standard")]) | 
            df$title.standard != "CEO", ] #remove duplicates of ceos
  #but potential issues with multiple titles
  
  
  return(df)
}