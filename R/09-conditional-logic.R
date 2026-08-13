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
#' @param comp.data A Part VII compensation data frame.
conditional_logic <- function(comp.data)
{
  
  df <- comp.data
  
  #manipulations here
  
  df %>%
    clean_up_ceos() %>%
    director_correction() -> df
  
  cat( "[OK] conditional logic step complete\n")
  
  return(df)
}





#' @title 
#' clean up ceos function
#' 
#' @description 
#' removes all duplicate instances of ceos within a single org
#' 
#' @export
#' @param comp.data A Part VII compensation data frame.
clean_up_ceos <- function(comp.data){
  df <- comp.data
  
  df = df[!duplicated(df[, c("ein", "dtk.name", "dtk.title", "title.standard")]) | 
            df$title.standard != "CEO", ] #remove duplicates of ceos
  #but potential issues with multiple titles
  
  
  return(df)
}


#' @title 
#' director correction function
#' 
#' @description 
#' confirm all directors are properly mapped 
#' correcting board members "directors" that are not actually directors
#' 
#' @export
#' @param df A Part VII compensation data frame.
director_correction <- function(df)
{
  
  df$title.standard <- ifelse(df$title.v7 == "DIRECTOR" & 
                           df$dtk.indiv.trustee.x == 0 & df$dtk.inst.trustee.x == 0, 
                           df$title.v7, df$title.standard)
  
  return(df)
}
