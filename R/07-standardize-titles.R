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
standardize_titles <- function(comp.data, title = "TitleTxt6"){
  TitleTxt = comp.data[[title]]
  
  # manipulations with google sheets
  #TitleTxt <- stand_titles(TitleTxt) #this will be replaced with the mapping
  
  TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  
  comp.data$TitleTxt7 <- TitleTxt
  
  print("standardize titles step complete")
  
  return(comp.data)
}
