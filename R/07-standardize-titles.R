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
  
  # MOVED TO SPELLING STEP
  # TitleTxt = comp.data[[title]]
  # TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  # TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  # comp.data[[title]] <- TitleTxt
  
  # manipulations with google sheets
  #TitleTxt <- stand_titles(TitleTxt) #this will be replaced with the mapping
  googlesheets4::gs4_deauth()
  df.standard <- googlesheets4::read_sheet( "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M", 
                                            sheet="title-standardization", range="A:B",
                                            col_types = "c" )  # c = character
  df.standard[ is.na( df.standard ) ] <- ""
  df.standard <- unique( df.standard )
  comp.data <- merge( comp.data, df.standard, by.x=title, by.y="title.variant", all.x=T )

  # comp.data$TitleTxt7 <- TitleTxt
  
  print("standardize titles step complete")
  return(comp.data)
}
