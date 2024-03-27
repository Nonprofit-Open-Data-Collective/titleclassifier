#Step 6: Generate Status Codes

# 06-gen-status-codes.R


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
#' @export
gen_status_codes <- function( comp.data, title="TitleTxt5" )
{
  
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
  numbers <- paste0( "\\b", number.words, "\\b", collapse="|" )
  TitleTxt <- gsub( numbers, "", TitleTxt ) 
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  
  # use TitleTxt6 moving forward b/c
  # passing the df through flag steps
  title <- "TitleTxt6"
  comp.data[[title]] <- TitleTxt

  # don't remove 'regional' because 
  # it changes the title meaning
  
  # drop ex-officio? 
  # sometimes is only title
  
  comp.data <- 
    comp.data %>% 
    flag_and_keep(    s.code="EXOFFICIO"  )  %>% 
    flag_and_remove(  s.code="FORMER"     )  %>% 
    flag_and_remove(  s.code="FOUNDER"    )  %>%
    flag_and_remove(  s.code="FUTURE"     )  %>%
    flag_and_remove(  s.code="INTERIM"    )  %>%
    flag_and_remove(  s.code="OUTGOING"   )  %>%
    flag_and_remove(  s.code="PARTIAL"    )  %>% 
    flag_and_remove(  s.code="SCHED O"    )  %>%  
    flag_and_remove(  s.code="AT LARGE"   )  %>%  
    flag_and_keep(    s.code="REGIONAL"   )    
  
  ##  sanity check:  
  ##  if FORMER.X and FUTURE.X both checked 
  ##  it's likely 'from ... until': recode as FORMER.X only
  comp.data$FUTURE.X[ comp.data$FUTURE.X == 1 & comp.data$FORMER.X == 1 ] <- 0 

  x <- comp.data[[title]]
  
  # fix exoffcio : remove at end if not the only title
  x <- gsub( "EXOFFICIO M$", "EXOFFICIO BOARD MEMBER", x )
  x <- gsub( "(.*) EXOFFICIO$", "\\1", x )
  
  # remove double regionals
  x <- gsub( "REGIONAL REGIONAL", "REGIONAL", x )
  
  # remove non-voting
  x <- gsub( "NON-VOTING", "", x )
  x <- gsub( "NON VOTING", "", x )
  
  # clean up empty parentheses
  x <- gsub(  "\\(\\s{0,3}\\)",  "",  x )
  x <- gsub(  "\\b\\(",          "",  x )
  x <- gsub(  "\\s\\(",         " ",  x )
  x <- gsub(  "\\)\\b",          "",  x )
  x <- gsub(  "\\)\\s",         " ",  x )
  x <- trimws( x )
  
  # clean up hashes
  x <- gsub( "\\bCO - ", "CO-", x )
  x <- gsub( "\\bCO -", "CO-", x )
  x <- gsub( "\\bCO- {1,3}", "CO-", x )
  x <- gsub( "-$", "", x )
  x <- gsub( "-", " ", x )
  x <- gsub( "CO ", "CO-", x )
  x <- gsub(  "\\s{2,4}",  " ",  x )
  x <- trimws( x )

  # clean up trailing ands
  x <- gsub( " AND$", "", x )
  
  comp.data[[title]] <- x
  
  cat( "✔ generate status codes step complete\n" )

  return( comp.data )

}





#' @title create a status code flag and remove string from title
#' 
#' @description  
#' Search for variants of a status code (stored in google sheets),
#' create a boolean flag in the dataset, and remove the status qualifier from the title.
#' The flag variable is named SCODE.X (FORMER.X, INTERIM.X, etc.). 
#' 
#' In cases where the status is the full title (e.g. 'ex officio') replace the variant
#' with the standardized version and keep it. 
#' 
#' @param df A compensation dataframe
#' @param title Which version of the title string to use (defaults to "TitleTxt6")
#' @param df.status The status variant dataframe loaded from google sheets
#' @param s.code Any of the unique status.qualifier strings from df.status (defaults to "FORMER")
#' 
#' @examples
#' x <- 
#'   c( "IMMEDIATE PAST CHAIR",
#'      "FORMER CEO (EXIT 12/31/17)" 
#'      "PAST PRESIDENT",
#'      "OUTGOING CEO",
#'      "PRESIDENT ELECT",
#'      "BOARD MEMBER (START OCT)",
#'      "TREASURER - BEGINNING 8/2018",
#'      "DIRECTOR (AS OF 2/22/18)",
#'      "INTERIM PRES",
#'      "ACTING DIRECTOR",
#'      "PAST INTERIM PRESIDENT" )
#' 
#' remove_date( x )
#' remove_date(x) %>% remove_status( s.code="FORMER" )
#' remove_date(x) %>% remove_status( s.code="FUTURE" )
#' remove_date(x) %>% remove_status( s.code="INTERIM" )
#' 
#' @export
flag_and_remove <- function( df, title="TitleTxt6", s.code="FORMER" )
{
  variants <- get_variants( s.code )
  df <- add_status_flag( df, title, s.code, variants )
  df[[title]] <- remove_status( df[[title]], variants )
  return( df )
}




#' @title create a status code flag and KEEP the standardized version of the string
#' 
#' @description  
#' Search for variants of a status code (stored in google sheets),
#' create a boolean flag in the dataset, and replace the status variant
#' in the title with the standardized version.
#' The flag variable is named SCODE.X (e.g. REGIONAL.X). 
#' 
#' @param df A compensation dataframe
#' @param title Which version of the title string to use (defaults to "TitleTxt6")
#' @param s.code Any of the unique status.qualifier strings from df.status (e.g. "REGIONAL")
#' 
#' @export
flag_and_keep <- function( df, title="TitleTxt6", s.code )
{
  variants <- get_variants( s.code )
  df <- add_status_flag( df, title, s.code, variants )
  x <- df[[title]]
  df[[title]] <- standardize_status( x, s.code, variants )
  return( df )
}



#' @title retrieve all status variants for a unique status code
#' 
#' @description  
#' Search for variants of a status code (stored in google sheets),
#' and combine them all into a single regex search string separated by OR.
#' 
#' @param s.code Any of the unique status.qualifier strings from df.status ("FUTURE","FORMER","INTERIM",etc)
#' 
#' @export
get_variants <- function( s.code )
{ 
  # load current status codes from google sheets
  googlesheets4::gs4_deauth()
  status.df <- 
    googlesheets4::read_sheet( "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M", 
                                sheet="status-codes", 
                                range="A:B",
                                col_types = "c" )  
  
  # collapse all variants into regex OR statement 
  # \\b = regex word boundary
  v <- status.df$status.variant[ status.df$status.qualifier == s.code ]
  search.terms <- paste0( "\\b", v, "\\b", collapse="|" )  
  
  return( search.terms )
}





#' @title add a boolean status flag to the compensation dataframe
#' 
#' @description  
#' Search for variants of a status code and add a boolean flag
#' to the data frame, 1 if any status variant is present in the tite, 
#' 0 otherwise.
#' 
#' @param df A compensation dataframe
#' @param title Which version of the title string to use (defaults to "TitleTxt6")
#' @param s.code Any of the unique status.qualifier strings from df.status (e.g. "FORMER")
#' 
#' @export
add_status_flag <- function( df, title, s.code, variants )
{
  x <- df[[title]]
  # create a flag if there are any matches
  df[ paste0( gsub(" ",".",s.code), ".X" ) ] <- grepl( variants, x )
  return( df )
}





#' @title remove a status qualifier from the title
#' 
#' @description  
#' Search for variants of a status code and remove them.
#' If the status code is the entire title (e.g. 'ex officio') 
#' it is replaced with the standardized version of the status code.
#' 
#' @param x Vector of titles
#' @param s.code Any of the unique status.qualifier strings from df.status (e.g. "FORMER")
#' 
#' @export
remove_status <- function( x, variants )
{
  # delete all variants
  x.temp <- x
  x <- gsub( variants, "", x )
  # keep status code if it's the full title
  x[ trimws(x) == "" ] <- x.temp
  return( x )
}  




#' @title replace status variant with the standardized version
#' 
#' @description  
#' Search for variants of a status code and replace the variant
#' with the standardized version of the status code. 
#' 
#' @param x Vector of titles
#' @param s.code Any of the unique status.qualifier strings from df.status 
#' 
#' @export
standardize_status <- function( x, s.code, variants )
{
  # replace all variants with the standardized version
  x <- gsub( variants, s.code, x )
  return( x )
}






