###---------------------------------------------------
###   UTILITY FUNCTIONS 
###---------------------------------------------------

# make the pipe operator 
# available through the
# magrittr package

#' @importFrom magrittr "%>%"


hash_row <- function(row) {
  row_string <- paste(as.character(row), collapse = "")
  digest::digest(row_string, algo = "md5", serialize = FALSE)
}

get_row_id <- function(df){
  row_hash <- apply( df, 1, hash_row )
  row_id <- paste0( "PID-", row_hash )
  return(row_id)
}

get_googlesheets_status_codes <- function(){ 

  # load current status codes from google sheets
  googlesheets4::gs4_deauth()
  
  SSID <- "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M"
  
  gs_status_codes <- 
    googlesheets4::with_gs4_quiet({
    
      googlesheets4::read_sheet( ss=SSID, 
                                  sheet="status-codes", 
                                  range="A:B",
                                  col_types = "c" ) 
                                  
    })
  
  gs_status_codes <<- gs_status_codes
  invisible( gs_status_codes)
}
  

get_googlesheets_title_xwalk <- function(){

  # read from google sheets
  googlesheets4::gs4_deauth()
  
  SSID <- "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M"
  
  gs_title_xwalk <- 
    googlesheets4::with_gs4_quiet({
      googlesheets4::read_sheet( 
          ss=SSID, 
          sheet="title-standardization", 
          range="A:D",
          col_types = "c" )  # c = character
    })    
    
  gs_title_xwalk[ is.na( gs_title_xwalk ) ] <- ""
  variant <- gs_title_xwalk$title.variant
  dupes <- variant[ duplicated( variant ) ] %>% sort()
  
  if( length(dupes) > 0 )
  { 
    print( "There are duplicate title variants: ")
    print( paste0( dupes, collapse=" ;; " ) ) 
  }
  
  gs_title_xwalk <- unique( gs_title_xwalk )
  gs_title_xwalk <- dplyr::filter( gs_title_xwalk, ! duplicated( title.variant ) )
  
  gs_title_xwalk <<- gs_title_xwalk
  invisible( gs_title_xwalk )
}


get_googlesheets_title_taxonomy <- function(){

    googlesheets4::gs4_deauth()
    google.id <- "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M"
    
    gs_title_taxonomy <- 
      googlesheets4::with_gs4_quiet({
      
        googlesheets4::read_sheet(  google.id,
                                    sheet="title-taxonomy", range="A:T",
                                    col_types = "c" )  # c = character
      })
      
    gs_title_taxonomy[ is.na( gs_title_taxonomy ) ] <- ""
    
    gs_title_taxonomy <<- gs_title_taxonomy
    
    invisible( gs_title_taxonomy )
}


to_boolean <- function(x)
{
  x[ x == "X" | x == "x" ] <- 1
  x[ x == "" ] <- 0
  x <- as.numeric(x)
  return(x)
}


# identify sample cases for function testing purposes
#   get_test_cases( condition="/", x=title.v3 )

get_test_cases <- function( condition, x=NULL, n=250 )
{
  if( is.null(x) )
  { x <- tinypartvii$F9_07_COMP_DTK_TITLE }
  x <- grep( condition, x, value=T )
  if( n > length(x) ){ n <- length(x) }
  x <- sample( x, n )
  return( x )
}


