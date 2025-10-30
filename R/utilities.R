###---------------------------------------------------
###   UTILITY FUNCTIONS 
###---------------------------------------------------

# make the pipe operator 
# available through the
# magrittr package

#' @importFrom magrittr "%>%"


classify_year <- function( year ){
  fn <- paste0( "data/F9-P07-T01-COMPENSATION-", year, ".csv" )
  d <- fread( fn  )

  start_time <- Sys.time()

  d_res <- 
    d  %>% 
    standardize_df() %>% 
    remove_dates() %>% 
    standardize_conj() %>% 
    split_titles() %>% 
    standardize_spelling() %>% 
    gen_status_codes() %>% 
    standardize_titles() %>% 
    categorize_titles()

  end_time <- Sys.time()
  end_time - start_time

  fnout <- paste0( "done/F9-P07-T01-", year, "-CLASSIFIED.csv" )
  write.csv( d_res, fnout, row.names=F )

  return(d_res)
}

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
  
  gs_status_codes <- as.data.frame(gs_status_codes)
  # gs_status_codes <<- gs_status_codes
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
  
  gs_title_xwalk <- as.data.frame(gs_title_xwalk)
  # gs_title_xwalk <<- gs_title_xwalk
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

    gs_title_taxonomy <- as.data.frame(gs_title_taxonomy)    
    # gs_title_taxonomy <<- gs_title_taxonomy
    invisible( gs_title_taxonomy )
}


check_taxonomy_xwalk <- function(){
  xwalk <- get_googlesheets_title_xwalk()
  taxonomy <- get_googlesheets_title_taxonomy()

  variant <- xwalk[["title.variant"]]
  dupes <- variante[ duplicated(variant) ]

  if( length(dupes) == 0 ){ cat("No duplicated variantes!\n" ) }
  if( length(dupes) > 0 ){ 
    cat( "There are duplicated variants:\n" )
    cat( paste0( dupes, collapse=";;" ), sep="\n\n" ) 
  }

  title1 <- xwalk[["title.standard"]]
  title2 <- xwalk[["title.standard"]]
  miss  <- setdiff( title1, title2 )
  undef <- setdiff( title2, title1 )
  if( length(miss) > 0 ){
    cat( paste0( "The following titles are missing from the taxonomy" ), sep="\n" )
    cat( paste0( miss, collapse=";;" ), sep="\n\n" )
  }
  if( length(undef) > 0 ){
    cat( paste0( "There is no standard version in the crosswalk:" ), sep="\n" )
    cat( paste0( undef, collapse=";;" ), sep="\n\n" )
  }  
  
  if( length(miss) == 0 & length(undef) == 0 )
  { cat( "The crosswalk and taxonomy are aligned!\n" ) }

  invisible( c(miss,undef) )
}


create_title_variant_report <- function( df ){
  xwalk <- get_googlesheets_title_xwalk()
  variants <- xwalk[["title.variant"]]
  title.v7 <- dd[["title.v7"]]
  x <- title.v7[ title.v7 %in% variants ]
  f <- factor( x, levels=variants )
  t <- table(f) |> sort( decr=T ) |> as.data.frame()
  names(t) <- c("title.variant","freq")
  t$freq <- format( t$freq, big.mark="," )
  t <- merge( t, xwalk[c("title.variant","title.standard")] )
  k <- knitr::kable(head(t,100))
  cat("The 100 most common title variants:\n")
  print(k)
  invisible(t)
}

# see which variants not used
# k <- create_title_variant_report(dd)
# k$freq <- trimws(k$freq)
# k[ k$freq == "0" , ]



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


