###---------------------------------------------------
###   UTILITY FUNCTIONS 
###---------------------------------------------------

# make the pipe operator 
# available through the
# magrittr package

#' @importFrom magrittr "%>%"


classify_year <- function( year ){
  fn <- paste0( "data/F9-P07-T01-COMPENSATION-", year, ".csv" )
  d <- data.table::fread( fn  )

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

# The status-code, title-standardization, and title-taxonomy crosswalks are
# package data (status.codes, title.xwalk, title.taxonomy), built from the
# CSV tables in data-raw/crosswalks/ by data-raw/crosswalks/build-crosswalks.R.
# They were kept in a Google Sheet until 2026-10-08; the CSVs replace it.

# fetch a package data object whether the package is installed (lazy data) or
# its files were sourced into the global environment (regression harness)
.crosswalk_data <- function( name )
{
  ns <- topenv( environment( .crosswalk_data ) )
  if( isNamespace( ns ) )
  {
    lazy <- getNamespaceInfo( ns, "lazydata" )
    if( exists( name, envir = lazy, inherits = FALSE ) )
    { return( get( name, envir = lazy ) ) }
  }
  get( name, envir = ns )
}


#' @title Load the status-code crosswalk
#'
#' @description Returns the status-qualifier variant crosswalk used by
#'   `gen_status_codes()`: the `status.codes` package data.
#'
#' @return A data frame with columns `status.variant` and `status.qualifier`.
#'
#' @export
get_status_codes <- function(){ .crosswalk_data( "status.codes" ) }


#' @title Load the title-standardization crosswalk
#'
#' @description Returns the title-variant to `title.standard` crosswalk used by
#'   `standardize_titles()`: the `title.xwalk` package data.
#'
#' @return A data frame with columns `title.variant`, `title.standard`,
#'   `strata`, and `strata.label`.
#'
#' @export
get_title_xwalk <- function(){ .crosswalk_data( "title.xwalk" ) }


#' @title Load the title-taxonomy crosswalk
#'
#' @description Returns the `title.standard` to taxonomy crosswalk used by
#'   `categorize_titles()`: the `title.taxonomy` package data.
#'
#' @return A data frame mapping `title.standard` to domain, SOC codes, and
#'   role/hierarchy flags.
#'
#' @export
get_title_taxonomy <- function(){ .crosswalk_data( "title.taxonomy" ) }


#' @title Crosswalk loaders under their former names (deprecated)
#'
#' @description The crosswalks were kept in a Google Sheet until 2026-10-08
#'   and are now package data. These functions return the package data, the
#'   same as [get_status_codes()], [get_title_xwalk()] and
#'   [get_title_taxonomy()]. `refresh = TRUE` no longer reads the sheet; it
#'   warns and returns the package data.
#'
#' @param refresh Ignored; kept so that existing calls keep working.
#'
#' @return A data frame, as returned by the matching `get_*()` function.
#'
#' @name get_googlesheets
NULL

.sheet_retired <- function( refresh )
{
  if( isTRUE( refresh ) )
  {
    warning( "The crosswalk Google Sheet is retired; returning the package data. ",
             "Edit data-raw/crosswalks/*.csv and run build-crosswalks.R instead.",
             call. = FALSE )
  }
}

#' @rdname get_googlesheets
#' @export
get_googlesheets_status_codes <- function( refresh = FALSE ){
  .sheet_retired( refresh ); get_status_codes()
}

#' @rdname get_googlesheets
#' @export
get_googlesheets_title_xwalk <- function( refresh = FALSE ){
  .sheet_retired( refresh ); get_title_xwalk()
}

#' @rdname get_googlesheets
#' @export
get_googlesheets_title_taxonomy <- function( refresh = FALSE ){
  .sheet_retired( refresh ); get_title_taxonomy()
}


check_taxonomy_xwalk <- function(){
  xwalk <- get_title_xwalk()
  taxonomy <- get_title_taxonomy()

  variant <- xwalk[["title.variant"]]
  dupes <- variant[ duplicated(variant) ]

  if( length(dupes) == 0 ){ cat("No duplicated variants!\n" ) }
  if( length(dupes) > 0 ){
    cat( "There are duplicated variants:\n" )
    cat( paste0( dupes, collapse=";;" ), sep="\n\n" )
  }

  title1 <- xwalk[["title.standard"]]
  title2 <- taxonomy[["title.standard"]]
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
  xwalk <- get_title_xwalk()
  variants <- xwalk[["title.variant"]]
  title.v7 <- df[["title.v7"]]
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


