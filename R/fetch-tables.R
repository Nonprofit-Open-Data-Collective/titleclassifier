# Fetch Part VII compensation tables directly from the NCCS efile store
# fetch-tables.R
#
# Slim, self-contained port of the panel990 download/read helpers so that a
# user can pull a Form 990 Part VII (F9-P07-T01-COMPENSATION) table by tax year
# and feed it straight into standardize_df() without hand-downloading the ~1 GB
# S3 CSVs.


# default public NCCS efile v2 store
.nccs_efile_root <- function()
{
  "https://nccs-efile.s3.us-east-1.amazonaws.com/public/efile_v2_0/"
}

# canonical Part VII compensation table id (file basename, no year/extension)
.partvii_table_id <- function()
{
  "F9-P07-T01-COMPENSATION"
}

# build the download URL / file name for one table + year
#   .table_filename( 2023 )  -->  "F9-P07-T01-COMPENSATION-2023.CSV"
.table_filename <- function( year, table = .partvii_table_id() )
{
  paste0( table, "-", year, ".CSV" )
}

.table_url <- function( year, table = .partvii_table_id(),
                        root = .nccs_efile_root() )
{
  paste0( root, .table_filename( year, table ) )
}



#' @title
#' Fetch Part VII compensation tables from the NCCS efile store
#'
#' @description
#' Downloads one or more years of the Form 990 Part VII compensation table
#' (`F9-P07-T01-COMPENSATION-<year>.CSV`) from the public NCCS efile store to a
#' local cache directory. Files already present are reused unless
#' `overwrite = TRUE`. Returns the local file paths (invisibly) so the result
#' can be piped into `read_partvii()`.
#'
#' Each yearly file is large (~1 GB), so downloads are cached on disk.
#'
#' @param years  Integer vector of tax years (e.g. `2023` or `2020:2023`).
#' @param dest   Local cache directory. Created if it does not exist.
#' @param table  Table id (file basename). Defaults to the Part VII
#'   compensation table; exposed so the same machinery can fetch sibling tables.
#' @param root   Base URL of the efile store. Defaults to the public NCCS v2 store.
#' @param overwrite  Re-download even if the file already exists. Default `FALSE`.
#' @param retry_max  Number of download attempts per file. Default `3`.
#' @param timeout    Per-file download timeout in seconds. Default `600`.
#' @param verbose    Print progress messages. Default `TRUE`.
#'
#' @return A character vector of local file paths (one per requested year),
#'   returned invisibly.
#'
#' @examples
#' \dontrun{
#' f  <- fetch_partvii( 2023 )
#' d  <- read_partvii( f )
#' df <- standardize_df( d )
#' }
#'
#' @export
fetch_partvii <- function( years,
                           dest = "efdata",
                           table = .partvii_table_id(),
                           root = .nccs_efile_root(),
                           overwrite = FALSE,
                           retry_max = 3L,
                           timeout = 600,
                           verbose = TRUE )
{
  if( ! dir.exists( dest ) )
  { dir.create( dest, recursive = TRUE, showWarnings = FALSE ) }

  # download.file() shares a global timeout option; set and restore it
  old.timeout <- getOption( "timeout" )
  on.exit( options( timeout = old.timeout ), add = TRUE )
  options( timeout = max( timeout, old.timeout ) )

  paths <- character( length( years ) )

  for( i in seq_along( years ) )
  {
    year   <- years[ i ]
    fn     <- .table_filename( year, table )
    url    <- .table_url( year, table, root )
    fpath  <- file.path( dest, fn )
    paths[ i ] <- fpath

    if( file.exists( fpath ) && ! overwrite )
    {
      if( verbose ){ cat( "- reused ", fn, "\n", sep="" ) }
      next
    }

    ok <- .download_one( url, fpath, retry_max = retry_max, verbose = verbose )

    if( ! ok )
    {
      warning( "failed to download ", fn, " after ", retry_max,
               " attempt(s); removing partial file", call. = FALSE )
      if( file.exists( fpath ) ){ unlink( fpath ) }
      paths[ i ] <- NA_character_
    }
    else if( verbose )
    {
      cat( "[OK] downloaded ", fn, "\n", sep="" )
    }
  }

  invisible( paths )
}


# download a single url to destfile with retries; returns TRUE on success
.download_one <- function( url, destfile, retry_max = 3L, verbose = TRUE )
{
  for( attempt in seq_len( retry_max ) )
  {
    status <- tryCatch(
      {
        utils::download.file( url, destfile = destfile,
                              mode = "wb", quiet = ! verbose )
      },
      error   = function(e){ if( verbose ) cat( "  ! ", conditionMessage(e), "\n", sep="" ); 1L },
      warning = function(w){ if( verbose ) cat( "  ! ", conditionMessage(w), "\n", sep="" ); 1L }
    )

    if( identical( as.integer( status ), 0L ) &&
        file.exists( destfile ) && file.size( destfile ) > 0 )
    { return( TRUE ) }

    if( verbose && attempt < retry_max )
    { cat( "  retrying (", attempt + 1L, "/", retry_max, ") ...\n", sep="" ) }
  }
  return( FALSE )
}



#' @title
#' Read cached Part VII compensation table(s)
#'
#' @description
#' Reads one or more Part VII compensation CSVs (as returned by
#' `fetch_partvii()`) into a single data frame. All columns are read as
#' character to preserve raw IRS values; `standardize_df()` handles type
#' coercion downstream. Multiple files are row-bound.
#'
#' @param path Character vector of local file paths (from `fetch_partvii()`),
#'   or a single directory to read all `*.CSV` files from.
#' @param verbose Print progress messages. Default `TRUE`.
#'
#' @return A data frame with the raw Part VII schema, ready for `standardize_df()`.
#'
#' @examples
#' \dontrun{
#' d <- read_partvii( fetch_partvii( 2023 ) )
#' }
#'
#' @export
read_partvii <- function( path, verbose = TRUE )
{
  # allow a directory argument
  if( length( path ) == 1 && dir.exists( path ) )
  { path <- list.files( path, pattern = "[.]CSV$", full.names = TRUE, ignore.case = TRUE ) }

  path <- path[ ! is.na( path ) ]

  if( length( path ) == 0 )
  { stop( "read_partvii(): no files to read", call. = FALSE ) }

  parts <- vector( "list", length( path ) )

  for( i in seq_along( path ) )
  {
    p <- path[ i ]
    if( ! file.exists( p ) )
    { stop( "read_partvii(): file not found: ", p, call. = FALSE ) }

    if( verbose ){ cat( "- reading ", basename( p ), "\n", sep="" ) }

    parts[[ i ]] <-
      data.table::fread( p, colClasses = "character",
                         showProgress = FALSE, data.table = FALSE )
  }

  d <- if( length( parts ) == 1 ){ parts[[ 1 ]] } else { dplyr::bind_rows( parts ) }

  if( verbose )
  { cat( "[OK] read ", nrow( d ), " rows from ", length( path ),
         " file(s)\n", sep="" ) }

  return( d )
}



#' @title
#' Fetch and read Part VII compensation tables in one call
#'
#' @description
#' Convenience wrapper: `fetch_partvii()` followed by `read_partvii()`. Returns
#' a raw Part VII data frame ready to pipe into `standardize_df()`.
#'
#' @inheritParams fetch_partvii
#'
#' @return A data frame with the raw Part VII schema.
#'
#' @examples
#' \dontrun{
#' df <- get_partvii( 2023 ) |> standardize_df()
#' }
#'
#' @export
get_partvii <- function( years,
                         dest = "efdata",
                         table = .partvii_table_id(),
                         root = .nccs_efile_root(),
                         overwrite = FALSE,
                         retry_max = 3L,
                         timeout = 600,
                         verbose = TRUE )
{
  paths <- fetch_partvii( years, dest = dest, table = table, root = root,
                          overwrite = overwrite, retry_max = retry_max,
                          timeout = timeout, verbose = verbose )
  read_partvii( paths, verbose = verbose )
}
