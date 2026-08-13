# Shared helpers for the titleclassifier regression baseline.
#
# Both the demo/reference builder (data-raw/demo/build-demo.R) and the diff
# harness (tests/regression/check-regression.R) source this file so they run
# the *identical* pipeline against *identical* pinned crosswalks. That is what
# makes a diff meaningful: any change in output must come from a code change,
# not from a moving Google Sheet or a different code path.

# ---------------------------------------------------------------------------
# locate the package root regardless of where the script is launched from
# ---------------------------------------------------------------------------
tc_pkg_root <- function( start = getwd() )
{
  d <- normalizePath( start, winslash = "/", mustWork = FALSE )
  for( i in 1:8 )
  {
    if( file.exists( file.path( d, "DESCRIPTION" ) ) &&
        dir.exists(  file.path( d, "R" ) ) )
    { return( d ) }
    parent <- dirname( d )
    if( identical( parent, d ) ) break
    d <- parent
  }
  stop( "tc_pkg_root(): could not locate package root from ", start )
}

# ---------------------------------------------------------------------------
# load the current working-tree package code + data objects WITHOUT installing,
# so the harness always reflects the code as it is right now
# ---------------------------------------------------------------------------
tc_load_package <- function( root = tc_pkg_root() )
{
  suppressWarnings( suppressMessages({
    library( dplyr ); library( magrittr ); library( hunspell )
    library( googlesheets4 ); library( digest ); library( data.table )
  }) )

  # step files first (numbered), then the rest
  rfiles <- list.files( file.path( root, "R" ), pattern = "[.]R$", full.names = TRUE )
  rfiles <- rfiles[ order( basename( rfiles ) ) ]
  for( f in rfiles ) sys.source( f, envir = globalenv() )

  # package data objects (number.words, likely.titles, date.words, ...) are
  # lazy-loaded when installed; when sourcing raw files we load them explicitly
  for( rda in list.files( file.path( root, "data" ), pattern = "[.]rda$", full.names = TRUE ) )
    load( rda, envir = globalenv() )

  invisible( TRUE )
}

# ---------------------------------------------------------------------------
# pinned crosswalks: snapshot to CSV and load back in the exact shape the
# pipeline's gs_* arguments expect
# ---------------------------------------------------------------------------
tc_xwalk_paths <- function( dir )
{
  list(
    status   = file.path( dir, "xwalk-status-codes.csv" ),
    xwalk    = file.path( dir, "xwalk-title-standardization.csv" ),
    taxonomy = file.path( dir, "xwalk-title-taxonomy.csv" )
  )
}

# pull the three tabs live from Google Sheets (via the package's own loaders)
# and write them to CSV so the reference is pinned, not live
tc_pin_crosswalks <- function( dir )
{
  if( ! dir.exists( dir ) ) dir.create( dir, recursive = TRUE, showWarnings = FALSE )
  p <- tc_xwalk_paths( dir )

  status   <- get_googlesheets_status_codes()
  xwalk    <- get_googlesheets_title_xwalk()
  taxonomy <- get_googlesheets_title_taxonomy()

  utils::write.csv( status,   p$status,   row.names = FALSE, na = "" )
  utils::write.csv( xwalk,    p$xwalk,    row.names = FALSE, na = "" )
  utils::write.csv( taxonomy, p$taxonomy, row.names = FALSE, na = "" )

  cat( "[OK] pinned crosswalks to", dir, "\n" )
  invisible( p )
}

# read the pinned CSVs back as all-character frames with empty strings preserved
tc_load_crosswalks <- function( dir )
{
  p <- tc_xwalk_paths( dir )
  for( f in unlist( p ) )
    if( ! file.exists( f ) )
      stop( "tc_load_crosswalks(): missing pinned crosswalk: ", f,
            "\nRun tc_pin_crosswalks() first.", call. = FALSE )

  rd <- function( f ) utils::read.csv( f, colClasses = "character",
                                       na.strings = character(0),
                                       check.names = FALSE )
  list( status = rd( p$status ), xwalk = rd( p$xwalk ), taxonomy = rd( p$taxonomy ) )
}

# ---------------------------------------------------------------------------
# the pipeline under test, run with pinned crosswalks
# ---------------------------------------------------------------------------
tc_run_pipeline <- function( raw_df, xwalks )
{
  raw_df |>
    standardize_df() |>
    remove_dates() |>
    standardize_conj() |>
    split_titles() |>
    standardize_spelling() |>
    gen_status_codes(   gs_status_codes   = xwalks$status   ) |>
    standardize_titles( gs_title_xwalk    = xwalks$xwalk    ) |>
    categorize_titles(  gs_title_taxonomy = xwalks$taxonomy )
}

# ---------------------------------------------------------------------------
# stable string image of a result frame for exact diffing + hashing
# ---------------------------------------------------------------------------
tc_frame_image <- function( df )
{
  df <- as.data.frame( df )
  df <- df[ , order( names( df ) ), drop = FALSE ]            # column-order independent
  cols <- lapply( df, function( x ) format( x, trim = TRUE, scientific = FALSE ) )
  apply( do.call( cbind, cols ), 1, paste, collapse = "" )  # one string per row
}
