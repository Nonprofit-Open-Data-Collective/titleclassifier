# titleclassifier regression check.
#
# Re-runs the full 8-step pipeline on the frozen demo sample using the pinned
# crosswalks and asserts the output is cell-for-cell identical to the committed
# reference. This is the guardrail for the WS-1 audit/streamline and WS-2 fetch
# work: any unintended behavior change fails here with a readable diff.
#
# Usage (from anywhere):
#   Rscript tests/regression/check-regression.R
# Exit code 0 = identical; 1 = differences (or setup problem).

suppressWarnings( suppressMessages({ library(data.table); library(dplyr) }) )

# locate helpers relative to this script, fall back to cwd
args <- commandArgs( trailingOnly = FALSE
)
file_arg <- sub( "^--file=", "", args[ grepl( "^--file=", args ) ] )
here <- if( length( file_arg ) ) dirname( normalizePath( file_arg ) ) else getwd()
helpers <- file.path( here, "regression-helpers.R" )
if( ! file.exists( helpers ) ) helpers <- file.path( "tests", "regression", "regression-helpers.R" )
source( helpers )

ROOT <- tc_pkg_root()
setwd( ROOT )
tc_load_package( ROOT )

DEMO_DIR <- file.path( ROOT, "data-raw", "demo" )
demo_path <- file.path( DEMO_DIR, "partvii-demo-2023.csv" )
ref_path  <- file.path( DEMO_DIR, "partvii-demo-2023-REFERENCE.csv" )

fail <- function( ... ){ cat( "[x] REGRESSION FAIL:", ..., "\n" ); quit( status = 1L ) }

if( ! file.exists( demo_path ) ) fail( "missing demo file:", demo_path )
if( ! file.exists( ref_path  ) ) fail( "missing reference file:", ref_path )

# --- run the pipeline on the pinned inputs -----------------------------------
xwalks <- tc_load_crosswalks( DEMO_DIR )
demo   <- data.table::fread( demo_path, colClasses = "character",
                             showProgress = FALSE, data.table = FALSE )
cur    <- tc_run_pipeline( demo, xwalks )

# --- serialize both sides the SAME way (as.character) for an exact compare ----
cur_chr <- as.data.frame( lapply( cur, as.character ), stringsAsFactors = FALSE,
                          check.names = FALSE )
ref_chr <- utils::read.csv( ref_path, colClasses = "character",
                            na.strings = character(0), check.names = FALSE )

# normalize NA -> "" on both sides so representation is comparable
na_blank <- function( df ){ df[] <- lapply( df, function(x){ x[is.na(x)] <- ""; x } ); df }
cur_chr <- na_blank( cur_chr )
ref_chr <- na_blank( ref_chr )

# --- structural checks --------------------------------------------------------
if( ! identical( dim( cur_chr ), dim( ref_chr ) ) )
  fail( sprintf( "dimensions differ: current %dx%d vs reference %dx%d",
                 nrow(cur_chr), ncol(cur_chr), nrow(ref_chr), ncol(ref_chr) ) )

if( ! identical( names( cur_chr ), names( ref_chr ) ) )
{
  only_cur <- setdiff( names(cur_chr), names(ref_chr) )
  only_ref <- setdiff( names(ref_chr), names(cur_chr) )
  cat( "column sets/order differ.\n" )
  if( length(only_cur) ) cat( "  only in current:  ", paste(only_cur, collapse=", "), "\n" )
  if( length(only_ref) ) cat( "  only in reference: ", paste(only_ref, collapse=", "), "\n" )
  fail( "column mismatch" )
}

# --- cell-for-cell diff -------------------------------------------------------
n_diff <- 0L
report <- character( 0 )
for( col in names( ref_chr ) )
{
  d <- which( cur_chr[[col]] != ref_chr[[col]] )
  if( length( d ) )
  {
    n_diff <- n_diff + length( d )
    for( r in head( d, 5 ) )
      report <- c( report, sprintf( "  [row %d | %s] current=%s  reference=%s",
                                    r, col, dQuote(cur_chr[[col]][r]), dQuote(ref_chr[[col]][r]) ) )
  }
}

if( n_diff > 0 )
{
  cat( "differing cells:", n_diff, "(showing up to 5 per column)\n" )
  cat( paste( head( report, 60 ), collapse = "\n" ), "\n" )
  fail( n_diff, "cell(s) changed vs reference" )
}

# --- summary hash cross-check against the manifest ----------------------------
md5 <- digest::digest( tc_frame_image( cur ), algo = "md5" )
cat( "[OK] REGRESSION PASS - output identical to reference\n" )
cat( "  rows:", nrow(cur), " cols:", ncol(cur), " md5:", md5, "\n" )
quit( status = 0L )
