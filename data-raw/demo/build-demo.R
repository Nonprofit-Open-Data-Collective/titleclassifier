# Build the regression demo dataset + frozen golden reference.
#
# WS-0 of the refresh. Run ONCE (or whenever we intentionally re-baseline).
# Produces, in data-raw/demo/:
#   - xwalk-*.csv                     pinned snapshots of the 3 Google-Sheet tabs
#   - partvii-demo-2023.csv           deterministic ~few-thousand-row raw sample
#   - partvii-demo-2023-REFERENCE.csv frozen output of the current pipeline
#   - reference-manifest.txt          dims / NA-rates / hash of the reference
#
# The 1 GB source file is NOT committed; point CACHE_FILE at a local copy
# (produced by fetch_partvii(2023, dest=...)).

suppressWarnings( suppressMessages({ library(data.table); library(dplyr) }) )

# --- locate helpers + package -------------------------------------------------
this_dir <- tryCatch( dirname( normalizePath( sys.frame(1)$ofile ) ),
                      error = function(e) getwd() )
source( file.path( "tests", "regression", "regression-helpers.R" ) )   # run from pkg root
ROOT <- tc_pkg_root()
setwd( ROOT )
tc_load_package( ROOT )

DEMO_DIR   <- file.path( ROOT, "data-raw", "demo" )
if( ! dir.exists( DEMO_DIR ) ) dir.create( DEMO_DIR, recursive = TRUE )

# local (uncommitted) copy of the full 2023 table
CACHE_FILE <- Sys.getenv(
  "TC_PARTVII_2023",
  unset = "C:/Users/jdlec/AppData/Local/Temp/claude/C--Users-jdlec-Dropbox--Personal--00---URBAN-00-GITHUB-titleclassifier/57f55c2d-d2e1-4a64-8b05-84b169023373/scratchpad/efdata-cache/F9-P07-T01-COMPENSATION-2023.CSV"
)
stopifnot( file.exists( CACHE_FILE ) )

TARGET_ROWS <- 4000L   # "a few thousand cases" (whole orgs kept intact)
SEED        <- 1234L

# --- 1. pin the crosswalks ----------------------------------------------------
tc_pin_crosswalks( DEMO_DIR )
xwalks <- tc_load_crosswalks( DEMO_DIR )

# --- 2. deterministic whole-org sample ---------------------------------------
cat( "• reading full 2023 table ...\n" )
d <- data.table::fread( CACHE_FILE, colClasses = "character",
                        showProgress = FALSE, data.table = FALSE )
cat( "  full table:", nrow(d), "rows,", ncol(d), "cols\n" )

set.seed( SEED )
ids  <- unique( d$OBJECTID )
ids  <- sample( ids )                                  # shuffle orgs
rpo  <- table( factor( d$OBJECTID, levels = ids ) )    # rows per org, in shuffled order
keep <- ids[ cumsum( as.integer( rpo ) ) <= TARGET_ROWS ]
if( length( keep ) == 0 ) keep <- ids[ 1 ]             # safety
demo <- d[ d$OBJECTID %in% keep, , drop = FALSE ]
demo <- demo[ order( demo$OBJECTID ), , drop = FALSE ]

cat( "  demo sample:", nrow(demo), "rows from", length(keep), "orgs\n" )
demo_path <- file.path( DEMO_DIR, "partvii-demo-2023.csv" )
utils::write.csv( demo, demo_path, row.names = FALSE, na = "" )
cat( "✔ wrote", demo_path, "\n" )

# --- 3. run current pipeline -> frozen reference ------------------------------
cat( "• running pipeline ...\n" )
ref <- tc_run_pipeline( demo, xwalks )
ref_path <- file.path( DEMO_DIR, "partvii-demo-2023-REFERENCE.csv" )
# write everything as character for a stable on-disk image
ref_chr <- as.data.frame( lapply( ref, as.character ), stringsAsFactors = FALSE,
                          check.names = FALSE )
utils::write.csv( ref_chr, ref_path, row.names = FALSE, na = "" )
cat( "✔ wrote", ref_path, "\n" )

# --- 4. manifest --------------------------------------------------------------
img  <- tc_frame_image( ref )
na_rate <- sapply( ref, function( x ) round( mean( is.na( x ) ), 4 ) )
man <- c(
  paste0( "titleclassifier regression reference" ),
  paste0( "source_file: ", basename( CACHE_FILE ) ),
  paste0( "seed: ", SEED, "   target_rows: ", TARGET_ROWS ),
  paste0( "demo_rows: ", nrow(demo), "   demo_orgs: ", length(keep) ),
  paste0( "reference_rows: ", nrow(ref), "   reference_cols: ", ncol(ref) ),
  paste0( "reference_hash_md5: ", digest::digest( img, algo = "md5" ) ),
  paste0( "title.standard_NA_rate: ",
          round( mean( is.na( ref$title.standard ) ), 4 ) ),
  "",
  "per-column NA rate:",
  paste0( "  ", names(na_rate), ": ", na_rate )
)
man_path <- file.path( DEMO_DIR, "reference-manifest.txt" )
writeLines( man, man_path )
cat( "✔ wrote", man_path, "\n" )
cat( "\nDONE. reference md5:", digest::digest( img, algo = "md5" ), "\n" )
