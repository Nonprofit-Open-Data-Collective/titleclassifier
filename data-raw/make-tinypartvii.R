# Regenerate the bundled demo dataset `tinypartvii` from a current NCCS Part VII
# file so it carries the MODERN schema the pipeline expects (OBJECTID, EIN2,
# RETURN_TYPE, ORG_NAME_L1/L2, F9_07_COMP_DTK_*). The previous tinypartvii used
# an old schema and the current pipeline errored on it.
#
# Run from the package root. Needs a local copy of the ~1 GB 2023 table
# (env TC_PARTVII_2023 or a path produced by fetch_partvii(2023, dest=...)).

suppressWarnings( suppressMessages({ library(data.table) }) )

CACHE_FILE <- Sys.getenv(
  "TC_PARTVII_2023",
  unset = "C:/Users/jdlec/AppData/Local/Temp/claude/C--Users-jdlec-Dropbox--Personal--00---URBAN-00-GITHUB-titleclassifier/57f55c2d-d2e1-4a64-8b05-84b169023373/scratchpad/efdata-cache/F9-P07-T01-COMPENSATION-2023.CSV"
)
stopifnot( file.exists( CACHE_FILE ) )

N_ORGS <- 10000L
SEED   <- 1234L

cat( "• reading full 2023 table ...\n" )
d <- data.table::fread( CACHE_FILE, colClasses = "character",
                        showProgress = FALSE, data.table = FALSE )

set.seed( SEED )
ids  <- sample( unique( d$OBJECTID ), N_ORGS )
tinypartvii <- d[ d$OBJECTID %in% ids, , drop = FALSE ]
tinypartvii <- tinypartvii[ order( tinypartvii$OBJECTID ), , drop = FALSE ]
rownames( tinypartvii ) <- NULL

cat( "  tinypartvii:", nrow(tinypartvii), "rows,", ncol(tinypartvii), "cols, from",
     length(ids), "orgs\n" )

save( tinypartvii, file = "data/tinypartvii.rda", compress = "xz" )
cat( "✔ wrote data/tinypartvii.rda (",
     round( file.size("data/tinypartvii.rda")/1e6, 2 ), "MB )\n", sep="" )
