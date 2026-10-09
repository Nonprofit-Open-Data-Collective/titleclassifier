# 09-role-model.R
# The no-checkbox role model used by resolve_roles() for 990-EZ filings
# (FINDINGS.md F-028, F-029). The model ships in inst/extdata/role-model/ and
# is trained by data-raw/partvii-validation/20-role-model-ship.R, which uses
# role_model_features() below so training and prediction share one definition.

.role_classes <- c( "BOARD", "CEO", "OFFICER", "MANAGER", "PROFESSIONAL", "STAFF" )

.role_keywords <- c(
  president = "PRESIDENT", vice = "\\bVICE\\b|\\bVP\\b", chair = "CHAIR",
  secretary = "SECRETARY|\\bSEC\\b", treasurer = "TREASURER|\\bTREAS\\b",
  director = "DIRECTOR", trustee = "TRUSTEE", member = "MEMBER", board = "BOARD",
  executive = "EXECUTIVE|\\bEXEC\\b", ceo = "\\bCEO\\b", chief = "CHIEF",
  officer = "OFFICER", manager = "MANAGER|\\bMGR\\b", coordinator = "COORDINAT",
  administrator = "ADMINISTRAT", founder = "FOUNDER", assistant = "ASSISTANT|\\bASST\\b",
  deputy = "DEPUTY", senior = "SENIOR|\\bSR\\b", governor = "GOVERNOR",
  past = "PAST|FORMER|EMERITUS", ex_officio = "EX.?OFFICIO", at_large = "AT.?LARGE",
  physician = "PHYSICIAN|\\bMD\\b|DOCTOR|SURGEON", nurse = "NURSE|\\bRN\\b",
  counsel = "COUNSEL|ATTORNEY|LAWYER", professor = "PROFESSOR|FACULTY", coach = "COACH",
  pastor = "PASTOR|RECTOR|REVEREND|\\bREV\\b|CLERGY|MINISTER",
  principal = "PRINCIPAL|HEADMASTER|HEAD OF SCHOOL", dean = "\\bDEAN\\b",
  program = "PROGRAM", development = "DEVELOPMENT|FUNDRAIS|ADVANCEMENT",
  finance = "FINANC|CFO|CONTROLLER|COMPTROLLER" )


#' @title
#' features for the no-checkbox role model
#'
#' @description
#' Builds the feature matrix the no-checkbox role model uses, from the output
#' of `categorize_titles()`. It uses only what a 990-EZ also reports:
#' - the title: keyword flags, the crosswalk's level, board role, domain, SOC
#'   major group and match type;
#' - pay and hours;
#' - the person's pay and hours rank and share within the filing;
#' - the filing's make-up: people, paid people, other CEO-level and board
#'   titles, people sharing the title;
#' - status flags.
#'
#' It never reads the Part VII checkboxes.
#'
#' @return A numeric matrix with one row per row of `comp.data`, in the same
#'   order.
#'
#' @export
#' @param comp.data The output of `categorize_titles()`.
role_model_features <- function( comp.data )
{
  x <- data.table::as.data.table( comp.data )
  # a column the input lacks reads as empty
  for( v in c( "title.raw", "title.standard", "emp.level", "board.role", "domain.category", "major.group",
               "title.match", "tot.comp", "tot.hours", "former.x", "interim.x", "co.x", "founder.x",
               "ex.officio.x", "title.count" ) )
  { if( ! v %in% names(x) ) data.table::set( x, j = v, value = rep( NA, nrow(x) ) ) }
  num <- function(v) { v <- suppressWarnings( as.numeric(v) ); v[ is.na(v) ] <- 0; v }
  chr <- function(v) { v <- as.character(v); v[ is.na(v) ] <- ""; v }
  lv  <- function(v, levels) { v <- chr(v); v[ ! v %in% levels ] <- "none"; factor( v, c( levels, "none" ) ) }

  t <- toupper( paste( chr(x$title.raw), chr(x$title.standard) ) )
  kw <- vapply( .role_keywords, function(p) as.integer( grepl( p, t, perl = TRUE ) ), integer( nrow(x) ) )
  if( is.null( dim(kw) ) ) kw <- matrix( kw, nrow = nrow(x) )
  colnames(kw) <- paste0( "kw_", names(.role_keywords) )

  lvl  <- chr( x$emp.level ); brole <- chr( x$board.role )
  x[ , `:=`( .comp = log1p( pmax( num(tot.comp), 0 ) ), .hours = pmax( num(tot.hours), 0 ) ) ]
  x[ , .paid := as.integer( num(tot.comp) > 0 ) ]
  x[ , .lvl := lvl ][ , .brd := brole ]
  x[ , `:=`( .n_people = .N, .n_paid = sum(.paid),
             .pay_rank = data.table::frank( -.comp, ties.method = "min" ),
             .hrs_rank = data.table::frank( -.hours, ties.method = "min" ),
             .pay_share = .comp / max( max(.comp), 1e-9 ), .hrs_share = .hours / max( max(.hours), 1e-9 ),
             .n_ceo_titles = sum( .lvl == "CEO" ), .n_board_titles = sum( nzchar(.brd) ),
             .n_exec_paid = sum( .lvl %in% c( "CEO", "OFFICER" ) & .paid == 1 ) ), by = object.id ]
  x[ , .other_ceo_title := as.integer( .n_ceo_titles - ( .lvl == "CEO" ) > 0 ) ]
  x[ , .same_title := .N, by = .( object.id, title.standard ) ]

  base <- cbind(
    comp = x$.comp, hours = x$.hours, paid = x$.paid,
    former = num(x$former.x), interim = num(x$interim.x), co = num(x$co.x),
    founder = num(x$founder.x), exoff = num(x$ex.officio.x), n_titles = num(x$title.count),
    n_people = x$.n_people, n_paid = x$.n_paid, pay_rank = x$.pay_rank, hrs_rank = x$.hrs_rank,
    pay_share = x$.pay_share, hrs_share = x$.hrs_share, n_ceo_titles = x$.n_ceo_titles,
    n_board_titles = x$.n_board_titles, n_exec_paid = x$.n_exec_paid,
    other_ceo_title = x$.other_ceo_title, same_title = x$.same_title,
    top_paid = as.integer( x$.pay_rank == 1 & x$.paid == 1 ),
    only_paid = as.integer( x$.n_paid == 1 & x$.paid == 1 ) )
  cats <- data.frame(
    lvl   = lv( lvl, .role_classes[-1] ),
    brd   = lv( brole, c( "CHAIR", "VICE CHAIR", "SECRETARY", "TREASURER", "MEMBER" ) ),
    dom   = lv( x$domain.category, c( "executive", "governance", "operations", "industry-specific", "non-job title" ) ),
    soc   = lv( substr( chr(x$major.group), 1, 2 ),
                c( "11","13","15","17","19","21","23","25","27","29","31","33","35","37","39","41","43","47","49","51","53" ) ),
    match = lv( x$title.match, c( "exact", "pattern" ) ) )
  dummies <- stats::model.matrix( ~ . - 1, data = cats,
                                  contrasts.arg = lapply( cats, stats::contrasts, contrasts = FALSE ) )
  cbind( base, kw, dummies )
}


# load the shipped model once per session; NULL when xgboost is not installed
.role_model_cache <- new.env( parent = emptyenv() )
.role_model <- function()
{
  if( ! requireNamespace( "xgboost", quietly = TRUE ) ) return( NULL )
  if( is.null( .role_model_cache$fit ) )
  {
    dir <- system.file( "extdata", "role-model", package = "titleclassifier" )
    if( ! nzchar(dir) ) dir <- file.path( "inst", "extdata", "role-model" )   # source tree
    f <- file.path( dir, "role-model.ubj" )
    if( ! file.exists(f) ) return( NULL )
    .role_model_cache$fit <- xgboost::xgb.load( f )
    .role_model_cache$features <- readLines( file.path( dir, "feature-names.txt" ) )
  }
  .role_model_cache
}


#' @title
#' predict roles with the no-checkbox role model
#'
#' @description
#' Predicts each row's role (BOARD, CEO, OFFICER, MANAGER, PROFESSIONAL,
#' STAFF) with the shipped no-checkbox model. Returns `NULL` if xgboost is not
#' installed.
#'
#' @return A character vector, one role per row of `comp.data`, or `NULL`.
#'
#' @export
#' @param comp.data The output of `categorize_titles()`.
predict_roles <- function( comp.data )
{
  m <- .role_model()
  if( is.null(m) ) return( NULL )
  X <- role_model_features( comp.data )
  miss <- setdiff( m$features, colnames(X) )
  if( length(miss) ) X <- cbind( X, matrix( 0, nrow(X), length(miss), dimnames = list( NULL, miss ) ) )
  X <- X[ , m$features, drop = FALSE ]
  pr <- stats::predict( m$fit, xgboost::xgb.DMatrix(X) )
  if( is.null( dim(pr) ) ) pr <- matrix( pr, ncol = length(.role_classes), byrow = TRUE )
  .role_classes[ max.col( pr, ties.method = "first" ) ]
}
