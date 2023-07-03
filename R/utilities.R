###---------------------------------------------------
###   UTILITY FUNCTIONS 
###---------------------------------------------------

# make the pipe operator 
# available through the
# magrittr package

#' @importFrom magrittr "%>%"


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


