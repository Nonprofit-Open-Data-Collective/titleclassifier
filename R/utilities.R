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
