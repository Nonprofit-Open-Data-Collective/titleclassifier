
# tinypartvii %>% 
#   standardize_df() %>% 
#   remove_dates() %>% 
#   standardize_conj() %>% 
#   split_titles() %>% 
#   standardize_spelling() %>% 
#   gen_status_codes() %>% 
#   standardize_titles() %>%
#   categorize_titles() %>%
#   conditional_logic()



get_n <- function( x, n, seed=123 )
{
  set.seed(seed)
  if( n > length(x) ){ n <- length(x) }
  x <- sample( x, n )
  return(x)
}



get_n_df <- function( df, n, seed=123 )
{
  set.seed(seed)
  if( n > nrow(df) ){ n <- nrow(df) }
  df <- dplyr::sample_n( df, n )
  return(df)
}



#' @title
#' test 'remove_dates' function
#'
#' @description
#' Generate a test table to show effects of the function.
#'
#' @param x A vector of test titles, if NULL uses internal data. 
#' @param n How many test cases to process. 
#' @return A data frame with the input titles (RAW) and processed titles (FIXED). 
#'
#' @export
test_remove_dates <- function( n=250, x=NULL )
{

  if( is.null(x) )
  { 
    x <- tinypartvii$F9_07_COMP_DTK_TITLE
    x <- pre_clean(x)
    x <- x[ has_date(x) ]
  }
  x <- convert_ordinal( x )
  x2 <- remove_date( x )
  df <- data.frame( RAW=x, FIXED=x2 )
  return(df)
}
  

# test_remove_dates()



####
#### STEP 03 - STANDARDIZE CONJUGATIONS 
####

#' @title
#' test 'standardize_and' function
#'
#' @description
#' Generate a test table to show effects of the function.
#'
#' @param x A vector of test titles, if NULL uses internal data. 
#' @param n How many test cases to process. 
#' @return A data frame with the input titles (RAW) and processed titles (FIXED). 
#'
#' @export
test_standardize_and <- function( n=250, x=NULL )
{
  if( is.null(x) )
  { 
    x <- tinypartvii$F9_07_COMP_DTK_TITLE
    x <- pre_clean(x)
    x <- convert_ordinal(x)
    x <- remove_date(x)
    x <- grep( "\\bAND\\b", x, value=TRUE )
  }
  x <- get_n( x, n )
  x2 <- standardize_and(x)
  df <- data.frame( RAW=x, FIXED=x2 )
  return(df)
}

  
  
 #' @title
#' test 'standardize_to' function
#'
#' @description
#' Generate a test table to show effects of the function.
#'
#' @param x A vector of test titles, if NULL uses internal data. 
#' @param n How many test cases to process. 
#' @return A data frame with the input titles (RAW) and processed titles (FIXED). 
#'
#' @export 
test_standardize_to <- function( n=250, x=NULL )
{
  
  if( is.null(x) )
  { 
    x <- tinypartvii$F9_07_COMP_DTK_TITLE
    x <- pre_clean(x)
    x <- convert_ordinal(x)
    x <- grep( "\\bTO\\b", x, value=TRUE )
  }
  x <- get_n( x, n )
  x2 <- remove_date(x)
  x3 <- standardize_to(x2)
  df <- data.frame( RAW=x, STAN=x2, FIXED=x3 )
  return(df)
}  

# test_standardize_to()




#' @title
#' test 'standardize_of' function
#'
#' @description
#' Generate a test table to show effects of the function.
#'
#' @param x A vector of test titles, if NULL uses internal data. 
#' @param n How many test cases to process. 
#' @return A data frame with the input titles (RAW) and processed titles (FIXED). 
#'
#' @export
test_standardize_of <- function( n=250, x=NULL )
{
  if( is.null(x) )
  { 
    x <- tinypartvii$F9_07_COMP_DTK_TITLE
    x <- pre_clean(x)
    x <- convert_ordinal(x)
    x <- remove_date(x)
    x <- grep( "\\bOF\\b", x, value=TRUE )
  }
  x <- get_n( x, n )
  x2 <- standardize_of(x)
  df <- data.frame( RAW=x, FIXED=x2 )
  return(df)
}

# test_standardize_of()




#' @title
#' test 'standardize_comma' function
#'
#' @description
#' Generate a test table to show effects of the function.
#'
#' @param x A vector of test titles, if NULL uses internal data. 
#' @param n How many test cases to process. 
#' @return A data frame with the input titles (RAW) and processed titles (FIXED). 
#'
#' @export
test_standardize_comma <- function( n=250, x=NULL )
{
  if( is.null(x) )
  { 
    x <- tinypartvii$F9_07_COMP_DTK_TITLE
    x <- pre_clean(x)
    x <- convert_ordinal(x)
    x <- remove_date(x)
    x <- grep( ",", x, value=TRUE )
  }
  x <- get_n( x, n )
  x2 <- standardize_comma(x)
  df <- data.frame( RAW=x, FIXED=x2 )
  return(df)
}

# test_standardize_comma()




#' @title
#' test 'standardize_slash' function
#'
#' @description
#' Generate a test table to show effects of the function.
#'
#' @param x A vector of test titles, if NULL uses internal data. 
#' @param n How many test cases to process. 
#' @return A data frame with the input titles (RAW) and processed titles (FIXED). 
#'
#' @export
test_standardize_slash <- function( n=250, x=NULL )
{
  if( is.null(x) )
  { 
    x <- tinypartvii$F9_07_COMP_DTK_TITLE
    x <- pre_clean(x)
    x <- convert_ordinal(x)
    x <- remove_date(x)
    x <- grep( "/", x, value=TRUE )
  }
  x <- get_n( x, n )
  x2 <- standardize_slash(x)
  df <- data.frame( RAW=x, FIXED=x2 )
  return(df)
}

# test_standardize_slash()




#' @title
#' test 'standardize_separator' function
#'
#' @description
#' Generate a test table to show effects of the function.
#'
#' @param x A vector of test titles, if NULL uses internal data. 
#' @param n How many test cases to process. 
#' @return A data frame with the input titles (RAW) and processed titles (FIXED). 
#'
#' @export
test_standardize_separator <- function( n=250, x=NULL )
{
  if( is.null(x) )
  { 
    x <- tinypartvii$F9_07_COMP_DTK_TITLE
    x <- pre_clean(x)
    x <- convert_ordinal(x)
    x <- remove_date(x)
    alt.separators <- c( ";", "\\\\", "/", " - ", " -", "- " )
    x <- grep( paste( alt.separators, collapse="|" ), x, value=TRUE )
  }
  x <- get_n( x, n )
  x2 <- standardize_separator(x)
  df <- data.frame( RAW=x, FIXED=x2 )
  return(df)
} 

# test_standardize_separator()
  
  
  
  
  

####
#### STEP 05 - FIX SPELLING 
####

#' @title
#' test 'fix_spelling' function
#'
#' @description
#' Generate a test table to show effects of the function.
#'
#' @param x A vector of test titles, if NULL uses internal data. 
#' @param n How many test cases to process. 
#' @return A data frame with the input titles (RAW) and processed titles (FIXED). 
#'
#' @export
test_fix_spelling <- function( n=250, x=NULL )
{
  if( is.null(x) )
  { 
    x <- tinypartvii$F9_07_COMP_DTK_TITLE
    x <- 
      x %>%
      pre_clean()         %>%
      convert_ordinal()   %>%
      remove_date()       %>%
      standardize_and()   %>%
      standardize_to()    %>%
      standardize_of()    %>%
      standardize_comma() %>%
      standardize_slash() %>%
      standardize_and()   %>%  
      standardize_separator() %>%
      standardize_and()   %>%
      txt_cleanup()
  }

  x2 <- fix_spelling( x )
  
  df <- data.frame( RAW=x, FIXED=x2 )
  df <- df[ x != x2 , ]

  df <- get_n_df( df, n )
  return(df)
}

# test_fix_spelling()

