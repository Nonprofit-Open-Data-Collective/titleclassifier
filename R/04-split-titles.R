#Step 4: Split Titles

# 04-split-titles.R
require(stringr)




#' @title 
#' split titles wrapper function
#' 
#' @description 
#' creates a flag for a split, and then builds out a new data frame 
#' (combining it back together) later --> should run much faster
#' 
#' runtime compared to old code is about several magnitudes faster than before
#' 
#' @export
split_titles <- function( df, title = "TitleTxt3" )
{
  x <- df[[title]]
  
  # edge cases
  x <- apply_misc_split_rules(x)
 
  # empty titles are returned as character(0) 
  # from strsplit()
  x[ x == "" ] <- " "
  
  # unique titles have & as separators:
  title.list <- strsplit( x, "&" )
  title.list <- lapply( title.list, trimws )
  
  # ADD title veracity check here

  # count number of unique titles per person:
  title.count <- sapply( title.list, length )
  
  # duplicate rows of df based 
  # on number of unique titles
  row.id <- 1:nrow(df)
  row.count <- rep( row.id, times=title.count )
  df <- df[  row.count , ]
  
  # single vector with all unique titles 
  new.titles.vec <- unlist( title.list )
  df$TitleTxt4 <- new.titles.vec
  
  # SANITY CHECK 
  # length(new.titles.vec) == nrow(df)
  
  # number of unique titles per person
  new.title.id <- sapply( title.count, seq ) %>% unlist()
  df$Num.Titles <- new.title.id  
  
  cat( "? split titles step complete\n" )
  return(df)
}
  


#' @title 
#' misc split rules for edge cases
#' 
#' @description 
#' Add some additional split rules for cases
#' not identified by previous steps. 
#' 
#' @export
apply_misc_split_rules <- function(x)
{
  # remove all lingering digits
  x <- gsub( "\\d", "", x )
  
  # random title splits
  x <- gsub( "CEO TRUSTEE", "CEO & TRUSTEE", x )
  x <- gsub( "DIRECTOR AND AD HOC", "DIRECTOR & AD HOC", x )
  x <- gsub( "AND CFO$", "& CFO", x )
  
  # abbreviated exec dir positions 
  x <- gsub( pattern = "\\bEX[A-Z]*\\b\\s*&\\s*DIR[A-Z]*\\b",
             replacement = "EXECUTIVE DIRECTOR", 
             x )
  x <- gsub( "CEO AND EXOFFICIO$", "CEO & EXOFFICIO", x )  
  x <- gsub( "DIRECTOR AND EXOFFICIO$", "DIRECTOR & EXOFFICIO", x ) 
  
  # joint board positions   
  x <- gsub( "^\\s*SEC[A-Z]*\\s*TREAS[A-Z]*\\b$", 
             "SECRETARY & TREASURER", x )
  x <- gsub( "TREASURER AND S$", "TREASURER & SECRETARY", x ) 

  # split all FOUNDER titles
  x <- gsub( "\\bFOUNDING\\b", "FOUNDER", x )
  x <- gsub( "\\bFOUNDER\\b", "& FOUNDER & ", x )
  x <- gsub( "& $", "", x ) # trailing ampersands
  x <- trimws( x )
  
  return(x)
}






#' @title 
#' identify split num function
#' 
#' @description 
#' identifies if a split is in a title using the & separator
#' and returns the number of titles actually present
#' 
#' @export
identify_split_num <- function(x){

  # remove digits
  x <- gsub( "\\d", "", x )

  # replace "exec & dir" versions with "executive director"
  x <- gsub( "\\bEX[A-Z]*\\b\\s*&\\s*DIR[A-Z]*\\b", "EXECUTIVE DIRECTOR", x )

  # replace "secretary treasurer" with "secretary & treasurer"
  x[ grepl( "^\\s*SEC[A-Z]*\\s*TREAS[A-Z]*\\b$", x ) ] <- "SECRETARY & TREASURER"
  
  num.titles <- stringr::str_count( x, "&" ) + 1
  
  return( num.titles )
}


#' @title 
#' remove first split function
#' 
#' @description 
#' removes the first occurrence of a split title
#' 
#' @export
remove_first_split <- function(x)
{
  #finds the first occurrence of the ampersand
  amp_loc <- regexpr( "&", x ) 
  
  # drop text before first ampersand
  x <- substr( x, amp_loc+1, nchar(x) )
  
  # x <- ifelse( nchar( substr( x, regexpr( "&", x ) + 1, 
  #                                 nchar(x) ) ),
  #                    substr( x, amp_loc+1, nchar(x) ), 
  #                    substr( x, 1, amp_loc-1) )
  
  return( x )
}


