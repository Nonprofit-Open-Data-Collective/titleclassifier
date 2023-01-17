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
  
  # final substitutions before split 
  # MOVE TO PREVIOUS STEP 
  x <- gsub( "\\d", "", x )
  
  x <- gsub( pattern = "\\bEX[A-Z]*\\b\\s*&\\s*DIR[A-Z]*\\b",
             replacement = "EXECUTIVE DIRECTOR", 
             x )
  
  x <- gsub( pattern = "^\\s*SEC[A-Z]*\\s*TREAS[A-Z]*\\b$", 
             replacement = "SECRETARY & TREASURER", 
             x )
 
  # split all FOUNDER titles
  x <- gsub( " & FOUNDER\\b", " FOUNDER", x )
  x <- gsub( "\\bFOUNDING\\b", "FOUNDER", x )
  x <- gsub( "\\bFOUNDER\\b", "& FOUNDER", x )
  
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
  
  print("split titles step complete")
  return(df)
}
  

  

# df$Num.Titles <- identify_split_num(TitleTxt)
# 
# df$TitleTxt4 <- TitleTxt 
# 
# 
# #WHAT ARE WE DOING FROM HERE ON??
# 
# #first pass thru
# 
# #boolean vector of all titles with only one occurrence
# has.one.title <- df$Num.Titles == 1 
# 
# #data frame of entires with at least 2 titles
# multiple.titles <- df[df$Num.Titles >= 2, ] 
# 
# #selecting only the first title in titles with multiple possible elements
# df$TitleTxt4 <- ifelse(has.one.title, TitleTxt,
#                        substr(TitleTxt,1, regexpr("&",TitleTxt)-1))
# 
# #saving the one-title filtered df as a temp data frame
# temp <- df
# 
# #assuming no field has more than 5 titles
# max.num.titles <- 5
# 
# #iterative approach
# for(i in 1:max.num.titles){
#   
#   #KEY STEPS
#   
#   #repeatedly creating a boolean vector reflecting which entries 
#   #have singular titles
#   has.one.title <- temp$Num.Titles == 1 
#   
#   #creating the data frame with multiple titles (for consistency this is duplicated)
#   if(i!=1) multiple.titles <- temp[temp$Num.Titles >= 2, ]
#   
#   #selecting only the first title in temp
#   temp$TitleTxt4 <- ifelse(has.one.title, TitleTxt, 
#                            substr(TitleTxt,1, regexpr("&",TitleTxt)-1))
#   
#   #binding the singular titles to the data frame (if not first step)
#   if(i != 1) df <- rbind(df,temp)
#   
#   #decrementing multi-titles title count
#   multiple.titles$Num.Titles <- multiple.titles$Num.Titles - 1
#   
#   #removing the first title from the multi=title entries
#   multiple.titles$TitleTxt4 <- remove_first_split(multiple.titles$TitleTxt4)
#   
#   #resetting the temp to the previously multi-title data frame
#   temp = multiple.titles
#   #at this point, one layer of titles should have been removed, and
#   #we will iteratively remove until there are only singular titles in temp
#   
#   #resetting titletxt
#   TitleTxt <- temp$TitleTxt4
# }
# 
# print("split titles step complete")
# return(df)
# }




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



# find all titles at once
# x <- "title1 & title2 & title3"
# titles <- strsplit( x, "&" )[[1]]
# titles <- trimws( titles )

# d <- data.frame( x=c("title1","title1 & title2","title1","title1 & title2 & title3","title1"), 
#                  y=1:5, 
#                  z=c("A","B","A","B","A") )
# 
# d
# 
# title.list <- strsplit( d$x, "&" )
# title.list <- lapply( titles, trimws )
# 
#### check title veracity here
# 
# title.count <- sapply( title.list, length )
# row.count <- rep( index, times=title.count )
# 
# d2 <- d[  row.count , ]
# d2$x2 <- unlist(title.list)
# d2
