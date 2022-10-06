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
split_titles <- function(df, title = "TitleTxt3"){
  
  TitleTxt <- df[[title]]
  
  TitleTxt <- gsub("\\d", "",TitleTxt)
  
  
  TitleTxt <- gsub("\\bEX[A-Z]*\\b\\s*&\\s*DIR[A-Z]*\\b",
                   "EXECUTIVE DIRECTOR", TitleTxt)
  
  TitleTxt <- gsub("^\\s*SEC[A-Z]*\\s*TREAS[A-Z]*\\b$", 
                   "SECRETARY & TREASURER", TitleTxt)
  
  df$Num.Titles <- identify_split_num(TitleTxt)
  
  df$TitleTxt4 <- TitleTxt
  
  has.one.title <- df$Num.Titles == 1
  multiple.titles <- df[df$Num.Titles >= 2, ]
  df$TitleTxt4 <- ifelse(has.one.title, TitleTxt,
                         substr(TitleTxt,1, regexpr("&",TitleTxt)-1))
  temp <- df
  
  #assuming no field has more than 5 titles
  for(i in 1:5){
    
    #KEY STEPS
    has.one.title <- temp$Num.Titles == 1
    if(i!=1) multiple.titles <- temp[temp$Num.Titles >= 2, ]
    temp$TitleTxt4 <- ifelse(has.one.title, TitleTxt, 
                             substr(TitleTxt,1, regexpr("&",TitleTxt)-1))
    if(i != 1) df <- rbind(df,temp)
    multiple.titles$Num.Titles <- multiple.titles$Num.Titles - 1
    
    multiple.titles$TitleTxt4 <- remove_first_split(multiple.titles$TitleTxt4)
    
    temp = multiple.titles
    TitleTxt <- temp$TitleTxt4
  }
  
  print("split titles step completed")
  return(df)
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
  TitleTxt <- x

  TitleTxt <- gsub("\\d", "",TitleTxt)


  TitleTxt <- gsub("\\bEX[A-Z]*\\b\\s*&\\s*DIR[A-Z]*\\b",
                "EXECUTIVE DIRECTOR", TitleTxt)

  TitleTxt[grepl("^\\s*SEC[A-Z]*\\s*TREAS[A-Z]*\\b$", TitleTxt)] <-
    "SECRETARY & TREASURER"
  
  return( stringr::str_count(TitleTxt,"&")+1 )
}


#' @title 
#' remove first split function
#' 
#' @description 
#' removes the first occurrence of a split title
#' 
#' @export
remove_first_split <- function(x){
  TitleTxt <- x
  
  amp_loc <- regexpr("&", TitleTxt) #finds the first occurrence of the ampersand
  TitleTxt <- ifelse(nchar(substr(TitleTxt, regexpr("&",TitleTxt)+1, 
                                  nchar(TitleTxt))),
                     substr(TitleTxt, amp_loc+1, nchar(TitleTxt)), 
                     substr(TitleTxt, 1, amp_loc-1))
  
  return(TitleTxt)
}


