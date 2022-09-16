#Step 4: Split Titles

# 04-split-titles.R


#' @title
#' split titles function
#'
#' @description
#' wrapper function:
#' splitting titles and returning a new data frame
#' it will be applied on a data frame
#'
#' simply splitting on "&"
#' assuming it's already cleaned well
#'
#' runtime on 100,000 titles ~ 45 minutes
#'
#' @export
split_titles <- function(comp.data, title="TitleTxt3"){
  # time1 <- Sys.time()
  raw.titles <- comp.data[[title]]
  pdf <- data.frame(matrix(ncol = ncol(comp.data)))
  colnames(pdf) <- names(comp.data)
  pdf$Num.Title <- 0
  pdf$TitleTxt4 <- NA
  k <- 1
  #not sure how to vectorize...
  
  for(i in 1:length(raw.titles)){
    # print(i)
    title <- raw.titles[i]
    if(!is.na(title)){
      title.list <- split_title(title)
      for(j in 1:length(title.list)){
        subtitle <- title.list[j]
        if(length(subtitle) >= 1){
          if(!is.na(subtitle) & nchar(subtitle) > 0){
            pdf[k,] <- comp.data[i,]
            pdf$TitleTxt4[k] <- subtitle
            pdf$Num.Title[k] <- j
            k <- k + 1
          }
        }
      }
    }
  }
  # time2 <- Sys.time()
  # print(paste0("RUNTIME: ", difftime(time2,time1, units = "mins"), " MINUTES"))
  
  print("split titles step complete")
  
  return(pdf)
}




#' @title 
#' unmingle function
#' 
#' @description 
#' unmingles titles that are stuck together
#' e.g. separates "executivedirector" into "executive director"
#'
#' @export
unmingle <- function(TitleTxt){
  
  title.list <- unlist(strsplit(TitleTxt," "))
  for(i in 1:length(title.list)){
    title <- title.list[i]
    if(length(title) >= 1 && !is.na(title) && nchar(title) > 0){
      if(nchar(title) > 10 & title != "INFORMATION" &
         title != "STEWARDSHIP" & title != "ORCHESTRATOR" &
         !grepl("\\bTRANSPORT",title) & !grepl("\\bREPRESENT", title)){
        unmingled.candidate <- hunspell::hunspell_suggest(title)[[1]][1]
        if(grepl("\\s", unmingled.candidate) && 
           regexpr("\\s", unmingled.candidate)[1] > 2){
          title.list[i] <- standardize_space(unmingled.candidate) #generally a slow step
          
        }
      }
    }
  }
  TitleTxt <- paste(title.list,collapse = " ")
  
  return(TitleTxt)
}


#' @title 
#' standardize space function
#' 
#' @description 
#' only really used with unmingle (otherwise it's too volatile)
#'
#' @export
standardize_space <-function(TitleTxt){
  
  space_split <- unlist(strsplit(TitleTxt," "))
  space_true <- TRUE #space used as a separator (defaulted to true)
  for(i in 1:length(space_split)){
    testTitle <- apply_substitutes(space_split[i])
    titlePresent <- FALSE
    for(title in likely.titles){
      if(grepl(title,testTitle))
        titlePresent <- TRUE
    }
    space_true <- (space_true & titlePresent)
  }
  #space as separator
  if(space_true) 
    TitleTxt <- gsub(" "," & ",TitleTxt)
  
  #otherwise do nothing
  return(TitleTxt)
}



#' @title 
#' split title function
#' 
#' @description 
#' splits compound titles using & as a separator, 
#' returning a list of possible list of titles
#' 
#' @export
split_title <- function(TitleTxt){
  
  title.list <- unlist(strsplit(TitleTxt,"&"))
  return.list <- c()
  for(i in 1:length(title.list)){
    #get rid of punctuation and numbers (except for apostrophe)
    title <- gsub("[^[:alnum:][:space:]']"," ",title.list[i]) 
    #extraneous spaces to be deleted
    
    title <- gsub("\\d", "",title)
    title <- gsub("'", "", title)
    # title <- gsub("-", " ",title)
    
    title <- unmingle(title)
    if(!grepl("&", title)){
      if(length(title) > 0 && grepl("[A-Z]",title))
        return.list <- append(return.list,title)
    }
    else{
      if(grepl("\\bEX[A-Z]*\\b\\s*&\\s*DIR[A-Z]*\\b", title)){
        title <- gsub("\\bEX[A-Z]*\\b\\s*&\\s*DIR[A-Z]*\\b", "EXECUTIVE DIRECTOR", title)
        return.list <- append(return.list, title)
      }
      else{
        subtitles <- unlist(strsplit(title, "&"))
        for(st in subtitles){
          return.list <- append(return.list, st)
        }
      }
    }
  }
  if(grepl("^\\s*SEC[A-Z]*\\b\\s+TREAS[A-Z]*\\b$",
           gsub("[[:punct:]]"," ", TitleTxt)))
    return.list <- c("SECRETARY", "TREASURER")
  return(return.list)
}


