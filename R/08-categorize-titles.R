#Step 8: Categorize Titles
#based on old title categorization schema 

# 08-categorize-titles.R   



require(dplyr)

#' @title 
#' categorize titles function
#' 
#' @description 
#' categorizes the titles in comp.data 
#' works on TitleTxt7
#' wrapper function
#' 
#' @export
categorize_titles <- function( comp.data )
{
  
  googlesheets4::gs4_deauth()
  google.id <- "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M"
  d.taxonomy <- 
    googlesheets4::read_sheet(  google.id,
                                sheet="title-taxonomy", range="A:T",
                                col_types = "c" )  # c = character
  d.taxonomy[ is.na( d.taxonomy ) ] <- ""
  
  
  comp.data <- merge( comp.data, d.taxonomy, 
                     by.x="title.standard", by.y="title.standard", 
                     all.x=T )
  
  comp.data <- 
    comp.data %>% 
    add_features() %>% 
    simplify_varnames()
  
  print("categorize titles step complete")
  
  return( comp.data )
}

#' @title 
#' engineer new features from existing fields
#' 
#' @description 
#' adds helpful context fields in comp dataset
#' like pay rank, hour rank, counts of types of employees, etc.
#' 
#' @export
add_features <- function( df )
{

    df$Multiple.Titles  <- as.numeric( df$Multiple.Titles )
    df$FORMER           <- as.numeric( df$FORMER )
    df$INTERIM          <- as.numeric( df$INTERIM ) 
    df$REGIONAL         <- as.numeric( df$REGIONAL )

    these <- c("emp", "board",
               "ceo", "c.level", "dir.vp", 
               "mgr", "spec", "pres", "vp", 
               "sec", "treas", "com" )

    df[ these ] <- 
      df[these] %>% 
      lapply( to_boolean )  # to_boolean is in utilities.R

    # weight people w multiple titles
    df <- 
      df %>% 
      dplyr::group_by( OBJECT_ID, F9_07_COMP_DTK_NAME_PERS ) %>% 
      dplyr::mutate( tot.titles = max(Num.Titles, na.rm=T ),
                     emp2 = ifelse( sum(emp)>0, emp/sum(emp), 0 ),
                     board2 = ifelse( sum(board)>0, board/sum(board), 0 )  ) %>%
      ungroup()  


    # RECALCULATE HOURS AND PAY W RELATED ORGS SEPARATED

    hours1 <-  as.numeric( df$F9_07_COMP_DTK_AVE_HOUR_WEEK )
    hours2 <-  as.numeric( df$F9_07_COMP_DTK_AVE_HOUR_WEEK_RL )

    df$TOT.HOURS <- hours1
    df$TOT.HOURS.TOT <- hours1 + hours2

    pay1 <-  as.numeric( df$F9_07_COMP_DTK_COMP_ORG )
    pay2 <-  as.numeric( df$F9_07_COMP_DTK_COMP_RLTD ) 
    pay3 <-  as.numeric( df$F9_07_COMP_DTK_COMP_OTH ) 
    pay4 <-  as.numeric( df$F9_07_COMP_DTK_EMPL_BEN ) 

    pay1[ is.na(pay1) ] <- 0
    pay2[ is.na(pay2) ] <- 0
    pay3[ is.na(pay3) ] <- 0
    pay4[ is.na(pay4) ] <- 0

    df$TOT.COMP <- pay1 + pay3 + pay4
    df$TOT.COMP.TOT <- pay1 + pay2 + pay3 + pay4



    df$tot.comp2 <- df$TOT.COMP
    df$tot.comp2[ df$Num.Titles > 1 ] <- NA
    df$tot.hours2 <- df$TOT.HOURS
    df$tot.hours2[ df$Num.Titles > 1 ] <- NA

    df <- 
      df %>%
      dplyr::group_by( EIN ) %>% 
      dplyr::mutate( num.dtk = n() - sum( Num.Titles > 1, na.rm=T ),
                     num.titles = n(),
                     num.emp = sum( emp2, na.rm=T ),  # weights people w multiple titles
                     num.ceos = sum( ceo, na.rm=T ),
                     num.clevel = sum( ceo, na.rm=T ) + sum( c.level, na.rm=T ),
                     num.dirvp = sum( dir.vp, na.rm=T ),
                     num.mgr = sum( mgr, na.rm=T ),
                     num.spec = sum( spec, na.rm=T ),
                     num.board = sum( board2, na.rm=T ),  # weights people w multiple titles
                     num.pres = sum( pres, na.rm=T ),
                     num.vp = sum( vp, na.rm=T ),
                     num.treas = sum( treas, na.rm=T ),
                     num.sec = sum( sec, na.rm=T ),
                     num.com = sum( com, na.rm=T ),
                     num.paid = sum( tot.comp2 > 0, na.rm=T ),  # don't double-count multiple titles
                     num.fte = sum( tot.hours2 >= 40, na.rm=T ),
                     num.fte.30h = sum( tot.hours2 >= 30, na.rm=T ),
                     num.pte = sum( tot.hours2 > 9 & tot.hours2 < 30, na.rm=T ),                   
                     pay.rank = dense_rank( -TOT.COMP ),
                     pay.pct.of.max = TOT.COMP / max( TOT.COMP, na.rm=T ),
                     pay.pct.of.tot = TOT.COMP / sum( tot.comp2, na.rm=T ),  # don't double-count multiple titles
                     hours.rank = dense_rank( -TOT.HOURS ),
                     hours.pct.of.max = TOT.HOURS / max( TOT.HOURS, na.rm=T )  ) %>% 
      ungroup() %>% 
      as.data.frame()
    

    new.order <- 
    c("NAME", "EIN", "TAXYR", "FORMTYPE", 
      "F9_07_COMP_DTK_NAME_PERS", "F9_07_COMP_DTK_TITLE",  
      "Multiple.Titles", "Num.Titles", "tot.titles", 

      "TITLE_RAW", "title.standard", 
      "TOT.HOURS", "TOT.HOURS.TOT",  "hours.rank", "hours.pct.of.max",  
      "TOT.COMP", "TOT.COMP.TOT",  
      "pay.rank", "pay.pct.of.max", "pay.pct.of.tot",

      # "hour_rank", "pay_rank", "has_leader", 

      "num.dtk", "num.titles", 
      "num.paid", "num.fte", "num.fte.30h", "num.pte",  

      "TitleTxt7", "TitleTxt6", "TitleTxt5", "TitleTxt4", 
      "TitleTxt3", "TitleTxt2", 

      "F9_07_COMP_DTK_POS_INDIV_TRUST_X", "F9_07_COMP_DTK_POS_INST_TRUST_X", 
      "F9_07_COMP_DTK_POS_OFF_X", "F9_07_COMP_DTK_POS_KEY_EMPL_X", 
      "F9_07_COMP_DTK_POS_HIGH_COMP_X", "F9_07_COMP_DTK_POS_FORMER_X", 
      "F9_07_COMP_DTK_AVE_HOUR_WEEK", "F9_07_COMP_DTK_AVE_HOUR_WEEK_RL", 
      "F9_07_COMP_DTK_COMP_ORG", "F9_07_COMP_DTK_COMP_RLTD", 
      "F9_07_COMP_DTK_COMP_OTH", "F9_07_COMP_DTK_EMPL_BEN", 

      "FORMER.X", "INTERIM.X", "FUTURE.X", "PARTIAL.X", "AT.LARGE.X", 
      "EXOFFICIO.X", "CO.X", "REGIONAL.X", "DATE.X", "SCHED.O.X", 

      "domain.category", "domainl.label", "soc.label", 
      "major.group", "minor.group", "broad.group", "detailed.occupation",
      
      "strata",

      "emp", "num.emp", "board", "num.board", 

      "ceo", "c.level", "dir.vp", "mgr", "spec",
      "num.ceos", "num.clevel", "num.dirvp", "num.mgr", "num.spec", 

      "pres", "vp", "sec", "treas", "com",
      "num.pres", "num.vp", "num.treas", "num.sec", "num.com", 

      "URL", "OBJECT_ID"  )

      df <- df[ new.order ]
      
      
      df = df[duplicated(df[, c("dtk.name", "dtk.title", "title.standard")]) & 
                  df$title.standard == "CEO", ]
      
      
      
   df <- clean_up_ceos(df)

   return( df )

}







#' @title 
#' rename variables in the dataset
#' 
#' @description 
#' simplify variable names 
#' 
#' @export
simplify_varnames <- function( df )
{

  df <- 
    df %>% 
    dplyr::rename(  
           org.name = NAME,
           ein = EIN, 
           taxyr = TAXYR, 
           formtype = FORMTYPE, 
           dtk.name = F9_07_COMP_DTK_NAME_PERS, 
           multiple.titles = Multiple.Titles, 
           title.count = Num.Titles, 
           title.tot = tot.titles,
           dtk.title = F9_07_COMP_DTK_TITLE, 
           title.raw = TITLE_RAW, 
           tot.hours = TOT.HOURS,
           tot.hours.tot = TOT.HOURS.TOT, 
           # hours.rank, 
           # hours.pct.of.max, 
           tot.comp = TOT.COMP, 
           tot.comp.tot = TOT.COMP.TOT, 
           # pay.rank, pay.pct.of.max, pay.pct.of.tot, 
           # num.dtk, num.titles, num.paid,  
           # num.fte, num.fte.30h, num.pte, 
           title.v7 = TitleTxt7, 
           title.v6 = TitleTxt6, 
           title.v5 = TitleTxt5, 
           title.v4 = TitleTxt4, 
           title.v3 = TitleTxt3, 
           title.v2 = TitleTxt2, 
           dtk.indiv.trustee.x = F9_07_COMP_DTK_POS_INDIV_TRUST_X, 
           dtk.inst.trustee.x = F9_07_COMP_DTK_POS_INST_TRUST_X, 
           dtk.officer.x = F9_07_COMP_DTK_POS_OFF_X, 
           dtk.key.empl.x = F9_07_COMP_DTK_POS_KEY_EMPL_X, 
           dtk.high.comp.x = F9_07_COMP_DTK_POS_HIGH_COMP_X, 
           dtk.former.x = F9_07_COMP_DTK_POS_FORMER_X, 
           dtk.hours = F9_07_COMP_DTK_AVE_HOUR_WEEK, 
           dtk.hours.rltd = F9_07_COMP_DTK_AVE_HOUR_WEEK_RL, 
           dtk.comp = F9_07_COMP_DTK_COMP_ORG, 
           dtk.comp.rltd = F9_07_COMP_DTK_COMP_RLTD, 
           dtk.comp.oth = F9_07_COMP_DTK_COMP_OTH, 
           dtk.comp.ben = F9_07_COMP_DTK_EMPL_BEN, 
           former.x = FORMER.X, 
           interim.x = INTERIM.X, 
           future.x = FUTURE.X,
           partial.x = PARTIAL.X,
           as.needed.x = AT.LARGE.X, 
           ex.officio.x = EXOFFICIO.X, 
           co.x = CO.X,
           region.x = REGIONAL.X, 
           date.x = DATE.X, 
           sched.o.x = SCHED.O.X, 
           # domain.category, domainl.label, soc.label, 
           # major.group, minor.group, broad.group, detailed.occupation, 
           # emp, num.emp, board, num.board, ceo, c.level, dir.vp, 
           # mgr, spec, num.ceos, num.clevel, num.dirvp, num.mgr, 
           # num.spec, pres, vp, sec, treas, com, num.pres, 
           # num.vp, num.treas, num.sec, num.com, 
           url = URL, 
           object.id = OBJECT_ID )

   return( df )
}

#' @title 
#' clean up ceos function
#' 
#' @description 
#' removes all duplicate instances of ceos within a single org
#' 
#' @export
clean_up_ceos <- function(comp.data){
  df <- comp.data
  
  df = df[!duplicated(df[, c("ein", "dtk.name", "dtk.title", "title.standard")]) | 
            df$title.standard != "CEO", ] #remove duplicates of ceos
  #but potential issues with multiple titles
  
  
  return(df)
}



#' 
#' #' @title 
#' #' categorize ceo function
#' #' 
#' #' @description 
#' #' categorizing ceo's and adding a flag
#' #'
#' #' @param df A compensation data frame. 
#' #'
#' #' @return Returns a data frame with a CEO boolean (1=CEO,0=other). 
#' categorize_ceo <- function(df){
#'   
#'   df$CEO <- 0
#'   
#'   #ceo's (but not assistant or associate)
#'   df$CEO [grepl("CEO",df$TitleTxt7)] <- 1
#'   df$CEO [grepl("ASSOCIATE",df$TitleTxt7)] <- 0
#'   df$CEO [grepl("ASSISTANT",df$TitleTxt7)] <- 0
#'   
#'   #exec dir
#'   df$CEO [grepl("\\bEXECDIR[A-Z]*\\b",df$TitleTxt7)] <- 1
#'   df$CEO [grepl("\\bEXDIR[A-Z]*\\b",df$TitleTxt7)] <- 1
#'   
#'   #president (if paid and working 40+ hours)
#'   df$CEO [grepl("PRESIDENT",df$TitleTxt7) && df$AvgHrs >= 40 &&
#'             df$TOT_COMP > 0 && !grepl("VICE", dfTitleTxt7)] <- 1
#'   
#'   #weird ones
#'   df$CEO [agrepl("CHANCELLOR", df$TitleTxt7) && df$AvgHrs >= 40 &&
#'             df$TOT_COMP > 0 && 
#'             (df$Officer == "X" || df$FmrOfficer == "X")] <- 1
#'   df$CEO [grepl("MANAGING DIRECTOR", df$TitleTxt7) && df$AvgHrs >= 40 &&
#'             df$TOT_COMP > 0 && 
#'             (df$Officer == "X" || df$FmrOfficer == "X")] <- 1
#'   df$CEO [grepl("HEADMASTER", df$TitleTxt7) && df$AvgHrs >= 40 &&
#'             df$TOT_COMP > 0 && 
#'             (df$Officer == "X" || df$FmrOfficer == "X")] <- 1
#'   return(df)
#' }
#' 
#' 
#' #' @title 
#' #' categorize company leader function
#' #' 
#' #' @description 
#' #' utilizes ceo and adds in president, chair, and managing director for those 
#' #' without explicit/clear ceo positions

#' categorize_company_leader <- function(df){
#'   
#'   df <- categorize_ceo( df )
#'   df$Org.Leader <- 0
#'   df$Org.Leader[df$CEO == 1] <- 1
#'   aoc <- unique(df$NAME.x[df$Org.Leader == 1])
#'   
#'   #presidents
#'   pres.leadership <- c("PRESIDENT", "BOARD PRESIDENT", "PRESIDENT OF BOARD")
#'   df$Org.Leader[!df$NAME.x %in% aoc && 
#'                   ((grepl("PRESIDENT",df$TitleTxt7) && 
#'                       !grepl("VICE",df$TitleTxt7)) ||
#'                      df$TitleTxt7[i] %in% pres.leadership)] <- 1
#'   for(i in 1:length(df$NAME.x)){
#'     if(!(df$NAME.x[i] %in% aoc)){
#'       if((grepl("PRESIDENT",df$TitleTxt7[i]) && !grepl("VICE",df$TitleTxt7[i])) ||
#'          df$TitleTxt7[i] %in% pres.leadership){
#'         df$Org.Leader[i] <- 1
#'       }
#'     }
#'   }
#'   aoc <- unique(df$NAME.x[df$Org.Leader == 1])
#'   
#'   #chairs and misc
#'   chair.leadership <- c("CHAIR", "BOARD CHAIR",
#'                         "MANAGING DIRECTOR", "CHAIR OF BOARD")
#'   for(i in 1:length(df$NAME.x)){
#'     if(!(df$NAME.x[i] %in% aoc)){
#'       if(df$TitleTxt7[i] %in% chair.leadership){
#'         df$Org.Leader[i] <- 1
#'       }
#'     }
#'   }
#'   aoc <- unique(df$NAME.x[df$Org.Leader == 1])
#'   
#'   return(df)
#' }
#' 
#' 
#' # 
#' # categorize_cfo <- function(df){
#' #   
#' # }
