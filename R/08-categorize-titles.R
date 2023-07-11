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
  
  cat( "✔ categorize titles step complete" )
  
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

    
    # df$AS.NEEDED.X        <- as.numeric( df$AS.NEEDED.X )
    df$Multiple.Titles    <- as.numeric( df$Multiple.Titles )
    df$FORMER.X           <- as.numeric( df$FORMER.X )
    df$INTERIM.X          <- as.numeric( df$INTERIM.X ) 
    df$REGIONAL.X         <- as.numeric( df$REGIONAL.X )
    df$PARTIAL.X          <- as.numeric( df$PARTIAL.X )
    df$EXOFFICIO.X        <- as.numeric( df$EXOFFICIO.X )
    df$CO.X               <- as.numeric( df$CO.X )
    df$SCHED.O.X          <- as.numeric( df$SCHED.O.X )

    df$FOUNDER.X          <- as.numeric( df$FOUNDER.X )
    df$OUTGOING.X         <- as.numeric( df$OUTGOING.X )
    df$AT.LARGE.X         <- as.numeric( df$AT.LARGE.X )
    

    #if a date code is present, setting to partial year if not already set
    df$PARTIAL.X[ df$DATE.X == 1 ] <- 1 

    # to_boolean is in utilities.R

    these <- c("emp", "board",
               "ceo", "c.level", "dir.vp", 
               "mgr", "spec", "pres", "vp", 
               "sec", "treas", "mem" )

    df[ these ] <- 
      df[these] %>% 
      lapply( to_boolean )     




    # weight people w multiple titles

    df <- 
      df %>% 
      dplyr::group_by( OBJECT_ID, 
                       F9_07_COMP_DTK_NAME_PERS ) %>%
      dplyr::mutate( tot.titles = max( Num.Titles, na.rm=T ),
                     emp2 = ifelse( sum(emp) > 0, 
                                    emp / sum(emp), 
                                    0 ),
                     board2 = ifelse( sum(board) > 0, 
                                      board / sum(board), 
                                      0 )  
      ) %>%
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


    # do not double-count split titles 
    df$tot.comp2 <- df$TOT.COMP
    df$tot.comp2[ df$Num.Titles > 1 ] <- NA
    df$tot.comp2.tot <- df$TOT.COMP.TOT
    df$tot.comp2.tot[ df$Num.Titles > 1 ] <- NA
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
                     num.mem = sum( mem, na.rm=T ),
                     num.paid = sum( tot.comp2 > 0, na.rm=T ),  # don't double-count multiple titles
                     num.fte = sum( tot.hours2 >= 40, na.rm=T ),
                     num.fte.30h = sum( tot.hours2 >= 30, na.rm=T ),
                     num.pte = sum( tot.hours2 > 9 & tot.hours2 < 30, na.rm=T ),                   
                     pay.rank = dense_rank( -TOT.COMP ),
                     pay.max = max( TOT.COMP, na.rm=T ),
                     pay.tot = sum( tot.comp2, na.rm=T ), # don't double-count multiple titles
                     pay.pct.of.max = TOT.COMP / pay.max,
                     pay.pct.of.tot = TOT.COMP / pay.tot, 
                     pay.max.incl.rltd = max( TOT.COMP.TOT, na.rm=T ),
                     pay.tot.incl.rltd = sum( tot.comp2.tot, na.rm=T ), # don't double-count multiple titles
                     pay.pct.of.max.incl.rltd = TOT.COMP.TOT / pay.max.incl.rltd,
                     pay.pct.of.tot.incl.rltd = TOT.COMP.TOT / pay.tot.incl.rltd, 
                     hours.rank = dense_rank( -TOT.HOURS ),
                     hours.pct.of.max = TOT.HOURS / max( TOT.HOURS, na.rm=T ), 
                     hours.rank.incl.rltd = dense_rank( -TOT.HOURS.TOT ),
                     hours.pct.of.max.incl.rltd = TOT.HOURS.TOT / max( TOT.HOURS.TOT, na.rm=T )  ) %>% 
      ungroup() %>% 
      as.data.frame()
    

    new.order <- 
    c("NAME", "EIN", "TAXYR", "FORMTYPE", 
      "F9_07_COMP_DTK_NAME_PERS", 
      
      "TITLE_RAW", "title.standard",
      "Multiple.Titles", "Num.Titles", "tot.titles",
      "TitleTxt7", "TitleTxt6", "TitleTxt5", "TitleTxt4", 
      "TitleTxt3", "TitleTxt2", "F9_07_COMP_DTK_TITLE",
      
      "TOT.HOURS", "TOT.HOURS.TOT",  
      "hours.rank", "hours.rank.all",
      "hours.pct.of.max", "hours.pct.of.max.incl.rltd",  
      "TOT.COMP", "TOT.COMP.TOT", 
      "pay.rank", 
      "pay.max", "pay.max.incl.rltd", 
      "pay.tot", "pay.tot.incl.rltd",
      "pay.pct.of.max", "pay.pct.of.max.incl.rltd", 
      "pay.pct.of.tot", "pay.pct.of.tot.incl.rltd",
      
      # "hour_rank", "pay_rank", "has_leader", 

      "num.dtk", "num.titles", 
      "num.paid", "num.fte", "num.fte.30h", "num.pte",   

      "F9_07_COMP_DTK_POS_INDIV_TRUST_X", "F9_07_COMP_DTK_POS_INST_TRUST_X", 
      "F9_07_COMP_DTK_POS_OFF_X", "F9_07_COMP_DTK_POS_KEY_EMPL_X", 
      "F9_07_COMP_DTK_POS_HIGH_COMP_X", "F9_07_COMP_DTK_POS_FORMER_X", 
      "F9_07_COMP_DTK_AVE_HOUR_WEEK", "F9_07_COMP_DTK_AVE_HOUR_WEEK_RL", 
      "F9_07_COMP_DTK_COMP_ORG", "F9_07_COMP_DTK_COMP_RLTD", 
      "F9_07_COMP_DTK_COMP_OTH", "F9_07_COMP_DTK_EMPL_BEN", 

      "FOUNDER.X", "FORMER.X",   
      "FUTURE.X",  "OUTGOING.X",
      "INTERIM.X", "PARTIAL.X", 
      "EXOFFICIO.X", "AT.LARGE.X", 
      "REGIONAL.X", "CO.X", "DATE.X", 
      "SCHED.O.X",  

      "domain.category", "domain.label", 
      "strata", "strata.label",
      "soc.label", "major.group", 
      "minor.group", "broad.group", 
      "detailed.occupation",

      "emp", "num.emp", "board", "num.board", 

      "ceo",     "num.ceos",
      "c.level", "num.clevel",
      "dir.vp",  "num.dirvp",
      "mgr",     "num.mgr",
      "spec",    "num.spec", 

      "pres",  "num.pres",
      "vp",    "num.vp",
      "treas", "num.treas",
      "sec",   "num.sec",
      "mem",   "num.mem", 

      "URL", "OBJECT_ID"  )

      df <- df[ new.order ]
      
      
 
   
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
           title.mult.x = Multiple.Titles, 
           title.order = Num.Titles, 
           title.count = tot.titles,
           title.v1 = F9_07_COMP_DTK_TITLE, 
           title.raw = TITLE_RAW, 
           tot.hours = TOT.HOURS,
           tot.hours.incl.rltd = TOT.HOURS.TOT, 
           # hours.rank, 
           # hours.rank.incl.rltd = hours.rank.all,
           # hours.pct.of.max,
           # hours.pct.of.max.incl.rltd = hours.pct.of.max.all, 
           tot.comp = TOT.COMP, 
           tot.comp.incl.rltd = TOT.COMP.TOT, 
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
           outgoing.x = OUTGOING.X,
           ex.officio.x = EXOFFICIO.X, 
           co.x = CO.X,
           region.x = REGIONAL.X, 
           date.x = DATE.X, 
           sched.o.x = SCHED.O.X, 
           at.large.x = AT.LARGE.X,
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

