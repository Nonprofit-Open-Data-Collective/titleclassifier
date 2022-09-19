#Step 8: Categorize Titles
#based on old title categorization schema 

# 08-categorize-titles.R   



require(dplyr)

#' @title 
#' categorize titles function
#' 
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
  d.map <- googlesheets4::read_sheet( "1iYEY2HYDZTV0uvu35UuwdgAUQNKXSyab260pPPutP1M", sheet="title-mapping-xiaofei", range="A:T" )
  comp.data <- merge( comp.data, d.map, by.x="title.standard", by.y="title.standard", all.x=T )
  return( comp.data )
}


# categorize_titles <- function( comp.data )
# {
#   df <- comp.data
#   # Creates new Title Categories - These are NOT mutually exclusive.
#   ## ------------------------------------------------------------------------
#   
#   
#   df$CEO           <- 0
#   df$CFO           <- 0
#   df$Treasurer     <- 0
#   df$DEP.CEO       <- 0
#   df$SEC           <- 0
#   df$COO           <- 0
#   df$TRUST         <- 0
#   df$HUM.RES       <- 0
#   df$DEP.HEAD      <- 0
#   df$MAN           <- 0
#   df$DEV           <- 0
#   df$TECH          <- 0
#   df$COMM          <- 0
#   df$OTHER         <- 0
#   df$PROJECT       <- 0
#   df$LEGAL         <- 0
#   df$FACILITIES    <- 0
#   df$ADMIN.SUP     <- 0
#   df$MED.MAN       <- 0
#   df$HEALTH.HUM    <- 0
#   df$TRAIN         <- 0
#   df$ACADEMIC.MAN  <- 0
#   df$PROFESIONAL   <- 0
#   df$OTHER.PROF    <- 0
#   df$ACADEMIC.PROF <- 0
#   df$MED.PROF      <- 0
#   
#   
#   
#   
#   # ### CEO
#   # 
#   # First, assign individuals to CEO catagory if they had a title 
#   # that appeared in the 1000 most common titles that we coded to CEO.
#   ## ------------------------------------------------------------------------
#   
#   df$CEO.Prob <- 0
#   df$CEO[ df$TitleTxt7 %in% all.titles$CEO.Clear] <- 1
#   df$CEO.Prob[ df$TitleTxt7 %in% all.titles$CEO.Prob] <- 1
#   
#   
#   # TRUSTEE not CEO
#   
#   df$CEO   [ df$CEO.Prob == 1 & df$TrustOrDir == 1 & df$Officer == 0]   <- 0
#   df$TRUST [ df$CEO.Prob == 1 & df$TrustOrDir == 1 & df$Officer == 0]   <- 1
#   
#   
#   # CEO Not Trustee
#   
#   #  TrustOrDir          Officer      KeyEmpl      HighComp       Meaning
#   # --------------     ----------    ----------   -----------    -----------
#   # 1                   0             0            0              Board Probably not CEO
#   # 0                   1             0            0              CEO
#   # 1                   1             0            0              CEO
#   # 0                   0             1            0              Management
#   # 0                   0             0            1              Other
#   ## ------------------------------------------------------------------------
#   
#   df$CEO[ df$CEO.Prob == 1 & df$TrustOrDir == 0 & df$Officer == 1 ]   <- 1
#   df$CEO[ df$CEO.Prob == 1 & df$TrustOrDir == 1 & df$Officer == 1 ] <- 1
#   df$TRUST[ df$CEO.Prob == 1 & df$TrustOrDir == 1 & df$Officer == 1 ] <- 1
#   df$TRUST[ df$TitleTxt7 %in% all.titles$CEO.Board ] <- 1
#   df$CEO[ df$TitleTxt7 %in% all.titles$CEO.Board ] <- 1
#   df$MAN[ df$CEO.Prob == 1 & df$TrustOrDir == 0 & df$Officer == 0 & df$KeyEmpl == 1 ] <- 1
#   df$MAN[ df$CEO.Prob == 1 & df$TrustOrDir == 0 & df$Officer == 0 & df$HighComp == 1 ] <- 1
#   
#   
#   # 
#   # Next we want to assign individuals to the CEO catagory if they had one of the most common phrases associated with that title
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$CEO [ grepl( " CHIEF EXECUTIVE OFFICER ", df$TitleTxt7) ] <- 1
#   df$CEO [ grepl( " EXECUTIVE DIRECTOR ", df$TitleTxt7) ] <- 1
#   df$CEO [ grepl( " ASSISTANT EXECUTIVE DIRECTOR ", df$TitleTxt7) ] <- 0
#   
#   #new changes
#   df$CEO [ grepl( "CEO", df$TitleTxt7) ] <- 1
#   
#   
#   # df$TitleTxt7[ df$CEO == 1 ] %>% table() %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### HEAD OF SCHOOL
#   # 
#   #  TrustOrDir          Officer      KeyEmpl      HighComp       Meaning
#   # --------------     ----------    ----------   -----------    -----------
#   # 1                   0             0            0              Board Probably not CFO?
#   # 0                   1             0            0              CFO
#   # 1                   1             0            0              CFO
#   # 
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$TRUST [df$TitleTxt7 %in% all.titles$SCHOOL.HEAD & df$TrustOrDir == 1] <- 1
#   df$CEO [df$TitleTxt7 %in% all.titles$SCHOOL.HEAD & df$Officer == 1] <- 1
#   df$MAN [df$TitleTxt7 %in% all.titles$SCHOOL.HEAD & df$Officer == 0 & df$TrustOrDir == 0 & df$KeyEmpl == 1 ] <- 1
#   df$OTHER [df$TitleTxt7 %in% all.titles$SCHOOL.HEAD & df$Officer == 0 & df$TrustOrDir == 0 & df$KeyEmpl == 0 ] <- 1
#   
#   
#   
#   # ### CFO
#   # 
#   # 
#   #  TrustOrDir          Officer      KeyEmpl      HighComp       Meaning
#   # --------------     ----------    ----------   -----------    -----------
#   # 1                   0             0            0              Board Probably not CFO?
#   # 0                   1             0            0              CFO
#   # 1                   1             0            0              CFO
#   # 
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$CFO.Prob <- 0
#   
#   df$CFO[ df$TitleTxt7 %in% all.titles$CFO.Clear] <- 1
#   df$CFO.Prob[ df$TitleTxt7 %in% all.titles$CFO.Prob] <- 1
#   
#   df$CFO   [ df$CFO.Prob == 1 & df$TrustOrDir == 1]   <- 0
#   df$TRUST [ df$CFO.Prob == 1 & df$Officer == 1   ]   <- 1
#   df$CFO   [ df$CFO.Prob == 1 & df$KeyEmpl == 1]      <- 1
#   df$CFO   [ df$CFO.Prob == 1 & df$HighComp == 1]     <- 1
#   df$CFO   [ grepl( " CFO ", df$TitleTxt7) ]          <- 1
#   df$CFO   [ grepl( " CHIEF FINANCIAL OFFICER ", df$TitleTxt7) ]          <- 1
#   df$CFO   [ grepl( " FINANCE ", df$TitleTxt7) ]          <- 1
#   
#   df$CFO   [ grepl( " CHIEF FINANCE OFFICER ", df$TitleTxt7) ]          <- 1
#   df$CFO   [ grepl( "CFO", df$TitleTxt7) ]          <- 1
#   
#   df$TRUST[ df$CFO.Prob == 1 & df$TrustOrDir == 1 & df$Officer == 1] <- 1
#   
#   # titles <- df$TitleTxt7[ df$CFO == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### Treasurer
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$Treasurer  [ df$TitleTxt7 %in% all.titles$Treasurer] <- 1
#   df$Treasurer  [ grepl( " TREASURER ", df$TitleTxt7) ]   <- 1
#   
#   df$Treasurer  [ grepl( "TREASURER", df$TitleTxt7) ]   <- 1
#   
#   
#   # titles <- df$TitleTxt7[ df$Treasurer == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### Deputy CEO
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$DEP.CEO[ df$TitleTxt7 %in% all.titles$DEP.CEO ] <- 1
#   df$DEP.CEO[ df$TitleTxt7 %in% all.titles$DEP.CEO.Prob & df$Officer == 1 ] <- 1
#   
#   #df[ df$TitleTxt7 %in% all.titles$DEP.CEO.Prob, ]
#   
#   df$MAN [ df$TitleTxt7 %in% all.titles$DEP.CEO.Prob  & df$Officer == 0 & (df$KeyEmpl == 1 | df$HighComp == 1 ) ] <- 1
#   
#   # titles <- df$TitleTxt7[ df$DEP.CEO == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # 
#   # ### Secretary
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$SEC[ df$TitleTxt7 %in%  all.titles$SEC] <- 1
#   df$SEC [ grepl( " SECRETARY ", df$TitleTxt7) ] <- 1
#   df$SEC [ grepl( " SEC ", df$TitleTxt7) ] <- 1
#   
#   df$SEC [ grepl( "SECRETARY", df$TitleTxt7) ] <- 1
#   
#   # titles <- df$TitleTxt7[ df$SEC == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # 
#   # ### COO
#   # 
#   ## ------------------------------------------------------------------------
#   
#   # all.titles$COO.Prob
#   
#   df$COO[ df$TitleTxt7 %in%  all.titles$COO] <- 1
#   df$COO[ df$TitleTxt7 %in%  all.titles$COO] <- 1
#   df$COO [ grepl( " OPERATIONS ", df$TitleTxt7) ] <- 1
#   df$COO [ grepl( " COO ", df$TitleTxt7) ] <- 1
#   
#   df$COO [ grepl( " OPERATING ", df$TitleTxt7) ] <- 1
#   
#   # titles <- df$TitleTxt7[ df$COO == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### Director/Trustee
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$TRUST[ df$TitleTxt7 %in%  all.titles$TRUST] <- 1
#   df$TRUST [ grepl( " TRUSTEE ", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( " TRUST ", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( " BOARD MEMBER ", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "CHAIR", df$TitleTxt7) ] <- 1
#   df$TRUST [ df$TrustOrDir == 1 ] <- 1
#   
#   df$TRUST [ grepl( "BOARD", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "TRUSTEE", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "MEMBER", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "COUNCIL", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "COMMITTEE", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "GOVERNOR", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "REGENT", df$TitleTxt7) ] <- 1
#   
#   df$TRUST [ grepl( "^\\s*EX-OFFICIO\\s*$", df$TitleTxt5) ] <- 1
#   df$TRUST [ grepl( "DIRECTOR AT LARGE", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "^DIRECTOR$", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "PARLIAMENTARIAN", df$TitleTxt7) ] <- 1
#   df$TRUST [ grepl( "^\\s*AT LARGE\\s*", df$TitleTxt5) ] <- 1
#   df$TRUST [ grepl( "^\\s*EMERITUS\\s*$", df$TitleTxt5) ] <- 1
#   df$TRUST [ grepl( "HONORARY DIRECTOR", df$TitleTxt7) ] <- 1
#   
#   # titles <- df$TitleTxt7[ df$TRUST == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   # 
#   # ### Human Resources
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$HUM.RES[ df$TitleTxt7 %in%  all.titles$HUM.RES] <- 1
#   df$HUM.RES [ grepl( " HUMAN RESOURCES ", df$TitleTxt7) ] <- 1
#   df$HUM.RES [ grepl( " HUMAN RESOURCE ", df$TitleTxt7) ] <- 1
#   df$HUM.RES [ grepl( " STAFFING ", df$TitleTxt7) ] <- 1
#   
#   
#   # titles <- df$TitleTxt7[ df$HUM.RES == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### Communications
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$COMM[ df$TitleTxt7 %in%  all.titles$COMM] <- 1
#   df$COMM [ grepl( " COMMUNICATION ", df$TitleTxt7) ] <- 1
#   df$COMM [ grepl( " COMMUNICATIONS ", df$TitleTxt7) ] <- 1
#   df$COMM [ grepl( " MARKETING ", df$TitleTxt7) ] <- 1
#   df$COMM [ grepl( " EXTERNAL AFFAIRS ", df$TitleTxt7) ] <- 1
#   df$COMM [ grepl( " PUBLIC AFFAIRS ", df$TitleTxt7) ] <- 1
#   df$COMM [ grepl( " EXTERNAL AFFAIRS ", df$TitleTxt7) ] <- 1
#   df$COMM [ grepl( " RELATIONS ", df$TitleTxt7) ] <- 1
#   
#   df$COMM [ grepl( "PR", df$TitleTxt7) ] <- 1
#   
#   # titles <- df$TitleTxt7[ df$COMM == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   # ### Management
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$MAN[ df$TitleTxt7 %in%  all.titles$MAN] <- 1
#   
#   # titles <- df$TitleTxt7[ df$MAN == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   
#   # ### Technology/Information
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$TECH[ df$TitleTxt7 %in%  all.titles$TECH] <- 1
#   
#   df$TECH [ grepl( " CHIEF INFORMATION OFFICER ", df$TitleTxt7) ] <- 1
#   df$TECH [ grepl( " CIO ", df$TitleTxt7) ] <- 1
#   df$TECH [ grepl( " IT ", df$TitleTxt7) ] <- 1
#   df$TECH [ grepl( " INFORMATION ", df$TitleTxt7) ] <- 1
#   df$TECH [ grepl( " TECHNOLOGY ", df$TitleTxt7) ] <- 1
#   df$TECH [ grepl( " CDO ", df$TitleTxt7) ] <- 1
#   
#   
#   # titles <- df$TitleTxt7[ df$TECH == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### Development
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$DEV [ df$TitleTxt7 %in%  all.titles$DEV]       <- 1
#   df$DEV [ grepl( " DEVELOPMENT ", df$TitleTxt7) ]  <- 1
#   df$DEV [ grepl( " PHILANTHROPY ", df$TitleTxt7) ] <- 1
#   df$DEV [ grepl( " FUND ", df$TitleTxt7) ]         <- 1
#   df$DEV [ grepl( " FUNDRAISING ", df$TitleTxt7) ]  <- 1
#   df$DEV [ grepl( " MAJOR GIFTS ", df$TitleTxt7) ]  <- 1
#   
#   # titles <- df$TitleTxt7[ df$DEV == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### OTHER
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$OTHER [ df$TitleTxt7 %in%  all.titles$OTHER] <- 1
#   
#   df$OTHER [ grepl( "CLERK", df$TitleTxt7) ]  <- 1
#   df$OTHER [ grepl( "ADVISOR", df$TitleTxt7) ]  <- 1
#   # titles <- df$TitleTxt7[ df$OTHER == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   
#   # ### PROJECT
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$PROJECT [ df$TitleTxt7 %in%  all.titles$PROJECT] <- 1
#   
#   df$PROJECT [ grepl( " PROJECT ", df$TitleTxt7) ]  <- 1
#   df$PROJECT [ grepl( " PROJECTS ", df$TitleTxt7) ]  <- 1
#   df$PROJECT [ grepl( " PROGRAM ", df$TitleTxt7) ] <- 1
#   df$PROJECT [ grepl( " PROGRAMS ", df$TitleTxt7) ] <- 1
#   
#   
#   
#   # titles <- df$TitleTxt7[ df$PROJECT == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # 
#   # ### LEGAL
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$LEGAL [ df$TitleTxt7 %in%  all.titles$LEGAL] <- 1
#   # titles <- df$TitleTxt7[ df$LEGAL == 1]
#   
#   df$LEGAL [ grepl( " COUNSEL ", df$TitleTxt7) ]  <- 1
#   df$LEGAL [ grepl( " ATTORNEY ", df$TitleTxt7) ]  <- 1
#   df$LEGAL [ grepl( " COMPLIANCE ", df$TitleTxt7) ] <- 1
#   df$LEGAL [ grepl( " POLICY ", df$TitleTxt7) ] <- 1
#   df$LEGAL [ grepl( " LEGAL ", df$TitleTxt7) ] <- 1
#   df$LEGAL [ grepl( " LITIGATION ", df$TitleTxt7) ] <- 1
#   
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### FACILITIES
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$FACILITIES [ df$TitleTxt7 %in%  all.titles$FACILITIES] <- 1
#   
#   df$FACILITIES [ grepl( " FACILITIES ", df$TitleTxt7) ]  <- 1
#   df$FACILITIES [ grepl( " MAINTENANCE ", df$TitleTxt7) ]  <- 1
#   df$FACILITIES [ grepl( " FIELD ", df$TitleTxt7) ] <- 1
#   df$FACILITIES [ grepl( " FIELDS ", df$TitleTxt7) ] <- 1
#   df$FACILITIES [ grepl( " FACILITY ", df$TitleTxt7) ] <- 1
#   
#   df$FACILITIES [ grepl( " BUILDING ", df$TitleTxt7) ] <- 1
#   df$FACILITIES [ grepl( " BUILDINGS ", df$TitleTxt7) ] <- 1
#   
#   # titles <- df$TitleTxt7[ df$FACILITIES == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   
#   # ### ADMIN.SUP
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$ADMIN.SUP [ df$TitleTxt7 %in%  all.titles$ADMIN.SUP] <- 1
#   
#   df$ADMIN.SUP [ grepl( " SUPPORT ", df$TitleTxt7)  & grepl( " SERVICES ", df$TitleTxt7) ] <- 1
#   df$ADMIN.SUP [ grepl( " ADMINISTRATION ", df$TitleTxt7)  & grepl( " ASSISTANT ", df$TitleTxt7) ] <- 1
#   df$ADMIN.SUP [ grepl( " ADMINISTRATIVE ", df$TitleTxt7)  & grepl( " ASSISTANT ", df$TitleTxt7) ] <- 1
#   df$ADMIN.SUP [ grepl( " EXECUTIVE ", df$TitleTxt7)  & grepl( " ASSISTANT ", df$TitleTxt7) ] <- 1
#   
#   df$ADMIN.SUP [ grepl( " ADMINISTRATOR ", df$TitleTxt7)  & grepl( " ASSISTANT ", df$TitleTxt7) ] <- 1
#   
#   # titles <- df$TitleTxt7[ df$ADMIN.SUP == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### MED.MAN
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$MED.MAN [ df$TitleTxt7 %in%  all.titles$MED.MAN] <- 1
#   
#   
#   # titles <- df$TitleTxt7[ df$MED.MAN == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### HEALTH.HUM
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$HEALTH.HUM [ df$TitleTxt7 %in%  all.titles$HEALTH.HUM] <- 1
#   
#   df$HEALTH.HUM[ grepl( " CASE ", df$TitleTxt7)] <- 1
#   
#   df$HEALTH.HUM[ grepl( " SHELTER ", df$TitleTxt7)] <- 1
#   df$HEALTH.HUM[ grepl( " HOUSING ", df$TitleTxt7)] <- 1
#   df$HEALTH.HUM[ grepl( " SOCIAL WORKER ", df$TitleTxt7)] <- 1
#   
#   
#   # titles <- df$TitleTxt7[ df$HEALTH.HUM == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   # ### TRAIN
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$TRAIN [ df$TitleTxt7 %in%  all.titles$TRAIN] <- 1
#   
#   # titles <- df$TitleTxt7[ df$TRAIN == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   # ### Academic Management
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$ACADEMIC.MAN [ df$TitleTxt7 %in%  all.titles$ACADEMIC.MAN] <- 1
#   
#   # titles <- df$TitleTxt7[ df$ACADEMIC.MAN == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   # ### DEP.HEAD
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$DEP.HEAD [ df$TitleTxt7 %in%  all.titles$DEP.HEAD] <- 1
#   
#   # titles <- df$TitleTxt7[ df$DEP.HEAD == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   # ### PROFESIONAL
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$PROFESIONAL [ df$TitleTxt7 %in%  all.titles$PROFESIONAL] <- 1
#   
#   # titles <- df$TitleTxt7[ df$PROFESIONAL == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   
#   # ### OTHER PROF
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$OTHER.PROF [ df$TitleTxt7 %in%  all.titles$OTHER.PROF] <- 1
#   
#   # titles <- df$TitleTxt7[ df$OTHER.PROF == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   # ### ACADEMIC PROF
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$ACADEMIC.PROF [ df$TitleTxt7 %in%  all.titles$ACADEMIC.PROF] <- 1
#   
#   # titles <- df$TitleTxt7[ df$ACADEMIC.PROF == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   # ### MED PROF
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$MED.PROF [ df$TitleTxt7 %in%  all.titles$MED.PROF] <- 1
#   
#   # titles <- df$TitleTxt7[ df$MED.PROF == 1]
#   # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
#   
#   
#   # ### Common Missed Catagories.
#   ## ----------------------------------------------------------------
#   
#   
#   # " CHIEF PARTY "
#   
#   df$CEO [ grepl( " CHIEF PARTY ", df$TitleTxt7) & df$Officer == 1 ] <- 1
#   df$TRUST [ grepl( " CHIEF PARTY ", df$TitleTxt7) & df$FmrOfficer == 1  ] <- 1
#   df$MAN [ grepl( " CHIEF PARTY ", df$TitleTxt7) & df$Officer == 0  & df$FmrOfficer == 0 & df$KeyEmpl == 1] <- 1
#   df$OTHER [ grepl( " CHIEF PARTY ", df$TitleTxt7) & df$Officer == 0  & df$FmrOfficer == 0 & df$KeyEmpl ==  0] <- 1
#   
#   
#   df$OTHER [ grepl( " FORMER KEY EMPLOYEE ", df$TitleTxt7) ] <- 1
#   df$OTHER [ grepl( " ASSOCIATE PROFESSOR ", df$TitleTxt7) ] <- 1
#   df$MAN [ grepl( " FORMER DIRECTOR ", df$TitleTxt7) ] <- 1
#   df$OTHER [ grepl( " ASSISTANT PROFESSOR ", df$TitleTxt7) ] <- 1
#   df$MAN [ grepl( " FORMER VICE PRESIDENT ", df$TitleTxt7) ] <- 1
#   df$DEP.HEAD [ grepl( " DIRECTOR OF NURSING ", df$TitleTxt7) ] <- 1
#   df$DEP.HEAD [ grepl( " VICE PRESIDENT PROFESSIONAL SERVICES ", df$TitleTxt7) ] <- 1
#   df$DEP.HEAD [ grepl( " CHIEF PROFESSIONAL OFFICER ", df$TitleTxt7) ] <- 1
#   
#   df$OTHER [ grepl( " LAW PROFESSOR ", df$TitleTxt7) & df$TrustOrDir == 0 ] <- 1
#   df$TRUST [ grepl( " LAW PROFESSOR ", df$TitleTxt7) & df$TrustOrDir == 1] <- 1
#   
#   df$CEO [ grepl( " FORMER HEAD SCHOOL ", df$TitleTxt7) & df$Officer == 1 ] <- 1
#   df$TRUST [ grepl( " FORMER HEAD SCHOOL ", df$TitleTxt7) & df$FmrOfficer == 1  ] <- 1
#   df$MAN [ grepl( " FORMER HEAD SCHOOL ", df$TitleTxt7) & df$Officer == 0  & df$FmrOfficer == 0 ] <- 1
#   
#   
#   
#   df$CEO [ grepl( " CEO FORMER ", df$TitleTxt7) ] <- 1
#   df$MAN [ grepl( " DEAN PROFESSOR ", df$TitleTxt7) ] <- 1
#   df$MAN [ grepl( " FORMER EXECUTIVE VICE PRESIDENT ", df$TitleTxt7) ] <- 1
#   df$DEP.CEO [ grepl( " ASSISTANT CEO ", df$TitleTxt7) ] <- 1
#   
#   df$OTHER [ grepl( " PHARMACY ", df$TitleTxt7) ] <- 1
#   df$OTHER [ grepl( " PHARMACY ", df$TitleTxt7) ] <- 1
#   
#   df$MAN [ grepl( " REGIONAL MANAGER ", df$TitleTxt7) ] <- 1
#   df$DEP.HEAD [ grepl( " RN CASE MANAGER ", df$TitleTxt7) ] <- 1
#   
#   df$OTHER [ grepl( " SOCIAL WORKER ", df$TitleTxt7) & df$TrustOrDir == 0 ] <- 1
#   df$TRUST [ grepl( " SOCIAL WORKER ", df$TitleTxt7) & df$TrustOrDir == 1] <- 1
#   
#   df$DEP.HEAD [ grepl( " VICE PRESIDENT PATIENT CARE CNO ", df$TitleTxt7) ] <- 1
#   
#   df$OTHER [ grepl( " CONCERTMASTER ", df$TitleTxt7) ] <- 1
#   df$OTHER [ grepl( " CONCERTMASTER ", df$TitleTxt7) ] <- 1
#   
#   df$CEO [ grepl( " HS PRINCIPAL ", df$TitleTxt7) & df$Officer == 1 ] <- 1
#   df$TRUST [ grepl( " HS PRINCIPAL ", df$TitleTxt7) & df$FmrOfficer == 1  ] <- 1
#   df$MAN [ grepl( " HS PRINCIPAL ", df$TitleTxt7) & df$Officer == 0  & df$FmrOfficer == 0 ] <- 1
#   
#   df$MAN [ grepl( " SENIOR MANAGER ", df$TitleTxt7) ] <- 1
#   df$DEP.HEAD [ grepl( " VICE PRESIDENT CAO ", df$TitleTxt7) ] <- 1
#   df$LEGAL [ grepl( " VICE PRESIDENT LEGAL ", df$TitleTxt7) ] <- 1
#   
#   
#   
#   # ## Total Number of Titles Catagorized
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$Num.Titles <- df$CEO + df$CFO + df$Treasurer + df$DEP.CEO + df$SEC + df$COO + df$TRUST + df$HUM.RES + df$DEP.HEAD + df$MAN + df$DEV + df$OTHER +df$TECH + df$COMM + df$PROJECT + df$LEGAL + df$FACILITIES + df$ADMIN.SUP + df$MED.MAN + df$HEALTH.HUM + df$TRAIN + df$ACADEMIC.MAN + df$PROFESIONAL + df$OTHER.PROF + df$ACADEMIC.PROF + df$MED.PROF
#   
#   df$Catagorized <- ( df$Num.Titles >= 1 )
#   
#   df.missed <- df[ df$Catagorized != TRUE, ]
#   
#   cat("\nAfter Standardizing, cleaning, and categorizing all of the 50 most Common titles, we Categorized ", sum(df$Catagorized), " of ", nrow(df) , " titles. \n"  )
#   cat("This accounted for ", sum(df$Catagorized)/nrow(df), " of the Observations\n")
#   
#   print("categorize titles step complete")
#   
#   
#   
#   # ## Catagory Descriptive Statistics
#   # 
#   ## ------------------------------------------------------------------------
#   n <- nrow(df)
#   Cat.Desc <- matrix(2,2,2) %>% as.data.frame()
#   
#   Cat.Desc[1,] <- c(sum(df$CEO), sum(df$CEO)/n)
#   Cat.Desc[2,] <- c(sum(df$CFO), sum(df$CFO)/n)
#   Cat.Desc[3,] <- c(sum(df$Treasurer), sum(df$Treasurer)/n)
#   Cat.Desc[4,] <- c(sum(df$DEP.CEO), sum(df$DEP.CEO)/n)
#   Cat.Desc[5,] <- c(sum(df$SEC), sum(df$SEC)/n)
#   Cat.Desc[6,] <- c(sum(df$COO), sum(df$COO)/n)
#   Cat.Desc[7,] <- c(sum(df$TRUST), sum(df$TRUST)/n)
#   Cat.Desc[8,] <- c(sum(df$HUM.RES), sum(df$HUM.RES)/n)
#   Cat.Desc[9,] <- c(sum(df$DEP.HEAD), sum(df$DEP.HEAD)/n)
#   Cat.Desc[10,] <- c(sum(df$MAN), sum(df$MAN)/n)
#   Cat.Desc[11,] <- c(sum(df$DEV), sum(df$DEV)/n)
#   Cat.Desc[12,] <- c(sum(df$TECH), sum(df$TECH)/n)
#   Cat.Desc[13,] <- c(sum(df$COMM), sum(df$COMM)/n)
#   Cat.Desc[14,] <- c(sum(df$PROJECT), sum(df$PROJECT)/n)
#   Cat.Desc[15,] <- c(sum(df$LEGAL), sum(df$LEGAL)/n)
#   Cat.Desc[16,] <- c(sum(df$FACILITIES), sum(df$FACILITIES)/n)
#   Cat.Desc[17,] <- c(sum(df$ADMIN.SUP), sum(df$ADMIN.SUP)/n)
#   Cat.Desc[18,] <- c(sum(df$MED.MAN), sum(df$MED.MAN)/n)
#   Cat.Desc[19,] <- c(sum(df$HEALTH.HUM), sum(df$HEALTH.HUM)/n)
#   Cat.Desc[20,] <- c(sum(df$TRAIN), sum(df$TRAIN)/n)
#   Cat.Desc[21,] <- c(sum(df$ACADEMIC.MAN), sum(df$ACADEMIC.MAN)/n)
#   Cat.Desc[22,] <- c(sum(df$PROFESIONAL), sum(df$PROFESIONAL)/n)
#   Cat.Desc[23,] <- c(sum(df$OTHER.PROF), sum(df$OTHER.PROF)/n)
#   Cat.Desc[24,] <- c(sum(df$ACADEMIC.PROF), sum(df$ACADEMIC.PROF)/n)
#   Cat.Desc[25,] <- c(sum(df$MED.PROF), sum(df$MED.PROF)/n)
#   Cat.Desc[26,] <- c(sum(df$OTHER), sum(df$OTHER)/n)
#   Cat.Desc[27,] <- c(sum(df$Catagorized), sum(df$Catagorized)/n)
#   
#   Cats <- c( "CEO", "CFO", "Treasuer", "Deputy CEO ", "Secretary", 
#              "COO", "Trustee or BoD", "Human Resources", "Department Head", 
#              "Generic Management", "Development", "Technology", 
#              "Communications", "Project", "Legal", "Facilities", 
#              "Administrative Support", "Medical Management", 
#              "Health and Human Services", "Training", "Academic Management", 
#              "Professional", "Other Profession", "Academic Professional", 
#              "Medical Professional", "Other", "Total" )
#   
#   Cat.Desc <- cbind( Cats, Cat.Desc )
#   colnames( Cat.Desc ) <- c( "Catagories", "Total", "Percentage of obs" )
#   # Cat.Desc %>% pander()
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   # ## Positions and Roles 
#   # 
#   # ![Example of Positions and Roles Assignment](Title Position Roles.png)
#   # 
#   # 4 Positions:
#   # 
#   # - Management
#   # - HPP
#   # - Other Staff
#   # - Trustee
#   # 
#   # 4 Roles
#   # 
#   # - Board Leadership
#   # - C-Level/Officer
#   # - Interim 
#   # - Former 
#   
#   
#   
#   # ### Management
#   ## ------------------------------------------------------------------------
#   
#   df$Mgmt <- 0
#   
#   df$Mgmt[ df$CEO == 1          ] <- 1
#   df$Mgmt[ df$CFO == 1          ] <- 1
#   df$Mgmt[ df$DEP.CEO == 1      ] <- 1
#   df$Mgmt[ df$COO == 1          ] <- 1
#   df$Mgmt[ df$HUM.RES == 1      ] <- 1
#   df$Mgmt[ df$DEP.HEAD == 1     ] <- 1
#   df$Mgmt[ df$MAN == 1          ] <- 1
#   df$Mgmt[ df$DEV == 1          ] <- 1
#   df$Mgmt[ df$TECH == 1         ] <- 1
#   df$Mgmt[ df$COMM == 1         ] <- 1
#   df$Mgmt[ df$MED.MAN == 1      ] <- 1
#   df$Mgmt[ df$ACADEMIC.MAN == 1 ] <- 1
#   
#   
#   
#   # ### HPP
#   # 
#   ## ------------------------------------------------------------------------
#   
#   df$HPP <- 0
#   
#   df$HPP[ df$PROFESIONAL == 1 ] <- 1
#   df$HPP[ df$LEGAL == 1 ] <- 1
#   df$HPP[ df$PROJECT == 1 ] <- 1
#   df$HPP[ df$TRAIN == 1 ] <- 1
#   
#   
#   
#   # ### Other Staff
#   ## ------------------------------------------------------------------------
#   
#   df$Other.Staff <- 0
#   df$Other.Staff[ df$HPP == 0 & df$Mgmt == 0 & df$TRUST == 0] <- 1
#   
#   
#   
#   # ### Trustee
#   ## ------------------------------------------------------------------------
#   
#   df$Trustee <- 0
#   
#   
#   
#   # ### Board Leadership
#   ## ------------------------------------------------------------------------
#   
#   
#   df$Board.Leadership <- 0
#   
#   df$Board.Leadership[ df$CEO == 1 & df$TRUST == 1 ] <- 1
#   df$Board.Leadership[ df$COO == 1 & df$TRUST == 1 ] <- 1
#   df$Board.Leadership[ df$CFO == 1 & df$TRUST == 1 ] <- 1
#   df$Board.Leadership[ df$Treasurer == 1 & df$TRUST == 1 ] <- 1
#   df$Board.Leadership[ df$DEP.CEO == 1 & df$TRUST == 1 ] <- 1
#   df$Board.Leadership[ df$SEC == 1 & df$TRUST == 1 ] <- 1
#   df$Board.Leadership[ df$Officer == 1 & df$TRUST == 1 ] <- 1
#   
#   
#   
#   # ### C-Level / Officer
#   ## ------------------------------------------------------------------------
#   
#   df$C.Level <- 0
#   
#   df$C.Level [ grepl( " C[A-Z]O ", df$TitleTxt7) ] <- 1
#   df$C.Level [ grepl( " CHIEF ", df$TitleTxt7) ] <- 1
#   df$C.Level [ df$Officer == 1 ] <- 1
#   
#   
#   # ### Interim
#   ## ------------------------------------------------------------------------
#   
#   df$Interim <- 0
#   
#   df$Interim [ grepl( "SINCE", toupper(df$TitleTxt) ) ] <- 1
#   df$Interim [ grepl( "FROM", toupper(df$TitleTxt) ) ] <- 1
#   df$Interim [ grepl( "INTERIM", toupper(df$TitleTxt) ) ] <- 1
#   df$Interim [ grepl( "TEMP", toupper(df$TitleTxt) ) ] <- 1
#   df$Interim [ grepl( "TEMPORARY", toupper(df$TitleTxt) ) ] <- 1
#   
#   
#   
#   # ### Former
#   ## ------------------------------------------------------------------------
#   
#   df$Former <- 0
#   df$Former [ grepl( "FORMER",   toupper(df$TitleTxt) ) ] <- 1
#   df$Former [ grepl( "THRU",     toupper(df$TitleTxt) ) ] <- 1
#   df$Former [ grepl( "THROUGH", toupper(df$TitleTxt) ) ] <- 1
#   df$Former [ grepl( "PAST", toupper(df$TitleTxt) ) ] <- 1
#   df$Former [ df$FmrOfficer == 1 ] <- 1
#   
#   
#   
#   # ## Roles and Positions Descriptive Statistics
#   ## ------------------------------------------------------------------------
#   
#   n <- nrow(df)
#   RnP.Desc <- matrix(2,2,2) %>% as.data.frame()
#   
#   RnP.Desc[1,] <- c(sum(df$Mgmt), sum(df$Mgmt)/n)
#   RnP.Desc[2,] <- c(sum(df$HPP), sum(df$HPP)/n)
#   RnP.Desc[3,] <- c(sum(df$Other.Staff), sum(df$Other.Staff)/n)
#   RnP.Desc[4,] <- c(sum(df$Board.Leadership), sum(df$Board.Leadership)/n)
#   RnP.Desc[5,] <- c(sum(df$C.Level), sum(df$C.Level)/n)
#   RnP.Desc[6,] <- c(sum(df$Interim), sum(df$Interim)/n)
#   RnP.Desc[7,] <- c(sum(df$Former), sum(df$Former)/n)
#   
#   Cats <- c("Mgmt", "HPP", "Other Staff","Board Leadership", "C-Level", "Interim", "Former")
#   RnP.Desc <- cbind(Cats, RnP.Desc)
#   colnames(RnP.Desc) <- c("Roles and Positions", "Total", "Percentage of obs")
#   # RnP.Desc %>% pander()
#   
#   
#   return( df )
#   
# }








#' @title 
#' categorize ceo function
#' 
#' @description 
#' categorizing ceo's and adding a flag
#'
#' @param df A compensation data frame. 
#'
#' @return Returns a data frame with a CEO boolean (1=CEO,0=other). 
#'
#' @export
categorize_ceo <- function(df){
  
  df$CEO <- 0
  
  #ceo's (but not assistant or associate)
  df$CEO [grepl("CEO",df$TitleTxt7)] <- 1
  df$CEO [grepl("ASSOCIATE",df$TitleTxt7)] <- 0
  df$CEO [grepl("ASSISTANT",df$TitleTxt7)] <- 0
  
  #exec dir
  df$CEO [grepl("\\bEXECDIR[A-Z]*\\b",df$TitleTxt7)] <- 1
  df$CEO [grepl("\\bEXDIR[A-Z]*\\b",df$TitleTxt7)] <- 1
  
  #president (if paid and working 40+ hours)
  df$CEO [grepl("PRESIDENT",df$TitleTxt7) && df$AvgHrs >= 40 &&
            df$TOT_COMP > 0 && !grepl("VICE", dfTitleTxt7)] <- 1
  
  #weird ones
  df$CEO [agrepl("CHANCELLOR", df$TitleTxt7) && df$AvgHrs >= 40 &&
            df$TOT_COMP > 0 && 
            (df$Officer == "X" || df$FmrOfficer == "X")] <- 1
  df$CEO [grepl("MANAGING DIRECTOR", df$TitleTxt7) && df$AvgHrs >= 40 &&
            df$TOT_COMP > 0 && 
            (df$Officer == "X" || df$FmrOfficer == "X")] <- 1
  df$CEO [grepl("HEADMASTER", df$TitleTxt7) && df$AvgHrs >= 40 &&
            df$TOT_COMP > 0 && 
            (df$Officer == "X" || df$FmrOfficer == "X")] <- 1
  return(df)
}


#' @title 
#' categorize company leader function
#' 
#' @description 
#' utilizes ceo and adds in president, chair, and managing director for those 
#' without explicit/clear ceo positions
#'
#' @export
categorize_company_leader <- function(df){
  
  df <- categorize_ceo( df )
  df$Org.Leader <- 0
  df$Org.Leader[df$CEO == 1] <- 1
  aoc <- unique(df$NAME.x[df$Org.Leader == 1])
  
  #presidents
  pres.leadership <- c("PRESIDENT", "BOARD PRESIDENT", "PRESIDENT OF BOARD")
  df$Org.Leader[!df$NAME.x %in% aoc && 
                  ((grepl("PRESIDENT",df$TitleTxt7) && 
                      !grepl("VICE",df$TitleTxt7)) ||
                     df$TitleTxt7[i] %in% pres.leadership)] <- 1
  for(i in 1:length(df$NAME.x)){
    if(!(df$NAME.x[i] %in% aoc)){
      if((grepl("PRESIDENT",df$TitleTxt7[i]) && !grepl("VICE",df$TitleTxt7[i])) ||
         df$TitleTxt7[i] %in% pres.leadership){
        df$Org.Leader[i] <- 1
      }
    }
  }
  aoc <- unique(df$NAME.x[df$Org.Leader == 1])
  
  #chairs and misc
  chair.leadership <- c("CHAIR", "BOARD CHAIR",
                        "MANAGING DIRECTOR", "CHAIR OF BOARD")
  for(i in 1:length(df$NAME.x)){
    if(!(df$NAME.x[i] %in% aoc)){
      if(df$TitleTxt7[i] %in% chair.leadership){
        df$Org.Leader[i] <- 1
      }
    }
  }
  aoc <- unique(df$NAME.x[df$Org.Leader == 1])
  
  return(df)
}


# 
# categorize_cfo <- function(df){
#   
# }
