#old-R

#old function just for testing purposes

#' @title 
#' categorize titles function
#' DEPRECATED but to be updated
#' 
#' @description 
#' categorizes the titles in comp.data 
#' (based on TitleTxt2, so if named otherwise will have to rename)
#' note: copied from old-R/OLD-FUNCTIONS
#' 
#' to work well, it's helpful to remove the of's in titles
#' 
#' @export
categorize_titles <- function( comp.data )
{
  
  d2 <- comp.data
  
  
  # ### Load Categorized Titles
  ## ------------------------------------------------------------------------
  ##
  all.titles <- readRDS("data/all.titles.rds")
  
  
  
  # Creates new Title Categories - These are NOT mutually exclusive.
  ## ------------------------------------------------------------------------
  
  
  d2$CEO           <- 0
  d2$CFO           <- 0
  d2$Treasurer     <- 0
  d2$DEP.CEO       <- 0
  d2$SEC           <- 0
  d2$COO           <- 0
  d2$TRUST         <- 0
  d2$HUM.RES       <- 0
  d2$DEP.HEAD      <- 0
  d2$MAN           <- 0
  d2$DEV           <- 0
  d2$TECH          <- 0
  d2$COMM          <- 0
  d2$OTHER         <- 0
  d2$PROJECT       <- 0
  d2$LEGAL         <- 0
  d2$FACILITIES    <- 0
  d2$ADMIN.SUP     <- 0
  d2$MED.MAN       <- 0
  d2$HEALTH.HUM    <- 0
  d2$TRAIN         <- 0
  d2$ACADEMIC.MAN  <- 0
  d2$PROFESIONAL   <- 0
  d2$OTHER.PROF    <- 0
  d2$ACADEMIC.PROF <- 0
  d2$MED.PROF      <- 0
  
  
  
  
  # ### CEO
  # 
  # First, assign individuals to CEO catagory if they had a title that appeared in the 1000 most common titles that we coded to CEO.
  ## ------------------------------------------------------------------------
  
  d2$CEO.Prob <- 0
  d2$CEO[ d2$TitleTxt2 %in% all.titles$CEO.Clear] <- 1
  d2$CEO.Prob[ d2$TitleTxt2 %in% all.titles$CEO.Prob] <- 1
  
  
  # TRUSTEE not CEO
  
  d2$CEO   [ d2$CEO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 0]   <- 0
  d2$TRUST [ d2$CEO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 0]   <- 1
  
  
  # CEO Not Trustee
  
  #  TrustOrDir          Officer      KeyEmpl      HighComp       Meaning
  # --------------     ----------    ----------   -----------    -----------
  # 1                   0             0            0              Board Probably not CEO
  # 0                   1             0            0              CEO
  # 1                   1             0            0              CEO
  # 0                   0             1            0              Management
  # 0                   0             0            1              Other
  ## ------------------------------------------------------------------------
  
  d2$CEO[ d2$CEO.Prob == 1 & d2$TrustOrDir == 0 & d2$Officer == 1 ]   <- 1
  d2$CEO[ d2$CEO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 1 ] <- 1
  d2$TRUST[ d2$CEO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 1 ] <- 1
  d2$TRUST[ d2$TitleTxt2 %in% all.titles$CEO.Board ] <- 1
  d2$CEO[ d2$TitleTxt2 %in% all.titles$CEO.Board ] <- 1
  d2$MAN[ d2$CEO.Prob == 1 & d2$TrustOrDir == 0 & d2$Officer == 0 & d2$KeyEmpl == 1 ] <- 1
  d2$MAN[ d2$CEO.Prob == 1 & d2$TrustOrDir == 0 & d2$Officer == 0 & d2$HighComp == 1 ] <- 1
  
  
  # 
  # Next we want to assign individuals to the CEO catagory if they had one of the most common phrases associated with that title
  # 
  ## ------------------------------------------------------------------------
  
  d2$CEO [ grepl( " CHIEF EXECUTIVE OFFICER ", d2$TitleTxt2) ] <- 1
  d2$CEO [ grepl( " EXECUTIVE DIRECTOR ", d2$TitleTxt2) ] <- 1
  d2$CEO [ grepl( " ASSISTANT EXECUTIVE DIRECTOR ", d2$TitleTxt2) ] <- 0
  
  #new changes
  d2$CEO [ grepl( "CEO", d2$TitleTxt2) ] <- 1
  
  
  # d2$TitleTxt2[ d2$CEO == 1 ] %>% table() %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### HEAD OF SCHOOL
  # 
  #  TrustOrDir          Officer      KeyEmpl      HighComp       Meaning
  # --------------     ----------    ----------   -----------    -----------
  # 1                   0             0            0              Board Probably not CFO?
  # 0                   1             0            0              CFO
  # 1                   1             0            0              CFO
  # 
  # 
  ## ------------------------------------------------------------------------
  
  d2$TRUST [d2$TitleTxt2 %in% all.titles$SCHOOL.HEAD & d2$TrustOrDir == 1] <- 1
  d2$CEO [d2$TitleTxt2 %in% all.titles$SCHOOL.HEAD & d2$Officer == 1] <- 1
  d2$MAN [d2$TitleTxt2 %in% all.titles$SCHOOL.HEAD & d2$Officer == 0 & d2$TrustOrDir == 0 & d2$KeyEmpl == 1 ] <- 1
  d2$OTHER [d2$TitleTxt2 %in% all.titles$SCHOOL.HEAD & d2$Officer == 0 & d2$TrustOrDir == 0 & d2$KeyEmpl == 0 ] <- 1
  
  
  
  # ### CFO
  # 
  # 
  #  TrustOrDir          Officer      KeyEmpl      HighComp       Meaning
  # --------------     ----------    ----------   -----------    -----------
  # 1                   0             0            0              Board Probably not CFO?
  # 0                   1             0            0              CFO
  # 1                   1             0            0              CFO
  # 
  # 
  ## ------------------------------------------------------------------------
  
  d2$CFO.Prob <- 0
  
  d2$CFO[ d2$TitleTxt2 %in% all.titles$CFO.Clear] <- 1
  d2$CFO.Prob[ d2$TitleTxt2 %in% all.titles$CFO.Prob] <- 1
  
  d2$CFO   [ d2$CFO.Prob == 1 & d2$TrustOrDir == 1]   <- 0
  d2$TRUST [ d2$CFO.Prob == 1 & d2$Officer == 1   ]   <- 1
  d2$CFO   [ d2$CFO.Prob == 1 & d2$KeyEmpl == 1]      <- 1
  d2$CFO   [ d2$CFO.Prob == 1 & d2$HighComp == 1]     <- 1
  d2$CFO   [ grepl( " CFO ", d2$TitleTxt2) ]          <- 1
  d2$CFO   [ grepl( " CHIEF FINANCIAL OFFICER ", d2$TitleTxt2) ]          <- 1
  d2$CFO   [ grepl( " FINANCE ", d2$TitleTxt2) ]          <- 1
  
  d2$CFO   [ grepl( " CHIEF FINANCE OFFICER ", d2$TitleTxt2) ]          <- 1
  d2$CFO   [ grepl( "CFO", d2$TitleTxt2) ]          <- 1
  
  d2$TRUST[ d2$CFO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 1] <- 1
  
  # titles <- d2$TitleTxt2[ d2$CFO == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### Treasurer
  # 
  ## ------------------------------------------------------------------------
  
  d2$Treasurer  [ d2$TitleTxt2 %in% all.titles$Treasurer] <- 1
  d2$Treasurer  [ grepl( " TREASURER ", d2$TitleTxt2) ]   <- 1
  
  d2$Treasurer  [ grepl( "TREASURER", d2$TitleTxt2) ]   <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$Treasurer == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### Deputy CEO
  # 
  ## ------------------------------------------------------------------------
  
  d2$DEP.CEO[ d2$TitleTxt2 %in% all.titles$DEP.CEO ] <- 1
  d2$DEP.CEO[ d2$TitleTxt2 %in% all.titles$DEP.CEO.Prob & d2$Officer == 1 ] <- 1
  
  #d2[ d2$TitleTxt2 %in% all.titles$DEP.CEO.Prob, ]
  
  d2$MAN [ d2$TitleTxt2 %in% all.titles$DEP.CEO.Prob  & d2$Officer == 0 & (d2$KeyEmpl == 1 | d2$HighComp == 1 ) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$DEP.CEO == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # 
  # ### Secretary
  # 
  ## ------------------------------------------------------------------------
  
  d2$SEC[ d2$TitleTxt2 %in%  all.titles$SEC] <- 1
  d2$SEC [ grepl( " SECRETARY ", d2$TitleTxt2) ] <- 1
  d2$SEC [ grepl( " SEC ", d2$TitleTxt2) ] <- 1
  
  d2$SEC [ grepl( "SECRETARY", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$SEC == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # 
  # ### COO
  # 
  ## ------------------------------------------------------------------------
  
  # all.titles$COO.Prob
  
  d2$COO[ d2$TitleTxt2 %in%  all.titles$COO] <- 1
  d2$COO[ d2$TitleTxt2 %in%  all.titles$COO] <- 1
  d2$COO [ grepl( " OPERATIONS ", d2$TitleTxt2) ] <- 1
  d2$COO [ grepl( " COO ", d2$TitleTxt2) ] <- 1
  
  d2$COO [ grepl( " OPERATING ", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$COO == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### Director/Trustee
  # 
  ## ------------------------------------------------------------------------
  
  d2$TRUST[ d2$TitleTxt2 %in%  all.titles$TRUST] <- 1
  d2$TRUST [ grepl( " TRUSTEE ", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( " TRUST ", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( " BOARD MEMBER ", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( "CHAIR", d2$TitleTxt2) ] <- 1
  d2$TRUST [ d2$TrustOrDir == 1 ] <- 1
  
  d2$TRUST [ grepl( "BOARD", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( "TRUSTEE", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( "MEMBER", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( "COUNCIL", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( "COMMITTEE", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( "GOVERNOR", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( "REGENT", d2$TitleTxt2) ] <- 1
  
  d2$TRUST [ grepl( "EX-OFFICIO", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( "DIRECTOR AT LARGE", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$TRUST == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  # 
  # ### Human Resources
  # 
  ## ------------------------------------------------------------------------
  
  d2$HUM.RES[ d2$TitleTxt2 %in%  all.titles$HUM.RES] <- 1
  d2$HUM.RES [ grepl( " HUMAN RESOURCES ", d2$TitleTxt2) ] <- 1
  d2$HUM.RES [ grepl( " HUMAN RESOURCE ", d2$TitleTxt2) ] <- 1
  d2$HUM.RES [ grepl( " STAFFING ", d2$TitleTxt2) ] <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$HUM.RES == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### Communications
  # 
  ## ------------------------------------------------------------------------
  
  d2$COMM[ d2$TitleTxt2 %in%  all.titles$COMM] <- 1
  d2$COMM [ grepl( " COMMUNICATION ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " COMMUNICATIONS ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " MARKETING ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " EXTERNAL AFFAIRS ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " PUBLIC AFFAIRS ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " EXTERNAL AFFAIRS ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " RELATIONS ", d2$TitleTxt2) ] <- 1
  
  d2$COMM [ grepl( "PR", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$COMM == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  # ### Management
  # 
  ## ------------------------------------------------------------------------
  
  d2$MAN[ d2$TitleTxt2 %in%  all.titles$MAN] <- 1
  
  # titles <- d2$TitleTxt2[ d2$MAN == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  
  # ### Technology/Information
  # 
  ## ------------------------------------------------------------------------
  
  d2$TECH[ d2$TitleTxt2 %in%  all.titles$TECH] <- 1
  
  d2$TECH [ grepl( " CHIEF INFORMATION OFFICER ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " CIO ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " IT ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " INFORMATION ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " TECHNOLOGY ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " CDO ", d2$TitleTxt2) ] <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$TECH == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### Development
  # 
  ## ------------------------------------------------------------------------
  
  d2$DEV [ d2$TitleTxt2 %in%  all.titles$DEV]       <- 1
  d2$DEV [ grepl( " DEVELOPMENT ", d2$TitleTxt2) ]  <- 1
  d2$DEV [ grepl( " PHILANTHROPY ", d2$TitleTxt2) ] <- 1
  d2$DEV [ grepl( " FUND ", d2$TitleTxt2) ]         <- 1
  d2$DEV [ grepl( " FUNDRAISING ", d2$TitleTxt2) ]  <- 1
  d2$DEV [ grepl( " MAJOR GIFTS ", d2$TitleTxt2) ]  <- 1
  
  # titles <- d2$TitleTxt2[ d2$DEV == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### OTHER
  # 
  ## ------------------------------------------------------------------------
  
  d2$OTHER [ d2$TitleTxt2 %in%  all.titles$OTHER] <- 1
  
  d2$OTHER [ grepl( "CLERK", d2$TitleTxt2) ]  <- 1
  d2$OTHER [ grepl( "ADVISOR", d2$TitleTxt2) ]  <- 1
  # titles <- d2$TitleTxt2[ d2$OTHER == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  
  # ### PROJECT
  # 
  ## ------------------------------------------------------------------------
  
  d2$PROJECT [ d2$TitleTxt2 %in%  all.titles$PROJECT] <- 1
  
  d2$PROJECT [ grepl( " PROJECT ", d2$TitleTxt2) ]  <- 1
  d2$PROJECT [ grepl( " PROJECTS ", d2$TitleTxt2) ]  <- 1
  d2$PROJECT [ grepl( " PROGRAM ", d2$TitleTxt2) ] <- 1
  d2$PROJECT [ grepl( " PROGRAMS ", d2$TitleTxt2) ] <- 1
  
  
  
  # titles <- d2$TitleTxt2[ d2$PROJECT == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # 
  # ### LEGAL
  # 
  ## ------------------------------------------------------------------------
  
  d2$LEGAL [ d2$TitleTxt2 %in%  all.titles$LEGAL] <- 1
  # titles <- d2$TitleTxt2[ d2$LEGAL == 1]
  
  d2$LEGAL [ grepl( " COUNSEL ", d2$TitleTxt2) ]  <- 1
  d2$LEGAL [ grepl( " ATTORNEY ", d2$TitleTxt2) ]  <- 1
  d2$LEGAL [ grepl( " COMPLIANCE ", d2$TitleTxt2) ] <- 1
  d2$LEGAL [ grepl( " POLICY ", d2$TitleTxt2) ] <- 1
  d2$LEGAL [ grepl( " LEGAL ", d2$TitleTxt2) ] <- 1
  d2$LEGAL [ grepl( " LITIGATION ", d2$TitleTxt2) ] <- 1
  
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### FACILITIES
  # 
  ## ------------------------------------------------------------------------
  
  d2$FACILITIES [ d2$TitleTxt2 %in%  all.titles$FACILITIES] <- 1
  
  d2$FACILITIES [ grepl( " FACILITIES ", d2$TitleTxt2) ]  <- 1
  d2$FACILITIES [ grepl( " MAINTENANCE ", d2$TitleTxt2) ]  <- 1
  d2$FACILITIES [ grepl( " FIELD ", d2$TitleTxt2) ] <- 1
  d2$FACILITIES [ grepl( " FIELDS ", d2$TitleTxt2) ] <- 1
  d2$FACILITIES [ grepl( " FACILITY ", d2$TitleTxt2) ] <- 1
  
  d2$FACILITIES [ grepl( " BUILDING ", d2$TitleTxt2) ] <- 1
  d2$FACILITIES [ grepl( " BUILDINGS ", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$FACILITIES == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  
  # ### ADMIN.SUP
  # 
  ## ------------------------------------------------------------------------
  
  d2$ADMIN.SUP [ d2$TitleTxt2 %in%  all.titles$ADMIN.SUP] <- 1
  
  d2$ADMIN.SUP [ grepl( " SUPPORT ", d2$TitleTxt2)  & grepl( " SERVICES ", d2$TitleTxt2) ] <- 1
  d2$ADMIN.SUP [ grepl( " ADMINISTRATION ", d2$TitleTxt2)  & grepl( " ASSISTANT ", d2$TitleTxt2) ] <- 1
  d2$ADMIN.SUP [ grepl( " ADMINISTRATIVE ", d2$TitleTxt2)  & grepl( " ASSISTANT ", d2$TitleTxt2) ] <- 1
  d2$ADMIN.SUP [ grepl( " EXECUTIVE ", d2$TitleTxt2)  & grepl( " ASSISTANT ", d2$TitleTxt2) ] <- 1
  
  d2$ADMIN.SUP [ grepl( " ADMINISTRATOR ", d2$TitleTxt2)  & grepl( " ASSISTANT ", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$ADMIN.SUP == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### MED.MAN
  # 
  ## ------------------------------------------------------------------------
  
  d2$MED.MAN [ d2$TitleTxt2 %in%  all.titles$MED.MAN] <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$MED.MAN == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### HEALTH.HUM
  # 
  ## ------------------------------------------------------------------------
  
  d2$HEALTH.HUM [ d2$TitleTxt2 %in%  all.titles$HEALTH.HUM] <- 1
  
  d2$HEALTH.HUM[ grepl( " CASE ", d2$TitleTxt2)] <- 1
  
  d2$HEALTH.HUM[ grepl( " SHELTER ", d2$TitleTxt2)] <- 1
  d2$HEALTH.HUM[ grepl( " HOUSING ", d2$TitleTxt2)] <- 1
  d2$HEALTH.HUM[ grepl( " SOCIAL WORKER ", d2$TitleTxt2)] <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$HEALTH.HUM == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  # ### TRAIN
  # 
  ## ------------------------------------------------------------------------
  
  d2$TRAIN [ d2$TitleTxt2 %in%  all.titles$TRAIN] <- 1
  
  # titles <- d2$TitleTxt2[ d2$TRAIN == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  # ### Academic Management
  # 
  ## ------------------------------------------------------------------------
  
  d2$ACADEMIC.MAN [ d2$TitleTxt2 %in%  all.titles$ACADEMIC.MAN] <- 1
  
  # titles <- d2$TitleTxt2[ d2$ACADEMIC.MAN == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  # ### DEP.HEAD
  # 
  ## ------------------------------------------------------------------------
  
  d2$DEP.HEAD [ d2$TitleTxt2 %in%  all.titles$DEP.HEAD] <- 1
  
  # titles <- d2$TitleTxt2[ d2$DEP.HEAD == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  # ### PROFESIONAL
  # 
  ## ------------------------------------------------------------------------
  
  d2$PROFESIONAL [ d2$TitleTxt2 %in%  all.titles$PROFESIONAL] <- 1
  
  # titles <- d2$TitleTxt2[ d2$PROFESIONAL == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  # ### OTHER PROF
  # 
  ## ------------------------------------------------------------------------
  
  d2$OTHER.PROF [ d2$TitleTxt2 %in%  all.titles$OTHER.PROF] <- 1
  
  # titles <- d2$TitleTxt2[ d2$OTHER.PROF == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  # ### ACADEMIC PROF
  # 
  ## ------------------------------------------------------------------------
  
  d2$ACADEMIC.PROF [ d2$TitleTxt2 %in%  all.titles$ACADEMIC.PROF] <- 1
  
  # titles <- d2$TitleTxt2[ d2$ACADEMIC.PROF == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  # ### MED PROF
  # 
  ## ------------------------------------------------------------------------
  
  d2$MED.PROF [ d2$TitleTxt2 %in%  all.titles$MED.PROF] <- 1
  
  # titles <- d2$TitleTxt2[ d2$MED.PROF == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  # ### Common Missed Catagories.
  ## ----------------------------------------------------------------
  
  
  # " CHIEF PARTY "
  
  d2$CEO [ grepl( " CHIEF PARTY ", d2$TitleTxt2) & d2$Officer == 1 ] <- 1
  d2$TRUST [ grepl( " CHIEF PARTY ", d2$TitleTxt2) & d2$FmrOfficer == 1  ] <- 1
  d2$MAN [ grepl( " CHIEF PARTY ", d2$TitleTxt2) & d2$Officer == 0  & d2$FmrOfficer == 0 & d2$KeyEmpl == 1] <- 1
  d2$OTHER [ grepl( " CHIEF PARTY ", d2$TitleTxt2) & d2$Officer == 0  & d2$FmrOfficer == 0 & d2$KeyEmpl ==  0] <- 1
  
  
  d2$OTHER [ grepl( " FORMER KEY EMPLOYEE ", d2$TitleTxt2) ] <- 1
  d2$OTHER [ grepl( " ASSOCIATE PROFESSOR ", d2$TitleTxt2) ] <- 1
  d2$MAN [ grepl( " FORMER DIRECTOR ", d2$TitleTxt2) ] <- 1
  d2$OTHER [ grepl( " ASSISTANT PROFESSOR ", d2$TitleTxt2) ] <- 1
  d2$MAN [ grepl( " FORMER VICE PRESIDENT ", d2$TitleTxt2) ] <- 1
  d2$DEP.HEAD [ grepl( " DIRECTOR OF NURSING ", d2$TitleTxt2) ] <- 1
  d2$DEP.HEAD [ grepl( " VICE PRESIDENT PROFESSIONAL SERVICES ", d2$TitleTxt2) ] <- 1
  d2$DEP.HEAD [ grepl( " CHIEF PROFESSIONAL OFFICER ", d2$TitleTxt2) ] <- 1
  
  d2$OTHER [ grepl( " LAW PROFESSOR ", d2$TitleTxt2) & d2$TrustOrDir == 0 ] <- 1
  d2$TRUST [ grepl( " LAW PROFESSOR ", d2$TitleTxt2) & d2$TrustOrDir == 1] <- 1
  
  d2$CEO [ grepl( " FORMER HEAD SCHOOL ", d2$TitleTxt2) & d2$Officer == 1 ] <- 1
  d2$TRUST [ grepl( " FORMER HEAD SCHOOL ", d2$TitleTxt2) & d2$FmrOfficer == 1  ] <- 1
  d2$MAN [ grepl( " FORMER HEAD SCHOOL ", d2$TitleTxt2) & d2$Officer == 0  & d2$FmrOfficer == 0 ] <- 1
  
  
  
  d2$CEO [ grepl( " CEO FORMER ", d2$TitleTxt2) ] <- 1
  d2$MAN [ grepl( " DEAN PROFESSOR ", d2$TitleTxt2) ] <- 1
  d2$MAN [ grepl( " FORMER EXECUTIVE VICE PRESIDENT ", d2$TitleTxt2) ] <- 1
  d2$DEP.CEO [ grepl( " ASSISTANT CEO ", d2$TitleTxt2) ] <- 1
  
  d2$OTHER [ grepl( " PHARMACY ", d2$TitleTxt2) ] <- 1
  d2$OTHER [ grepl( " PHARMACY ", d2$TitleTxt2) ] <- 1
  
  d2$MAN [ grepl( " REGIONAL MANAGER ", d2$TitleTxt2) ] <- 1
  d2$DEP.HEAD [ grepl( " RN CASE MANAGER ", d2$TitleTxt2) ] <- 1
  
  d2$OTHER [ grepl( " SOCIAL WORKER ", d2$TitleTxt2) & d2$TrustOrDir == 0 ] <- 1
  d2$TRUST [ grepl( " SOCIAL WORKER ", d2$TitleTxt2) & d2$TrustOrDir == 1] <- 1
  
  d2$DEP.HEAD [ grepl( " VICE PRESIDENT PATIENT CARE CNO ", d2$TitleTxt2) ] <- 1
  
  d2$OTHER [ grepl( " CONCERTMASTER ", d2$TitleTxt2) ] <- 1
  d2$OTHER [ grepl( " CONCERTMASTER ", d2$TitleTxt2) ] <- 1
  
  d2$CEO [ grepl( " HS PRINCIPAL ", d2$TitleTxt2) & d2$Officer == 1 ] <- 1
  d2$TRUST [ grepl( " HS PRINCIPAL ", d2$TitleTxt2) & d2$FmrOfficer == 1  ] <- 1
  d2$MAN [ grepl( " HS PRINCIPAL ", d2$TitleTxt2) & d2$Officer == 0  & d2$FmrOfficer == 0 ] <- 1
  
  d2$MAN [ grepl( " SENIOR MANAGER ", d2$TitleTxt2) ] <- 1
  d2$DEP.HEAD [ grepl( " VICE PRESIDENT CAO ", d2$TitleTxt2) ] <- 1
  d2$LEGAL [ grepl( " VICE PRESIDENT LEGAL ", d2$TitleTxt2) ] <- 1
  
  
  
  # ## Total Number of Titles Catagorized
  # 
  ## ------------------------------------------------------------------------
  
  d2$Num.Titles <- d2$CEO + d2$CFO + d2$Treasurer + d2$DEP.CEO + d2$SEC + d2$COO + d2$TRUST + d2$HUM.RES + d2$DEP.HEAD + d2$MAN + d2$DEV + d2$OTHER +d2$TECH + d2$COMM + d2$PROJECT + d2$LEGAL + d2$FACILITIES + d2$ADMIN.SUP + d2$MED.MAN + d2$HEALTH.HUM + d2$TRAIN + d2$ACADEMIC.MAN + d2$PROFESIONAL + d2$OTHER.PROF + d2$ACADEMIC.PROF + d2$MED.PROF
  
  d2$Catagorized <- ( d2$Num.Titles >= 1 )
  
  d2.missed <- d2[ d2$Catagorized != TRUE, ]
  
  cat("\nAfter Standardizing, cleaning, and categorizing all of the 50 most Common titles, we Categorized ", sum(d2$Catagorized), " of ", nrow(d2) , " titles. \n"  )
  cat("This accounted for ", sum(d2$Catagorized)/nrow(d2), " of the Observations\n")
  
  
  
  
  # ## Catagory Descriptive Statistics
  # 
  ## ------------------------------------------------------------------------
  n <- nrow(d2)
  Cat.Desc <- matrix(2,2,2) %>% as.data.frame()
  
  Cat.Desc[1,] <- c(sum(d2$CEO), sum(d2$CEO)/n)
  Cat.Desc[2,] <- c(sum(d2$CFO), sum(d2$CFO)/n)
  Cat.Desc[3,] <- c(sum(d2$Treasurer), sum(d2$Treasurer)/n)
  Cat.Desc[4,] <- c(sum(d2$DEP.CEO), sum(d2$DEP.CEO)/n)
  Cat.Desc[5,] <- c(sum(d2$SEC), sum(d2$SEC)/n)
  Cat.Desc[6,] <- c(sum(d2$COO), sum(d2$COO)/n)
  Cat.Desc[7,] <- c(sum(d2$TRUST), sum(d2$TRUST)/n)
  Cat.Desc[8,] <- c(sum(d2$HUM.RES), sum(d2$HUM.RES)/n)
  Cat.Desc[9,] <- c(sum(d2$DEP.HEAD), sum(d2$DEP.HEAD)/n)
  Cat.Desc[10,] <- c(sum(d2$MAN), sum(d2$MAN)/n)
  Cat.Desc[11,] <- c(sum(d2$DEV), sum(d2$DEV)/n)
  Cat.Desc[12,] <- c(sum(d2$TECH), sum(d2$TECH)/n)
  Cat.Desc[13,] <- c(sum(d2$COMM), sum(d2$COMM)/n)
  Cat.Desc[14,] <- c(sum(d2$PROJECT), sum(d2$PROJECT)/n)
  Cat.Desc[15,] <- c(sum(d2$LEGAL), sum(d2$LEGAL)/n)
  Cat.Desc[16,] <- c(sum(d2$FACILITIES), sum(d2$FACILITIES)/n)
  Cat.Desc[17,] <- c(sum(d2$ADMIN.SUP), sum(d2$ADMIN.SUP)/n)
  Cat.Desc[18,] <- c(sum(d2$MED.MAN), sum(d2$MED.MAN)/n)
  Cat.Desc[19,] <- c(sum(d2$HEALTH.HUM), sum(d2$HEALTH.HUM)/n)
  Cat.Desc[20,] <- c(sum(d2$TRAIN), sum(d2$TRAIN)/n)
  Cat.Desc[21,] <- c(sum(d2$ACADEMIC.MAN), sum(d2$ACADEMIC.MAN)/n)
  Cat.Desc[22,] <- c(sum(d2$PROFESIONAL), sum(d2$PROFESIONAL)/n)
  Cat.Desc[23,] <- c(sum(d2$OTHER.PROF), sum(d2$OTHER.PROF)/n)
  Cat.Desc[24,] <- c(sum(d2$ACADEMIC.PROF), sum(d2$ACADEMIC.PROF)/n)
  Cat.Desc[25,] <- c(sum(d2$MED.PROF), sum(d2$MED.PROF)/n)
  Cat.Desc[26,] <- c(sum(d2$OTHER), sum(d2$OTHER)/n)
  Cat.Desc[27,] <- c(sum(d2$Catagorized), sum(d2$Catagorized)/n)
  
  Cats <- c( "CEO", "CFO", "Treasuer", "Deputy CEO ", "Secretary", 
             "COO", "Trustee or BoD", "Human Resources", "Department Head", 
             "Generic Management", "Development", "Technology", 
             "Communications", "Project", "Legal", "Facilities", 
             "Administrative Support", "Medical Management", 
             "Health and Human Services", "Training", "Academic Management", 
             "Professional", "Other Profession", "Academic Professional", 
             "Medical Professional", "Other", "Total" )
  
  Cat.Desc <- cbind( Cats, Cat.Desc )
  colnames( Cat.Desc ) <- c( "Catagories", "Total", "Percentage of obs" )
  # Cat.Desc %>% pander()
  
  
  
  
  
  
  
  
  
  # ## Positions and Roles 
  # 
  # ![Example of Positions and Roles Assignment](Title Position Roles.png)
  # 
  # 4 Positions:
  # 
  # - Management
  # - HPP
  # - Other Staff
  # - Trustee
  # 
  # 4 Roles
  # 
  # - Board Leadership
  # - C-Level/Officer
  # - Interim 
  # - Former 
  
  
  
  # ### Management
  ## ------------------------------------------------------------------------
  
  d2$Mgmt <- 0
  
  d2$Mgmt[ d2$CEO == 1          ] <- 1
  d2$Mgmt[ d2$CFO == 1          ] <- 1
  d2$Mgmt[ d2$DEP.CEO == 1      ] <- 1
  d2$Mgmt[ d2$COO == 1          ] <- 1
  d2$Mgmt[ d2$HUM.RES == 1      ] <- 1
  d2$Mgmt[ d2$DEP.HEAD == 1     ] <- 1
  d2$Mgmt[ d2$MAN == 1          ] <- 1
  d2$Mgmt[ d2$DEV == 1          ] <- 1
  d2$Mgmt[ d2$TECH == 1         ] <- 1
  d2$Mgmt[ d2$COMM == 1         ] <- 1
  d2$Mgmt[ d2$MED.MAN == 1      ] <- 1
  d2$Mgmt[ d2$ACADEMIC.MAN == 1 ] <- 1
  
  
  
  # ### HPP
  # 
  ## ------------------------------------------------------------------------
  
  d2$HPP <- 0
  
  d2$HPP[ d2$PROFESIONAL == 1 ] <- 1
  d2$HPP[ d2$LEGAL == 1 ] <- 1
  d2$HPP[ d2$PROJECT == 1 ] <- 1
  d2$HPP[ d2$TRAIN == 1 ] <- 1
  
  
  
  # ### Other Staff
  ## ------------------------------------------------------------------------
  
  d2$Other.Staff <- 0
  d2$Other.Staff[ d2$HPP == 0 & d2$Mgmt == 0 & d2$TRUST == 0] <- 1
  
  
  
  # ### Trustee
  ## ------------------------------------------------------------------------
  
  d2$Trustee <- 0
  
  
  
  # ### Board Leadership
  ## ------------------------------------------------------------------------
  
  
  d2$Board.Leadership <- 0
  
  d2$Board.Leadership[ d2$CEO == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$COO == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$CFO == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$Treasurer == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$DEP.CEO == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$SEC == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$Officer == 1 & d2$TRUST == 1 ] <- 1
  
  
  
  # ### C-Level / Officer
  ## ------------------------------------------------------------------------
  
  d2$C.Level <- 0
  
  d2$C.Level [ grepl( " C[A-Z]O ", d2$TitleTxt2) ] <- 1
  d2$C.Level [ grepl( " CHIEF ", d2$TitleTxt2) ] <- 1
  d2$C.Level [ d2$Officer == 1 ] <- 1
  
  
  # ### Interim
  ## ------------------------------------------------------------------------
  
  d2$Interim <- 0
  
  d2$Interim [ grepl( "SINCE", toupper(d2$TitleTxt) ) ] <- 1
  d2$Interim [ grepl( "FROM", toupper(d2$TitleTxt) ) ] <- 1
  d2$Interim [ grepl( "INTERIM", toupper(d2$TitleTxt) ) ] <- 1
  d2$Interim [ grepl( "TEMP", toupper(d2$TitleTxt) ) ] <- 1
  d2$Interim [ grepl( "TEMPORARY", toupper(d2$TitleTxt) ) ] <- 1
  
  
  
  # ### Former
  ## ------------------------------------------------------------------------
  
  d2$Former <- 0
  d2$Former [ grepl( "FORMER",   toupper(d2$TitleTxt) ) ] <- 1
  d2$Former [ grepl( "THRU",     toupper(d2$TitleTxt) ) ] <- 1
  d2$Former [ grepl( "THROUGH", toupper(d2$TitleTxt) ) ] <- 1
  d2$Former [ grepl( "PAST", toupper(d2$TitleTxt) ) ] <- 1
  d2$Former [ d2$FmrOfficer == 1 ] <- 1
  
  
  
  # ## Roles and Positions Descriptive Statistics
  ## ------------------------------------------------------------------------
  
  n <- nrow(d2)
  RnP.Desc <- matrix(2,2,2) %>% as.data.frame()
  
  RnP.Desc[1,] <- c(sum(d2$Mgmt), sum(d2$Mgmt)/n)
  RnP.Desc[2,] <- c(sum(d2$HPP), sum(d2$HPP)/n)
  RnP.Desc[3,] <- c(sum(d2$Other.Staff), sum(d2$Other.Staff)/n)
  RnP.Desc[4,] <- c(sum(d2$Board.Leadership), sum(d2$Board.Leadership)/n)
  RnP.Desc[5,] <- c(sum(d2$C.Level), sum(d2$C.Level)/n)
  RnP.Desc[6,] <- c(sum(d2$Interim), sum(d2$Interim)/n)
  RnP.Desc[7,] <- c(sum(d2$Former), sum(d2$Former)/n)
  
  Cats <- c("Mgmt", "HPP", "Other Staff","Board Leadership", "C-Level", "Interim", "Former")
  RnP.Desc <- cbind(Cats, RnP.Desc)
  colnames(RnP.Desc) <- c("Roles and Positions", "Total", "Percentage of obs")
  # RnP.Desc %>% pander()
  
  
  return( d2 )
  
}