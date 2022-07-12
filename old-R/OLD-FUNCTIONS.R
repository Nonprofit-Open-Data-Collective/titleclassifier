### PREPARE DF

format_comp_df <- function( comp.dat )
{
  
  
  #' Re-name new version so variable names work with script:
  ## ------------------------------------------------------------------------
  # names( dat )
  # 
  # "FilingId"    
  # "FilerEIN"    
  # "TaxYr"       
  # "Amended"     
  # "FilerName1"  
  # "FilerName2"  
  # "PdBeginDt"   
  # "PdEndDt"    
  # 
  # "Org501c3"    
  # "Org501cInd"  
  # "Org501cType" 
  # "Org4947a1"   
  # "Org527Ind"   
  # 
  # "FormYr"      
  # "PersonNm"    
  # "TitleTxt"   
  # "AvgHrs"      
  # "AvgHrsRltd"  
  # "TrustOrDir"  
  # "Officer"     
  # "KeyEmpl"     
  # "HighComp"    
  # "FmrOfficer"  
  # "RptCmpOrg"  
  # "RptCmpRltd"  
  # "OtherComp" 
  
  
  d2 <- 
    dat %>%
    rename( 
      FilingId = OBJECT_ID,                  
      FilerEIN = EIN,                      
      FilerName1 = NAME,                          
      FormYr = TAXYR,                           
      FORMTYPE  = FORMTYPE,                   
      URL = URL,                      
      PersonNm = F9_07_PZ_DTK_NAME,
      TitleTxt = F9_07_PZ_DTK_TITLE,          
      AvgHrs = F9_07_PZ_DTK_AVE_HOURS_WEEK, 
      TrustOrDir = F9_07_PC_DTK_POS_TRUSTEE_INDIV,
      Officer = F9_07_PC_DTK_POS_OFFICER,    
      RptCmpOrg = F9_07_PZ_COMP_DIRECT,     
      RptCmpRltd = F9_07_PZ_COMP_RELATED,  
      OtherComp = F9_07_PZ_COMP_OTHER,    
      KeyEmpl = F9_07_PC_DTK_POS_KEY_EMPLOYEE,
      HighComp = F9_07_PC_DTK_POS_HIGH_COMP_EMP,
      FmrOfficer = F9_07_PC_DTK_POS_FORMER )  
  # F9_07_PC_DTK_POS_TRUSTEE_INST = F9_07_PC_DTK_POS_TRUSTEE_INST, 
  # AvgHrsRltd = F9_07_PZ_DTK_AVE_HOURS_WEEK_RLTD,
  # F9_07_EZ_COMP_BENF = F9_07_EZ_COMP_BENF )
  
  
  #' Clean up main variables:
  ## ------------------------------------------------------------------------
  
  d2$RptCmpOrg  <- as.numeric( d2$RptCmpOrg )
  d2$RptCmpRltd <- as.numeric( d2$RptCmpRltd )
  d2$OtherComp  <- as.numeric( d2$OtherComp )
  
  d2$RptCmpOrg[ is.na(d2$RptCmpOrg) ]   <- 0
  d2$RptCmpRltd[ is.na(d2$RptCmpRltd) ] <- 0
  d2$OtherComp[ is.na(d2$OtherComp) ]   <- 0
  
  d2$totalComp <- d2$RptCmpOrg + d2$RptCmpRltd + d2$OtherComp
  
  d2$AvgHrs <- as.numeric( d2$AvgHrs )
  d2$AvgHrs[ is.na(d2$AvgHrs) ]   <- 0
  
  d2$TitleTxt[ is.na(d2$TitleTxt) ] <- ""
  
  d2$TrustOrDir[ is.na(d2$TrustOrDir) ] <- "" 
  d2$Officer[ is.na(d2$Officer) ] <- "" 
  d2$KeyEmpl[ is.na(d2$KeyEmpl) ] <- ""  
  d2$HighComp[ is.na(d2$HighComp) ] <- "" 
  d2$FmrOfficer[ is.na(d2$FmrOfficer) ] <- ""
  
  return( d2 )
  
}






#' ## DATA CLEANING


remove_punctuation <- function( title.text )
{
  
  
  TitleTxt <- title.text 
  
  #' ### String Processing
  #' 
  #' * Add Space to Start and end of Strings
  #' 
  ## ------------------------------------------------------------------------
  
  TitleTxt <- TitleTxt %>% toupper()
  TitleTxt <- str_c( " ",TitleTxt )
  TitleTxt <- str_c( TitleTxt," " )
  
  #' 
  #' * Remove punctuation 
  #' 
  ## ------------------------------------------------------------------------
  
  
  TitleTxt <- gsub( "[[:punct:]]", " ", TitleTxt )
  
  
  #' 
  #' * Replace / with &
  #' 
  #' * Remove dates and digits
  #' 
  ## ------------------------------------------------------------------------
  TitleTxt <- gsub( "C0", "CO", TitleTxt )
  TitleTxt <- gsub( "[0-9]+", " ", TitleTxt )
  
  #' 
  #' * Remove Spaces
  #' 
  ## ------------------------------------------------------------------------
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  
  
  return( TitleTxt )
  
}







standardize_titles <- function( title.text )
{
  
  TitleTxt <- title.text
  
  #' ### Vice Presidents
  #' 
  ## ------------------------------------------------------------------------
  
  #Vice President
  TitleTxt <- gsub( " AVP ", " ASSISTANT VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " A VP ", " ASSISTANT VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " A V P ", " ASSISTANT VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " VC ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " VP ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " V P ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " V P ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " V PRES ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " V PRESI ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " V PRESID ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " V PRESIDE ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " V PRESIDEN ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " V PRESIDENT ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " VICEPRESIDENT ", " VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " EVP ", " EXECUTIVE VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " E VP ", " EXECUTIVE VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " E V P ", " EXECUTIVE VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " E V PRESIDENT ", " EXECUTIVE VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " E VICE ", " EXECUTIVE VICE ", TitleTxt )
  TitleTxt <- gsub( " SVP ", " SENIOR VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " SRVP ", " SENIOR VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " S VP ", " SENIOR VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " S V P ", " SENIOR VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " S VICE ", " SENIOR VICE ", TitleTxt )
  TitleTxt <- gsub( " E S V P ", " EXECUTIVE SENIOR VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " E S VP ", " EXECUTIVE SENIOR VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " E SVP ", " EXECUTIVE SENIOR VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " ESVP ", " EXECUTIVE SENIOR VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " AVP ", " ASSOCIATE VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " S E V P ", " SENIOR EXECUTIVE VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " S E VP ", " SENIOR EXECUTIVE VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " S EVP ", " SENIOR EXECUTIVE VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " SEVP ", " SENIOR EXECUTIVE VICE PRESIDENT ", TitleTxt )
  
  TitleTxt <- gsub( " V P F ", "  VICE PRESIDENT FINANCE ", TitleTxt )
  TitleTxt <- gsub( " VPF ", "  VICE PRESIDENT FINANCE ", TitleTxt )
  TitleTxt <- gsub( " V P O ", "  VICE PRESIDENT OPERATIONS ", TitleTxt )
  TitleTxt <- gsub( " V P O ", "  VICE PRESIDENT OPERATIONS ", TitleTxt )
  
  TitleTxt <- gsub( " VPHR ", "  VICE PRESIDENT HUMAN RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " V P H R ", "  VICE PRESIDENT HUMAN RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " VPMA ", "  VICE PRESIDENT MEDICAL AFFAIRS ", TitleTxt )
  TitleTxt <- gsub( " V P M A ", "  VICE PRESIDENT MEDICAL AFFAIRS ", TitleTxt )
  TitleTxt <- gsub( " VP", "  VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " PRESIDENT PRESIDENT ", " PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " PRESIDENT PRESIDENT ", " PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " VICE PRESIDENT VICE PRESIDENT ", " VICE PRESIDENT ", TitleTxt )
  
  
  #' 
  #' 
  #' ### Convert To Uniform Titles
  #' 
  ## ------------------------------------------------------------------------
  #EXECUTIVE
  TitleTxt <- gsub( " EXC ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " EXE ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " EXEC ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " EXECU ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " EXECUT ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " EXECUTI ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " EXECUTIV ", " EXECUTIVE ", TitleTxt )
  
  #Director
  TitleTxt <- gsub( " DI ", " DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " DIR ", " DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " DIRE ", " DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " DIREC ", " DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " DIRECT ", " DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " DIRECTO ", " DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " DIRECTOR", " DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " DIRCTR", " DIRECTOR ", TitleTxt )
  
  #Operations
  TitleTxt <- gsub( " OP ", " OPERATIONS ", TitleTxt )
  TitleTxt <- gsub( " OPS ", " OPERATIONS ", TitleTxt )
  TitleTxt <- gsub( " OPE ", " OPERATIONS ", TitleTxt )
  TitleTxt <- gsub( " OPER ", " OPERATIONS ", TitleTxt )
  TitleTxt <- gsub( " OPERA ", " OPERATIONS ", TitleTxt )
  TitleTxt <- gsub( " OPERAT ", " OPERATIONS ", TitleTxt )
  TitleTxt <- gsub( " OPERATI ", " OPERATIONS ", TitleTxt )
  TitleTxt <- gsub( " OPERATIO ", " OPERATIONS ", TitleTxt )
  TitleTxt <- gsub( " OPERATION ", " OPERATIONS ", TitleTxt )
  
  #ASSITANT
  TitleTxt <- gsub( " ASST ", " ASSISTANT ", TitleTxt )
  TitleTxt <- gsub( " ASSOC ", " ASSOCIATE ", TitleTxt )
  
  #PRESIDENT
  TitleTxt <- gsub( " PRES ", " PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " PRESI ", " PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " PRESID ", " PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " PRESIDE ", " PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " PRESIDEN ", " PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " PRESIDENT", " PRESIDENT ", TitleTxt )
  
  #SECRETARY
  TitleTxt <- gsub( " SEC ", " SECRETARY ", TitleTxt )
  TitleTxt <- gsub( " SECR ", " SECRETARY ", TitleTxt )
  TitleTxt <- gsub( " SECRE ", " SECRETARY ", TitleTxt )
  TitleTxt <- gsub( " SECRET ", " SECRETARY ", TitleTxt )
  TitleTxt <- gsub( " SECRETA ", " SECRETARY ", TitleTxt )
  TitleTxt <- gsub( " SECRETAR ", " SECRETARY ", TitleTxt )
  
  #TREASURER
  TitleTxt <- gsub( " TR ", " TREASURER ", TitleTxt )
  TitleTxt <- gsub( " TRE ", " TREASURER ", TitleTxt )
  TitleTxt <- gsub( " TREA ", " TREASURER ", TitleTxt )
  TitleTxt <- gsub( " TREAS ", " TREASURER ", TitleTxt )
  TitleTxt <- gsub( " TREASU ", " TREASURER ", TitleTxt )
  TitleTxt <- gsub( " TREASUR ", " TREASURER ", TitleTxt )
  TitleTxt <- gsub( " TREASURE ", " TREASURER ", TitleTxt )
  
  #FINANCE
  TitleTxt <- gsub( " FIN ", " FINANCE ", TitleTxt )
  TitleTxt <- gsub( " FINA ", " FINANCE ", TitleTxt )
  TitleTxt <- gsub( " FINAN ", " FINANCE ", TitleTxt )
  TitleTxt <- gsub( " FINANC ", " FINANCE ", TitleTxt )
  
  #SENIOR
  TitleTxt <- gsub( " SR ", " SENIOR ", TitleTxt )
  TitleTxt <- gsub( " SEN ", " SENIOR ", TitleTxt )
  TitleTxt <- gsub( " SENI ", " SENIOR ", TitleTxt )
  TitleTxt <- gsub( " SENIO ", " SENIOR ", TitleTxt )
  
  #Development
  TitleTxt <- gsub( " DEV ", " DEVELOPMENT ", TitleTxt )
  TitleTxt <- gsub( " DEVE ", " DEVELOPMENT ", TitleTxt )
  TitleTxt <- gsub( " DEVEL ", " DEVELOPMENT ", TitleTxt )
  TitleTxt <- gsub( " DEVELO ", " DEVELOPMENT ", TitleTxt )
  TitleTxt <- gsub( " DEVELOP ", " DEVELOPMENT ", TitleTxt )
  TitleTxt <- gsub( " DEVELOPM ", " DEVELOPMENT ", TitleTxt )
  TitleTxt <- gsub( " DEVELOPME ", " DEVELOPMENT ", TitleTxt )
  TitleTxt <- gsub( " DEVELOPMEN ", " DEVELOPMENT ", TitleTxt )
  
  #VICE CHAIR
  TitleTxt <- gsub( " V C ", " VICE CHAIR ", TitleTxt )
  TitleTxt <- gsub( " VC ", " VICE CHAIR ", TitleTxt )
  TitleTxt <- gsub( " V CHAIR ", " VICE CHAIR ", TitleTxt )
  TitleTxt <- gsub( " VICE C ", " VICE CHAIR ", TitleTxt )
  
  TitleTxt <- gsub( " CHAIRPERSON ", " CHAIR ", TitleTxt )
  TitleTxt <- gsub( " CHAIRWOMAN ", " CHAIR ", TitleTxt )
  TitleTxt <- gsub( " CHAIRMAN ", " CHAIR ", TitleTxt )
  
  #Condense Abbreviations
  TitleTxt <- gsub( " C E O ", " CEO ", TitleTxt )
  TitleTxt <- gsub( " C O O ", " COO ", TitleTxt )
  TitleTxt <- gsub( " C F O ", " CFO ", TitleTxt )
  TitleTxt <- gsub( " C D O ", " CDO ", TitleTxt )
  TitleTxt <- gsub( " C T O ", " CTO ", TitleTxt )
  TitleTxt <- gsub( " C A O ", " CAO ", TitleTxt )
  TitleTxt <- gsub( " C I O ", " CIO ", TitleTxt )
  TitleTxt <- gsub( " C R N A ", " CRNA ", TitleTxt )
  TitleTxt <- gsub( " C N O ", " CNO ", TitleTxt )
  TitleTxt <- gsub( " C M O ", " CMO ", TitleTxt )
  
  
  #EXECUTIVE DIRECTOR
  
  TitleTxt <- gsub( " E D ", " EXECUTIVE DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " E DIRECTOR ", " EXECUTIVE DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " EXECUTIVE EXECUTIVE ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " KEY E ", " KEY EMPLOYEE", TitleTxt )
  TitleTxt <- gsub( " EXECUTIVE DIRECTOR EXECUTIVE DIRECTOR ", " EXECUTIVE DIRECTOR ", TitleTxt )
  
  
  # FMR OFCR
  TitleTxt <- gsub( " FMR ", " FORMER ", TitleTxt )
  TitleTxt <- gsub( " OFCR ", " OFFICER ", TitleTxt )
  TitleTxt <- gsub( " OFF ", " OFFICER ", TitleTxt )
  TitleTxt <- gsub( " OFFI ", " OFFICER ", TitleTxt )
  TitleTxt <- gsub( " OFFIC ", " OFFICER ", TitleTxt )
  TitleTxt <- gsub( " OFFICE ", " OFFICER ", TitleTxt )
  TitleTxt <- gsub( " OFFICO ", " OFFICER ", TitleTxt )
  TitleTxt <- gsub( " FORNER ", " FORMER ", TitleTxt )
  
  TitleTxt <- gsub( " BOARD MEMBERDEPT CHAIR ", " BOARD MEMBER DEPT CHAIR ", TitleTxt )
  
  #ADMIN
  TitleTxt <- gsub( " ADMIN ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " ADMINI ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " ADMINIS ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " ADMINIST ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " ADMINISTR ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " ADMINISTRA ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " ADMINISTRAT ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " ADMINISTRATI ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " ADMINISTRATIO ", " ADMINISTRATION ", TitleTxt )
  
  TitleTxt <- gsub( " ADMINISTRATO ", " ADMINISTRATOR ", TitleTxt )
  
  #COORDINATOR
  TitleTxt <- gsub( " COOR ", " COORDINATOR ", TitleTxt )
  TitleTxt <- gsub( " COORD ", " COORDINATOR ", TitleTxt )
  TitleTxt <- gsub( " COORDI ", " COORDINATOR ", TitleTxt )
  TitleTxt <- gsub( " COORDIN ", " COORDINATOR ", TitleTxt )
  TitleTxt <- gsub( " COORDINA ", " COORDINATOR ", TitleTxt )
  TitleTxt <- gsub( " COORDINAT ", " COORDINATOR ", TitleTxt )
  TitleTxt <- gsub( " COORDINATO ", " COORDINATOR ", TitleTxt )
  
  #STRATEGY
  
  TitleTxt <- gsub( " STRAT ", " STRATEGY ", TitleTxt )
  TitleTxt <- gsub( " STRATE ", " STRATEGY ", TitleTxt )
  TitleTxt <- gsub( " STRATEG ", " STRATEGY ", TitleTxt )
  TitleTxt <- gsub( " STRATEGI ", " STRATEGY ", TitleTxt )
  TitleTxt <- gsub( " STRATEGIC ", " STRATEGY ", TitleTxt )
  
  #HUMAN RESOURCES
  
  TitleTxt <- gsub( " HU ", " HUMAN ", TitleTxt ) 
  TitleTxt <- gsub( " HUM ", " HUMAN ", TitleTxt )
  TitleTxt <- gsub( " HUMA ", " HUMAN ", TitleTxt )
  TitleTxt <- gsub( " HR ", " HUMAN RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " RES ", " RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " RESO ", " RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " RESOU ", " RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " RESOUR ", " RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " RESOURC ", " RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " RESOURCE ", " RESOURCES ", TitleTxt )
  
  
  #MANAGEMENT
  TitleTxt <- gsub( " MAN ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MANA ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MANAG ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MANAGE ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MANAGE ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MANAGEM ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MANAGEME ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MANAGEMEN ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MANAGI ", " MANAGING ", TitleTxt )
  TitleTxt <- gsub( " MANAGIN ", " MANAGING ", TitleTxt )
  TitleTxt <- gsub( " MGMT ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MGM ", " MANAGEMENT ", TitleTxt )
  
  TitleTxt <- gsub( " MANGER ", " MANAGER ", TitleTxt )
  TitleTxt <- gsub( " MGR ", " MANAGER ", TitleTxt )
  
  
  #PROGRAM
  TitleTxt <- gsub( " PROG ", " PROGRAM ", TitleTxt )
  TitleTxt <- gsub( " PROGR ", " PROGRAM ", TitleTxt )
  TitleTxt <- gsub( " PROGRA ", " PROGRAM ", TitleTxt )
  TitleTxt <- gsub( " PROGRAMS ", " PROGRAM ", TitleTxt )
  
  TitleTxt <- gsub( " PROJ ", " PROJECT ", TitleTxt )
  TitleTxt <- gsub( " PROJE ", " PROJECT ", TitleTxt )
  TitleTxt <- gsub( " PROJEC ", " PROJECT ", TitleTxt )
  TitleTxt <- gsub( " PROJECTS ", " PROJECT ", TitleTxt )
  
  #NA
  TitleTxt[TitleTxt == " N A "] <- NA
  TitleTxt[TitleTxt == " NA "] <- NA
  
  #PUBLIC
  TitleTxt <- gsub( " PUB ", " PUBLIC ", TitleTxt )
  TitleTxt <- gsub( " PUBL ", " PUBLIC ", TitleTxt )
  TitleTxt <- gsub( " PUBLI ", " PUBLIC ", TitleTxt )
  
  #BUSINESS
  TitleTxt <- gsub( " BUS ", " BUSINESS ", TitleTxt )
  TitleTxt <- gsub( " BUSI ", " BUSINESS ", TitleTxt )
  TitleTxt <- gsub( " BUSI ", " BUSINESS ", TitleTxt )
  TitleTxt <- gsub( " BUSIN ", " BUSINESS ", TitleTxt )
  TitleTxt <- gsub( " BUSINE ", " BUSINESS ", TitleTxt )
  TitleTxt <- gsub( " BUSINES ", " BUSINESS ", TitleTxt )
  
  #HUMAN RESOURCES
  TitleTxt <- gsub( " HR ", "  HUMAN RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " H R ", "  HUMAN RESOURCES ", TitleTxt )
  
  # COMMUNICATIONS
  TitleTxt <- gsub( " COM ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMM ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMMU ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMMUN ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMMUNI ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMMUNIC ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMMUNICA ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMMUNICAT ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMMUNICATI ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMMUNICATIO ", "  COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " COMMUNICATION ", "  COMMUNICATIONS ", TitleTxt )
  
  #INFORMATION
  TitleTxt <- gsub( " INF ", "  INFORMATION ", TitleTxt )
  TitleTxt <- gsub( " INFO ", "  INFORMATION ", TitleTxt )
  TitleTxt <- gsub( " INFOR ", "  INFORMATION ", TitleTxt )
  TitleTxt <- gsub( " INFORM ", "  INFORMATION ", TitleTxt )
  TitleTxt <- gsub( " INFORMA ", "  INFORMATION ", TitleTxt )
  TitleTxt <- gsub( " INFORMAT ", "  INFORMATION ", TitleTxt )
  TitleTxt <- gsub( " INFORMATI ", "  INFORMATION ", TitleTxt )
  TitleTxt <- gsub( " INFORMATIO ", "  INFORMATION ", TitleTxt )
  
  
  #INTELLIGENCE
  TitleTxt <- gsub( " INT ", "  INTELLIGENCE ", TitleTxt )
  TitleTxt <- gsub( " INTE ", "  INTELLIGENCE ", TitleTxt )
  TitleTxt <- gsub( " INTEL ", "  INTELLIGENCE ", TitleTxt )
  TitleTxt <- gsub( " INTELL ", "  INTELLIGENCE ", TitleTxt )
  TitleTxt <- gsub( " INTELLI ", "  INTELLIGENCE ", TitleTxt )
  TitleTxt <- gsub( " INTELLIG ", "  INTELLIGENCE ", TitleTxt )
  TitleTxt <- gsub( " INTELLIGE ", "  INTELLIGENCE ", TitleTxt )
  TitleTxt <- gsub( " INTELLIGENC ", "  INTELLIGENCE ", TitleTxt )
  
  #TECHNOLOGY
  TitleTxt <- gsub( " TECH ", "  TECHNOLOGY ", TitleTxt )
  TitleTxt <- gsub( " TECHN ", "  TECHNOLOGY ", TitleTxt )
  TitleTxt <- gsub( " TECHNO ", "  TECHNOLOGY ", TitleTxt )
  TitleTxt <- gsub( " TECHNOL ", "  TECHNOLOGY ", TitleTxt )
  TitleTxt <- gsub( " TECHNOLO ", "  TECHNOLOGY ", TitleTxt )
  TitleTxt <- gsub( " TECHNOLOG ", "  TECHNOLOGY ", TitleTxt )
  
  #INSTITUTION
  TitleTxt <- gsub( " INST ", "  INSTITUTION ", TitleTxt )
  TitleTxt <- gsub( " INSTI ", "  INSTITUTION ", TitleTxt )
  TitleTxt <- gsub( " INSTIT ", "  INSTITUTION ", TitleTxt )
  TitleTxt <- gsub( " INSTITU ", "  INSTITUTION ", TitleTxt )
  TitleTxt <- gsub( " INSTITUT ", "  INSTITUTION ", TitleTxt )
  TitleTxt <- gsub( " INSTITUTI ", "  INSTITUTION ", TitleTxt )
  TitleTxt <- gsub( " INSTITUTIO ", "  INSTITUTION ", TitleTxt )
  
  #ACADEMIC
  TitleTxt <- gsub( " ACAD ", "  ACADEMICS ", TitleTxt )
  TitleTxt <- gsub( " ACADE ", "  ACADEMICS ", TitleTxt )
  TitleTxt <- gsub( " ACADEM ", "  ACADEMICS ", TitleTxt )
  TitleTxt <- gsub( " ACADEMI ", "  ACADEMICS ", TitleTxt )
  TitleTxt <- gsub( " ACADEMIC ", "  ACADEMIC ", TitleTxt )
  
  #MARKETING
  TitleTxt <- gsub( " MARK ", " MARKETING ", TitleTxt )
  TitleTxt <- gsub( " MARKE ", " MARKETING ", TitleTxt )
  TitleTxt <- gsub( " MARKET ", " MARKETING ", TitleTxt )
  TitleTxt <- gsub( " MARKETI ", " MARKETING ", TitleTxt )
  TitleTxt <- gsub( " MARKETIN ", " MARKETING ", TitleTxt )
  
  #ADVANCEMENT
  TitleTxt <- gsub( " ADV ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVA ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVAN ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVANC ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVANCE ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVANCEM ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVANCEME ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVANCEMEN ", " ADVANCEMENT ", TitleTxt )
  
  # PHILANTHROPY
  TitleTxt <- gsub( " PHIL ", " PHILANTHROPY ", TitleTxt )
  TitleTxt <- gsub( " PHILA ", " PHILANTHROPY ", TitleTxt )
  TitleTxt <- gsub( " PHILAN ", " PHILANTHROPY ", TitleTxt )
  TitleTxt <- gsub( " PHILANT ", " PHILANTHROPY ", TitleTxt )
  TitleTxt <- gsub( " PHILANTH ", " PHILANTHROPY ", TitleTxt )
  TitleTxt <- gsub( " PHILANTHR ", " PHILANTHROPY ", TitleTxt )
  TitleTxt <- gsub( " PHILANTHRO ", " PHILANTHROPY ", TitleTxt )
  TitleTxt <- gsub( " PHILANTHROP ", " PHILANTHROPY ", TitleTxt )
  
  #SYSTEMS
  TitleTxt <- gsub( " SYS ", " SYSTEMS ", TitleTxt )
  TitleTxt <- gsub( " SYST ", " SYSTEMS ", TitleTxt )
  TitleTxt <- gsub( " SYSTE ", " SYSTEMS ", TitleTxt )
  TitleTxt <- gsub( " SYSTEM ", " SYSTEMS ", TitleTxt )
  
  #GENERAL
  TitleTxt <- gsub( " GENL ", " GENERAL ", TitleTxt )
  TitleTxt <- gsub( " GEN ", " GENERAL ", TitleTxt )
  TitleTxt <- gsub( " GENE ", " GENERAL ", TitleTxt )
  TitleTxt <- gsub( " GENER ", " GENERAL ", TitleTxt )
  TitleTxt <- gsub( " GENERA ", " GENERAL ", TitleTxt )
  
  #PLANNING
  TitleTxt <- gsub( " PLAN ", " PLANNING ", TitleTxt )
  TitleTxt <- gsub( " PLANN ", " PLANNING ", TitleTxt )
  TitleTxt <- gsub( " PLANNI ", " PLANNING ", TitleTxt )
  TitleTxt <- gsub( " PLANNIN ", " PLANNING ", TitleTxt )
  
  #COMPLIANCE
  TitleTxt <- gsub( " COMPL ", " COMPLIANCE ", TitleTxt )
  TitleTxt <- gsub( " COMPLI ", " COMPLIANCE ", TitleTxt )
  TitleTxt <- gsub( " COMPLIA ", " COMPLIANCE ", TitleTxt )
  TitleTxt <- gsub( " COMPLIAN ", " COMPLIANCE ", TitleTxt )
  TitleTxt <- gsub( " COMPLIANC ", " COMPLIANCE ", TitleTxt )
  
  #ENROLLMENT
  TitleTxt <- gsub( " ENROL ", " ENROLLMENT ", TitleTxt )
  TitleTxt <- gsub( " ENROLL ", " ENROLLMENT ", TitleTxt )
  TitleTxt <- gsub( " ENROLLM ", " ENROLLMENT ", TitleTxt )
  TitleTxt <- gsub( " ENROLLME ", " ENROLLMENT ", TitleTxt )
  TitleTxt <- gsub( " ENROLLMEN ", " ENROLLMENT ", TitleTxt )
  
  #ADMISSIONS
  TitleTxt <- gsub( " ADMIS ", " ADMISSIONS ", TitleTxt )
  TitleTxt <- gsub( " ADMISS ", " ADMISSIONS ", TitleTxt )
  TitleTxt <- gsub( " ADMISS ", " ADMISSIONS ", TitleTxt )
  TitleTxt <- gsub( " ADMISS ", " ADMISSIONS ", TitleTxt )
  TitleTxt <- gsub( " ADMISS ", " ADMISSIONS ", TitleTxt )
  
  #ADVANCEMENT
  TitleTxt <- gsub( " ADVANCEMNT ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVANCEMNT ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVANCMNT ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVNCMNT ", " ADVANCEMENT ", TitleTxt )
  
  TitleTxt <- gsub( " SECY ", " SECRETARY ", TitleTxt )
  
  
  
  #' ### Removes Stop Words
  #' 
  ## ------------------------------------------------------------------------
  # PARTIAL YEAR
  TitleTxt <- gsub( " THRU ", "", TitleTxt )
  TitleTxt <- gsub( " FROM ", "", TitleTxt )
  TitleTxt <- gsub( " UNTIL ", "", TitleTxt )
  TitleTxt <- gsub( " THROUGH ", "", TitleTxt )
  TitleTxt <- gsub( " BEG ", "", TitleTxt )
  TitleTxt <- gsub( " LEFT ", "", TitleTxt )
  TitleTxt <- gsub( " ENDED ", "", TitleTxt )
  TitleTxt <- gsub( " TERM ", "", TitleTxt )
  TitleTxt <- gsub( " TIL ", "", TitleTxt )
  TitleTxt <- gsub( " THR ", "", TitleTxt )
  
  # CONJUNCTION WORDS 
  TitleTxt <- gsub( " TO ", " ", TitleTxt )
  TitleTxt <- gsub( " OF ", " ", TitleTxt )
  TitleTxt <- gsub( " FOR ", " ", TitleTxt )
  TitleTxt <- gsub( " AND ", " ", TitleTxt )
  
  
  #' ### Remove Months
  #' 
  ## ------------------------------------------------------------------------
  TitleTxt <- gsub( " JANUARY ", " ", TitleTxt )
  TitleTxt <- gsub( " JAN ", "", TitleTxt )
  TitleTxt <- gsub( " FEBUARY ", " ", TitleTxt )
  TitleTxt <- gsub( " FEB ", " ", TitleTxt )
  
  
  TitleTxt <- gsub( " MARCH ", " ", TitleTxt )
  TitleTxt <- gsub( " APRIL ", " ", TitleTxt )
  TitleTxt <- gsub( " MAY ", " ", TitleTxt )
  TitleTxt <- gsub( " JUNE ", " ", TitleTxt )
  TitleTxt <- gsub( " JULY ", " ", TitleTxt )
  
  TitleTxt <- gsub( "AUGUST", " ", TitleTxt )
  
  TitleTxt <- gsub( " SEPTEMBER ", " ", TitleTxt )
  TitleTxt <- gsub( " SEP ", "", TitleTxt )
  
  TitleTxt <- gsub( " OCTOBER ", " ", TitleTxt )
  TitleTxt <- gsub( " OCT ", "", TitleTxt )
  
  TitleTxt <- gsub( " NOVEMBER ", " ", TitleTxt )
  TitleTxt <- gsub( " NOV ", "", TitleTxt )
  
  TitleTxt <- gsub( "DECEMBER", " ", TitleTxt )
  TitleTxt <- gsub( " DEC ", "", TitleTxt )
  
  
  # CONJUNCTION WORDS 
  TitleTxt <- gsub( " TO ", " ", TitleTxt )
  TitleTxt <- gsub( " OF ", " ", TitleTxt )
  TitleTxt <- gsub( " FOR ", " ", TitleTxt )
  TitleTxt <- gsub( " AND ", " ", TitleTxt )
  
  return( TitleTxt )
  
}









fix_spelling <- function( title.text, officer, former.officer )
{
  
  
  #' ### MISSPELLING 5 letters or Longer
  #' 
  ## ------------------------------------------------------------------------
  
  
  TitleTxt <- title.text
  Officer <- officer
  FmrOfficer <- former.officer
  
  
  Parsed <- hunspell_parse( TitleTxt )
  allwords <- unlist( Parsed )
  checked <- hunspell_check( allwords )
  Mispellings <- allwords[ !checked ]
  
  
  Mispellings.4 <- Mispellings[ str_length( Mispellings ) > 4 ]
  
  # Mispellings.4 %>% length
  
  Mispellings.4 <- Mispellings.4[ grepl( "[^aeiou]+", Mispellings.4 ) ]
  
  
  #Suggested <- hunspell_suggest( Mispellings.4[1:25] )
  
  
  Most_MS <- table( Mispellings.4 ) %>% sort( decreasing=T ) %>% as.data.frame( )
  Most_MS <- table( Mispellings.4 ) %>% sort( decreasing=T ) %>% as.data.frame( )
  
  # Most Common Mis Spellings
  
  TitleTxt <- gsub( " OFFICIO ", " OFFICER ", TitleTxt )
  TitleTxt <- gsub( " CHEIF ", " CHEIF ", TitleTxt )
  
  TitleTxt <- gsub( " STRAT ", " STRATEGY ", TitleTxt )
  
  TitleTxt <- gsub( " EXCUTIVE ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " MANAG ", " MANAGEMENT ", TitleTxt )
  TitleTxt <- gsub( " SRVCS ", " SERVICE ", TitleTxt )
  TitleTxt <- gsub( " ADMINSTRATOR ", " ADMINISTRATOR ", TitleTxt )
  TitleTxt <- gsub( " EXECTIVE ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " SERVI ", " SERVICE ", TitleTxt )
  TitleTxt <- gsub( " SERVIC ", " SERVICE ", TitleTxt )
  TitleTxt <- gsub( " DIRECTR ", " DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " PRACTIONER ", " PRACTITIONER  ", TitleTxt )
  TitleTxt <- gsub( " DIRC ", " DIRECTOR ", TitleTxt )
  
  TitleTxt <- gsub( " SUPERINTENDE ", " SUPERINTENDENT ", TitleTxt )
  TitleTxt <- gsub( " COMMUN ", " COMMUNICATIONS ", TitleTxt )
  TitleTxt <- gsub( " ADVANCEME ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " COORD ", " COORDINATOR ", TitleTxt )
  TitleTxt <- gsub( " EXECDIR ", " EXECUTIVE DIRECTOR ", TitleTxt )
  TitleTxt <- gsub( " OFFCR ", " OFFICER ", TitleTxt )
  TitleTxt <- gsub( " PRACT ", " PRACTITIONER ", TitleTxt )
  TitleTxt <- gsub( " SECTY ", " SECRETARY ", TitleTxt )
  
  TitleTxt <- gsub( " AFFAI ", " AFFAIRS ", TitleTxt )
  TitleTxt <- gsub( " PRESIDENTCEO ", " PRESIDENT CEO ", TitleTxt )
  TitleTxt <- gsub( " RELAT ", " RELATIONS ", TitleTxt )
  TitleTxt <- gsub( " RESOUR ", " RESOURCES ", TitleTxt )
  TitleTxt <- gsub( " PRACTI ", " PRACTITIONER ", TitleTxt )
  TitleTxt <- gsub( " EXCECUTIVE ", " EXECUTIVE ", TitleTxt )
  TitleTxt <- gsub( " EXCE ", " EXECUTIVE ", TitleTxt )
  
  TitleTxt <- gsub( " DVLPMT ", " DEVELOPMENT ", TitleTxt )
  TitleTxt <- gsub( " ADVAN ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " PROGR ", " PROGRAMS ", TitleTxt )
  TitleTxt <- gsub( " INNOV ", " INNOVATION ", TitleTxt )
  
  
  #' ### MISSPELLING 3-4 letters Long
  #' # ------------------------------------------------------------------------
  
  Mispellings.3 <- Mispellings[str_length( Mispellings ) <= 4 & str_length( Mispellings ) >=3]
  Most_MS <- table( Mispellings.3 ) %>% sort( decreasing=T ) %>% as.data.frame( )
  
  # REMOVES C SUITE
  Non.CSuite.MS <- 
    Most_MS[ str_sub( Most_MS$Mispellings.3, 1, 1 ) != "C" | 
               str_length( Most_MS$Mispellings.3 ) != 3,]
  
  TitleTxt <- gsub( " SVCS ", " SERVICE ", TitleTxt )
  TitleTxt <- gsub( " SERV ", " SERVICE ", TitleTxt )
  TitleTxt <- gsub( " SVC ", " SERVICE ", TitleTxt ) 
  TitleTxt <- gsub( " SCHO ", " SCHOOL ", TitleTxt )
  TitleTxt <- gsub( " MKTG ", " MARKETING ", TitleTxt )
  TitleTxt <- gsub( " SYS ", " SYSTEM ", TitleTxt ) 
  TitleTxt <- gsub( " DEP ", " DEPUTY ", TitleTxt ) 
  TitleTxt <- gsub( " AFF ", " AFFAIRS ", TitleTxt ) 
  TitleTxt <- gsub( " MGT ", " MANAGEMENT ", TitleTxt ) 
  TitleTxt <- gsub( " MEM ", " MEMBER ", TitleTxt ) 
  TitleTxt <- gsub( " CLIN ", " CLINICAL ", TitleTxt )
  TitleTxt <- gsub( " PRE ", " PRESIDENT ", TitleTxt ) 
  TitleTxt <- gsub( " SRVP ", " SENIOR VICE PRESIDENT ", TitleTxt )
  TitleTxt <- gsub( " ACAD ", " ACADEMIC ", TitleTxt )
  TitleTxt <- gsub( " HLTH ", " HEALTH ", TitleTxt )
  TitleTxt <- gsub( " ADMI ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " MKT ", " MARKETING ", TitleTxt ) 
  TitleTxt <- gsub( " SVS ", " SERVICES ", TitleTxt ) 
  TitleTxt <- gsub( " PHY ", " PHYSICIAN ", TitleTxt ) 
  TitleTxt <- gsub( " COUN ", " COUNSEL ", TitleTxt )
  TitleTxt <- gsub( " SURG ", " SURGEON ", TitleTxt )
  TitleTxt <- gsub( " SER ", " SERVICE ", TitleTxt )
  TitleTxt <- gsub( " CHRO ", " CHIEF HUMAN RESOURCES OFFICER ", TitleTxt )
  TitleTxt <- gsub( " EMP ", " EMPLOYEE ", TitleTxt ) 
  TitleTxt <- gsub( " FAC ", " FACULTY ", TitleTxt ) 
  TitleTxt <- gsub( " MIS ", " MANAGEMENT INFORMATION SYSTEMS ", TitleTxt ) 
  TitleTxt <- gsub( " EDU ", " EDUCATION ", TitleTxt ) 
  TitleTxt <- gsub( " MBR ", " MEMBER ", TitleTxt ) 
  TitleTxt <- gsub( " TRES ", " TREASURER ", TitleTxt )
  TitleTxt <- gsub( " FDN ", " FOUNDATION ", TitleTxt ) 
  TitleTxt <- gsub( " ARNP ", " ADVANCED NURSE PRACTITIONER ", TitleTxt )
  TitleTxt <- gsub( " OFC ", " OFFICER ", TitleTxt ) 
  TitleTxt <- gsub( " PRGM ", " PROGRAM ", TitleTxt )
  TitleTxt <- gsub( " SNR ", " SENIOR ", TitleTxt ) 
  TitleTxt <- gsub( " INTL ", " INTELLIGENCE ", TitleTxt )
  TitleTxt <- gsub( " MNGR ", " MANAGER ", TitleTxt )
  TitleTxt <- gsub( " AST ", " ASSITANT ", TitleTxt ) 
  TitleTxt <- gsub( " DTR ", " DIRECTOR ", TitleTxt ) 
  TitleTxt <- gsub( " QUAL ", " QUALITY ", TitleTxt )
  TitleTxt <- gsub( " BRD ", " BOARD ", TitleTxt ) 
  TitleTxt <- gsub( " SRVS", " SERVICE ", TitleTxt ) 
  TitleTxt <- gsub( " LTC ", " LONG TERM CARE ", TitleTxt ) 
  TitleTxt <- gsub( " PRIN ", " PRINCIPAL ", TitleTxt )
  TitleTxt <- gsub( " SRV ", " SERVICE ", TitleTxt ) 
  TitleTxt <- gsub( " AED ", " ASSISTANT EXECUTIVE DIRECTOR ", TitleTxt ) 
  TitleTxt <- gsub( " INV ", " INNOVATION ", TitleTxt ) 
  
  TitleTxt <- gsub( " AMB ", " AMBULANCE ", TitleTxt ) 
  TitleTxt <- gsub( " PHAR ", " PHARMACY ", TitleTxt )
  TitleTxt <- gsub( " EDUC ", " EDUCATION ", TitleTxt )
  TitleTxt <- gsub( " FRMR ", " FORMER ", TitleTxt )
  
  TitleTxt <- gsub( " EVAL ", " EVALUATION ", TitleTxt )
  
  TitleTxt <- gsub( " FAM ", " FAMILY ", TitleTxt ) 
  TitleTxt <- gsub( " MEMB ", " MEMBER ", TitleTxt )
  TitleTxt <- gsub( " PRG ", " PROGRAM ", TitleTxt ) 
  TitleTxt <- gsub( " ASSC", " ASSOCIATE ", TitleTxt ) 
  TitleTxt <- gsub( " ASSO ", " ASSOCIATE ", TitleTxt )
  
  TitleTxt <- gsub( " PERF ", " PERFORMANCE ", TitleTxt )
  TitleTxt <- gsub( " GRP ", " GROUP ", TitleTxt ) 
  TitleTxt <- gsub( " NURS ", " NURSE ", TitleTxt )
  TitleTxt <- gsub( " ADMN ", " ADMINISTRATION ", TitleTxt )
  TitleTxt <- gsub( " CFAO ", " CHIEF ACCOUNTING AND FINANCIAL OFFICER ", TitleTxt )
  TitleTxt <- gsub( " SUPV ", " SUPERVISON ", TitleTxt )
  
  TitleTxt <- gsub( " CMIO ", " CHIEF MEDICAL INFORMATICS OFFICER ", TitleTxt )
  
  TitleTxt <- gsub( " PRAC ", " PRACTICE ", TitleTxt )
  
  TitleTxt <- gsub( " INIT ", " INITATIVES ", TitleTxt )
  
  TitleTxt <- gsub( " MNG ", " MANAGEMENT ", TitleTxt ) 
  TitleTxt <- gsub( " EMPL ", " EMPLOYEE ", TitleTxt )
  TitleTxt <- gsub( " PGM ", " PROGRAM ", TitleTxt )
  TitleTxt <- gsub( " PLNG ", " PLANNING ", TitleTxt )
  TitleTxt <- gsub( " ASSI ", " ASSISTANT ", TitleTxt )
  TitleTxt <- gsub( " CNSL ", " COUNSEL ", TitleTxt )
  TitleTxt <- gsub( " ACC ", " ACCOUNTING ", TitleTxt ) 
  
  TitleTxt <- gsub( " DON ", " DIRECTOR OF NURSING ", TitleTxt ) 
  
  TitleTxt <- gsub( " SCH ", " SCHOOL ", TitleTxt ) 
  TitleTxt <- gsub( " SCHL ", " SCHOOL ", TitleTxt ) 
  
  TitleTxt <- gsub( " ES ", " ELEMENTARY SCHOOL ", TitleTxt )
  TitleTxt <- gsub( " MS ", " MIDDLE SCHOOL ", TitleTxt )
  TitleTxt <- gsub( " HS ", " HIGH SCHOOL ", TitleTxt )
  
  
  #' ### MISSPELLING 2 letters Long
  #' #   ------------------------------------------------------------------------
  
  Mispellings.2 <- Mispellings[str_length( Mispellings ) == 2]
  
  Most_MS <- table( Mispellings.2 ) %>% sort( decreasing=T ) %>% as.data.frame( )
  
  TitleTxt <- gsub( " BD ", " BOARD ", TitleTxt ) 
  TitleTxt <- gsub( " EE ", " EMPLOYEE ", TitleTxt ) 
  TitleTxt <- gsub( " KEY ", " EMPLOYEE ", TitleTxt )
  TitleTxt <- gsub( " EMPLOYEE EMPLOYEE ", " KEY EMPLOYEE ", TitleTxt )
  TitleTxt <- gsub( " SV ", " SERVICES ", TitleTxt ) 
  TitleTxt <- gsub( " TR ", " TREASURER ", TitleTxt ) 
  
  
  
  ## ------------------------------------------------------------------------
  
  VP.TEST<-TitleTxt[grepl( " VICE PRESIDENT ", TitleTxt)]
  VP.Parsed <- hunspell_parse( VP.TEST )
  VP.words <- unlist( VP.Parsed )
  
  checked.VP <- hunspell_check( VP.words )
  Mispellings.VP <- VP.words[!checked.VP]
  
  TitleTxt <- gsub( " ADV ", " ADVANCEMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ADVA ", " ADVANCEMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ADVAN ", " ADVANCEMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ADVANC ", " ADVANCEMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ADVANCE ", " ADVANCEMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ADVANCEM ", " ADVANCEMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ADVANCEME ", " ADVANCEMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ADVANCEMEN ", " ADVANCEMENT ", TitleTxt ) 
  
  TitleTxt <- gsub( " INSTI ", " INSTITUTIONAL ", TitleTxt ) 
  TitleTxt <- gsub( " INSTITU ", " INSTITUTIONAL ", TitleTxt ) 
  TitleTxt <- gsub( " INSTITUT ", " INSTITUTIONAL ", TitleTxt ) 
  TitleTxt <- gsub( " INSTITUTI ", " INSTITUTIONAL ", TitleTxt ) 
  TitleTxt <- gsub( " INSTITUTIO ", " INSTITUTIONAL ", TitleTxt ) 
  TitleTxt <- gsub( " INSTITUTIONA ", " INSTITUTIONAL ", TitleTxt ) 
  
  TitleTxt <- gsub( " LDRSHIP ", " LEADERSHIP ", TitleTxt ) 
  
  TitleTxt <- gsub( " REDITATION ", " RADIATION ", TitleTxt ) 
  TitleTxt <- gsub( " OUNTING ", " ACCOUNTING ", TitleTxt ) 
  
  TitleTxt <- gsub( " DVLPMNT ", " DEVELOPMENT ", TitleTxt ) 
  
  TitleTxt <- gsub( " ENROL ", " ENROLLMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ENROLL ", " ENROLLMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ENROLLM ", " ENROLLMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ENROLLME ", " ENROLLMENT ", TitleTxt ) 
  TitleTxt <- gsub( " ENROLLMEN ", " ENROLLMENT ", TitleTxt ) 
  
  TitleTxt <- gsub( " PRGMS ", " PROGRAMS ", TitleTxt ) 
  TitleTxt <- gsub( " AFFIARS ", " AFFAIRS ", TitleTxt ) 
  TitleTxt <- gsub( " MKTING ", " MARKETING ", TitleTxt ) 
  TitleTxt <- gsub( " AMBUL ", " AMBULANCE ", TitleTxt ) 
  
  TitleTxt <- gsub( " COUNS ", " COUNSEL ", TitleTxt ) 
  
  TitleTxt <- gsub( " CHF ", " CHIEF ", TitleTxt ) 
  
  TitleTxt <- gsub( " DVLP ", " DEVELOPMENT ", TitleTxt ) 
  TitleTxt <- gsub( " TECHNOLO ", " TECHNOLOGY ", TitleTxt ) 
  TitleTxt <- gsub( " ASSITANT ", " ASSISTANT ", TitleTxt ) 
  TitleTxt <- gsub( " INFORMATI ", " INFORMATION ", TitleTxt ) 
  TitleTxt <- gsub( " CHF ", " CHIEF ", TitleTxt ) 
  TitleTxt <- gsub( " CHF ", " CHIEF ", TitleTxt ) 
  TitleTxt <- gsub( " CHF ", " CHIEF ", TitleTxt ) 
  TitleTxt <- gsub( " CHF ", " CHIEF ", TitleTxt ) 
  
  TitleTxt <- gsub( " MKTNG ", " MARKETING ", TitleTxt )
  TitleTxt <- gsub( " QLTY ", " QUALITY ", TitleTxt ) 
  
  TitleTxt <- gsub( " SPONS ", " SPONSOR ", TitleTxt ) 
  
  TitleTxt <- gsub( " INITATIVES ", " INITIATIVES ", TitleTxt )
  TitleTxt <- gsub( " MNGT ", " MANGEMENT ", TitleTxt )
  TitleTxt <- gsub( " MRKTG ", " MARKETING ", TitleTxt )
  TitleTxt <- gsub( " OFFR ", " OFFICER ", TitleTxt )
  TitleTxt <- gsub( " ADVA ", " ADVANCEMENT ", TitleTxt )
  TitleTxt <- gsub( " MARKE ", " MARKETING ", TitleTxt )
  
  TitleTxt <- gsub( " MGMNTSVCES ", " MANAGEMENT SERVICES ", TitleTxt )
  
  TitleTxt <- gsub( " DEVELOPMT ", " DEVELOPMENT ", TitleTxt )
  TitleTxt <- gsub( " CHEIF ", " CHIEF ", TitleTxt )
  TitleTxt <- gsub( " RESEARC ", " RESEARCH ", TitleTxt )
  TitleTxt <- gsub( " NUR ", " NURSE ", TitleTxt )
  
  TitleTxt <- gsub( " ASSTSEC ", " ASSISTANT SECRETARY ", TitleTxt )
  
  TitleTxt <- gsub( " STATEGY ", " STRATEGY ", TitleTxt )
  
  TitleTxt <- gsub( " SERVS ", " SERVICES ", TitleTxt )
  
  TitleTxt <- gsub( " AFFARIS ", " AFFAIRS ", TitleTxt )
  
  TitleTxt <- gsub( " OPEARTIONS ", " OPERATIONS ", TitleTxt )
  
  TitleTxt <- gsub( " MEDICA ", " MEDICAL ", TitleTxt )
  
  TitleTxt <- gsub( " RSRCH ", " RESEARCH ", TitleTxt )
  
  TitleTxt <- gsub( " SRVC ", " SERVICE ", TitleTxt )
  TitleTxt <- gsub( " COUNSE ", " COUNSEL ", TitleTxt )
  TitleTxt <- gsub( " COUNS ", " COUNSEL ", TitleTxt )
  TitleTxt <- gsub( " COUN ", " COUNSEL ", TitleTxt )
  
  TitleTxt <- gsub( " FINACIAL ", " FINANCIAL ", TitleTxt )
  
  TitleTxt <- gsub( " EXECDIRECTOR ", " EXECUTIVE DIRECTOR ", TitleTxt )
  
  TitleTxt[ grepl( " EX DIRECTOR ", TitleTxt ) & 
              Officer == 1 & FmrOfficer == 0 ] <- gsub( " EX DIRECTOR ", " EXECUTIVE DIRECTOR ", TitleTxt )[ grepl( " EX DIRECTOR ", TitleTxt ) & Officer == 1 & FmrOfficer == 0 ]
  
  TitleTxt <- str_c( " ",TitleTxt )
  TitleTxt <- str_c( TitleTxt," " )
  
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  TitleTxt <- gsub( "  ", " ", TitleTxt )
  
  return( TitleTxt )
  
  
}









#' ## Title Catagories  
#' 
#' Shows the sorting of Standardized and Cleaned into titles into Catagories in the following steps:
#' 
#' 1. Individuals are sorted to Catagories based on if they have one the 1000 most common titles.   
#' 2. The most common words or phrases associated with the catagories are used to sort remaining individuals into catagories (where they exist)  
#' 3. Catagorized Titles are used to create meta-titles (C-Suite, Management, Board, ect)  
#' 
#' 
#' 
#' ### Catagories
#' 
#' Variable Name	        Category Name               
#' --------------       --------------			          
#' CEO	                  Chief Executive		          
#' CFO	                  Financial				            
#' Treasurer	            Treasurer				            
#' DEP.CEO	              Deputy Chief Executive		  
#' SEC	                  Secretary				            
#' COO	                  Operations				          
#' TRUST	                Trustee/Board				        
#' HUM.RES	              Human Resources				      
#' DEP.HEAD	            Department Head				      
#' MAN	Generic           Management				          
#' DEV	                  Development				          
#' TECH	                Technology/IT				        
#' COMM	                Communications				      
#' OTHER	                Other Category				      
#' PROJECT	              Project Manager				      
#' LEGAL	                Legal				           		  
#' FACILITIES            Facilities				          
#' ADMIN.SUP	            Administrative Support		  
#' MED.MAN	              Medical Management				  
#' HEALTH.HUM	          Health and Human Services		
#' TRAIN	                Training/Education		      
#' ACADEMIC.MAN	        Academic Manager				    
#' PROFESIONAL	          Professional				        
#' OTHER.PROF	          Other Professional				  
#' ACADEMIC.PROF	        Academic Professional		    
#' MED.PROF	            Medical Professional			  
#' 
#' 
#' ### Form 990 Title Instructions
#' 
#' **Titles**
#' 
#' * Current officers, directors, and trustees ( no minimum compensation threshold ).
#' 
#' * Current key employees ( over $150,000 of reportable compensation ).
#' 
#' * Current five highest compensated employees other than officers, directors, trustees, or listed key employees ( over $100,000 of reportable compensation ).
#' 
#' * Former officers, key employees, and highest compensated employees ( over $100,000 of reportable compensation, with special rules for former highest compensated employees ).
#' 
#' * Former directors and trustees ( over $10,000 of reportable compensation in the capacity as a former director or trustee ).
#' 
#' **Director or Trustee**
#' 
#' A �director or trustee� is a member of the organization's governing body, but only if the member has voting rights. A director or trustee that served at any time during
#' the organization's tax year is deemed a current director or trustee. Members of advisory boards that don't exercise any governance authority over the organization aren't considered
#' directors or trustees.
#' 
#' An �institutional trustee� is a trustee that isn't an individual or natural person but an organization. For instance, a bank or trust company serving as the trustee of a trust is an institutional trustee.
#' 
#' **Officer**
#' 
#' An officer is a person elected or appointed to manage the organization's daily operations. An officer that served at any time during the organization's tax year is deemed a current officer. The officers of an organization are determined by reference to its organizing document, bylaws, or resolutions of its governing body, or as otherwise designated consistent with state law, but, at a minimum, include those officers required by applicable state law. Officers can include a president, vice-president, secretary, treasurer and, in some cases, a Board Chair. In addition, for purposes of Form 990, including Part VII, Section A, and Schedule J ( Form 990 ), treat as an officer the following persons, regardless of their titles.
#' 
#' 1. *Top management official*. The person who has ultimate responsibility for implementing the decisions of the governing body or for supervising the management, administration, or operation of the organization; for example, the organization's president, CEO, or executive director.
#' 
#' 2. *Top financial official*. The person who has ultimate responsibility for managing the organization's finances; for example, the organization's treasurer or chief financial officer.
#' 
#' If ultimate responsibility resides with two or more individuals ( for example, co-presidents or co-treasurers ), who can exercise such responsibility in concert or individually, then treat all such individuals as officers.
#' 
#' **Key Employees**
#' 
#' Key employee. For purposes of Form 990, a current key employee is an employee of the organization ( other than an officer, director, or trustee ) who meets all three of the following tests, applied in the following order:
#' 
#' 1. \$150,000 Test: Receives reportable compensation from the organization and all related organizations in excess of \$150,000 for the calendar year ending with or within the organization's tax year.
#' 
#' 2. Responsibility Test: At any time during the calendar year ending with or within the organization's tax year:
#' 
#'   a. Has responsibilities, powers, or influence over the organization as a whole that is similar to those of officers, directors, or trustees;
#' 
#'   b. Manages a discrete segment or activity of the organization that represents 10% or more of the activities, assets, income, or expenses of the organization, as compared to the organization as a whole; or
#' 
#'   c. Has or shares authority to control or determine 10% or more of the organization's capital expenditures, operating budget, or compensation for employees.
#' 
#' 3. Top 20 Test: Is one of the 20 employees other than officers, directors, and trustees who satisfy the \$150,000 Test and Responsibility Test with the highest reportable compensation from the organization and related organizations for the calendar year ending with or within the organization's tax year.
#' 
#'   If the organization has more than 20 individuals who meet the \$150,000 Test and Responsibility Test, report as key employees only the 20 individuals that have the highest reportable compensation from the organization and related organizations. Note that any others, up to five, might be reportable as current highest compensated employees, with over $100,000 in reportable compensation. Use the calendar year ending with or within the organization's tax year for determining the organization's current key employees.
#'   
#'   An individual that isn't an employee of the organization ( or of a disregarded entity of the organization ) is nonetheless treated as a key employee if he or she serves as an officer or director of a disregarded entity of the organization and otherwise meets the standards of a key employee set forth above. See Disregarded Entities, later, for treatment of certain employees of a disregarded entity as a key employee of the organization.
#'   
#'   If an employee is a key employee of the organization for only a portion of the year, that person's entire compensation for the calendar year ending with or within the organization's tax year, from both the filing organization and related organizations,should be reported in Part VII, Section A.
#'   
#'   Management companies and similar entities that are independent contractors should not be reported as key employees. The organization's top management official and top financial official are deemed officers rather than key employees.
#'   
#' In the examples set forth below, assume the individual involved is an employee that satisfies the \$150,000 Test and Top 20 Test and isn't an officer, director, or trustee.\\
#'   
#'   **Example 1.** T is a large section 501( c )( 3 ) university. L is the dean of the law school of T, which generates more than 10% of the revenue of T, including contributions from alumni and foundations. Although L does not have ultimate responsibility for managing the university as a whole, L meets the Responsibility Test and is reportable as a key employee of T.
#'   
#'   **Example 2.** S chairs a small academic department in the College of Arts and Sciences of the same university, T, described above. As department chair, S supervises faculty in the department, approves the course curriculum, and oversees the operating budget for the department. The department represents less than 10% of the university's activities, assets,
#' income, expenses, capital expenditures, operating budget, and employee compensation. Under these facts and circumstances, S does not meet the Responsibility Test and isn't a key
#' employee of T.
#' 
#'   **Example 3.** U is a large acute-care section 501( c )( 3 ) hospital. U employs X as a radiologist. X gives instructions to staff for the radiology work X conducts, but X does not supervise other U employees, manage the radiology department, or have or share authority to control or determine 10% or more of U's capital expenditures, operating budget, or employee compensation. Under these facts and circumstances, X does not meet the Responsibility Test and isn't a key employee of U. \\
#' 
#'   **Example 4.** W is a cardiologist and head of the cardiology department of the same hospital U described above. The cardiology department is a major source of patients admitted to U and consequently represents more than 10% of U's income, as compared to U as a whole. As department head, W manages the cardiology department. Under these facts and circumstances, W meets the Responsibility Test and is a key employee of U. \\




new_titles <- function( comp.dat )
{
  
  
  
  d2 <- comp.dat
  
  
  #' ### Load Categorized Titles
  ## ------------------------------------------------------------------------
  ##
  ## load( "title-list.rdata" )  # moved to load packages step
  
  
  
  #' Creates new Title Categories - These are NOT mutually exclusive.
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
  
  
  
  
  #' ### CEO
  #' 
  #' First, assign individuals to CEO catagory if they had a title that appeared in the 1000 most common titles that we coded to CEO.
  ## ------------------------------------------------------------------------
  
  d2$CEO.Prob <- 0
  d2$CEO[ d2$TitleTxt2 %in% All.Titles$CEO.Clear] <- 1
  d2$CEO.Prob[ d2$TitleTxt2 %in% All.Titles$CEO.Prob] <- 1
  
  
  # TRUSTEE not CEO
  
  d2$CEO   [ d2$CEO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 0]   <- 0
  d2$TRUST [ d2$CEO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 0]   <- 1
  
  
  # CEO Not Trustee
  
  #'  TrustOrDir          Officer      KeyEmpl      HighComp       Meaning
  #' --------------     ----------    ----------   -----------    -----------
  #' 1                   0             0            0              Board Probably not CEO
  #' 0                   1             0            0              CEO
  #' 1                   1             0            0              CEO
  #' 0                   0             1            0              Management
  #' 0                   0             0            1              Other
  ## ------------------------------------------------------------------------
  
  d2$CEO[ d2$CEO.Prob == 1 & d2$TrustOrDir == 0 & d2$Officer == 1 ]   <- 1
  d2$CEO[ d2$CEO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 1 ] <- 1
  d2$TRUST[ d2$CEO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 1 ] <- 1
  d2$TRUST[ d2$TitleTxt2 %in% All.Titles$CEO.Board ] <- 1
  d2$CEO[ d2$TitleTxt2 %in% All.Titles$CEO.Board ] <- 1
  d2$MAN[ d2$CEO.Prob == 1 & d2$TrustOrDir == 0 & d2$Officer == 0 & d2$KeyEmpl == 1 ] <- 1
  d2$MAN[ d2$CEO.Prob == 1 & d2$TrustOrDir == 0 & d2$Officer == 0 & d2$HighComp == 1 ] <- 1
  
  
  #' 
  #' Next we want to assign individuals to the CEO catagory if they had one of the most common phrases associated with that title
  #' 
  ## ------------------------------------------------------------------------
  
  d2$CEO [ grepl( " CHIEF EXECUTIVE OFFICER ", d2$TitleTxt2) ] <- 1
  d2$CEO [ grepl( " EXECUTIVE DIRECTOR ", d2$TitleTxt2) ] <- 1
  d2$CEO [ grepl( " ASSISTANT EXECUTIVE DIRECTOR ", d2$TitleTxt2) ] <- 0
  
  
  # d2$TitleTxt2[ d2$CEO == 1 ] %>% table() %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### HEAD OF SCHOOL
  #' 
  #'  TrustOrDir          Officer      KeyEmpl      HighComp       Meaning
  #' --------------     ----------    ----------   -----------    -----------
  #' 1                   0             0            0              Board Probably not CFO?
  #' 0                   1             0            0              CFO
  #' 1                   1             0            0              CFO
  #' 
  #' 
  ## ------------------------------------------------------------------------
  
  d2$TRUST [d2$TitleTxt2 %in% All.Titles$SCHOOL.HEAD & d2$TrustOrDir == 1] <- 1
  d2$CEO [d2$TitleTxt2 %in% All.Titles$SCHOOL.HEAD & d2$Officer == 1] <- 1
  d2$MAN [d2$TitleTxt2 %in% All.Titles$SCHOOL.HEAD & d2$Officer == 0 & d2$TrustOrDir == 0 & d2$KeyEmpl == 1 ] <- 1
  d2$OTHER [d2$TitleTxt2 %in% All.Titles$SCHOOL.HEAD & d2$Officer == 0 & d2$TrustOrDir == 0 & d2$KeyEmpl == 0 ] <- 1
  
  
  
  #' ### CFO
  #' 
  #' 
  #'  TrustOrDir          Officer      KeyEmpl      HighComp       Meaning
  #' --------------     ----------    ----------   -----------    -----------
  #' 1                   0             0            0              Board Probably not CFO?
  #' 0                   1             0            0              CFO
  #' 1                   1             0            0              CFO
  #' 
  #' 
  ## ------------------------------------------------------------------------
  
  d2$CFO.Prob <- 0
  
  d2$CFO[ d2$TitleTxt2 %in% All.Titles$CFO.Clear] <- 1
  d2$CFO.Prob[ d2$TitleTxt2 %in% All.Titles$CFO.Prob] <- 1
  
  d2$CFO   [ d2$CFO.Prob == 1 & d2$TrustOrDir == 1]   <- 0
  d2$TRUST [ d2$CFO.Prob == 1 & d2$Officer == 1   ]   <- 1
  d2$CFO   [ d2$CFO.Prob == 1 & d2$KeyEmpl == 1]      <- 1
  d2$CFO   [ d2$CFO.Prob == 1 & d2$HighComp == 1]     <- 1
  d2$CFO   [ grepl( " CFO ", d2$TitleTxt2) ]          <- 1
  d2$CFO   [ grepl( " CHIEF FINANCIAL OFFICER ", d2$TitleTxt2) ]          <- 1
  d2$CFO   [ grepl( " FINANCE ", d2$TitleTxt2) ]          <- 1
  
  d2$TRUST[ d2$CFO.Prob == 1 & d2$TrustOrDir == 1 & d2$Officer == 1] <- 1
  
  # titles <- d2$TitleTxt2[ d2$CFO == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### Treasurer
  #' 
  ## ------------------------------------------------------------------------
  
  d2$Treasurer  [ d2$TitleTxt2 %in% All.Titles$Treasurer] <- 1
  d2$Treasurer  [ grepl( " TREASURER ", d2$TitleTxt2) ]   <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$Treasurer == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### Deputy CEO
  #' 
  ## ------------------------------------------------------------------------
  
  d2$DEP.CEO[ d2$TitleTxt2 %in% All.Titles$DEP.CEO ] <- 1
  d2$DEP.CEO[ d2$TitleTxt2 %in% All.Titles$DEP.CEO.Prob & d2$Officer == 1 ] <- 1
  
  #d2[ d2$TitleTxt2 %in% All.Titles$DEP.CEO.Prob, ]
  
  d2$MAN [ d2$TitleTxt2 %in% All.Titles$DEP.CEO.Prob  & d2$Officer == 0 & (d2$KeyEmpl == 1 | d2$HighComp == 1 ) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$DEP.CEO == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' 
  #' ### Secretary
  #' 
  ## ------------------------------------------------------------------------
  
  d2$SEC[ d2$TitleTxt2 %in%  All.Titles$SEC] <- 1
  d2$SEC [ grepl( " SECRETARY ", d2$TitleTxt2) ] <- 1
  d2$SEC [ grepl( " SEC ", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$SEC == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' 
  #' ### COO
  #' 
  ## ------------------------------------------------------------------------
  
  # All.Titles$COO.Prob
  
  d2$COO[ d2$TitleTxt2 %in%  All.Titles$COO] <- 1
  d2$COO[ d2$TitleTxt2 %in%  All.Titles$COO] <- 1
  d2$COO [ grepl( " OPERATIONS ", d2$TitleTxt2) ] <- 1
  d2$COO [ grepl( " COO ", d2$TitleTxt2) ] <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$COO == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### Director/Trustee
  #' 
  ## ------------------------------------------------------------------------
  
  d2$TRUST[ d2$TitleTxt2 %in%  All.Titles$TRUST] <- 1
  d2$TRUST [ grepl( " TRUSTEE ", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( " TRUST ", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( " BOARD MEMBER ", d2$TitleTxt2) ] <- 1
  d2$TRUST [ grepl( " CHAIR ", d2$TitleTxt2) ] <- 1
  d2$TRUST [ d2$TrustOrDir == 1 ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$TRUST == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  #' 
  #' ### Human Resources
  #' 
  ## ------------------------------------------------------------------------
  
  d2$HUM.RES[ d2$TitleTxt2 %in%  All.Titles$HUM.RES] <- 1
  d2$HUM.RES [ grepl( " HUMAN RESOURCES ", d2$TitleTxt2) ] <- 1
  d2$HUM.RES [ grepl( " HUMAN RESOURCE ", d2$TitleTxt2) ] <- 1
  d2$HUM.RES [ grepl( " STAFFING ", d2$TitleTxt2) ] <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$HUM.RES == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### Communications
  #' 
  ## ------------------------------------------------------------------------
  
  d2$COMM[ d2$TitleTxt2 %in%  All.Titles$COMM] <- 1
  d2$COMM [ grepl( " COMMUNICATION ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " COMMUNICATIONS ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " MARKETING ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " EXTERNAL AFFAIRS ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " PUBLIC AFFAIRS ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " EXTERNAL AFFAIRS ", d2$TitleTxt2) ] <- 1
  d2$COMM [ grepl( " RELATIONS ", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$COMM == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  #' ### Management
  #' 
  ## ------------------------------------------------------------------------
  
  d2$MAN[ d2$TitleTxt2 %in%  All.Titles$MAN] <- 1
  
  # titles <- d2$TitleTxt2[ d2$MAN == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  
  #' ### Technology/Information
  #' 
  ## ------------------------------------------------------------------------
  
  d2$TECH[ d2$TitleTxt2 %in%  All.Titles$TECH] <- 1
  
  d2$TECH [ grepl( " CHIEF INFORMATION OFFICER ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " CIO ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " IT ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " INFORMATION ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " TECHNOLOGY ", d2$TitleTxt2) ] <- 1
  d2$TECH [ grepl( " CDO ", d2$TitleTxt2) ] <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$TECH == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### Development
  #' 
  ## ------------------------------------------------------------------------
  
  d2$DEV [ d2$TitleTxt2 %in%  All.Titles$DEV]       <- 1
  d2$DEV [ grepl( " DEVELOPMENT ", d2$TitleTxt2) ]  <- 1
  d2$DEV [ grepl( " PHILANTHROPY ", d2$TitleTxt2) ] <- 1
  d2$DEV [ grepl( " FUND ", d2$TitleTxt2) ]         <- 1
  d2$DEV [ grepl( " FUNDRAISING ", d2$TitleTxt2) ]  <- 1
  d2$DEV [ grepl( " MAJOR GIFTS ", d2$TitleTxt2) ]  <- 1
  
  # titles <- d2$TitleTxt2[ d2$DEV == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### OTHER
  #' 
  ## ------------------------------------------------------------------------
  
  d2$OTHER [ d2$TitleTxt2 %in%  All.Titles$OTHER] <- 1
  # titles <- d2$TitleTxt2[ d2$OTHER == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  
  #' ### PROJECT
  #' 
  ## ------------------------------------------------------------------------
  
  d2$PROJECT [ d2$TitleTxt2 %in%  All.Titles$PROJECT] <- 1
  
  d2$PROJECT [ grepl( " PROJECT ", d2$TitleTxt2) ]  <- 1
  d2$PROJECT [ grepl( " PROJECTS ", d2$TitleTxt2) ]  <- 1
  d2$PROJECT [ grepl( " PROGRAM ", d2$TitleTxt2) ] <- 1
  d2$PROJECT [ grepl( " PROGRAMS ", d2$TitleTxt2) ] <- 1
  
  
  
  # titles <- d2$TitleTxt2[ d2$PROJECT == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' 
  #' ### LEGAL
  #' 
  ## ------------------------------------------------------------------------
  
  d2$LEGAL [ d2$TitleTxt2 %in%  All.Titles$LEGAL] <- 1
  # titles <- d2$TitleTxt2[ d2$LEGAL == 1]
  
  d2$LEGAL [ grepl( " COUNSEL ", d2$TitleTxt2) ]  <- 1
  d2$LEGAL [ grepl( " ATTORNEY ", d2$TitleTxt2) ]  <- 1
  d2$LEGAL [ grepl( " COMPLIANCE ", d2$TitleTxt2) ] <- 1
  d2$LEGAL [ grepl( " POLICY ", d2$TitleTxt2) ] <- 1
  d2$LEGAL [ grepl( " LEGAL ", d2$TitleTxt2) ] <- 1
  d2$LEGAL [ grepl( " LITIGATION ", d2$TitleTxt2) ] <- 1
  
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### FACILITIES
  #' 
  ## ------------------------------------------------------------------------
  
  d2$FACILITIES [ d2$TitleTxt2 %in%  All.Titles$FACILITIES] <- 1
  
  d2$FACILITIES [ grepl( " FACILITIES ", d2$TitleTxt2) ]  <- 1
  d2$FACILITIES [ grepl( " MAINTENANCE ", d2$TitleTxt2) ]  <- 1
  d2$FACILITIES [ grepl( " FIELD ", d2$TitleTxt2) ] <- 1
  d2$FACILITIES [ grepl( " FIELDS ", d2$TitleTxt2) ] <- 1
  d2$FACILITIES [ grepl( " FACILITY ", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$FACILITIES == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  
  #' ### ADMIN.SUP
  #' 
  ## ------------------------------------------------------------------------
  
  d2$ADMIN.SUP [ d2$TitleTxt2 %in%  All.Titles$ADMIN.SUP] <- 1
  
  d2$ADMIN.SUP [ grepl( " SUPPORT ", d2$TitleTxt2)  & grepl( " SERVICES ", d2$TitleTxt2) ] <- 1
  d2$ADMIN.SUP [ grepl( " ADMINISTRATION ", d2$TitleTxt2)  & grepl( " ASSISTANT ", d2$TitleTxt2) ] <- 1
  d2$ADMIN.SUP [ grepl( " ADMINISTRATIVE ", d2$TitleTxt2)  & grepl( " ASSISTANT ", d2$TitleTxt2) ] <- 1
  d2$ADMIN.SUP [ grepl( " EXECUTIVE ", d2$TitleTxt2)  & grepl( " ASSISTANT ", d2$TitleTxt2) ] <- 1
  
  # titles <- d2$TitleTxt2[ d2$ADMIN.SUP == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### MED.MAN
  #' 
  ## ------------------------------------------------------------------------
  
  d2$MED.MAN [ d2$TitleTxt2 %in%  All.Titles$MED.MAN] <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$MED.MAN == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### HEALTH.HUM
  #' 
  ## ------------------------------------------------------------------------
  
  d2$HEALTH.HUM [ d2$TitleTxt2 %in%  All.Titles$HEALTH.HUM] <- 1
  
  d2$HEALTH.HUM[ grepl( " CASE ", d2$TitleTxt2)] <- 1
  
  d2$HEALTH.HUM[ grepl( " SHELTER ", d2$TitleTxt2)] <- 1
  d2$HEALTH.HUM[ grepl( " HOUSING ", d2$TitleTxt2)] <- 1
  d2$HEALTH.HUM[ grepl( " SOCIAL WORKER ", d2$TitleTxt2)] <- 1
  
  
  # titles <- d2$TitleTxt2[ d2$HEALTH.HUM == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  #' ### TRAIN
  #' 
  ## ------------------------------------------------------------------------
  
  d2$TRAIN [ d2$TitleTxt2 %in%  All.Titles$TRAIN] <- 1
  
  # titles <- d2$TitleTxt2[ d2$TRAIN == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  #' ### Academic Management
  #' 
  ## ------------------------------------------------------------------------
  
  d2$ACADEMIC.MAN [ d2$TitleTxt2 %in%  All.Titles$ACADEMIC.MAN] <- 1
  
  # titles <- d2$TitleTxt2[ d2$ACADEMIC.MAN == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  #' ### DEP.HEAD
  #' 
  ## ------------------------------------------------------------------------
  
  d2$DEP.HEAD [ d2$TitleTxt2 %in%  All.Titles$DEP.HEAD] <- 1
  
  # titles <- d2$TitleTxt2[ d2$DEP.HEAD == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  #' ### PROFESIONAL
  #' 
  ## ------------------------------------------------------------------------
  
  d2$PROFESIONAL [ d2$TitleTxt2 %in%  All.Titles$PROFESIONAL] <- 1
  
  # titles <- d2$TitleTxt2[ d2$PROFESIONAL == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  
  #' ### OTHER PROF
  #' 
  ## ------------------------------------------------------------------------
  
  d2$OTHER.PROF [ d2$TitleTxt2 %in%  All.Titles$OTHER.PROF] <- 1
  
  # titles <- d2$TitleTxt2[ d2$OTHER.PROF == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  #' ### ACADEMIC PROF
  #' 
  ## ------------------------------------------------------------------------
  
  d2$ACADEMIC.PROF [ d2$TitleTxt2 %in%  All.Titles$ACADEMIC.PROF] <- 1
  
  # titles <- d2$TitleTxt2[ d2$ACADEMIC.PROF == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  #' ### MED PROF
  #' 
  ## ------------------------------------------------------------------------
  
  d2$MED.PROF [ d2$TitleTxt2 %in%  All.Titles$MED.PROF] <- 1
  
  # titles <- d2$TitleTxt2[ d2$MED.PROF == 1]
  # table( titles ) %>% sort( decreasing=T ) %>% as.data.frame( ) %>% head( 20 ) %>% pander
  
  
  #' ### Common Missed Catagories.
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
  
  
  
  #' ## Total Number of Titles Catagorized
  #' 
  ## ------------------------------------------------------------------------
  
  d2$Num.Titles <- d2$CEO + d2$CFO + d2$Treasurer + d2$DEP.CEO + d2$SEC + d2$COO + d2$TRUST + d2$HUM.RES + d2$DEP.HEAD + d2$MAN + d2$DEV + d2$OTHER +d2$TECH + d2$COMM + d2$PROJECT + d2$LEGAL + d2$FACILITIES + d2$ADMIN.SUP + d2$MED.MAN + d2$HEALTH.HUM + d2$TRAIN + d2$ACADEMIC.MAN + d2$PROFESIONAL + d2$OTHER.PROF + d2$ACADEMIC.PROF + d2$MED.PROF
  
  d2$Catagorized <- ( d2$Num.Titles >= 1 )
  
  d2.missed <- d2[ d2$Catagorized != TRUE, ]
  
  cat("\nAfter Standardizing, cleaning, and categorizing all of the 50 most Common titles, we Categorized ", sum(d2$Catagorized), " of ", nrow(d2) , " titles. \n"  )
  cat("This accounted for ", sum(d2$Catagorized)/nrow(d2), " of the Observations\n")
  
  
  
  
  #' ## Catagory Descriptive Statistics
  #' 
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
  
  
  
  
  
  
  
  
  
  #' ## Positions and Roles 
  #' 
  #' ![Example of Positions and Roles Assignment](Title Position Roles.png)
  #' 
  #' 4 Positions:
  #' 
  #' - Management
  #' - HPP
  #' - Other Staff
  #' - Trustee
  #' 
  #' 4 Roles
  #' 
  #' - Board Leadership
  #' - C-Level/Officer
  #' - Interim 
  #' - Former 
  
  
  
  #' ### Management
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
  
  
  
  #' ### HPP
  #' 
  ## ------------------------------------------------------------------------
  
  d2$HPP <- 0
  
  d2$HPP[ d2$PROFESIONAL == 1 ] <- 1
  d2$HPP[ d2$LEGAL == 1 ] <- 1
  d2$HPP[ d2$PROJECT == 1 ] <- 1
  d2$HPP[ d2$TRAIN == 1 ] <- 1
  
  
  
  #' ### Other Staff
  ## ------------------------------------------------------------------------
  
  d2$Other.Staff <- 0
  d2$Other.Staff[ d2$HPP == 0 & d2$Mgmt == 0 & d2$TRUST == 0] <- 1
  
  
  
  #' ### Trustee
  ## ------------------------------------------------------------------------
  
  d2$Trustee <- 0
  
  
  
  #' ### Board Leadership
  ## ------------------------------------------------------------------------
  
  
  d2$Board.Leadership <- 0
  
  d2$Board.Leadership[ d2$CEO == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$COO == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$CFO == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$Treasurer == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$DEP.CEO == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$SEC == 1 & d2$TRUST == 1 ] <- 1
  d2$Board.Leadership[ d2$Officer == 1 & d2$TRUST == 1 ] <- 1
  
  
  
  #' ### C-Level / Officer
  ## ------------------------------------------------------------------------
  
  d2$C.Level <- 0
  
  d2$C.Level [ grepl( " C[A-Z]O ", d2$TitleTxt2) ] <- 1
  d2$C.Level [ grepl( " CHIEF ", d2$TitleTxt2) ] <- 1
  d2$C.Level [ d2$Officer == 1 ] <- 1
  
  
  #' ### Interim
  ## ------------------------------------------------------------------------
  
  d2$Interim <- 0
  
  d2$Interim [ grepl( "SINCE", toupper(d2$TitleTxt) ) ] <- 1
  d2$Interim [ grepl( "FROM", toupper(d2$TitleTxt) ) ] <- 1
  d2$Interim [ grepl( "INTERIM", toupper(d2$TitleTxt) ) ] <- 1
  d2$Interim [ grepl( "TEMP", toupper(d2$TitleTxt) ) ] <- 1
  d2$Interim [ grepl( "TEMPORARY", toupper(d2$TitleTxt) ) ] <- 1
  
  
  
  #' ### Former
  ## ------------------------------------------------------------------------
  
  d2$Former <- 0
  d2$Former [ grepl( "FORMER",   toupper(d2$TitleTxt) ) ] <- 1
  d2$Former [ grepl( "THRU",     toupper(d2$TitleTxt) ) ] <- 1
  d2$Former [ grepl( "THROUGH", toupper(d2$TitleTxt) ) ] <- 1
  d2$Former [ grepl( "PAST", toupper(d2$TitleTxt) ) ] <- 1
  d2$Former [ d2$FmrOfficer == 1 ] <- 1
  
  
  
  #' ## Roles and Positions Descriptive Statistics
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











assign_roles <- function( comp.dat )
{
  
  d2 <- comp.dat 
  
  #' ##  Roles vs Responsibilities
  #' 
  #' Role specifies a hierarchical role within the organization. 
  #' 
  #' Responsibility is the domain in which a person operates. 
  #' 
  #' For example, the director of development has a role as a director, and is responsible for development. CFO has a role of chief officer, and responsibility of finance. Vice president of academic affairs has a role as vice president, responsibility over academic affairs. 
  #' 
  #' The goal is to be able to select by hierarchical position (director, officer, former, manager, vice-president, etc.) or domain in which they work (finance, development, academic, legal, marketing, etc.). 
  #' 
  #' 
  #' 
  #' ##  Seperates Roles and Responsiblities
  #' 
  #' Find the Most Common Words that Indicate Role
  #' 
  #' 1. Address executive titles
  #' 2. Seperate unambigious roles from responsibilities
  #' 3. Seperate ambigious roles from responsibilities using logical opperators
  #' 4. Clean up Extra Spaces
  #' 5. Check 25 Random Titles
  #' 
  #' 
  #' 
  #' 
  ## ------------------------------------------------------------------------
  
  Role.Dictionary <-  
    c( " PAST ", " FORMER ", " INTERIM ", 
       " SENIOR ", " DIRECTOR ", " DEPUTY ", 
       " TRUSTEE ", " MANAGER "," MGR ", 
       " MANAGEMENT ", " HEAD ", " COORDINATOR ", 
       " CO ", " TRUSTEE ", " SENIOR VICE PRESIDENT ", 
       " EXECUTIVE VICE ", " DEAN ", " ASSISTANT ", 
       " ASSOCIATE ", " EXECUTIVE VICE PRESIDENT ", 
       " VICE ", " ASSOCIATE ", " SUPERVISING ", 
       " PRESIDENT ", " HEAD_START " )
  
  # Role.Dictionary
  
  
  #' ## Executive titles
  #' 
  #' Title                        Role              Responsibility
  #' -----------------------     ----------------   ----------------
  #' Chief Executive OFFicer      Chief Officer      Cheif Executive Officer
  #' CEO                          Chief Officer      CEO
  #' Chief Finiancial Officer     Chief Officer      Chief Finiancial Officer
  #' CFO                          Chief Officer      CFO
  #' 
  #' 
  ## ------------------------------------------------------------------------
  d2$TitleRole <- "  "
  d2$TitleResp <- d2$TitleTxt2
  
  d2$TitleResp <- str_c( " ", d2$TitleResp )
  d2$TitleResp <- str_c( d2$TitleResp, " " )
  
  # REPLACE
  # d2$TitleResp <- gsub( " VICE PRESIDENT ", " VICE#PRESIDENT ", d2$TitleResp )
  # d2$TitleResp <- gsub( " EXECUTIVE DIRECTOR ", " EXECUTIVE_DIRECTOR ", d2$TitleResp )
  # d2$TitleResp <- gsub( " HEAD START ", " HEAD_START ", d2$TitleResp )
  
  #Chief Executive
  d2$TitleRole[ grepl( " CHIEF ", d2$TitleResp ) & grepl( " EXECUTIVE ", d2$TitleResp ) ] <- " CHIEF EXECUTIVE "
  d2$TitleRole[ grepl( " CHIEF ", d2$TitleResp ) & grepl( " OFFICER ", d2$TitleResp ) ] <- " CHIEF OFFICER "
  d2$TitleRole[ grepl( " CHIEF ", d2$TitleResp ) & !grepl( " OFFICER ", d2$TitleResp ) ] <- " CHIEF "
  d2$TitleResp[ grepl( " CHIEF ", d2$TitleResp ) & !grepl( " OFFICER ", d2$TitleResp ) ] <- (gsub( " CHIEF ", " ", d2$TitleResp))[ grepl( " CHIEF ", d2$TitleResp ) & !grepl( " OFFICER ", d2$TitleResp ) ]
  
  d2$TitleRole[ grepl( " C[A-Z]O ", d2$TitleResp) ] <- " CHIEF OFFICER "
  
  #EXECUTIVE DIRECTOR
  d2$TitleRole[ grepl( " EXECUTIVE DIRECTOR ", d2$TitleResp ) ] <- paste(d2$TitleRole, " EXECUTIVE DIRECTOR ", sep ="")[ grepl( " EXECUTIVE DIRECTOR ", d2$TitleResp ) ]
  d2$TitleResp <- gsub( "EXECUTIVE DIRECTOR ", " ", d2$TitleResp)
  
  
  #' ## Clear Titles
  ## ------------------------------------------------------------------------
  
  #PAST
  d2$TitleRole[ grepl( " PAST ", d2$TitleResp) ] <- paste(d2$TitleRole, " PAST ", sep ="")[ grepl( " PAST ", d2$TitleResp) ]
  d2$TitleResp <- gsub( "PAST", " ", d2$TitleResp)
  
  #FORMER
  d2$TitleRole[ grepl( " FORMER ", d2$ TitleResp) ] <- paste(d2$TitleRole, " FORMER ", sep ="")[ grepl( " FORMER ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " FORMER ", " ", d2$TitleResp)
  
  #INTERIM
  d2$TitleRole[ grepl( " INTERIM ", d2$ TitleResp) ] <- paste(d2$TitleRole, " INTERIM ", sep ="")[ grepl( " INTERIM ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " INTERIM ", " ", d2$TitleResp)
  
  #" CO "
  d2$TitleRole[ grepl( " CO ", d2$ TitleResp) ] <- paste(d2$TitleRole, " CO ", sep ="")[ grepl( " CO ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " CO ", " ", d2$TitleResp)
  
  d2$TitleRole[ grepl( " FOUNDER ", d2$ TitleResp) ] <- paste(d2$TitleRole, " FOUNDER ", sep ="")[ grepl( " FOUNDER ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " FOUNDER ", " ", d2$TitleResp)
  
  
  #Executive Senior Vice President
  d2$TitleRole[ grepl( " EXECUTIVE SENIOR VICE PRESIDENT ", d2$ TitleResp) ] <- paste(d2$TitleRole, " EXECUTIVE SENIOR VICE PRESIDENT ", sep ="")[ grepl( " EXECUTIVE SENIOR VICE PRESIDENT ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " EXECUTIVE SENIOR VICE PRESIDENT ", " ", d2$TitleResp)
  
  #Senior Executive Vice President
  d2$TitleRole[ grepl( " SENIOR EXECUTIVE VICE PRESIDENT ", d2$ TitleResp) ] <- paste(d2$TitleRole, " SENIOR EXECUTIVE VICE PRESIDENT ", sep ="")[ grepl( " SENIOR EXECUTIVE VICE PRESIDENT ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " SENIOR EXECUTIVE VICE PRESIDENT ", " ", d2$TitleResp)
  
  # Senior Vice President
  d2$TitleRole[ grepl( " SENIOR VICE PRESIDENT ", d2$ TitleResp) ] <- paste(d2$TitleRole, " SENIOR VICE PRESIDENT ", sep ="")[ grepl( " SENIOR VICE PRESIDENT ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " SENIOR VICE PRESIDENT ", " ", d2$TitleResp)
  
  # Executive Vice President
  d2$TitleRole[ grepl( " EXECUTIVE VICE PRESIDENT ", d2$ TitleResp) ] <- paste(d2$TitleRole, " EXECUTIVE VICE PRESIDENT ", sep ="")[ grepl( " EXECUTIVE VICE PRESIDENT ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " EXECUTIVE VICE PRESIDENT ", " ", d2$TitleResp)
  
  # Vice President
  d2$TitleRole[ grepl( " VICE PRESIDENT ", d2$ TitleResp) ] <- paste(d2$TitleRole, " VICE PRESIDENT ", sep ="")[ grepl( " VICE PRESIDENT ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " VICE PRESIDENT ", " ", d2$TitleResp)
  
  # Senior 
  d2$TitleRole[ grepl( " SENIOR ", d2$ TitleResp) ] <- paste(d2$TitleRole, " SENIOR ", sep ="")[ grepl( " SENIOR ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " SENIOR ", " ", d2$TitleResp)
  
  # DEPUTY
  d2$TitleRole[ grepl( " DEPUTY ", d2$ TitleResp) ] <- paste(d2$TitleRole, " DEPUTY ", sep ="")[ grepl( " DEPUTY ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " DEPUTY ", " ", d2$TitleResp)
  
  # President
  d2$TitleRole[ grepl( " PRESIDENT ", d2$ TitleResp) ] <- paste(d2$TitleRole, " PRESIDENT ", sep ="")[ grepl( " PRESIDENT ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " PRESIDENT ", " ", d2$TitleResp)
  
  # " TRUSTEE ",
  d2$TitleRole[ grepl( " TRUSTEE ", d2$ TitleResp) ] <- paste(d2$TitleRole, " TRUSTEE ", sep ="")[ grepl( " TRUSTEE ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " TRUSTEE ", " ", d2$TitleResp)
  
  # " MANAGER ",
  d2$TitleRole[ grepl( " MANAGER ", d2$ TitleResp) ] <- paste(d2$TitleRole, " MANAGER ", sep ="")[ grepl( " MANAGER ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " MANAGER ", " ", d2$TitleResp)
  
  # " MANAGEMENT "
  d2$TitleRole[ grepl( " MANAGEMENT ", d2$ TitleResp) ] <- paste(d2$TitleRole, " MANAGEMENT ", sep ="")[ grepl( " MANAGEMENT ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " MANAGEMENT ", " ", d2$TitleResp)
  
  # " COORDINATOR "
  d2$TitleRole[ grepl( " COORDINATOR ", d2$ TitleResp) ] <- paste(d2$TitleRole, " COORDINATOR ", sep ="")[ grepl( " COORDINATOR ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " COORDINATOR ", " ", d2$TitleResp)
  
  # " DEAN " 
  d2$TitleRole[ grepl( " DEAN ", d2$ TitleResp) ] <- paste(d2$TitleRole, " DEAN ", sep ="")[ grepl( " DEAN ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " DEAN ", " ", d2$TitleResp)
  
  # " SUPERVISING "
  d2$TitleRole[ grepl( " SUPERVISING ", d2$ TitleResp) ] <- paste(d2$TitleRole, " SUPERVISING ", sep ="")[ grepl( " SUPERVISING ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " SUPERVISING ", " ", d2$TitleResp)
  
  # " DEPARTMENT CHAIR "
  d2$TitleRole[ grepl( " DEPARTMENT CHAIR ", d2$ TitleResp) ] <- paste(d2$TitleRole, " DEPARTMENT CHAIR ", sep ="")[ grepl( " DEPARTMENT CHAIR ", d2$ TitleResp) ]
  d2$TitleResp <- gsub( " DEPARTMENT CHAIR ", " ", d2$TitleResp)
  
  
  
  #' ## Titles that require logical Operators to create
  ## ------------------------------------------------------------------------
  
  # Executive
  d2$TitleRole[ grepl( " EXECUTIVE ", d2$ TitleResp ) & !grepl( " CHIEF ", d2$TitleResp )  & !grepl( " EXECUTIVE DIRECTOR ", d2$TitleResp ) & d2$ADMIN.SUP != 1 ] <- paste(d2$TitleRole, " EXECUTIVE ", sep ="")[ grepl( " EXECUTIVE ", d2$TitleResp ) & !grepl( " CHIEF ", d2$TitleResp )  & !grepl( " EXECUTIVE DIRECTOR ", d2$TitleResp ) & d2$ADMIN.SUP != 1 ]
  d2$TitleResp[ grepl( " EXECUTIVE ", d2$ TitleResp ) & !grepl( " CHIEF ", d2$TitleResp )  & !grepl( " EXECUTIVE DIRECTOR ", d2$TitleResp ) & d2$ADMIN.SUP != 1 ] <- gsub( " EXECUTIVE ", " ",  d2$TitleResp )[ grepl( " EXECUTIVE ", d2$ TitleResp ) & !grepl( " CHIEF ", d2$TitleResp )  & !grepl( " EXECUTIVE DIRECTOR ", d2$TitleResp ) & d2$ADMIN.SUP != 1 ]
  
  # DIRECTOR
  d2$TitleRole[ grepl( " DIRECTOR ", d2$ TitleResp ) &  !grepl( " EXECUTIVE ", d2$TitleResp ) ] <- paste(d2$TitleRole, " DIRECTOR ", sep ="")[ grepl( " DIRECTOR ", d2$ TitleResp ) &  !grepl( " EXECUTIVE ", d2$TitleResp ) ]
  d2$TitleResp[ grepl( " DIRECTOR ", d2$ TitleResp ) &  !grepl( " EXECUTIVE ", d2$TitleResp ) ] <- gsub( " DIRECTOR ", " ", d2$TitleResp )[ grepl( " DIRECTOR ", d2$ TitleResp ) &  !grepl( " EXECUTIVE ", d2$TitleResp ) ]
  
  
  # ASSISTANT Only if not Administrative Support
  d2$TitleRole[ grepl( " ASSISTANT ", d2$ TitleResp ) & d2$ADMIN.SUP != 1 ] <- paste(d2$TitleRole, " ASSISTANT ", sep ="")[ grepl( " ASSISTANT ", d2$ TitleResp ) & d2$ADMIN.SUP != 1 ]
  d2$TitleResp[ grepl( " ASSISTANT ", d2$ TitleResp ) & d2$ADMIN.SUP != 1 ] <- gsub( " ASSISTANT ", " ", d2$TitleResp)[ grepl( " ASSISTANT ", d2$ TitleResp ) & d2$ADMIN.SUP != 1 ]
  
  # ASSOCIATE  Only if not Administrative Support
  d2$TitleRole[ grepl( " ASSOCIATE ", d2$ TitleResp ) & d2$ADMIN.SUP != 1 ] <- paste(d2$TitleRole, " ASSOCIATE ", sep ="")[ grepl( " ASSOCIATE ", d2$ TitleResp ) & d2$ADMIN.SUP != 1 ]
  d2$TitleResp[ grepl( " ASSOCIATE ", d2$ TitleResp ) & d2$ADMIN.SUP != 1 ] <- gsub( " ASSOCIATE ", " ", d2$TitleResp)[ grepl( " ASSOCIATE ", d2$ TitleResp ) & d2$ADMIN.SUP != 1 ]
  
  # Title.Test <- data.frame( TitleTxt=as.character(d2$TitleTxt), 
  #                           TitleTxt2=d2$TitleTxt2, 
  #                           TitleRole=d2$TitleRole, 
  #                           TitleResp=d2$TitleResp ) %>% as.data.frame() 
  # colnames( Title.Test ) <- c( "Title", "Cleaned Title", "Role", "Responsibilty" )
  
  #DIVISION HEAD
  d2$TitleRole[ grepl( " DIVISION HEAD ", d2$TitleResp ) ] <- paste(d2$TitleRole, " DIVISION HEAD ", sep ="")[ grepl( " DIVISION HEAD ", d2$TitleResp )]
  d2$TitleResp[ grepl( " DIVISION HEAD ", d2$TitleResp ) ] <- gsub( " DIVISION HEAD ", " ", d2$TitleResp)[ grepl( " DIVISION HEAD ", d2$ TitleResp )]
  
  #" HEAD "
  d2$TitleRole[ grepl( " HEAD ", d2$TitleResp) & !grepl( " HEAD START ", d2$ TitleResp) & !grepl(" MASTER ", d2$TitleResp) & !grepl(" SCHOOL ", d2$TitleResp)  ] <- paste(d2$TitleRole, " HEAD ", sep ="")[ grepl( " HEAD ", d2$TitleResp) & !grepl( " HEAD START ", d2$ TitleResp) & !grepl(" MASTER ", d2$TitleResp) & !grepl(" SCHOOL ", d2$TitleResp)  ]
  d2$TitleResp[ grepl( " HEAD ", d2$TitleResp) & !grepl( " HEAD START ", d2$ TitleResp) & !grepl(" MASTER ", d2$TitleResp) & !grepl(" SCHOOL ", d2$TitleResp)  ] <- gsub( " HEAD ", " ", d2$TitleResp )[ grepl( " HEAD ", d2$TitleResp) & !grepl( " HEAD START ", d2$TitleResp) & !grepl(" MASTER ", d2$TitleResp) & !grepl(" SCHOOL ", d2$TitleResp ) ]
  
  
  #' ## Remove Extra Spaces from Titles
  ## ------------------------------------------------------------------------
  
  d2$TitleResp <- gsub( "  ", " ", d2$TitleResp )
  d2$TitleResp <- gsub( "  ", " ", d2$TitleResp )
  d2$TitleResp <- gsub( "  ", " ", d2$TitleResp )
  d2$TitleResp <- gsub( "  ", " ", d2$TitleResp )
  d2$TitleResp <- gsub( "  ", " ", d2$TitleResp )
  d2$TitleResp <- gsub( "  ", " ", d2$TitleResp )
  d2$TitleResp <- gsub( "  ", " ", d2$TitleResp )
  d2$TitleResp <- gsub( "  ", " ", d2$TitleResp )
  
  
  d2$TitleRole <- gsub( "  ", " ", d2$TitleRole )
  d2$TitleRole <- gsub( "  ", " ", d2$TitleRole )
  d2$TitleRole <- gsub( "  ", " ", d2$TitleRole )
  d2$TitleRole <- gsub( "  ", " ", d2$TitleRole )
  d2$TitleRole <- gsub( "  ", " ", d2$TitleRole )
  d2$TitleRole <- gsub( "  ", " ", d2$TitleRole )
  d2$TitleRole <- gsub( "  ", " ", d2$TitleRole )
  d2$TitleRole <- gsub( "  ", " ", d2$TitleRole )
  
  return( d2 )
  
}
