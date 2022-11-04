#Step 5: Standardize Spelling

require(hunspell)

# 05-standardize-spelling.R

#' @title 
#' standardize spelling wrapper function
#'
#' @description 
#' apply custom dictionary of common abbreviations and misspellings 
#' to standardize titles
#' 
#' @export
standardize_spelling <- function(comp.data, title="TitleTxt4"){
  TitleTxt = comp.data[[title]]
  
  TitleTxt <- apply_substitutes(TitleTxt)
  TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  
  comp.data$TitleTxt5 <- TitleTxt
  
  print("standardize spelling step complete")
  
  return(comp.data)
}



#' @title 
#' apply fix spelling function
#' 
#' @description 
#' subwrapper function for standardize spelling by applying all the substitutes
#' on the titletxt vector (or singular string)
#' 
#' @export
apply_substitutes <- function(TitleTxt){
  TitleTxt <- fix_vice(TitleTxt)
  TitleTxt <- fix_executive(TitleTxt)
  TitleTxt <- fix_director(TitleTxt)
  TitleTxt <- fix_operations(TitleTxt) #operations/operating
  TitleTxt <- fix_assistant(TitleTxt) #assistant/associate
  TitleTxt <- fix_president(TitleTxt)
  TitleTxt <- fix_secretary(TitleTxt)
  TitleTxt <- fix_treasurer(TitleTxt)
  TitleTxt <- fix_finance(TitleTxt) #finance/financial
  TitleTxt <- fix_senior(TitleTxt) #senior/junior
  TitleTxt <- fix_development(TitleTxt)
  TitleTxt <- fix_chair(TitleTxt)
  TitleTxt <- fix_officer(TitleTxt)
  TitleTxt <- fix_admin(TitleTxt)
  TitleTxt <- fix_coordinator(TitleTxt)
  TitleTxt <- fix_strategy(TitleTxt) #strategy/strategic
  TitleTxt <- fix_hr(TitleTxt)
  TitleTxt <- fix_manage(TitleTxt) #management/managing/manager
  TitleTxt <- fix_programs(TitleTxt) #programs/programming
  TitleTxt <- fix_projects(TitleTxt)
  TitleTxt <- fix_public(TitleTxt)
  TitleTxt <- fix_business(TitleTxt)
  TitleTxt <- fix_comm(TitleTxt) #communication/committee
  TitleTxt <- fix_information(TitleTxt)
  TitleTxt <- fix_intelligence(TitleTxt)
  TitleTxt <- fix_technology(TitleTxt)
  TitleTxt <- fix_institute(TitleTxt) #institute/institutional
  TitleTxt <- fix_academics(TitleTxt) #academics/academy
  TitleTxt <- fix_marketing(TitleTxt)
  TitleTxt <- fix_advancement(TitleTxt)
  TitleTxt <- fix_philanthropy(TitleTxt)
  TitleTxt <- fix_systems(TitleTxt)
  TitleTxt <- fix_general(TitleTxt)
  TitleTxt <- fix_planning(TitleTxt) #planning/planned
  TitleTxt <- fix_compliance(TitleTxt)
  TitleTxt <- fix_enrollment(TitleTxt)
  TitleTxt <- fix_admissions(TitleTxt)
  TitleTxt <- fix_deputy(TitleTxt)
  TitleTxt <- fix_corresponding(TitleTxt) #correspondent/corresponding
  TitleTxt <- fix_emeritus(TitleTxt)
  TitleTxt <- fix_relations(TitleTxt)
  TitleTxt <- fix_representative(TitleTxt)
  TitleTxt <- fix_board(TitleTxt)
  TitleTxt <- fix_transportation(TitleTxt)
  TitleTxt <- fix_exofficio(TitleTxt)
  TitleTxt <- fix_atlarge(TitleTxt)
  TitleTxt <- fix_member(TitleTxt)
  TitleTxt <- fix_governor(TitleTxt)
  
  TitleTxt <- condense_abbreviations(TitleTxt) 
  
  TitleTxt <- fix_miscellaneous(TitleTxt)
  
  #duplicate removal
  TitleTxt <- gsub("PRESIDENT\\s+PRESIDENT", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("PRESIDENTPR.*\\b", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("CEOCEO", "CEO", TitleTxt)
  TitleTxt <- gsub("\\bDIRECTOR DIRECTOR\\b", "DIRECTOR", TitleTxt)
  
  #remove residual spacing issues
  TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  
  #remove unnecessary conjunctions at the end
  TitleTxt <- remove_trailing_conjunctions(TitleTxt)
  
  TitleTxt <- stand_titles(TitleTxt)
  
  TitleTxt <- fix_of(TitleTxt)
  TitleTxt <- gsub("\\bOF AND\\b", "OF", TitleTxt)
  # TitleTxt <- spellcheck(TitleTxt) #slows things down, but is useful
  
  return(TitleTxt)
}








#' @title 
#' standardize versions of 'vice' 
#'
#' @description 
#' condenses vice president abbreviations to a standardized form
#'
#' @export
fix_vice <- function(TitleTxt){
  
  TitleTxt <- gsub( "\\bE\\sV\\sPRESIDENT", "EXECUTIVE VICE PRESIDENT", TitleTxt )
  TitleTxt <- gsub( "\\bE\\sVICE", "EXECUTIVE VICE", TitleTxt )
  TitleTxt <- gsub( "\\bS\\sVICE", "SENIOR VICE", TitleTxt )
  TitleTxt <- gsub( "\\bSR\\s*V\\s*P\\b", "SENIOR VICE PRESIDENT", TitleTxt )
  TitleTxt <- gsub( "\\bE\\s*S\\s*V\\s*P\\b", "EXECUTIVE SENIOR VICE PRESIDENT", TitleTxt )
  TitleTxt <- gsub( "\\bS\\s*E\\s*V\\s*P\\b", "SENIOR EXECUTIVE VICE PRESIDENT", TitleTxt )
  TitleTxt <- gsub( "\\bS\\s*V\\s*P\\b", "SENIOR VICE PRESIDENT", TitleTxt )
  TitleTxt <- gsub( "\\bS\\s*V\\s*PI\\b", "SENIOR VICE PRESIDENT", TitleTxt )
  TitleTxt <- gsub( "\\bE\\s*V\\s*P\\b", "EXECUTIVE VICE PRESIDENT", TitleTxt )
  TitleTxt <- gsub( "\\bA\\s*V\\s*P\\b", "ASSISTANT VICE PRESIDENT", TitleTxt )
  TitleTxt <- gsub( "\\bR\\s*V\\s*P\\b", "REGIONAL VICE PRESIDENT", TitleTxt )
  
  TitleTxt <- gsub( "\\bV\\s*P\\s*F\\b", "VICE PRESIDENT OF FINANCE", TitleTxt )
  TitleTxt <- gsub( "\\bV\\s*P\\s*O\\b", "VICE PRESIDENT OF OPERATIONS", TitleTxt )
  TitleTxt <- gsub( "\\bV\\s*P\\s*H\\s*R\\b", "VICE PRESIDENT OF HUMAN RESOURCES", TitleTxt )
  TitleTxt <- gsub( "\\bV\\s*P\\s*M\\s*A\\b", "VICE PRESIDENT OF MEDICAL AFFAIRS", TitleTxt )
  
  TitleTxt <- gsub( "\\bV\\s*P[A-Z]*\\b", "VICE PRESIDENT", TitleTxt )
  TitleTxt <- gsub("\\bVICE\\s*P[A-Z]*\\b", "VICE PRESIDENT", TitleTxt)
  
  
  TitleTxt <- gsub("\\bVI$", "VICE PRESIDENT", TitleTxt) #heuristic
  TitleTxt <- gsub("\\bVIC$", "VICE PRESIDENT", TitleTxt) #heuristic
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'executive'  
#'
#' @description 
#' condenses executive abbreviations to a standardized form
#'
#' @export
fix_executive <- function(TitleTxt){
  
  #smushed words
  TitleTxt <- gsub("\\bEXECDIR[A-Z]*\\b", "EXECUTIVE DIRECTOR", TitleTxt)
  TitleTxt <- gsub("\\bEXDIR[A-Z]*\\b", "EXECUTIVE DIRECTOR", TitleTxt)
  
  TitleTxt <- gsub("\\bEXE[A-Z]*\\b", "EXECUTIVE",TitleTxt)
  TitleTxt <- gsub("\\bEXC[A-Z]*\\b", "EXECUTIVE",TitleTxt)
  
  #ex officio
  TitleTxt <- ifelse(!grepl("\\bEX O", TitleTxt) & !grepl("\\bEX-O", TitleTxt),
         gsub("\\bEX\\b","EXECUTIVE", TitleTxt), TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'director' 
#' 
#' @description 
#' condenses director abbreviations to a standardized form
#'
#' @export
fix_director <- function(TitleTxt){
  TitleTxt <- ifelse(!grepl("CEO", TitleTxt),
                     gsub("\\bDIR[A-Z]*\\b", "DIRECTOR", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(!grepl("CEO", TitleTxt),
                     gsub("\\bDI\\b","DIRECTOR", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(!grepl("CEO", TitleTxt),
                     gsub("\\bDTR\\b","DIRECTOR", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(!grepl("CEO", TitleTxt),
                     gsub("\\bDRECTOR\\b","DIRECTOR", TitleTxt), TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'operations' 
#' 
#' @description 
#' condenses operations abbreviations to a standardized form
#' function leaves "operating" alone --> does not convert to operations
#'
#' @export
fix_operations <- function(TitleTxt){
  
  TitleTxt <- ifelse(grepl("\\bOPERATIN",TitleTxt),
                     gsub("\\bOPERATIN\\b", "OPERATING", TitleTxt),
                     gsub("\\bOP[A-Z]*\\b", "OPERATIONS", TitleTxt))
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'assistant' 
#' 
#' @description 
#' condenses assistant abbreviations to a standardized form
#' also condenses associate
#'
#' @export
fix_assistant <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bASSI[A-Z]*\\b", "ASSISTANT", TitleTxt)
  TitleTxt <- gsub( "\\bASS\\s*T\\b", "ASSISTANT", TitleTxt)
  TitleTxt <- gsub( "^\\s*A\\b", "ASSISTANT", TitleTxt)
  
  #skipping association
  TitleTxt <- ifelse(!grepl("\\bASSOCIATI[A-Z]*\\b", TitleTxt), 
                     gsub( "\\bASSOC[A-Z]*\\b", "ASSOCIATE", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(!grepl("\\bASSOCIATI[A-Z]*\\b", TitleTxt), 
                     gsub( "\\bASSC[A-Z]*\\b", "ASSOCIATE", TitleTxt), TitleTxt)
  return(TitleTxt)
  
}

#' @title 
#' standardize versions of 'president' 
#' 
#' @description 
#' condenses president abbreviations to a standardized form
#' ignores "presiding"
#'
#' @export
fix_president <- function(TitleTxt){
  presidingCheck <- ifelse(grepl("PRESIDING", TitleTxt), FALSE, TRUE)
  
  TitleTxt <- ifelse(presidingCheck, 
                     gsub("\\bPRESI[A-Z]*\\b", "PRESIDENT", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(presidingCheck, 
                     gsub("\\bPRES\\b", "PRESIDENT", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(presidingCheck, 
                     gsub("\\bPRE\\b", "PRESIDENT", TitleTxt), TitleTxt)
  TitleTxt <- ifelse(presidingCheck, 
                     gsub("^\\bP\\b$", "PRESIDENT", TitleTxt), TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'secretary'
#' 
#' @description 
#' condenses secretary abbreviations to a standardized form
#'
#' @export
fix_secretary <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bSECR[A-Z]*\\b", "SECRETARY",TitleTxt)
  
  # TitleTxt <- gsub("\\bS\\b", "SECRETARY", TitleTxt) #assume standalone s is sec
  #cant do s+r
  TitleTxt <- gsub("\\bSE\\b", "SECRETARY", TitleTxt) #ditto but with se
  TitleTxt <- gsub("\\bSEC\\b", "SECRETARY", TitleTxt) #ditto but with sec
  TitleTxt <- gsub("\\bSECY\\b", "SECRETARY", TitleTxt) #ditto but with secy
  TitleTxt <- gsub("\\bSCRTRY\\b", "SECRETARY", TitleTxt)
  
  TitleTxt <- gsub("\\bSECT[A-Z]*\\b", "SECRETARY", TitleTxt) 
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of treasurer helper function
#' 
#' @description 
#' condenses treasurer abbreviations to a standardized form
#'
#' @export
fix_treasurer <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bTRE[A-Z]*\\b", "TREASURER", TitleTxt)
  TitleTxt <- gsub("\\bTR\\b", "TREASURER", TitleTxt)
  TitleTxt <- gsub("\\bTRSR\\b", "TREASURER", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'finance'
#' 
#' @description 
#' condenses finance abbreviations to a standardized form
#' financial is included
#'
#' @export
fix_finance <- function(TitleTxt){
  
  #everything gets converted to finance (including financial)
  TitleTxt <- gsub("\\bFIN[A-Z]*\\b", "FINANCE", TitleTxt)
  TitleTxt <- gsub("\\bFI\\b", "FINANCE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'seniority'
#' 
#' @description 
#' condenses senior/junior abbreviations to a standardized form
#'
#' @export
fix_senior <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bSENI[A-Z]*\\b", "SENIOR", TitleTxt)
  TitleTxt <- gsub("\\bSEN\\b", "SENIOR", TitleTxt)
  TitleTxt <- gsub("\\bSR\\b", "SENIOR", TitleTxt)
  TitleTxt <- gsub("\\bSNR\\b", "SENIOR", TitleTxt)
  
  TitleTxt <- gsub("\\bJUNI[A-Z]*\\b", "JUNIOR", TitleTxt)
  TitleTxt <- gsub( "\\bJR\\b", "JUNIOR", TitleTxt)
  TitleTxt <- gsub( "\\bJT\\b", "JUNIOR", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'development'
#' 
#' @description 
#' condenses development abbreviations to a standardized form
#'
#' @export
fix_development <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bDEV[A-Z]*\\b", "DEVELOPMENT", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'chair'
#' 
#' @description 
#' condenses chair abbreviations to a standardized form
#' e.g. chairperson,chairman,chairwoman --> chair
#' includes vice chair standardizations
#'
#' @export
fix_chair <- function(TitleTxt){
  
  TitleTxt <- gsub( "\\bV\\s*C\\b", "VICE CHAIR", TitleTxt )
  TitleTxt <- gsub( "\\bV\\s\\bCHAIR\\b", "VICE CHAIR", TitleTxt )
  TitleTxt <- gsub( "\\bVICE\\b\\sC\\b", "VICE CHAIR", TitleTxt )
  
  TitleTxt <- gsub("\\bCHAI[A-Z]*\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bCHAIR PERSON\\b", "CHAIR", TitleTxt)
  
  TitleTxt <- gsub("\\bCHA\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bCH\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bC\\b", "CHAIR", TitleTxt) #assume standalone c is chair
  
  TitleTxt <- gsub("\\bCHAR\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bCHR\\b", "CHAIR", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' condense c-suite abbreviations function
#' 
#' @description 
#' condenses spaced out c-suite abbreviations
#' e.g. C E O --> CEO
#' 
#' also includes miscellaneous executive director and key employee substitutions
#'
#' @export
condense_abbreviations <- function(TitleTxt){
  
  TitleTxt <- gsub( "C\\sE\\sO", "CEO", TitleTxt ) #executive
  TitleTxt <- gsub( "C\\sO\\sO", "COO", TitleTxt ) #operating
  TitleTxt <- gsub( "C\\sF\\sO", "CFO", TitleTxt ) #finance
  TitleTxt <- gsub( "C\\sD\\sO", "CDO", TitleTxt ) #data
  TitleTxt <- gsub( "C\\sT\\sO", "CTO", TitleTxt ) #technology
  TitleTxt <- gsub( "C\\sA\\sO", "CAO", TitleTxt ) #administrative
  TitleTxt <- gsub( "C\\sI\\sO", "CIO", TitleTxt ) #information
  TitleTxt <- gsub( "C\\sR\\sN\\sA", "CRNA", TitleTxt ) #certified registered nurse anesthetist
  TitleTxt <- gsub( "C\\sN\\sO", "CNO", TitleTxt ) #nursing
  TitleTxt <- gsub( "C\\sM\\sO", "CMO", TitleTxt ) #marketing
  
  TitleTxt <- gsub( "\\bE\\b DIRECTOR", "EXECUTIVE DIRECTOR", TitleTxt )
  TitleTxt <- gsub( "\\bE\\s*D\\b", "EXECUTIVE DIRECTOR", TitleTxt )
  TitleTxt <- gsub( "\\bEXECUTIVE D\\b", "EXECUTIVE DIRECTOR", TitleTxt )
  
  TitleTxt <- gsub( "\\bKEY\\s\\bE\\b", "KEY EMPLOYEE\\b", TitleTxt )
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'officer'
#' 
#' @description 
#' condenses officer abbreviations to a standardized form
#'
#' @export
fix_officer <- function(TitleTxt){
  
  noReplaceCheck <- ifelse(grepl("OFFICE ", TitleTxt) | 
                             grepl("\\bEX O", TitleTxt) | 
                             grepl("\\bEX-O", TitleTxt), FALSE, TRUE)
  TitleTxt <- ifelse(noReplaceCheck, 
                     gsub("\\bOFF[A-Z]*\\b", "OFFICER", TitleTxt), TitleTxt)
  
  TitleTxt <- gsub("\\bOFCR\\b", "OFFICER", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'admin'
#' 
#' @description 
#' condenses administration/administrator/administrating 
#' abbreviations to a standardized form
#'
#' @export
fix_admin <- function(TitleTxt){
  
  #administrator
  TitleTxt <- ifelse(grepl("\\bADMINISTRATO", TitleTxt),
                     gsub("\\bADMINISTRATO\\b", "ADMINISTRATOR", TitleTxt),
                     TitleTxt)
  
  #administrative
  TitleTxt <- ifelse(grepl("\\bADMINISTRATIV", TitleTxt), 
                      gsub("\\bADMINISTRATIV\\b", "ADMINISTRATIVE", TitleTxt),
                      TitleTxt)
  
  adminCheck <- ifelse(grepl("\\bADMINISTRATOR", TitleTxt) | 
                         grepl("\\bADMINISTRATIVE", TitleTxt), FALSE, TRUE)
  
  
  TitleTxt <- ifelse(adminCheck,
                     gsub("\\bADMIN[A-Z]*\\b", "ADMINISTRATION", TitleTxt),
                     TitleTxt)
  TitleTxt <- ifelse(adminCheck,
                     gsub("\\bADMI\\b", "ADMINISTRATION", TitleTxt),
                     TitleTxt)
  TitleTxt <- ifelse(adminCheck,
                     gsub("\\bADM\\b", "ADMINISTRATION", TitleTxt),
                     TitleTxt)
  TitleTxt <- ifelse(adminCheck,
                     gsub("\\bADMN\\b", "ADMINISTRATION", TitleTxt),
                     TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'coordinator'
#' 
#' @description 
#' condenses coordinator abbreviations to a standardized form
#' note: instead of standardizing "CO" at the end of a title to coordinator,
#' it is instead mapped to "COMMITTEE"
#'
#' @export
fix_coordinator <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bCOOR[A-Z]*\\b", "COORDINATOR",TitleTxt)
  
  TitleTxt <- gsub("\\bCO$", "COMMITTEE", TitleTxt) 
  #heuristic (could also be company, committee, coordinator, etc.)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'strategy'
#' 
#' @description 
#' condenses strategy abbreviations to a standardized form
#' also condenses strategic to strategy
#'
#' @export
fix_strategy <- function(TitleTxt){
  
  #strategic gets mapped to strategy as well
  TitleTxt <- gsub("\\bSTRAT[A-Z]*\\b", "STRATEGY", TitleTxt)
  TitleTxt <- gsub("\\bSTRGY\\b", "STRATEGY", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'human resources'
#' 
#' @description 
#' condenses hr abbreviations to a standardized form
#'
#' @export
fix_hr <- function(TitleTxt){
  
  TitleTxt <- gsub( "\\bHU\\b", "HUMAN", TitleTxt )
  TitleTxt <- gsub( "\\bHUM\\b", "HUMAN", TitleTxt )
  TitleTxt <- gsub( "\\bHUMA\\b", "HUMAN", TitleTxt )
  
  TitleTxt <- gsub("\\bRESO[A-Z]*\\b", "RESOURCES", TitleTxt)
  TitleTxt <- gsub( "\\bRES\\b", "RESOURCES", TitleTxt )
  
  TitleTxt <- gsub( "\\bH\\s*R\\b", "HUMAN RESOURCES", TitleTxt )
  TitleTxt <- gsub( "\\bHUMAN RE\\b", "HUMAN RESOURCES", TitleTxt )
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'manage' (root of management)
#' 
#' @description 
#' condenses management/manager/managing
#'  abbreviations to a standardized form
#'
#' @export
fix_manage <- function(TitleTxt){
  
  management.texts <- c("\\bMANAGEMEN\\b","\\bMANAGEME\\b",
                        "\\bMANAGEM\\b","\\bMANAGE\\b",
                        "\\bMANAG\\b","\\bMANA\\b","\\bMAN\\b",
                        "\\bMGMT\\b","\\bMGM\\b")
  for(name in management.texts){
    TitleTxt <- gsub(name,"MANAGEMENT",TitleTxt)
  }
  
  #managing
  TitleTxt <- gsub("\\bMANAGIN\\b","MANAGING",TitleTxt)
  TitleTxt <- gsub("\\bMANAGI\\b","MANAGING",TitleTxt)
  
  #manager
  TitleTxt <- gsub("\\bMANGER\\b","MANAGER",TitleTxt)
  TitleTxt <- gsub("\\bMGR\\b","MANAGER",TitleTxt)
  TitleTxt <- gsub("\\bMNGR\\b","MANAGER",TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'programs'
#' 
#' @description 
#' condenses programs abbreviations to a standardized form
#' skips over programming
#'
#' @export
fix_programs <- function(TitleTxt){
  
  TitleTxt <- ifelse(grepl("\\bPROGRAMMI", TitleTxt), 
                     gsub("\\bPROGRAMMI[A-Z]*\\b", "PROGRAMMING", TitleTxt), TitleTxt)
  
  #everything else = programs
  TitleTxt <- ifelse(!grepl("\\bPROGRAMMING", TitleTxt), 
                     gsub("\\bPROG[A-Z]*\\b", "PROGRAMS", TitleTxt), TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'projects'
#' 
#' @description 
#' condenses projects abbreviations to a standardized form
#'
#' @export
fix_projects <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bPROJ[A-Z]*\\b", "PROJECTS", TitleTxt)
  
  return(TitleTxt)  
}

#' @title 
#' standardize versions of 'public' 
#' 
#' @description 
#' condenses public abbreviations to a standardized form
#'
#' @export
fix_public <- function(TitleTxt){
  
  public.texts <- c("\\bPUBLI\\b", "\\bPUBL\\b", "\\bPUB\\b")
  for(name in public.texts){
    TitleTxt <- gsub(name,"PUBLIC",TitleTxt)
  }
  
  return(TitleTxt)
  
}

#' @title 
#' standardize versions of 'business'
#' 
#' @description 
#' condenses business abbreviations to a standardized form
#'
#' @export
fix_business <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bBUS[A-Z]*\\b", "BUSINESS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize abbreviations for 'comm'
#' 
#' @description 
#' condenses communication and committee abbreviations to a standardized form
#'
#' @export
fix_comm <- function(TitleTxt){
  
  #TitleT
  
  #communication
  TitleTxt <- ifelse(!grepl("COMMUNIT", TitleTxt), 
                     gsub("\\bCOMMU[A-Z]*\\b", "COMMUNICATIONS", TitleTxt),
                     TitleTxt)
  TitleTxt <- gsub("\\bCOMMS\\b", "COMMUNICATIONS", TitleTxt)
  
  #committee
  TitleTxt <- gsub("\\bCOMMIT[A-Z]*\\b", "COMMITTEE", TitleTxt)
  itleTxt <- gsub("\\bCOMMI\\b", "COMMITTEE", TitleTxt)
  TitleTxt <- gsub("\\bCOMM\\b", "COMMITTEE", TitleTxt)
  TitleTxt <- gsub("\\bCOM\\b", "COMMITTEE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'information'
#' 
#' @description 
#' condenses info abbreviations to a standardized form
#'
#' @export
fix_information <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bINFO[A-Z]*\\b", "INFORMATION", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'intelligence' 
#' 
#' @description 
#' condenses intel abbreviations to a standardized form
#'
#' @export
fix_intelligence <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bINTEL[A-Z]*\\b","INTELLIGENCE",TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'technology'
#' 
#' @description 
#' condenses tech abbreviations to a standardized form
#'
#' @export
fix_technology <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bTECH[A-Z]*\\b", "TECHNOLOGY", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'institute' (root of institutional)
#' 
#' @description 
#' condenses institute/institutional abbreviations to a standardized form
#' 
#' also includes instructor standardization
#'
#' @export
fix_institute <- function(TitleTxt){
  
  TitleTxt <- ifelse(grepl("\\bINSTITUTIONA", TitleTxt), 
                     gsub("\\bINSTITUTIONA\\b", "INSTITUTIONAL", TitleTxt),
                     TitleTxt)
  
  TitleTxt <- ifelse(!grepl("INSTITUTIONAL", TitleTxt), 
                     gsub("\\bINSTI[A-Z]*\\b", "INSTITUTE", TitleTxt),
                     TitleTxt)
  TitleTxt <- gsub("\\bINST\\s", "INSTITUTIONAL ", TitleTxt)
  
  TitleTxt <- gsub("\\bINSTRUC[A-Z]*\\b","INSTRUCTOR", TitleTxt)
  TitleTxt <- gsub("\\bINST$", "INSTRUCTOR", TitleTxt)
  
  return(TitleTxt)
  
}

#' @title 
#' standardize versions of 'academics'
#' 
#' @description 
#' condenses academics abbreviations to a standardized form
#'
#' @export
fix_academics <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bACAD[A-Z]*\\b", "ACADEMICS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'marketing'
#' 
#' @description 
#' condenses marketing abbreviations to a standardized form
#'
#' @export
fix_marketing <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bMARK[A-Z]*\\b", "MARKETING", TitleTxt)
  TitleTxt <- gsub("\\bMKTG\\b", "MARKETING", TitleTxt)
  TitleTxt <- gsub("\\bMKT\\b", "MARKETING", TitleTxt)
  TitleTxt <- gsub("\\bMRKTNG\\b", "MARKETING", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'advancement'
#' 
#' @description 
#' condenses advancement abbreviations to a standardized form
#'
#' @export
fix_advancement <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bADVA[A-Z]*\\b", "ADVANCEMENT", TitleTxt)
  
  # TitleTxt <- gsub("\\bADV\\b", "ADVISOR", TitleTxt) #could be advisor
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'philanthropy'
#' 
#' @description 
#' condenses philanthropy abbreviations to a standardized form
#'
#' @export
fix_philanthropy <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bPHILAN[A-Z]*\\b", "PHILANTHROPY", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'systems'
#' 
#' @description 
#' condenses systems abbreviations to a standardized form
#'
#' @export
fix_systems <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bSYS[A-Z]*\\b", "SYSTEMS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'general'
#' 
#' @description 
#' condenses general abbreviations to a standardized form
#'
#' @export
fix_general <- function(TitleTxt){
  
  TitleTxt <- ifelse(!grepl("GENEALOG",TitleTxt),
                     gsub("\\bGEN[A-Z]*\\b", "GENERAL", TitleTxt),
                     TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'planning'
#' 
#' @description 
#' condenses planning abbreviations to a standardized form
#' includes planned
#'
#' @export
fix_planning <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bPLANN[A-Z]*\\b", "PLANNING", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'compliance'
#' 
#' @description 
#' condenses compliance abbreviations to a standardized form
#'
#' @export
fix_compliance <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bCOMPL[A-Z]*\\b", "COMPLIANCE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'enrollment'
#' 
#' @description
#' condenses enrollment abbreviations to a standardized form 
#'
#' @export
fix_enrollment <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bENRO[A-Z]*\\b", "ENROLLMENT", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'admissions'
#' 
#' @description 
#' condenses admissions abbreviations to a standardized form
#'
#' @export
fix_admissions <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bADMIS[A-Z]*\\b", "ADMISSIONS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'deputy' 
#' 
#' @description 
#' condenses deputy abbreviations to a standardized form
#'
#' @export
fix_deputy <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bDEP[A-Z]*\\b", "DEPUTY", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'corresponding' 
#' 
#' @description 
#' condenses corresponding abbreviations to a standardized form
#'
#' @export
fix_corresponding <- function(TitleTxt){
  
  TitleTxt <- ifelse(!grepl("CORRESPONDENT", TitleTxt), 
                     gsub("\\bCORR[A-Z]*\\b", "CORRESPONDING", TitleTxt),
                     TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'emeritus' 
#' 
#' @description 
#' condenses emeritus abbreviations to a standardized form
#'
#' @export
fix_emeritus <- function(TitleTxt){
  
  TitleTxt <- ifelse(!grepl("EMPLOYEE",TitleTxt), 
                     gsub("\\bEM[A-Z]*\\b", "EMERITUS", TitleTxt),
                     TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'relations' 
#' 
#' @description 
#' condenses relations abbreviations to a standardized form
#'
#' @export
fix_relations <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bRELA[A-Z]*\\b", "RELATIONS", TitleTxt)
  TitleTxt <- gsub("\\bREL\\b", "RELATIONS", TitleTxt)
  
  TitleTxt <- gsub("\\bPR\\s", "PUBLIC RELATIONS ", TitleTxt)
  TitleTxt <- gsub("\\bPUBLIC REALTIONS\\b", "PUBLIC RELATIONS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'representative'
#' 
#' @description 
#' condenses rep abbreviations to a standardized form
#'
#' @export
fix_representative <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bREP[A-Z]*\\b", "REPRESENTATIVE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'board' 
#' 
#' @description 
#' condenses board abbreviations to a standardized form
#'
#' @export
fix_board <- function(TitleTxt){
  
  board.texts <- c("\\bBOAR\\b", "\\bBOA\\b", "\\bBO\\b",
                   "\\bBRD\\b","\\bBOD\\b","\\bBD\\b")
  for(title in board.texts){
    TitleTxt <- gsub(title,"BOARD",TitleTxt)
  }
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'member' 
#' 
#' @description 
#' condenses member abbreviations to a standardized form
#'
#' @export
fix_member <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bMBR\\b", "MEMBER", TitleTxt)
  TitleTxt <- gsub("\\bMBER\\b", "MEMBER", TitleTxt)
  TitleTxt <- gsub("\\bMMBR\\b", "MEMBER", TitleTxt)
  TitleTxt <- gsub("\\bMEM\\b", "MEMBER",TitleTxt)
  TitleTxt <- gsub("\\bMEMB\\b", "MEMBER",TitleTxt)
  TitleTxt <- gsub("\\bME\\b", "MEMBER",TitleTxt)
  TitleTxt <- gsub("\\bMEMEBER\\b", "MEMBER",TitleTxt)
  TitleTxt <- gsub("\\bMEMER\\b", "MEMBER",TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'transportation' 
#' 
#' @description
#' condenses transportation abbreviations to a standardized form 
#'
#' @export
fix_transportation <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bTRANS[A-Z]*\\b", "TRANSPORTATION", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'ex officio' 
#' 
#' @description 
#' condenses ex officio abbreviations to a standardized form
#'
#' @export
fix_exofficio <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bEX\\s+OFF[A-Z]*\\b", "EX-OFFICIO", TitleTxt)
  TitleTxt <- gsub("\\bEX-OFF[A-Z]*\\b", "EX-OFFICIO", TitleTxt)
  
  #typos
  TitleTxt <- gsub("\\bEX OFICIO*\\b", "EX-OFFICIO", TitleTxt)
  TitleTxt <- gsub("\\bEXOFFICIO", "EX-OFFICIO", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'at large'
#' 
#' @description 
#' condenses at large abbreviations to a standardized form
#'
#' @export
fix_atlarge <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bAT LA[A-Z]*\\b", "AT LARGE", TitleTxt)
  TitleTxt <- gsub("\\bAT\\s*$", "AT LARGE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' standardize versions of 'governor'
#' 
#' @description 
#' condenses governor abbreviations to a standardized form
#' also works on governance and government
#'
#' @export
fix_governor <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bGOVT\\b", "GOVERNMENT", TitleTxt)
  TitleTxt <- gsub("\\bGOV'T\\b", "GOVERNMENT", TitleTxt)
  
  TitleTxt <- ifelse(!grepl("GOVERNANCE", TitleTxt) & 
                       grepl("GOVERNMENT", TitleTxt) & 
                       grepl("GOVERNING", TitleTxt), 
                     gsub("\\bGOV[A-Z]*\\b", "GOVERNOR", TitleTxt),
                     TitleTxt)
  return(TitleTxt)
}


#' @title 
#' standardize miscellaneous list of misspellings and abbreviations
#' 
#' @description 
#' substituting all the one's we missed/haven't generalized
#'
#' @export
fix_miscellaneous <- function(TitleTxt){
  
  #miscellaneous:
  TitleTxt <- gsub(  "\\bMINISTR\\b",         "MINISTER",         TitleTxt )
  TitleTxt <- gsub(  "\\bSGT\\b",             "SERGEANT",         TitleTxt)
  TitleTxt <- gsub(  "\\bSARG[A-Z]*\\b",      "SERGEANT",         TitleTxt)
  TitleTxt <- gsub(  "\\bSERG[A-Z]*\\b",      "SERGEANT",         TitleTxt)
  TitleTxt <- gsub(  "\\bSERGEANT ARMS\\b",   "SERGEANT AT ARMS", TitleTxt)
  TitleTxt <- gsub(  "\\bFO\\b",              "FOUNDER",          TitleTxt)
  TitleTxt <- gsub(  "\\bFOU\\b",             "FOUNDER",          TitleTxt)
  TitleTxt <- gsub(  "\\bFOUN\\b",            "FOUNDER",          TitleTxt)
  TitleTxt <- gsub(  "\\bGM\\b",              "GENERAL MANAGER",  TitleTxt)
  TitleTxt <- gsub(  "\\bD AND T\\b",         "DEVELOPMENT AND TECHNOLOGY", TitleTxt)
  TitleTxt <- gsub(  "\\bD\\b", "DIRECTOR", TitleTxt) #assume standalone d is director
  TitleTxt <- gsub(  "\\bDD\\b", "DEPUTY DIRECTOR", TitleTxt) #assume dd = dep director
  TitleTxt <- gsub(  "\\bCO\\s", "CO-", TitleTxt) #co-chair over co chair for example
  TitleTxt <- gsub(  "\\bCL\\b", "CLERK", TitleTxt)
  TitleTxt <- gsub(  "\\bCUL\\b", "CULTURE", TitleTxt)
  TitleTxt <- gsub(  "\\bSERV\\b", "SERVICE", TitleTxt)
  # TitleTxt <- gsub("\\bGOVERN\\b", "GOVERNANCE", TitleTxt)
  TitleTxt <- gsub(  "\\bCHF\\b", "CHIEF", TitleTxt)
  TitleTxt <- gsub(  "\\bREC\\b", "RECORDING", TitleTxt)
  TitleTxt <- gsub(  "\\bORG\\s", "ORGANIZING ", TitleTxt)
  TitleTxt <- gsub(  "\\bCOU\\b", "COUNCIL", TitleTxt)
  TitleTxt <- gsub(  "\\bLIASON\\b", "LIAISON",TitleTxt)
  TitleTxt <- gsub(  "\\bEDUCA\\b", "EDUCATION", TitleTxt)
  TitleTxt <- gsub(  "\\bADV\\s*$", "ADVISOR", TitleTxt)
  TitleTxt <- gsub(  "\\bADV\\b", "ADVANCEMENT", TitleTxt)
  TitleTxt <- gsub(  "\\bEXP\\b","EXPERIENCE", TitleTxt)
  TitleTxt <- gsub(  "\\bOFCR\\b","OFFICER", TitleTxt)
  TitleTxt <- gsub(  "\\bEXT\\b","EXTERNAL", TitleTxt)
  TitleTxt <- gsub(  "\\bCHANCEL[A-Z]*\\b","CHANCELLOR", TitleTxt)
  TitleTxt <- gsub(  "\\bSCHOLARSHIP\\b", "SCHOLARSHIPS", TitleTxt) #standardize plurality
  TitleTxt <- gsub(  "\\bJ\\b","JUNIOR", TitleTxt)
  TitleTxt <- gsub(  "\\bCOUCIL\\b","COUNCIL", TitleTxt)

  TitleTxt <- gsub("\\bFUNDRA\\b","FUNDRAISING", TitleTxt)
  
  TitleTxt <- gsub("\\bER\\b", "EDITOR", TitleTxt)
  TitleTxt <- gsub("\\bEDR\\b", "EDITOR", TitleTxt)
  TitleTxt <- gsub("\\bEDI\\b", "EDITOR", TitleTxt)
  
  TitleTxt <- gsub("\\bAFFA[A-Z]*\\b", "AFFAIRS", TitleTxt)
  TitleTxt <- gsub("\\bCONSU[A-Z]*\\b","CONSULTANT", TitleTxt)
  
  TitleTxt <- gsub("\\bART[A-Z]*\\s", "ARTISTIC ", TitleTxt)
  TitleTxt <- gsub("\\bART[A-Z]*\\b$", "ARTIST", TitleTxt)
  
  TitleTxt <- gsub("\\bCHOREO[A-Z]*\\b","CHOREOGRAPHER",TitleTxt)
  TitleTxt <- gsub("\\bCOREO[A-Z]*\\b","CHOREOGRAPHER",TitleTxt)
  
  # TitleTxt <- gsub("\\bPHY[A-Z]*\\b", "PHYSICIAN", TitleTxt)  # catches PHYSICS
  TitleTxt <- gsub("\\bTHEATR[A-Z]*\\b","THEATER",TitleTxt)
  TitleTxt <- gsub("\\bPARLIA[A-Z]*\\b","PARLIAMENTARIAN",TitleTxt)
  TitleTxt <- gsub("DIRECTOR\\s+CHAPTER", "CHAPTER DIRECTOR", TitleTxt)
  TitleTxt <- gsub("DIRECTOR\\s+JUNIOR", "JUNIOR DIRECTOR", TitleTxt)
  TitleTxt <- gsub("DIRECTOR\\s+SENIOR", "SENIOR DIRECTOR", TitleTxt)
  
  TitleTxt <- gsub("\\bVICE\\s*$", "VICE PRESIDENT",TitleTxt)
  TitleTxt <- gsub("\\bHUMAN\\s*$", "HUMAN RESOURCES", TitleTxt)
  
  #TRUSTEE
  TitleTxt <- gsub("\\bTRTEE\\b", "TRUSTEE", TitleTxt)
  TitleTxt <- gsub("\\bTRU\\b", "TRUSTEE", TitleTxt)
  TitleTxt <- gsub("\\bTSTEE\\b", "TRUSTEE", TitleTxt)
  TitleTxt <- gsub("\\bTTEE\\b", "TRUSTEE", TitleTxt)
  TitleTxt <- gsub("\\bTRUSTE\\b", "TRUSTEE", TitleTxt)
  
  
  #spelling corrections
  TitleTxt <- gsub("\\bCONTRUCTION\\b", "CONSTRUCTION", TitleTxt)
  TitleTxt <- gsub("\\bFORNER\\b", "FORMER", TitleTxt)
  
  #regionals
  TitleTxt <- gsub("\\bSOUTHWES\\b", "SOUTHWEST", TitleTxt)
  TitleTxt <- gsub("\\bSTATE\\b", " ", TitleTxt) #state removed from titles
  
  #duplicate removal
  TitleTxt <- gsub("PRESIDENT\\s+PRESIDENT", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("PRESIDENTPR.*\\b", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("CEOCEO", "CEO", TitleTxt)
  TitleTxt <- gsub("\\bDIRECTOR DIRECTOR\\b", "DIRECTOR", TitleTxt)
  TitleTxt <- gsub("\\bDIRECTOR EXECUTIVE\\b", "EXECUTIVE DIRECTOR", TitleTxt)
  
  #heuristics (won't be much use until we get rid of role statuses)
  
  TitleTxt <- ifelse(grepl("^\\s*VICE\\s*$",TitleTxt), "VICE PRESIDENT", TitleTxt)
  #standalone vice is likely vp
  
  TitleTxt <- ifelse(grepl("^\\s*PR\\s*$",TitleTxt), "PRESIDENT", TitleTxt)
  #standalone pr is likely pres
  
  TitleTxt <- ifelse(grepl("^\\s*EX\\s*$",TitleTxt), "EXECUTIVE", TitleTxt)
  #standalone ex is likely exec
  
  TitleTxt <- ifelse(grepl("\\bT\\b", TitleTxt), gsub("\\bT\\b","TRUSTEE",TitleTxt), TitleTxt)
  #standalone t is likely trustee, but could be treasurer
  
  TitleTxt <- ifelse(grepl("\\bEXECUTIVE V\\b", TitleTxt),
                     gsub("\\bEXECUTIVE V\\b","EXECUTIVE VICE PRESIDENT", TitleTxt),
                     TitleTxt)
  
  TitleTxt <- ifelse(grepl("\\bCE$",TitleTxt), gsub("\\bCE$","CEO",TitleTxt), TitleTxt)
  #ceo misspelling
  
  TitleTxt <- ifelse(grepl("\\bCF$",TitleTxt), gsub("\\bCF$","CFO",TitleTxt), TitleTxt)
  
  TitleTxt <- ifelse(grepl("^EXECUTIVE OFFICER$",TitleTxt), "CEO", TitleTxt)
  
  TitleTxt <- ifelse(grepl("^AT\\s*LARGE$",TitleTxt), "DIRECTOR", TitleTxt) 
  #at large sub
  
  TitleTxt <- ifelse(grepl("^N/A$",TitleTxt) | grepl("^N\\s*A$",TitleTxt) | 
                       grepl("^\\s*$",TitleTxt),
                     "", TitleTxt)

  return(TitleTxt)
}


#' @title 
#' standardize title mapping function
#' 
#' @description 
#' maps long names to abbreviations if CEO, COO, or CFO
#' maps executive director to CEO
#' 
#' @export
stand_titles <- function(TitleTxt){
  
  #convert known c-suite positions into abbrev
  TitleTxt <- gsub( "CHIEF\\sEX[A-Z]*\\sO[A-Z]*\\b", "CEO", TitleTxt ) #executive
  TitleTxt <- gsub( "CHIEF\\sEX[A-Z]*$", "CEO", TitleTxt ) #executive
  
  TitleTxt <- gsub( "CHIEF\\sOP[A-Z]*\\sO[A-Z]*\\b", "COO", TitleTxt ) #operating
  TitleTxt <- gsub( "CHIEF\\sOP[A-Z]*\\b$", "COO", TitleTxt ) #operating
  # TitleTxt <- gsub( "CHIEF\\sOPERATING\\sOFFICER", "COO", TitleTxt ) #operating
  
  TitleTxt <- gsub( "CHIEF\\sFIN[A-Z]*\\sOFFICER", "CFO", TitleTxt ) #finance
  # TitleTxt <- gsub( "CHIEF\\sFINANCIAL\\sOFFICER", "CFO", TitleTxt ) #finance
  TitleTxt <- gsub( "CHIEF\\sFIN[A-Z]*$", "CFO", TitleTxt ) #finance
  #all other c-suites are unclear (for now)
  
  #change executive director and chief executive director to CEO
  TitleTxt <- gsub("EXECUTIVE DIRECTOR", "CEO", TitleTxt)
  TitleTxt <- gsub("EXEC DIR[A-Z]*\\b", "CEO", TitleTxt)
  TitleTxt <- gsub("\\bE\\s*D\\b", "CEO", TitleTxt)
  TitleTxt <- gsub("\\bCOCEO\\b", "CO-CEO", TitleTxt)
  
  #board member substitutions
  # TitleTxt <- gsub("CHAPTER DIRECTOR", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("^\\s*TRUSTEE\\s*$", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("^\\s*DIRECTOR\\s*$", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("GOVERNOR", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("COUNCILOR", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("COUNCIL MEMBER", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("REGENT", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("HONORARY DIRECTOR", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("HONORARY MEMBER", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("PARLIAMENTARIAN", "BOARD MEMBER", TitleTxt)
  # TitleTxt <- gsub("^\\s*EX-OFFICIO\\s*$", "BOARD MEMBER", TitleTxt)
  
  return(TitleTxt)
}


#' @title 
#' fix "of" function
#' 
#' @description 
#' inserts an "of" in between the title and subject if not present and needed
#' e.g. would insert "of" in "VP Operations" --> "VP of Operations"
#' 
#' currently occurs after title standardization 
#' (but can potentially occur before too)
#' 
#' @export
fix_of <- function(TitleTxt){
  
  # TitleTxt <- apply_substitutes(TitleTxt) #depending on order of op's
  
  ofMatch <- ifelse(!grepl("\\bOF\\b", TitleTxt), 
                    unlist(lapply(TitleTxt,of_title_helper)), NA)
  TitleTxt <- ifelse(!is.na(ofMatch), ofMatch, TitleTxt)
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt)
  return(TitleTxt)
}

#' @title 
#' standardize titles _something_ 'of' _something_ 
#' 
#' @description 
#' finds instances where of should be in the title and replaces as necessary
#' 
#' @export
of_title_helper <- function(x){
  titleMatch <- FALSE
  subjectMatch <- FALSE
  current.title <- NA
  titlePos <- 0
  TitleTxt <- x
  
  if(!grepl("\\bOF\\b", TitleTxt)){
    for(title in possible.titles){
      if(grepl(title,TitleTxt)){
        titleMatch <- TRUE
        current.title <- title
        titlePos <- regexpr(title,TitleTxt)[1]
        break
      }
    }
    for(subject in likely.subjects){
      if(grepl(subject,TitleTxt)){
        if(regexpr(subject,TitleTxt)[1] > titlePos){
          subjectMatch <- TRUE
          break
        }
      }
    }
  }
  if(titleMatch & subjectMatch) {
    TitleTxt <- sub(current.title, paste0(current.title, " OF "), TitleTxt)
    return(TitleTxt)
  }
  return(NA)
}


#' @title 
#' spell check function
#' 
#' @description 
#' correct small spelling mistakes
#' right now only correcting president, treasurer, and trustee misspellings
#' 
#' pretty slow function right now...
#' 
#' @export
spellcheck <- function(TitleTxt){
  
  if(!grepl("\\s",TitleTxt) & !is.na(TitleTxt) &
     (grepl("^\\s*P",TitleTxt) | grepl("^\\s*T",TitleTxt) | 
      grepl("^\\s*M",TitleTxt))){
    suggested.titles <- hunspell::hunspell_suggest(TitleTxt)[[1]]
    for(title in suggested.titles){
      if((title == "PRESIDENT" & grepl("^\\s*P",TitleTxt) & TitleTxt != "PRESIDENT"))
        TitleTxt <- "PRESIDENT"
      else if(title == "TREASURER" & grepl("^\\s*T",TitleTxt) & TitleTxt != "TREASURER")
        TitleTxt <- "TREASURER"
      else if(title == "TRUSTEE" & grepl("^\\s*T",TitleTxt) & TitleTxt != "TRUSTEE"
              & TitleTxt != "TRUSTEES")
        TitleTxt <- "TRUSTEE"
      else if(title == "MEMBER" & grepl("^\\s*M",TitleTxt) & TitleTxt != "MEMBER"
              & TitleTxt != "MEMBERSHIP")
        TitleTxt <- "MEMBER"
    }
  }
  return(TitleTxt)
}


#' @title 
#' remove trailing conjunctions
#' 
#' @description 
#' removes any trailing conjunctions (and, of, to) 
#' as well as any leading conjunctions
#' 
#' @export
remove_trailing_conjunctions <- function(TitleTxt){
  
  TitleTxt <- gsub("\\bAND$","",TitleTxt)
  TitleTxt <- gsub("\\bOF$","",TitleTxt)
  TitleTxt <- gsub("\\bTO$","",TitleTxt)
  
  TitleTxt <- gsub("^AND\\s+","",TitleTxt)
  
  return(TitleTxt)
}




