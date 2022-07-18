#apply-substitutions

require(hunspell)

#' @title 
#' substitute vice helper function
#' 
#' @description 
#' condenses vice president abbreviations to a standardized form
substitute_vice <- function(title.text){
  TitleTxt <- title.text
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
  
  return(TitleTxt)
}

#' @title 
#' substitute executive helper function
#' 
#' @description 
#' condenses executive abbreviations to a standardized form
substitute_executive <- function(title.text){
  TitleTxt <- title.text
  
  #smushed words
  TitleTxt <- gsub("\\bEXECDIR[A-Z]*\\b", "EXECUTIVE DIRECTOR", TitleTxt)
  TitleTxt <- gsub("\\bEXDIR[A-Z]*\\b", "EXECUTIVE DIRECTOR", TitleTxt)
  
  TitleTxt <- gsub("\\bEXE[A-Z]*\\b", "EXECUTIVE",TitleTxt)
  TitleTxt <- gsub("\\bEXC[A-Z]*\\b", "EXECUTIVE",TitleTxt)
  
  #ex officio
  if(!grepl("\\bEX O", TitleTxt) & !grepl("\\bEX-O", TitleTxt)) 
    gsub("\\bEX\\b","EXECUTIVE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute director helper functions
#' 
#' @description 
#' condenses director abbreviations to a standardized form
substitute_director <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bDIR[A-Z]*\\b", "DIRECTOR", TitleTxt)
  TitleTxt <- gsub("\\bDI\\b","DIRECTOR", TitleTxt)
  TitleTxt <- gsub("\\bDTR\\b","DIRECTOR", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute operations helper function
#' 
#' @description 
#' condenses operations abbreviations to a standardized form
#' function leaves "operating" alone --> does not convert to operations
substitute_operations <- function(title.text){
  TitleTxt <- title.text
  
  if(grepl("\\bOPERATIN",TitleTxt)){
    gsub("\\bOPERATIN\\b", "OPERATING", TitleTxt)
    return(TitleTxt)
  }
  
  TitleTxt <- gsub("\\bOP[A-Z]*\\b", "OPERATIONS", TitleTxt)
  return(TitleTxt)
}

#' @title 
#' substitute assistant helper function
#' 
#' @description 
#' condenses assistant abbreviations to a standardized form
#' also condenses associate
substitute_assistant <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bASSI[A-Z]*\\b", "ASSISTANT", TitleTxt)
  TitleTxt <- gsub( "\\bASS\\s*T\\b", "ASSISTANT", TitleTxt)
  TitleTxt <- gsub( "^\\s*A\\b", "ASSISTANT", TitleTxt)
  
  if(!grepl("\\bASSOCIATI[A-Z]*\\b", TitleTxt)){ #skip association
    TitleTxt <- gsub( "\\bASSOC[A-Z]*\\b", "ASSOCIATE", TitleTxt)
    TitleTxt <- gsub( "\\bASSC[A-Z]*\\b", "ASSOCIATE", TitleTxt)
  }
  return(TitleTxt)
  
}

#' @title 
#' substitute president helper function
#' 
#' @description 
#' condenses president abbreviations to a standardized form
#' ignores "presiding"
substitute_president <- function(title.text){
  TitleTxt <- title.text
  
  if(grepl("PRESIDING", TitleTxt)) return(TitleTxt)
  
  TitleTxt <- gsub("\\bPRESI[A-Z]*\\b", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("\\bPRES\\b", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("\\bPRE\\b", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("\\bP\\b", "PRESIDENT", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute secretary helper function
#' 
#' @description 
#' condenses secretary abbreviations to a standardized form
substitute_secretary <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bSECR[A-Z]*\\b", "SECRETARY",TitleTxt)
  
  TitleTxt <- gsub("\\bS\\b", "SECRETARY", TitleTxt) #assume standalone s is sec
  TitleTxt <- gsub("\\bSE\\b", "SECRETARY", TitleTxt) #ditto but with se
  TitleTxt <- gsub("\\bSEC\\b", "SECRETARY", TitleTxt) #ditto but with se
  TitleTxt <- gsub("\\bSECY\\b", "SECRETARY", TitleTxt) #ditto but with se
  
  TitleTxt <- gsub("\\bSECT[A-Z]*\\b", "SECRETARY", TitleTxt) 
  
  return(TitleTxt)
}

#' @title 
#' substitute treasurer helper function
#' 
#' @description 
#' condenses treasurer abbreviations to a standardized form
substitute_treasurer <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bTRE[A-Z]*\\b", "TREASURER", TitleTxt)
  TitleTxt <- gsub("\\bTR\\b", "TREASURER", TitleTxt)
  TitleTxt <- gsub("\\bTRSR\\b", "TREASURER", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute finance helper function
#' 
#' @description 
#' condenses finance abbreviations to a standardized form
#' financial is included
substitute_finance <- function(title.text){
  TitleTxt <- title.text
  
  #everything gets converted to finance (including financial)
  TitleTxt <- gsub("\\bFIN[A-Z]*\\b", "FINANCE", TitleTxt)
  TitleTxt <- gsub("\\bFI\\b", "FINANCE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute seniority helper function
#' 
#' @description 
#' condenses senior/junior abbreviations to a standardized form
substitute_senior <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bSENI[A-Z]*\\b", "SENIOR", TitleTxt)
  TitleTxt <- gsub("\\bSEN\\b", "SENIOR", TitleTxt)
  TitleTxt <- gsub("\\bSR\\b", "SENIOR", TitleTxt)
  
  TitleTxt <- gsub("\\bJUNI[A-Z]*\\b", "JUNIOR", TitleTxt)
  TitleTxt <- gsub( "\\bJR\\b", "JUNIOR", TitleTxt)
  TitleTxt <- gsub( "\\bJT\\b", "JUNIOR", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute development helper function
#' 
#' @description 
#' condenses development abbreviations to a standardized form
substitute_development <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bDEV[A-Z]*\\b", "DEVELOPMENT", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute chair helper function
#' 
#' @description 
#' condenses chair abbreviations to a standardized form
#' e.g. chairperson,chairman,chairwoman --> chair
#' includes vice chair standardizations
substitute_chair <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub( "\\bV\\s*C\\b", "VICE CHAIR", TitleTxt )
  TitleTxt <- gsub( "\\bV\\s\\bCHAIR\\b", "VICE CHAIR", TitleTxt )
  TitleTxt <- gsub( "\\bVICE\\b\\sC\\b", "VICE CHAIR", TitleTxt )
  
  TitleTxt <- gsub("\\bCHAI[A-Z]*\\b", "CHAIR", TitleTxt)

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
condense_abbreviations <- function(title.text){
  TitleTxt <- title.text
  
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
#' substitute officer helper function
#' 
#' @description 
#' condenses officer abbreviations to a standardized form
substitute_officer <- function(title.text){
  TitleTxt <- title.text
  
  if(grepl("OFFICE ", TitleTxt)) return(TitleTxt)
  if(grepl("\\bEX O", TitleTxt) | grepl("\\bEX-O", TitleTxt)) 
    return(TitleTxt)
  
  TitleTxt <- gsub("\\bOFF[A-Z]*\\b", "OFFICER", TitleTxt)
  TitleTxt <- gsub("\\bOFCR\\b", "OFFICER", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute admin helper function
#' 
#' @description 
#' condenses administration/administrator/administrating 
#' abbreviations to a standardized form
substitute_admin <- function(title.text){
  TitleTxt <- title.text
  
  #administrator
  if(grepl("\\bADMINISTRATO", TitleTxt)){
    TitleTxt <- gsub("\\bADMINISTRATO\\b", "ADMINISTRATOR", TitleTxt)
    return(TitleTxt)
  }
  
  #administrative
  if(grepl("\\bADMINISTRATIV", TitleTxt)){
    TitleTxt <- gsub("\\bADMINISTRATIV\\b", "ADMINISTRATIVE", TitleTxt)
    return(TitleTxt)
  }

  TitleTxt <- gsub("\\bADMIN[A-Z]*\\b", "ADMINISTRATION", TitleTxt)
  TitleTxt <- gsub("\\bADMI\\b", "ADMINISTRATION", TitleTxt)
  TitleTxt <- gsub("\\bADM\\b", "ADMINISTRATION", TitleTxt)
  TitleTxt <- gsub("\\bADMN\\b", "ADMINISTRATION", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute coordinator helper function
#' 
#' @description 
#' condenses coordinator abbreviations to a standardized form
#' note: instead of standardizing "CO" at the end of a title to coordinator,
#' it is instead mapped to "COMMITTEE"
substitute_coordinator <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bCOOR[A-Z]*\\b", "COORDINATOR",TitleTxt)
  
  TitleTxt <- gsub("\\bCO$", "COMMITTEE", TitleTxt) 
  #heuristic (could also be company, committee, coordinator, etc.)
  
  return(TitleTxt)
}

#' @title 
#' substitute strategy helper function
#' 
#' @description 
#' condenses strategy abbreviations to a standardized form
#' also condenses strategic to strategy
substitute_strategy <- function(title.text){
  TitleTxt <- title.text
  
  #strategic gets mapped to strategy as well
  TitleTxt <- gsub("\\bSTRAT[A-Z]*\\b", "STRATEGY", TitleTxt)
  TitleTxt <- gsub("\\bSTRGY\\b", "STRATEGY", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute human resources helper function
#' 
#' @description 
#' condenses hr abbreviations to a standardized form
substitute_hr <- function(title.text){
  TitleTxt <- title.text
  
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
#' substitute manage root helper function
#' 
#' @description 
#' condenses management/manager/managing
#'  abbreviations to a standardized form
substitute_manage <- function(title.text){
  TitleTxt <- title.text
  
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
#' substitute programs helper function
#' 
#' @description 
#' condenses programs abbreviations to a standardized form
#' skips over programming
substitute_programs <- function(title.text){
  TitleTxt <- title.text
  
  if(grepl("\\bPROGRAMMI", TitleTxt)){
    TitleTxt <- gsub("\\bPROGRAMMI[A-Z]*\\b", "PROGRAMMING", TitleTxt)
    return(TitleTxt)
  }
  
  #everything else = programs
  TitleTxt <- gsub("\\bPROG[A-Z]*\\b", "PROGRAMS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute projects helper function
#' 
#' @description 
#' condenses projects abbreviations to a standardized form
substitute_projects <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bPROJ[A-Z]*\\b", "PROJECTS", TitleTxt)

  return(TitleTxt)  
}

#' @title 
#' substitute public helper function
#' 
#' @description 
#' condenses public abbreviations to a standardized form
substitute_public <- function(title.text){
  TitleTxt <- title.text
  
  public.texts <- c("\\bPUBLI\\b", "\\bPUBL\\b", "\\bPUB\\b")
  for(name in public.texts){
    TitleTxt <- gsub(name,"PUBLIC",TitleTxt)
  }
  
  return(TitleTxt)
  
}

#' @title 
#' substitute business helper function
#' 
#' @description 
#' condenses business abbreviations to a standardized form
substitute_business <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bBUS[A-Z]*\\b", "BUSINESS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute comm helper functions
#' 
#' @description 
#' condenses communication and committee abbreviations to a standardized form
substitute_comm <- function(title.text){
  TitleTxt <- title.text
  
  #communication
  TitleTxt <- gsub("\\bCOMMU[A-Z]*\\b", "COMMUNICATIONS", TitleTxt)
  TitleTxt <- gsub("\\bCOMMS\\b", "COMMUNICATIONS", TitleTxt)
  
  #committee
  TitleTxt <- gsub("\\bCOMMIT[A-Z]*\\b", "COMMITTEE", TitleTxt)
  itleTxt <- gsub("\\bCOMMI\\b", "COMMITTEE", TitleTxt)
  TitleTxt <- gsub("\\bCOMM\\b", "COMMITTEE", TitleTxt)
  TitleTxt <- gsub("\\bCOM\\b", "COMMITTEE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute information helper function
#' 
#' @description 
#' condenses info abbreviations to a standardized form
substitute_information <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bINFO[A-Z]*\\b", "INFORMATION", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute intelligence helper function
#' 
#' @description 
#' condenses intel abbreviations to a standardized form
substitute_intelligence <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bINTEL[A-Z]*\\b","INTELLIGENCE",TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute technology helper function
#' 
#' @description 
#' condenses tech abbreviations to a standardized form
substitute_technology <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bTECH[A-Z]*\\b", "TECHNOLOGY", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute institute root helper function
#' 
#' @description 
#' condenses institute/institutional abbreviations to a standardized form
#' 
#' also includes instructor standardization
substitute_institute <- function(title.text){
  TitleTxt <- title.text
  
  if(grepl("\\bINSTITUTIONA", TitleTxt)){
    TitleTxt <- gsub("\\bINSTITUTIONA\\b", "INSTITUTIONAL", TitleTxt)
    return(TitleTxt)
  }
  TitleTxt <- gsub("\\bINSTI[A-Z]*\\b", "INSTITUTE", TitleTxt)
  TitleTxt <- gsub("\\bINST\\s", "INSTITUTIONAL ", TitleTxt)
  
  TitleTxt <- gsub("\\bINSTRUC[A-Z]*\\b","INSTRUCTOR", TitleTxt)
  TitleTxt <- gsub("\\bINST$", "INSTRUCTOR", TitleTxt)
  
  return(TitleTxt)
  
}

#' @title 
#' substitute academics helper function
#' 
#' @description 
#' condenses academics abbreviations to a standardized form
substitute_academics <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bACAD[A-Z]*\\b", "ACADEMICS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute marketing helper function
#' 
#' @description 
#' condenses marketing abbreviations to a standardized form
substitute_marketing <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bMARK[A-Z]*\\b", "MARKETING", TitleTxt)
  TitleTxt <- gsub("\\bMKTG\\b", "MARKETING", TitleTxt)
  TitleTxt <- gsub("\\bMKT\\b", "MARKETING", TitleTxt)
  TitleTxt <- gsub("\\bMRKTNG\\b", "MARKETING", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute advancement helper function
#' 
#' @description 
#' condenses advancement abbreviations to a standardized form
substitute_advancement <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bADVA[A-Z]*\\b", "ADVANCEMENT", TitleTxt)
  
  # TitleTxt <- gsub("\\bADV\\b", "ADVISOR", TitleTxt) #could be advisor
  
  return(TitleTxt)
}

#' @title 
#' substitute philanthropy helper function
#' 
#' @description 
#' condenses philanthropy abbreviations to a standardized form
substitute_philanthropy <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bPHILAN[A-Z]*\\b", "PHILANTHROPY", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute systems helper function
#' 
#' @description 
#' condenses systems abbreviations to a standardized form
substitute_systems <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bSYS[A-Z]*\\b", "SYSTEMS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute general helper function
#' 
#' @description 
#' condenses general abbreviations to a standardized form
substitute_general <- function(title.text){
  TitleTxt <- title.text
  
  if(grepl("GENEALOG",TitleTxt)) return(TitleTxt) #skipping genealogist
  TitleTxt <- gsub("\\bGEN[A-Z]*\\b", "GENERAL", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute planning helper function
#' 
#' @description 
#' condenses planning abbreviations to a standardized form
#' includes planned
substitute_planning <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bPLANN[A-Z]*\\b", "PLANNING", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute compliance helper function
#' 
#' @description 
#' condenses compliance abbreviations to a standardized form
substitute_compliance <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bCOMPL[A-Z]*\\b", "COMPLIANCE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute enrollment helper function
#' 
#' @description
#' condenses enrollment abbreviations to a standardized form 
substitute_enrollment <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bENRO[A-Z]*\\b", "ENROLLMENT", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute admissions helper function
#' 
#' @description 
#' condenses admissions abbreviations to a standardized form
substitute_admissions <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bADMIS[A-Z]*\\b", "ADMISSIONS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute deputy helper function
#' 
#' @description 
#' condenses deputy abbreviations to a standardized form
substitute_deputy <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bDEP[A-Z]*\\b", "DEPUTY", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute corresponding helper function
#' 
#' @description 
#' condenses corresponding abbreviations to a standardized form
substitute_corresponding <- function(title.text){
  TitleTxt <- title.text
  
  if(grepl("CORRESPONDENT", TitleTxt)) return(TitleTxt)
  TitleTxt <- gsub("\\bCORR[A-Z]*\\b", "CORRESPONDING", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute emeritus helper function
#' 
#' @description 
#' condenses emeritus abbreviations to a standardized form
substitute_emeritus <- function(title.text){
  TitleTxt <- title.text
  
  if(grepl("EMPLOYEE",TitleTxt)) return(TitleTxt)
  TitleTxt <- gsub("\\bEM[A-Z]*\\b", "EMERITUS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute relations helper function
#' 
#' @description 
#' condenses relations abbreviations to a standardized form
substitute_relations <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bRELA[A-Z]*\\b", "RELATIONS", TitleTxt)
  TitleTxt <- gsub("\\bREL\\b", "RELATIONS", TitleTxt)
  
  TitleTxt <- gsub("\\bPR\\s", "PUBLIC RELATIONS ", TitleTxt)
  TitleTxt <- gsub("\\bPUBLIC REALTIONS\\b", "PUBLIC RELATIONS", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute representative helper function
#' 
#' @description 
#' condenses rep abbreviations to a standardized form
substitute_representative <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bREP[A-Z]*\\b", "REPRESENTATIVE", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute board helper function
#' 
#' @description 
#' condenses board abbreviations to a standardized form
substitute_board <- function(title.text){
  TitleTxt <- title.text
  
  board.texts <- c("\\bBOAR\\b", "\\bBOA\\b", "\\bBO\\b",
                   "\\bBRD\\b","\\bBOD\\b","\\bBD\\b")
  for(title in board.texts){
    TitleTxt <- gsub(title,"BOARD",TitleTxt)
  }
  
  return(TitleTxt)
}

#' @title 
#' substitute member helper function
#' 
#' @description 
#' condenses member abbreviations to a standardized form
substitute_member <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bMBR\\b", "MEMBER", TitleTxt)
  TitleTxt <- gsub("\\bMBER\\b", "MEMBER", TitleTxt)
  TitleTxt <- gsub("\\bMMBR\\b", "MEMBER", TitleTxt)
  TitleTxt <- gsub("\\bMEM\\b", "MEMBER",TitleTxt)
  TitleTxt <- gsub("\\bME\\b", "MEMBER",TitleTxt)
  
  
  return(TitleTxt)
}

#' @title 
#' substitute transportation helper function
#' 
#' @description
#' condenses transportation abbreviations to a standardized form 
substitute_transportation <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bTRANS[A-Z]*\\b", "TRANSPORTATION", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute ex officio helper function
#' 
#' @description 
#' condenses ex officio abbreviations to a standardized form
substitute_exofficio <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bEX OFF[A-Z]*\\b", "EX-OFFICIO", TitleTxt)
  
  return(TitleTxt)
}

#' @title 
#' substitute at large helper function
#' 
#' @description 
#' condenses at large abbreviations to a standardized form
substitute_atlarge <- function(title.text){
  TitleTxt <- title.text
  
  TitleTxt <- gsub("\\bAT LA[A-Z]*\\b", "AT LARGE", TitleTxt)
  TitleTxt <- gsub("\\bAT\\s*$", "AT LARGE", TitleTxt)
  
  return(TitleTxt)
}


#' @title 
#' substitute miscellaneous helper function
#' 
#' @description 
#' substituting all the one's we missed/haven't generalized
substitute_miscellaneous <- function(title.text){
  TitleTxt <- title.text
  
  #miscellaneous:
  TitleTxt <- gsub("\\bMINISTR\\b","MINISTER", TitleTxt)
  TitleTxt <- gsub("\\bSGT\\b","SERGEANT", TitleTxt)
  TitleTxt <- gsub("\\bFO\\b","FOUNDER", TitleTxt)
  TitleTxt <- gsub("\\bGM\\b", "GENERAL MANAGER", TitleTxt)
  TitleTxt <- gsub("\\bD AND T\\b", "DEVELOPMENT AND TECHNOLOGY", TitleTxt)
  TitleTxt <- gsub("\\bD\\b", "DIRECTOR", TitleTxt) #assume standalone d is director
  TitleTxt <- gsub("\\bDD\\b", "DEPUTY DIRECTOR", TitleTxt) #assume dd = dep director
  TitleTxt <- gsub("\\bCO\\s", "CO-", TitleTxt) #co-chair over co chair for example
  TitleTxt <- gsub("\\bCL\\b", "CLERK", TitleTxt)
  TitleTxt <- gsub("\\bER\\b", "EDITOR", TitleTxt)
  TitleTxt <- gsub("\\bEDR\\b", "EDITOR", TitleTxt)
  TitleTxt <- gsub("\\bCUL\\b", "CULTURE", TitleTxt)
  TitleTxt <- gsub("\\bSERV\\b", "SERVICE", TitleTxt)
  TitleTxt <- gsub("\\bGOVERN\\b", "GOVERNANCE", TitleTxt)
  TitleTxt <- gsub("\\bCHF\\b", "CHIEF", TitleTxt)
  TitleTxt <- gsub("\\bREC\\b", "RECORDING", TitleTxt)
  TitleTxt <- gsub("\\bORG\\s", "ORGANIZING ", TitleTxt)
  TitleTxt <- gsub("\\bCOU\\b", "COUNCIL", TitleTxt)
  TitleTxt <- gsub("\\bLIASON\\b", "LIAISON",TitleTxt)
  TitleTxt <- gsub("\\bEDUCA\\b", "EDUCATION", TitleTxt)
  TitleTxt <- gsub("\\bADV\\s*$", "ADVISOR", TitleTxt)
  TitleTxt <- gsub("\\bADV\\b", "ADVANCEMENT", TitleTxt)
  
  TitleTxt <- gsub("\\bAFFA[A-Z]*\\b", "AFFAIRS", TitleTxt)
  TitleTxt <- gsub("\\bCONSU[A-Z]*\\b","CONSULTANT", TitleTxt)
  
  TitleTxt <- gsub("\\bART[A-Z]*\\s", "ARTISTIC ", TitleTxt)
  TitleTxt <- gsub("\\bART[A-Z]*\\b$", "ARTIST", TitleTxt)
  
  TitleTxt <- gsub("\\bCHOREO[A-Z]*\\b","CHOREOGRAPHER",TitleTxt)
  TitleTxt <- gsub("\\bCOREO[A-Z]*\\b","CHOREOGRAPHER",TitleTxt)
  
  TitleTxt <- gsub("\\bPHY[A-Z]*\\b", "PHYSICAL", TitleTxt)
  TitleTxt <- gsub("\\bTHEATR[A-Z]*\\b","THEATER",TitleTxt)
  
  TitleTxt <- gsub("\\bVICE$", "VICE PRESIDENT",TitleTxt)
  
  #TRUSTEE
  TitleTxt <- gsub("\\bTRTEE\\b", "TRUSTEE", TitleTxt)
  TitleTxt <- gsub("\\bTRU\\b", "TRUSTEE", TitleTxt)
  TitleTxt <- gsub("\\bTSTEE\\b", "TRUSTEE", TitleTxt)
  TitleTxt <- gsub("\\bTTEE\\b", "TRUSTEE", TitleTxt)

  
  #spelling corrections
  TitleTxt <- gsub("\\bCONTRUCTION\\b", "CONSTRUCTION", TitleTxt)
  TitleTxt <- gsub("\\bFORNER\\b", "FORMER", TitleTxt)
  
  #duplicate removal
  TitleTxt <- gsub("PRESIDENT PRESIDENT", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("PRESIDENTPR.*\\b", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("CEOCEO", "CEO", TitleTxt)
  
  #heuristics (won't be much use until we get rid of role statuses)
  if(TitleTxt == "VICE") TitleTxt <- "VICE PRESIDENT" #standalone vice is likely vp
  else if(TitleTxt == "PR") TitleTxt <- "PRESIDENT" #standalone pr is likely pres
  else if(TitleTxt == "EX") TitleTxt <- "EXECUTIVE" #standalone ex is likely exec
  else if(grepl("\\bT\\b", TitleTxt)) TitleTxt <- gsub("\\bT\\b","TRUSTEE",TitleTxt)
  #standalone t is likely trustee,#but could be treasurer
  else if(grepl("\\bEXECUTIVE V\\b", TitleTxt)) 
    TitleTxt <- gsub("\\bEXECUTIVE V\\b","EXECUTIVE VICE PRESIDENT", TitleTxt)
  else if(grepl("\\bCE$",TitleTxt)) TitleTxt <- gsub("\\bCE$","CEO",TitleTxt) #ceo misspelling
  else if (grepl("\\bCF$",TitleTxt)) TitleTxt <- gsub("\\bCF$","CFO",TitleTxt)
  else if(TitleTxt == "EXECUTIVE OFFICER") TitleTxt <- "CEO"
  else if(TitleTxt == "N/A" | TitleTxt == "\\bN\\s*A\\b" | TitleTxt == "\\s*")
    TitleTxt <- NA
  # NA could be north america but most likely null value
  
  return(TitleTxt)
}



#' @title 
#' apply substitutes wrapper function
#'
#' @description 
#' apply custom dictionary of common abbreviations and misspellings 
#' to standardize titles
#' @export
#' 
apply_substitutes <- function (title.text){
  TitleTxt <- title.text
  
  TitleTxt <- substitute_vice(TitleTxt)
  TitleTxt <- substitute_executive(TitleTxt)
  TitleTxt <- substitute_director(TitleTxt)
  TitleTxt <- substitute_operations(TitleTxt) #operations/operating
  TitleTxt <- substitute_assistant(TitleTxt) #assistant/associate
  TitleTxt <- substitute_president(TitleTxt)
  TitleTxt <- substitute_secretary(TitleTxt)
  TitleTxt <- substitute_treasurer(TitleTxt)
  TitleTxt <- substitute_finance(TitleTxt) #finance/financial
  TitleTxt <- substitute_senior(TitleTxt) #senior/junior
  TitleTxt <- substitute_development(TitleTxt)
  TitleTxt <- substitute_chair(TitleTxt)
  TitleTxt <- substitute_officer(TitleTxt)
  TitleTxt <- substitute_admin(TitleTxt)
  TitleTxt <- substitute_coordinator(TitleTxt)
  TitleTxt <- substitute_strategy(TitleTxt) #strategy/strategic
  TitleTxt <- substitute_hr(TitleTxt)
  TitleTxt <- substitute_manage(TitleTxt) #management/managing/manager
  TitleTxt <- substitute_programs(TitleTxt) #programs/programming
  TitleTxt <- substitute_projects(TitleTxt)
  TitleTxt <- substitute_public(TitleTxt)
  TitleTxt <- substitute_business(TitleTxt)
  TitleTxt <- substitute_comm(TitleTxt) #communication/committee
  TitleTxt <- substitute_information(TitleTxt)
  TitleTxt <- substitute_intelligence(TitleTxt)
  TitleTxt <- substitute_technology(TitleTxt)
  TitleTxt <- substitute_institute(TitleTxt) #institute/institutional
  TitleTxt <- substitute_academics(TitleTxt) #academics/academy
  TitleTxt <- substitute_marketing(TitleTxt)
  TitleTxt <- substitute_advancement(TitleTxt)
  TitleTxt <- substitute_philanthropy(TitleTxt)
  TitleTxt <- substitute_systems(TitleTxt)
  TitleTxt <- substitute_general(TitleTxt)
  TitleTxt <- substitute_planning(TitleTxt) #planning/planned
  TitleTxt <- substitute_compliance(TitleTxt)
  TitleTxt <- substitute_enrollment(TitleTxt)
  TitleTxt <- substitute_admissions(TitleTxt)
  TitleTxt <- substitute_deputy(TitleTxt)
  TitleTxt <- substitute_corresponding(TitleTxt) #correspondent/corresponding
  TitleTxt <- substitute_emeritus(TitleTxt)
  TitleTxt <- substitute_relations(TitleTxt)
  TitleTxt <- substitute_representative(TitleTxt)
  TitleTxt <- substitute_board(TitleTxt)
  TitleTxt <- substitute_transportation(TitleTxt)
  TitleTxt <- substitute_exofficio(TitleTxt)
  TitleTxt <- substitute_atlarge(TitleTxt)
  TitleTxt <- substitute_member(TitleTxt)
  
  TitleTxt <- condense_abbreviations(TitleTxt) 
  
  TitleTxt <- substitute_miscellaneous(TitleTxt)
  
  #remove residual spacing issues
  TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  
  TitleTxt <- stand_titles(TitleTxt)
  
  return( TitleTxt )
}


#' @title 
#' standardize title mapping function
#' 
#' @description 
#' maps long names to abbreviations if CEO, COO, or CFO
#' maps executive director to CEO
#' 
#' @export
stand_titles <- function(title.text){
  TitleTxt <- title.text
  #convert known c-suite positions into abbrev
  TitleTxt <- gsub( "CHIEF\\sEXECUTIVE\\sOFFICER", "CEO", TitleTxt ) #executive
  TitleTxt <- gsub( "CHIEF\\sOPERATIONS\\sOFFICER", "COO", TitleTxt ) #operating
  TitleTxt <- gsub( "CHIEF\\sOPERATING\\sOFFICER", "COO", TitleTxt ) #operating
  TitleTxt <- gsub( "CHIEF\\sFINANCE\\sOFFICER", "CFO", TitleTxt ) #finance
  TitleTxt <- gsub( "CHIEF\\sFINANCIAL\\sOFFICER", "CFO", TitleTxt ) #finance
  #all other c-suites are unclear (for now)
  
  #change executive director and chief executive director to CEO
  TitleTxt <- gsub("EXECUTIVE DIRECTOR", "CEO", TitleTxt)
  
  return(TitleTxt)
}



#' @title 
#' spell check function
#' 
#' @description 
#' correct small spelling mistakes
#' right now only correcting president, treasurer, and trustee misspellings
spellcheck <- function(title.text){
  TitleTxt <- title.text
  
  if(!grepl("\\s",TitleTxt) && !is.na(TitleTxt)){
    suggested.titles <- hunspell::hunspell_suggest(TitleTxt)[[1]]
    for(title in suggested.titles){
      if((title == "PRESIDENT" && grepl("^\\s*P",TitleTxt) && TitleTxt != "PRESIDENT"))
        TitleTxt <- "PRESIDENT"
      else if(title == "TREASURER" && grepl("^\\s*T",TitleTxt) && TitleTxt != "TREASURER")
        TitleTxt <- "TREASURER"
      else if(title == "TRUSTEE" && grepl("^\\s*T",TitleTxt) && TitleTxt != "TRUSTEE"
         && TitleTxt != "TRUSTEES")
        TitleTxt <- "TRUSTEE"
    }
  }
  return(TitleTxt)
}






#' @title 
#' generate status codes wrapper function
#' NOT YET WRITTEN
#' 
#' 
#' @description 
#' phase 4
#' 
#' o	remove_sched_o()  # add binary code for “see schedule o” comments and remove text 
#' standardize_qualifiers()  # use mapping of variants of qualifiers like past, former, ex-officio, emeritus all mapped to canonical version “PAST”
#' future (“elect”) 
#' current 
#' interim (acting, through, as of, etc.)
#' past (emeritus,
#' co
#' at large
#' ex-officio
#' 
#' categorize_qualifiers()  # create binary codes for above categories 
#' standardize_quantifiers()  # replace 1st with first, etc., only if at the start of the title 
#' categorize_quantifiers () # create binary codes for above categories
#' should we leave qualifiers or remove them from clean title text??? 
#' I think removing is better, but would need to see examples 
#' 
#' @export
gen_status_codes <- function(title.text){
  
}





