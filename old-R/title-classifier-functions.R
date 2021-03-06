#general
#all deprecated!


###########
### DATA CLEANING
#' @title
#' Split compound titles
#' DEPRECATED
#'
#' @description
#' `split_compound_title` splits compound titles into separate strings if evident.
#' For example, it would split "Treasurer/Secretary" into a vector of
#' strings containing ("Treasurer", "Secretary"). The production rules for title
#' delineation are "/", ";", "&", "AND", and "," in certain cases ("," is the
#' weakest delineator, so it is only used if there is a guarantee that the second
#' title is standardized). The input is a string, and the output is a vector
#' with variable length.
#'
split_compound_title <- function(title.text){
  title.text <- toupper(title.text)
  if(is.na(title.text)) return("")
  if(nchar(title.text) == 0) return(title.text)
  
  #delineators: AND, &, /, ;, ","
  if(comma_split(title.text))
    title.list <- unlist(strsplit(title.text," AND |&|/|;|,"))
  else
    title.list <- unlist(strsplit(title.text," AND |&|/|;"))
  
  return(title.list)
}

#' @title
#' comma split function
#' DEPRECATED
#'
#' @description
#' `comma_split` checks if using a comma as a title delineator works (if appl.).
#' It works by creating a vector of the comma split titles, and checking that
#' the standardization of the second title is valid. It does this to ensure
#' cases like "VP, Sales and Marketing" aren't broken up into "VP" and "Sales
#' and Marketing", but cases like "CFO, Treasurer" are split into "CFO" and
#' "Treasurer". The standardized list of titles is pretty general, and could be
#' changed/improved as more edge cases are discovered.
#'
comma_split <- function(title.text){
  #only using comma splits if the other title is guaranteed
  TitleTxt <- toupper(title.text)
  
  #if there are no commas, there's no need to continue
  if(!grepl(",",TitleTxt)) return(FALSE)
  
  #split title using "," delineator, then standardize second title
  title.list <- unlist(strsplit(title.text,","))
  st2 <- standardize_titles(apply_cleaning(split_compound_title(title.list[2])[1]))
  if(nchar(st2) == 0) return(FALSE)
  
  #list of possible standardized second title strings
  possibles <- c("PRESIDENT", "CEO", "DIRECTOR\\b", "CHAIR",
                 "TRUSTEE\\b", "TREASURER", "SECRETARY", "OFFICER",
                 "COUNSEL","FOUNDER", "PUBLISHER","EDITOR", "MEMBER\\b",
                 "\\bCOO\\b", "CFO","MANAGEMENT", "CTO", "OFFICE ADMINISTRATION",
                 "CRO")
  for(title in possibles){
    if(grepl(title,st2))
      return(TRUE)
  }
  return(FALSE)
}

######
#' @title
#' apply cleaning function
#' DEPRECATED (need to extract the gen_status_codes parts)
#'
#' @description
#' `apply_cleaning` applies cleaning steps (capitalization, punctuation and number
#' removal, leading spaces, etc.) on a title text. For example, it would convert
#'  an input of "  pResid." into "PRESID". For future work, the goal is to extract
#'  dates and role statuses during this cleaning step in order to improve
#'  efficiency, but currently that is a separate step. The function takes in an
#'  input of a string and outputs a string.
#'
apply_cleaning <- function(title.text){
  
  #capitalize all text
  TitleTxt <- toupper(title.text)
  
  #remove anything in between parentheses - first paren is indicator
  TitleTxt <- gsub("\\(.*\\)", "", TitleTxt)
  TitleTxt <- gsub("\\(.*$", "", TitleTxt)
  
  #substitute ordinal numbers
  TitleTxt <- gsub("1ST","FIRST",TitleTxt)
  TitleTxt <- gsub("2ND","SECOND",TitleTxt)
  TitleTxt <- gsub("3RD","THIRD",TitleTxt)
  TitleTxt <- gsub("4TH","FOURTH",TitleTxt)
  TitleTxt <- gsub("5TH","FIFTH",TitleTxt)
  TitleTxt <- gsub("6TH","SIXTH",TitleTxt)
  TitleTxt <- gsub("7TH","SEVENTH",TitleTxt)
  TitleTxt <- gsub("8TH","EIGHTH",TitleTxt)
  TitleTxt <- gsub("9TH","NINTH",TitleTxt)
  TitleTxt <- gsub("10TH","TENTH",TitleTxt)
  
  #remove punctuation, numbers, and "stop" words with dates (replaced with spaces)
  TitleTxt <- gsub("\\d[A-Z]*\\s", " ", TitleTxt)
  TitleTxt <- gsub("\\d", " ", TitleTxt)
  TitleTxt <- gsub("[[:punct:]]", " ", TitleTxt) #very brutal punct removal
  
  transition.words <- c("THRU","THROUGH","THR\\b","\\bFROM\\b",
                        "UNTIL","\\bTIL\\b","SINCE", "\\bAS OF\\b",
                        "BEGINNING","\\bBEG\\b","\\bLEFT\\b","LEAVING",
                        "\\bENDED\\b","\\bENDING\\b","\\bEND\\b",
                        "RETIRED","RETIRING","\\bTERM\\b",
                        "\\bRESIGN\\b", "\\bRESIGNED\\b",
                        "\\bPARTIAL\\sYEAR\\b", "\\bPARTIAL\\sYR\\b",
                        "\\bPAST\\b","CURRENT","FORMER", "INTERIM",
                        "EFFECTIVE","CEASED","\\bACTING", "\\bELECT",
                        "\\bAS NEEDED\\b","\\bSEE SCHEDULE O\\b", 
                        "SEE SCHED O\\b", "\\bSEE SCH O\\b",
                        "\\bPRIOR\\b","\\bINCOMING\\b","\\bAFTER\\b",
                        "\\bINCOMING\\b", "\\bPARTIAL\\b", "\\bAT LARGE\\b")
  for(word in transition.words){
    TitleTxt <- gsub(word, " ", TitleTxt)
  }
  
  # CONJUNCTION WORDS
  conjunction.words <- c("\\bTO\\b","\\bOR\\b", "\\bAN\\b","\\bAS\\b")
  #"AN" could be and misspelled
  for(word in conjunction.words){
    TitleTxt <- gsub(word, " ", TitleTxt)
  }
  
  #months
  month.words <- c("JANUARY","\\bJAN\\b","FEBRUARY","\\bFEB\\b",
                   "MARCH","\\bMAR\\b","APRIL","APR","\\bMAY\\b",
                   "JUNE","\\bJUN\\b","JULY","\\bJUL\\b","AUGUST\\b",
                   "\\bAUG\\b","SEPTEMBER","\\bSEP\\b","\\bSEPT\\b","OCTOBER",
                   "\\bOCT\\b","NOVEMBER","\\bNOV\\b","DECEMBER","\\bDEC\\b"
  )
  for(word in month.words){
    TitleTxt <- gsub(word, " ", TitleTxt)
  }
  
  #number words
  number.words <- c("FIRST","SECOND","THIRD","FOURTH","FIFTH",
                    "SIXTH","SEVENTH","EIGHTH","NINTH","TENTH",
                    "\\bONE\\b","\\bTWO\\b","THREE","FOUR",
                    "FIVE","\\bSIX\\b","SEVEN","EIGHT","NINE","\\bTEN\\b")
  for(word in number.words){
    TitleTxt <- gsub(word, " ", TitleTxt)
  }
  
  #remove starting and leading spaces and excess spacing
  TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  
  #miscellaneous fixes
  #replace common C0 (c zero) with CO (c o)
  TitleTxt <- gsub( "C0", "CO", TitleTxt )
  
  return(TitleTxt)
}

#########

#' @title
#' standardize titles function
#' DEPRECATED
#'
#' @description
#' `standardize_titles` provides a hard-coded mapping from raw titles to their
#' standardized form. For example, "PRES" and "PRESIDE" will both be mapped to
#' "PRESIDENT", and "CEO" and "EX DIR" will both be mapped to "CEO". This function
#' takes in a string and outputs a string. As a note, for future work, it's
#' potentially possible to use agrep to allow for spelling mistakes in long titles,
#' and/or also narrow down the title range depending on sector to improve efficiency.
#'
standardize_titles <- function( title.text ){
  
  TitleTxt <- toupper(title.text) #in case it fell thru
  
  #Some notes:
  #don't want to double replace
  #gotta go more specific than general
  #using regex \\s and \\b to allow for more flexibility and cleaner code
  
  ## ------------------------------------------------------------------------
  
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
  
  TitleTxt <- gsub( "V\\s*P\\s*F\\b", "VICE PRESIDENT FINANCE", TitleTxt )
  TitleTxt <- gsub( "V\\s*P\\s*O\\b", "VICE PRESIDENT OPERATIONS", TitleTxt )
  TitleTxt <- gsub( "V\\s*P\\s*H\\s*R\\b", "VICE PRESIDENT HUMAN RESOURCES", TitleTxt )
  TitleTxt <- gsub( "V\\s*P\\s*M\\s*A\\b", "VICE PRESIDENT MEDICAL AFFAIRS", TitleTxt )
  
  # TitleTxt <- gsub( "\\bVC$", "VICE PRESIDENT", TitleTxt ) #WRONG: vice chair
  TitleTxt <- gsub( "V\\s*P\\b", "VICE PRESIDENT", TitleTxt )
  
  vice.president.texts <- c("V\\s*PRESIDENT\\b", "V\\s*PRESIDEN\\b",
                            "V\\s*PRESIDE\\b", "V\\s*PRESID\\b",
                            "V\\s*PRESI\\b", "V\\s*PRES\\b",
                            "\\bVICE\\s\\bPRESIDEN\\b","\\bVICE\\s\\bPRESIDE\\b",
                            "\\bVICE\\s\\bPRESID\\b","\\bVICE\\s\\bPRESI\\b",
                            "\\bVICE\\s\\bPRES\\b","\\bVICE\\s\\bP\\b")
  for(name in vice.president.texts){
    TitleTxt <- gsub(name, "VICE PRESIDENT", TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #EXECUTIVE
  executive.texts <- c("\\bEXECUTIV\\b","\\bEXECUTI\\b",
                       "\\bEXECUT\\b","\\bEXECU\\b",
                       "\\bEXEC\\b","\\bEXE\\b",
                       "\\bEXC\\b","\\bEX\\b")
  for(name in executive.texts){
    TitleTxt <- gsub( name, "EXECUTIVE", TitleTxt )
  }
  
  ## ------------------------------------------------------------------------
  #Director
  director.texts <- c("\\bDIRCTR\\b", "\\bDTR\\b",
                      "\\bDIRECTO\\b", "\\bDIRECT\\b",
                      "\\bDIREC\\b","\\bDIRE\\b",
                      "\\bDIR\\b","\\bDI\\b")
  for(name in director.texts){
    TitleTxt <- gsub( name, "DIRECTOR", TitleTxt )
  }
  
  ## ------------------------------------------------------------------------
  #Operations
  operation.texts <- c("\\bOPERATION\\b","\\bOPERATIO\\b",
                       "\\bOPERATI\\b","\\bOPERAT\\b",
                       "\\bOPERA\\b","\\bOPER\\b",
                       "\\bOPE\\b","\\bOPS\\b","\\bOP\\b",
                       "\\bOPERATING\\b", "\\bOPERATIN\\b")
  for(name in operation.texts){
    TitleTxt <- gsub( name, "OPERATIONS", TitleTxt )
  }
  
  ## ------------------------------------------------------------------------
  #ASSISTANT/ASSOCIATE
  TitleTxt <- gsub( "\\bASS\\s*T\\b", "ASSISTANT", TitleTxt )
  TitleTxt <- gsub("\\bASSIST\\b", "ASSISTANT", TitleTxt)
  TitleTxt <- gsub( "\\bASSOC\\b", "ASSOCIATE", TitleTxt )
  
  ## ------------------------------------------------------------------------
  #PRESIDENT
  president.texts <- c("\\bPRESIDEN\\b", "\\bPRESIDE\\b",
                       "\\bPRESID\\b", "\\bPRESI\\b", "\\bPRE\\b",
                       "\\bPRES\\b","\\bP\\b") #no pr or press
  #pr could be public relations
  for(name in president.texts){
    TitleTxt <- gsub( name, "PRESIDENT", TitleTxt )
  }
  
  ## ------------------------------------------------------------------------
  ###SECRETARY
  secretary.texts <- c("\\bSECRETAR\\b", "\\bSECT\\b",
                       "\\bSECRETA\\b","\\bSECRET\\b",
                       "\\bSECRE\\b","\\bSECR\\b",
                       "\\bSECY\\b","\\bSEC\\b", "\\bSECTY\\b")
  for(name in secretary.texts){
    TitleTxt <- gsub( name, "SECRETARY", TitleTxt )
  }
  # if(agrepl("SECRETARY",TitleTxt)) TitleTxt <- "SECRETARY"
  
  ## ------------------------------------------------------------------------
  #TREASURER
  treasurer.texts <- c("TREASURE\\b","TREASUR\\b",
                       "TREASU\\b","TREAS\\b",
                       "TREA\\b","TRE\\b","TR\\b",
                       "TRASURER\\b","\\bTRSR\\b")
  for(name in treasurer.texts){
    TitleTxt <- gsub( name, "TREASURER", TitleTxt )
  }
  
  ## ------------------------------------------------------------------------
  #FINANCE
  finance.texts <- c("\\bFINANC\\b","\\bFINAN\\b",
                     "\\bFINA\\b","\\bFIN\\b",
                     "\\bFINANCIAL\\b","\\bFINANCIA\\b",
                     "\\bFINANCI\\b")
  for(name in finance.texts){
    TitleTxt <- gsub( name, "FINANCE", TitleTxt )
  }
  
  ## ------------------------------------------------------------------------
  #SENIOR
  senior.texts <- c("\\bSENIO\\b","\\bSENI\\b",
                    "\\bSEN\\b","\\bSR\\b")
  for(name in senior.texts){
    TitleTxt <- gsub( name, "SENIOR", TitleTxt )
  }
  
  ## ------------------------------------------------------------------------
  #JUNIOR
  TitleTxt <- gsub( "JUNIO", "JUNIOR", TitleTxt )
  TitleTxt <- gsub( "JR", "JUNIOR", TitleTxt )
  
  ## ------------------------------------------------------------------------
  #Development
  development.texts <- c("\\bDEVELOPMEN\\b","\\bDEVELOPME\\b",
                         "\\bDEVELOPM\\b","\\bDEVELOP\\b",
                         "\\bDEVELO\\b","\\bDEVEL\\b",
                         "\\bDEVE\\b","\\bDEV\\b")
  for(name in development.texts){
    TitleTxt <- gsub( name, "DEVELOPMENT", TitleTxt )
  }
  
  ## ------------------------------------------------------------------------
  #VICE CHAIR
  TitleTxt <- gsub( "V\\s*C\\b", "VICE CHAIR", TitleTxt )
  TitleTxt <- gsub( "V\\s\\bCHAIR\\b", "VICE CHAIR", TitleTxt )
  TitleTxt <- gsub( "\\bVICE\\b\\sC\\b", "VICE CHAIR", TitleTxt )
  
  #CHAIR
  TitleTxt <- gsub( "CHAIRPERSON", "CHAIR", TitleTxt )
  TitleTxt <- gsub( "CHAIRWOMAN", "CHAIR", TitleTxt )
  TitleTxt <- gsub( "CHAIRMAN", "CHAIR", TitleTxt )
  TitleTxt <- gsub("\\bCHAI\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bCHA\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bCHR\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bCHAR\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bCH\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bC\\b", "CHAIR", TitleTxt) #assume standalone c is chair
  TitleTxt <- gsub("\\bS\\b", "SECRETARY", TitleTxt) #assume standalone s is sec
  TitleTxt <- gsub("\\bSE\\b", "SECRETARY", TitleTxt) #ditto but with se
  TitleTxt <- gsub("\\bCHAIRM\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bCHAIRMA\\b", "CHAIR", TitleTxt)
  
  ## ------------------------------------------------------------------------
  #Condense Abbreviations
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
  
  #EXECUTIVE DIRECTOR
  TitleTxt <- gsub( "\\bE\\b DIRECTOR", "EXECUTIVE DIRECTOR", TitleTxt )
  TitleTxt <- gsub( "\\bE\\s*D\\b", "EXECUTIVE DIRECTOR", TitleTxt )
  
  ## ------------------------------------------------------------------------
  #KEY EMPLOYEE
  TitleTxt <- gsub( "KEY\\s\\bE\\b", "KEY EMPLOYEE\\b", TitleTxt )
  
  # FORMER OFFICER
  #Note: weird category
  #thought we were deleting old formers?
  # TitleTxt <- gsub( "FMR", "FORMER", TitleTxt )
  # TitleTxt <- gsub( "FORNER", "FORMER", TitleTxt )
  officer.texts <- c("\\bOFFICO\\b", #"\\bOFFICE\\b", #office is its separate thing
                     "\\bOFFIC\\b","\\bOFFI\\b",
                     "\\bOFF\\b","\\bOFCR\\b")
  for(name in officer.texts){
    TitleTxt <- gsub(name,"OFFICER",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #ADMIN
  administration.texts <- c("\\bADMINISTRATIO\\b","\\bADMINISTRATI\\b",
                            "\\bADMINISTRAT\\b","\\bADMINISTRA\\b",
                            "\\bADMINISTR\\b","\\bADMINIST\\b",
                            "\\bADMINIS\\b","\\bADMINI\\b",
                            "\\bADMIN\\b")
  for(name in administration.texts){
    TitleTxt <- gsub(name,"ADMINISTRATION",TitleTxt)
  }
  
  TitleTxt <- gsub("\\bADMINISTRATO\\b","ADMINISTRATOR",TitleTxt)
  TitleTxt <- gsub("\\bADMINISTRATIV\\b","ADMINISTRATIVE",TitleTxt)
  
  ## ------------------------------------------------------------------------
  #COORDINATOR
  coordinator.texts <- c("\\bCOORDINATO\\b","\\bCOORDINAT\\b",
                         "\\bCOORDINA\\b","\\bCOORDIN\\b",
                         "\\bCOORDI\\b","\\bCOORD\\b",
                         "\\COOR\\b")
  for(name in coordinator.texts){
    TitleTxt <- gsub(name,"COORDINATOR",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #STRATEGY
  strategy.texts <- c("\\bSTRATEGIC\\b", "\\bSTRATEGI\\b",
                      "\\bSTRATEG\\b","\\bSTRATE\\b",
                      "\\bSTRAT\\b")
  for(name in strategy.texts){
    TitleTxt <- gsub(name,"STRATEGY",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #HUMAN RESOURCES
  TitleTxt <- gsub( "\\bHU\\b", "HUMAN", TitleTxt )
  TitleTxt <- gsub( "\\bHUM\\b", "HUMAN", TitleTxt )
  TitleTxt <- gsub( "\\bHUMA\\b", "HUMAN", TitleTxt )
  TitleTxt <- gsub( "\\bH\\s*R\\b", "HUMAN RESOURCES", TitleTxt )
  TitleTxt <- gsub( "\\bRES\\b", "RESOURCES", TitleTxt )
  TitleTxt <- gsub( "\\bRESO\\b", "RESOURCES", TitleTxt )
  TitleTxt <- gsub( "\\bRESOU\\b", "RESOURCES", TitleTxt )
  TitleTxt <- gsub( "\\bRESOUR\\b", "RESOURCES", TitleTxt )
  TitleTxt <- gsub( "\\bRESOURC\\b", "RESOURCES", TitleTxt )
  TitleTxt <- gsub( "\\bRESOURCE\\b", "RESOURCES", TitleTxt )
  TitleTxt <- gsub( "\\bHUMAN RE\\b", "HUMAN RESOURCES", TitleTxt )
  
  
  ## ------------------------------------------------------------------------
  
  #MANAGEMENT (most general)
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
  
  ## ------------------------------------------------------------------------
  #PROGRAM
  program.texts <- c("\\bPROGRAMS\\b","\\bPROGRA\\b",
                     "\\bPROGR\\b","\\bPROG\\b")
  for(name in program.texts){
    TitleTxt <- gsub(name,"PROGRAM",TitleTxt)
  }
  
  #PROJECT
  project.texts <- c("\\bPROJECTS\\b","\\bPROJEC\\b",
                     "\\bPROJE\\b","\\bPROJ\\b")
  for(name in project.texts){
    TitleTxt <- gsub(name,"PROJECT",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #PUBLIC
  public.texts <- c("\\bPUBLI\\b", "\\bPUBL\\b", "\\bPUB\\b")
  for(name in public.texts){
    TitleTxt <- gsub(name,"PUBLIC",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #BUSINESS
  business.texts <- c("\\bBUSINES\\b", "\\bBUSINE\\b",
                      "\\bBUSIN\\b", "\\bBUSI\\b",
                      "\\bBUS\\b")
  for(name in business.texts){
    TitleTxt <- gsub(name,"BUSINESS",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  # COMMUNICATIONS
  communications.texts <- c("\\bCOMMUNICATION\\b", "\\bCOMMUNICATIO\\b",
                            "\\bCOMMUNICATI\\b","\\bCOMMUNICAT\\b",
                            "\\bCOMMUNICA\\b","\\bCOMMUNIC\\b",
                            "\\bCOMMUNI\\b","\\bCOMMUN\\b",
                            "\\bCOMMU\\b","\\bCOMM\\b",
                            "\\bCOM\\b")
  for(name in communications.texts){
    TitleTxt <- gsub(name,"COMMUNICATIONS",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #INFORMATION
  information.texts <- c("\\bINFORMATIO\\b", "\\bINFORMATI\\b",
                         "\\bINFORMAT\\b", "\\bINFORMA\\b",
                         "\\bINFORM\\b", "\\bINFOR\\b",
                         "\\bINFO\\b", "\\bINF\\b")
  for(name in information.texts){
    TitleTxt <- gsub(name,"INFORMATION",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #INTELLIGENCE
  intelligence.texts <- c("\\bINTELLIGENC\\b", "\\bINTELLIGEN\\b",
                          "\\bINTELLIGE\\b", "\\bINTELLIG\\b",
                          "\\bINTELLI\\b", "\\bINTELL\\b",
                          "\\bINTEL\\b", "\\bINTE\\b") #int can be interim
  for(name in intelligence.texts){
    TitleTxt <- gsub(name,"INTELLIGENCE",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #TECHNOLOGY
  technology.texts <- c("\\bTECHNOLOG\\b","\\bTECHNOLO\\b",
                        "\\bTECHNOL\\b","\\bTECHNO\\b",
                        "\\bTECHN\\b","\\bTECH\\b")
  for(name in technology.texts){
    TitleTxt <- gsub(name,"TECHNOLOGY",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #INSTITUTION
  institution.texts <- c("\\bINSTITUTIO\\b", "\\bINSTITUTI\\b",
                         "\\bINSTITUT\\b","\\bINSTITU\\b",
                         "\\bINSTIT\\b","\\bINSTI\\b",
                         "\\bINST\\b")
  for(name in institution.texts){
    TitleTxt <- gsub(name,"INSTITUTION",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #ACADEMICS
  academics.texts <- c("\\bACADEMIC\\b","\\bACADEMI\\b",
                       "\\bACADEM\\b","\\bACADE\\b",
                       "\\bACAD\\b", "\\bACADEMY\\b")
  for(name in academics.texts){
    TitleTxt <- gsub(name,"ACADEMICS",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #MARKETING
  marketing.texts <- c("\\bMARKETIN\\b","\\bMARKETI\\b",
                       "\\bMARKET\\b","\\bMARKE\\b",
                       "\\bMARK\\b","\\bMKTG\\b",
                       "\\bMKT\\b","\\bMRKTNG")
  for(name in marketing.texts){
    TitleTxt <- gsub(name,"MARKETING",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #ADVANCEMENT
  advancement.texts <- c("\\bADVANCEMEN\\b", "\\bADVANCEME\\b",
                         "\\bADVANCEM\\b", "\\bADVANCE\\b",
                         "\\bADVANC\\b", "\\bADVAN\\b",
                         "\\bADVA\\b", "\\bADV\\b",
                         "\\bADVANCEMNT\\b","\\bADVANCMNT",
                         "\\bADVNCMNT\\b")
  for(name in advancement.texts){
    TitleTxt <- gsub(name,"ADVANCEMENT",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  # PHILANTHROPY
  philanthropy.texts <- c("\\bPHILANTHROP\\b","\\bPHILANTHRO\\b",
                          "\\bPHILANTHR\\b","\\bPHILANTH\\b",
                          "\\bPHILANT\\b","\\bPHILAN\\b",
                          "\\bPHILA\\b","\\bPHIL\\b")
  for(name in philanthropy.texts){
    TitleTxt <- gsub(name,"PHILANTHROPY",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #SYSTEMS
  systems.texts <- c("\\bSYSTEM\\b","\\bSYSTE\\b",
                     "\\bSYST\\b","\\bSYS\\b")
  for(name in systems.texts){
    TitleTxt <- gsub(name,"SYSTEMS",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #GENERAL
  general.texts <- c("\\bGENERA\\b", "\\bGENER\\b",
                     "\\bGENE\\b", "\\bGENL\\b",
                     "\\bGEN\\b")
  for(name in general.texts){
    TitleTxt <- gsub(name,"GENERAL",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #PLANNING
  planning.texts <- c("\\bPLANNIN\\b","\\bPLANNI\\b",
                      "\\bPLANN\\b","\\bPLAN\\b")
  for(name in planning.texts){
    TitleTxt <- gsub(name,"PLANNING",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #COMPLIANCE
  compliance.texts <- c("\\bCOMPLIANC\\b","\\bCOMPLIAN\\b",
                        "\\bCOMPLIA\\b","\\bCOMPLI\\b",
                        "\\bCOMPL\\b")
  for(name in compliance.texts){
    TitleTxt <- gsub(name,"COMPLIANCE",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #ENROLLMENT
  enrollment.texts <- c("\\bENROLLMEN\\b","\\bENROLLME\\b",
                        "\\bENROLLM\\b","\\bENROLL\\b",
                        "\\bENROL\\b")
  for(name in enrollment.texts){
    TitleTxt <- gsub(name,"ENROLLMENT",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #ADMISSIONS
  admissions.texts <- c("\\bADMISSION\\b","\\bADMISS\\b","\\bADMIS\\b")
  for(name in admissions.texts){
    TitleTxt <- gsub(name,"ADMISSIONS",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #CHIEF
  chief.texts <- c("\\bCHIE\\b","\\bCHI\\b")
  for(name in chief.texts){
    TitleTxt <- gsub(name,"CHIEF",TitleTxt)
  }
  
  #### ------------------------------------------------------------------------
  #CORREPSONDING
  corresponding.texts <- c("\\bCORRESPONDIN\\b","\\bCORRESPONDI\\b",
                           "\\bCORRESPOND\\b","\\bCORRESPON\\b",
                           "\\bCORRESPO\\b","\\bCORRESP\\b","\\bCORRES\\b",
                           "\\bCORRE\\b","\\bCORR\\b", "\\bCORRS\\b",
                           "\\bCORRESPD\\b","\\bCORRESPONDANCE\\b",
                           "\\bCORRESPONDENCE\\b")
  for(name in corresponding.texts){
    TitleTxt <- gsub(name,"CORRESPONDING",TitleTxt)
  }
  
  
  ## ------------------------------------------------------------------------
  #should be sooner
  
  #EMERITUS
  emeritus.texts <- c("\\bEMERITU\\b", "\\bEMERITA\\b", "\\bEMERIT\\b",
                      "\\bEMERI\\b", "\\bEMER\\b", "\\bEME\\b", "\\bEM\\b",
                      "\\bEMERITIS\\b", "\\bEMERIUS\\b"
  )
  for(title in emeritus.texts){
    TitleTxt <- gsub(title,"EMERITUS",TitleTxt)
  }
  
  ## ------------------------------------------------------------------------
  #convert all full c-suite positions into abbrev
  TitleTxt <- gsub( "CHIEF\\sEXECUTIVE\\sOFFICER", "CEO", TitleTxt ) #executive
  TitleTxt <- gsub( "CHIEF\\sOPERATIONS\\sOFFICER", "COO", TitleTxt ) #operating
  TitleTxt <- gsub( "CHIEF\\sFINANCE\\sOFFICER", "CFO", TitleTxt ) #finance
  # TitleTxt <- gsub( "CHIEF\\sDATA\\sOFFICER", "CDO", TitleTxt ) #data
  # TitleTxt <- gsub( "CHIEF\\sTECHNOLOGY\\sOFFICER", "CTO", TitleTxt ) #technology
  # TitleTxt <- gsub( "CHIEF\\sADMINISTRATION\\sOFFICER", "CAO", TitleTxt ) #administrative
  # TitleTxt <- gsub( "CHIEF\\sINFORMATION\\sOFFICER", "CIO", TitleTxt ) #information
  # TitleTxt <- gsub( "CHIEF\\sINNOVATION\\sOFFICER", "CIO", TitleTxt ) #innovation
  # TitleTxt <- gsub( "CHIEF\\sNURSING\\sOFFICER", "CNO", TitleTxt ) #nursing
  # TitleTxt <- gsub( "CHIEF\\sMARKETING\\sOFFICER", "CMO", TitleTxt ) #marketing
  #change executive director and chief executive director to CEO
  TitleTxt <- gsub("EXECUTIVE DIRECTOR", "CEO", TitleTxt)
  
  
  ## ------------------------------------------------------------------------
  #miscellaneous:
  TitleTxt <- gsub( "BOARD MEMBERDEPT CHAIR", "BOARD MEMBER DEPT CHAIR", TitleTxt )
  TitleTxt <- gsub("\\bREP\\b","REPRESENTATIVE", TitleTxt)
  TitleTxt <- gsub("\\bMINISTR\\b","MINISTRY", TitleTxt)
  TitleTxt <- gsub("\\bMINISTER\\b","MINISTRY", TitleTxt)
  TitleTxt <- gsub("\\bSGT\\b","SERGEANT", TitleTxt)
  TitleTxt <- gsub("\\bCONSU\\b","CONSULTANT", TitleTxt)
  TitleTxt <- gsub("\\bM\\b","MANAGER", TitleTxt) #standalone m is likely manager
  #m could also be museum
  TitleTxt <- gsub("\\bP\\b","PRESIDENT", TitleTxt) #standalone p is likely pres
  
  TitleTxt <- gsub("\\bFO\\b","FOUNDER", TitleTxt)
  
  
  board.texts <- c("\\bBOAR\\b", "\\bBOA\\b", "\\bBO\\b",
                   "\\bB\\b", "\\bBRD\\b","\\bBOD\\b","\\bBD\\b")
  for(title in board.texts){
    TitleTxt <- gsub(title,"BOARD",TitleTxt)
  }
  
  TitleTxt <- gsub("\\bMBR\\b", "MEMBER", TitleTxt)
  TitleTxt <- gsub("\\bMBER\\b", "MEMBER", TitleTxt)
  TitleTxt <- gsub("\\bMMBR\\b", "MEMBER", TitleTxt)
  TitleTxt <- gsub("PRESIDENT PRESIDENT", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("PRESIDENTPR.*\\b", "PRESIDENT", TitleTxt)
  TitleTxt <- gsub("CEOCEO", "CEO", TitleTxt)
  
  
  
  #heuristics
  if(TitleTxt == "VICE") TitleTxt <- "VICE PRESIDENT" #standalone vice is likely vp
  else if(TitleTxt == "PR") TitleTxt <- "PRESIDENT" #standalone pr is likely pres
  else if(TitleTxt == "T") TitleTxt <- "TRUSTEE" #standalone t is likely trustee
  else if(grepl("\\bCE$",TitleTxt)) TitleTxt <- gsub("\\bCE$","CEO",TitleTxt) #ceo misspelling
  else if (grepl("\\bCF$",TitleTxt)) TitleTxt <- gsub("\\bCF$","CFO",TitleTxt)
  else if(TitleTxt == "EXECUTIVE OFFICER") TitleTxt <- "CEO"
  else if(TitleTxt == "N/A" | TitleTxt == "\\bN\\s*A\\b" | TitleTxt == "\\s*") 
    TitleTxt <- NA
  #NA could be north america but most likely null value
  
  ## ------------------------------------------------------------------------
  
  #CORRECT TITLES ONES
  ## ---------
  TitleTxt <- gsub("EXECUTIVE OFFICER", "EX-OFFICIO", TitleTxt)
  #if it falls thru, then it's ex-officio mistake
  TitleTxt <- gsub("EXECUTIVE OFFICIO", "EX-OFFICIO", TitleTxt) #mistake
  TitleTxt <- gsub("\\bD\\b", " DIRECTOR", TitleTxt) #assume standalone d is director
  TitleTxt <- gsub("EXECUTIVE DIRECTOR", "CEO", TitleTxt) #necessary replacement
  TitleTxt <- gsub("\\bCO\\s", "CO-", TitleTxt) #co-chair over co chair for example
  
  #missed abbreviation
  TitleTxt <- gsub("\\bMEM\\b", "MEMBER",TitleTxt)
  TitleTxt <- gsub("\\bME\\b", "MEMBER",TitleTxt)
  TitleTxt <- gsub("\\bCL\\b", "CLERK", TitleTxt)
  TitleTxt <- gsub("\\bER\\b", "EDITOR", TitleTxt)
  TitleTxt <- gsub("\\bEDR\\b", "EDITOR", TitleTxt)
  TitleTxt <- gsub("\\bASSIT\\b", "ASSISTANT", TitleTxt)
  TitleTxt <- gsub("\\bADMI\\b", "ADMINISTRATION", TitleTxt)
  TitleTxt <- gsub("\\bADMN\\b", "ADMINISTRATION", TitleTxt)
  TitleTxt <- gsub("\\bCHAIRPE\\b", "CHAIR", TitleTxt)
  TitleTxt <- gsub("\\bTRTEE\\b", "TRUSTEE", TitleTxt)
  TitleTxt <- gsub("\\bCUL\\b", "CULTURE", TitleTxt)
  TitleTxt <- gsub("\\bSERV\\b", "SERVICE", TitleTxt)
  TitleTxt <- gsub("\\bGOVERN\\b", "GOVERNANCE", TitleTxt)
  
  TitleTxt <- gsub("\\bPR\\b", "PUBLIC RELATIONS", TitleTxt)
  TitleTxt <- gsub("\\bPUBLIC RELATION\\b", "PUBLIC RELATIONS", TitleTxt)
  TitleTxt <- gsub("\\bPUBLIC RELAT\\b", "PUBLIC RELATIONS", TitleTxt)
  TitleTxt <- gsub("\\bPUBLIC REALTIONS\\b", "PUBLIC RELATIONS", TitleTxt)
  TitleTxt <- gsub("\\bPUBLIC REL\\b", "PUBLIC RELATIONS", TitleTxt)
  TitleTxt <- gsub("\\bFI\\b", "FINANCE", TitleTxt)
  
  #transportation
  transportation.texts <- c("\\bTRANSPORTATIO\\b", "\\bTRANSPORTATI\\b",
                            "\\bTRANSPORTAT\\b", "\\bTRANSPORTA\\b",
                            "\\bTRANSPORT\\b", "\\bTRANSPOR\\b",
                            "\\bTRANSPO\\b", "\\bTRANSP\\b", "\\bTRANS\\b")
  for(name in transportation.texts){
    TitleTxt <- gsub(name, "TRANSPORTATION", TitleTxt)
  }
  
  TitleTxt <- gsub("\\bGM\\b", "GENERAL MANAGER", TitleTxt)
  TItleTxt <- gsub("\\bCO$", "COMMITTEE", TitleTxt) # more often than not
  
  
  #remove residual spacing issues
  TitleTxt <- gsub("^\\s* | \\s*$", "", TitleTxt)
  TitleTxt <- gsub( "\\s{2,}", " ", TitleTxt )
  
  ######
  return( TitleTxt )
  
}

#########
#testing function


#' @title
#' build standard titles function
#' DEPRECATED
#'
#'
#' @description
#' `build_standard_titles` takes in a compensation table and cleans up the titles.
#' It does some basic data extraction from the titles (including date extraction,
#' standardization, and more), but there is still more work to do.
#' Note: this function was specifically designed for jesse's title-test-run.csv,
#' but it has been slightly modified so that it works more generally from part
#' vii table 01 builds.
#' This test function combines all the functions from this file to manipulate
#' a data frame. It takes in a data frame and outputs a filtered data frame.
#'
build_standard_titles <- function(comp.table){
  df <- comp.table
  oldTitles <- df$TitleTxt
  standardTitles <- list()
  print("DONE READING")
  k <- 1
  pdf <- data.frame(matrix(ncol = ncol(df) + 2))
  for(i in 1:length(oldTitles)){
    #splitting then cleaning then standardizing
    cleanedTitles <- c()
    if(nchar(oldTitles[i]) != 0){
      #split
      print(i)
      standardTitles[[i]] <- split_compound_title(oldTitles[i])
      for(j in 1:length(standardTitles[[i]])){
        #clean
        standardTitles[[i]][j] <- apply_cleaning(standardTitles[[i]][j])
        if(nchar(standardTitles[[i]][j]) != 0){
          #standardize
          cleanedTitles <- append(cleanedTitles,standardize_titles(standardTitles[[i]][j]))
        }
      }
    }
    standardTitles[[i]] <- cleanedTitles
    if(!is.null(cleanedTitles)){
      for(j in 1:length(standardTitles[[i]])){
        standard_title <- standardTitles[[i]][j]
        pdf[k,] = cbind(df[i,], standard_title,j)
        k <- k+1
      }
    }
  }
  print("FINISHED STANDARDIZING")
  column_names <- names(df)
  column_names <- append(column_names,"StandardTitles")
  column_names <- append(column_names, "TitleNum")
  colnames(pdf) <- column_names
  pdf$Date <- NA
  # pdf$Num.Dates <- 0
  for(i in 1:length(pdf$TitleTxt)){
    dates <- extract_date(pdf$TitleTxt[i])
    pdf$Date[i] <- dates[1]
    # pdf$Num.Dates <- dates[2]
  }
  
  cdf <- pdf
  #if reading from jesse's data
  # cdf <- pdf %>% rename(Cleaned_Title = TitleTxt2)
  # cdf$Cleaned_Title <- cdf$StandardTitles
  
  print("ALL DONE!")
  write.csv(cdf,paste0("refined-titles-UTD-",Sys.Date(),".csv"))
  return(cdf)
}


#
#if using anything related to jesse's test dataset
##example building
#raw <- read.csv("test-tables/title-test-run.csv")
#df <- raw

##if using anything related to jesse's test dataset
#df <- unique(subset(format_comp_df(raw),select = FilerEIN:NTMAJ12))

#build_standard_titles(df)


######


