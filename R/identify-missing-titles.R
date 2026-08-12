add_strata <- function(df){

  titles <- df[["title.variant"]]

  strata <- rep( "", length(titles) )
  label <- rep( "", length(titles) )

  # executive committee
  yes <- grepl( "\\bEXECUTIVE COMMITTEE\\b", titles )
  strata[ yes ] <- "committee"
  label[ yes ] <- "executive"

  # committee member
  yes <- grepl( "\\bCOMMITTEE MEMBER\\b", titles )
  strata[ yes ] <- "committee"
  label[ yes ] <- "member"

  # chair
  yes <- grepl( "\\bCHAIR\\b", titles )
  strata[ yes ] <- "committee"
  label[ yes ] <- "chair"

  ###  DUPLEX

  # alternate
  yes <- grepl( "\\bALT\\b", titles )
  strata[ yes ] <- "duplex"
  label[ yes ] <- "alternate"
  yes <- grepl( "\\bALTERNATE\\b", titles )
  strata[ yes ] <- "duplex"
  label[ yes ] <- "alternate"

  # advisory
  yes <- grepl( "\\bADVISORY\\b", titles )
  strata[ yes ] <- "duplex"
  label[ yes ] <- "advisory"

  # adhoc
  yes <- grepl( "\\bADHOC\\b", titles )
  strata[ yes ] <- "duplex"
  label[ yes ] <- "adhoc"

  # co
  yes <- grepl( "\\bCO-\\b", titles )
  strata[ yes ] <- "duplex"
  label[ yes ] <- "co"

  ###  REGIONAL

  # regional
  yes <- grepl( "\\bREGIONAL\\b", titles )
  strata[ yes ] <- "regional"
  label[ yes ] <- "regional"

  ###  SUBORDINATE
 
  # assistant
  yes <- grepl( "\\bASSISTANT\\b", titles )
  strata[ yes ] <- "subordinate"
  label[ yes ] <- "assistant"

  # associate
  yes <- grepl( "\\bASSOCIATE\\b", titles )
  strata[ yes ] <- "subordinate"
  label[ yes ] <- "associate"

  # deputy
  yes <- grepl( "\\bDEPUTY\\b", titles )
  strata[ yes ] <- "subordinate"
  label[ yes ] <- "deputy"

  # junior
  yes <- grepl( "\\bJUNIOR\\b", titles )
  strata[ yes ] <- "subordinate"
  label[ yes ] <- "junior"

  ###  SUPERIOR

  # senior
  yes <- grepl( "^SENIOR\\b", titles )
  strata[ yes ] <- "superior"
  label[ yes ] <- "senior"

  # senior
  yes <- grepl( "^EXECUTIVE\\b", titles )
  strata[ yes ] <- "superior"
  label[ yes ] <- "executive"

  df$strata <- strata
  df$strata.label <- label

  return(df)
}


missing_title_variants <- function( df ){

  df_miss <- df[ is.na( df$title.standard ), c("title.raw","title.v7") ]
  title_no_match <- df_miss$title.v7
  cat( paste0("There are ", length(unique(title_no_match)), " unique missing titles.\n") )

  filter_t <- function(x){ x[ x > 3 ] }

  nmlist <- list()

  # VICE PRESIDENT
  vp_of <- grep( "\\bVICE PRESIDENT OF\\b", title_no_match, value=T )
  # vp_of |> table() |> sort( decr=T ) |> head(100)
  # vp_of |> unique() |> length()

  tt <- vp_of |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="VICE PRESIDENT" )
  nmlist$VP <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  # DIRECTOR OF ...
  dir_of <- grep( "\\bDIRECTOR OF\\b", title_no_match, value=T )
  # dir_of |> table() |> sort( decr=T ) |> head(100)
  # dir_of |> unique() |> length()

  tt <- dir_of |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="DIRECTOR" )
  nmlist$DIRECTOR_OF <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  # ... DIRECTOR
  dir <- grep( "\\bDIRECTOR$", title_no_match, value=T )
  # dir |> table() |> sort( decr=T ) |> head(100)
  # dir |> unique() |> length()

  tt <- dir |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="DIRECTOR" )
  nmlist$DIRECTOR <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  # CHIEF ... OFFICER
  chief_off <- grep( "^CHIEF\\b.*OFFICER$", title_no_match, value=T )
  # chief_off |> table() |> sort( decr=T ) |> head(100)
  # chief_off |> unique() |> length()

  tt <- chief_off |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="CHIEF _X_ OFFICER" )
  nmlist$CHIEF_X_OFF <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  # CHIEF (NO OFFICER)
  chief <- grep( "\\bCHIEF\\b", title_no_match, value=T )
  chief <- chief[ ! ( chief %in% chief_off ) ]
  # chief |> table() |> sort( decr=T ) |> head(100)
  # chief |> unique() |> length()

  tt <- chief |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="CHIEF" )
  nmlist$CHIEF <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  # OPERATIONS
  opps <- grep( "\\bOPERATIONS\\b", title_no_match, value=T )
  # opps |> table() |> sort( decr=T ) |> head(100)
  # opps |> unique() |> length()

  tt <- opps |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="OPERATIONS" )
  nmlist$OPERATIONS <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  # MANAGER
  man <- grep( "\\bMANAGER\\b", title_no_match, value=T )
  # man |> table() |> sort( decr=T ) |> head(100)
  # man |> unique() |> length()

  tt <- man |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="MANAGER" )
  nmlist$MANAGER <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  is_chair <- grepl( "\\bCHAIR\\b", title_no_match ) 
  is_committee <- grepl( "\\bCOMMITTEE\\b", title_no_match )
  is_member <- grepl( "\\bMEMBER\\b", title_no_match ) 

  # COMMITTEE CHAIR
  comm_chair <- title_no_match[ is_committee & is_chair ]
  # comm_chair |> table() |> sort( decr=T ) |> head(100)
  # comm_chair |> unique() |> length()

  tt <- comm_chair |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="BOARD MEMBER" )
  nmlist$BOARD_CHAIR <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  # COMMITTEE MEMBER
  comm_member <- title_no_match[ is_committee & is_member ]
  # comm_member |> table() |> sort( decr=T ) |> head(100)
  # comm_member |> unique() |> length()

  tt <- comm_member |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="BOARD MEMBER" )
  nmlist$BOARD_COMM <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  # CHAIR OF
  chair <- title_no_match[ is_chair & ! (is_committee | is_member) ]
  # chair |> table() |> sort( decr=T ) |> head(100)
  # chair |> unique() |> length()

  tt <- chair |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="BOARD MEMBER" )
  nmlist$BOARD_CHAIR_OF <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]


  # REPRESENTATIVE
  rep <- grep( "\\bREPRESENTATIVE\\b", title_no_match, value=T )
  # rep |> table() |> sort( decr=T ) |> head(100)
  # rep |> unique() |> length()

  tt <- rep |> table() |> filter_t() 
  dft <- data.frame( count=tt, title.variant=names(tt), title.standard="BOARD MEMBER" )
  nmlist$REPRESENTATIVE <- dft
  title_no_match <- title_no_match[ ! title_no_match %in% variant ]

  dff <- dplyr::bind_rows( nmlist )
  dff <- add_strata( dff )
  return( dff )

}
