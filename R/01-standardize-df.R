#Step 1: Standardize Data Frame
#01-standardize-df.R
require(dplyr) #should we port this to somewhere else?


#' @title
#' Standardize the compensation table data frame (from 990 form, Part VII, Table 01)
#'
#' @description
#' `standardize_df` returns a processed data frame with renamed column titles
#' and cleaned up fields from the raw table extracted from PVII T01 build. Also,
#' this function removes duplicated lines if present. Both the input and output
#' are data frames. Ultimately this function provides takes in raw data and
#' returns a data frame with understandable information about DTK individuals.
#'
#' @export
standardize_df <- function( df, 
                            title="F9_07_COMP_DTK_TITLE", 
                            form.type="FORMTYPE",
                            ave.hours="F9_07_COMP_DTK_AVE_HOUR_WEEK", 
                            ave.hours.rltd="F9_07_COMP_DTK_AVE_HOUR_WEEK_RL", 
                            comp.base="F9_07_COMP_DTK_COMP_ORG", 
                            comp.benefits="F9_07_COMP_DTK_EMPL_BEN", 
                            comp.related="F9_07_COMP_DTK_COMP_RLTD", 
                            comp.other="F9_07_COMP_DTK_COMP_OTH",
                            trustee.ind="F9_07_COMP_DTK_POS_INDIV_TRUST_X", 
                            trustee.inst="F9_07_COMP_DTK_POS_INST_TRUST_X", 
                            officer="F9_07_COMP_DTK_POS_OFF_X", 
                            key.employee="F9_07_COMP_DTK_POS_KEY_EMPL_X", 
                            high.comp.ind="F9_07_COMP_DTK_POS_HIGH_COMP_X", 
                            former="F9_07_COMP_DTK_POS_FORMER_X",
                            name = "F9_07_COMP_DTK_NAME_PERS"  )
{

  # make sure it has all vars
  df <- check_names(df)
  
  #### TITLE

  TitleTxt <- df[[ title ]]
  df$TITLE_RAW <- TitleTxt

  #converting missing titles to an empty string
  TitleTxt[ is.na(TitleTxt) ] <- ""
  
  #pre_clean call
  TitleTxt <- pre_clean(TitleTxt)

  df[[ title ]] <- TitleTxt


  #### HOURS

  x1 <- df[[ ave.hours ]]
  x2 <- df[[ ave.hours.rltd ]]
  x1 <- to_numeric(x1)
  x2 <- to_numeric(x2)

  df$TOT.HOURS <- x1 + x2
  df[[ ave.hours ]] <- x1
  df[[ ave.hours.rltd ]] <- x2


  #### COMPENSATION

  c.base  <- df[[ comp.base ]]
  c.ben   <- df[[ comp.benefits ]]
  c.rltd  <- df[[ comp.related ]]
  c.other <- df[[ comp.other ]]


  #converting all compensation fields to numeric if not already
  c.base  <- to_numeric( c.base )
  c.ben   <- to_numeric( c.ben )
  c.rltd  <- to_numeric( c.rltd )
  c.other <- to_numeric( c.other )
  
  #sum up all compensations for total comp column
  df$TOT.COMP <- c.base + c.ben + c.rltd + c.other


  #### CHECKBOXES

  formtype <- df[[ form.type ]]

  df[[  trustee.ind   ]]   <- to_boole( df[[  trustee.ind   ]], formtype ) 
  df[[  trustee.inst  ]]   <- to_boole( df[[  trustee.inst  ]], formtype )
  df[[  officer       ]]   <- to_boole( df[[  officer       ]], formtype )
  df[[  key.employee  ]]   <- to_boole( df[[  key.employee  ]], formtype )
  df[[  high.comp.ind ]]   <- to_boole( df[[  high.comp.ind ]], formtype )
  df[[  former        ]]   <- to_boole( df[[  former        ]], formtype )
  
  df = df[!duplicated(df[, c("EIN", name, title)]) & !is.na(df[[ name ]]), ]
  #remove duplicates of entries (if present)
  #catching duplicates if orgs upload individuals with multiple titles
  
  #some of the NA names are issues with the previous step of properly extracting
  #the one to many table

  #### RETURN CLEAN DF
  cat( "✔ standardize df step complete\n" )
  return( df )
  
}




#' @title
#' pre cleaning function
#'
#' @description
#' gets rid of meaningless punctuation (like periods), 
#' converts titles to uppercase form
#' 
#' @export
pre_clean <- function( x )
{
  x <- toupper(x)
  x <- gsub("\\.","", x )
  return( x )
}



to_numeric <- function( x )
{
  x <- gsub( "[^0-9.]", "", x )
  x <- as.numeric(x)
  x[ is.null(x) ] <- 0
  x[ is.na(x) ] <- 0
  return(x)
}


to_boole <- function( x, form.type )
{
  x <- trimws( x )
  x <- toupper( x )
  # CHECKBOX YES
  x[ x == "TRUE" ] <- 1
  x[ x == "X" ] <- 1
  x[ x == "YES" ] <- 1
  # CHECKBOX NO
  x[ x == "FALSE" ] <- 0
  x[ x == "NO" ] <- 0
  x[ x == "" ] <- 0
  # CONDITIONAL ON FORMTYPE
  x[ is.na(x) ] <- 0
  x <- as.numeric(x)
  x <- ifelse( form.type == "990EZ", NA, x )
  return(x)
}

check_names <- function( df )
{
  all.names <- 
    c("F9_07_COMP_DTK_POS_INDIV_TRUST_X", "F9_07_COMP_DTK_POS_INST_TRUST_X", 
      "F9_07_COMP_DTK_POS_OFF_X", "F9_07_COMP_DTK_POS_KEY_EMPL_X", 
      "F9_07_COMP_DTK_POS_HIGH_COMP_X", "F9_07_COMP_DTK_POS_FORMER_X", 
      "F9_07_COMP_DTK_AVE_HOUR_WEEK", "F9_07_COMP_DTK_AVE_HOUR_WEEK_RL", 
      "F9_07_COMP_DTK_COMP_ORG", "F9_07_COMP_DTK_COMP_RLTD", 
      "F9_07_COMP_DTK_COMP_OTH", "F9_07_COMP_DTK_EMPL_BEN")

  missing <- setdiff( all.names, names(df) )
  if( length(missing) == 0 )
  { return( df ) }

  for( i in missing )
  {
    df[[ i ]] <- NA
  }

  return(df)
}