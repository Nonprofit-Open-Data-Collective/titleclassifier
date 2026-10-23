# Step 09 helper: the organization type that the CEO thesaurus (ceo.thesaurus)
# is keyed on. FINDINGS.md F-033.

#' @title organization types for the CEO thesaurus
#'
#' @description
#' The types the CEO thesaurus ([ceo.thesaurus]) is keyed on. Each is a kind of
#' organization whose chief executive often carries a title other than CEO or
#' executive director (an adjutant, a general manager, a business manager...).
#'
#' @export
#' @return A character vector.
org_type_levels <- function()
  c( "veterans", "fraternal", "union", "club", "coop", "trust", "cemetery",
     "university", "ministry", "fire", "orchestra" )


#' @title organization type from NTEE code and 501(c) subsection
#'
#' @description
#' Maps the IRS Business Master File's NTEE code and 501(c) subsection (for
#' example from the NCCS unified BMF, `panel990::bmf_retrieve()`) to an
#' [org_type_levels()] type, or `NA`.
#'
#' @export
#' @param ntee NTEE code (e.g. "X21", "M24"); NA or "UNDEFINED" when unknown.
#' @param subsection 501(c) subsection number (e.g. 3, 5, 19).
#' @param name Organization name, used only to keep a university's foundation,
#'   alumni or student association out of "university".
#' @return A character vector of types, `NA` where none applies.
org_type_from_codes <- function( ntee, subsection, name = "" )
{
  n <- max( length(ntee), length(subsection), length(name) )
  ntee <- rep_len( ntee, n ); subsection <- rep_len( subsection, n ); name <- rep_len( name, n )
  n3  <- toupper( substr( ifelse( is.na(ntee) | ntee == "UNDEFINED", "", ntee ), 1, 3 ) )
  n1  <- substr( n3, 1, 1 )
  sub <- suppressWarnings( as.integer( subsection ) ); sub[ is.na(sub) ] <- -1L
  nm  <- toupper( ifelse( is.na(name), "", name ) )
  support <- "FOUNDATION|AUXILIARY|ALUMNI|FRIENDS OF|BOOSTER|STUDENT|ASSOCIATION"
  data.table::fcase(
    sub %in% c( 19L, 23L ),                                      "veterans",
    sub %in% c( 8L, 10L ),                                       "fraternal",
    sub == 5L & n1 %in% c( "J", "" ),                            "union",
    sub == 7L,                                                   "club",
    sub == 12L,                                                  "coop",
    sub %in% c( 9L, 17L ),                                       "trust",
    sub == 13L,                                                  "cemetery",
    n3 %in% c( "B41", "B42", "B43" ) & ! grepl( support, nm ),   "university",
    n3 == "M24",                                                 "fire",
    n3 %in% c( "A68", "A69" ),                                   "orchestra",
    n1 == "X",                                                   "ministry",
    default = NA_character_ )
}


#' @title organization type inferred from the filing
#'
#' @description
#' Infers an [org_type_levels()] type from the organization's name and the
#' titles listed in its Part VII, for filings with no NTEE code or 501(c)
#' subsection. A title can mark a type on its own (ADJUTANT, EXALTED RULER,
#' BUSINESS AGENT).
#'
#' @export
#' @param name Organization name.
#' @param titles All the raw titles in the filing, pasted into one string.
#' @return A character vector of types, `NA` where none applies.
org_type_from_name <- function( name, titles = "" )
{
  n <- max( length(name), length(titles) )
  name <- rep_len( name, n ); titles <- rep_len( titles, n )
  nm <- toupper( ifelse( is.na(name), "", name ) )
  tt <- toupper( ifelse( is.na(titles), "", titles ) )
  support <- "FOUNDATION|AUXILIARY|ALUMNI|FRIENDS OF|BOOSTER|SCHOLARSHIP"
  data.table::fcase(
    grepl( "AMERICAN LEGION|\\bVFW\\b|VETERANS OF FOREIGN|AMVETS|DISABLED AMERICAN VETERANS|VIETNAM VETERANS|\\bDAV\\b", nm ) |
      grepl( "\\bADJUTANT\\b|QUARTERMASTER", tt ),                                         "veterans",
    grepl( "\\bLODGE\\b|\\bAERIE\\b|\\bELKS\\b|MASON|MASONIC|SHRINE|\\bEAGLES\\b|\\bMOOSE\\b|ODD FELLOWS|KNIGHTS OF COLUMBUS|\\bGRANGE\\b", nm ) |
      grepl( "EXALTED RULER|WORSHIPFUL|POTENTATE|NOBLE GRAND|\\bWARDEN\\b|ESTEEMED", tt ),   "fraternal",
    grepl( "\\bLOCAL\\s*(NO\\.?|#)?\\s*[0-9]+|\\bUNION\\b|BROTHERHOOD|TEAMSTERS|FEDERATION OF|\\bWORKERS\\b|EDUCATION ASSOCIATION|TEACHERS ASSOCIATION|\\bAFSCME\\b|\\bIBEW\\b|\\bAFL\\b", nm ) &
      ! grepl( "CREDIT UNION|STUDENT UNION", nm ) |
      grepl( "BUSINESS AGENT|BUSINESS MANAGER|BUS MGR|SHOP STEWARD", tt ),                 "union",
    grepl( "COUNTRY CLUB|GOLF CLUB|YACHT CLUB|ATHLETIC CLUB|TENNIS CLUB|SWIM CLUB|BEACH CLUB|HUNT CLUB|CITY CLUB|RACQUET", nm ), "club",
    grepl( "COOPERATIVE|\\bCO-?OP\\b|ELECTRIC MEMBERSHIP|WATER SUPPLY|WATER ASSOCIATION|IRRIGATION|TELEPHONE", nm ), "coop",
    grepl( "TRUST FUND|WELFARE FUND|PENSION|BENEFIT (TRUST|FUND|PLAN)|HEALTH (PLAN|FUND)|ANNUITY|\\bVEBA\\b|BENEFITS? TRUST", nm ), "trust",
    grepl( "CEMETERY|MEMORIAL PARK|BURIAL", nm ),                                            "cemetery",
    grepl( "\\bUNIVERSITY\\b|\\bCOLLEGE\\b(?! OF)|\\bSEMINARY\\b", nm, perl = TRUE ) & ! grepl( paste0( support, "|ASSOCIATION|STUDENT" ), nm ), "university",
    grepl( "FIRE (DEPT|DEPARTMENT|COMPANY|CO|DISTRICT|PROTECTION|RESCUE)\\b|VOLUNTEER FIRE|HOSE CO|FIREMEN", nm ) & ! grepl( support, nm ), "fire",
    grepl( "ORCHESTRA|SYMPHON|PHILHARMONIC", nm ),                                           "orchestra",
    grepl( "CHURCH|MINISTR|CONGREGATION|SYNAGOGUE|PARISH|DIOCESE|\\bCHAPEL\\b", nm ) |
      grepl( "\\bPASTOR\\b|\\bREVEREND\\b|\\bBISHOP\\b", tt ),                                "ministry",
    default = NA_character_ )
}


#' @title organization type per filing
#'
#' @description
#' One type per filing (`object.id`), for the CEO thesaurus. The source, in
#' order of preference:
#' 1. **supplied**: an `org.type` column (one of [org_type_levels()]);
#' 2. **codes**: NTEE and 501(c) subsection columns (`ntee` or
#'    `ntee_code_clean`, `subsection` or `subsection_code`), as joined from the
#'    IRS Business Master File, mapped by [org_type_from_codes()];
#' 3. **inferred**: the organization name and the filing's titles
#'    ([org_type_from_name()]).
#'
#' @export
#' @param comp.data Step 08 output (one row per person-title).
#' @return A data.table with `object.id`, `org.type` and `org.type.source`.
org_types <- function( comp.data )
{
  d <- data.table::as.data.table( comp.data )
  col <- function( ... ) { for( v in c(...) ) if( v %in% names(d) ) return( as.character( d[[v]] ) ); rep( NA_character_, nrow(d) ) }
  d[ , `:=`( .sup = col( "org.type" ), .ntee = col( "ntee", "ntee_code_clean" ),
             .sub = col( "subsection", "subsection_code" ), .name = col( "org.name", "ORG_NAME_L1" ),
             .title = col( "title.raw", "F9_07_COMP_DTK_TITLE" ) ) ]
  f <- d[ , .( sup = .sup[ ! is.na(.sup) & nzchar(.sup) ][1], ntee = .ntee[ ! is.na(.ntee) & nzchar(.ntee) ][1],
               sub = .sub[ ! is.na(.sub) & nzchar(.sub) ][1], name = .name[1],
               titles = paste( unique( .title ), collapse = " | " ) ), by = object.id ]
  f[ ! sup %in% org_type_levels(), sup := NA_character_ ]
  f[ , codes := org_type_from_codes( ntee, sub, name ) ]
  f[ , inferred := org_type_from_name( name, titles ) ]
  # with codes, the subsection settles the membership types (a 501(c)(3)
  # "Union Rescue Mission" is not a union); the name can still add a type that
  # the NTEE code would give but is missing or miscoded
  has_codes <- ! is.na( f$ntee ) | ! is.na( f$sub )
  ntee_kinds <- c( "university", "fire", "orchestra", "ministry" )
  use_inf <- ! has_codes | ( is.na( f$codes ) & f$inferred %in% ntee_kinds )
  f[ , `:=`( org.type = data.table::fcoalesce( sup, codes, data.table::fifelse( use_inf, inferred, NA_character_ ) ),
             org.type.source = data.table::fcase( ! is.na(sup), "supplied",
                                                  ! is.na(codes), "codes",
                                                  use_inf & ! is.na(inferred), "inferred",
                                                  default = "none" ) ) ]
  f[ , .( object.id, org.type, org.type.source ) ]
}
