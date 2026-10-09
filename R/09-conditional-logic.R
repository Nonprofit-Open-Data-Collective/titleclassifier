#Step 9: Conditional Logic

# 09-conditional-logic.R

#' @title
#' conditional logic function
#'
#' @description
#' Step 09. Resolves each person's role from the filer's own Part VII
#' checkboxes, their pay and hours, and the title, instead of the title alone.
#' This settles the titles whose meaning depends on context: a paid PRESIDENT
#' who runs the organization versus a volunteer board president, a VICE
#' PRESIDENT on the board versus on staff, a DIRECTOR who is a board member
#' versus a department head. It also finds the chief executive when no title
#' names one. See [resolve_roles()] for the rules and the columns added.
#'
#' @export
#' @param comp.data The output of `categorize_titles()`.
conditional_logic <- function(comp.data)
{
  df <- resolve_roles( comp.data )
  cat( "[OK] conditional logic step complete\n" )
  return(df)
}


#' @title
#' resolve roles from checkboxes, pay, and title
#'
#' @description
#' Adds role columns to the output of `categorize_titles()`. The rules were
#' developed and checked against a hand-labeled sample in
#' `synthid/dev` (06_role_cascade_prototype.R, 10_role_passes.R). They work in
#' three layers:
#'
#' 1. **Position**, per row: the filer's checkboxes come first. The title and
#'    pay are used when a paid executive title outranks a missing officer box,
#'    and on 990-EZ returns, which have no checkboxes.
#'    - The trustee box means `board`.
#'    - The officer box means `officer`.
#'    - Both boxes mean `board_officer` (a working board).
#'    - The key-employee and highly-compensated boxes mean `staff`.
#' 2. **Person.** A person whose entry was split into several titles gets one
#'    position, the most senior of their rows. `role.primary` marks one row
#'    per person, so counts of people don't double up.
#' 3. **Leadership**, per filing:
#'    - A person with a CEO-level title in an officer position is the
#'      `designated` CEO (`co-ceo` when there are several, `interim` when
#'      flagged so).
#'    - Otherwise, the highest-paid plausible leader is `imputed`: a paid
#'      person with the officer box or a leadership title. A paid board chair
#'      who ticks the trustee box and works under 25 hours a week doesn't count.
#'    - If no paid person qualifies, the filing is `board_governed` and no CEO
#'      is named.
#'
#' A board title (president, secretary, treasurer...) held by someone who
#' works under 10 hours a week and is paid under $25,000 is a board seat, even
#' with the officer box. Such a person is a titular officer receiving a
#' stipend, not a paid executive.
#'
#' Scored against a 158-person hand-labeled sample of 2010-12 filings
#' (`data-raw/partvii-validation/09-gold-check.R`), the role agrees with the
#' label for 92.9% of people.
#'
#' @return `comp.data` with these columns added (person-level values repeat on
#'   each of the person's rows):
#'   \describe{
#'     \item{role.position}{board, officer, board_officer, staff, or unknown}
#'     \item{role.final}{CEO, OFFICER, MANAGER, PROFESSIONAL, STAFF, or BOARD}
#'     \item{role.board}{for BOARD: CHAIR, VICE CHAIR, SECRETARY, TREASURER, or MEMBER}
#'     \item{role.ceo}{for CEO: designated, co-ceo, interim, or imputed}
#'     \item{role.source}{what settled the position: checkbox, title+pay, or title}
#'     \item{role.primary}{TRUE on one row per person}
#'     \item{org.leadership}{per filing: designated, imputed, board_governed, or no_paid}
#'   }
#'
#' @export
#' @param comp.data The output of `categorize_titles()`.
resolve_roles <- function( comp.data )
{
  d <- data.table::as.data.table( comp.data )
  d[ , .row := .I ]

  num  <- function(x) { x <- suppressWarnings( as.numeric(x) ); x[ is.na(x) ] <- 0; x }
  box  <- function(x) ! is.na(x) & suppressWarnings( as.numeric(x) ) %in% 1
  chr  <- function(x) { x <- as.character(x); x[ is.na(x) ] <- ""; x }

  title  <- toupper( paste( chr(d$title.raw), chr(d$title.standard) ) )
  level  <- chr( d$emp.level )
  brole  <- chr( d$board.role )
  comp   <- num( d$tot.comp )
  hrs    <- num( d$tot.hours )
  paid   <- comp > 0
  cb_tru <- box( d$dtk.indiv.trustee.x ) | box( d$dtk.inst.trustee.x )
  cb_off <- box( d$dtk.officer.x )
  cb_key <- box( d$dtk.key.empl.x ) | box( d$dtk.high.comp.x )
  # 990-EZ returns have no position checkboxes (the columns hold 0, not NA)
  boxes  <- ! toupper( chr( d$formtype ) ) %in% c( "990EZ", "990-EZ" )

  exec_title  <- level %in% c( "CEO", "OFFICER" )
  board_title <- nzchar( brole )
  pres_chair  <- brole == "CHAIR" |
                 ( grepl( "PRESIDENT|CHAIR", title ) & ! grepl( "VICE", title ) )
  lead_title  <- level %in% c( "CEO", "OFFICER", "MANAGER" ) |
                 grepl( "\\bCEO\\b|CHIEF|EXECUTIVE|PRESIDENT|\\bDIRECTOR\\b|MANAGER|ADMINISTRATOR|SUPERINTENDENT|PRINCIPAL|\\bDEAN\\b|\\bHEAD\\b|OFFICER", title )

  # ---- 1. position, per row ----------------------------------------------------
  position <- data.table::fcase(
    exec_title & paid & ! cb_tru,       "officer",
    exec_title & paid &   cb_tru,       "board_officer",
    cb_off &   cb_tru,                  "board_officer",
    cb_off & ! cb_tru,                  "officer",
    ! cb_off & cb_tru,                  "board",
    cb_key,                             "staff",
    board_title & ! paid,               "board",
    paid,                               "staff",
    ! boxes & level == "OFFICER",       "board",      # unpaid VP, deputy... on a 990-EZ
    default = "unknown" )
  source <- data.table::fcase(
    exec_title & paid,                  "title+pay",
    cb_off | cb_tru | cb_key,           "checkbox",
    default = "title" )

  d[ , `:=`( .pos = position, .src = source, .comp = comp, .hrs = hrs, .paid = paid,
             .cb_tru = cb_tru, .cb_off = cb_off, .ceo_t = level == "CEO",
             .exec = exec_title, .board_t = board_title, .pres = pres_chair,
             .lead = lead_title, .level = level, .brole = brole,
             .interim = box( d$interim.x ) ) ]

  # ---- 2. one position per person ----------------------------------------------
  rank <- c( officer = 1L, board_officer = 2L, board = 3L, staff = 4L, unknown = 5L )
  d[ , .rank := rank[ .pos ] ]
  d[ , `:=`( .pmin = min(.rank),
             .primary = seq_len(.N) == which.min(.rank) ),
     by = .( object.id, person.id ) ]
  d[ , role.position := names(rank)[ .pmin ] ]
  # person-level signals, from any of their rows
  d[ , `:=`( .p_paid = any(.paid), .p_comp = max(.comp), .p_hrs = max(.hrs),
             .p_ceo_t = any(.ceo_t), .p_exec = any(.exec), .p_board_t = any(.board_t),
             .p_lead = any(.lead), .p_off = any(.cb_off), .p_tru = any(.cb_tru),
             .p_pres = any(.pres), .p_interim = any(.interim) ),
     by = .( object.id, person.id ) ]

  # ---- 3. leadership, per filing (on one row per person) -----------------------
  p <- d[ .primary == TRUE ]
  # an unpaid executive director with no checkbox is still the executive
  # director: only a board-only position rules a CEO title out
  p[ , .desig := .p_ceo_t & role.position %in% c( "officer", "board_officer", "unknown" ) ]
  p[ , .chair_pattern := .p_tru & .p_hrs < 25 & .p_pres ]
  # an imputed leader must do more than token work: 15+ hours a week or $25k+
  p[ , .elig := .p_paid & ! .chair_pattern & ( .p_off | .p_lead ) &
                ( .p_hrs >= 15 | .p_comp >= 25000 ) ]
  p[ , `:=`( .n_desig = sum(.desig), .any_paid = any(.p_paid),
             .best = if( any(.elig) ) max( .p_comp[.elig] ) else NA_real_ ),
     by = object.id ]
  p[ , role.ceo := data.table::fcase(
          .desig & .p_interim,                            "interim",
          .desig & .n_desig >= 2,                         "co-ceo",
          .desig,                                         "designated",
          .n_desig == 0 & .elig & .p_comp == .best,       "imputed",
          default = NA_character_ ) ]
  # a tie for top pay among imputed leaders: keep the first person
  p[ role.ceo %in% "imputed", .k := seq_len(.N), by = object.id ]
  p[ role.ceo %in% "imputed" & .k > 1, role.ceo := NA_character_ ]
  p[ , org.leadership := data.table::fcase(
          .n_desig[1] > 0,                   "designated",
          any( role.ceo %in% "imputed" ),    "imputed",
          .any_paid[1],                      "board_governed",
          default = "no_paid" ),
     by = object.id ]

  # ---- 4. final role and board role, per person ---------------------------------
  p[ , role.final := data.table::fcase(
          ! is.na(role.ceo),                                              "CEO",
          role.position == "board",                                       "BOARD",
          role.position == "board_officer" & ! ( .p_exec & .p_paid ),     "BOARD",
          role.position == "officer" & .p_board_t & ! .p_paid,            "BOARD",
          # a board title held a few hours a week for a stipend is a board
          # seat, not a paid officer (a titular president, F-023)
          role.position %in% c( "officer", "board_officer" ) & .p_board_t &
            .p_hrs < 10 & .p_comp < 25000,                                "BOARD",
          role.position %in% c( "officer", "board_officer" ),             "OFFICER",
          role.position == "unknown" & .p_board_t,                        "BOARD",
          role.position == "unknown" & .p_exec,                           "OFFICER",
          .level %in% c( "MANAGER", "PROFESSIONAL", "STAFF" ),            .level,
          default = "STAFF" ) ]

  # the board role: the crosswalk's, or read from the title for an employee
  # title that turned out to be a board seat (VICE PRESIDENT, SECRETARY, ...)
  brank <- c( CHAIR = 1L, `VICE CHAIR` = 2L, SECRETARY = 3L, TREASURER = 4L, MEMBER = 5L )
  d[ , .brole_t := data.table::fcase(
          nzchar(.brole),                                   .brole,
          grepl( "VICE (PRESIDENT|CHAIR)", title ),         "VICE CHAIR",
          grepl( "PRESIDENT|CHAIR", title ),                "CHAIR",
          grepl( "SECRETARY", title ),                      "SECRETARY",
          grepl( "TREASURER", title ),                      "TREASURER",
          default = "MEMBER" ) ]
  pb <- d[ , .( role.board = names(brank)[ min( brank[.brole_t] ) ] ), by = .( object.id, person.id ) ]
  p <- merge( p, pb, by = c( "object.id", "person.id" ), all.x = TRUE )
  p[ role.final != "BOARD", role.board := NA_character_ ]

  # ---- map back onto every row ---------------------------------------------------
  out <- merge( d[ , .( .row, object.id, person.id, role.position, role.source = .src,
                        role.primary = .primary ) ],
                p[ , .( object.id, person.id, role.final, role.board, role.ceo, org.leadership ) ],
                by = c( "object.id", "person.id" ), all.x = TRUE )
  data.table::setorder( out, .row )

  res <- as.data.frame( comp.data )
  for( v in c( "role.position", "role.final", "role.board", "role.ceo",
               "role.source", "role.primary", "org.leadership" ) )
  { res[[v]] <- out[[v]] }
  res
}




#' @title
#' clean up ceos function
#'
#' @description
#' Removes duplicate CEO rows within a filing. Not used by the pipeline:
#' `resolve_roles()` marks one row per person (`role.primary`) instead of
#' dropping rows.
#'
#' @export
#' @param comp.data A Part VII compensation data frame.
clean_up_ceos <- function(comp.data){
  df <- comp.data

  df = df[!duplicated(df[, c("ein", "dtk.name", "dtk.title", "title.standard")]) |
            df$title.standard != "CEO", ] #remove duplicates of ceos
  #but potential issues with multiple titles


  return(df)
}


#' @title
#' director correction function
#'
#' @description
#' Restores DIRECTOR for directors with no trustee box. Not used by the
#' pipeline: `resolve_roles()` settles board versus staff directors from the
#' checkboxes and pay.
#'
#' @export
#' @param df A Part VII compensation data frame.
director_correction <- function(df)
{

  df$title.standard <- ifelse(df$title.v7 == "DIRECTOR" &
                           df$dtk.indiv.trustee.x == 0 & df$dtk.inst.trustee.x == 0,
                           df$title.v7, df$title.standard)

  return(df)
}
