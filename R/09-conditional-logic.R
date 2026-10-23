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
#' stipend, not a paid executive. On a 990-EZ, which has no boxes, the same
#' holds for a board title paid a small fee.
#'
#' **990-EZ returns** have no checkboxes. For them, when the xgboost package is
#' installed, the role comes from a model trained on full-990 people with the
#' boxes hidden ([predict_roles()]): title, pay, hours and the filing's
#' make-up. Without xgboost, or with `use_model = FALSE`, the rules above
#' decide. `role.method` records which one.
#'
#' Scored against the 316-person labeled set of 2010-12 filings
#' (`data-raw/partvii-validation/09-gold-check.R`, `gold-check/gold-v2.csv`),
#' the rules agree with the label for 88.8% of full-990 people. On 990-EZ
#' people the model agrees for 92.5%, and the rules for 82.5%. On 23,931 people
#' from organizations that switch between the forms, labeled by their
#' full-990-year role, the model agrees for 95.0% and the rules for 92.0%.
#'
#' @return `comp.data` with these columns added (person-level values repeat on
#'   each of the person's rows):
#'   \describe{
#'     \item{role.position}{board, officer, board_officer, staff, or unknown}
#'     \item{role.final}{CEO, OFFICER, MANAGER, PROFESSIONAL, STAFF, or BOARD}
#'     \item{role.board}{for BOARD: CHAIR, VICE CHAIR, SECRETARY TREASURER, SECRETARY, TREASURER, or MEMBER}
#'     \item{role.ceo}{for CEO: designated, co-ceo, interim, imputed, or model}
#'     \item{role.source}{what settled the position: checkbox, title+pay, or title}
#'     \item{role.method}{rules, or model (990-EZ, xgboost installed)}
#'     \item{role.primary}{TRUE on one row per person}
#'     \item{org.leadership}{per filing: designated, imputed, model, board_governed, or no_paid}
#'   }
#'
#' @export
#' @param comp.data The output of `categorize_titles()`.
#' @param use_model Use the no-checkbox role model for 990-EZ filings when
#'   xgboost is installed (default `TRUE`).
resolve_roles <- function( comp.data, use_model = TRUE )
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
             .interim = box( d$interim.x ), .boxes = boxes ) ]

  # ---- 2. one position per person ----------------------------------------------
  rank <- c( officer = 1L, board_officer = 2L, board = 3L, staff = 4L, unknown = 5L )
  d[ , .rank := rank[ .pos ] ]
  d[ , `:=`( .pmin = min(.rank),
             .primary = seq_len(.N) == which.min(.rank) ),
     by = .( object.id, person.id ) ]
  d[ , role.position := names(rank)[ .pmin ] ]
  # the CEO thesaurus (F-033): chief-executive titles by organization type.
  # A "ceo" entry marks a holder who runs that kind of organization (an
  # adjutant, a club's general manager); a "not_ceo" entry a holder who never
  # does on stipend-level pay (a lodge secretary, a post treasurer)
  ot <- org_types( comp.data )
  d[ ot, on = "object.id", .otype := i.org.type ]
  d[ , .v7 := toupper( chr( if( is.null( d$title.v7 ) ) d$title.raw else d$title.v7 ) ) ]
  d[ , `:=`( .th_ceo = FALSE, .th_not = FALSE, .th_out = NA_character_ ) ]
  thes <- get_ceo_thesaurus()
  if( ! is.null( thes ) && nrow( thes ) && any( ! is.na( d$.otype ) ) )
    for( i in seq_len( nrow( thes ) ) )
    {
      hit <- d$.otype %in% thes$org_type[i] & grepl( thes$title_pattern[i], d$.v7, perl = TRUE )
      if( ! any( hit ) ) next
      mp <- thes$min_pay[i]; if( is.na(mp) ) mp <- 0
      mh <- thes$min_hours[i]; if( is.na(mh) ) mh <- 0
      if( thes$effect[i] == "ceo" ) {
        # unreported hours (0) do not rule a paid holder out
        ok <- hit & d$.comp >= mp & ( d$.hrs >= mh | ( d$.hrs == 0 & d$.comp > 0 ) | mh == 0 )
        d[ ok, `:=`( .th_ceo = TRUE, .th_out = thes$outranks[i] ) ]
      } else {
        # for not_ceo, min_hours bounds a stipend: a holder working that much
        # (a full-time post secretary-treasurer) is not ruled out
        d[ hit & d$.comp < mp & ( mh == 0 | d$.hrs < mh ), .th_not := TRUE ]
      }
    }

  # person-level signals, from any of their rows
  d[ , `:=`( .p_paid = any(.paid), .p_comp = max(.comp), .p_hrs = max(.hrs),
             .p_ceo_t = any(.ceo_t), .p_exec = any(.exec), .p_board_t = any(.board_t),
             .p_lead = any(.lead), .p_off = any(.cb_off), .p_tru = any(.cb_tru),
             .p_pres = any(.pres), .p_interim = any(.interim),
             .p_th_ceo = any(.th_ceo), .p_th_not = any(.th_not) & ! any(.th_ceo),
             .p_th_out = .th_out[ .th_ceo ][1], .p_titles = paste( unique(.v7), collapse = " | " ) ),
     by = .( object.id, person.id ) ]

  # ---- 3. leadership, per filing (on one row per person) -----------------------
  p <- d[ .primary == TRUE ]
  # an unpaid executive director with no checkbox is still the executive
  # director: only a board-only position rules a CEO title out
  p[ , .desig := .p_ceo_t & role.position %in% c( "officer", "board_officer", "unknown" ) & ! .p_th_not ]
  p[ , .chair_pattern := .p_tru & .p_hrs < 25 & .p_pres ]
  # an imputed leader must do more than token work: 15+ hours a week or $25k+,
  # and, when hours are reported, at least 5 a week: pay for a seat held an
  # hour a week is a stipend or comes from a related organization (F-032).
  # A thesaurus "not_ceo" title is never imputed (F-033)
  p[ , .elig := .p_paid & ! .chair_pattern & ( .p_off | .p_lead ) & ! .p_th_not &
                ( .p_hrs >= 15 | .p_comp >= 25000 ) & ( .p_hrs >= 5 | .p_hrs == 0 | .p_pres ) ]
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
  # an unpaid full-time leader (labeling guide rule 7): when no one else is the
  # CEO, an unpaid president, chair or other leader working 35+ hours a week
  # with the officer box runs the organization (F-032)
  p[ , .no_ceo := ! any( ! is.na( role.ceo ) ), by = object.id ]
  p[ , .rule7 := .no_ceo & .boxes & ! .p_paid & .p_hrs >= 35 & .p_off & ( .p_lead | .p_pres ) ]
  # only one clear leader: when several candidates tie for the most hours (a
  # student group whose officers all report 50), no one is imputed
  if( any( p$.rule7 ) )
    p[ .rule7 == TRUE, .k7 := .p_hrs == max( .p_hrs ) & sum( .p_hrs == max( .p_hrs ) ) == 1, by = object.id ]
  else p[ , .k7 := FALSE ]
  p[ .rule7 & .k7 %in% TRUE, role.ceo := "imputed" ]
  # the CEO thesaurus (F-033): the best-paid holder of a "ceo" title for the
  # organization's type is the CEO when the filing has none, when its CEO was
  # only inferred from pay, or when the CEO's title is one the entry outranks
  # (a university president over a chancellor)
  th_apply <- function( pick, ceo, out, titles )
  {
    k <- which( pick ); cur <- which( ! is.na( ceo ) )
    if( ! length( k ) || k %in% cur ) return( FALSE )
    if( ! length( cur ) || all( ceo[cur] %in% c( "imputed", "model" ) ) ) return( TRUE )
    o <- out[k]
    ! is.na( o ) && nzchar( o ) && any( grepl( o, titles[cur], perl = TRUE ) )
  }
  p[ , .th_best := if( any( .p_th_ceo ) ) max( .p_comp[ .p_th_ceo ] ) else NA_real_, by = object.id ]
  p[ , .th_pick := .p_th_ceo & .p_comp == .th_best ]
  p[ .th_pick == TRUE, .kt := seq_len(.N), by = object.id ]
  p[ .th_pick & .kt > 1, .th_pick := FALSE ]
  p[ , .th_apply := th_apply( .th_pick, role.ceo, .p_th_out, .p_titles ), by = object.id ]
  p[ .th_apply & ! .th_pick, role.ceo := NA_character_ ]
  p[ .th_apply & .th_pick, role.ceo := "thesaurus" ]
  p[ , org.leadership := data.table::fcase(
          any( role.ceo %in% c( "designated", "co-ceo", "interim" ) ), "designated",
          any( role.ceo %in% "thesaurus" ),                             "thesaurus",
          any( role.ceo %in% "imputed" ),                               "imputed",
          .any_paid[1],                                                 "board_governed",
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
          # the same on a 990-EZ, which has no boxes to say "board": a paid
          # board title with a few hours and a small fee is a board seat
          role.position == "staff" & ! .boxes & .p_board_t &
            .p_hrs < 10 & .p_comp < 25000,                                "BOARD",
          role.position == "unknown" & .p_board_t,                        "BOARD",
          role.position == "unknown" & .p_exec,                           "OFFICER",
          .level %in% c( "MANAGER", "PROFESSIONAL", "STAFF" ),            .level,
          default = "STAFF" ) ]
  p[ , role.method := "rules" ]

  # ---- 5. 990-EZ: the no-checkbox role model (F-028, F-029) ----------------------
  # Without the boxes the rules miss most officers and over-call staff; the
  # model, trained on full-990 people with the boxes hidden, does better
  # (labeled 990-EZ people: 92.5% against 82.5%). It replaces role.final for
  # 990-EZ people when xgboost is installed.
  if( use_model && any( ! p$.boxes ) )
  {
    pred <- predict_roles( comp.data )
    if( ! is.null(pred) )
    {
      d[ , .model := pred[ .row ] ]
      p[ d[ .primary == TRUE ], on = c( "object.id", "person.id" ), .model := i..model ]
      # a CEO the model names on a 990-EZ must work 20+ hours a week, unless the
      # title names the CEO; otherwise it is the role the rules give (F-032)
      p[ ! .boxes & .model %in% "CEO" & ! .desig & .p_hrs < 20 & .p_comp < 50000,
           .model := data.table::fifelse( role.final == "CEO",
                       data.table::fifelse( .level %in% c( "MANAGER", "PROFESSIONAL", "STAFF" ), .level,
                         data.table::fifelse( .p_board_t, "BOARD", "STAFF" ) ), role.final ) ]
      ez <- ! p$.boxes
      p[ , .rules_final := role.final ]
      p[ ez, `:=`( role.final = .model, role.method = "model" ) ]
      # a CEO named by the model keeps a designated or co-CEO label from its
      # title, and is "model" otherwise; anyone else is not a CEO
      p[ ez, role.ceo := data.table::fifelse( role.final == "CEO",
                                              data.table::fcoalesce( role.ceo, "model" ), NA_character_ ) ]
      p[ ez & role.ceo %in% "imputed", role.ceo := "model" ]
      # the thesaurus also outranks the model's own pick (F-033)
      p[ ez, .th_apply := th_apply( .th_pick, role.ceo, .p_th_out, .p_titles ) | .th_apply, by = object.id ]
      p[ ez & .th_apply & ! .th_pick & role.final == "CEO" & ! role.ceo %in% c( "designated", "co-ceo", "interim" ),
         `:=`( role.final = data.table::fifelse( .rules_final == "CEO", "STAFF", .rules_final ), role.ceo = NA_character_ ) ]
      p[ ez & .th_apply & .th_pick, `:=`( role.final = "CEO", role.ceo = "thesaurus" ) ]
      p[ , org.leadership := data.table::fcase(
              any( role.ceo %in% c( "designated", "co-ceo", "interim" ) ), "designated",
              any( role.ceo %in% "thesaurus" ),                             "thesaurus",
              any( role.ceo %in% "imputed" ),                               "imputed",
              any( role.ceo %in% "model" ),                                 "model",
              .any_paid[1],                                                 "board_governed",
              default = "no_paid" ),
         by = object.id ]
    }
  }

  # the board role: the crosswalk's, or read from the title for an employee
  # title that turned out to be a board seat (VICE PRESIDENT, SECRETARY, ...)
  brank <- c( CHAIR = 1L, `VICE CHAIR` = 2L, `SECRETARY TREASURER` = 3L, SECRETARY = 4L, TREASURER = 5L, MEMBER = 6L )
  # an assistant secretary or treasurer on the board holds a seat, not the
  # office: the board keeps one secretary and one treasurer
  asst_office <- chr( d$title.standard ) %in% c( "ASSISTANT SECRETARY", "ASSISTANT TREASURER" )
  # a past or elected-but-not-yet-serving officer (PAST PRESIDENT, PRESIDENT
  # ELECT) holds a seat, not the office: step 06 flags them FORMER or FUTURE.
  # A flag that came with a date marks a change during the year (CHAIR THRU
  # 6/20, TREASURER SINCE 3/21): that person did hold the office, so it stays
  flagnum <- function(x) if( is.null(x) ) rep( 0, nrow(d) ) else { x <- suppressWarnings( as.numeric(x) ); x[ is.na(x) ] <- 0; x }
  not_seated <- ( flagnum( d$former.x ) > 0 | flagnum( d$future.x ) > 0 ) & flagnum( d$date.x ) == 0
  # an employee title that turned out to be a board seat keeps its office
  # (VICE PRESIDENT, SECRETARY). The office is read from the standard title; an
  # uncoded title counts only when it starts with the office (TREASURER &
  # TRUSTREE), so one naming another body's officer (SGA PRESIDENT, CHAPTER
  # PRESIDENT, MEDICAL STAFF PRESIDENT) is a plain seat (F-032)
  std <- toupper( chr( d$title.standard ) )
  v7  <- toupper( chr( if( is.null( d$title.v7 ) ) d$title.raw else d$title.v7 ) )
  lead_office <- "^(VICE (PRESIDENT|CHAIR)|PRESIDENT|CHAIR|SECRETARY|TREASURER)\\b"
  fb  <- data.table::fifelse( nzchar( std ), std,
           data.table::fifelse( grepl( lead_office, v7 ) & ! grepl( "DESIGNEE|ALUMNI", v7 ), v7, "" ) )
  d[ , .brole_t := data.table::fcase(
          not_seated,                                       "MEMBER",
          nzchar(.brole),                                   .brole,
          asst_office,                                      "MEMBER",
          grepl( "OF MEDICAL STAFF", fb ),                  "MEMBER",
          grepl( "VICE (PRESIDENT|CHAIR)", fb ),            "VICE CHAIR",
          grepl( "PRESIDENT|CHAIR", fb ),                   "CHAIR",
          grepl( "SECRETARY.*TREASURER|TREASURER.*SECRETARY", fb ), "SECRETARY TREASURER",
          grepl( "SECRETARY", fb ),                         "SECRETARY",
          grepl( "TREASURER", fb ),                         "TREASURER",
          default = "MEMBER" ) ]
  # a person whose title was split into a secretary row and a treasurer row
  # holds the combined office
  pb <- d[ , .( role.board = if( all( c( "SECRETARY", "TREASURER" ) %in% .brole_t ) && min( brank[.brole_t] ) >= 3L )
                               "SECRETARY TREASURER" else names(brank)[ min( brank[.brole_t] ) ] ),
           by = .( object.id, person.id ) ]
  p <- merge( p, pb, by = c( "object.id", "person.id" ), all.x = TRUE )
  p[ role.final != "BOARD", role.board := NA_character_ ]

  # ---- map back onto every row ---------------------------------------------------
  out <- merge( d[ , .( .row, object.id, person.id, role.position, role.source = .src,
                        role.primary = .primary ) ],
                p[ , .( object.id, person.id, role.final, role.board, role.ceo, role.method, org.leadership ) ],
                by = c( "object.id", "person.id" ), all.x = TRUE )
  data.table::setorder( out, .row )
  out[ ot, on = "object.id", `:=`( org.type = i.org.type, org.type.source = i.org.type.source ) ]

  res <- as.data.frame( comp.data )
  for( v in c( "role.position", "role.final", "role.board", "role.ceo",
               "role.source", "role.method", "role.primary", "org.leadership",
               "org.type", "org.type.source" ) )
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
