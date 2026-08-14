# library(dplyr)
# library(furrr)
# library(future)

rebatch <- function( lst, n = 10 ) {
  split( lst, ceiling( seq_along(lst) / n ))
}

process_chunk <- function( chunk, 
                           batch_id, total_batches,
                           gs_status_codes=NULL, 
                           gs_title_xwalk=NULL, 
                           gs_title_taxonomy=NULL) {

  df <- dplyr::bind_rows(chunk)
  
  capture.output({
  
    dd <-
      df %>%
      standardize_df() %>%
      remove_dates() %>%
      standardize_conj() %>%
      split_titles() %>%
      standardize_spelling() %>%
      gen_status_codes( gs_status_codes=gs_status_codes ) %>%
      standardize_titles( gs_title_xwalk=gs_title_xwalk ) %>%
      categorize_titles( gs_title_taxonomy=gs_title_taxonomy )
      
  })
  
  # report progress
  message(sprintf("batch %d of %d complete", batch_id, total_batches))
  
  return(dd)
}

#' @title
#' Classify Part VII titles in parallel
#'
#' @description
#' Runs the full title-cleaning and classification pipeline over a Part VII
#' compensation data frame, batching by filing (`OBJECTID`) across worker
#' processes. Returns one row per person-title.
#'
#' @details
#' By default the returned frame uses the pipeline's standardized (renamed,
#' cleaned, and pruned) columns. Set `preserve_input = TRUE` to instead return
#' the **original input frame untouched** — every original column kept verbatim
#' in name and value, including columns the pipeline would normally rename or
#' drop — with the classification fields appended. Because a multi-title person
#' expands into multiple rows, each original row is repeated once per title.
#'
#' @export
#' @param df A Part VII compensation data frame.
#' @param batch_size Number of filings per worker batch.
#' @param workers Number of parallel workers; defaults to half the cores.
#' @param preserve_input If `TRUE`, return the original columns intact with the
#'   classification fields appended (joined by a synthetic row key). If `FALSE`
#'   (default), return the pipeline's standardized columns.
classify_titles <- function( df, batch_size = 2000, workers = NULL,
                             preserve_input = FALSE ) {

  if( is.null(workers) ){ workers = parallel::detectCores() / 2 }

  # When preserving the input, stamp a collision-free row key on the pristine
  # frame before it enters the (row-expanding, column-renaming) pipeline. The
  # key rides along through every step and lets us map each classified row back
  # to its source row, so the original columns can be returned untouched.
  if( preserve_input ) {
    original <- as.data.frame( df )
    if( ".tc_row_id" %in% names(original) ) {
      stop( "classify_titles(): input already has a '.tc_row_id' column; ",
            "rename it before using preserve_input = TRUE." )
    }
    original$.tc_row_id <- seq_len( nrow(original) )
    df <- original
  }

  groups <- df %>%
    dplyr::group_split(OBJECTID)
  
  batches <- rebatch(groups, n = batch_size)
  total_batches <- length(batches)
  
  if (.Platform$OS.type == "windows") {
    future::plan(future::multisession, workers = workers)
  } else {
    future::plan(future::multicore, workers = workers)
  }
  
  # Load title assets from google sheets 
  gs_status_codes   <- get_googlesheets_status_codes()
  gs_title_xwalk    <- get_googlesheets_title_xwalk()
  gs_title_taxonomy <- get_googlesheets_title_taxonomy()
  
  # Pass them to each worker
  # results <- furrr::future_map(
  #   batches,
  #   process_chunk,
  #   gs_status_codes   = gs_status_codes,
  #   gs_title_xwalk    = gs_title_xwalk,
  #   gs_title_taxonomy = gs_title_taxonomy,
  #   .progress = TRUE
  # )
  
  # pass to workers with batch number
  results <- furrr::future_map2(
    batches,
    seq_along(batches),
    ~ process_chunk(.x,
                    batch_id = .y,
                    total_batches = total_batches,
                    gs_status_codes   = gs_status_codes,
                    gs_title_xwalk    = gs_title_xwalk,
                    gs_title_taxonomy = gs_title_taxonomy),
    .progress = FALSE
  )  

  final_df <- dplyr::bind_rows(results)

  if( preserve_input ) {
    final_df <- append_classification( original, final_df )
  }

  return(final_df)
}


#' @title
#' Append classified fields to the original (untouched) input frame
#'
#' @description
#' Joins the titleclassifier-derived columns back onto the pristine input
#' frame by the `.tc_row_id` key, returning every original column verbatim
#' (values, names, and columns the pipeline would otherwise rename or drop)
#' with the new classification fields appended. Because title splitting
#' expands rows one-to-many, an original row is repeated once per classified
#' title.
#'
#' @param original The pristine input frame carrying a `.tc_row_id` key.
#' @param classified The pipeline output carrying the same `.tc_row_id` key.
#' @keywords internal
append_classification <- function( original, classified ) {

  if( ! ".tc_row_id" %in% names(classified) ) {
    stop( "append_classification(): classified output is missing '.tc_row_id'; ",
          "the join key did not survive the pipeline." )
  }

  # Every pipeline column not already present in the original is a derived
  # field (the pipeline renames the originals, so their cleaned/renamed
  # versions surface here as new columns alongside the genuinely new ones).
  derived_cols <- setdiff( names(classified), names(original) )
  derived_cols <- setdiff( derived_cols, ".tc_row_id" )

  result <-
    dplyr::left_join(
      original,
      classified[ c(".tc_row_id", derived_cols) ],
      by = ".tc_row_id"
    )

  result$.tc_row_id <- NULL
  return( as.data.frame(result) )
}
