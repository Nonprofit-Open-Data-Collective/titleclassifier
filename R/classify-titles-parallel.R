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

classify_titles <- function( df, batch_size = 2000, workers = NULL ) {
  
  if( is.null(workers) ){ workers = parallel::detectCores() / 2 }

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
  return(final_df)
}
