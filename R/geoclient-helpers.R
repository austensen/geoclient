
vectorized_requests <- function(inputs, operation, id, key) {

  # To avoid sending multiple requests for the same address, preserve original
  # inputs, use deduplicated version for request, then join the respone back to
  # original inputs before returning final result

  var_names <- names(inputs)

  inputs_dedup <- dplyr::distinct(inputs)

  pb <- dplyr::progress_estimated(nrow(inputs_dedup))

  # TODO: incorporate rate limiting, 2500/min
  res <- purrr::pmap_df(
    inputs_dedup,
    single_request,
    operation = operation,
    id = id,
    key = key,
    pb = pb
  )

  res <- dplyr::bind_cols(inputs_dedup, res)

  res <- dplyr::left_join(inputs, res, by = var_names)

  res
}


single_request <- function(..., operation, id, key, pb = NULL) {

  # Build the list of query elements, then if an element is NA, remove it entirely (eg. for address borough/zip)
  params <- purrr::splice(..., app_id = id, app_key = key)
  params <- purrr::discard(params, is.na)

  # For creating a progress bar
  # Thanks to Bob Rudis' post: https://rud.is/b/2017/05/05/scrapeover-friday-a-k-a-another-r-scraping-makeover/
  if (!is_null(pb) && (pb$n > 10)) pb$tick()$print()

  resp <- rGET(
    glue::glue("https://api.cityofnewyork.us/geoclient/v1/{operation}.json?"),
    httr::accept_json(),
    query = params
  )

  httr::stop_for_status(resp)

  parsed <- content_as_json_UTF8(resp)[[operation]]

  tibble::as_tibble(parsed)
}

