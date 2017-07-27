

get_credentials <- function(id = NULL, key = NULL) {

  # TODO: is there a way to validate credentials before making main request?
  #       see tests/testthat/test-tests.R
  #       not sure it makes sense to do this every time
  #       is there a way to only do it once per session?
  #       should it at least be checked in geoclient_api_keys()?

  # TODO: is it too annoying to remind the user about stores api keys every time?
  #       is there a way to only do it once per session?

  if (!is_null(id) && !is_null(key)) {

    msg_glue(
      "To avoid entering your Geoclient API app ID and key with each call and saving them in your code, ",
      "you can use `geoclient_api_keys` to store them for future use.
      See ?geoclient_api_keys for details."
    )

  } else if (Sys.getenv('GEOCLIENT_APP_ID') != '' || Sys.getenv('GEOCLIENT_APP_KEY') != '') {

    id <- Sys.getenv('GEOCLIENT_APP_ID')
    key <- Sys.getenv('GEOCLIENT_APP_KEY')

  } else {

    stop_glue(
      "A Geoclient API app ID and key are required.
      Obtain them at https://developer.cityofnewyork.us/user/register?destination=api",
      "You can then use `geoclient_api_keys` to store them for future use.
      See ?geoclient_api_keys for details."
    )
  }

  creds <- list(id = id, key = key)

  creds
}


make_requests <- function(inputs, operation, creds) {

  # To avoid sending multiple requests for the same address, preserve original
  # inputs, use deduplicated version for request, then join the respone back to
  # original inputs before returning final result

  var_names <- names(inputs)

  inputs_dedup <- dplyr::distinct(inputs)

  pb <- dplyr::progress_estimated(nrow(inputs_dedup))

  # TODO: incorporate rate limiting, 2500/min
  res <- purrr::pmap_df(
    inputs_dedup,
    make_request,
    operation = operation,
    creds = creds,
    pb = pb
  )

  res <- dplyr::bind_cols(inputs_dedup, res)

  res <- dplyr::left_join(inputs, res, by = var_names)

  res
}


make_request <- function(..., operation, creds, pb = NULL) {

  # Build the list of query elements
  params <- purrr::splice(..., app_id = creds[["id"]], app_key = creds[["key"]])

  # If an element is NA, remove it entirely (eg. for address borough/zip)
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

  if (parsed[[1]] == "Authentication failed") {
    stop_glue(
      "Authentication failed: Geoclient API app ID and/or Key are invalid.
      See ?geoclient_api_keys for details on how to aquire valid credentials."
    )
  }

  tibble::as_tibble(parsed)
}




