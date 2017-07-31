
# Depending on value of `rate_limit` uses appropriate function to make API
# requests, passing on the arguments `operation`, `creds`,
# `cap_daily_requests`, and `pb` to the limited/unlimited functions
make_requests <- function(inputs, ...) {

  rate_limit <- list(...)[["rate_limit"]]

  if (rate_limit == TRUE) {
    ret <- make_rate_limited_requests(inputs, ...)
  } else if (rate_limit == FALSE) {
    ret <- make_unlimited_requests(inputs, ...)
  }

  ret
}

# Limits API requests to 2,500 per minute in adherence to Geoclient's Service
# Usage Guidelines. The arguments  `operation`, `creds`,
# `cap_daily_requests`, and `pb` are passed on to make_unlimited_requests()
make_rate_limited_requests <- function(inputs, ..., chunk_size = 2500, duration_hms = "00:01:00") {

  # Transform the single dataframe into a list of dataframes each with no more than 2500 rows
  chunked_inputs <- split(inputs, ceiling(seq_len(nrow(inputs)) / chunk_size))
  chunked_inputs <- set_names(chunked_inputs, NULL)

  n_chunks <- length(chunked_inputs)

  duration_hms <- hms::as.hms(duration_hms)

  # TODO: change to imap_dfr() when new purrr released
  purrr::map2_df(chunked_inputs, seq_along(chunked_inputs), function(.x, .y) {

    start_time <- lubridate::now()

    ret <- make_unlimited_requests(.x, ...)

    if (.y == n_chunks) {
      return(ret)
    }

    time_passed <- hms::as.hms(lubridate::now() - start_time)

    time_to_wait <- duration_hms - time_passed

    if (time_to_wait > 0) {
      Sys.sleep(time_to_wait)
    }

    ret
  })
}


# Makes API requests without rate limiting, passing the arguments `operation`,
# `creds`, `cap_daily_requests`, and `pb` to make_single_request(). The main
# purpose of this funciton is to vectorize make_single_request() by interating
# over each row of the input dataframe.

# Inputs: a dataframe wherein each column corresponds to a API query parameter
# Returns: a dataframe containing the API response

make_unlimited_requests <- function(inputs, ...) {

  # To avoid sending multiple requests for the same address, preserve original
  # inputs, use deduplicated version for request, then join the respone back to
  # original inputs before returning final result

  var_names <- names(inputs)

  inputs_dedup <- dplyr::distinct(inputs)

  n_requests <- nrow(inputs_dedup)

  cap_daily_requests <- list(...)[["cap_daily_requests"]]

  if (n_requests > 500000 && cap_daily_requests == TRUE) {
    stop_glue(
      "The required number of API requests exxceed Geoclient's Service Usage Guidelines of maximum 500,000 per day.
      To ignore this daily maximum set `cap_daily_requests = FALSE`.
      See ?geoclient for more information."
    )
  }

  pb <- dplyr::progress_estimated(n_requests)

  operation <- list(...)[["operation"]]
  creds <- list(...)[["creds"]]

  ret <- purrr::pmap_df(
    inputs_dedup,
    make_single_request,
    operation = operation,
    creds = creds,
    pb = pb
  )

  ret <- dplyr::bind_cols(inputs_dedup, ret)

  ret <- dplyr::left_join(inputs, ret, by = var_names)

  ret
}


# Makes a single API request (Geoclient does not support a single request for multiple addres/bbls/bins/etc.).

# Inputs: takes a length-1 vector for each of the API query parameters
# Returns: API response as a dataframe
make_single_request <- function(..., operation, creds, pb = NULL) {

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

  auth_failed <- suppressWarnings(httr::content(resp)[[1]] == "Authentication failed")

  if (is_true(auth_failed)) {
    stop_glue(
      "Authentication failed: Geoclient API app ID and/or Key are invalid.
      See ?geoclient_api_keys for details on how to aquire valid credentials."
    )
  }

  httr::stop_for_status(resp)

  if (operation == "search") {
    parsed <- content_as_json_UTF8(resp)[["results"]][["response"]]
  } else {
    parsed <- content_as_json_UTF8(resp)[[operation]]
  }

  tibble::as_tibble(parsed)
}



get_credentials <- function(id = NULL, key = NULL) {

  # TODO: is there a way to validate credentials before making main request?
  #       see tests/testthat/test-tests.R
  #       not sure it makes sense to do this every time
  #       is there a way to only do it once per session?
  #       should it at least be checked in geoclient_api_keys()?

  # TODO: is it too annoying to remind the user about stores api keys every time?
  #       is there a way to only do it once per session?
  #       maybe just make a startup-message?

  if (!is_null(id) && !is_null(key)) {

    msg_glue(
      "To avoid entering your Geoclient API app ID and key with each call and saving them in your code, ",
      "you can use `geoclient_api_keys` to store them for future use.
      See ?geoclient_api_keys for details."
    )

  } else if (Sys.getenv("GEOCLIENT_APP_ID") != "" || Sys.getenv("GEOCLIENT_APP_KEY") != "") {

    id <- Sys.getenv("GEOCLIENT_APP_ID")
    key <- Sys.getenv("GEOCLIENT_APP_KEY")

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


clean_borough <- function(borough) {
  dplyr::case_when(
    stringr::str_detect(borough, "^(?i)(mn)|(new york)") ~ "manhattan",
    stringr::str_detect(borough, "^(?i)(bx)")            ~ "bronx",
    stringr::str_detect(borough, "^(?i)(bk)|(kings)")    ~ "brooklyn",
    stringr::str_detect(borough, "^(?i)(qn)")            ~ "queens",
    stringr::str_detect(borough, "^(?i)(si)(richmond)")  ~ "staten island",
    TRUE ~ borough
  )
}
