
# Takes dataframe of inputs, filters out duplicates and invalid rows to limit
# the number of API requests and avoid errors, applies rate-limiting if
# necessary, then iterates over rows in input dataframe making requests and
# stacking all the 1-row results, then the results are joined back to the
# original inputs so that the final return dataframe has the same number of rows
# in the same order and contains the input columns (with the names of the
# geo_*() arguments rather than API names).

geoclient_reqs <- function(inputs, operation, creds, rate_limit) {

  if (!(is_logical(rate_limit, 1L))) stop_glue("`rate_limit` must be either TRUE or FALSE")

  if (rate_limit) {
    geoclient_req <- ratelimitr::limit_rate(geoclient_req, ratelimitr::rate(n = 2500, period = 60))
  }

  inputs_dedup <- inputs %>% dplyr::distinct() %>% drop_invalid_rows(operation)

  if (nrow(inputs_dedup) == 0) {
    all_invalid <- inputs %>%
      dplyr::mutate(!!"no_results" := TRUE) %>%
      fix_input_names(operation)

    return(all_invalid)
  }

  pb <- progress::progress_bar$new(total = nrow(inputs_dedup))

  ret <- purrr::pmap_dfr(
    inputs_dedup,
    geoclient_req,
    operation = operation,
    creds = creds,
    pb = pb
  )

  inputs_dedup %>%
    fix_input_names(operation) %>%
    dplyr::bind_cols(ret) %>%
    dplyr::right_join(
      fix_input_names(inputs, operation),
      by = names(fix_input_names(inputs, operation))
    ) %>%
    dplyr::mutate(!!"no_results" := replace_na(!!sym("no_results"), TRUE)) # Rows dropped by drop_invalid_rows()
}


# Makes a single API request (Geoclient does not support a single request for
# multiple input locations).

# Inputs: takes a length-1 vector for each of the API query parameters
# Returns: API response as a dataframe
geoclient_req <- function(..., operation, creds, pb = NULL) {

  if (!is_null(pb) && !pb$finished) pb$tick()

  # Build query param list, removing element if NA (eg. address borough/zip)
  params <- purrr::splice(..., creds) %>% purrr::discard(is_na)

  resp <- rGET(
    glue::glue("https://api.cityofnewyork.us/geoclient/v1/{operation}.json?"),
    httr::accept_json(),
    query = params
  )

  auth_failed <- try(httr::content(resp)[[1]][[1]] == "Authentication failed", silent = TRUE)

  if (is_true(auth_failed)) {
    stop_glue(
      "Authentication failed: Geoclient API app ID and/or Key are invalid.
      See ?geoclient_api_keys for details on how to aquire valid credentials."
    )
  }

  # Sometimes bad inputs can cause 500 Internal Server Error. I think for these
  # it should just return the empty tibble like when returns null.
  geoclient_stop_for_status(resp)

  if (httr::status_code(resp) %in% c(400, 500)) {
    return(dplyr::tibble(no_results = TRUE))
  }

  if (operation == "search") {
    parsed <- content_as_json_UTF8(resp)[["results"]][["response"]]
  } else {
    parsed <- content_as_json_UTF8(resp)[[operation]]
  }

  # If these is no response (bad inputs, but request executed normally) return a
  # 1-row tibble with column indicating the issue
  placeholder <- dplyr::tibble(no_results = is_empty(parsed))

  if (is_empty(parsed)) {
    return(placeholder)
  }

  # Sometimes, for no clear reason, it will return two rows for one request with
  # no important differences, so slice one row
  # TODO: Look into this more. I don't remember but this could have to do with
  # single-input-search returning multiple possible matches
  parsed <- dplyr::as_tibble(parsed) %>% dplyr::slice(1)

  dplyr::bind_cols(placeholder, dplyr::as_tibble(parsed))
}

# For the geoclient API request we rename the inputs, but for the return
# dataframe we want to use the R function argument names for consistency. These
# are always at the beginning of the dataframe.
fix_input_names <- function(.data, operation) {

  # All other operation the number of user args and api inputs are the same,
  # except for BBL which gets split. So concatenate it back together
  if (operation == "bbl") {
    ret <- .data %>%
      dplyr::mutate(!!"input_bbl" := stringr::str_c(!!sym("borough"), !!sym("block"), !!sym("lot"))) %>%
      dplyr::select(-(1:3)) %>%
      dplyr::select(!!sym("input_bbl"), dplyr::everything())

    return(ret)
  }

  input_names <- switch(
    operation,
    address = c("house_number", "street", "borough", "zip"),
    bbl = "bbl",
    bin = "bin",
    blockface = c("on_street", "cross_street_1", "cross_street_2", "borough"),
    intersection = c("cross_street_1", "cross_street_2", "borough"),
    place = c("place", "borough", "zip"),
    search = "location"
  )

  colnames(.data)[seq_along(input_names)] <- paste0("input_", input_names)

  .data
}

# If their are mandatory inputs that are missing the API will return either
# nothing or raise http errors. Either way it's not useful to make these
# requests, so we drop all these rows before making the requests and join back
# the responses with the original inputs so the final dataframe returned to the
# user has the same number of rows as the inputs
drop_invalid_rows <- function(.data, operation) {

  mandatory_vars <- switch(operation,
    address = c("houseNumber", "street"),
    bbl = c("borough", "block", "lot"),
    bin = "bin",
    blockface = c("onStreet", "crossStreetOne", "crossStreetTwo", "borough"),
    intersection = c("crossStreetOne", "crossStreetTwo", "borough"),
    place = "name",
    search = "input"
  )

  ret <- .data %>% dplyr::filter_at(mandatory_vars, dplyr::all_vars(!are_na(.)))

  # Borough and zip can't both be NA
  if (operation %in% c("address", "place")) {
    ret <- ret %>% dplyr::filter(!(are_na(!!sym("borough")) & are_na(!!sym("zip"))))
  }

  ret
}

replace_na <- function(x, replace) {
  dplyr::if_else(are_na(x), replace, x)
}
