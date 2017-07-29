#' Retrieve Dataframe Response from Geoclient for Addresses
#'
#' This function takes components of addresses and returns the Geoclient
#' response as a tibble. The house number, street name, and one of either
#' borough or zipcode are required. The address components can be provided
#' either in separate vectors as a named arguments or with a dataframe and
#' column names containing each component. The Geoclient API's app ID and key
#' can either be provided directly as arguments, or you can first use
#' [geoclient_api_keys()] to add them to your `.Renviron` file so they can be
#' called securely without being stored in your code.
#'
#' @param df Dataframe that contains a column of BBLs. Defaults to `NULL` and
#'   `bbl` is taken as a vector.
#' @param bbl Either a vector of BBLs (numeric or character is accepted), or a
#'   bare column name of the bbl field if a dataframe is provided.
#' @param id The API app ID provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and your key is accessed from your
#'   `.Renviron`.
#' @param key The API app key provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and your key is accessed from your
#'   `.Renviron`.
#' @param rate_limit Wether you would like to limit the rate of API requests in
#'   adherence to Geoclient's Service Usage Guidelines. See `?geoclient` for
#'   more information.
#' @param cap_daily_requests Wether you would like to cap the daily number of
#'   API requests in adherence to Geoclient's Service Usage Guidelines. See
#'   `?geoclient` for more information.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geoclient_address(
#'   number = "139",
#'   street = "MacDougal St",
#'   borough = "manhattan",
#'   zip = "11231"
#' )
#'
#' library(tibble)
#' library(dplyr)
#'
#' df <- tribble(
#'   ~num,  ~st,                ~boro,         ~zip,
#'   "139", "MacDougal St",     "manhattan",   "11231",
#'   "295", "Lafayette street", NA_character_, "10012-2722",
#'   "40",  "WASHINGTON SQ S",  "MN",          NA_character_
#' )
#'
#' geoclient_address(df, num, st, boro, zip)
#'
#' df %>%
#'   mutate(
#'     bbl = geoclient_address(number = num, street = st, borough = boro, zip = zip)[["bbl"]]
#'   )
#' }
#'
#' @export

geoclient_address <- function(df = NULL,
                              number,
                              street,
                              borough = NULL,
                              zip = NULL,
                              id = NULL,
                              key = NULL,
                              rate_limit = TRUE,
                              cap_daily_requests = TRUE) {

  # Temporarily change option to prevent scientific notation when coercing double to character
  op <- options(scipen = 999)
  on.exit(options(op))

  # Get Geoclient App ID and Key (either from .Renviron or arguments)
  creds <- get_credentials(id, key)

  # If a dataframe is provided, get the vectors from there, otherwise just use input vectors
  # If borough or zip are not provided, fill with NAs
  if (!is.null(df)) {

    if (!is.data.frame(df)) {
      stop_glue("If a dataframe is not given as the first argument, the other arguments must be named")
    }

    number <- enquo(number)
    street <- enquo(street)
    borough <- enquo(borough)
    zip <- enquo(zip)

    number <- dplyr::pull(df, !!number)
    street <- dplyr::pull(df, !!street)

    len <- length(number)

    if (quo_is_null(borough)) {
      borough <- rep(NA_character_, len)
    } else {
      borough <- dplyr::pull(df, !!borough)
    }

    if (quo_is_null(zip)) {
      zip <- rep(NA_character_, len)
    } else {
      zip <- dplyr::pull(df, !!zip)
    }

  } else {

    len <- length(number)

    if (is.null(borough)) {
      borough <- rep(NA_character_, len)
    }

    if (is.null(zip)) {
      zip <- rep(NA_character_, len)
    }
  }

  borough <- dplyr::case_when(
    stringr::str_detect(borough, "^(?i)(mn)|(new york)") ~ "manhattan",
    stringr::str_detect(borough, "^(?i)(bx)")            ~ "bronx",
    stringr::str_detect(borough, "^(?i)(bk)|(kings)")    ~ "brooklyn",
    stringr::str_detect(borough, "^(?i)(qn)")            ~ "queens",
    stringr::str_detect(borough, "^(?i)(si)(richmond)")  ~ "staten island",
    TRUE ~ borough
  )

  address_inputs <- tibble::tibble(
    houseNumber = number,
    street = street,
    borough = borough,
    zip = zip
  )

  res <- make_requests(
    inputs = address_inputs,
    operation = "address",
    creds = creds,
    rate_limit = rate_limit,
    cap_daily_requests = cap_daily_requests
  )

  res
}

