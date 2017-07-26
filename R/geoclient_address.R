#' Retrieve Dataframe Response from Geoclient for Addresses
#'
#' This function takes BBLs (borough-block-lot) and returns Geoclient response
#' in a tibble. The BBLs can be provided either in a vector as a named argument
#' or as a dataframe and column name of the BBL field. The Geoclient API's app
#' ID and key can either be provided directly as arguments, or you can first use
#' [geoclient_api_keys()] to add them to your `.Renviron` file so they can be
#' called securely without being stored in your code. You can acquire your
#' Geoclient app ID and Key by first registering with the
#' [https://developer.cityofnewyork.us/user/register?destination=api](NYC
#' Developer Portal) at, then
#' [https://developer.cityofnewyork.us/create/project](create a new project),
#' selecting "Geoclient v1" from available APIs.
#'
#' @param df Dataframe that contains a column of BBLs. Defaults to `NULL` and
#'   `bbl` is taked as a vector.
#' @param bbl Either a vector of BBLs (numeric or character is accepted), or a
#'   bare column name of the bbl field if a dataframe is provided.
#' @param id The API app ID provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and your key is accessed from your
#'   `.Renviron`.
#' @param key The API app key provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and your key is accessed from your
#'   `.Renviron`.
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh", install = TRUE)
#' # First time, reload your environment so you can use the keys without restarting R.
#' readRenviron("~/.Renviron")
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
#' addr <- tribble(
#'   ~num,  ~st,                ~boro,         ~zip,
#'   "139", "MacDougal St",     "manhattan",   "11231",
#'   "295", "Lafayette street", NA_character_, "10012-2722",
#'   "40",  "WASHINGTON SQ S",  "MN",          NA_character_
#' )
#'
#' geoclient_address(addr, num, st, boro, zip)
#'
#' addr %>%
#'   mutate(
#'     bbl = geoclient_address(number = num, street = st, borough = boro, zip = zip)[["bbl"]]
#'   )
#' }
#'
#' @export

geoclient_address <- function(df = NULL, number, street, borough = NULL, zip = NULL, id = NULL, key = NULL) {

  # Temporarily change option to prevent scientific notation when coercing double to character
  op <- options(scipen = 999)
  on.exit(options(op))

  if (Sys.getenv('GEOCLIENT_APP_ID') != '' || Sys.getenv('GEOCLIENT_APP_KEY') != '') {

    id <- Sys.getenv('GEOCLIENT_APP_ID')
    key <- Sys.getenv('GEOCLIENT_APP_KEY')

  } else if (is_null("id") || is_null("key")) {

    stop_glue(
      "A Geoclient app ID and key are required.
      Obtain them at https://developer.cityofnewyork.us/user/register?destination=api,",
      "and then supply them to the `geoclient_api_keys` function to avoid entering them with each call."
    )
  }

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

  res <- vectorized_requests(
    inputs = address_inputs,
    operation = "address",
    id = id,
    key = key
  )

  res
}

