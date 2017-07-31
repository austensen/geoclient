#' Retrieve Geoclient Response for Well-Known NYC Places as a Dataframe
#'
#' This function takes the name of a well-known NYC place and returns the
#' Geoclient response as a tibble. The place name, and one of either borough
#' or Zip code are required. The place components can be provided either in
#' separate vectors as named arguments or with a dataframe and column names
#' containing each component. The Geoclient API's app ID and key
#' can either be provided directly as arguments, or you can first use
#' [geoclient_api_keys()] to add them to your `.Renviron` file so they can be
#' called securely without being stored in your code.
#'
#' @param df Dataframe that contains a column of BBLs. Defaults to `NULL` and
#'   `bbl` is taken as a vector.
#' @param place Either a vector of BBLs (numeric or character is accepted), or a
#'   bare column name of the bbl field if a dataframe is provided.
#' @param borough The name of the borough of the address, as either a vector
#'   or a bare column name of the borough field if a dataframe is provided.
#'   The borough is only required if Zip code is not provided.
#' @param zip The Zip code of the address, as either a vector (numeric or character
#'   is accepted) or a bare column name of the borough field if a dataframe
#'   is provided. Five- and seven-digit Zip codes are accepted. The Zip code is
#'   only required if borough is not provided.
#' @param id The API app ID provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and your key is accessed from your
#'   `.Renviron`.
#' @param key The API app key provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and your key is accessed from your
#'   `.Renviron`.
#' @param rate_limit Whether you would like to limit the rate of API requests in
#'   adherence to Geoclient's Service Usage Guidelines. See `?geoclient` for
#'   more information.
#' @param cap_daily_requests Whether you would like to cap the daily number of
#'   API requests in adherence to Geoclient's Service Usage Guidelines. See
#'   `?geoclient` for more information.
#'
#' @details For more details see the Geoclient Documentation's guide to
#'   [making address requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.1),
#'   interpreting the [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2),
#'   the [data returned by `geoclient_address`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.1),
#'   and a [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0)
#'   for all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geoclient_place(place = "empire state building", borough = "mn")
#'
#' library(tibble)
#' library(dplyr)
#'
#' df <- tribble(
#'   ~place,         ~boro,         ~zip,
#'   "NYU",          NA_character_, "10012",
#'   "CITY HALL",    "MN",          NA_character_,
#'   "Pospect Park", "Brooklyn",    NA_character_
#' )
#'
#' geoclient_place(df, place, boro, zip)
#'
#' df %>%
#'   mutate(
#'     bbl = geoclient_place(place = place, borough = boro, zip = zip)[["bbl"]]
#'   )
#' }
#'
#' @export

geoclient_place <- function(df = NULL,
                            place,
                            borough = NULL,
                            zip = NULL,
                            id = NULL,
                            key = NULL,
                            rate_limit = TRUE,
                            cap_daily_requests = TRUE) {

  # Get Geoclient App ID and Key (either from .Renviron or arguments)
  creds <- get_credentials(id, key)

  # If a dataframe is provided, get the vectors from there, otherwise just use input vectors
  # If borough or zip are not provided, fill with NAs
  if (!is_null(df)) {

    if (!is.data.frame(df)) {
      stop_glue("If a dataframe is not given as the first argument, the other arguments must be named")
    }

    place <- enquo(place)
    borough <- enquo(borough)
    zip <- enquo(zip)

    place <- dplyr::pull(df, !!place)

    len <- length(place)

    missing_borough <- quo_is_null(borough)
    missing_zip <- quo_is_null(zip)

    if (missing_borough && missing_zip) {
      stop_glue("One of either borough or zip must be provided")
    }

    if (missing_borough) {
      borough <- rep(NA_character_, len)
    } else {
      borough <- dplyr::pull(df, !!borough)
    }

    if (missing_zip) {
      zip <- rep(NA_character_, len)
    } else {
      zip <- dplyr::pull(df, !!zip)
    }

  } else {

    len <- length(place)

    if (is_null(borough) && is_null(zip)) {
      stop_glue("One of either borough or zip must be provided")
    }

    if (is_null(borough)) {
      borough <- rep(NA_character_, len)
    }

    if (is_null(zip)) {
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

  place_inputs <- tibble::tibble(
    name = place,
    borough = borough,
    zip = zip
  )

  res <- make_requests(
    inputs = place_inputs,
    operation = "place",
    creds = creds,
    rate_limit = rate_limit,
    cap_daily_requests = cap_daily_requests
  )

  res <- dplyr::rename(res, "place" = "name")

  res
}
