#' Retrieve Dataframe Response from Geoclient for Any Location Type
#'
#' This function takes any of the 6 location types as a single input and returns
#' the Geoclient response as a tibble. The locations are provided either in a
#' single vector as a named argument or with a dataframe and column name of the
#' location field. The Geoclient API's app ID and key can either be provided
#' directly as arguments, or you can first use [geoclient_api_keys()] to add
#' them to your `.Renviron` file so they can be called securely without being
#' stored in your code.
#'
#' @param df Dataframe that contains a column of BBLs. Defaults to `NULL` and
#'   `bbl` is taken as a vector.
#' @param location Any of the 6 locations types from other functions: Address,
#'   BBL, BIN, Blockface, Intersection, or Place. The argument can be either a
#'   single vector of locations, or a bare column name of the location field if
#'   a dataframe is provided.
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
#' @details Geoclient is quite flexible with the format of the address when
#'   provided as a single input for this function, however the results may be
#'   slower than the more restrictive [geoclient_address()] because Geoclient
#'   make have to make multiple requests to narrow in on the correct location.
#'
#'   For more details see the Geoclient Documentation's guide to
#'   [single-input requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.3),
#'   interpreting the [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2), and a
#'   [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0) for
#'   all possible data elemets returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geoclient_search(location = 1005430053)
#' geoclient_search(location = "139 macdougal st mn")
#' geoclient_search(location = c("1008760", "1007941"))
#'
#' library(dplyr)
#'
#' df <- tibble(loc = c("1005430053", "139 MacDougal Street, New York, 10012", "1008760"))
#'
#' geoclient_search(df, loc)
#' }
#'
#' @export

geoclient_search <- function(df = NULL,
                             location,
                             id = NULL,
                             key = NULL,
                             rate_limit = TRUE,
                             cap_daily_requests = TRUE) {

  # Temporarily change option to prevent scientific notation when coercing double to character
  op <- options(scipen = 999)
  on.exit(options(op))

  # Get Geoclient App ID and Key (either from .Renviron or arguments)
  creds <- get_credentials(id, key)

  # If a dataframe is provided, get the vector from there, otherwise just use input vector
  if (!is.null(df)) {
    if (!is.data.frame(df)) {
      stop_glue("If a dataframe is not given as the first argument, the location argument must be named")
    }

    location <- enquo(location)
    location <- dplyr::pull(df, !!location)
  }

  location <- as.character(location)

  location_inputs <- tibble::tibble(
    input = location
  )

  res <- make_requests(
    inputs = location_inputs,
    operation = "search",
    creds = creds,
    rate_limit = rate_limit,
    cap_daily_requests = cap_daily_requests
  )

  res <- dplyr::rename(res, location = input)

  res
}

