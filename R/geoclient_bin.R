#' Retrieve Geoclient Response for BINs as a Dataframe
#'
#' This function takes BINs (Building Identification Number) and returns the
#' Geoclient response as a tibble. The BINs can be provided either in a vector
#' as a named argument or with a dataframe and column name of the BIN field. The
#' Geoclient API's app ID and key can either be provided directly as arguments,
#' or you can first use [geoclient_api_keys()] to add them to your `.Renviron`
#' file so they can be called securely without being stored in your code.
#'
#' @param df Dataframe that contains a column of BINs. Defaults to `NULL` and
#'   `bin` is taken as a vector.
#' @param bbl Either a vector of BINs (numeric or character is accepted), or a
#'   bare column name of the bin field if a dataframe is provided.
#' @param id The API app ID provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and the id is accessed from your
#'   `.Renviron`.
#' @param key The API app key provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and the key is accessed from your
#'   `.Renviron`.
#' @param rate_limit Whether you would like to limit the rate of API requests in
#'   adherence to Geoclient's Service Usage Guidelines. See `?geoclient` for
#'   more information.
#' @param cap_daily_requests Whether you would like to cap the daily number of
#'   API requests in adherence to Geoclient's Service Usage Guidelines. See
#'   `?geoclient` for more information.
#'
#' @details For more details see the Geoclient Documentation's guide to
#'   [making BIN requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.3),
#'   interpreting the [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2), the
#'   [data returned by `geoclient_bin`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.3),
#'   and a [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0) for
#'   all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geoclient_bin(bin = 1015862)
#' geoclient_bin(bin = c("1008760", "1007941"))
#'
#' library(dplyr)
#'
#' df <- tibble(BIN = c("1008760", "1007941"))
#'
#' geoclient_bin(df, BIN)
#' }
#'
#' @export

geoclient_bin <- function(df = NULL,
                          bin,
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
      stop_glue("If a dataframe is not given as the first argument, the other argument must be named")
    }

    bin <- enquo(bin)
    bin <- dplyr::pull(df, !!bin)
  }

  bin <- as.character(bin)

  if (stringr::str_length(bin) != 7) {
    stop_glue("BIN must be formatted as a 7-digit code, with the first digit being the borough code")
  }

  bin_inputs <- tibble::tibble(
    bin = bin
  )

  res <- make_requests(
    inputs = bin_inputs,
    operation = "bin",
    creds = creds,
    rate_limit = rate_limit,
    cap_daily_requests = cap_daily_requests
  )

  res
}
