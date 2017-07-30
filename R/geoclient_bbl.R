#' Retrieve Geoclient Response for BBLs as a Dataframe
#'
#' This function takes BBLs (borough-block-lot) and returns the Geoclient
#' response as a tibble. The BBLs can be provided either in a vector as a named
#' argument or with a dataframe and column name of the BBL field. The Geoclient
#' API's app ID and key can either be provided directly as arguments, or you can
#' first use [geoclient_api_keys()] to add them to your `.Renviron` file so they
#' can be called securely without being stored in your code.
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
#' @param rate_limit Whether you would like to limit the rate of API requests in
#'   adherence to Geoclient's Service Usage Guidelines. See `?geoclient` for
#'   more information.
#' @param cap_daily_requests Whether you would like to cap the daily number of
#'   API requests in adherence to Geoclient's Service Usage Guidelines. See
#'   `?geoclient` for more information.
#'
#' @details For more details see the Geoclient Documentation's guide to
#'   [making BBL requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.2),
#'   interpreting the [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2), the
#'   [data returned by `geoclient_bbl`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.2),
#'   and a [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0) for
#'   all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geoclient_bbl(bbl = 1005430053)
#' geoclient_bbl(bbl = c("1005430053", "1005107502"))
#'
#' library(dplyr)
#'
#' df <- tibble(BBL = c("1005430053", "1005107502"))
#'
#' geoclient_bbl(df, BBL)
#' }
#'
#' @export

geoclient_bbl <- function(df = NULL,
                          bbl,
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
      stop_glue("If a dataframe is not given as the first argument, the bbl argument must be named")
    }

    bbl <- enquo(bbl)
    bbl <- dplyr::pull(df, !!bbl)
  }

  bbl <- as.character(bbl)

  all_bbls_correct <- bbl %>% stringr::str_length() %>% all(. == 10)

  if (is_false(all_bbls_correct)) {
    stop_glue("BBL must be formatted as a 10-digit code, with 1-digit borough, 5-digit block, 4-digit lot")
  }

  borough <- stringr::str_sub(bbl, 1, 1)
  borough <- dplyr::case_when(
    borough == "1" ~ "manhattan",
    borough == "2" ~ "bronx",
    borough == "3" ~ "brooklyn",
    borough == "4" ~ "queens",
    borough == "5" ~ "staten island"
  )

  block <- stringr::str_sub(bbl, 2, 6)
  lot <- stringr::str_sub(bbl, 7, 10)

  bbl_inputs <- tibble::tibble(
    borough = borough,
    block = block,
    lot = lot
  )

  res <- make_requests(
    inputs = bbl_inputs,
    operation = "bbl",
    creds = creds,
    rate_limit = rate_limit,
    cap_daily_requests = cap_daily_requests
  )

  res
}

