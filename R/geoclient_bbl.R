#' Retrieve Dataframe Response from Geoclient for BBLs
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
#' geoclient_bbl(bbl = 1005430053)
#' geoclient_bbl(bbl = c("1005430053", "1005107502"))
#'
#' library(dplyr)
#'
#' df <- tibble(BBL = c("1005430053", "1005107502"))
#'
#' df %>% geoclient_bbl(BBL)
#' }
#'
#' @export

geoclient_bbl <- function(df = NULL, bbl, id = NULL, key = NULL) {

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

  res <- vectorized_requests(
    inputs = bbl_inputs,
    operation = "bbl",
    id = id,
    key = key
  )

  res
}

