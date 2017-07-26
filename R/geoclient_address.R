#' Install Geoclient API ID and Key in Your `.Renviron` File for Repeated Use
#' @description This function will add your Geoclient app ID and key to your
#'   `.Renviron` file so they can be called securely without being stored in
#'   your code. After you have installed your ID and key, they can be called any
#'   time by typing `Sys.getenv("GEOCLIENT_APP_ID")` and
#'   `Sys.getenv("GEOCLIENT_APP_KEY")` and can be used in package functions by
#'   simply typing GEOCLIENT_APP_ID or GEOCLIENT_APPKEY. If you do not have an
#'   `.Renviron` file, the function will create on for you. If you already have
#'   an `.Renviron` file, the function will append the ID and key to your
#'   existing file, while making a backup of your original file for disaster
#'   recovery purposes. You can acquire your Geoclient app ID and Key by first
#'   registering with the
#'   ![https://developer.cityofnewyork.us/user/register?destination=api](NYC
#'   Developer Portal) at, then
#'   ![https://developer.cityofnewyork.us/create/project](create a new project),
#'   selecting "Geoclient v1" from available APIs.
#' @param id The API app ID provided to you from the NYC Developer Portal
#'   formated in quotes.
#' @param key The API app key provided to you from the NYC Developer Portal
#'   formated in quotes.
#' @param install if TRUE, will install the key in your `.Renviron` file for use
#'   in future sessions.  Defaults to FALSE.
#' @param overwrite If this is set to TRUE, it will overwrite the existing
#'   GEOCLIENT_APP_ID and GEOCLIENT_APP_KEY that you already have in your
#'   `.Renviron` file.
#' @importFrom utils write.table read.table
#' @examples
#'
#' \dontrun{
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh", install = TRUE)
#' # First time, reload your environment so you can use the keys without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check them with:
#' Sys.getenv("GEOCLIENT_APP_ID")
#' Sys.getenv("GEOCLIENT_APP_KEY")
#' }
#'
#' \dontrun{
#' # If you need to overwrite existing keys:
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh", overwrite = TRUE, install = TRUE)
#' # First time, reload your environment so you can use the keys without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check them with:
#' Sys.getenv("GEOCLIENT_APP_ID")
#' Sys.getenv("GEOCLIENT_APP_KEY")
#' }
#' @export

geoclient_address <- function(df = NULL, number, street, borough = NULL, zip = NULL) {

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

  # To avoid sending multiple requests for the same address, preserve original
  # inputs, use deduplicated version for request, then join the respone back to
  # original inputs before returning final result

  addr_orig <- tibble::tibble(
    number = number,
    street = street,
    borough = borough,
    zip = zip
  )

  addr_dedup <- dplyr::distinct(addr_orig)

  pb <- dplyr::progress_estimated(nrow(addr_dedup))

  # TODO: incorporate rate limiting, 2500/min
  res <- purrr::pmap_df(as_list(addr_dedup), single_address, pb = pb)

  res <- dplyr::bind_cols(addr_dedup, res)

  res <- dplyr::left_join(addr_orig, res, by = c("number", "street", "borough", "zip"))

  res
}

single_address <- function(number, street, borough = NULL, zip = NULL, pb = NULL, ...) {

  if (Sys.getenv('GEOCLIENT_APP_ID') == '' || Sys.getenv('GEOCLIENT_APP_KEY') == '') {

    stop_glue(
      "A Geoclient app ID and key are required.
      Obtain them at https://developer.cityofnewyork.us/user/register?destination=api,",
      "and then supply them to the `geoclient_api_keys` function to avoid entering them with each call."
    )
  }

  id <- Sys.getenv('CENSUS_API_ID')
  key <- Sys.getenv('CENSUS_API_KEY')

  # For creating a progress bar
  # Thanks to Bob Rudis' post: https://rud.is/b/2017/05/05/scrapeover-friday-a-k-a-another-r-scraping-makeover/
  if (!is.null(pb) && (pb$n > 10)) pb$tick()$print()

  if (!is.null(borough)) {
    if (is.na(borough)) {
      borough <- NULL
    }
  } else {
    borough <- case_when(
      stringr::str_detect(borough, "^(?i)(mn)|(manhattan)(new york)")     ~ "manhattan",
      stringr::str_detect(borough, "^(?i)(bx)|(bronx)")                   ~ "bronx",
      stringr::str_detect(borough, "^(?i)(bk)|(brooklyn)(kings)")         ~ "brooklyn",
      stringr::str_detect(borough, "^(?i)(qn)|(queens)")                  ~ "queens",
      stringr::str_detect(borough, "^(?i)(si)|(staten island)(richmond)") ~ "staten island",
      TRUE ~ borough
    )
  }

  if (!is.null(zip)) {
    if (is.na(zip)) {
      zip <- NULL
    }
  }

  resp <- rGET(
    "https://api.cityofnewyork.us/geoclient/v1/address.json?",
    httr::accept_json(),
    query = list(
      "houseNumber" = number,
      "street" = street,
      "borough" = borough,
      "zip" = zip,
      "app_id" = Sys.getenv("GEOCLIENT_APP_ID"),
      "app_key" = Sys.getenv("GEOCLIENT_APP_KEY")
    )
  )

  httr::stop_for_status(resp)

  parsed <- content_as_json_UTF8(resp)[["address"]]

  tibble::as_tibble(parsed)
}

