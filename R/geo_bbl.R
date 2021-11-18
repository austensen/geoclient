#' Retrieve Geoclient Response for BBLs as a Dataframe
#'
#' This function takes BBLs (borough-block-lot) and returns the Geoclient
#' response as a tibble. The BBLs can be provided either in a vector as a named
#' argument or with a dataframe and column name of the BBL field. The Geoclient
#' API key can either be provided directly as an argument, or you
#' can first use [`geoclient_api_key()`] to add it to your `.Renviron` file
#' so it can be called securely without being stored in your code.
#'
#' @inheritParams geo_address
#' @param bbl Either a vector of BBLs (numeric or character is accepted), or a
#'   bare column name of the bbl field if a dataframe is provided.
#'
#' @details For more details see the Geoclient Documentation's guide to
#'   [making BBL requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.2),
#'   interpreting the [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2), the
#'   [data returned by `geo_bbl`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.2),
#'   and a [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0) for
#'   all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_key("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geo_bbl(1005430053)
#'
#' library(dplyr)
#'
#' df <- tibble(BBL = c("1005430053", "1005107502"))
#'
#' geo_bbl_data(df, BBL)
#'
#' bind_cols(df, geo_bbl_data(df, BBL))
#'
#' mutate(df, condo_bbl = geo_bbl(BBL)[["condominiumBillingBbl"]])
#'
#' }
#'
#' @name geo_bbl
#' @export
geo_bbl_data <- function(.data, bbl, id = NULL, key = NULL, rate_limit = TRUE) {

  creds <- get_creds(key)

  bbl_inputs <- validate_bbl_inputs(
    bbl = pull_or_null(.data, enquo(bbl))
  )

  geoclient_reqs(bbl_inputs, "bbl", creds, rate_limit)
}

#' @rdname geo_bbl
#' @export
geo_bbl <- function(bbl, id = NULL, key = NULL, rate_limit = TRUE) {

  creds <- get_creds(key)

  bbl_inputs <- validate_bbl_inputs(bbl)

  geoclient_reqs(bbl_inputs, "bbl", creds, rate_limit)
}


validate_bbl_inputs <- function(bbl) {

  # Temporarily change option to prevent scientific notation when coercing
  # double to character
  op <- options(scipen = 999)
  on.exit(options(op))

  all_bbls_valid <- all(stringr::str_detect(bbl, "^[1-5](?!0{5})\\d{5}(?!0{4})\\d{4}$"), na.rm = TRUE)

  if (!all_bbls_valid) {
    stop_glue("
    Invalid values for BBL (borough-block-lot).
      * BBLs are 10 characters, containing only digits
      * 1 (borough), 5 (block), and 4(lot)
      * The first digit must be a DCP borough code (1=MN, 2=BX, 3=BK, 4=QN, 5=SI)
      * Block and lot should use left-zero-padding were necessary
      * Neither block nor lot can be all 0s
    ")
  }

  dplyr::tibble(
    borough = stringr::str_sub(bbl, 1, 1),
    block = stringr::str_sub(bbl, 2, 6),
    lot = stringr::str_sub(bbl, 7, 10)
  )
}
