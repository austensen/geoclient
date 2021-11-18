#' Retrieve Geoclient Response for BINs as a Dataframe
#'
#' This function takes BINs (Building Identification Number) and returns the
#' Geoclient response as a tibble. The BINs can be provided either in a vector
#' as a named argument or with a dataframe and column name of the BIN field. Your
#' Geoclient API key can either be provided directly as arguments,
#' or you can first use [`geoclient_api_key()`] to add them to your `.Renviron`
#' file so they can be called securely without being stored in your code.
#'
#' @inheritParams geo_address
#' @param bin Either a vector of BINs (numeric or character is accepted), or a
#'   bare column name of the bin field if a dataframe is provided.
#'
#' @details For more details see the Geoclient Documentation's guide to
#'   [making BIN requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.3),
#'   interpreting the [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2), the
#'   [data returned by `geo_bin`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.3),
#'   and a [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0) for
#'   all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_key("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geo_bin(1015862)
#'
#' library(dplyr)
#'
#' df <- tibble(BIN = c("1008760", "1007941"))
#'
#' geo_bin_data(df, BIN)
#'
#' bind_cols(df, geo_bin_data(df, BIN))
#'
#' mutate(df, bid = geo_bin(BIN)[["businessImprovementDistrict"]])
#'
#' }
#'
#' @name geo_bin
#' @export
geo_bin_data <- function(.data, bin, key = NULL, rate_limit = TRUE) {

  creds <- get_creds(key)

  bin_inputs <- validate_bin_inputs(
    bin = pull_or_null(.data, enquo(bin))
  )

  geoclient_reqs(bin_inputs, "bin", creds, rate_limit)
}

#' @rdname geo_bin
#' @export
geo_bin <- function(bin, key = NULL, rate_limit = TRUE) {

  creds <- get_creds(key)

  bin_inputs <- validate_bin_inputs(bin)

  geoclient_reqs(bin_inputs, "bin", creds, rate_limit)
}


validate_bin_inputs <- function(bin) {

  # Temporarily change option to prevent scientific notation when coercing
  # double to character
  op <- options(scipen = 999)
  on.exit(options(op))

  all_bins_valid <- all(stringr::str_detect(bin, "^[1-5](?!0{6})\\d{6}$"), na.rm = TRUE)

  if (!all_bins_valid) {
    stop_glue("
    Invalid values for BIN (Building Identification Number).
      * Bins are 7 characters, containing only digits
      * The first digit must be a DCP borough code (1=MN, 2=BX, 3=BK, 4=QN, 5=SI)
      * The last 6 digits can't be all 0s
    ")
  }

  dplyr::tibble(bin = as.character(bin))
}
