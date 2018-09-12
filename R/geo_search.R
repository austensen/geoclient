#' Retrieve Geoclient Response for Any Location Type as a Dataframe
#'
#' This function takes any of the 6 location types as a single input and returns
#' the Geoclient response as a tibble. The locations are provided either in a
#' single vector as a named argument or with a dataframe and column name of the
#' location field. This function is helpful when your address data is not
#' separated into components. The Geoclient API's app ID and key can either be
#' provided directly as arguments, or you can first use [geoclient_api_keys()]
#' to add them to your `.Renviron` file so they can be called securely without
#' being stored in your code.
#'
#' @inheritParams geo_address
#' @param location Any of the 6 locations types from other functions: Address,
#'   BBL, BIN, Blockface, Intersection, or Place. The argument can be either a
#'   single vector of locations, or a bare column name of the location field if
#'   a dataframe is provided.
#'
#' @details Geoclient is quite flexible with the format of the address when
#'   provided as a single input for this function, however the results may be
#'   slower than the more restrictive [geo_address()] because Geoclient
#'   may have to make multiple requests to narrow in on the correct location.
#'
#'   For more details see the Geoclient Documentation's guide to [single-input
#'   requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.3),
#'   interpreting the [Geosupport return
#'   codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2), and a
#'   [complete data
#'   dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0) for
#'   all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geo_search(1005430053) # BBL
#' geo_search("139 macdougal st mn") # Address
#' geo_search(c("1008760", "1007941")) # BIN
#'
#' library(dplyr)
#'
#' df <- tibble(
#'   location = c(
#'     "1005430053",
#'     "139 MacDougal Street, New York, 10012",
#'     "1008760"
#'    )
#'  )
#'
#' geo_search_data(df, location)
#'
#' bind_cols(df, geo_search_data(df, location))
#'
#' }
#'
#' @name geo_search
#' @export
geo_search_data <- function(.data, location, id = NULL, key = NULL, rate_limit = TRUE) {

  creds <- get_creds(id, key)

  search_inputs <- validate_search_inputs(pull_or_null(.data, enquo(location)))

  geoclient_reqs(search_inputs, "search", creds, rate_limit)
}

#' @rdname geo_search
#' @export
geo_search <- function(location, id = NULL, key = NULL, rate_limit = TRUE) {

  creds <- get_creds(id, key)

  search_inputs <- validate_search_inputs(location)

  geoclient_reqs(search_inputs, "search", creds, rate_limit)
}

validate_search_inputs <- function(location) {
  dplyr::tibble(input = location)
}
