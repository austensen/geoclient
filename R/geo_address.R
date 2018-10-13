#' Retrieve Geoclient Response for Addresses as a Dataframe
#'
#' This function takes components of addresses and returns the Geoclient
#' response as a tibble. The house number, street name, and one of either
#' borough or Zip code are required. The address components can be provided
#' either in separate vectors as named arguments or with a dataframe and column
#' names containing each component. If your address data is not easily separted
#' into these components you can use [`geo_search()`] with the full address as a
#' string. The Geoclient API's app ID and key can either be provided directly as
#' arguments, or you can first use [`geoclient_api_keys()`] to add them to your
#' `.Renviron` file so they can be called securely without being stored in your
#' code.
#'
#' @param .data Dataframe containing columns to be used for other arguments.
#' @param house_number The house number of the address, as either a vector of
#'   numbers (numeric or character is accepted), or a bare column name of the
#'   number field if a dataframe is provided.
#' @param street The street name of the address, as either a vector of names, or
#'   a bare column name of the number field if a dataframe is provided.
#' @param borough The name of the borough of the address, as either a vector or
#'   a bare column name of the borough field if a dataframe is provided. The
#'   borough is only required if Zip code is not provided.
#' @param zip The Zip code of the address, as either a vector (numeric or
#'   character is accepted) or a bare column name of the borough field if a
#'   dataframe is provided. The Zip code is only required if borough is not
#'   provided.
#' @param id The API app ID provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and your key is accessed from your
#'   `.Renviron`.
#' @param key The API app key provided to you from the NYC Developer Portal
#'   formated in quotes. Defaults to `NULL` and your key is accessed from your
#'   `.Renviron`.
#' @param rate_limit Whether you would like to limit the rate of API requests in
#'   adherence to Geoclient's Service Usage Guidelines. See `?geoclient` for
#'   more information.
#'
#' @details For more details see the Geoclient Documentation's guide to [making
#'   address
#'   requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.1),
#'   interpreting the [Geosupport return
#'   codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2), the
#'   [data returned by
#'   `geo_address`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.1),
#'   and a [complete data
#'   dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0) for
#'   all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geo_address("139", "MacDougal St", "MN")
#'
#' df <- tibble::tribble(
#'   ~num,  ~st,                ~boro,         ~zip,
#'   "139", "MacDougal St",     "manhattan",   "11231",
#'   "295", "Lafayette street", NA_character_, "10012-2722",
#'   "40",  "WASHINGTON SQ S",  "MN",          NA_character_
#' )
#'
#' geo_address_data(df, num, st, boro, zip)
#'
#' dplyr::mutate(df, bbl = geo_address(num, st, boro, zip)[["bbl"]])
#'
#' }
#'
#' @name geo_address
#' @export
geo_address_data <- function(.data,
                             house_number,
                             street,
                             borough = NULL,
                             zip = NULL,
                             id = NULL,
                             key = NULL,
                             rate_limit = TRUE) {

  creds <- get_creds(id, key)

  address_inputs <- validate_address_inputs(
    house_number = pull_or_null(.data, enquo(house_number)),
          street = pull_or_null(.data, enquo(street)),
         borough = pull_or_null(.data, enquo(borough)),
             zip = pull_or_null(.data, enquo(zip))
  )

  geoclient_reqs(address_inputs, "address", creds, rate_limit)
}

#' @rdname geo_address
#' @export
geo_address <- function(house_number,
                        street,
                        borough = NULL,
                        zip = NULL,
                        id = NULL,
                        key = NULL,
                        rate_limit = TRUE) {

  creds <- get_creds(id, key)

  address_inputs <- validate_address_inputs(house_number, street, borough, zip)

  geoclient_reqs(address_inputs, "address", creds, rate_limit)
}


validate_address_inputs <- function(house_number, street, borough, zip) {

  len <- length(house_number)

  if (is_null(borough) && is_null(zip)) {
    stop_glue("One of either borough or zip must be provided")
  }

  borough <- if_null_fill_na(borough, len) %>% clean_borough()
  zip <- if_null_fill_na(zip, len)

  # If unequal lengths tibble raises error with column names so change for API after
  dplyr::tibble(house_number, street, borough, zip) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    rlang::set_names("houseNumber", "street", "borough", "zip")
}
