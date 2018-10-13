#' Retrieve Geoclient Response for Blockfaces as a Dataframe
#'
#' This function takes components of addresses and returns the Geoclient
#' response as a tibble. The house number, street name, and one of either
#' borough or Zip code are required. The address components can be provided
#' either in separate vectors as named arguments or with a dataframe and column
#' names containing each component. The Geoclient API's app ID and key can
#' either be provided directly as arguments, or you can first use
#' [`geoclient_api_keys()`] to add them to your `.Renviron` file so they can be
#' called securely without being stored in your code.
#'
#' @inheritParams geo_address
#' @param on_street The street of the target blockface, as either a vector of
#' street names, or a bare column name of the field if a dataframe is
#' provided.
#' @param cross_street_1 The first cross street of the blockface, as either a
#' vector of street names, or a bare column name of the field if a dataframe
#' is provided.
#' @param cross_street_2 The second cross street of the blockface, as either a
#' vector of street names, or a bare column name of the field if a dataframe
#' is provided.
#' @param borough The name of the borough of the "on street", as either a vector
#' or a bare column name of the borough field if a dataframe is provided.
#' @param cross_street_1_borough Optionally, the name of the borough of the
#' first cross street, as either a vector or a bare column name of the borough
#' field if a dataframe is provided. By default this is `NULL` and the "on
#' street" borough is used.
#' @param cross_street_2_borough Optionally, the name of the borough of the
#' second cross street, as either a vector or a bare column name of the
#' borough field if a dataframe is provided. By default this is `NULL` and the
#' "on street" borough is used.
#' @param compass_direction Optionally, the direction indicating a side of the
#' street to request information about only one side of the street. The
#' argument can be provided as either a vector or a bare column name of the
#' field if a dataframe is provided. The valid values of are `"N"`, `"E"`,
#' `"S"`, or `"W"`
#'
#' @details For more details see the Geoclient Documentation's guide to
#' [making blockface requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.4),
#' interpreting the
#' [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2),
#' the [data returned by `geo_blockface`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.4),
#' and a [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0)
#' for all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geo_blockface(
#'   on_street = "cypress ave",
#'   cross_street_1 = "dekalb ave",
#'   cross_street_2 = "hart st",
#'   borough = "qn"
#' )
#'
#' df <- tibble::tribble(
#'   ~street,        ~cross1,           ~cross2,    ~boro,
#'   "macdougal st", "washington sq s", "w 3rd st", "mn",
#'   "Cypress Ave",  "DeKalb Ave",      "Hart St",  "Brooklyn"
#' )
#'
#' geo_blockface_data(df, street, cross1, cross2, boro)
#'
#' library(dplyr)
#'
#' bind_cols(df, geo_blockface_data(df, street, cross1, cross2, boro))
#'
#' mutate(df, traffic_dir = geo_blockface(street, cross1, cross2, boro)[["trafficDirection"]])
#'
#' }
#'
#' @name geo_blockface
#' @export
geo_blockface_data <- function(.data,
                               on_street,
                               cross_street_1,
                               cross_street_2,
                               borough,
                               cross_street_1_borough = NULL,
                               cross_street_2_borough = NULL,
                               compass_direction = NULL,
                               id = NULL,
                               key = NULL,
                               rate_limit = TRUE) {

  creds <- get_creds(id, key)

  blockface_inputs <- validate_blockface_inputs(
                  on_street = pull_or_null(.data, enquo(on_street)),
             cross_street_1 = pull_or_null(.data, enquo(cross_street_1)),
             cross_street_2 = pull_or_null(.data, enquo(cross_street_2)),
                    borough = pull_or_null(.data, enquo(borough)),
     cross_street_1_borough = pull_or_null(.data, enquo(cross_street_1_borough)),
     cross_street_2_borough = pull_or_null(.data, enquo(cross_street_2_borough)),
          compass_direction = pull_or_null(.data, enquo(compass_direction))
  )

  geoclient_reqs(blockface_inputs, "blockface", creds, rate_limit)
}

#' @rdname geo_blockface
#' @export
geo_blockface <- function(on_street,
                          cross_street_1,
                          cross_street_2,
                          borough,
                          cross_street_1_borough = NULL,
                          cross_street_2_borough = NULL,
                          compass_direction = NULL,
                          id = NULL,
                          key = NULL,
                          rate_limit = TRUE) {

  creds <- get_creds(id, key)

  blockface_inputs <- validate_blockface_inputs(
    on_street,
    cross_street_1,
    cross_street_2,
    borough,
    cross_street_1_borough,
    cross_street_2_borough,
    compass_direction
  )

  geoclient_reqs(blockface_inputs, "blockface", creds, rate_limit)
}


validate_blockface_inputs <- function(on_street,
                                      cross_street_1,
                                      cross_street_2,
                                      borough,
                                      cross_street_1_borough,
                                      cross_street_2_borough,
                                      compass_direction) {

  len <- length(on_street)

  borough <- clean_borough(borough)

  cross_street_1_borough <- if_null_fill_na(cross_street_1_borough, len) %>% clean_borough()
  cross_street_2_borough <- if_null_fill_na(cross_street_2_borough, len) %>% clean_borough()
  compass_direction <- if_null_fill_na(compass_direction, len)

  all_compass_valid <- all(stringr::str_detect(compass_direction, "^(?i)[nesw]$"), na.rm = TRUE)

  if (!all_compass_valid) {
    stop_glue('Invalid values for Compass Direction. Must be one of "N", "E", "S", "W"')
  }

  # If unequal lengths tibble raises error with column names so change for API after
  dplyr::tibble(
    on_street,
    cross_street_1,
    cross_street_2,
    borough,
    cross_street_1_borough,
    cross_street_2_borough,
    compass_direction
  ) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    rlang::set_names(
      "onStreet",
      "crossStreetOne",
      "crossStreetTwo",
      "borough",
      "boroughCrossStreetOne",
      "boroughCrossStreetTwo",
      "compassDirection"
    )
}
