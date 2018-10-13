#' Retrieve Geoclient Response for Intersections as a Dataframe
#'
#' This function takes components of addresses and returns the Geoclient
#' response as a tibble. The house number, street name, and one of either
#' borough or Zip code are required. The address components can be provided
#' either in separate vectors as named arguments or with a dataframe and
#' column names containing each component. The Geoclient API's app ID and key
#' can either be provided directly as arguments, or you can first use
#' [`geoclient_api_keys()`] to add them to your `.Renviron` file so they can be
#' called securely without being stored in your code.
#'
#' @inheritParams geo_blockface
#' @param cross_street_1 The first cross street of the intersection, as either a vector
#' of street names, or a bare column name of the field if a dataframe is provided.
#' @param cross_street_2 The second cross street of the intersection, as either a vector
#' of street names, or a bare column name of the field if a dataframe is provided.
#' @param borough The name of the borough of the the first cross street, or the entire
#' intersection if `cross_street_2_borough` is not provided. The argument can be provided
#' as either a vector or a bare column name of the borough field if a dataframe is provided.
#' @param cross_street_2_borough Optionally, the name of the borough of the second
#' cross street if it differs from the first cross street. The argument can be provided
#' as either a vector or a bare column name of the borough field if a dataframe is provided.
#'
#' @details For more details see the Geoclient Documentation's guide to
#'   [making intersection requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.5),
#'   interpreting the [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2), the
#'   [data returned by `geo_intersection`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.5),
#'   and a [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0) for
#'   all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geo_intersection("macdougal st", "w 3rd st", "mn")
#'
#' df <- tibble::tribble(
#'   ~st_1,           ~st_2,       ~boro,
#'   "macdougal st",  "w 3rd st",    "mn",
#'   "Lexington Ave", "125th Steet", "Manhattan"
#' )
#'
#' geo_intersection(df, cross1, cross2, boro)
#'
#' library(dplyr)
#'
#' bind_cols(df, geo_intersection(df, cross1, cross2, boro))
#'
#' mutate(.data, lion_node_num = geo_intersection(st_1, st_2, boro)[["lionNodeNumber"]])
#'
#' }
#'
#' @name geo_intersection
#' @export
geo_intersection_data <- function(.data,
                                  cross_street_1,
                                  cross_street_2,
                                  borough,
                                  cross_street_2_borough = NULL,
                                  compass_direction = NULL,
                                  id = NULL,
                                  key = NULL,
                                  rate_limit = TRUE) {

  creds <- get_creds(id, key)

  intersection_inputs <- validate_intersection_inputs(
             cross_street_1 = pull_or_null(.data, enquo(cross_street_1)),
             cross_street_2 = pull_or_null(.data, enquo(cross_street_2)),
                    borough = pull_or_null(.data, enquo(borough)),
     cross_street_2_borough = pull_or_null(.data, enquo(cross_street_2_borough)),
          compass_direction = pull_or_null(.data, enquo(compass_direction))
  )

  geoclient_reqs(intersection_inputs, "intersection", creds, rate_limit)
}

#' @rdname geo_intersection
#' @export
geo_intersection <- function(cross_street_1,
                             cross_street_2,
                             borough,
                             cross_street_2_borough = NULL,
                             compass_direction = NULL,
                             id = NULL,
                             key = NULL,
                             rate_limit = TRUE) {

  creds <- get_creds(id, key)

  intersection_inputs <- validate_intersection_inputs(
    cross_street_1,
    cross_street_2,
    borough,
    cross_street_2_borough,
    compass_direction
  )

  geoclient_reqs(intersection_inputs, "intersection", creds, rate_limit)
}


validate_intersection_inputs <- function(cross_street_1,
                                         cross_street_2,
                                         borough,
                                         cross_street_2_borough,
                                         compass_direction) {

  len <- length(cross_street_1)

  borough <- clean_borough(borough)

  cross_street_2_borough <- if_null_fill_na(cross_street_2_borough, len) %>% clean_borough()
  compass_direction <- if_null_fill_na(compass_direction, len)

  all_compass_valid <- all(stringr::str_detect(compass_direction, "^(?i)[nesw]$"), na.rm = TRUE)

  if (!all_compass_valid) {
    stop_glue('Invalid values for Compass Direction. Must be one of "N", "E", "S", "W"')
  }

  # If unequal lengths tibble raises error with column names so change for API after
  dplyr::tibble(
    cross_street_1,
    cross_street_2,
    borough,
    cross_street_2_borough,
    compass_direction
  ) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    rlang::set_names(
      "crossStreetOne",
      "crossStreetTwo",
      "borough",
      "boroughCrossStreetTwo",
      "compassDirection"
    )
}
