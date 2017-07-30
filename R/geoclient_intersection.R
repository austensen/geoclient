#' Retrieve Geoclient Response for Blockfaces as a Dataframe
#'
#' This function takes components of addresses and returns the Geoclient
#' response as a tibble. The house number, street name, and one of either
#' borough or Zip code are required. The address components can be provided
#' either in separate vectors as named arguments or with a dataframe and
#' column names containing each component. The Geoclient API's app ID and key
#' can either be provided directly as arguments, or you can first use
#' [geoclient_api_keys()] to add them to your `.Renviron` file so they can be
#' called securely without being stored in your code.
#'
#' @param df Dataframe that contains a column of BBLs. Defaults to `NULL` and
#'   `bbl` is taken as a vector.
#' @param on_street The street of the target blockface, as either a vector of
#' street names, or a bare column name of the field if a dataframe is provided.
#' @param cross_street_1 The first cross street of the blockface, as either a vector
#' of street names, or a bare column name of the field if a dataframe is provided.
#' @param cross_street_2 The second cross street of the blockface, as either a vector
#' of street names, or a bare column name of the field if a dataframe is provided.
#' @param borough The name of the borough of the the first cross street, or the entire
#' intersection if `cross_street_2_borough` is not provided. The argument can be provided
#' as either a vector or a bare column name of the borough field if a dataframe is provided.
#' @param cross_street_2_borough Optionally, the name of the borough of the second
#' cross street if it differs from the first cross street. The argument can be provided
#' as either a vector or a bare column name of the borough field if a dataframe is provided.
#' @param compass_direction Optionally, the direction indicating which intersection is desired
#' when the two street intersect in multiple locations. The argument can be provided
#' as either a vector or a bare column name of the field if a dataframe is provided.
#' The valid values of are `"N"`, `"E"`, `"S"`, or `"W"`
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
#'   [making address requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.1),
#'   interpreting the [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2), the
#'   [data returned by `geoclient_address`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.1),
#'   and a [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0) for
#'   all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geoclient_intersection(
#'   cross_street_1 = "macdougal st",
#'   cross_street_2 = "w 3rd st",
#'   borough = "mn"
#' )
#'
#'
#' library(tibble)
#' library(dplyr)
#'
#' df <- tribble(
#'   ~cross1,         ~cross2,       ~boro,
#'   "macdougal st",  "w 3rd st",    "mn",
#'   "Lexington Ave", "125th Steet", "Manhattan"
#' )
#'
#' geoclient_intersection(df, cross1, cross2, boro)
#'
#' df %>%
#'   mutate(
#'     lion_node_number = geoclient_intersection(
#'       cross_street_1 = cross1,
#'       cross_street_2 = cross2,
#'       borough = boro
#'       )[["lionNodeNumber"]]
#'   )
#' }
#'
#' @export

geoclient_intersection <- function(df = NULL,
                                   cross_street_1,
                                   cross_street_2,
                                   borough,
                                   cross_street_2_borough = NULL,
                                   compass_direction = NULL,
                                   id = NULL,
                                   key = NULL,
                                   rate_limit = TRUE,
                                   cap_daily_requests = TRUE) {

  # Get Geoclient App ID and Key (either from .Renviron or arguments)
  creds <- get_credentials(id, key)

  # If a dataframe is provided, get the vectors from there, otherwise just use input vectors
  # If borough or zip are not provided, fill with NAs
  if (!is.null(df)) {

    if (!is.data.frame(df)) {
      stop_glue("If a dataframe is not given as the first argument, the other arguments must be named")
    }

    cross_street_1 <- enquo(cross_street_1)
    cross_street_2 <- enquo(cross_street_2)
    borough <- enquo(borough)
    cross_street_2_borough <- enquo(cross_street_2_borough)
    compass_direction <- enquo(compass_direction)

    cross_street_1 <- dplyr::pull(df, !!cross_street_1)
    cross_street_2 <- dplyr::pull(df, !!cross_street_2)
    borough <- dplyr::pull(df, !!borough)

    len <- length(cross_street_1)

    if (quo_is_null(cross_street_2_borough)) {
      cross_street_2_borough <- borough
    } else {
      cross_street_2_borough <- dplyr::pull(df, !!cross_street_2_borough)
    }

    if (quo_is_null(compass_direction)) {
      compass_direction <- rep(NA_character_, len)
    } else {
      compass_direction <- dplyr::pull(df, !!compass_direction)
    }

  } else {

    len <- length(cross_street_1)

    if (is_null(cross_street_2_borough)) {
      cross_street_2_borough <- borough
    }

    if (is_null(compass_direction)) {
      compass_direction <- rep(NA_character_, len)
    }
  }

  borough <- clean_borough(borough)
  cross_street_2_borough <- clean_borough(cross_street_2_borough)

  intersection_inputs <- tibble::tibble(
    crossStreetOne = cross_street_1,
    crossStreetTwo = cross_street_2,
    borough = borough,
    boroughCrossStreetTwo = cross_street_2_borough,
    compassDirection = compass_direction
  )

  res <- make_requests(
    inputs = intersection_inputs,
    operation = "intersection",
    creds = creds,
    rate_limit = rate_limit,
    cap_daily_requests = cap_daily_requests
  )

  res
}

