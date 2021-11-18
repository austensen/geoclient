#' Retrieve Geoclient Response for Well-Known NYC Places as a Dataframe
#'
#' This function takes the name of a well-known NYC place and returns the
#' Geoclient response as a tibble. The place name, and one of either borough
#' or Zip code are required. The place components can be provided either in
#' separate vectors as named arguments or with a dataframe and column names
#' containing each component. The Geoclient API key
#' can either be provided directly as an argument, or you can first use
#' [geoclient_api_key()] to add it to your `.Renviron` file so it can be
#' called securely without being stored in your code.
#'
#' @inheritParams geo_address
#' @param place Either a vector of BBLs (numeric or character is accepted), or a
#'   bare column name of the bbl field if a dataframe is provided.
#' @param borough The name of the borough of the place, as either a vector
#'   or a bare column name of the borough field if a dataframe is provided.
#'   The borough is only required if Zip code is not provided.
#' @param zip The Zip code of the place, as either a vector (numeric or character
#'   is accepted) or a bare column name of the borough field if a dataframe
#'   is provided. Five- and seven-digit Zip codes are accepted. The Zip code is
#'   only required if borough is not provided.
#'
#' @details For more details see the Geoclient Documentation's guide to
#'   [making place requests](https://api.cityofnewyork.us/geoclient/v1/doc#section-1.2.6),
#'   interpreting the [Geosupport return codes](https://api.cityofnewyork.us/geoclient/v1/doc#section-2.2),
#'   the [data returned by `geo_place`](https://api.cityofnewyork.us/geoclient/v1/doc#section-3.6),
#'   and a [complete data dictionary](https://api.cityofnewyork.us/geoclient/v1/doc#section-4.0)
#'   for all possible data elements returned by any geoclient function.
#'
#' @examples
#'
#' \dontrun{
#'
#' geoclient_api_key("9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' geoclient_place(place = "empire state building", borough = "mn")
#'
#' library(tibble)
#' library(dplyr)
#'
#' df <- tribble(
#'   ~place,         ~boro,         ~zip,
#'   "NYU",          NA_character_, "10012",
#'   "CITY HALL",    "MN",          NA_character_,
#'   "Pospect Park", "Brooklyn",    NA_character_
#' )
#'
#' geoclient_place(df, place, boro, zip)
#'
#' df %>%
#'   mutate(
#'     bbl = geoclient_place(place = place, borough = boro, zip = zip)[["bbl"]]
#'   )
#' }
#'
#' @name geo_place
#' @export
geo_place_data <- function(.data,
                           place,
                           borough = NULL,
                           zip = NULL,
                           key = NULL,
                           rate_limit = TRUE) {

  creds <- get_creds(key)

  place_inputs <- validate_place_inputs(
    place = pull_or_null(.data, enquo(place)),
    borough = pull_or_null(.data, enquo(borough)),
    zip = pull_or_null(.data, enquo(zip))
  )

  geoclient_reqs(place_inputs, "place", creds, rate_limit)
}

#' @rdname geo_place
#' @export
geo_place <- function(place,
                      borough = NULL,
                      zip = NULL,
                      key = NULL,
                      rate_limit = TRUE) {

  creds <- get_creds(key)

  place_inputs <- validate_place_inputs(place, borough, zip)

  geoclient_reqs(place_inputs, "place", creds, rate_limit)
}

validate_place_inputs <- function(place, borough, zip) {

  len <- length(place)

  if (is_null(borough) && is_null(zip)) {
    stop_glue("One of either borough or zip must be provided")
  }

  borough <- if_null_fill_na(borough, len) %>% clean_borough()
  zip <- if_null_fill_na(zip, len)

  # If unequal lengths tibble raises error with column names so change for API after
  dplyr::tibble(place, borough, zip) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    rlang::set_names("name", "borough", "zip")
}
