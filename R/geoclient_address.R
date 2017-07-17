

single_address <- function(number, street, borough = NULL, zip = NULL, pb = NULL, ...) {

  if (Sys.getenv('GEOCLIENT_APP_ID') != '' || Sys.getenv('GEOCLIENT_APP_KEY') != '') {

    id <- Sys.getenv('CENSUS_API_ID')
    key <- Sys.getenv('CENSUS_API_KEY')

  } else if (is.null(id) || is.null(key)) {

    stop_glue(
      "A Geoclient app ID and key are required.
      Obtain them at https://developer.cityofnewyork.us/user/register?destination=api,",
      "and then supply them to the `geoclient_api_keys` function to avoid entering them with each call."
    )
  }

  # For creating a progress bar
  # Thanks to Bob Rudis' post: https://rud.is/b/2017/05/05/scrapeover-friday-a-k-a-another-r-scraping-makeover/
  if (!is.null(pb)) pb$tick()$print()

  if (!is.null(borough)) {
    if (is.na(borough)) {
      borough <- NULL
    }
  } else {
    borough <- case_when(
      stringr::str_detect(borough, "^(?i)(mn)|(manhattan)(new york)")     ~ "brooklyn",
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


geoclient_address <- function(number, street, borough = NULL, zip = NULL) {

  len <- length(number)

  pb <- dplyr::progress_estimated(len)

  if (is.null(zip)) {
    zip <- rep(NA_character_, len)
  }

  if (is.null(borough)) {
    borough <- rep(NA_character_, len)
  }

  # TODO: incorporate rate limiting, 2500/min
  purrr::pmap_df(
    list(
      "number" = number,
      "street" = street,
      "borough" = borough,
      "zip" = zip
    ),
    single_address,
    pb = pb
  )
}


single_address("139", "MacDougal St", "Manhattan", "10012")

single_address("139", "MacDougal St", "Manhattan", "10012")[["bbl"]]

library(tibble)
library(dplyr)

addr_df <- tribble(
  ~num,  ~st,             ~boro,      ~zip,
  "139", "MacDougal St", "Manhattan", NA,
  "114", "east 1st street", NA,          "10009-7924"
)

mutate(addr_df, bbl = geoclient_address(num, st, boro, zip)[["bbl"]])


single_address("517", "clinton st", "brooklyn")
single_address("414", "jersey st", "staten island")
single_address(517, "clinton st", "brooklyn")

geoclient_address(
  c(168, 517),
  c("ludlow st", "clinton st"),
  c("manhattan", NA),
  c("10002", "11231-3350")
)[["bbl"]] -> foo

