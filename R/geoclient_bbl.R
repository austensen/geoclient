

geoclient_bbl <- function(df = NULL, bbl) {

  # If a dataframe is provided, get the vector from there, otherwise just use input vector
  if (!is.null(df)) {
    if (!is.data.frame(df)) {
      stop_glue("If a dataframe is not given as the first argument, the other argument must be named")
    }

    bbl <- enquo(bbl)
    bbl <- dplyr::pull(df, !!bbl)
  }

  # To avoid sending multiple requests for the same address, preserve original
  # inputs, use deduplicated version for request, then join the respone back to
  # original inputs before returning final result

  bbl_orig <- tibble::tibble(bbl = bbl)

  bbl_dedup <- dplyr::distinct(bbl_orig)

  pb <- dplyr::progress_estimated(nrow(bbl_dedup))

  # TODO: incorporate rate limiting, 2500/min
  res <- purrr::pmap_df(as_list(bbl_dedup), single_bbl, pb = pb)

  res <- dplyr::bind_cols(bbl_dedup, res)
  res <- dplyr::select(res, -bbl1)

  res <- dplyr::left_join(bbl_orig, res, by = c("bbl"))

  res
}

single_bbl <- function(bbl, pb = NULL, ...) {

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

  if (is.numeric(bbl)) {
    bbl <- as.integer(bbl)
  }

  bbl <- as.character(bbl)

  if (stringr::str_length(bbl) != 10) {
    stop_glue("BBL must be formatted as a 10-digit code, with 1-digit borough, 5-digit block, 4-digit lot")
  }

  borough <- stringr::str_sub(bbl, 1, 1)
  borough <- case_when(
    borough == "1" ~ "manhattan",
    borough == "2" ~ "bronx",
    borough == "3" ~ "brooklyn",
    borough == "4" ~ "queens",
    borough == "5" ~ "staten island"
  )

  block <- stringr::str_sub(bbl, 2, 6)
  lot <- stringr::str_sub(bbl, 7, 10)

  resp <- rGET(
    "https://api.cityofnewyork.us/geoclient/v1/bbl.json?",
    httr::accept_json(),
    query = list(
      "borough" = borough,
      "block" = block,
      "lot" = lot,
      "app_id" = Sys.getenv("GEOCLIENT_APP_ID"),
      "app_key" = Sys.getenv("GEOCLIENT_APP_KEY")
    )
  )

  httr::stop_for_status(resp)

  parsed <- content_as_json_UTF8(resp)[["bbl"]]

  tibble::as_tibble(parsed)
}


