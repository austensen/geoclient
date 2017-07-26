


geoclient_bin <- function(df = NULL, bin) {

  # If a dataframe is provided, get the vector from there, otherwise just use input vector
  if (!is.null(df)) {
    if (!is.data.frame(df)) {
      stop_glue("If a dataframe is not given as the first argument, the other argument must be named")
    }

    bin <- enquo(bin)
    bin <- dplyr::pull(df, !!bin)
  }

  # To avoid sending multiple requests for the same address, preserve original
  # inputs, use deduplicated version for request, then join the respone back to
  # original inputs before returning final result

  bin_orig <- tibble::tibble(bin = bin)

  bin_dedup <- dplyr::distinct(bin_orig)

  pb <- dplyr::progress_estimated(nrow(bin_dedup))

  # TODO: incorporate rate limiting, 2500/min
  res <- purrr::pmap_df(as_list(bin_dedup), single_bin, pb = pb)

  res <- dplyr::bind_cols(bin_dedup, res)

  res <- dplyr::left_join(bin_orig, res, by = c("bin"))

  res
}

single_bin <- function(bin, pb = NULL, ...) {

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

  if (is.numeric(bin)) {
    bin <- as.integer(bin)
  }

  bin <- as.character(bin)

  if (stringr::str_length(bin) != 7) {
    stop_glue("BIN must be formatted as a 7-digit code, with the first digit being the borough code")
  }

  resp <- rGET(
    "https://api.cityofnewyork.us/geoclient/v1/bin.json?",
    httr::accept_json(),
    query = list(
      "bin" = bin,
      "app_id" = Sys.getenv("GEOCLIENT_APP_ID"),
      "app_key" = Sys.getenv("GEOCLIENT_APP_KEY")
    )
  )

  httr::stop_for_status(resp)

  parsed <- content_as_json_UTF8(resp)[["bin"]]

  tibble::as_tibble(parsed)
}
