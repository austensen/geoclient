context("API credentials")


# Setup -------------------------------------------------------------------

test_credentials <- function(creds) {
  resp <- rGET(
    glue::glue("https://api.cityofnewyork.us/geoclient/v1/place.json?"),
    httr::accept_json(),
    query = list(
      name = "Empire State Building",
      borough = "manhattan",
      app_id = creds[["id"]],
      app_key = creds[["key"]]
    )
  )

  httr::stop_for_status(resp)

  parsed <- content_as_json_UTF8(resp)

  parsed[["place"]]
}

id <- secret::get_secret("app_id", vault = "inst/vault")
key <- secret::get_secret("app_key", vault = "inst/vault")

creds <- list(id = id, key = key)


# Tests -------------------------------------------------------------------

test_that("API credentials are valid",
  expect_equal(test_credentials(creds)[["bbl"]], "1008350041")
)



# test that make inputs return geoclient's error response (eg dummy bins)

# test that set of inputs with valid and invalid elements returns tibble with all columns


