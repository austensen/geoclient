context("geoclient_req")

creds <- get_creds()

test_that("opertion=address works", {
  input <- tibble::tribble(
    ~houseNumber,  ~street,        ~borough,      ~zip,
    "139",         "macdougal st", "mn",          "10012",
    "139",         "macdougal st", "mn",          NA_character_,
    "139",         "macdougal st", NA_character_, "10012",
    "139",         "macdougal st", "",            NA_character_
  )

  res <- purrr::pmap(input, geoclient_req, operation = "address", creds = creds)

  expect_identical(res[[1]][["bbl"]], "1005430053")
  expect_identical(res[[2]][["bbl"]], "1005430053")
  expect_identical(res[[3]][["bbl"]], "1005430053")
  expect_identical(res[[4]][["message"]], "INVALID BOROUGH CODE. MUST BE 1, 2, 3, 4 OR 5.")

})

test_that("opertion=bbl works", {

  input <- tibble::tribble(
    ~borough, ~block,  ~lot,
    "1",      "00543", "0053",
    "",       "",      ""
  )

  res <- purrr::pmap(input, geoclient_req, operation = "bbl", creds = creds)

  expect_identical(res[[1]][["buildingIdentificationNumber"]], "1088641")
  expect_identical(res[[2]][["message"]], "BOROUGH CODE IS MISSING")
})



test_that("opertion=bin works", {

  input <- tibble(bin = c("1088641", ""))

  res <- purrr::pmap(input, geoclient_req, operation = "bin", creds = creds)

  expect_identical(res[[1]][["bbl"]], "1005430053")
  expect_identical(res[[2]][["message"]], "BUILDING IDENTIFICATION NUMBER (BIN) IS MISSING")
})



test_that("opertion=blockface works", {

  input <- tibble::tribble(
    ~onStreet,      ~crossStreetOne,   ~crossStreetTwo, ~borough,
    "macdougal st", "washington sq s", "w 3rd st",      "mn",
    "",             "",                "",              ""
  )

  res <- purrr::pmap(input, geoclient_req, operation = "blockface", creds = creds)

  expect_identical(res[[1]][["bikeLane"]], "2")
  expect_identical(res[[2]][["message"]], "NO INPUT DATA RECEIVED")
})


test_that("opertion=intersection works", {

  input <- tibble::tribble(
    ~crossStreetOne, ~crossStreetTwo, ~borough,
    "macdougal st",  "w 3rd st",      "mn",
    "",              "",              ""
  )

  res <- purrr::pmap(input, geoclient_req, operation = "intersection", creds = creds)

  expect_identical(res[[1]][["assemblyDistrict"]], "66")
  expect_identical(res[[2]][["message"]], "INVALID BOROUGH CODE. MUST BE 1, 2, 3, 4 OR 5.")
})


test_that("opertion=place works", {

  input <- tibble::tribble(
    ~name,          ~borough,      ~zip,
    "NYU",          NA_character_, "10012",
    "CITY HALL",    "MN",          NA_character_,
    "Pospect Park", "",            NA_character_
  )
  res <- purrr::pmap(input, geoclient_req, operation = "place", creds = creds)

  expect_identical(res[[1]][["assemblyDistrict"]], "66")
  expect_identical(res[[2]][["message"]], "254 BROADWAY IS THE UNDERLYING ADDRESS OF CITY HALL")
  expect_identical(res[[3]][["message"]], "INVALID BOROUGH CODE. MUST BE 1, 2, 3, 4 OR 5.")
})


test_that("opertion=search works", {

  input <- tibble(input = c("139 MacDougal Street, New York, 10012", "", NA_character_))

  res <- purrr::pmap(input, geoclient_req, operation = "search", creds = creds)

  expect_identical(res[[1]][["assemblyDistrict"]], "66")
  expect_identical(res[[2]][["no_results"]], TRUE)
  expect_identical(res[[3]][["no_results"]], TRUE)
})

test_that("http:500 errors are handled for invlaid inputs", {

  # Search operation returns http:500 error when it can't find results. But we
  # should still getback a placeholder tibble.
  search_req <- purrr::partial(geoclient_req, input = "?", operation = "search", creds = creds)

  expect_message(search_req(), "HTTP error 500")
  expect_identical(search_req()[["no_results"]], TRUE)
})


test_that("error for invalid api creds", {
  bad_creds_call <- purrr::partial(
    geoclient_req,
    borough = "1",
    block = "00543",
    lot = "0053",
    operation = "bbl",
    creds = list(app_id = "foo", app_key = "bar")
  )

  expect_error(bad_creds_call(), "Authentication failed: Geoclient API app ID and/or Key are invalid")
})


test_that("placeholder returned if inputs are invalid", {

  ret <- geoclient_req(
    houseNumber = 139,
    street = "macdougal st",
    borough = NA, # either borough or zip required, so this invalid
    zip = NA,
    creds = get_creds(),
    operation = "address"
  )

  expect_identical(ret, tibble(no_results = TRUE))

})
