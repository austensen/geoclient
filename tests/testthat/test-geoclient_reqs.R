context("geoclient_reqs")

creds <- get_creds()

inputs <- tibble::tribble(
  ~houseNumber,  ~street,        ~borough,      ~zip,
  "139",         "macdougal st", "mn",          "10012",
  "139",         "macdougal st", "mn",          "10012",
  "139",         "macdougal st", "mn",          NA_character_,
  "139",         "macdougal st", NA_character_, "10012",
  "139",         "macdougal st", NA_character_, NA_character_,
  NA_character_, "macdougal st", NA_character_, "10012"
)

test_that("placeholder dataframe still returned if inputs are invalid", {
  multi_inputs <- tibble(houseNumber = rep(517, 2), street = rep("clinton st", 2), borough = c(NA, "mn"), zip = rep(NA, 2))
  multi_ret <- geoclient_reqs(multi_inputs, creds = creds, operation = "address", rate_limit = TRUE)
  expect_identical(select(multi_ret, no_results), tibble(no_results = c(TRUE, FALSE)))

  single_inputs <- slice(multi_inputs, 1)
  single_ret <- geoclient_reqs(single_inputs, creds = creds,  operation = "address", rate_limit = TRUE )
  expect_identical(select(single_ret, no_results), tibble(no_results = TRUE))
})

test_that("invalid rate_limit value raises error", {
  expect_error(geoclient_reqs(inputs, "address", creds, rate_limit = 0), "must be either TRUE or FALSE")
})



