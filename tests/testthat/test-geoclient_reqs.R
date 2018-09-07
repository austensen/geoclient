context("geoclient_reqs")

library(dplyr)

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

test_that("handles invalid inputs", {

  ret <- geoclient_reqs(inputs, "address", creds, TRUE)

  ret_inputs <- ret %>%
    select(1:4) %>%
    purrr::set_names(c("houseNumber", "street", "borough", "zip"))

  expect_identical(inputs, ret_inputs)
  expect_identical(ret[["no_results"]], c(F, F, F, F, T, T))

})


test_that("invalid rate_limit value raises error", {
  expect_error(geoclient_reqs(inputs, "address", creds, rate_limit = 0), "must be either TRUE or FALSE")
})
