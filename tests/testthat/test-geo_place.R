context("geo_place")

library(dplyr)

test_that("geo_place() works", {

  df <- tibble::tribble(
      ~place,          ~boro,        ~zip,
      "NYU",          NA_character_, "10012",
      "CITY HALL",    "MN",          NA_character_,
      "Pospect Park", "",            NA_character_
    )

  place <- df[["place"]]
  boro <- df[["boro"]]
  zip <- df[["zip"]]

  vec_ret <- geo_place(place, boro, zip) %>% select(-ends_with("InternalLabel"))
  df_ret <- geo_place_data(df, place, boro, zip) %>% select(-ends_with("InternalLabel"))

  # The values for these "InternalLabel" columns are inconsistently returned by
  # the API (only off slightly far in the decimals). If you run the exact same
  # call multiple times it can give different values. This is unrelated to
  # problems with the dataframe vs vector approach.

  expect_identical(vec_ret, df_ret)
  expect_identical(vec_ret[["input_place"]], place)
  expect_identical(vec_ret[["healthArea"]], c("5700", "7700", NA))
})

test_that("validate_place_inputs() works", {
  expect_error(
    validate_place_inputs("city hall", NULL, NULL),
    "One of either borough or zip must be provided"
  )
})
