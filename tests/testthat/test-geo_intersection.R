context("geo_intersection")

library(dplyr)

test_that("geo_intersection() works", {

  df <- tibble::tribble(
    ~cross_street_1, ~cross_street_2, ~borough,
    "macdougal st",  "w 3rd st",      "mn"
  )

  cross_street_1 <- df[["cross_street_1"]]
  cross_street_2 <- df[["cross_street_2"]]
  borough <- df[["borough"]]

  vec_ret <- geo_intersection(cross_street_1, cross_street_2, borough) %>% select(-ends_with("InternalLabel"))
  df_ret <- geo_intersection_data(df, cross_street_1, cross_street_2, borough) %>% select(-ends_with("InternalLabel"))

  # The values for these "InternalLabel" columns are inconsistently returned by
  # the API (only off slightly far in the decimals). If you run the exact same
  # call multiple times it can give different values. This is unrelated to
  # problems with the dataframe vs vector approach.

  expect_identical(vec_ret, df_ret)
  expect_identical(vec_ret[["input_cross_street_1"]], cross_street_1)
  expect_identical(vec_ret[["firstStreetNameNormalized"]], "MACDOUGAL STREET")
})

test_that("validate_intersection_inputs() works", {

  expect_error(
    validate_intersection_inputs("washington sq s", "w 3rd st", "mn", NULL, "SW"),
    "Invalid values for Compass Direction"
  )
})
