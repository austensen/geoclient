context("geo_bin")

library(dplyr)

test_that("geo_bin() works", {

  df <- tibble::tibble(bin = c("1008760", "1007941", NA_character_))

  bin <- df[["bin"]]

  vec_ret <- geo_bin(bin) %>% select(-ends_with("InternalLabel"))
  df_ret <- geo_bin_data(df, bin) %>% select(-ends_with("InternalLabel"))

  # The values for these "InternalLabel" columns are inconsistently returned by
  # the API (only off slightly far in the decimals). If you run the exact same
  # call multiple times it can give different values. This is unrelated to
  # problems with the dataframe vs vector approach.

  expect_identical(vec_ret, df_ret)
  expect_identical(vec_ret[["input_bin"]], bin)
  expect_identical(vec_ret[["sanbornPageNumber"]], c("006", "075", NA))
})

test_that("validate_bin_inputs() works", {
  expect_error(validate_bin_inputs("6008760"), "Invalid values for BIN")
  expect_error(validate_bin_inputs("100876"), "Invalid values for BIN")
})