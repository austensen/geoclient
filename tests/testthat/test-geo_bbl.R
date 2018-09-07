context("geo_bbl")

library(dplyr)

test_that("geo_bbl() works", {

  df <- tibble::tibble(bbl = c("1005430053", "1005107502", NA_character_))

  bbl <- df[["bbl"]]

  vec_ret <- geo_bbl(bbl) %>% select(-ends_with("InternalLabel"))
  df_ret <- geo_bbl_data(df, bbl) %>% select(-ends_with("InternalLabel"))

  # The values for these "InternalLabel" columns are inconsistently returned by
  # the API (only off slightly far in the decimals). If you run the exact same
  # call multiple times it can give different values. This is unrelated to
  # problems with the dataframe vs vector approach.

  expect_identical(vec_ret, df_ret)
  expect_identical(vec_ret[["input_bbl"]], bbl)
  expect_identical(vec_ret[["sanbornPageNumber"]], c("006", "075", NA))
})

test_that("validate_bbl_inputs() works", {
  b_b_l <- tibble::tibble(borough = "1", block = "00543", lot = "0053")
  expect_identical(validate_bbl_inputs("1005430053"), b_b_l)

  expect_error(validate_bbl_inputs("6005430053"), "Invalid values for BBL")
  expect_error(validate_bbl_inputs("154353"), "Invalid values for BBL")
})
