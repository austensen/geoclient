context("geo_bin")

test_that("geo_bin() works", {

  df <- tibble(bin = c("1007941", "1088641", NA_character_))

  bin <- df[["bin"]]

  vec_ret <- geo_bin(bin) %>% select(-ends_with("InternalLabel"))
  df_ret <- geo_bin_data(df, bin) %>% select(-ends_with("InternalLabel"))

  # The values for these "InternalLabel" columns are inconsistently returned by
  # the API (only off slightly far in the decimals). If you run the exact same
  # call multiple times it can give different values. This is unrelated to
  # problems with the dataframe vs vector approach.

  expect_identical(vec_ret, df_ret)
  expect_identical(vec_ret[["input_bin"]], bin)
  expect_identical(vec_ret[["sanbornPageNumber"]], c("075", "006", NA))
})

test_that("input validator catches invalid BINs", {
  bad_bins <- c(
    "6008760",
    "1000000",
    "12345678",
    "123456",
    "1abcdef"
  )
  purrr::walk(bad_bins, ~expect_error(validate_bin_inputs(.x), "Invalid values for BIN"))
})

test_that("input validator catches invalid BINs", {
  bad_bins <- c(
    "6008760",
    "1000000",
    "12345678",
    "123456",
    "1abcdef"
  )
  purrr::walk(bad_bins, ~expect_error(validate_bin_inputs(.x), "Invalid values for BIN"))
})

test_that("input validator handles factors", {
  expect_all_cols_chr(validate_bin_inputs(factor(1234567)))
})
