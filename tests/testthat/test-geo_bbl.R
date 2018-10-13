context("geo_bbl")

test_that("geo_bbl() works", {

  df <- tibble(bbl = c("1005430053", "1005107502", NA_character_))

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

test_that("input validator splits BBLs correctly", {
  b_b_l <- tibble(borough = "1", block = "00543", lot = "0053")
  expect_identical(validate_bbl_inputs("1005430053"), b_b_l)
})

test_that("input validator catches invalid BBLs", {
  bad_bbls <- c(
    "6005430053",
    "1000001234",
    "1000000000",
    "1123450000",
    "112345678",
    "11234567890",
    "1abcdefghi"
  )
  purrr::walk(bad_bbls, ~expect_error(validate_bbl_inputs(.x), "Invalid values for BBL"))
})

test_that("input validator handles factors", {
  expect_all_cols_chr(validate_bbl_inputs(factor(1123451234)))
})
