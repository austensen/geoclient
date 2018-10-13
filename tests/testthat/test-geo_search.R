context("geo_search")

test_that("geo_search() works", {

  df <- tibble(location = c("139 MacDougal Street, New York, 10012", "", NA_character_))

  location <- df[["location"]]

  vec_ret <- geo_search(location) %>% select(-ends_with("InternalLabel"))
  df_ret <- geo_search_data(df, location) %>% select(-ends_with("InternalLabel"))

  # The values for these "InternalLabel" columns are inconsistently returned by
  # the API (only off slightly far in the decimals). If you run the exact same
  # call multiple times it can give different values. This is unrelated to
  # problems with the dataframe vs vector approach.

  expect_identical(vec_ret, df_ret)
  expect_identical(vec_ret[["input_location"]], location)
  expect_identical(vec_ret[["communityDistrict"]], c("102", NA, NA))
})

test_that("input validator handles factors", {
  expect_all_cols_chr(validate_search_inputs(factor("139 MacDougal Street, New York, 10012")))
})
