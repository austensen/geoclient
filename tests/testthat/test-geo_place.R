context("geo_place")

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


  # There are a few columns that are inconsistently returned by the API (only
  # off slightly far in the decimals). If you run the exact same call multiple
  # times it can give different values. This is unrelated to problems with the
  # dataframe vs vector approach. So select only the first few columns for
  # comparison

  vec_ret <- geo_place(place, boro, zip) %>% select(1:9)
  df_ret <- geo_place_data(df, place, boro, zip) %>% select(1:9)

  expect_identical(vec_ret, df_ret)
  expect_identical(vec_ret[["input_place"]], place)
  expect_identical(vec_ret[["bbl"]], c("1005350001", "1001220001", NA))
})

test_that("input validator reqires borough or zip", {
  expect_error(validate_place_inputs("city hall", NULL, NULL), "borough or zip must be provided")
  expect_error_free(validate_place_inputs("city hall", "ny", NULL))
  expect_error_free(validate_place_inputs("city hall", "Mn", NULL))
  expect_error_free(validate_place_inputs("city hall", NULL, 10007))
})

test_that("input validator handles factors", {
  expect_all_cols_chr(validate_place_inputs(factor("city hall"), factor("mn"), factor(10007)))
})
