context("geo_blockface")

test_that("geo_blockface() works", {

  df <- tibble::tribble(
    ~on_street,     ~cross_street_1,   ~cross_street_2, ~borough,
    "macdougal st", "washington sq s", "w 3rd st",      "mn"
  )

  on_street <- df[["on_street"]]
  cross_street_1 <- df[["cross_street_1"]]
  cross_street_2 <- df[["cross_street_2"]]
  borough <- df[["borough"]]

  vec_ret <- geo_blockface(on_street, cross_street_1, cross_street_2, borough) %>% select(-ends_with("InternalLabel"))
  df_ret <- geo_blockface_data(df, on_street, cross_street_1, cross_street_2, borough) %>% select(-ends_with("InternalLabel"))

  # The values for these "InternalLabel" columns are inconsistently returned by
  # the API (only off slightly far in the decimals). If you run the exact same
  # call multiple times it can give different values. This is unrelated to
  # problems with the dataframe vs vector approach.

  expect_identical(vec_ret, df_ret)
  expect_identical(vec_ret[["input_on_street"]], on_street)
  expect_identical(vec_ret[["firstStreetNameNormalized"]], "MACDOUGAL STREET")
})

test_that("input validator works", {

  expect_error(
    validate_blockface_inputs("macdougal st", "washington sq s", "w 3rd st", "mn", NULL, NULL, "SW"),
    "Invalid values for Compass Direction"
  )
})

test_that("input validator handles factors", {
  expect_all_cols_chr(
    validate_blockface_inputs(
      factor("macdougal st"),
      factor("washington sq s"),
      factor("w 3rd st"),
      factor("mn"),
      factor("mh"),
      factor("MN"),
      factor("S")
    )
  )
})
