context("geo_address")

test_that("vector and dataframe versions work identically", {

  df <- tibble::tribble(
    ~num,  ~st,                ~boro,         ~zip,
    139, "MacDougal St",     "manhattan",   "11231",
    295, "Lafayette street", NA_character_, "10012-2722",
    40,  "WASHINGTON SQ S",  "MN",          NA_character_
  )

  num <- df[["num"]]
  st <- df[["st"]]
  boro <- df[["boro"]]
  zip <- df[["zip"]]

  vec_ret <- geo_address(num, st, boro, zip) %>% select(-ends_with("InternalLabel"))
  df_ret <- geo_address_data(df, num, st, boro, zip) %>% select(-ends_with("InternalLabel"))

  expect_identical(vec_ret, df_ret)
  expect_identical(vec_ret[["input_street"]], st)
  expect_identical(vec_ret[["bbl"]], c("1005430053", "1005107502", "1005410001"))
})

test_that("input validator handles factors", {
  expect_error_free(validate_address_inputs(factor(139), factor("macdougal"), factor("MN"), factor(10012)))
})

test_that("input validator reqires borough or zip", {
  expect_error_free(validate_address_inputs(139, "macdougal", "MN", NULL))
  expect_error_free(validate_address_inputs(139, "macdougal", NULL, "10012"))
  expect_error(validate_address_inputs(139, "macdougal", NULL, NULL), "borough or zip must be provided")
})

test_that("input validator requires number and street", {
  expect_error(validate_address_inputs(NULL, "macdougal", "MN", NULL), "Column `house_number` must be")
  expect_error(validate_address_inputs(139, NULL, "MN", NULL), "Column `street` must be")
})

test_that("input validator requires equal length inputs", {
  expect_error(validate_address_inputs(c(1, 2), c("a", "b", "c"), "MN", NULL), "must be length")
})
