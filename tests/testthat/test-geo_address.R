context("geo_address")

library(dplyr)

test_that("geo_address() works", {

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

test_that("validate_address_inputs() works", {

  expect_error(
    validate_address_inputs(139, "macdougal", NULL, NULL),
    "One of either borough or zip must be provided"
  )

  expect_error(
    validate_address_inputs(NULL, "macdougal", "MN", NULL),
    "Column `house_number` must be a 1d atomic vector or a list"
  )

  expect_error(validate_address_inputs(c(1, 2), c("a", "b", "c"), "MN", NULL), "must be length")

})
