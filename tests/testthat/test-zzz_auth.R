context("deauthorization")

# Clean up creds from environment after testing
suppressMessages(geoclient_api_keys(id = NULL, key = NULL, overwrite = TRUE, install = TRUE))

test_that("environment cleaned", {
  expect_identical(Sys.getenv("GEOCLIENT_APP_ID"), "")
  expect_identical(Sys.getenv("GEOCLIENT_APP_KEY"), "")
})
