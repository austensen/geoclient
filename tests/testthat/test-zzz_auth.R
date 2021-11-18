context("deauthorization")

# Clean up creds from environment after testing
suppressMessages(geoclient_api_key(key = NULL, overwrite = TRUE, install = TRUE))

test_that("environment cleaned", {
  expect_identical(Sys.getenv("GEOCLIENT_KEY"), "")
})
