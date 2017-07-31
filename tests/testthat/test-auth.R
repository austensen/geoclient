context("authorization")

test_that("api credientials are installed", {
  expect_true(Sys.getenv("GEOCLIENT_APP_ID") != "")
  expect_true(Sys.getenv("GEOCLIENT_APP_KEY") != "")
})
