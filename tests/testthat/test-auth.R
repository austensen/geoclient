context("authorization")

test_that("api credentials are not set", {
  expect_equal(Sys.getenv("GEOCLIENT_APP_ID"), "")
  expect_equal(Sys.getenv("GEOCLIENT_APP_KEY"), "")
})

# access encrypted api credentials
creds <- readRDS("geoclient_app_creds.rds")
id <- creds$id
key <- creds$key

# make prespecified functions for readability in tests
# nolint start
set_creds <- function() geoclient_api_keys(id = id, key = key)
install_creds <- function() geoclient_api_keys(id = id, key = key, install = TRUE)
remove_creds <- function() geoclient_api_keys(id = NULL, key = NULL, install = TRUE, overwrite = TRUE)
overwrite_creds <- function() geoclient_api_keys(id = id, key = key, install = TRUE, overwrite = TRUE)
# nolint end

test_that("setting api credentials works with defaults", {

  expect_message(set_creds(), "future sessions")

  expect_equal(Sys.getenv("GEOCLIENT_APP_ID"), id)
  expect_equal(Sys.getenv("GEOCLIENT_APP_KEY"), key)

  Sys.unsetenv("GEOCLIENT_APP_ID")
  Sys.unsetenv("GEOCLIENT_APP_KEY")

  expect_equal(Sys.getenv("GEOCLIENT_APP_ID"), "")
  expect_equal(Sys.getenv("GEOCLIENT_APP_KEY"), "")
})


test_that("installing/overwriting api credentials in `.Renviron` works", {

  expect_message(install_creds(), "have been stored")
  readRenviron("~/.Renviron") # nolint

  expect_equal(Sys.getenv("GEOCLIENT_APP_ID"), id)
  expect_equal(Sys.getenv("GEOCLIENT_APP_KEY"), key)

  expect_error(install_creds(), "already exists")
})

test_that("removing api credentials from `.Renviron` works", {

  expect_message(remove_creds(), "have been removed")

  Sys.unsetenv("GEOCLIENT_APP_ID")
  Sys.unsetenv("GEOCLIENT_APP_KEY")

  readRenviron("~/.Renviron") # nolint

  expect_equal(Sys.getenv("GEOCLIENT_APP_ID"), "")
  expect_equal(Sys.getenv("GEOCLIENT_APP_KEY"), "")

})


test_that("overwriting api credentials in `.Renviron` works", {

  suppressMessages(geoclient_api_keys(id = "foo", key = "bar", install = TRUE))

  expect_message(overwrite_creds(), "backed up")
  expect_message(overwrite_creds(), "have been stored")
  readRenviron("~/.Renviron") # nolint

  expect_equal(Sys.getenv("GEOCLIENT_APP_ID"), id)
  expect_equal(Sys.getenv("GEOCLIENT_APP_KEY"), key)
})


# set credentials for test of testing
suppressMessages(set_creds())

# clean up after tests
suppressMessages(remove_creds())
