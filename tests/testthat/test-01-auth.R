context("authorization")

# access encrypted api credentials

# The "creds" list used throughout the package has since changed from "id" and
# "key" to "app_id" and "app_key", but I haven't changed the Travis/Appveyor.
# This doesn't really affect anything in these tests...
# See https://github.com/hadley/secure if need to update

# TODO: they've now changed it to just key, will now need to update with new valid key

creds <- readRDS("geoclient_app_creds.rds")
creds <- purrr::set_names(creds, "key")
key <- creds$key

# make prespecified functions for readability in tests
set_creds <- purrr::partial(geoclient_api_key, key = key)
install_creds <- purrr::partial(geoclient_api_key, key = key, install = TRUE)
remove_creds <- purrr::partial(geoclient_api_key, key = NULL, install = TRUE, overwrite = TRUE)
overwrite_creds <- purrr::partial(geoclient_api_key, key = key, install = TRUE, overwrite = TRUE)

# Make sure nothing set before testing
remove_creds()
readRenviron("~/.Renviron")


expect_creds <- function(status = c("set", "unset")) {
  if (status == "set") {
    expect_identical(Sys.getenv("GEOCLIENT_KEY"), key)
  } else if (status == "unset") {
    expect_identical(Sys.getenv("GEOCLIENT_KEY"), "")
  }
}


test_that("api credentials are not set", {
  expect_creds("unset")
})

test_that("setting api credentials works with defaults", {

  expect_message(set_creds(), "future sessions")
  expect_creds("set")

  remove_creds()
  expect_creds("unset")
})


test_that("installing/overwriting api credentials in `.Renviron` works", {

  expect_message(install_creds(), "has been stored")
  readRenviron("~/.Renviron")

  expect_creds("set")

  expect_error(install_creds(), "already exists")
  remove_creds()
})

test_that("removing api credentials from `.Renviron` works", {

  expect_message(remove_creds(), "has been removed")
  readRenviron("~/.Renviron")

  expect_creds("unset")
  remove_creds()
})


test_that("overwriting api credentials in `.Renviron` works", {

  suppressMessages(geoclient_api_key(key = "foo", install = TRUE))

  expect_message(overwrite_creds(), "backed up")
  expect_message(overwrite_creds(), "has been stored")
  readRenviron("~/.Renviron")

  expect_creds("set")
  remove_creds()
})

test_that("get_creds() works", {

  remove_creds()
  expect_error(get_creds(), "Geoclient API key is required")
  expect_identical(get_creds(key = key), creds)

  set_creds()
  expect_identical(get_creds(), creds)
  remove_creds()
})

# Set creds for rest of testing (cleaned up at end)
suppressMessages(install_creds())
readRenviron("~/.Renviron")
