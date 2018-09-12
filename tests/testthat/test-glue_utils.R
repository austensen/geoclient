context("glue_utils")


test_that("raise correct output types", {
  x <- "foo"

  expect_message(msg_glue("{x}bar"), "foobar")
  expect_warning(warn_glue("{x}bar"), "foobar")
  expect_error(stop_glue("{x}bar"), "foobar")
})

test_that("handle nested environments correctly", {
  x <- "foo"

  nested_msg_glue <- function(x) msg_glue("{x}")
  nested_warn_glue <- function(x) warn_glue("{x}")
  nested_stop_glue <- function(x) stop_glue("{x}")

  # If the environment is not handled properly these would all give "foo"
  expect_message(nested_msg_glue("bar"), "bar")
  expect_warning(nested_warn_glue("bar"), "bar")
  expect_error(nested_stop_glue("bar"), "bar")

})
