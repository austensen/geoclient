library(dplyr) # Load for use in all the tests

expect_error_free <- function(object) {
  expect_error(object, NA)
}

expect_all_cols_chr <- function(.data) {
  purrr::walk(.data, function(col) expect_type(col, "character"))
}
