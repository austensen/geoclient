context("helpers")

test_that("invalid rows are dropped", {

  all_inputs <- tibble::tribble(
    ~houseNumber,  ~street,        ~borough,      ~zip,
    "139",         "macdougal st", "mn",          "10012",
    "139",         "macdougal st", "mn",          NA_character_,
    "139",         "macdougal st", NA_character_, "10012",
    "139",         "macdougal st", NA_character_, NA_character_,
    NA_character_, "macdougal st", "mn",          "10012",
    "139",         NA_character_,  "mn",          "10012"
  )

  valid_inputs <- all_inputs %>% dplyr::slice(1:3)

  expect_identical(drop_invalid_rows(all_inputs, "address"), valid_inputs)

})

test_that("fix_input_names() works", {
  input <- mtcars
  output <- fix_input_names(mtcars, "address")

  expect_equal(class(input), class(output))
  expect_match(dplyr::all_equal(input, output)[[1]], "Cols in y but not x")

  # bbl is special case where boro, block, and lot are concatenated to match
  # what the user provdides (versus what the api accepts)
  bbl_ret <- tibble::tibble(borough = "1", block = "00543", lot = "0053") %>%
    fix_input_names("bbl")

  expect_identical(bbl_ret, tibble::tibble(input_bbl = "1005430053"))
})


test_that("clean_borough() works", {
  expect_identical(clean_borough(" 1 "), "manhattan")
  expect_identical(clean_borough(1), "manhattan")
  expect_identical(clean_borough("MN"), "manhattan")
  expect_identical(clean_borough("new   York"), "manhattan")
  expect_identical(clean_borough(NULL), NULL)

  # If not identified as borough, returns input as-is
  expect_identical(clean_borough(c("mn", NA, "nassau")), c("manhattan", NA, "nassau"))
})

test_that("if_null_fill_na() works", {
  expect_identical(if_null_fill_na(NULL, 2), c(NA_character_, NA_character_))
  expect_identical(if_null_fill_na(1, 2), 1)
})

test_that("pull_or_null() works", {
  test_call <- function(x) pull_or_null(mtcars, enquo(x))
  expect_identical(test_call(mpg), dplyr::pull(mtcars, mpg))
  expect_identical(test_call(NULL), NULL)
})
