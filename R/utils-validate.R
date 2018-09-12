# util functions for validating user-provided arguments

# Clean up borough inputs - both text and DCP boro codes.
clean_borough <- function(borough) {

  if (is_null(borough)) return(NULL)

  detect <- function(string, pattern) {
    pattern <- paste0("^\\s*", pattern, "\\s*$")
    stringr::str_detect(string, stringr::regex(pattern, ignore_case = TRUE))
  }

  borough <- as.character(borough)

  dplyr::case_when(
    detect(borough, "(1)|(mn)|(manhattan)|(new\\s*york)")     ~ "manhattan",
    detect(borough, "(2)|(bx)|(bronx)")                       ~ "bronx",
    detect(borough, "(3)|(bk)|(kings)|(brooklyn)")            ~ "brooklyn",
    detect(borough, "(4)|(qn)|(qu)|(queens)")                 ~ "queens",
    detect(borough, "(5)|(si)|(richmond)|(staten\\s*island)") ~ "staten island",
    TRUE ~ borough
  )
}

# If x is null return NA (character) for a given length, otherwise return x
if_null_fill_na <- function(x, n) {
  x %||% rep(rlang::na_chr, n)
}

# For a given dataframe and "enquo(col)", return null if the quo is null,
# otherwise return the column pulled as a vector
pull_or_null <- function(.data, quo_col) {
  if (quo_is_null(quo_col)) {
    return(NULL)
  } else {
    return(dplyr::pull(.data, !!quo_col))
  }
}
