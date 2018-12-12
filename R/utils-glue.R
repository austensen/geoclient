# Adapted from Jenny Bryan's googlesheets package

msg_glue <- function(..., .envir = parent.frame(), .subclass = NULL) {
  rlang::inform(glue::glue(..., .envir = .envir), .subclass = .subclass)
}

warn_glue <- function(..., .envir = parent.frame(), .subclass = NULL) {
  rlang::warn(glue::glue(..., .envir = .envir), .subclass = .subclass)
}

stop_glue <- function(..., .envir = parent.frame(), .subclass = NULL) {
  rlang::abort(glue::glue(..., .envir = .envir), .subclass = .subclass)
}
