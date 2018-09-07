# Adapted from Jenny Bryan's googlesheets package

msg_glue <- function(..., .envir = parent.frame(), type = NULL, call = FALSE) {
  rlang::inform(glue::glue(..., .envir = .envir), type = type, call = call)
}

warn_glue <- function(..., .envir = parent.frame(), type = NULL, call = FALSE) {
  rlang::warn(glue::glue(..., .envir = .envir), type = type, call = call)
}

stop_glue <- function(..., .envir = parent.frame(), type = NULL, call = FALSE) {
  rlang::abort(glue::glue(..., .envir = .envir), type = type, call = call)
}
