# Adapted from Jenny Bryan's googlesheets package

cat_glue <- function(...) cat(glue::glue(..., .sep = "\n"))
msg_glue <- function(...) message(glue::glue(...))
warn_glue <- function(...) warning(glue::glue(...), call. = FALSE)
stop_glue <- function(...) stop(glue::glue(...), call. = FALSE)
