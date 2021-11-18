# These functions were borrowed fron Jenny Bryan's googlesheets package
# after reading her great post:
# http://stat545.com/bit007_draw-the-rest-of-the-owl.html

stop_for_content_type <- function(req, expected) {
  actual <- req$headers$`Content-Type`
  if (!stringr::str_detect(actual, expected)) {
    stop_glue(
      "Expected content-type:
         {expected}

       Actual content-type:
         {actual}"
    )
  }
  invisible(NULL)
}

content_as_json_UTF8 <- function(req) {
  stop_for_content_type(req, expected = "application/json(;charset=UTF-8)?")
  jsonlite::fromJSON(httr::content(req, as = "text", encoding = "UTF-8"))
}

# Addapted from httr::stop_for_status(). For this package http:500 shouldn't
# raise an error here, because these cases are handled to return a placeholder
geoclient_stop_for_status <- function(x, task = NULL) {

  if (httr::status_code(x) < 300 || httr::status_code(x) %in% c(400, 429, 500)) {
    return(invisible(x))
  }

  call <- sys.call(-1)
  stop(httr::http_condition(x, "error", task = task, call = call))
}

VERB_n <- function(VERB, n = 3) {
  force(VERB)
  force(n)
  function(...) {
    for (i in seq_len(n)) {
      out <- VERB(...)
      status <- httr::status_code(out)
      if (status != "429" && (status < 499 || i == n)) break
      backoff <- stats::runif(n = 1, min = 0.5, max = 2 ^ i - 1)
      msg_glue(
        "HTTP error {status} on attempt {i} ...
          backing off {round(backoff, 2)} seconds, retrying"
      )
      Sys.sleep(backoff)
    }
    out
  }
}

rGET <- VERB_n(httr::GET)
