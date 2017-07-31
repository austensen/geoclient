# These functions were borrowed fron Jenny Bryan's googlesheets package
# after reading her great post:
# http://stat545.com/bit007_draw-the-rest-of-the-owl.html

stop_for_content_type <- function(req, expected) {
  actual <- req$headers$`Content-Type`
  if (actual != expected) {
    stop_glue(
      "Expected content-type:
         {expected}

       Actual content-type:
         {actual}"
    )
  }
  invisible(NULL)
}

content_as_json_UTF8 <- function(req) { # nolint
  stop_for_content_type(req, expected = "application/json;charset=UTF-8")
  jsonlite::fromJSON(httr::content(req, as = "text", encoding = "UTF-8"))
}


VERB_n <- function(VERB, n = 3) { # nolint
  force(VERB)
  force(n)
  function(...) {
    for (i in seq_len(n)) {
      out <- VERB(...)
      status <- httr::status_code(out)
      if (status < 499 || i == n) break
      backoff <- stats::runif(n = 1, min = 0, max = 2 ^ i - 1)
      msg_glue(
        "HTTP error {status} on attempt {i} ...
          backing off {round(backoff, 2)} seconds, retrying"
      )
      Sys.sleep(backoff)
    }
    out
  }
}

rGET <- VERB_n(httr::GET) # nolint
