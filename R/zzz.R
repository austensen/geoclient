.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    msg_glue(
      "To avoid entering your Geoclient API app ID and key with each call and saving them in your code, ",
      "you can use `geoclient_api_keys` to store them for future use.
      See ?geoclient_api_keys for details."
    )
  )
}

utils::globalVariables(".")
