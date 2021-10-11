.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    msg_glue(
      "To avoid entering your Geoclient API key with each call and saving it in your code, ",
      "you can use `geoclient_api_key` to store them for future use.
      See ?geoclient_api_key for details."
    )
  )
}

utils::globalVariables(".")
