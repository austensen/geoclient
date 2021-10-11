#' Install Geoclient API Key for Repeated Use
#'
#' This function will add your Geoclient app ID and key to your `.Renviron` file
#' so they can be called securely without being stored in your code. After you
#' have installed your key, it can be called any time by typing
#' `Sys.getenv("GEOCLIENT_KEY")` and can be used in package functions by simply
#' typing GEOCLIENT_KEY. If you do not have an `.Renviron` file, the function
#' will create on for you. If you already have an `.Renviron` file, the function
#' will append the key to your existing file, while making a backup of
#' your original file for disaster recovery purposes. You can acquire your
#' Geoclient API Key by first registering with the [NYC's API
#' Portal](https://api-portal.nyc.gov/) at, then adding a "subscription" to the
#' [Geoclient User](https://api-portal.nyc.gov/products/geoclient-user) API.
#'
#' @param key The API key provided to you from the NYC Developer Portal
#'   formatted in quotes.
#' @param install if TRUE, will install the key in your `.Renviron` file for use
#'   in future sessions.  Defaults to FALSE.
#' @param overwrite If this is set to TRUE, it will overwrite the existing
#'   GEOCLIENT_KEY that you already have in your `.Renviron` file. And if `key`
#'   is set to `NULL` it will completely remove that line.
#'
#' @examples
#'
#' \dontrun{
#' # To set the keys for use in this session only:
#' geoclient_api_key(key = "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' # To save the keys in your R.environ file for use in future sessions:
#' geoclient_api_key(
#'   key = "9d8f7b6wh4jfgud67s89jfyw68vj38fh",
#'   install = TRUE
#'  )
#'
#' # If you need to overwrite existing keys:
#' geoclient_api_key("9d8f7b6wh4jfgud67s89jfyw68vj38fh", overwrite = TRUE, install = TRUE)
#'
#' # First time, reload your environment so you can use the keys without restarting R.
#' readRenviron("~/.Renviron")
#'
#' # You can check them with:
#' Sys.getenv("GEOCLIENT_KEY")
#'
#' }
#'
#' @export

# This function was adapted from Kyle Walkerke's amazing tidycensus package

geoclient_api_key <- function(key, overwrite = FALSE, install = FALSE){

  if (!install) {

    if (!is_null(key)) {
      message("To install your API key for use in future sessions, run this function with `install = TRUE`.")
      Sys.setenv(GEOCLIENT_KEY = key)
    }

    if (is_null(key)) {
      message("Your API ID and key have been unset.")
      Sys.unsetenv("GEOCLIENT_KEY")
    }

    return(invisible(NULL))
  }

  wd <- getwd()
  on.exit(setwd(wd))
  setwd(Sys.getenv("HOME"))

  if (!file.exists(".Renviron")) file.create(".Renviron")

  message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
  file.copy(".Renviron", ".Renviron_backup")

  old_env <- readLines(".Renviron")

  already_set <- any(stringr::str_detect(old_env, "GEOCLIENT_KEY"))

  if (already_set && !overwrite) {
    stop_glue(
      "A GEOCLIENT_KEY already exists.
      You can overwrite them with the argument `overwrite = TRUE`"
    )
  }

  env_no_keys <- purrr::discard(old_env, ~stringr::str_detect(.x, "GEOCLIENT_KEY"))

  if (is_null(key)) {
    message("Your API key has been removed from your .Renviron")
    writeLines(env_no_keys, ".Renviron", sep = "\n")
    Sys.unsetenv("GEOCLIENT_KEY")
    return(invisible(NULL))
  }

  env_no_keys %>%
    c(glue::glue("GEOCLIENT_KEY='{key}'")) %>%
    writeLines(".Renviron", sep = "\n")

  msg_glue(
    'Your API key has been stored in your .Renviron and can be accessed by \\
     Sys.getenv("GEOCLIENT_KEY").
        To use now, restart R or run `readRenviron("~/.Renviron")`'
  )
}

get_creds <- function(key = NULL) {

  if (is_null(key)) {
    key <- Sys.getenv("GEOCLIENT_KEY")
  }

  if (key == "") {
    stop_glue(
      "A Geoclient API key is required.
      Obtain one at https://developer.cityofnewyork.us/user/register?destination=api",
      "You can then use `geoclient_api_key` to store it for future use.
      See ?geoclient_api_key for details."
    )
  }

  list(key = key)
}
