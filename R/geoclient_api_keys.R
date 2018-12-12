#' Install Geoclient API ID and Key for Repeated Use
#'
#' This function will add your Geoclient app ID and key to your `.Renviron` file
#' so they can be called securely without being stored in your code. After you
#' have installed your ID and key, they can be called any time by typing
#' `Sys.getenv("GEOCLIENT_APP_ID")` and `Sys.getenv("GEOCLIENT_APP_KEY")` and
#' can be used in package functions by simply typing GEOCLIENT_APP_ID or
#' GEOCLIENT_APPKEY. If you do not have an `.Renviron` file, the function will
#' create on for you. If you already have an `.Renviron` file, the function will
#' append the ID and key to your existing file, while making a backup of your
#' original file for disaster recovery purposes. You can acquire your Geoclient
#' app ID and Key by first registering with the
#' [NYC's Developer Portal](https://developer.cityofnewyork.us/user/register?destination=api)
#' at, then [create a new project](https://developer.cityofnewyork.us/create/project),
#' selecting "Geoclient v1" from available APIs.
#'
#' @param id The API app ID provided to you from the NYC Developer Portal
#'   formated in quotes.
#' @param key The API app key provided to you from the NYC Developer Portal
#'   formated in quotes.
#' @param install if TRUE, will install the key in your `.Renviron` file for use
#'   in future sessions.  Defaults to FALSE.
#' @param overwrite If this is set to TRUE, it will overwrite the existing
#'   GEOCLIENT_APP_ID and GEOCLIENT_APP_KEY that you already have in your
#'   `.Renviron` file. And if `id` and `key` are set to `NULL` it will
#'   completely remove these lines.
#'
#' @examples
#'
#' \dontrun{
#' # To set the keys for use in this session only:
#' geoclient_api_keys(id = "1a2b3c4", key = "9d8f7b6wh4jfgud67s89jfyw68vj38fh")
#'
#' # To save the keys in your R.environ file for use in future sessions:
#' geoclient_api_keys(
#'   id = "1a2b3c4",
#'   key = "9d8f7b6wh4jfgud67s89jfyw68vj38fh",
#'   install = TRUE
#'  )
#'
#' # If you need to overwrite existing keys:
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh", overwrite = TRUE, install = TRUE)
#'
#' # First time, reload your environment so you can use the keys without restarting R.
#' readRenviron("~/.Renviron")
#'
#' # You can check them with:
#' Sys.getenv("GEOCLIENT_APP_ID")
#' Sys.getenv("GEOCLIENT_APP_KEY")
#'
#' }
#'
#' @export

# This function was adapted from Kyle Walkerke's amazing tidycensus package

geoclient_api_keys <- function(id, key, overwrite = FALSE, install = FALSE){

  if (!install) {

    if (!is_null(id) && !is_null(key)) {
      message("To install your API keys for use in future sessions, run this function with `install = TRUE`.")
      Sys.setenv(GEOCLIENT_APP_ID = id)
      Sys.setenv(GEOCLIENT_APP_KEY = key)
    }

    if (is_null(id) && is_null(key)) {
      message("Your API ID and key have been unset.")
      Sys.unsetenv("GEOCLIENT_APP_ID")
      Sys.unsetenv("GEOCLIENT_APP_KEY")
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

  already_set <- any(stringr::str_detect(old_env, "GEOCLIENT_APP_ID|GEOCLIENT_APP_KEY"))

  if (already_set && !overwrite) {
    stop_glue(
      "A GEOCLIENT_APP_ID or GEOCLIENT_APP_KEY already exists.
      You can overwrite them with the argument `overwrite = TRUE`"
    )
  }

  env_no_keys <- purrr::discard(old_env, ~stringr::str_detect(.x, "GEOCLIENT_APP_ID|GEOCLIENT_APP_KEY"))

  if (is_null(id) && is_null(key)) {
    message("Your API ID and key have been removed from your .Renviron")
    writeLines(env_no_keys, ".Renviron", sep = "\n")
    Sys.unsetenv("GEOCLIENT_APP_ID")
    Sys.unsetenv("GEOCLIENT_APP_KEY")
    return(invisible(NULL))
  }

  env_no_keys %>%
    c(glue::glue("GEOCLIENT_APP_ID='{id}'")) %>%
    c(glue::glue("GEOCLIENT_APP_KEY='{key}'")) %>%
    writeLines(".Renviron", sep = "\n")

  msg_glue(
    'Your API ID and key have been stored in your .Renviron and can be accessed by \\
     Sys.getenv("GEOCLIENT_APP_ID") and Sys.getenv("GEOCLIENT_APP_KEY").
        To use now, restart R or run `readRenviron("~/.Renviron")`'
  )
}

get_creds <- function(id = NULL, key = NULL) {

  if (is_null(id) && is_null(key)) {
    id <- Sys.getenv("GEOCLIENT_APP_ID")
    key <- Sys.getenv("GEOCLIENT_APP_KEY")
  }

  if (id == "" && key == "") {
    stop_glue(
      "A Geoclient API app ID and key are required.
      Obtain them at https://developer.cityofnewyork.us/user/register?destination=api",
      "You can then use `geoclient_api_keys` to store them for future use.
      See ?geoclient_api_keys for details."
    )
  }

  list(app_id = id, app_key = key)
}
