#' Install Geoclient API ID and Key in Your `.Renviron` File for Repeated Use
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
#' app ID and Key by first registering with the [NYC's Developer
#' Portal](https://developer.cityofnewyork.us/user/register?destination=api) at,
#' then [create a new
#' project](https://developer.cityofnewyork.us/create/project), selecting
#' "Geoclient v1" from available APIs.
#'
#' @param id The API app ID provided to you from the NYC Developer Portal
#'   formated in quotes.
#' @param key The API app key provided to you from the NYC Developer Portal
#'   formated in quotes.
#' @param install if TRUE, will install the key in your `.Renviron` file for use
#'   in future sessions.  Defaults to FALSE.
#' @param overwrite If this is set to TRUE, it will overwrite the existing
#'   GEOCLIENT_APP_ID and GEOCLIENT_APP_KEY that you already have in your
#'   `.Renviron` file.
#'
#' @examples
#'
#' \dontrun{
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh", install = TRUE)
#' # First time, reload your environment so you can use the keys without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check them with:
#' Sys.getenv("GEOCLIENT_APP_ID")
#' Sys.getenv("GEOCLIENT_APP_KEY")
#' }
#'
#' \dontrun{
#' # If you need to overwrite existing keys:
#' geoclient_api_keys("1a2b3c4", "9d8f7b6wh4jfgud67s89jfyw68vj38fh", overwrite = TRUE, install = TRUE)
#' # First time, reload your environment so you can use the keys without restarting R.
#' readRenviron("~/.Renviron")
#' # You can check them with:
#' Sys.getenv("GEOCLIENT_APP_ID")
#' Sys.getenv("GEOCLIENT_APP_KEY")
#' }
#' @export


# This function was borrowed from Kyle Walkerke's amazing tidycensus package

geoclient_api_keys <- function(id, key, overwrite = FALSE, install = FALSE){

  if (install == TRUE) {
    setwd(Sys.getenv("HOME"))
    if(file.exists(".Renviron")){
      # Backup original .Renviron before doing anything else here.
      file.copy(".Renviron", ".Renviron_backup")
    }
    if(!file.exists(".Renviron")){
      file.create(".Renviron")
    }
    else{
      if(isTRUE(overwrite)){
        message("Your original .Renviron will be backed up and stored in your R HOME directory if needed.")
        oldenv <- utils::read.table(".Renviron", stringsAsFactors = FALSE)
        newenv <- oldenv[-grep("GEOCLIENT_APP_ID|GEOCLIENT_APP_KEY", oldenv), ]
        utils::write.table(newenv, ".Renviron", quote = FALSE, sep = "\n",
                           col.names = FALSE, row.names = FALSE)
      }
      else{
        tv <- readLines(".Renviron")
        if(isTRUE(any(grepl("GEOCLIENT_APP_ID|GEOCLIENT_APP_KEY", tv)))){
          stop_glue(
            "A GEOCLIENT_APP_ID or GEOCLIENT_APP_KEY already exists.
            You can overwrite them with the argument `overwrite = TRUE`"
          )
        }
      }
    }

    keyconcat <- glue::glue(
      "GEOCLIENT_APP_ID='{id}'
      GEOCLIENT_APP_KEY='{key}'"
    )

    # Append API key to .Renviron file
    write(keyconcat, ".Renviron", sep = "\n", append = TRUE)
    msg_glue(
      'Your API ID and key have been stored in your .Renviron and can be accessed by ',
      'Sys.getenv("GEOCLIENT_APP_ID") and Sys.getenv("GEOCLIENT_APP_KEY").
        To use now, restart R or run `readRenviron("~/.Renviron")`'
    )
  } else {
    message("To install your API keys for use in future sessions, run this function with `install = TRUE`.")
    Sys.setenv(GEOCLIENT_APP_ID = id)
    Sys.setenv(GEOCLIENT_APP_KEY = key)
  }

}
