# SF return sf dataframe with crs = 2263 (used by NYC DCP)

add_bbl <- function(df, number, street, borough = NULL, zip = NULL, bbl = "bbl", geometry = FALSE) {

  bbl <- quo_name(enquo(bbl))

  number <- enquo(number)
  street <- enquo(street)
  borough <- enquo(borough)
  zip <- enquo(zip)

  if (!"data.frame" %in% class(df)) {
    stop_glue("`add_bbl` must be call on a dataframe")
  }

  if (bbl %in% names(df)) {
    stop_glue("A column named {bbl} already exists in in this dataframe.
              Either supply an alternative name for the new column, or rename the existing bbl column")
  }

  number <- dplyr::pull(df, !!number)
  street <- dplyr::pull(df, !!street)
  if (!is.null(borough)) {
    borough <- dplyr::pull(df, !!borough)
  }
  if (!is.null(zip)) {
    zip <- dplyr::pull(df, !!zip)
  }

  address_info <- geoclient_address(number, street, borough, zip)

  df <- dplyr::mutate(df, !!bbl := address_info[["bbl"]])


  if (geometry == FALSE) {

    return(df)

  } else if (geometry == TRUE) {

    df <- dplyr::mutate(df,
      lat = address_info[["latitude"]],
      lon = address_info[["longitude"]]
    )

    sf_df <- sf::st_as_sf(df,
      coords = c("lon", "lat"),
      na.fail = FALSE,
      crs = 2263)

    return(sf_df)
  }
}


# library(tibble)
# library(dplyr)

# addr_df <- tribble(
#   ~num,  ~st,             ~boro,      ~zip,
#   "139", "MacDougal St", "Manhattan", NA,
#   "114", "east 1st street", NA,          "10009-7924"
# )

# addr_df %>% add_bbl(num, st, boro, zip)

# addr_df %>% add_bbl(num, st, boro, zip, geometry = TRUE) -> foo

# library(sf)
# library(tigris)
# nyc <- tigris::places(36, class = "sf") %>% filter(GEOID == "3651000")

# plot(nyc[0])
# plot(foo[0], add = TRUE)
