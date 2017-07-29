#' Tools to work with NYC's Geoclient REST API.
#'
#' This packages uses NYC's Geoclient API but is neither endorsed nor supported
#' by the the City of New York.
#'
#' To learn more about usig this package, visit
#' https://austensen.github.io/geoclient/ or `browseVignettes(package =
#' "geoclient")`
#'
#' For information about the Geoclient API visit [NYC's Developers
#' Portal](https://developer.cityofnewyork.us/api/geoclient-api).
#'
#' You can acquire your Geoclient app ID and Key by first registering with the
#' [NYC's Developer
#' Portal](https://developer.cityofnewyork.us/user/register?destination=api) at,
#' then [create a new
#' project](https://developer.cityofnewyork.us/create/project), selecting
#' "Geoclient v1" from available APIs. For more information on using these
#' credentials with this package see [geoclient_api_keys()]
#'
#' @section Geoclient Description:
#'
#'   The Geoclient API is a RESTful web service interface to the NYC Department
#'   of City Planning’s Geosupport system developed by the Department of
#'   Information Technology and Telecommunications GIS/Mapping unit. Geosupport
#'   is a mainframe-based geocoding system used by NYC government. Geosupport
#'   provides coordinate and geographic attributes for supported input locations
#'   (address, intersection, blockface). Geoclient exposes the most widely used
#'   Geosupport functions through simple and easy to use REST representations.
#'
#' @section Geoclient Service Usage Guidelines:
#'
#'   The Geoclient API is a free geocoding service offered by the City of New
#'   York for use by the general public. Access to the service requires
#'   registering for a free account on this website and requesting an access
#'   key. Due to rapidly increasing usage of Geoclient, we are seeing a
#'   commensurate decline in overall application availability based our current
#'   contractual resource allowance from third party platform services. While we
#'   work towards increasing this capacity, we are forced to set service usage
#'   guidelines.
#'
#'   From this point forward, the following service usage guidelines apply:
#'
#'   * Maximum of 2,500 requests per minute;
#'   * Maximum of 500,000 requests per day.
#'
#'   Note that these are guidelines and not hard limits. However, if we see
#'   applications continually exceeding these limits, we will attempt to notify
#'   the account administrator based on contact information provided in the
#'   sign-up forms. If corrective action is not taken, we will then set whatever
#'   limits we deem necessary to provide equal resources averaged by all active
#'   accounts. Moving forward, as we increase capacity, we may adjust the usage
#'   guidelines.
#'
#'   By default all functions in this package adhere to these guidelines.
#'   However, both restrictions can be overriden by setting the arguments
#'   `rate_limit` and `cap_daily_requests` to `FALSE`.
#'
#' @import rlang
#' @importFrom dplyr %>%
"_PACKAGE"
