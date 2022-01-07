
<!-- README.md is generated from README.Rmd. Please edit that file -->
geoclient
=========

[![Travis-CI Build Status](https://travis-ci.org/austensen/geoclient.svg?branch=master)](https://travis-ci.org/austensen/geoclient) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/austensen/geoclient?branch=master&svg=true)](https://ci.appveyor.com/project/austensen/geoclient) [![Coverage Status](https://img.shields.io/codecov/c/github/austensen/geoclient/master.svg)](https://codecov.io/github/austensen/geoclient?branch=master) [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

Tools to work with NYC's *Geoclient* REST API.

This packages uses NYC's Geoclient API but is neither endorsed nor supported by the the City of New York.

For information about the Geoclient API visit [NYC's Developers Portal](https://api-portal.nyc.gov/docs/services/geoclient/operations/geoclient).

### Installation

Install from Github with [remotes](https://github.com/r-lib/remotes):

``` r
# install.packages("remotes")
remotes::install_github("austensen/geoclient")
```

### Set up *Geoclient* API key

You can acquire your Geoclient API Key by first registering with the [NYC's API Portal](https://api-portal.nyc.gov/), then adding a "subscription" to the [Geoclient User](https://api-portal.nyc.gov/products/geoclient-user) API. Once completed, you will then be able to access your 'Primary key' at any point by visiting the [NYC's API Portal](https://api-portal.nyc.gov/) and viewing your user profile.

To avoid having to provide the Key with each function call you can use `geoclient_api_key()` to add your Geoclient Key to your `.Renviron` file so they can be called securely without being stored in your code.

#### Sample .Renviron file

Create a file `.Renviron` in the root of your project directory defining the variables `GEOCLIENT_APP_ID` and `GEOCLIENT_APP_KEY`.

```
GEOCLIENT_APP_ID=yourAppId
GEOCLIENT_APP_KEY=yourAppKey
```

Early in your R script, run `readRenviron(".Renviron")`.

### Basic Usage

There are 6 main location types that can be set with *Geoclient*: Address, BBL (Borough-Block-Lot), BIN (Building Identification Number), Blockface, Intersection, and Place ("well-known NYC place name"). All of these functions return the results of the *Geoclient* API call as a dataframe, with additional columns for the arguments provided to the function.

``` r
geo_address(
  house_number = "139", 
  street = "MacDougal St", 
  borough = "MN",
  zip = "10012"
)
#> # A tibble: 1 x 154
#>   input_house_num… input_street input_borough input_zip no_results
#>   <chr>            <chr>        <chr>         <chr>     <lgl>     
#> 1 139              MacDougal St manhattan     10012     FALSE     
#> # ... with 149 more variables: assemblyDistrict <chr>, bbl <chr>,
#> #   bblBoroughCode <chr>, bblTaxBlock <chr>, bblTaxLot <chr>, …
```

You can also pull out just a single column if that is all you need.

``` r
df <- tibble::tribble(
  ~num,  ~st,                ~boro,         ~zip,
  "139", "MacDougal St",     "manhattan",   "11231",
  "295", "Lafayette street", NA,            "10012-2722",
  "40",  "WASHINGTON SQ S",  "MN",          NA
)

dplyr::mutate(df, bbl = geo_address(num, st, boro, zip)[["bbl"]])
#> # A tibble: 3 x 5
#>   num   st               boro      zip        bbl       
#>   <chr> <chr>            <chr>     <chr>      <chr>     
#> 1 139   MacDougal St     manhattan 11231      1005430053
#> 2 295   Lafayette street <NA>      10012-2722 1005107502
#> 3 40    WASHINGTON SQ S  MN        <NA>       1005410001
```

For each of these location types there are two functions in this package that allow the arguments to be supplied either as individual vector, or with a dataframe and bare column names.

``` r
geo_address_data(df, num, st, boro, zip)
#> # A tibble: 3 x 241
#>   input_house_num… input_street input_borough input_zip no_results
#>   <chr>            <chr>        <chr>         <chr>     <lgl>     
#> 1 139              MacDougal St manhattan     11231     FALSE     
#> 2 295              Lafayette s… <NA>          10012-27… FALSE     
#> 3 40               WASHINGTON … manhattan     <NA>      FALSE     
#> # ... with 236 more variables: assemblyDistrict <chr>, bbl <chr>,
#> #   bblBoroughCode <chr>, bblTaxBlock <chr>, bblTaxLot <chr>, …
```

The return dataframe will always be the same length and in the same order, so you can easily add all the return columns to your existing dataframe.

``` r
dplyr::bind_cols(df, geo_address_data(df, num, st, boro, zip))
#> # A tibble: 3 x 245
#>   num   st    boro  zip   input_house_num… input_street input_borough
#>   <chr> <chr> <chr> <chr> <chr>            <chr>        <chr>        
#> 1 139   MacD… manh… 11231 139              MacDougal St manhattan    
#> 2 295   Lafa… <NA>  1001… 295              Lafayette s… <NA>         
#> 3 40    WASH… MN    <NA>  40               WASHINGTON … manhattan    
#> # ... with 238 more variables: input_zip <chr>, no_results <lgl>,
#> #   assemblyDistrict <chr>, bbl <chr>, bblBoroughCode <chr>, …
```

In addition to the 6 location types, *Geoclient* also provides a single-field search option, which will guess the location type. This can be particularly helpful when you have address data that is not easily separated for use with `geo_address()`.

``` r
df <- tibble::tribble(
  ~address,
  "139 MacDougal St manhattan, 11231",
  "295 Lafayette street, 10012-2722",
  "40 WASHINGTON SQ S MN"
)

geo_search_data(df, address)
#> # A tibble: 3 x 233
#>   input_location no_results alleyCrossStree… assemblyDistrict bbl  
#>   <chr>          <lgl>      <chr>            <chr>            <chr>
#> 1 139 MacDougal… FALSE      X                55               3015…
#> 2 295 Lafayette… FALSE      X                66               1005…
#> 3 40 WASHINGTON… FALSE      <NA>             66               1005…
#> # ... with 228 more variables: bblBoroughCode <chr>, bblTaxBlock <chr>,
#> #   bblTaxLot <chr>, boardOfElectionsPreferredLgc <chr>,
#> #   boePreferredStreetName <chr>, …
```
