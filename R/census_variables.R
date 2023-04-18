#' List of census variables for SVI calculation
#'
#' Each of these datasets contains a list of census variable names for a year
#' between 2014-2021.
#'
#' @name census_variables
#' @format ## `who` A data frame with 7,240 rows and 60 columns:
#' \describe{
#'   \item{country}{Country name}
#'   \item{iso2, iso3}{2 & 3 letter ISO country codes}
#'   \item{year}{Year}
#'   ...
#' }
#' @source CDC/ATSDR SVI Documentation
#'   <https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html>
"census_variables_2014"

#' @rdname census_variables
"census_variables_2015"

#' @rdname census_variables
"census_variables_2016"

#' @rdname census_variables
"census_variables_2017"

#' @rdname census_variables
"census_variables_2018"

#' @rdname census_variables
"census_variables_2019"

#' @rdname census_variables
"census_variables_2020"

#' @rdname census_variables
"census_variables_2021"
