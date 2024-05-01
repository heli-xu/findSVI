#' Table of census variables and formula for SVI calculation
#'
#' Each of these datasets contains a table of SVI variable names, related census
#' variable names and their corresponding calculation formula for a year between
#' 2012-2021. This is used to construct SVI results for the variables starting
#' with "E_"(estimate) and "EP_"(percentage) after obtaining census data.
#' Sometimes SVI variables are directly linked to census variables, and other
#' times one or more census variable(s) are included to derive an SVI variable.
#'
#' @name variable_calculation
#' @format ## A data frame with 3 columns and various number of rows, depending
#'   on the year:
#' \describe{
#'   \item{_variable_name}{With a prefix "x" followed by the year, eg. x2018_variable_name, this column is the SVI variable name}
#'   \item{theme}{SVI variables are categorized into four themes/domains:
#'   socioeconomic, household composition/disability, minority status/language
#'   and housing type/transportation. Theme 0 is used for 3 variables
#'   representing total counts, while theme 5 is used for adjunct variables (not
#'   included in calculation). Adjunct variables are not included in 2012 due to
#'   unavailable data/documentation.}
#'   \item{_table_field_calculation}{With a prefix "x" followed by the year, eg. x2018_table_field_calculation,
#'   this column contains the corresponding census variable names, and/or the calculation
#'   using SVI/census variables. `variable_e_denom_2020` uses census variables explicitly as denominators, as opposed to retrieving percent from ACS when available (as described by CDC SVI documentation).}
#' }
#' @source CDC/ATSDR SVI Documentation
#'   <https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html>
"variable_e_ep_calculation_2012"

#' @rdname variable_calculation
"variable_e_ep_calculation_2013"

#' @rdname variable_calculation
"variable_e_ep_calculation_2014"

#' @rdname variable_calculation
"variable_e_ep_calculation_2015"

#' @rdname variable_calculation
"variable_e_ep_calculation_2016"

#' @rdname variable_calculation
"variable_e_ep_calculation_2017"

#' @rdname variable_calculation
"variable_e_ep_calculation_2018"

#' @rdname variable_calculation
"variable_e_ep_calculation_2019"

#' @rdname variable_calculation
"variable_e_ep_calculation_2020"

#' @rdname variable_calculation
"variable_e_ep_calculation_2021"

#' @rdname variable_calculation
"variable_e_denom_2020"
