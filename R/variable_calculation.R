#' Table of census variables and formula for SVI calculation
#'
#' Each of these datasets contains a table of SVI variable names, related census
#' variable names and their corresponding calculation formula for a year between
#' 2014-2021. This is used to construct SVI results for the variables starting
#' with "E_"(estimate) and "EP_"(percentage) after obtaining census data.
#' Sometimes SVI variables are directly linked to census variables, and other
#' times one or more census variable(s) are included to derive an SVI variable.
#'
#' @name variable_calculation
#' @format ## A data frame with 3 columns and variable number of rows, depending on the year:
#' \describe{
#'   \item{_variable_name}{With a prefix x_year, eg. x2018_variable_name,
#'   this column is the SVI variable name}
#'   \item{theme}{SVI variables are catagorized into four themes/domains;
#'   theme 0 is used for 3 variables representing total counts,
#'   while theme 5 is used for adjunct variables recently introduced in
#'   2020 documentation (not used in calculation, included for reference).}
#'   \item{_table_field_calculation}{With a prefix x_year, eg. x2018_table_field_calculation,
#'   this column contains the corresponding census variable names, and/or the calculation
#'   using SVI/census variables.}
#' }
#' @source CDC/ATSDR SVI Documentation
#'   <https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html>
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
