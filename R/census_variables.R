#' List of census variables for SVI calculation
#'
#' Each of these datasets contains a list of census variable names for a year
#' between 2014-2021.
#'
#' @name census_variables
#' @format ## a list of census variables, grouped into 6 sublists named t0-t5.
#'   t1-t4 represent the 4 themes the corresponding SVI variables are
#'   categorized into; t0 represents 3 census variables of total counts, and
#'   their corresponding SVI variables are not categorized into any theme. t5
#'   contains the census variables for SVI "adjunct variables" (first introduced
#'   in 2020 documentation), which are included for reference but not used in
#'   SVI calculation.

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
