#' List of census variables for SVI calculation
#'
#' Each of these datasets contains a list of census variable names for a year
#' between 2012-2021.
#'
#' @name census_variables
#' @format ## a list of census variables, grouped into sublists named t0-t5 or
#'   t0-t4 (for 2012). t1-t4 represent the 4 themes the corresponding SVI
#'   variables are categorized into:
#'   * Socioeconomic
#'   * Household Composition/Disability
#'   * Minority Status/Language
#'   * Housing Type/Transportation
#'
#'   t0 represents 3 census variables of total counts, and their corresponding
#'   SVI variables are not categorized into any theme. t5 contains the census
#'   variables for SVI "adjunct variables", which are included for reference but
#'   not used in SVI calculation. For 2012, adjunct variables are not included,
#'   as the variable listed in 2014 documentation was not in 2012 Census data,
#'   and there's no adjunct variables in 2010 documentation.
#'
#'   Datasets starting with `census_variable_` contains variables corresponding
#'   to `variable_e_ep_calculation_` series of tables, where "EP_" (percent)
#'   variables are retrieved directly from Census when available; Datasets
#'   starting with `census_variables_exp_` contains variables corresponding to
#'   `variable_cal_exp_` tables, where denominators for "EP_" variables are
#'   explicitly defined using census variables.

#' @source CDC/ATSDR SVI Documentation
#'   <https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html>
"census_variables_2012"

#' @rdname census_variables
"census_variables_2013"

#' @rdname census_variables
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

#' @rdname census_variables
"census_variables_exp_2020"

#' @rdname census_variables
"census_variables_exp_2021"

#' @rdname census_variables
"census_variables_exp_2019"

#' @rdname census_variables
"census_variables_exp_2018"

#' @rdname census_variables
"census_variables_exp_2017"

#' @rdname census_variables
"census_variables_exp_2016"

#' @rdname census_variables
"census_variables_exp_2015"

#' @rdname census_variables
"census_variables_exp_2014"

#' @rdname census_variables
"census_variables_exp_2013"

#' @rdname census_variables
"census_variables_exp_2012"
