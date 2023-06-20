#' Table of valid full names/abbreviations/FIPS codes of 52 states
#'
#' A reference table for valid input for `state` in `get_census_data()` and
#' `find_svi()`. In addition, `state = "US"` or `state = NULL` is also accepted
#' for nation-level data.
#'
#' @name state_valid
#' @format ## A data frame with 3 columns and 52 rows:
#' \describe{
#'   \item{st_abbr}{2-letter abbreviation for states.}
#'   \item{state}{State full name.}
#'   \item{fips_code}{Federal Information Processing System (FIPS) Codes for states.}
#'
#' }
#' @source County-state reference file 2020
#'   https://www.census.gov/programs-surveys/popest/geographies/reference-files.html
"state_valid"
