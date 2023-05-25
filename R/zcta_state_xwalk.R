#' Relationship file (crosswalk) for ZCTAs by state
#'
#' Each of these tables contains ZIP Code Tabulation Areas (ZCTAs), their
#' intersecting counties and the states (state name, abbreviation, state FIPS
#' code) they are nested in. It's used in `get_census_data()` for retrieving ZCTA-level census data by
#' state, as [tidycensus::get_acs()] (CRAN version) currently does not support
#' obtaining state-specific ZCTA-level data.
#'
#' @name zcta_state_xwalk
#' @format ## A data frame with 5 columns and various number of rows, depending on the year:
#' \describe{
#'   \item{ZCTA}{5 digit ZCTA code.}
#'   \item{st_code}{2 digit Federal Information Processing System (FIPS) Codes for States.}
#'   \item{county}{County name within the state that the ZCTA intersects/corresponds to.}
#'   \item{state}{State full name corresponding to the FIPS code.}
#'   \item{st_abb}{Two-letter state abbreviation.}
#' }

#' @source Census ZCTA-county relationship file (2010)
#'   https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2010.html#list-tab-1709067297
#'   Geocorr ZCTA-county relationship file (2020)
#'   https://mcdc.missouri.edu/applications/geocorr2022.html
#'   County-state reference file (2019, 2020, 2021)
#'   https://www.census.gov/programs-surveys/popest/geographies/reference-files.html
"zcta_state_xwalk2021"

#' @rdname zcta_state_xwalk
"zcta_state_xwalk2020"

#' @rdname zcta_state_xwalk
"zcta_state_xwalk2019"
