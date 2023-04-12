#' Retrieve census data for American Community Survey (with Tidycensus)
#'
#'@description This function uses `get_acs()`from [Tidycensus](https://cran.r-project.org/web/packages/tidycensus/index.html) with a pre-defined list of variables to retrieves ACS data for SVI calculation.
#'Note that a Census API key is required for this function to work, which can be obtained at <https://api.census.gov/data/key_signup.html>. To set up your key, use `census_api_key("YOUR KEY GOES HERE")`, or include it as an argument.
#'
#' @param year The year of interest (available 2014-2021)
#' @param geography The geography of interest (eg. state, county, zcta, tract)
#' @param state (Optional) Specify the state of interest. If data for multiple states are retrieved together, ranking for SVI calculation will be performed among all states.
#' @param county (Optional) Specify the county(s) of interest, must be combined with a value supplied to "state".
#' @param key Your Census API key.
#' @param ... Other arguments; more details please see `?get_acs()`
#'
#' @return A tibble of ACS data with each row represents an enumeration unit and each column represents the variables ("wide" form).
#' @export
#'
#' @examples
#' \dontrun{
#' census_api_key("YOUR KEY GOES HERE")
#' get_census_data(2018, "county", "PA")}

get_census_data <- function(year,
  geography,
  state = NULL,
  county = NULL,
  key = NULL,
  ...)
{
  var_list <-
    load(paste0("../data/census_variables_", year, ".rda")) %>%
    get() %>%
    unlist() %>%
    unname()


  tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    variables = var_list,
    output = "wide"
  )
}
