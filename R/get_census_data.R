#' Retrieve census data for American Community Survey (with Tidycensus)
#'
#'@description This function uses [tidycensus::get_acs()] with a pre-defined
#'  list of variables to retrieves ACS data for SVI calculation. Note that a
#'  Census API key is required for this function to work, which can be obtained
#'  at <https://api.census.gov/data/key_signup.html>. To set up your key, use
#'  `census_api_key("YOUR KEY GOES HERE")`, or include it as an argument.
#'
#'@param year The year of interest (available 2014-2021)
#'@param geography The geography of interest (eg. state, county, zcta, tract)
#'@param state (Optional) Specify the state of interest. If data for multiple
#'  states are retrieved together, ranking for SVI calculation will be performed
#'  among all states.
#'@param county (Optional) Specify the county(s) of interest, must be combined
#'  with a value supplied to "state".
#'@param key Your Census API key.
#'@param ... Other arguments; more details please see [tidycensus::get_acs()]
#'
#'@return A tibble of ACS data with each row represents an enumeration unit and
#'  each column represents the variables ("wide" form).
#'@export
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

  filename <- paste0("census_variables_", year)

  var_list <- get(filename) %>%
    unlist() %>%
    unname()

if(geography == "zcta"&& year >= 2019) {
  message(
    sprintf("State-specific ZCTA-level data for %s is currently not supported by Census API. \nGetting Nation-based data and selecting ZCTAs in %s...(it might take a bit longer)", year, state)
    )

  us_data <- tidycensus::get_acs(
    geography = geography,
    state = NULL,
    year = year,
    variables = var_list,
    output = "wide"
  )
  xwalk_name <- paste0("zcta_state_xwalk", year)
  st_input <- state

  zcta_by_state <- get(xwalk_name) %>%
    dplyr::filter(state == st_input | st_code == st_input | st_abb == st_input) %>%
    dplyr::pull(ZCTA)

  state_data <- us_data %>%
    dplyr::filter(GEOID %in% tidyselect::all_of(zcta_by_state))

  return(state_data)
}

  tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    variables = var_list,
    output = "wide"
  )
}
