#' Retrieve census data for American Community Survey (with Tidycensus)
#'
#'@description This function uses [tidycensus::get_acs()] with a pre-defined
#'  list of variables to retrieves ACS data for SVI calculation. Note that a
#'  Census API key is required for this function to work, which can be obtained
#'  at <https://api.census.gov/data/key_signup.html>. To set up your key, use
#'  `census_api_key("YOUR KEY GOES HERE")`, or include it as an argument.
#'
#'@param year The year of interest (available 2012-2021).
#'@param state (Optional) Specify the state of interest. If data for multiple
#'  states are retrieved together, ranking for SVI calculation will be performed
#'  among all states. `state = NULL` as default, or `state = 'US'` return nation-level data.
#'@param geography The geography of interest (eg. state, county, zcta, tract)
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
  state = NULL,
  geography,
  county = NULL,
  key = NULL,
  ...)
{
  #predicate
  year_valid <- 2012:2021

  state_valid_chr <- zcta_state_xwalk2020 %>%
    dplyr::distinct(st_abb, state) %>%
    unlist(use.names = FALSE)
  state_valid_chr_us <- c("US", state_valid_chr)

  state_valid_dbl <- zcta_state_xwalk2020 %>%
    dplyr::distinct(st_code) %>%
    unlist(use.names = FALSE)

  if (length(year) > 1) {
    cli::cli_abort(c(
      "x" = "`year` contains {length(year)} years.",
      "i" = "Data retrieval is performed one year at a time. To retrieve data (and compute SVI) for multiple years separately, use `find_svi()`."
      ))
  }

  if (length(year) == 1 && !(year %in% year_valid)) {
    cli::cli_abort(c(
      "x" = "{year} is not a valid input for `year`.",
      "i" = "Years available for census data retrieval: 2012-2021."
      ))
  }



  if (inherits(state, "numeric")) {
  if (any(!(state %in% state_valid_dbl))) {
    cli::cli_abort(c(
      "x" = "One or more elements of {state} is not a valid input for `state`.",
      "i" = "Use `state_valid` for a table of valid input (besides 'US'). Note state full names/abbreviations are characters; FIPS codes are numbers."
    ))
  }
}

  if(inherits(state, "character")) {
    if (any(!(state %in% state_valid_chr_us))) {
      cli::cli_abort(c(
        "x" = "One or more elements of {state} is not a valid input for `state`.",
        "i" = "Use `state_valid` for a table of valid input (besides 'US'). Note state full names/abbreviations are characters; FIPS codes are numbers."
      ))
    }
  }

#after input validated
  filename <- paste0("census_variables_", year)

  var_list <- get(filename) %>%
    unlist() %>%
    unname()

if (length(state) == 1) {

  if (state == "US") {
    raw_data <- tidycensus::get_acs(
      geography = geography,
      state = NULL,
      year = year,
      variables = var_list,
      output = "wide"
    )
    return(raw_data)
  }

  if(geography == "zcta"&& year >= 2019) {
    cli::cli_alert_info(
      "State-specific ZCTA-level data for {year} is currently not supported by Census API.
Getting nation-based data and selecting ZCTAs in {state}...(it might take a bit longer)"
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

    raw_data <- tidycensus::get_acs(
      geography = geography,
      state = state,
      year = year,
      variables = var_list,
      output = "wide"
    )
    return(raw_data)
}

if (length(state) > 1) {

  if(geography == "zcta"&& year >= 2019) {
    cli::cli_alert_info(
      "State-specific ZCTA-level data for {year} is currently not supported by Census API.
Getting nation-based data and selecting ZCTAs in {state}...(it might take a bit longer)"
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
      dplyr::filter(state %in% st_input | st_code %in% st_input | st_abb %in% st_input) %>%
      dplyr::pull(ZCTA)

    state_data <- us_data %>%
      dplyr::filter(GEOID %in% tidyselect::all_of(zcta_by_state))

    return(state_data)
  }

  raw_data <- tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    variables = var_list,
    output = "wide"
  )
  return(raw_data)
  }

}


