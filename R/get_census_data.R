#' Retrieve American Community Survey data for SVI variables
#'
#' @description This function uses [tidycensus::get_acs()] with a pre-defined
#'   list of variables to retrieves ACS data for SVI calculation. Note that a
#'   Census API key is required for this function to work, which can be obtained
#'   at <https://api.census.gov/data/key_signup.html> and set up using
#'   [tidycensus::census_api_key()].
#'
#' @param year The year of interest (available 2010-2022).
#' @param state (Optional) Specify the state of interest. If data for multiple
#'   states are retrieved together, ranking for SVI calculation will be
#'   performed among all states. `state = NULL` as default, or `state = 'US'`
#'   return nation-level data.
#' @param geography The geography of interest (eg. state, county, zcta, tract)
#' @param county (Optional) Specify the county(s) of interest, must be combined
#'   with a value supplied to "state".
#' @param key Your Census API key. Obtain one at
#'   <https://api.census.gov/data/key_signup.html>. Include it in this argument
#'   or set up your key using `tidycensus::census_api_key("YOUR KEY GOES
#'   HERE")`.
#' @param geometry Default as `FALSE` for a regular tibble of census data. If
#'   set as `TRUE`, returns a tibble with an additional `geometry` column
#'   containing simple feature geometry.
#' @param exp Default as `FALSE` for retrieving `EP_`(percent estimate)
#'   variables directly from ACS when available (as described in CDC/ADSTR SVI
#'   dictionary (https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html)).
#'   If set as `TRUE`, uses explicitly defined denominators and retrieves the
#'   ACS data to calculate the `EP` variables for easier aggregation in
#'   downstream analysis (e.g. `get_svi_x()`).
#' @param ... Other arguments; more details please see [tidycensus::get_acs()]
#'
#' @returns A tibble of ACS data with each row represents an enumeration
#'   (geographic) unit and each column represents a census variable ("wide"
#'   form).
#'
#' @examplesIf Sys.getenv("CENSUS_API_KEY") != ""
#' # Census API key required
#'  get_census_data(
#'     year = 2018,
#'     geography = "county",
#'     state = "PA"
#'   )
#'
#' @importFrom rlang .data
#' @export

get_census_data <- function(year,
  geography,
  state = NULL,
  county = NULL,
  key = NULL,
  geometry = FALSE,
  exp = FALSE,
  ...)
{
  #predicate
  year_valid <- 2010:2022

  state_valid <- state_valid

  state_valid_chr <- state_valid %>%
    dplyr::select("st_abbr", "st_name") %>%
    unlist(use.names = FALSE)

  state_valid_chr_us <- c("US", state_valid_chr)

  state_valid_dbl <- state_valid %>%
    dplyr::select("fips_code") %>%
    unlist(use.names = FALSE)

  if (length(year) > 1) {
    cli::cli_abort(c(
      "x" = "`year` contains {length(year)} years.",
      "i" = "Data retrieval is performed one year at a time. To retrieve data (and compute SVI) for multiple years separately, use `find_svi()`."
      ))
  }

  if (length(year) == 1) {

    if (!(year %in% year_valid)) {
    cli::cli_abort(c(
      "x" = "{year} is not a valid input for `year`.",
      "i" = "Years available for census data retrieval: 2010-2022."
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
    if (exp == TRUE) {
      filename <- paste0("census_variables_exp_", year)
      var_list <- get(filename) %>%
        unlist() %>%
        unname()
    } else {

      filename <- paste0("census_variables_", year)

      var_list <- get(filename) %>%
        unlist() %>%
        unname()
    }

#state = 1
if (length(state) == 1) {

  ##US
  if (state == "US") {
    raw_data <- tidycensus::get_acs(
      geography = geography,
      state = NULL,
      year = year,
      variables = var_list,
      output = "wide",
      geometry = geometry,
      ...
    )
    return(raw_data)
  }

  ##zcta >=2019
  else if(geography == "zcta"&& year >= 2019) {
    cli::cli_alert_info(
      "State-specific ZCTA-level data for {year} is currently not supported by Census API.
Getting nation-based data and selecting ZCTAs in {state}...(it might take a bit longer)"
      )

  us_data <- tidycensus::get_acs(
    geography = geography,
    state = NULL,
    county = NULL,
    year = year,
    variables = var_list,
    output = "wide",
    geometry = geometry,
    ...
  )

  xwalk_name <- paste0("zcta_state_xwalk", year)
  st_input <- state

  if (length(county) == 0) {
  zcta_by_state <- get(xwalk_name) %>%
    dplyr::filter(.data$state == st_input | .data$st_code == st_input | .data$st_abb == st_input) %>%
    dplyr::pull("ZCTA")
  } else {

    cty_pat <- paste0(paste(county, collapse = " |")," ")
    zcta_by_state <- get(xwalk_name) %>%
      dplyr::filter(
        .data$state == st_input | .data$st_code == st_input | .data$st_abb == st_input,
        stringr::str_starts(county, tidyselect::all_of(cty_pat)) ) %>%
      dplyr::pull("ZCTA")
  }
  state_data <- us_data %>%
    dplyr::filter(.data$GEOID %in% tidyselect::all_of(zcta_by_state))

  return(state_data)
  } else {

    ##not >=2019 zcta
    raw_data <- tidycensus::get_acs(
      geography = geography,
      state = state,
      county = county,
      year = year,
      variables = var_list,
      output = "wide",
      geometry = geometry,
      ...
    )
    return(raw_data)
  }
}

#state >1
  if (length(state) > 1) {

    if (!length(county) == 0) {
      cli::cli_abort(c(
        "x" = "You specified `county` for {length(state)} states.",
        "i" = "County-specific data retrieval is only supported with a single entry in `state`."
      ))
    }

    ##zcta >=2019
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
      output = "wide",
      geometry = geometry,
      ...
    )

    xwalk_name <- paste0("zcta_state_xwalk", year)
    st_input <- state

    zcta_by_state <- get(xwalk_name) %>%
      dplyr::filter(.data$state %in% st_input | .data$st_code %in% st_input | .data$st_abb %in% st_input) %>%
      dplyr::pull("ZCTA")

    state_data <- us_data %>%
      dplyr::filter(.data$GEOID %in% tidyselect::all_of(zcta_by_state))

    return(state_data)
  } else {

  ##not >=2019 zcta data
  raw_data <- tidycensus::get_acs(
    geography = geography,
    state = state,
    year = year,
    variables = var_list,
    output = "wide",
    geometry = geometry,
    ...
  )
  return(raw_data)
  }

  }  else {
    #state not >1, not =1, so = NULL, or default
    raw_data <- tidycensus::get_acs(
      geography = geography,
      state = state,
      year = year,
      variables = var_list,
      output = "wide",
      geometry = geometry,
      ...
    )
    return(raw_data)
  }
  }
}


