#' Retrieve census data and calculate SVI for one or multiple year(s)/state(s)
#'
#' @description `find_svi()` is like a wrapper for [findSVI::get_census_data()] and
#'  [findSVI::get_svi()] that retrieves census data and produces SVI for one or
#'  multiple years(s) and state(s). For multiple year-state entries, SVI is
#'  obtained from percentile rankings for each entry and summarised into one
#'  table. Note that a Census API key is required for this function to work,
#'  which can be obtained at <https://api.census.gov/data/key_signup.html> and
#'  set up using [tidycensus::census_api_key()].
#'
#' @param year A vector containing years of interest (available 2012-2022).
#'   Length >=1. Acting as pairs with `state`, `year` should be of the same
#'   length as `state`. The exception is when it's a single year entry (length
#'   1), other than providing one state of interest, supply `state = NULL` as
#'   default or `state = 'US'` retrieves and processes nation level data to
#'   obtain SVI.
#' @param state A vector containing states of interest. Length >=0. Length 0
#'   (`state = NULL`), or `state = 'US'` must be used with single year argument,
#'   when SVI is calculated from nation-level census data. In other cases,
#'   `state` must have the same elements as `year` (same length).
#' @param geography One geography level of interest for all year-state
#'  combination (e.g."county", "zcta", "tract").
#' @param key Your Census API key. Obtain one at
#'  <https://api.census.gov/data/key_signup.html>. To set up, use
#'  `tidycensus::census_api_key("YOUR KEY GOES HERE")`, or include it as an argument.
#' @param full.table Default as `FALSE`, returning SVI table with only "GEOID",
#'   and SVI for each theme and all themes. If set as `TRUE`, a full SVI table
#'   with individual SVI variables and intermediate ranking calculations are
#'   also included in addition to the theme-related SVIs (similar style to
#'   tables from [CDC/ATSDR
#'   database](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html)).
#'
#' @returns A tibble of summarised SVI for one or multiple year-state combination(s)
#'  of interest. Rows represent the geographic units, and columns represent its
#'  SVI for each theme and all themes. Additional two columns at the end
#'  indicate the corresponding state and year information. For `full.table =
#'  TRUE`, estimated count and percentage values for individual SVI variables
#'  are also included. For description of variable names (column names), please
#'  refer to [CDC/ATSDR
#'  documentation](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html).
#'
#' @examplesIf Sys.getenv("CENSUS_API_KEY") != ""
#' # Census API key required
#' # For one year-state entry
#' find_svi(
#'       year = 2019,
#'       state = "AZ",
#'       geography = "county"
#'    )
#'
#'
#' # For multiple year-state pairs
#' ## All ZCTAs for 2017-AZ; 2017-DE; and 2018-DC
#' year <- c(2017, 2017, 2018)
#' state <- c("AZ", "DE", "DC")
#' info <- data.frame(year, state)
#'
#' find_svi(
#'       year = info$year,
#'       state = info$state,
#'       geography = "zcta"
#'    )
#'
#'
#' @importFrom rlang .data
#' @export

find_svi  <- function(
  year,
  state = NULL,
  geography,
  key = NULL,
  full.table = FALSE
  )
{
  state_valid <- state_valid

  state_valid_chr <- state_valid %>%
    dplyr::select("st_abbr", "st_name") %>%
    unlist(use.names = FALSE)
  state_valid_chr_us <- c("US", state_valid_chr)

  state_valid_dbl <- state_valid %>%
    dplyr::select("fips_code") %>%
    unlist(use.names = FALSE)

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


  if (length(year) == 1) {

    if (length(state) == 0) {
      data_tmp <- findSVI::get_census_data(year, state = NULL, geography = geography)
      cli::cli_alert_success("Finished retrieving nation-level census data for {year}")
      results <- findSVI::get_svi(year, data_tmp) %>% dplyr::mutate(year = {{year}}, state = "US")
      results_RPL <- results %>% dplyr::select("GEOID", tidyselect::contains("RPL_theme"), "year", "state")
      cli::cli_alert_success(
        "Finished summarising theme-specific and overall SVI. For all variables, set 'full.table = TRUE'"
      )

      if (full.table == TRUE) {
        return(results)
      }
      return(results_RPL)
    }

    if(length(state) == 1) {
        if(state == "US") {
      data_tmp <- findSVI::get_census_data(year, state = NULL, geography = geography)
      cli::cli_alert_success("Finished retrieving nation-level census data for {year}")
      results <- findSVI::get_svi(year, data_tmp) %>% dplyr::mutate(year = {{year}}, state = "US")
      results_RPL <- results %>% dplyr::select("GEOID", tidyselect::contains("RPL_theme"), "year", "state")
      cli::cli_alert_success(
        "Finished summarising theme-specific and overall SVI. For all variables, set 'full.table = TRUE'"
      )

      if (full.table == TRUE) {
        return(results)
      }
      return(results_RPL)

      }  else {

      #length =1, not US
      data_tmp <- findSVI::get_census_data(year, state, geography = geography)
      cli::cli_alert_success("Finished retrieving census data for {year} {state}")
      results <- findSVI::get_svi(year, data_tmp) %>% dplyr::mutate(year = {{year}}, state = {{state}})
      results_RPL <- results %>% dplyr::select("GEOID", tidyselect::contains("RPL_theme"), "year", "state")
      cli::cli_alert_success(
        "Finished summarising theme-specific and overall SVI. For all variables, set 'full.table = TRUE'"
        )

    if (full.table == TRUE) {
      return(results)
    }
    return(results_RPL)
    }}


    if (length(state) > 1) {
      cli::cli_abort(c(
        "!" = "You inputted 1 year but {length(state)} states. find_svi() requires `year` and `state` to be of the same length.",
        "i" = "If you'd like to find SVI of multiple states for the same year, try `rep({year}, length(state))` in `year` argument."
        ))

    }
  }


  if (length(year) > 1) {

    year_valid <- 2012:2022
    if (any(!(year %in% year_valid))) {
      cli::cli_abort(c(
        "x" = "One or more elements of {year} is not a valid input for `year`.",
        "i" = "Years available for census data retrieval: 2012-2022."
      ))
    }

    if (length(state) == 0) {
      cli::cli_abort(c(
        "!" = "You inputted {length(year)} years for nation-level data.",
        "i" = "For nation-level data(`state = 'US'` or unspecified), find_svi() requires single year argument."
      ))
    }


    if (length(state) == 1) {

      if (state == "US") {
        cli::cli_abort(c(
          "!" = "You inputted {length(year)} years for nation-level data.",
          "i" = "For nation-level data(`state = 'US'` or unspecified), find_svi() requires single year argument."
        ))
      }  else {

      #length = 1, not US
      cli::cli_abort(c(
        "!" = "You inputted {length(year)} years but only 1 state. find_svi() requires `year` and `state` to be of the same length.",
        "i" = "If you'd like to find SVI of the same state for multiple years, try `rep({state}, length(year))` in `state` argument."
      ))
    }}


    if (length(state) > 1 & length(year) != length(state)) {
      cli::cli_abort(c(
        "!" = "You inputted {length(year)} years and {length(state)} states. find_svi() requires `year` and `state` to be of the same length.",
        "i" = "Consider using pairs of `year` and `state`. For nation-level data (`state = NULL` or `state = 'US'`), use single year argument. "
      ))
    }  else {

    results <- purrr::map2_dfr(
      year,state,
      function(year_tmp, state_tmp){

        ## Get Census Data
        census_tmp <- findSVI::get_census_data(year_tmp, state_tmp, geography = geography)
        cli::cli_alert_success("Finished retrieving census data for {year_tmp} {state_tmp}")

        ## Calculate SVI
        svi_tmp <- findSVI::get_svi(year_tmp, census_tmp) %>%
          dplyr::mutate(year = {{year_tmp}}, state = {{state_tmp}})
        return(svi_tmp)
      })

    results_RPL <- results %>% dplyr::select("GEOID", tidyselect::contains("RPL_theme"), "year", "state")

    cli::cli_alert_success(
      "Finished summarising theme-specific and overall SVI. For all variables, set 'full.table = TRUE'"
    )

    if (full.table == TRUE) {
      return(results)
    }
    return(results_RPL)

  }

  }
}
