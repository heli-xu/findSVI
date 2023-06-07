#'Retrieve Census Data and Calculate Social Vulnerability Index (SVI) for One or
#'Multiple Year(s)/State(s)
#'
#'@description This is a wrapper for [findSVI::get_census_data()] and
#'  [findSVI::get_svi()] that retrieves census data and produces SVI for one or
#'  multiple years(s) and state(s). For multiple year-state entries, SVI is
#'  obtained from entry-specific percentile ranking and summarised into one
#'  table.
#'
#'@param year A vector containing years of interest. Length >=1. Acting as pairs
#'  with `state`, `year` should be of the same length as `state`. The exception
#'  is when it's a single year entry (length 1), other than providing a single
#'  state of interest, `state` can also be set as `NULL`, in which case nation
#'  level data is retrieved and processed to obtain SVI.
#'@param geography One geography level of interest for all year-state
#'  combination (e.g."county", "zcta", "tract").
#'@param state A vector containing states of interest. Length >=0. Length 0
#'  (`state = NULL`) must be used with single year argument, when SVI is
#'  calculated from nation-level census data. In other cases, `state` must have
#'  the same elements as `year` (same length).
#'@param key Your Census API key. Obtain one at
#'  <https://api.census.gov/data/key_signup.html>. To set up, use
#'  `census_api_key("YOUR KEY GOES HERE")`, or include it as an argument.
#'@param full.table Default as `FALSE`, returning SVI table with only "GEOID",
#'  and SVI for each theme and all themes. If set as `TRUE`, a full SVI table
#'  with individual SVI variables and intermediate ranking calculations are also
#'  included in addition to the theme-related SVIs (similar style to tables from
#'  [CDC/ATSDR
#'  database](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html)).
#'
#'@return A tibble of summarised SVI for one or multiple year-state combination
#'  of interest. Rows represent the geographic units, and columns represent its
#'  SVI for each theme and all themes. Additional two columns at the end
#'  indicate the corresponding state and year information. For `full.table =
#'  TRUE`, estimated count and percentage values for individual SVI variables
#'  are also included. For description of variable names (column names), please
#'  refer to [CDC/ATSDR documentation](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html).
#'@export
#'
#' @examples
#' \dontrun{
#' census_api_key("YOUR KEY GOES HERE")
#'
#' # Use with vectors for year and state
#' ## for one year-state information (all ZCTAs of PA in 2019)
#' summarise_svi(
#'       year = 2019,
#'       state = "PA",
#'       geography = "tract"
#'    )
#'
#'
#' ## for multiple year-state combination (all ZCTAs of RI in 2017 and PA in 2018)
#' summarise_svi(
#'       year = c(2017, 2018),
#'       state = c("RI", "PA"),
#'       geography = "zcta"
#'    )
#'
#'# Use with a table of year-state information
#' info <- tribble(
#'     ~state, ~year,
#'     "AZ", 2015,
#'     "AZ", 2016,
#'     "PA", 2020,
#'     "RI", 2018)
#'
#' summarise_svi(
#'     year = info$year,
#'     state = info$state,
#'     geography = "county"
#'    )
#' }
#'
summarise_svi  <- function(
  year,
  state = NULL,
  geography,
  key = NULL,
  full.table = FALSE
  )
{
  if (length(year) == 1 && length(state) <= 1) {
    data_tmp <- findSVI::get_census_data(year, state, geography = geography)
    cli::cli_alert_success("Finished retrieving census data")
    results <- findSVI::get_svi(year, data_tmp) %>% dplyr::mutate(year = year, state = state)
    results_RPL <- results %>% dplyr::select(GEOID, contains("RPL_theme"), year, state)
    cli::cli_alert_success("Finished summarising theme-specific and overall SVI. For all variables, set 'full.table = TRUE'")

    if (full.table == TRUE) {
      return(results)
    }
    return(results_RPL)
  }

  if (length(year) > 1) {
    if (length(year) != length(state)) {
      cli::cli_alert_warning("The number of years and states requested are not the same.")
      cli::cli_alert(
        "Consider using pairs of year and state. For nation level data ('state = NULL'), use single year argument."
      )
      stop("year-state pairing error")
    }

    results <- purrr::map2_dfr(
      year,state,
      function(year_tmp, state_tmp){

        ## Get Census Data
        census_tmp <- findSVI::get_census_data(year_tmp, state_tmp, geography = geography)
        cli::cli_alert_success("Finished retrieving census data for {year_tmp} {state_tmp}")

        ## Calculate SVI
        svi_tmp <- findSVI::get_svi(year_tmp, census_tmp) %>%
          dplyr::mutate(year = year_tmp, state = state_tmp)
        return(svi_tmp)
      })

    results_RPL <- results %>% dplyr::select(GEOID, tidyselect::contains("RPL_theme"), year, state)

    cli::cli_alert_success(
      "Finished summarising theme-specific and overall SVI. For all variables, set 'full.table = TRUE'"
    )

    if (full.table == TRUE) {
      return(results)
    }
    return(results_RPL)

  }

}
