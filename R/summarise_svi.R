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
#'@param full.table De
#'@param ...
#'
#'@return
#'@export
#'
#' @examples
#'
#'
summarise_svi  <- function(
  year,
  geography,
  state = NULL,
  key = NULL,
  full.table = FALSE,
  ...)
{
  if (length(year) == 1 && length(state) <= 1) {
    data_tmp <- findSVI::get_census_data(year, geography = geography, state)
    cli::cli_alert_success("Finished retrieving census data")
    results <- findSVI::get_svi(year, data_tmp)
    results_RPL <- results %>% dplyr::select(GEOID, contains("RPL_theme"))
    cli::cli_alert_success("Finished summarising theme-specific and overall SVI. For all variables, set 'full.table = TRUE'")
  }

  if (length(year) > 1) {
    if (length(year) != length(state)) {
      cli::cli_alert_warning("The number of years and states requested are not the same.")
      cli::cli_alert(
        "Consider using pairs of year and state. For nation level data ('state = NULL'), use single year argument."
      )
      stop("year-state pairing error")
    }

    data_tmp <-
      purrr::map2(year,
        state,
        \(x, y) findSVI::get_census_data(x, y, geography = geography))
    cli::cli_alert_success("Finished retriving census data")
    results <-  purrr::map2_dfr(year, data_tmp, get_svi)
    results_RPL <-
      results %>% dplyr::select(GEOID, tidyselect::contains("RPL_theme"))
    cli::cli_alert_success(
      "Finished summarising theme-specific and overall SVI. For all variables, set 'full.table = TRUE'"
    )

    if (full.table == TRUE) {
      return(results)
    }
    return(results_RPL)

  }

}
