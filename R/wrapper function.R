summarise_svi  <- function(
  year,
  geography,
  state = NULL,
  key = NULL,
  full.table = FALSE,
  ...
  )
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
