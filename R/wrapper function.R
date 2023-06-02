find_svi  <- function(
  year,
  geography,
  state = NULL,
  key = NULL,
  verbose = FALSE,
  ...
  )
{
  data_tmp <- purrr::map2(year, state, \(x, y) findSVI::get_census_data(x, y, geography = geography))
  results <-  purrr::map2_dfr(year, data_tmp, get_svi)
  results_RPL <- results %>% dplyr::select(GEOID, contains("RPL_theme"))

  if (verbose = TRUE) {return(results)}
  return(results_RPL)


}
