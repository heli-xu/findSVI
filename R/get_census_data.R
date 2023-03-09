get_census_data <- function(year, geography, state = NULL, ...){

  var_list <- readRDS(paste0("data/census_variables_",year,".rda")) %>%
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
