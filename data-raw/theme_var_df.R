# generate var list from a parsed calculation table
theme_var_df <- function(n, data){
  data %>%
    filter(theme == n) %>%
    select(1, census_var) %>% #1st col is var_name, adaptable to year
    separate_rows(census_var, sep = " ")  %>%
    filter(!str_starts(census_var, "E_"),
      !census_var%in%c("","100")) %>%
    pull(census_var) %>%
    unique()
}
