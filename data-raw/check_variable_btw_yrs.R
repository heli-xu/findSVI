library(tidycensus)
library(tidyverse)
##suppose you know and have a file for variables for "year1"
##and you'd like to check whether "year2" has the same variables

check_variable <- function(year1, year2, dataset){
  var1 <- load_variables(year1, dataset)
  var2 <- load_variables(year2, dataset)
  var_to_check <- get(paste0("census_variables_",year1)) %>%
    unlist() %>%
    unname() %>%
    str_sub(1L, -2L)

  suffix_x <- as.character(year1)
  suffix_y <- as.character(year2)

  var1_2 <- var1 %>%
    filter(name%in%var_to_check) %>%
    select(name, label) %>%
    left_join(
      var2 %>%
        select(name, label) %>%
        filter(name%in%all_of(var_to_check)),
      by = "name",
      suffix = c(suffix_x, suffix_y)
    )

  return(var1_2)
}

var1_2 <- check_variable(2020, 2021, "acs5/profile")
acs_var_1_2 <- check_variable(2020, 2021, "acs5")
sub_var_1_2 <- check_variable(2020, 2021, "acs5/subject")

var1_2 <- check_variable(2012, 2011, "acs5/profile") |>
  mutate(matched = if_else(label2012 == label2011, 1, 0))
acs_var_1_2 <- check_variable(2012, 2011, "acs5") |>
  mutate(matched = if_else(label2012 == label2011, 1, 0))
sub_var_1_2 <- check_variable(2012, 2011, "acs5/subject") |>
  mutate(matched = if_else(label2012 == label2011, 1, 0))

# disability var not shown different, but not available for 2010 and 2011 (all NA)


#sometimes it's format, or year, that causes "unequal"

