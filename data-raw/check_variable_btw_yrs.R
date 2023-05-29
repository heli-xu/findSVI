library(tidycensus)
library(tidyverse)
##suppose you know and have a file for variables for "year1"
##and you'd like to check whether "year2" has the same variables

check_variable <- function(year1, year2, dataset){
  var1 <- load_variables(year1, dataset)
  var2 <- load_variables(year2, dataset)
  var_to_check <- readRDS(paste0("data/census_variables_",year1,".rds")) %>% 
    unlist() %>% 
    unname() %>% 
    str_sub(1L, -2L)
  
  var1_2 <- var1 %>% 
    filter(name%in%all_of(var_to_check)) %>% 
    select(name, label) %>% 
    left_join(
      var2 %>% 
        filter(name%in%all_of(var_to_check)),
      by = "name"
    )
  
  return(var1_2)
}

var1_2 <- check_variable(2020, 2021, "acs5/profile")
acs_var_1_2 <- check_variable(2020, 2021, "acs5")
sub_var_1_2 <- check_variable(2020, 2021, "acs5/subject")

var1_2$label.x == var1_2$label.y
#sometimes it's format that causes "unequal"
acs_var_1_2$label.x == acs_var_1_2$label.y
sub_var_1_2$label.x == sub_var_1_2$label.y
