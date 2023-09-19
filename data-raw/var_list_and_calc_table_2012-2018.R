## code to prepare census_variables_2018.rda and variable_e_ep_calculation_2018.rda
## https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
#documentation was downloaded as pdf, with the dictionary table manually imported in xlsx.

library(tidyverse)
library(readxl)
library(janitor)
library(stringr)


# Import dictionary (available for even year)---------------------------------

svi_var <- read_xlsx("xls/2018svi_dictionary.xlsx") %>%
  clean_names()

skim(svi_var)


# Remove MOE --------------------------------------------------------------

#during import into excel:
#select the autodetect tables across pages during import table to excel from pdf
#so you don't have to deal with page numbers

#*keeping rows with na in variable names -some are wrapped content from previous row*

svi_var_clean <- svi_var %>%
  mutate(x2018_variable_name = replace_na(x2018_variable_name, "x")) %>%
  filter(!str_starts(x2018_variable_name,"M"))

# Simplify and modify table --------------------------------------
## filter theme, variable, calculation
var_cal <- svi_var_clean %>%
  select(x2018_variable_name, theme, x2018_table_field_calculation) %>%
  drop_na(x2018_table_field_calculation) %>%
  filter(!str_detect(x2018_table_field_calculation,"\\^"))

## Inspect, modify calculations that spill over (within E_, EP_)
##(something like "10 0" will be dealt with later - it's the \r\n)
# var_cal$x2018_table_field_calculation[21] has a "+" at the end

##If you manually assign value to this cell, and hit enter to wrap lines during your input
#it doesn't change what you see, BUT it actually adds \n to strings
#BEWARE OF HITTING that ENTER for line breaks!
##ofc can just paste without line breaking, here use subtracting last x chr
var_cal$x2018_table_field_calculation[21] <- var_cal %>%
  filter(x2018_variable_name == "E_LIMENG") %>%
  pull(x2018_table_field_calculation) %>%
  str_sub(.,1, -2) #sigh, last chr is space, so last 2

# Extract E_ and EP_ vars with calculation-------------------------------------

var_prefix <- c("E_", "EP_")

var_cal2 <-
  var_cal %>%
  filter(str_detect(x2018_variable_name, paste(var_prefix, collapse = "|"))) %>%
  #modify the blank themes
  mutate(theme = case_when(
    x2018_variable_name%in%c("E_TOTPOP","E_HU","E_HH") ~ 0,  #must be same class
    is.na(theme) ~ 5, #adjunct variables
    TRUE ~ theme
  )) %>%
  mutate(
    #pdf to excel introduce line breaks, not visible in view- use $ index to see
    x2018_table_field_calculation = str_replace_all(x2018_table_field_calculation, "\r\n",""),
    census_var = str_replace_all(x2018_table_field_calculation,
      "[^[:alnum:][:blank:]_]",
      " "))
#replace with blank instead of nothing:
#100 will be separated by at least one blank with string

# SAVE: Table for svi calculation ----------------------------
variable_e_ep_calculation_2018 <- var_cal2 %>%
  select(-census_var)

# SAVE: List of variables from each theme----------------------
theme_var_df <- function(n){
  var_cal2 %>%
    filter(theme == n) %>%
    select(1, census_var) %>%  #first column is the var_name
    separate_rows(census_var, sep = " ")  %>%
    filter(!str_starts(census_var, "E_"),
      !census_var%in%c("","100")) %>%
    pull(census_var)
}

census_variables_2018 <- map(0:5, theme_var_df)

##name elements in the list by theme (t0 = total, t5 = adjunct)
names(census_variables_2018) <- c("t0","t1","t2","t3","t4","t5")


usethis::use_data(variable_e_ep_calculation_2018, overwrite = TRUE)
usethis::use_data(census_variables_2018, overwrite = TRUE)


# Modify for 2017 (same as 2018) ---------------------------------------------
variable_e_ep_calculation_2017 <- variable_e_ep_calculation_2018 %>%
  rename(x2017_variable_name = x2018_variable_name,
    x2017_table_field_calculation = x2018_table_field_calculation)

census_variables_2017 <- census_variables_2018

usethis::use_data(variable_e_ep_calculation_2017, overwrite = TRUE)
usethis::use_data(census_variables_2017, overwrite = TRUE)


# Modify for 2016 (based on 2018)----------------------------------------------
variable_e_ep_calculation_2016 <- variable_e_ep_calculation_2018 %>%
  rename(x2016_variable_name = x2018_variable_name,
  x2016_table_field_calculation = x2018_table_field_calculation)

#E_AGE65, EP_AGE65 changed var_name in census
#no count data from census, the label "total estimate" is actually a percentage
# we calculate count from percentage
variable_e_ep_calculation_2016$x2016_table_field_calculation[8] <- "S0101_C01_028E * E_TOTPOP / 100"
variable_e_ep_calculation_2016$x2016_table_field_calculation[23] <- "S0101_C01_028E"

#this will overwrite var_cal2 from above for 2018
var_cal2 <- variable_e_ep_calculation_2016 %>%
  mutate(census_var = str_replace_all(x2016_table_field_calculation,
    "[^[:alnum:][:blank:]_]",
    " "))

census_variables_2016 <- map(0:5, theme_var_df)
#using same function as above for 2018
names(census_variables_2016) <- c("t0","t1","t2","t3","t4","t5")

usethis::use_data(variable_e_ep_calculation_2016, overwrite = TRUE)
usethis::use_data(census_variables_2016, overwrite = TRUE)


# Modify for 2015 (same as 2016)------------------------------------------------
variable_e_ep_calculation_2015 <- variable_e_ep_calculation_2016 %>%
  rename(x2015_variable_name = x2016_variable_name,
    x2015_table_field_calculation = x2016_table_field_calculation)

census_variables_2015 <- census_variables_2016

usethis::use_data(variable_e_ep_calculation_2015, overwrite = TRUE)
usethis::use_data(census_variables_2015, overwrite = TRUE)


# Modify for 2014 (based on 2016)-----------------------------------------------
variable_e_ep_calculation_2014 <- variable_e_ep_calculation_2016 %>%
  rename(x2014_variable_name = x2016_variable_name,
    x2014_table_field_calculation = x2016_table_field_calculation)

#E_CROWD
variable_e_ep_calculation_2014$x2014_table_field_calculation[16] <- "DP04_0077E +DP04_0078E"
#E_NOVEH
variable_e_ep_calculation_2014$x2014_table_field_calculation[17] <- "DP04_0057E"
#remember to check the EP_ value too!!! sometimes it's also pulled from census
#EP_CROWD no need to adjust
#EP_NOVEH
variable_e_ep_calculation_2014$x2014_table_field_calculation[32] <- "DP04_0057PE"

#check calculation column for line break (make sure there's none)

#assign var_cal2 again for theme_var_df function
var_cal2 <- variable_e_ep_calculation_2014 %>%
  mutate(census_var = str_replace_all(x2014_table_field_calculation,
    "[^[:alnum:][:blank:]_]",
    " "))

census_variables_2014 <- map(0:5, theme_var_df)
names(census_variables_2014) <- c("t0","t1","t2","t3","t4","t5")

usethis::use_data(variable_e_ep_calculation_2014, overwrite = TRUE)
usethis::use_data(census_variables_2014, overwrite = TRUE)


# Modify for 2013 (same as 2014) ------------------------------------------
#after checking for variables between years, 2014 and 2013 the same.
variable_e_ep_calculation_2013 <- variable_e_ep_calculation_2014 %>%
  rename(x2013_variable_name = x2014_variable_name,
    x2013_table_field_calculation = x2014_table_field_calculation)

census_variables_2013 <- census_variables_2014

usethis::use_data(variable_e_ep_calculation_2013, overwrite = TRUE)
usethis::use_data(census_variables_2013, overwrite = TRUE)

# Modify for 2012 (same as 2014, no UNINSUR variable in census) -----------
#checked variables, S2701_C04_001 for uninsured population not include in census
variable_e_ep_calculation_2012 <- variable_e_ep_calculation_2013 %>%
  rename(x2012_variable_name = x2013_variable_name,
    x2012_table_field_calculation = x2013_table_field_calculation) %>%
  filter(!theme == 5)

census_variables_2012 <- census_variables_2013[1:5] # no theme 5

usethis::use_data(variable_e_ep_calculation_2012, overwrite = TRUE)
usethis::use_data(census_variables_2012, overwrite = TRUE)
