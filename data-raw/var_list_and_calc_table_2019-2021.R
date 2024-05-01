##code to prepare 2019-2021 census_variables and variable_e_ep_calculation datasets(.rda)
## https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
#documentation was downloaded as pdf, with the dictionary table manually imported in xlsx.
##use 2020 to prepare, then minor modify for 2019, 2021

library(tidyverse)
library(readxl)
library(skimr)
library(janitor)
library(stringr)


# Import 2020 dictionary -------------------------------------------------------

svi_var <- read_xlsx("data-raw/xls/2020svi_dictionary.xlsx") %>%
  clean_names()

skim(svi_var)


# Remove MOE --------------------------------------------------------------

#during import into excel:
#select the autodetect tables across pages during import table to excel from pdf
#so you don't have to deal with page numbers

#*keeping rows with na in variable names -some are wrapped content from previous row*

svi_var_clean <- svi_var %>%
  mutate(x2020_variable_name = replace_na(x2020_variable_name, "x")) %>%
  filter(!str_starts(x2020_variable_name,"M"))

# Simplify and modify table --------------------------------------
## filter theme, variable, calculation
var_cal <- svi_var_clean %>%
  select(x2020_variable_name, theme, x2020_table_field_calculation) %>%
  drop_na(x2020_table_field_calculation) %>%
  filter(!str_detect(x2020_table_field_calculation,"\\^"))

## Inspect, modify calculations that spill over (within E_, EP_)
##(something like "10 0" will be dealt with later - it's the \r\n)

var_cal$x2020_table_field_calculation[29] <- "(E_POV150 /S1701_C01_001E) * 100"


# Extract E_ and EP_ vars with calculation-------------------------------------

var_prefix <- c("E_", "EP_")

var_cal2 <-
  var_cal %>%
  filter(str_detect(x2020_variable_name, paste(var_prefix, collapse = "|"))) %>%
  #modify the blank themes
  mutate(theme = case_when(
    x2020_variable_name%in%c("E_TOTPOP","E_HU","E_HH") ~ 0,  #must be same class
    is.na(theme) ~ 5, #adjunct variables
    TRUE ~ theme
  )) %>%
  mutate(
    #pdf to excel introduce line breaks, not visible in view- use $ index to see
    x2020_table_field_calculation = str_replace_all(x2020_table_field_calculation, "\r\n",""),
    census_var = str_replace_all(x2020_table_field_calculation,
      "[^[:alnum:][:blank:]_]",
      " "))
    #replace with blank instead of nothing:
    #100 will be separated by at least one blank with string

# SAVE: Table for svi calculation ----------------------------
variable_e_ep_calculation_2020 <- var_cal2 %>%
  select(-census_var)

# SAVE: List of variables from each theme----------------------
theme_var_df <- function(n){
  var_cal2 %>%
    filter(theme == n) %>%
    select(x2020_variable_name, census_var) %>%
    separate_rows(census_var, sep = " ")  %>%
    filter(!str_starts(census_var, "E_"),
      !census_var%in%c("","100")) %>%
    pull(census_var)
}

census_variables_2020 <- map(0:5, theme_var_df)

##name elements in the list by theme (t0 = total, t5 = adjunct)
names(census_variables_2020) <- c("t0","t1","t2","t3","t4","t5")


usethis::use_data(variable_e_ep_calculation_2020, overwrite = TRUE)
usethis::use_data(census_variables_2020, overwrite = TRUE)


# Modify datasets for 2019 ------------------------------------------------
variable_e_ep_calculation_2019 <- variable_e_ep_calculation_2020 %>%
  rename(x2019_variable_name = x2020_variable_name,
    x2019_table_field_calculation = x2020_table_field_calculation)

census_variables_2019 <- census_variables_2020

usethis::use_data(variable_e_ep_calculation_2019, overwrite = TRUE)
usethis::use_data(census_variables_2019, overwrite = TRUE)


# Modify datasets for 2021 ------------------------------------------------
variable_e_ep_calculation_2021 <- variable_e_ep_calculation_2020 %>%
  rename(x2021_variable_name = x2020_variable_name,
    x2021_table_field_calculation = x2020_table_field_calculation)

census_variables_2021 <- census_variables_2020

usethis::use_data(variable_e_ep_calculation_2021, overwrite = TRUE)
usethis::use_data(census_variables_2021, overwrite = TRUE)

# Modify using explicitly defined denominator (2020) -----------------
#EP_UNEMP
variable_e_ep_calculation_2020$x2020_table_field_calculation[21] <- "(E_UNEMP /	DP03_0003) * 100"
#EP_NOHSDP/25year and over
variable_e_ep_calculation_2020$x2020_table_field_calculation[23] <- "(E_NOHSDP /	S0501_C01_038) * 100"
#EP_UNINSUR/noninstitutionalized civilian
variable_e_ep_calculation_2020$x2020_table_field_calculation[24] <- "(E_UNINSUR / S2701_C01_001) * 100"
#EP_AGE65
variable_e_ep_calculation_2020$x2020_table_field_calculation[25] <- "(E_AGE65 / E_TOTPOP) * 100"
#EP_DISABL/noninsti
variable_e_ep_calculation_2020$x2020_table_field_calculation[27] <- "(E_DISABL / S2701_C01_001) * 100"
#EP_MOBILE/total housing unit (DP05_0086?)
variable_e_ep_calculation_2020$x2020_table_field_calculation[32] <- "(E_MOBILE / DP04_0001) * 100"
#EP_NOVEH/Occupied housing units!DP04_0002
variable_e_ep_calculation_2020$x2020_table_field_calculation[34] <- "(E_NOVEH / DP04_0002) * 100"

variable_e_denom_2020 <- variable_e_ep_calculation_2020

var_denom <- variable_e_denom_2020 %>%
  mutate(census_var = str_replace_all(x2020_table_field_calculation,
    "[^[:alnum:][:blank:]_]",
    " "))

theme_var_df <- function(n){
  var_denom %>%  #different df, otherwise same as above
    filter(theme == n) %>%
    select(x2020_variable_name, census_var) %>%
    separate_rows(census_var, sep = " ")  %>%
    filter(!str_starts(census_var, "E_"),
      !census_var%in%c("","100")) %>%
    pull(census_var)
}

census_variables_2020_new <- map(0:5, theme_var_df)

usethis::use_data(variable_e_denom_2020, overwrite = TRUE)
usethis::use_data(census_variables_2020_new, overwrite = TRUE)
