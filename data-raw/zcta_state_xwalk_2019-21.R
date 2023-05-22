library(readxl)
library(tidyverse)
library(janitor)

zcta_cty20  <- read.table("xls/tab20_zcta520_county20_natl.txt", sep = "|", header = T)
cty_st20 <- read_xlsx("xls/all-geocodes-v2020.xlsx", skip = 4) %>% #skip first 4 rows (table titles..)
  clean_names()
add_state <- cty_st20 %>%
  distinct(state_code_fips, .keep_all = TRUE) %>%
  select(state_code_fips, state = area_name_including_legal_statistical_area_description)

cty_st20_clean <- cty_st20 %>%
  mutate(GEOID = paste0(state_code_fips, county_code_fips)) %>%
  left_join(add_state, by = "state_code_fips") %>%
  select(GEOID,
    county = area_name_including_legal_statistical_area_description,
    state)


zcta_cty20_clean <- zcta_cty20 %>%
  select(NAMELSAD_ZCTA5_20, GEOID = GEOID_COUNTY_20, NAMELSAD_COUNTY_20) %>%
  mutate(GEOID = paste(GEOID)) %>%
  #remove_empty()
  #drop_na()
  #zcta_cty20_clean[1,1]--> not NA, that cell is ""
  filter(!NAMELSAD_ZCTA5_20 == "") %>%
  left_join(cty_st20_clean, by = "GEOID") %>%
  drop_na()

