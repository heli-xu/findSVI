#code to generate zcta_state_xwalk2019, zcta_state_xwalk2020, zcta_state_xwalk2021.rda
#CRAN tidycensus doesn't support pulling zcta data by state (2023-5-23)

library(readxl)
library(readr)
library(tidyverse)
library(janitor)
library(stringr)

# zcta_state_xwalk2022---------------------------------

## zcta-county-----------------------------
#same as 2020
zcta_cty20 <- read_csv("xls/zcta_county_2020_geocorr.csv") %>%
  slice(-1) %>%
  clean_names()

st_abb <- zcta_cty20 %>%
  select(GEOID = county, county_name) %>%
  mutate(st_code = str_sub(GEOID, 1, 2),
    st_abb = str_sub(county_name, -2)) %>%
  select(st_code, st_abb) %>%
  distinct(st_code, .keep_all = TRUE)


## county-state--------------------
cty_st22 <- read_xlsx("xls/all-geocodes-v2022.xlsx", skip = 4) %>%
  #skip first 4 rows (table titles..)
  clean_names()
# change to county-equivalents in CT for 2022
#https://www.census.gov/programs-surveys/acs/technical-documentation/user-notes/2023-01.html
#only affect 1 year ACS, 5 year ACS still follows prior2022 boundaries

### modify CT-----------
cty_st21_ct <- read_xlsx("xls/all-geocodes-v2021.xlsx", skip = 4) %>%
  clean_names() %>%
  filter(state_code_fips == "09")

#this double assign just to make column names auto complete
cty_st21_ct <- cty_st21_ct %>%
  rename(
    state_fips_code = state_code_fips,
    county_fips_code = county_code_fips,
    county_subdivision_fips_code = county_subdivision_code_fips,
    place_fips_code = place_code_fips,
    consolidtated_city_fips_code = consolidtated_city_code_fips,
    area_name = area_name_including_legal_statistical_area_description
    )

cty_st22_ct <- cty_st22 %>%
  filter(!state_fips_code == "09") %>%
  bind_rows(cty_st21_ct)

#join state name and abbr
add_state <- cty_st22_ct %>%
  distinct(state_fips_code, .keep_all = TRUE) %>%
  select(st_code = state_fips_code,
    state = area_name) %>% #col names updated
  left_join(st_abb, by = "st_code") %>%
  drop_na()


#add state abb and full name to county-state
cty_st22_clean <- cty_st22_ct %>%
  rename(st_code = state_fips_code) %>%
  mutate(GEOID = paste0(st_code, county_fips_code)) %>%
  left_join(add_state, by = "st_code") %>%
  select(st_code,
    state,
    GEOID,
    county = area_name,
    st_abb)

cty_st22_clean %>% count(state)
#53 rows with 1 NA (whole US)
# turn out the same as 2021

diff <- cty_st22_clean %>%
  select(GEOID, st_code, county) %>%
  left_join(cty_st21_clean %>%
      select(GEOID, st_code, county),
    by = c("st_code","county")
) %>%
  mutate(diff = as.numeric(GEOID.x)-as.numeric(GEOID.y))
#some of the "city" has two GEOID, like 51000 and 51840

## join zcta-county-state----------
zcta_state_xwalk2022 <- zcta_cty20 %>%
  select(GEOID = county, ZCTA = zcta, county_name) %>%
  #GEOID(county) already character and right length with pad"0"
  left_join(cty_st22_clean, by = "GEOID", multiple = "all") %>%
  #there will be multimatch
  drop_na() %>%
  select(ZCTA, st_code, county, state, st_abb) %>%
#not selecting GEOID to avoid confusion, pulled data(for zcta) GEOID is ZCTA
  distinct()

zcta_state_xwalk2022 <- zcta_state_xwalk2022 %>%
  mutate(st_code = as.numeric(st_code))

#check how many states
z <- zcta_state_xwalk2022 %>% group_by(state) %>% count()

usethis::use_data(zcta_state_xwalk2022, overwrite = TRUE)

# zcta_state_xwalk2021 ----------------------------------------------------

##zcta-county relationship file(2020)------

#zcta_cty20  <- read.table("xls/zcta_county_2020.txt", sep = "|", header = T)
#only has 9695 rows (ZCTAs), matching to 19 states only
#use Geocorr2022 data instead:
#https://mcdc.missouri.edu/applications/geocorr2022.html
zcta_cty20 <- read_csv("xls/zcta_county_2020_geocorr.csv") %>%
  slice(-1) %>% #a row of col_name explanation
  clean_names()

##state and abbreviation...
st_abb <- zcta_cty20 %>%
  select(GEOID = county, county_name) %>%
  mutate(st_code = str_sub(GEOID, 1, 2),
    st_abb = str_sub(county_name, -2)) %>%
  select(st_code, st_abb) %>%
  distinct(st_code, .keep_all = TRUE)

##county-state reference file(2021) ---------
#https://www.census.gov/programs-surveys/popest/geographies/reference-files.html
cty_st21 <- read_xlsx("xls/all-geocodes-v2021.xlsx", skip = 4) %>% #skip first 4 rows (table titles..)
  clean_names()

#state full name
add_state <- cty_st21 %>%
  distinct(state_code_fips, .keep_all = TRUE) %>%
  select(st_code = state_code_fips,
    state = area_name_including_legal_statistical_area_description) %>%
  left_join(st_abb, by = "st_code") %>%
  drop_na()


#add state abb and full name
cty_st21_clean <- cty_st21 %>%
  rename(st_code = state_code_fips) %>%
  mutate(GEOID = paste0(st_code, county_code_fips)) %>%
  left_join(add_state, by = "st_code") %>%
  select(st_code,
    state,
    GEOID,
    county = area_name_including_legal_statistical_area_description,
    st_abb)

cty_st21_clean %>% count(state)

zcta_state_xwalk2021 <- zcta_cty20 %>%
  select(GEOID = county, ZCTA = zcta, county_name) %>%
  #GEOID(county) already character and right length with pad"0"
  left_join(cty_st21_clean, by = "GEOID", multiple = "all") %>%
  drop_na() %>%
  select(ZCTA, st_code, county, state, st_abb)
  #not selecting GEOID to avoid confusion, pulled data(for zcta) GEOID is ZCTA

zcta_state_xwalk2021 <- zcta_state_xwalk2021 %>%
  mutate(st_code = as.numeric(st_code))

#check how many states
x <- zcta_state_xwalk2021 %>% group_by(state) %>% count()

#check repeats
zcta_state_xwalk2021 <- zcta_state_xwalk2021 %>% distinct()

usethis::use_data(zcta_state_xwalk2021, overwrite = TRUE)


# zcta_state_2020 ---------------------------------------------------------

### ZCTA-county--------------
#relationship file only has 9695 zcta entries, matched to 22 states, not complete
#zcta_cty20  <- read.table("xls/zcta_county_2020.txt", sep = "|", header = T)
#here use https://mcdc.missouri.edu/applications/geocorr2022.html
zcta_cty20 <- read_csv("xls/zcta_county_2020_geocorr.csv") %>%
  slice(-1) %>% #a row of col_name explanation
  clean_names()

#st_abb (same as above)
st_abb <- zcta_cty20 %>%
  select(GEOID = county, county_name) %>%
  mutate(st_code = str_sub(GEOID, 1, 2),
    st_abb = str_sub(county_name, -2)) %>%
  select(st_code, st_abb) %>%
  distinct(st_code, .keep_all = TRUE)

## county-state-----------------
#county to state reference file(2020):
#https://www.census.gov/programs-surveys/popest/geographies/reference-files.html
cty_st20 <- read_xlsx("xls/all-geocodes-v2020.xlsx", skip = 4) %>% #skip first 4 rows (table titles..)
  clean_names()

add_state <- cty_st20 %>%
  distinct(state_code_fips, .keep_all = TRUE) %>%
  select(st_code = state_code_fips,
    state = area_name_including_legal_statistical_area_description) %>%
  left_join(st_abb, by = "st_code") %>%
  drop_na()


cty_st20_clean <- cty_st20 %>%
  rename(st_code = state_code_fips) %>%
  mutate(GEOID = paste0(st_code, county_code_fips)) %>%
  left_join(add_state, by = "st_code") %>%
  select(st_code,
    state,
    GEOID,
    county = area_name_including_legal_statistical_area_description,
    st_abb)

## join ZCTA-county-state--------------------
zcta_state_xwalk2020 <- zcta_cty20 %>%
  select(GEOID = county, ZCTA = zcta, county_name) %>%
  #GEOID(county) already character and right length with pad"0"
  left_join(cty_st20_clean, by = "GEOID", multiple = "all") %>%
  drop_na() %>%
  select(ZCTA, st_code, county, state, st_abb)
  #not selecting GEOID to avoid confusion, pulled data(for zcta) GEOID is ZCTA
  #ZCTA padded with 0 already

zcta_state_xwalk2020 <- zcta_state_xwalk2020 %>%
  mutate(st_code = as.numeric(st_code))

#checking how many states included (52)
y2 <- zcta_state_xwalk2020 %>% group_by(state) %>% count()

#check repeats
zcta_state_xwalk2020 <- zcta_state_xwalk2020 %>% distinct()

usethis::use_data(zcta_state_xwalk2020, overwrite = TRUE)


# zcta_state_xwalk2019 ----------------------------------------------------
## ZCTA-county----------------
#zcta to county relationship file(2010)
#https://www.census.gov/geographies/reference-files/time-series/geo/relationship-files.2010.html#list-tab-1709067297
zcta_cty10 <-
  read.table(paste0("xls/zcta_county_2010.txt"),
    sep = ",",
    header = T)



## county-state---------------
#county to state reference file(2019)
#https://www.census.gov/programs-surveys/popest/geographies/reference-files.html
cty_st19 <-
  read_xlsx(paste0("xls/all-geocodes-v2019.xlsx"), skip = 4) %>% #skip first 4 rows (table titles..)
  clean_names()

#same add_state
add_state <- cty_st19 %>%
  distinct(state_code_fips, .keep_all = TRUE) %>%
  select(st_code = state_code_fips,
    state = area_name_including_legal_statistical_area_description) %>%
  left_join(st_abb, by = "st_code") %>%  #st_abb from 2020table, but the same
  drop_na()


#add separate column of state abb/full name
cty_st19_clean <- cty_st19 %>%
  rename(st_code = state_code_fips) %>%
  mutate(GEOID = paste0(st_code, county_code_fips)) %>%
  left_join(add_state, by = "st_code") %>%
  select(st_code,
    state,
    GEOID,
    county = area_name_including_legal_statistical_area_description,
    st_abb)

## join ZCTA-county-state---------
zcta_state_xwalk2019 <- zcta_cty10 %>%
  select(ZCTA5, GEOID) %>%
  ##Below step VERY important!! state code of 01 will be 1 and not matching.
  #also GEOID originally is numeric from file -->character
  mutate(GEOID = str_pad(GEOID, width = 5, side = "left", pad = "0")) %>%
  #remove_empty()
  #drop_na()
  #zcta_cty10_clean[1,1]--> not NA, that cell is ""
  filter(!ZCTA5 == "") %>%
  left_join(cty_st19_clean, by = "GEOID", multiple = "all") %>%
  drop_na() %>%
  select(ZCTA = ZCTA5, st_code, county, state, st_abb) %>%
  mutate(ZCTA = str_pad(ZCTA, width = 5, side = "left", pad = "0"))

zcta_state_xwalk2019 <- zcta_state_xwalk2019 %>%
  mutate(st_code = as.numeric(st_code))

#check how many states
x <- zcta_state_xwalk2019 %>% group_by(state) %>% count()

#check repeats
zcta_state_xwalk2019 <- zcta_state_xwalk2019 %>% distinct()

usethis::use_data(zcta_state_xwalk2019, overwrite = TRUE)


# Extra: valid state table ----------------------------------------------

state_valid <- zcta_state_xwalk2020 %>%
  distinct(st_abb, state, st_code) %>%
  rename(st_name = state,
    st_abbr = st_abb,
    fips_code = st_code)

usethis::use_data(state_valid, overwrite = TRUE)
