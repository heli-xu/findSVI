library(sf)
library(tidyverse)
library(leaflet)

# CommutingZones2020_County_GIS_files
# data source: https://sites.psu.edu/psucz/files/2024/01/CommutingZones2020_County_GIS_files-d81d51023719c241.zip

cty_cz_20 <- st_read("../xtra/CommutingZones2020_County_GIS_files-d81d51023719c241/county20.shp")

cty_cz_20 %>%
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  leaflet() %>%
    addTiles() %>%
    addPolygons(color = "blue",
      fillColor = "blue")

cty_cz_2020_xwalk <- cty_cz_20 %>%
  st_drop_geometry() %>%
  select(GEOID, GEOID2 = CZ20)

usethis::use_data(cty_cz_2020_xwalk, overwrite = TRUE)

data <- get_census_data(2020, "county")

#crosswalk doesn't have 'NAME' column
data_cz <- data %>%
  left_join(cty_cz_2020_xwalk, by = "GEOID")

# a <- cty_cz_20_xwalk %>%
#   left_join(data, by = "GEOID") %>%
#   filter(is.na(NAME))


data_cz2 <- data_cz %>%
  select(-NAME) %>%
  group_by(GEOID2) %>%
  summarise(
    across(-GEOID, ~sum(.x), .names = "{.col}")
  ) %>%
  rename(GEOID = GEOID2) %>%
  mutate(NAME = "")

svi_cz <- get_svi(2020, data_cz2)

#crosswalk has `NAME` column
test <- cty_cz_2020_xwalk %>%
  mutate(NAME = "a")

test2 <- data %>%
  select(-NAME) %>%
  left_join(test, by = "GEOID") %>%
  group_by(GEOID2) %>%
  summarise(
    across(-c(GEOID, NAME), ~sum(.x), .names = "{.col}")
  ) %>%
  left_join(test %>% select(NAME, GEOID2) %>% distinct(), by = "GEOID2") %>%
  rename(GEOID = GEOID2)

get_svi(2020, test2)


#testing unionizing geometries
cz_sf <- cty_cz_20 %>%
  select(-CBSA) %>%
  group_by(CZ20) %>%
  summarise(geometry = st_union(geometry))

svi_cz_sf <- cz_sf %>%
  left_join(svi_cz, by = c("CZ20" = "GEOID"))

ggplot(svi_cz_sf) +
  geom_sf(aes(fill = RPL_themes))

#when geometry comes from data
data_geo <- data %>%
  left_join(cty_cz_20 %>% select(GEOID, geometry), by = "GEOID") %>%
  st_as_sf()

xwalk_geo <- data_geo %>%
  dplyr::select(GEOID, geometry) %>%
  dplyr::left_join(cty_cz_2020_xwalk, by = "GEOID") %>%
  dplyr::group_by(GEOID2) %>%
  dplyr::summarise(geometry = st_union(geometry))

#visual of county and cz
leaflet() %>%
  addTiles() %>%
  addPolygons(data = xwalk_geo %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    color = "blue",
    fillColor = "blue") %>%
  addPolygons(data = cty_cz_20 %>%
      st_transform(crs = st_crs("+proj=longlat +datum=WGS84")),
    color = "yellow", weight = 0.3)
