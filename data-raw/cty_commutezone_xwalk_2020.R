library(sf)
library(tidyverse)
library(leaflet)

# CommutingZones2020_County_GIS_files
# data source: https://sites.psu.edu/psucz/files/2024/01/CommutingZones2020_County_GIS_files-d81d51023719c241.zip

cty_cz_20 <- st_read("../xtra/CommutingZones2020_County_GIS_files-d81d51023719c241/county20.shp")

cty_cz_2020_xwalk <- cty_cz_20 %>%
  st_drop_geometry() %>%
  select(GEOID, CZ20)

cty_cz_20 %>%
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  leaflet() %>%
    addTiles() %>%
    addPolygons(color = "blue",
      fillColor = "blue")

usethis::use_data(cty_cz_2020_xwalk, overwrite = TRUE)

data <- get_census_data(2020, "county")

data_cz <- data %>%
  left_join(cty_cz_20_xwalk, by = "GEOID")

a <- cty_cz_20_xwalk %>%
  left_join(data, by = "GEOID") %>%
  filter(is.na(NAME))

data_cz2 <- data_cz %>%
  group_by(CZ20) %>%
  summarise(
    across(-c(GEOID, NAME), ~sum(.x), .names = "{.col}")
  ) %>%
  rename(GEOID = CZ20) %>%
  mutate(NAME = "")

svi_cz <- get_svi(2020, data_cz2)

cz_sf <- cty_cz_20 %>%
  select(-CBSA) %>%
  group_by(CZ20) %>%
  summarise(geometry = st_union(geometry))

cz_sf %>%
  st_transform(crs = st_crs("+proj=longlat +datum=WGS84")) %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(color = "blue",
    fillColor = "blue")

svi_cz_sf <- cz_sf %>%
  left_join(svi_cz, by = c("CZ20" = "GEOID"))

ggplot(svi_cz_sf) +
  geom_sf(aes(fill = RPL_themes))
