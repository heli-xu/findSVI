library(dplyr)
library(sf)
library(tmap)
library(findSVI)


# country---------------

#us_svi <- read_csv("../svi-calculation/cdc_us_svi/SVI2020_US.csv")

us_svi <- st_read("../../SVI2020_US_tract.gdb/")

#us_svi_layers <- st_layers(dsn = "../../SVI2020_US_tract.gdb")
#only 1 layer

us_svi_phl <- us_svi %>%
  select(1:7, contains("RPL_THEME")) %>%
  rename(GEOID = FIPS,
    #format switched to ESRI gdb, col name change -7/28/23
    geometry = Shape) %>%
  filter(RPL_THEMES >= 0,
    ST_ABBR == "PA",
    COUNTY == "Philadelphia")

# state---------------------------------
unzip("../../../Pennsylvania.zip",
  exdir = "pa_ct_2020_shapefile", junkpaths = T,
  overwrite = F)

cdc_svi_pa_ct_2020 <- sf::st_read("pa_ct_2020_shapefile/SVI2020_PENNSYLVANIA_tract.shp")

pa_svi_phl <- cdc_svi_pa_ct_2020 %>%
  select(1:7, contains("RPL_THEME")) %>%
  rename(GEOID = FIPS) %>%
  #CDC use -999 as NAs
  filter(RPL_THEMES>= 0,
    COUNTY == "Philadelphia")


{ # Correlation matrix ------------------------------------------------------

  cor(us_svi_phl$RPL_THEMES, pa_svi_phl$RPL_THEMES)

  df_svi = list(
    us_svi_phl %>% mutate(grp = 'us') %>% as.data.frame() %>% select(grp, GEOID, RPL_THEMES),
    pa_svi_phl %>% mutate(grp = 'state') %>% as.data.frame() %>% select(grp, GEOID, RPL_THEMES),
    phl_svi %>% mutate(grp = 'county')  %>% as.data.frame() %>% select(grp, GEOID, RPL_THEMES = RPL_themes)
  ) %>%
    bind_rows() %>% as_tibble()

  template = tibble(a = unique(df_svi$grp),
    b = list(unique(df_svi$grp))) %>%
    unnest(col = b)
  df_cor = template %>%
    group_by(row_number()) %>%
    group_map(function(row,i){
      # row = template %>% slice(8)
      data_a = df_svi %>%
        filter(grp == row$a)
      data_b = df_svi %>%
        filter(grp == row$b,
          GEOID%in%data_a$GEOID)
      data_a = data_a %>%
        filter(GEOID %in% data_b$GEOID)
      row %>%
        mutate(cor = cor(data_a$RPL_THEMES, data_b$RPL_THEMES) ) %>%
        return()
    }) %>%
    bind_rows()

  df_cor_matrix

  df_cor %>% pivot_wider(names_from = a, values_from = cor)
  }


# county------------------
phl_ct_2020_data <- get_census_data(
  2020,
  state = "PA",
  county = "Philadelphia",
  geography = "tract",
  geometry = TRUE
)

phl_ct_svi_2020 <- get_svi(2020, phl_ct_2020_data)

save(pa_svi_phl, file = "xtra/pa_svi_phl_2020_geo.rda")
save(us_svi_phl, file = "xtra/us_svi_phl_2020_geo.rda")
save(phl_svi, file = "xtra/phl_svi_2020_geo.rda")


match_id <- pa_svi_phl$GEOID

phl_svi <- phl_ct_svi_2020 %>%
  select(GEOID, contains("RPL_theme")) %>%
  drop_na() %>%
  #to make sure they have same GEOIDs
  filter(GEOID %in% match_id)



#map
# given_breaks <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
# num_breaks <- 10
# break_intervals <- seq(given_breaks[1], given_breaks[length(given_breaks)], length.out = num_breaks + 1)
# generated_breaks <- c(break_intervals[-length(break_intervals)],1)
#


# Print the result


country <- us_svi_phl %>%
  select(GEOID, geometry, RPL_THEMES) %>%
  drop_na() %>%
  tm_shape(projection = sf::st_crs(26915))+
  tm_polygons("RPL_THEMES",
    palette = c("orange","navy"),
    style = "cont",
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    title = "SVI (by tract)")+
  tm_layout(title = "Nation-level SVI: \nPhilly subset",
    title.size = 1,
    title.position = c("left", "TOP"),
    legend.position = c("RIGHT", "bottom"),
    legend.title.size = 0.9,
    legend.width = 2)

state <- pa_svi_phl %>%
  select(GEOID, geometry, RPL_THEMES) %>%
  drop_na() %>%
  tm_shape(projection = sf::st_crs(26915))+
  tm_polygons("RPL_THEMES",
    palette = c("orange","navy"),
    style = "cont",
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    title = "SVI (by tract)")+
  tm_layout(title = "PA-level SVI: \nPhilly subset",
    title.size = 1,
    title.position = c("left", "TOP"),
    legend.position = c("RIGHT", "bottom"),
    legend.title.size = 0.9,
    legend.width = 2)

county <- phl_svi %>%
  select(GEOID, geometry, RPL_themes) %>%
  drop_na() %>%
  tm_shape(projection = sf::st_crs(26915))+
  tm_polygons("RPL_themes",
    palette = c("orange","navy"),
    style = "cont",
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    title = "SVI (by tract)")+
  tm_layout(title = "County-level SVI: \nPhilly",
    title.size = 1,
    title.position = c("left", "TOP"),
    legend.position = c("RIGHT", "bottom"),
    legend.title.size = 0.9,
    legend.width = 2)

plots <- list(country, state, county)

current.mode <- tmap_mode("plot")

tmap_arrange(
  plots,
  nrow = 1,
  width = c(0.34, 0.33, 0.33)
)


