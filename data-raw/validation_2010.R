library(tidyverse)


cdc <- read_csv("data-raw/xls/2010svi_pa_co_cdc.csv") |>
  select(!contains("M_"), GEOID = FIPS) |>
  dplyr::mutate(GEOID = paste(GEOID))

pa_cty_raw <- load(system.file("testdata","pa_cty_raw2010.rda",package = "findSVI")) %>%
  get()

output <- get_svi(2010, pa_cty_raw)

join_RPL <- cdc |>
  select(
    GEOID,
    cdc_RPL_themes = R_PL_THEMES, #note col name change
    cdc_RPL_theme1 = R_PL_THEME1,
    cdc_RPL_theme2 = R_PL_THEME2,
    cdc_RPL_theme3 = R_PL_THEME3,
    cdc_RPL_theme4 = R_PL_THEME4) %>%
  dplyr::left_join(output %>%
      dplyr::select(GEOID,
        RPL_themes,
        RPL_theme1,
        RPL_theme2,
        RPL_theme3,
        RPL_theme4)) |>
  tidyr::drop_na() %>%   ## remove NA rows
  dplyr::filter_all(dplyr::all_vars(. >= 0))

join_RPL |>
  ggplot()+
  geom_point(aes(x = cdc_RPL_themes, y = RPL_themes))+
  geom_abline(intercept = 0, slope = 1, linetype = 2)

join_RPL |>
  ggplot()+
  geom_point(aes(x = cdc_RPL_theme2, y = RPL_theme2))+
  geom_abline(intercept = 0, slope =1, linetype =2)

high_diff_id <- join_RPL |>
  mutate(diff = abs(cdc_RPL_themes - RPL_themes)) |>
  arrange(desc(diff)) |>
  head(10) |>
  pull(GEOID)

var_t2 <- c("AGE65|AGE17|SNG")
# with E_ are from ACS
# without are from decennials

high_variab <- cdc |>
  select(GEOID, matches(var_t2), contains("R_PL")) |>
  left_join(output |> select(GEOID, matches(var_t2), contains("RPL")), by = "GEOID") |>
  filter(GEOID %in% high_diff_id)

E_var <- high_variab |> select(GEOID, AGE65, E_AGE65, AGE17, E_AGE17, SNGPRNT, E_SNGPNT)

ep_var <- high_variab |> select(GEOID, starts_with("EP_"), starts_with("P_"))

rpl <- high_variab |> select(GEOID, starts_with("R_PL"), starts_with("RPL_"))


join_RPL |>
  filter(!GEOID %in% high_diff_id) |>
  ggplot()+
  geom_point(aes(x = cdc_RPL_themes, y = RPL_themes))+
  geom_abline(intercept = 0, slope = 1, linetype = 2)

join_RPL |>
  filter(!GEOID %in% high_diff_id) |>
  mutate(cor = cor(cdc_RPL_themes, RPL_themes), .before = GEOID)

# low correlation because of decennial vs ACS data
