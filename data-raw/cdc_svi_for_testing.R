# 2022 ----
cdc_pa_cty_svi2022 <- read_csv("data-raw/xls/2022svi_pa_co_cdc.csv") |>
  dplyr::select(ST, GEOID = FIPS, contains("RPL_"))

save(cdc_pa_cty_svi2022, file = "inst/testdata/cdc_pa_cty_svi2022.rda")

# 2020 ----
cdc_pa_cty_svi2020 <- read_csv("data-raw/xls/2020svi_pa_co_cdc.csv") |>
  dplyr::select(ST, GEOID = FIPS, contains("RPL_"))

save(cdc_pa_cty_svi2020, file = "inst/testdata/cdc_pa_cty_svi2020.rda")

# 2018 ----
cdc_pa_cty_svi2018 <- read_csv("data-raw/xls/2018svi_pa_co_cdc.csv") |>
  dplyr::select(ST, GEOID = FIPS, contains("RPL_"))

save(cdc_pa_cty_svi2018, file = "inst/testdata/cdc_pa_cty_svi2018.rda")

# 2016-------
cdc_pa_cty_svi2016 <- read_csv("data-raw/xls/2016svi_pa_co_cdc.csv") |>
  dplyr::select(ST, GEOID = FIPS, contains("RPL_"))

save(cdc_pa_cty_svi2016, file = "inst/testdata/cdc_pa_cty_svi2016.rda")

# 2014 --------
cdc_pa_cty_svi2014 <- read_csv("data-raw/xls/2014svi_pa_co_cdc.csv") |>
  dplyr::select(ST, GEOID = FIPS, contains("RPL_"))

save(cdc_pa_cty_svi2014, file = "inst/testdata/cdc_pa_cty_svi2014.rda")

# 2010 -------
cdc_pa_cty_svi2010 <- read_csv("data-raw/xls/2010svi_pa_co_cdc.csv") |>
  dplyr::select(ST, GEOID = FIPS, contains("R_PL_")) #note diff col name

save(cdc_pa_cty_svi2010, file = "inst/testdata/cdc_pa_cty_svi2010.rda")
