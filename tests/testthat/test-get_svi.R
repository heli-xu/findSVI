test_that("2020 svi calculation works", {
  cdc_pa_cty_svi2020 <- readr::read_csv(system.file("testdata","2020svi_pa_co_cdc.csv", package = "findSVI")) %>%
    dplyr::rename(GEOID = FIPS)
  pa_cty_raw <- load(system.file("testdata","pa_cty_raw2020.rda",package = "findSVI")) %>%
    get()
  output <- get_svi(2020, pa_cty_raw)

  join_RPL <- cdc_pa_cty_svi2020 %>%
    dplyr::select(GEOID,
      cdc_RPL_themes = RPL_THEMES,
      cdc_RPL_theme1 = RPL_THEME1,
      cdc_RPL_theme2 = RPL_THEME2,
      cdc_RPL_theme3 = RPL_THEME3,
      cdc_RPL_theme4 = RPL_THEME4) %>%
    dplyr::mutate(GEOID = paste(GEOID)) %>%
    dplyr::left_join(output %>%
        dplyr::select(GEOID,
          RPL_themes,
          RPL_theme1,
          RPL_theme2,
          RPL_theme3,
          RPL_theme4)) %>%
    tidyr::drop_na() %>%   ## remove NA rows
    dplyr::filter_all(all_vars(. >= 0)) #-999 in cdc data

    check <- join_RPL %>%
      dplyr::transmute(overall = cor(cdc_RPL_themes, RPL_themes),
        theme1 = cor(cdc_RPL_theme1, RPL_theme1),
        theme2 = cor(cdc_RPL_theme2, RPL_theme2),
        theme3 = cor(cdc_RPL_theme3, RPL_theme3),
        theme4 = cor(cdc_RPL_theme4, RPL_theme4)) %>%
      dplyr::distinct()

    expect_equal(as.numeric(check[1,]), c(1,1,1,1,1), tolerance = 0.0001)

})

test_that("2018 svi calculation works", {
  cdc_pa_cty_svi2018 <- readr::read_csv(system.file("testdata","2018svi_pa_co_cdc.csv", package = "findSVI")) %>%
    dplyr::rename(GEOID = FIPS)
  pa_cty_raw <- load(system.file("testdata","pa_cty_raw2018.rda",package = "findSVI")) %>%
    get()
  output <- get_svi(2018, pa_cty_raw)

  join_RPL <- cdc_pa_cty_svi2018 %>%
    dplyr::select(GEOID,
      cdc_RPL_themes = RPL_THEMES,
      cdc_RPL_theme1 = RPL_THEME1,
      cdc_RPL_theme2 = RPL_THEME2,
      cdc_RPL_theme3 = RPL_THEME3,
      cdc_RPL_theme4 = RPL_THEME4) %>%
    dplyr::mutate(GEOID = paste(GEOID)) %>%
    dplyr::left_join(output %>%
        dplyr::select(GEOID,
          RPL_themes,
          RPL_theme1,
          RPL_theme2,
          RPL_theme3,
          RPL_theme4)) %>%
    tidyr::drop_na() %>%   ## remove NA rows
    dplyr::filter_all(all_vars(. >= 0)) #-999 in cdc data

  check <- join_RPL %>%
    dplyr::transmute(overall = cor(cdc_RPL_themes, RPL_themes),
      theme1 = cor(cdc_RPL_theme1, RPL_theme1),
      theme2 = cor(cdc_RPL_theme2, RPL_theme2),
      theme3 = cor(cdc_RPL_theme3, RPL_theme3),
      theme4 = cor(cdc_RPL_theme4, RPL_theme4)) %>%
    dplyr::distinct()

  expect_equal(as.numeric(check[1,]), c(1,1,1,1,1), tolerance = 0.0001)

})


