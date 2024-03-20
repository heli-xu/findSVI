test_that("2020 svi calculation works", {
  load(system.file("testdata", "cdc_pa_cty_svi2020.rda", package = "findSVI"))
  pa_cty_raw <- load(system.file("testdata", "pa_cty_raw2020.rda", package = "findSVI")) %>%
    get()
  output <- get_svi(2020, pa_cty_raw)

  join_RPL <- cdc_pa_cty_svi2020 %>%
    dplyr::select(GEOID,
      cdc_RPL_themes = RPL_THEMES,
      cdc_RPL_theme1 = RPL_THEME1,
      cdc_RPL_theme2 = RPL_THEME2,
      cdc_RPL_theme3 = RPL_THEME3,
      cdc_RPL_theme4 = RPL_THEME4
    ) %>%
    dplyr::mutate(GEOID = paste(GEOID)) %>%
    dplyr::left_join(output %>%
      dplyr::select(
        GEOID,
        RPL_themes,
        RPL_theme1,
        RPL_theme2,
        RPL_theme3,
        RPL_theme4
      )) %>%
    tidyr::drop_na() %>% ## remove NA rows
    dplyr::filter_all(dplyr::all_vars(. >= 0)) #-999 in cdc data

  check <- join_RPL %>%
    dplyr::transmute(
      overall = cor(cdc_RPL_themes, RPL_themes),
      theme1 = cor(cdc_RPL_theme1, RPL_theme1),
      theme2 = cor(cdc_RPL_theme2, RPL_theme2),
      theme3 = cor(cdc_RPL_theme3, RPL_theme3),
      theme4 = cor(cdc_RPL_theme4, RPL_theme4)
    ) %>%
    dplyr::distinct()

  expect_equal(as.numeric(check[1, ]), c(1, 1, 1, 1, 1), tolerance = 0.0001)
  # change tolerance from 0.00005 to 0.0001 for mac_release check
})

test_that("2018 svi calculation works", {
  load(system.file("testdata", "cdc_pa_cty_svi2018.rda", package = "findSVI"))
  pa_cty_raw <- load(system.file("testdata", "pa_cty_raw2018.rda", package = "findSVI")) %>%
    get()
  output <- get_svi(2018, pa_cty_raw)

  join_RPL <- cdc_pa_cty_svi2018 %>%
    dplyr::select(GEOID,
      cdc_RPL_themes = RPL_THEMES,
      cdc_RPL_theme1 = RPL_THEME1,
      cdc_RPL_theme2 = RPL_THEME2,
      cdc_RPL_theme3 = RPL_THEME3,
      cdc_RPL_theme4 = RPL_THEME4
    ) %>%
    dplyr::mutate(GEOID = paste(GEOID)) %>%
    dplyr::left_join(output %>%
      dplyr::select(
        GEOID,
        RPL_themes,
        RPL_theme1,
        RPL_theme2,
        RPL_theme3,
        RPL_theme4
      )) %>%
    tidyr::drop_na() %>% ## remove NA rows
    dplyr::filter_all(dplyr::all_vars(. >= 0)) #-999 in cdc data

  check <- join_RPL %>%
    dplyr::transmute(
      overall = cor(cdc_RPL_themes, RPL_themes),
      theme1 = cor(cdc_RPL_theme1, RPL_theme1),
      theme2 = cor(cdc_RPL_theme2, RPL_theme2),
      theme3 = cor(cdc_RPL_theme3, RPL_theme3),
      theme4 = cor(cdc_RPL_theme4, RPL_theme4)
    ) %>%
    dplyr::distinct()

  expect_equal(as.numeric(check[1, ]), c(1, 1, 1, 1, 1), tolerance = 0.0001)
  # change tolerance from 0.00005 to 0.0001 for mac_release check
})

test_that("2016 svi calculation works", {
  load(system.file("testdata", "cdc_pa_cty_svi2016.rda", package = "findSVI"))
  pa_cty_raw <- load(system.file("testdata", "pa_cty_raw2016.rda", package = "findSVI")) %>%
    get()
  output <- get_svi(2016, pa_cty_raw)

  join_RPL <- cdc_pa_cty_svi2016 %>%
    dplyr::select(GEOID,
      cdc_RPL_themes = RPL_THEMES,
      cdc_RPL_theme1 = RPL_THEME1,
      cdc_RPL_theme2 = RPL_THEME2,
      cdc_RPL_theme3 = RPL_THEME3,
      cdc_RPL_theme4 = RPL_THEME4
    ) %>%
    dplyr::mutate(GEOID = paste(GEOID)) %>%
    dplyr::left_join(output %>%
      dplyr::select(
        GEOID,
        RPL_themes,
        RPL_theme1,
        RPL_theme2,
        RPL_theme3,
        RPL_theme4
      )) %>%
    tidyr::drop_na() %>% ## remove NA rows
    dplyr::filter_all(dplyr::all_vars(. >= 0)) #-999 in cdc data

  check <- join_RPL %>%
    dplyr::transmute(
      overall = cor(cdc_RPL_themes, RPL_themes),
      theme1 = cor(cdc_RPL_theme1, RPL_theme1),
      theme2 = cor(cdc_RPL_theme2, RPL_theme2),
      theme3 = cor(cdc_RPL_theme3, RPL_theme3),
      theme4 = cor(cdc_RPL_theme4, RPL_theme4)
    ) %>%
    dplyr::distinct()

  expect_equal(as.numeric(check[1, ]), c(1, 1, 1, 1, 1), tolerance = 0.005)
})

test_that("2014 svi calculation works", {
  load(system.file("testdata", "cdc_pa_cty_svi2014.rda", package = "findSVI"))
  pa_cty_raw <- load(system.file("testdata", "pa_cty_raw2014.rda", package = "findSVI")) %>%
    get()
  output <- get_svi(2014, pa_cty_raw)

  join_RPL <- cdc_pa_cty_svi2014 %>%
    dplyr::select(GEOID,
      cdc_RPL_themes = RPL_THEMES,
      cdc_RPL_theme1 = RPL_THEME1,
      cdc_RPL_theme2 = RPL_THEME2,
      cdc_RPL_theme3 = RPL_THEME3,
      cdc_RPL_theme4 = RPL_THEME4
    ) %>%
    dplyr::mutate(GEOID = paste(GEOID)) %>%
    dplyr::left_join(output %>%
      dplyr::select(
        GEOID,
        RPL_themes,
        RPL_theme1,
        RPL_theme2,
        RPL_theme3,
        RPL_theme4
      )) %>%
    tidyr::drop_na() %>% ## remove NA rows
    dplyr::filter_all(dplyr::all_vars(. >= 0)) #-999 in cdc data

  check <- join_RPL %>%
    dplyr::transmute(
      overall = cor(cdc_RPL_themes, RPL_themes),
      theme1 = cor(cdc_RPL_theme1, RPL_theme1),
      theme2 = cor(cdc_RPL_theme2, RPL_theme2),
      theme3 = cor(cdc_RPL_theme3, RPL_theme3),
      theme4 = cor(cdc_RPL_theme4, RPL_theme4)
    ) %>%
    dplyr::distinct()

  expect_equal(as.numeric(check[1, ]), c(1, 1, 1, 1, 1), tolerance = 0.005)
})
