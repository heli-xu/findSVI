#' Calculate SVI for communities in a region from census data
#'
#' @description `get_svi()` calculates and constructs an SVI table for a
#'   geographic level of interest based on [CDC/ATSDR SVI
#'   documentation](<https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html>).
#'   Briefly, by taking into account 4 themes of census variables that represent
#'   challenges in socioeconomic status, household characteristics, racial and
#'   ethnic minority status and housing/transportation, SVI uses percentile
#'   ranking within a region to indicate the relative social vulnerability of
#'   the geographic units (communities) in that region.
#'
#' @param year The year of interest (available 2014-2022), must match the year
#'   specified in retrieving census data.
#' @param data The census data retrieved by `get_census_data()`.
#'
#' @returns A tibble of SVI with rows representing geographic units, and columns
#'   indicating variable names (first two columns containing geographic
#'   information). For detailed description of the variable names (column
#'   names), please refer to [CDC/ATSDR
#'   documentation](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html).
#'
#' @examplesIf Sys.getenv("CENSUS_API_KEY") != ""
#' # Census API key required
#' pa2018 <- get_census_data(
#'   year = 2018,
#'   geography = "county",
#'   state = "PA"
#' )
#'
#' get_svi(2018, pa2018)
#'
#' @importFrom rlang .data
#'
#' @export
get_svi <- function(year, data) {
  # E_ & EP_

  filename <- paste0("variable_e_ep_calculation_", year)

  var_cal_table <- get(filename)

  ## set up theme 0 vector for E_var calculations
  var_0 <- var_cal_table %>%
    dplyr::filter(.data$theme == 0)

  var_0_name <- var_0[[1]]
  var_0_expr <- var_0[[3]]
  names(var_0_expr) <- var_0_name

  ## set up E_ vector
  E_var <-
    var_cal_table %>%
    dplyr::filter(
      .data$theme %in% c(1:4),
      stringr::str_detect(.[[1]], "E_")
    )

  E_var_name <- E_var[[1]]
  E_var_expr <- E_var[[3]]
  names(E_var_expr) <- E_var_name

  ## set up EP_ vector
  EP_var <-
    var_cal_table %>%
    dplyr::filter(
      .data$theme %in% c(1:4),
      stringr::str_detect(.[[1]], "EP_")
    )

  EP_var_name <- EP_var[[1]]
  EP_var_expr <- EP_var[[3]]
  names(EP_var_expr) <- EP_var_name

  if ("geometry" %in% colnames(data)) {
    data_tmp <- data %>%
      as.data.frame() %>%
      dplyr::select(-"geometry") %>%
      dplyr::as_tibble()
  } else {
    data_tmp <- data
  }

  ## iterate with E_ vector and THEN EP_ vector
  svi0 <-
    purrr::map2_dfc(var_0_name, var_0_expr, function(var_0_name, var_0_expr) {
      data_tmp %>%
        dplyr::transmute(
          {{ var_0_name }} := eval(str2lang({{ var_0_expr }}))
        )
    }) %>%
    dplyr::bind_cols(data_tmp, .)

  svi_e <-
    purrr::map2_dfc(E_var_name, E_var_expr, function(E_var_name, E_var_expr) {
      svi0 %>%
        dplyr::transmute(
          {{ E_var_name }} := eval(str2lang({{ E_var_expr }}))
        )
    }) %>%
    dplyr::bind_cols(svi0, .)

  svi_e_ep <-
    purrr::map2(EP_var_name, EP_var_expr, function(EP_var_name, EP_var_expr) {
      svi_e %>%
        dplyr::transmute(
          {{ EP_var_name }} := eval(str2lang({{ EP_var_expr }}))
        )
    }) %>%
    dplyr::bind_cols(svi_e, .) %>%
    # keep the new columns, GEOID, NAME
    dplyr::select(
      "GEOID",
      "NAME",
      tidyselect::all_of(var_0_name),
      tidyselect::all_of(E_var_name),
      tidyselect::all_of(EP_var_name)
    ) %>%
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(EP_var_name), ~ round(.x, 1)),
      E_AGE65 = dplyr::case_when(
        year >= 2017 ~ E_AGE65,
        TRUE ~ round(E_AGE65, 0)
      )
    )

  # EPL_ --------------------------------------------------------------------

  svi_epl <-
    svi_e_ep %>%
    dplyr::filter(
      .data$E_TOTPOP > 0,
      .data$E_HU > 0
    ) %>% # remove from ranking, but keep in table
    dplyr::select(
      "GEOID",
      "NAME",
      tidyselect::all_of(EP_var_name)
    ) %>%
    tidyr::pivot_longer(!c("GEOID", "NAME"),
      names_to = "svi_var",
      values_to = "value"
    ) %>%
    tidyr::drop_na("value") %>% # in case of variables missing in tracts
    dplyr::group_by(.data$svi_var) %>%
    dplyr::mutate(
      EPL_var = dplyr::case_when(
        year >= 2019 ~ dplyr::percent_rank(.data$value),
        .data$svi_var == "EP_PCI" ~ 1 - dplyr::percent_rank(.data$value),
        TRUE ~ dplyr::percent_rank(.data$value)
      ),
      EPL_var = round(EPL_var, 4)
    ) %>%
    dplyr::ungroup()

  # y <- svi_epl(2014, eep_data = x)

  # SPL_ and RPL_ for each theme --------------------------------------------

  xwalk_theme_var <- EP_var %>%
    dplyr::select(-3) %>%
    dplyr::rename(svi_var = 1)

  svi_spl_rpl <-
    svi_epl %>%
    # SPL_each theme
    dplyr::left_join(xwalk_theme_var, by = "svi_var") %>%
    dplyr::group_by(.data$theme, .data$GEOID, .data$NAME) %>%
    dplyr::summarise(
      SPL_theme = sum(EPL_var),
      .groups = "drop"
    ) %>%
    dplyr::ungroup() %>%
    # RPL_
    dplyr::group_by(.data$theme) %>%
    dplyr::mutate(
      RPL_theme = dplyr::percent_rank(.data$SPL_theme),
      RPL_theme = round(.data$RPL_theme, 4)
    ) %>%
    dplyr::ungroup()

  # z <- spl_rpl_tm(2014, epl_data = y)

  # SPL_ and RPL_ for all themes --------------------------------------------

  svi_spls_rpls <-
    svi_spl_rpl %>%
    dplyr::group_by(.data$GEOID, .data$NAME) %>%
    dplyr::summarise(
      SPL_themes = sum(SPL_theme),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      RPL_themes = round(dplyr::percent_rank(.data$SPL_themes), 4)
    ) %>%
    dplyr::ungroup()

  # merge all variabels to svi ----------------------------------------------

  EPL_var <-
    svi_epl %>%
    dplyr::mutate(
      EPL_var_name = paste0("EPL_", stringr::str_remove(.data$svi_var, "EP_")),
      .before = EPL_var
    ) %>%
    dplyr::select(-c("svi_var", "value")) %>%
    tidyr::pivot_wider(
      names_from = "EPL_var_name",
      values_from = "EPL_var"
    )

  SPL_theme <- svi_spl_rpl %>%
    dplyr::select(-"RPL_theme") %>%
    tidyr::pivot_wider(
      names_from = "theme",
      names_prefix = "SPL_theme",
      values_from = "SPL_theme"
    )

  RPL_theme <- svi_spl_rpl %>%
    dplyr::select(-"SPL_theme") %>%
    tidyr::pivot_wider(
      names_from = "theme",
      names_prefix = "RPL_theme",
      values_from = "RPL_theme"
    )

  SPL_RPL_themes <- svi_spls_rpls

  svi_complete <- list(
    svi_e_ep,
    EPL_var,
    SPL_theme,
    RPL_theme,
    SPL_RPL_themes
  ) %>%
    purrr::reduce(dplyr::left_join, by = c("GEOID", "NAME"))

  if ("geometry" %in% colnames(data)) {
    data_geo <- data %>%
      dplyr::select("GEOID", "NAME", "geometry")
    svi_complete_geo <- list(
      data_geo,
      svi_e_ep,
      EPL_var,
      SPL_theme,
      RPL_theme,
      SPL_RPL_themes
    ) %>%
      purrr::reduce(dplyr::left_join, by = c("GEOID", "NAME"))

    return(svi_complete_geo)
  }

  return(svi_complete)
}
