#' Calculate SVI for communities from census data using customized boundaries
#'
#' @description `get_svi_x()` calculates and constructs an SVI table for a
#'   *customized* geographic level of interest based on [CDC/ATSDR SVI
#'   documentation](<https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html>).
#'   By supplying a crosswalk (relationship table) between a Census geographic
#'   level and a customized geographic level, census data are summed across the
#'   customized geographic units, and SVI is calculated accordingly to indicate
#'   the relative social vulnerability of the geographic units (communities).
#'
#' @param year The year of interest (available 2012-2021), must match the year
#'   specified in retrieving census data.
#' @param data The census data retrieved by `get_census_data()`.
#' @param xwalk A crosswalk (relationship table) between the Census geographic
#'   level and the customized geographic level of interest. A crosswalk between
#'   US counties and commuting zones `cty_cz_2020_xwalk` is included as an
#'   example, and please set the column names of the crosswalk as follows:
#'  \describe{
#'   \item{GEOID}{Identifiers for the Census geographic level. Must contain values from `GEOID` column in `data`, and be in a compatible data type (character).}
#'   \item{GEOID2}{Identifiers (characters or numeric values) for the customized geographic level that is larger geographic than the Census geographic level. The Census geographic level should be nested in the customized geographic level.}
#'   \item{NAME}{An optional column of the names or description of the customized geographic level.}
#' }
#'
#' @seealso [get_svi()] for SVI calculation from census data at a Census
#'   geographic level, and [find_svi()] for retrieving census data and
#'   calculating SVI for multiple year-state pairs.
#'
#' @returns A tibble of SVI with rows representing the customized geographic
#'   units (with a column name of `GEOID`), and columns indicating variable
#'   names (first two columns containing geographic information). For detailed
#'   description of the variable names (column names), please refer to
#'   [CDC/ATSDR
#'   documentation](https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html).
#'
#' @examplesIf Sys.getenv("CENSUS_API_KEY") != ""
#' # Census API key required
#' cty2020 <- get_census_data(
#'     year = 2020,
#'     geography = "county",
#'     exp = TRUE
#'    )
#'
#' get_svi_x(year = 2020, data = cty2020, xwalk = cty_cz_2020_xwalk)
#'
#' @importFrom rlang .data
#'
#' @export

get_svi_x <- function(year, data, xwalk) {
  if (!all(c("GEOID", "GEOID2") %in% colnames(xwalk))) {
    cli::cli_abort("The crosswalk does not contain `GEOID` (Census) and `GEOID2 (customized) columns.")
  }

  xwalk_check <- xwalk %>% dplyr::count(.data$GEOID)
  if (!all(xwalk_check$n <= 1)) {
    cli::cli_abort("`GEOID`(Census) level is not completely nested in `GEOID2`(customized) level.")
  }

  filename <- paste0("variable_cal_exp_", year)

  var_cal_table <- get(filename)

  ## set up theme 0 vector
  var_0 <- var_cal_table %>%
    dplyr::filter(.data$theme == 0)

  var_0_name <- var_0[[1]]
  var_0_expr <- var_0[[3]]
  names(var_0_expr) <- var_0_name

  ## set up E_ vector
  E_var <-
    var_cal_table %>%
    dplyr::filter(.data$theme %in% c(1:4),
      stringr::str_detect(.[[1]], "E_"))

  E_var_name <- E_var[[1]]
  E_var_expr <- E_var[[3]]
  names(E_var_expr) <- E_var_name

  ## set up EP_ vector
  EP_var <-
    var_cal_table %>%
    dplyr::filter(.data$theme %in% c(1:4),
      stringr::str_detect(.[[1]], "EP_"))

  EP_var_name <- EP_var[[1]]
  EP_var_expr <- EP_var[[3]]
  names(EP_var_expr) <- EP_var_name


  if ("geometry" %in% colnames(data)) {
    data_tmp <- data %>%
      sf::st_drop_geometry()
  } else {
    data_tmp <- data
  }

  ## iterate with E_ vector
  svi0 <-
    purrr::map2_dfc(var_0_name, var_0_expr, function(var_0_name, var_0_expr){
      data_tmp %>%
        dplyr::transmute(
          {{var_0_name}} := eval(str2lang({{var_0_expr}}))
        )
    }) %>%
    dplyr::bind_cols(data_tmp, .)

  svi_e <-
    purrr::map2_dfc(E_var_name, E_var_expr, function(E_var_name, E_var_expr){
      svi0 %>%
        dplyr::transmute(
          {{E_var_name}} := eval(str2lang({{E_var_expr}}))
        )
    }) %>%
    dplyr::bind_cols(svi0, .)
  #important: keep all retrieved census var (in their original name)


  if ("NAME" %in% colnames(xwalk)) {
    data_custom <- svi_e %>%
      dplyr::select(-"NAME") %>%
      dplyr::left_join(xwalk, by = "GEOID") %>%
      dplyr::group_by(.data$GEOID2) %>%
      dplyr::summarise(dplyr::across(-c("GEOID", "NAME"), ~ sum(.x), .names = "{.col}")) %>%
      dplyr::left_join(xwalk %>%
          dplyr::select("GEOID2", "NAME") %>%
          dplyr::distinct(),
        by = "GEOID2") %>%
      dplyr::rename(GEOID = "GEOID2")
  } else {
    data_custom <- svi_e %>%
      dplyr::select(-"NAME") %>%
      dplyr::left_join(xwalk, by = "GEOID") %>%
      dplyr::group_by(.data$GEOID2) %>%
      dplyr::summarise(dplyr::across(-"GEOID", ~ sum(.x), .names = "{.col}")) %>%
      dplyr::rename(GEOID = "GEOID2") %>%
      dplyr::mutate(NAME = "")
  }

  svi_e_ep <-
    purrr::map2(EP_var_name, EP_var_expr, function(EP_var_name, EP_var_expr){
      data_custom %>%
        dplyr::transmute(
          {{EP_var_name}} := eval(str2lang({{EP_var_expr}}))
        )
    }) %>%
    dplyr::bind_cols(data_custom, .) %>%
    #keep the new columns, GEOID, NAME
    dplyr::select("GEOID", "NAME", tidyselect::all_of(var_0_name), tidyselect::all_of(E_var_name), tidyselect::all_of(EP_var_name)) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(EP_var_name), ~ round(.x, 1)),
      E_AGE65 = dplyr::case_when(year >= 2017 ~ E_AGE65,
        TRUE ~ round(E_AGE65, 0)))


  cli::cli_alert_success("Finished aggregating census variables by customized boundaries.")

  # EPL_ --------------------------------------------------------------------

  svi_epl <-
    svi_e_ep %>%
    dplyr::filter(.data$E_TOTPOP > 0) %>%  #added according to documentation (removed from ranking, but kept in table)
    dplyr::select("GEOID", "NAME", tidyselect::all_of(EP_var_name)) %>%   #tidyselect, column or external vector
    tidyr::pivot_longer(!c("GEOID", "NAME"),   #all but GEOID and NAME - no need to know total columns
      names_to = "svi_var",
      values_to = "value") %>%
    tidyr::drop_na("value") %>%  # in case there's *some* variables missing in some tracts
    dplyr::group_by(.data$svi_var) %>%
    dplyr::mutate(rank =  rank(.data$value, ties.method = "min")) %>%
    #check out count() "wt" arg, if NULL, count rows
    dplyr::add_count(.data$svi_var) %>%
    dplyr::mutate(EPL_var = dplyr::case_when(
      year >= 2019 ~(rank - 1) / (n - 1),
      .data$svi_var == "EP_PCI"~ 1 - ((rank - 1) / (n - 1)),
      TRUE ~ (rank - 1) / (n - 1)),
      EPL_var = round(EPL_var, 4)
    )  %>%
    dplyr::ungroup()


  # SPL_ and RPL_ for each theme --------------------------------------------

  xwalk_theme_var <- EP_var %>%
    dplyr::select(-3) %>%
    dplyr::rename(svi_var = 1)


  svi_spl_rpl <-
    svi_epl %>%
    #SPL_each theme
    dplyr::left_join(xwalk_theme_var, by = "svi_var") %>%
    dplyr::group_by(.data$theme, .data$GEOID, .data$NAME) %>%
    dplyr::summarise(SPL_theme = sum(EPL_var),
      .groups = "drop") %>%
    dplyr::ungroup() %>%
    #RPL_
    dplyr::group_by(.data$theme) %>%
    dplyr::mutate(rank_theme = rank(.data$SPL_theme, ties.method = "min")) %>%
    dplyr::add_count(.data$theme) %>%  #rows per group, count the group_by param
    dplyr::mutate(RPL_theme = (.data$rank_theme-1)/(.data$n-1),
      RPL_theme = round(.data$RPL_theme, 4)) %>%
    dplyr::ungroup()


  # SPL_ and RPL_ for all themes --------------------------------------------

  svi_spls_rpls <-
    svi_spl_rpl %>%
    dplyr::group_by(.data$GEOID, .data$NAME) %>%
    dplyr::summarise(SPL_themes = sum(SPL_theme),
      .groups = "drop") %>%
    dplyr::add_count() %>%
    dplyr::mutate(rank_themes = rank(.data$SPL_themes, ties.method = "min"),
      RPL_themes = (.data$rank_themes-1)/(.data$n-1),
      RPL_themes = round(.data$RPL_themes, 4)) %>%
    dplyr::ungroup()


  # merge all variabels to svi ----------------------------------------------

  EPL_var <-
    svi_epl %>%
    dplyr::mutate(EPL_var_name = paste0("EPL_", stringr::str_remove(.data$svi_var, "EP_")),
      .before = EPL_var) %>%
    dplyr::select(-c("svi_var", "value", "rank", "n")) %>%
    tidyr::pivot_wider(names_from = "EPL_var_name",
      values_from = "EPL_var")

  SPL_theme <- svi_spl_rpl %>%
    dplyr::select(-c("RPL_theme", "rank_theme", "n")) %>%
    tidyr::pivot_wider(names_from = "theme",
      names_prefix = "SPL_theme",
      values_from = "SPL_theme")

  RPL_theme <- svi_spl_rpl %>%
    dplyr::select(-c("SPL_theme", "rank_theme", "n")) %>%
    tidyr::pivot_wider(names_from = "theme",
      names_prefix = "RPL_theme",
      values_from = "RPL_theme")

  SPL_RPL_themes <- svi_spls_rpls %>%
    dplyr::select(-c("n", "rank_themes"))


  if ("geometry" %in% colnames(data)) {
    cli::cli_alert_info("Merging geometries from census data to customized geographic levels.")

    xwalk_geo <- data %>%
      dplyr::select("GEOID", "geometry") %>%
      dplyr::left_join(xwalk, by = "GEOID") %>%
      dplyr::group_by(.data$GEOID2) %>%
      dplyr::summarise(geometry = sf::st_union(.data$geometry))

    cli::cli_alert_success("Finished merging geometries.")

    if ("NAME" %in% colnames(xwalk)) {
      xwalk_geo_name <- xwalk_geo %>%
        dplyr::left_join(xwalk %>%
            dplyr::select("GEOID2", "NAME"), by = "GEOID2") %>%
        dplyr::rename(GEOID = "GEOID2")

      svi_complete_geo <-
        list(xwalk_geo_name,
          svi_e_ep,
          EPL_var,
          SPL_theme,
          RPL_theme,
          SPL_RPL_themes) %>%
        purrr::reduce(dplyr::left_join, by = c("GEOID", "NAME"))

    } else {
      xwalk_geo_name <- xwalk_geo %>%
        dplyr::mutate(NAME = "") %>%
        dplyr::rename(GEOID = "GEOID2")

      svi_complete_geo <-
        list(xwalk_geo_name,
          svi_e_ep,
          EPL_var,
          SPL_theme,
          RPL_theme,
          SPL_RPL_themes) %>%
        purrr::reduce(dplyr::left_join, by = c("GEOID", "NAME")) %>%
        dplyr::select(-"NAME")
    }
    cli::cli_alert_success("Finished SVI calculation with geometries.")
    return(svi_complete_geo)


  } else {
    if ("NAME" %in% colnames(xwalk)) {
      svi_complete <-
        list(svi_e_ep, EPL_var, SPL_theme, RPL_theme, SPL_RPL_themes) %>%
        purrr::reduce(dplyr::left_join, by = c("GEOID", "NAME"))

    } else {
      svi_complete <-
        list(svi_e_ep, EPL_var, SPL_theme, RPL_theme, SPL_RPL_themes) %>%
        purrr::reduce(dplyr::left_join, by = c("GEOID", "NAME")) %>%
        dplyr::select(-"NAME")
    }

    cli::cli_alert_success("Finished SVI calculation.")
    return(svi_complete)

  }

}
