get_svi <- function(year, data){


  # E_&EP_

  var_cal_table <- load(paste0("data/variable_e_ep_calculation_", year, ".rda")) %>%
    get()

  ## set up theme 0 vector, because sometimes other E_var calculation refer to them
  var_0 <- var_cal_table %>%
    dplyr::filter(theme == 0)

  var_0_name <- var_0[[1]]
  var_0_expr <- var_0[[3]]
  names(var_0_expr) <- var_0_name

  ## set up E_ vector
  E_var <-
    var_cal_table %>%
    dplyr::filter(theme%in%c(1:4),
      stringr::str_detect(.[[1]], "E_"))

  E_var_name <- E_var[[1]]
  E_var_expr <- E_var[[3]]
  names(E_var_expr) <- E_var_name

  ## set up EP_ vector
  EP_var <-
    var_cal_table %>%
    dplyr::filter(theme%in%c(1:4),
      stringr::str_detect(.[[1]], "EP_"))

  EP_var_name <- EP_var[[1]]
  EP_var_expr <- EP_var[[3]]
  names(EP_var_expr) <- EP_var_name

  ## iterate with E_ vector and THEN EP_ vector
  svi0 <-
    purrr::map2_dfc(var_0_name, var_0_expr, function(var_0_name, var_0_expr){
      data %>%
        dplyr::transmute(
          !!all_of(var_0_name) := eval(str2lang(var_0_expr))
        )
    }) %>%
    dplyr::bind_cols(data, .)

  svi_e <-
    purrr::map2_dfc(E_var_name, E_var_expr, function(E_var_name, E_var_expr){
      svi0 %>%
        dplyr::transmute(
          !!all_of(E_var_name) := eval(str2lang(E_var_expr))
        )
    }) %>%
    dplyr::bind_cols(svi0, .)


  svi_e_ep <-
    purrr::map2(EP_var_name, EP_var_expr, function(EP_var_name, EP_var_expr){
      svi_e %>%
        dplyr::transmute(
          !!all_of(EP_var_name) := eval(str2lang(EP_var_expr))
        )
    }) %>%
    dplyr::bind_cols(svi_e, .) %>%
    #keep the new columns, GEOID, NAME
    dplyr::select(GEOID, NAME, all_of(var_0_name), all_of(E_var_name), all_of(EP_var_name)) %>%
    dplyr::mutate(across(all_of(EP_var_name), ~ round(.x, 1)),
      E_AGE65 = case_when(year >= 2017 ~ E_AGE65,
        TRUE ~ round(E_AGE65, 0)))


  # EPL_ --------------------------------------------------------------------

  svi_epl <-
    svi_e_ep %>%
    dplyr::filter(E_TOTPOP > 0) %>%  #added according to documentation (removed from ranking, but kept in table)
    dplyr::select(GEOID, NAME, all_of(EP_var_name)) %>%   #tidyselect, column or external vector
    tidyr::pivot_longer(!c(GEOID, NAME),   #all but GEOID and NAME - no need to know total columns
      names_to = "svi_var",
      values_to = "value") %>%
    tidyr::drop_na(value) %>%  # in case there's *some* variables missing in some tracts
    dplyr::group_by(svi_var) %>%
    dplyr::mutate(rank =  rank(value, ties.method = "min")) %>%
    #check out count() "wt" arg, if NULL, count rows
    dplyr::add_count(svi_var) %>%
    dplyr::mutate(EPL_var = case_when(
      year >= 2019 ~(rank - 1) / (n - 1),
      svi_var == "EP_PCI"~ 1 - ((rank - 1) / (n - 1)),
      TRUE ~ (rank - 1) / (n - 1)),
      EPL_var = round(EPL_var, 4)
    )  %>%
    dplyr::ungroup()


  #y <- svi_epl(2014, eep_data = x)

  # SPL_ and RPL_ for each theme --------------------------------------------

  xwalk_theme_var <- EP_var %>%
    dplyr::select(-3) %>%
    dplyr::rename(svi_var = 1)


  svi_spl_rpl <-
    svi_epl %>%
    #SPL_each theme
    dplyr::left_join(xwalk_theme_var, by = "svi_var") %>%
    dplyr::group_by(theme, GEOID, NAME) %>%  #GEOID and NAME just there to keep the column
    dplyr::summarise(SPL_theme = sum(EPL_var),
      .groups = "drop") %>%
    dplyr::ungroup() %>%
    #RPL_
    dplyr::group_by(theme) %>%
    dplyr::mutate(rank_theme = rank(SPL_theme, ties.method = "min")) %>%
    dplyr::add_count(theme) %>%  #rows per group, count the group_by param
    dplyr::mutate(RPL_theme = (rank_theme-1)/(n-1),
      RPL_theme = round(RPL_theme, 4)) %>%
    dplyr::ungroup()


  #z <- spl_rpl_tm(2014, epl_data = y)

  # SPL_ and RPL_ for all themes --------------------------------------------

  svi_spls_rpls <-
    svi_spl_rpl %>%
    dplyr::group_by(GEOID, NAME) %>%
    dplyr::summarise(SPL_themes = sum(SPL_theme),
      .groups = "drop") %>%
    dplyr::add_count() %>%
    dplyr::mutate(rank_themes = rank(SPL_themes, ties.method = "min"),
      RPL_themes = (rank_themes-1)/(n-1),
      RPL_themes = round(RPL_themes, 4)) %>%
    dplyr::ungroup()


  # merge all variabels to svi ----------------------------------------------

  EPL_var <-
    svi_epl %>%
    dplyr::mutate(EPL_var_name = paste0("EPL_", str_remove(svi_var, "EP_")),
      .before = EPL_var) %>%
    dplyr::select(-c(svi_var, value, rank, n)) %>%
    tidyr::pivot_wider(names_from = EPL_var_name,
      values_from = EPL_var)

  SPL_theme <- svi_spl_rpl %>%
    dplyr::select(-c(RPL_theme, rank_theme, n)) %>%
    tidyr::pivot_wider(names_from = theme,
      names_prefix = "SPL_theme",
      values_from = SPL_theme)

  RPL_theme <- svi_spl_rpl %>%
    dplyr::select(-c(SPL_theme, rank_theme, n)) %>%
    tidyr::pivot_wider(names_from = theme,
      names_prefix = "RPL_theme",
      values_from = RPL_theme)

  SPL_RPL_themes <- svi_spls_rpls %>%
    dplyr::select(-c(n, rank_themes))

  svi_complete <- list(svi_e_ep, EPL_var, SPL_theme, RPL_theme, SPL_RPL_themes) %>%
    purrr::reduce(left_join, by = c("GEOID", "NAME"))

  return(svi_complete)
}
