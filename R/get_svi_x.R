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
#' cty2020 <- get_census_data(year = 2020, geography = "county")
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

  xwalk_check <- xwalk %>% dplyr::count(GEOID)
  if (!all(xwalk_check$n <= 1)) {
    cli::cli_abort("`GEOID`(Census) level is not completely nested in `GEOID2`(customized) level.")
  }

  ## set up theme 0 vector, because sometimes other E_var calculation refer to them
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




  if ("NAME" %in% colnames(xwalk)) {
    data_custom <- data_tmp %>%
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
    data_custom <- data_tmp %>%
      dplyr::select(-"NAME") %>%
      dplyr::left_join(xwalk, by = "GEOID") %>%
      dplyr::group_by(.data$GEOID2) %>%
      dplyr::summarise(dplyr::across(-"GEOID", ~ sum(.x), .names = "{.col}")) %>%
      dplyr::rename(GEOID = "GEOID2") %>%
      dplyr::mutate(NAME = "")
  }

  cli::cli_alert_success("Finished aggregating census data by customized boundaries.")

  if ("geometry" %in% colnames(data)) {
    cli::cli_alert_info("Merging geometries from census data to customized geographic levels.")

    xwalk_geo <- data %>%
      dplyr::select("GEOID", "geometry") %>%
      dplyr::left_join(xwalk, by = "GEOID") %>%
      dplyr::group_by(.data$GEOID2) %>%
      dplyr::summarise(geometry = sf::st_union(.data$geometry))

    cli::cli_alert_success("Finished merging geometries.")
    cli::cli_alert_info("Calulating SVI at the customized geographic level")

    data2 <- xwalk_geo %>%
      dplyr::rename(GEOID = "GEOID2") %>%
      dplyr::left_join(data_custom, by = "GEOID")
  } else {
    data2 <- data_custom
  }

  svi <- findSVI::get_svi(2020, data2)
  cli::cli_alert_success("Finished SVI calculation.")

  if ("NAME" %in% colnames(xwalk)) {
    return(svi)

  } else {
    svi_no_name <- svi %>%
      dplyr::select(-"NAME")

    return(svi_no_name)
  }

}
