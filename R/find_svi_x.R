#' Retrieve census data and calculate SVI for a customized geographic level
#'
#' @description `find_svi_x()` is like a wrapper for
#'   [findSVI::get_census_data()] and [findSVI::get_svi_x()] that retrieves
#'   census data and produces SVI for a customized geographic level consisted of
#'   a Census geography. The census data is retrieved at the Census geographic
#'   level, and estimate counts are summed across the customized geographic
#'   level to calculate SVI. Note that a Census API key is required for this
#'   function to work, which can be obtained at
#'   <https://api.census.gov/data/key_signup.html> and set up using
#'   [tidycensus::census_api_key()].
#'
#' @param year A year of interest (available 2012-2022).
#' @param geography The Census geography level of interest (e.g."county",
#'   "zcta", "tract").
#' @param state (Optional) Specify the state of interest. Default `state = NULL`
#'   or `state = 'US'` return nation-level data.
#' @param county (Optional) Specify the county of interest, must be combined
#'   with a value supplied to "state".
#' @param key Your Census API key. Obtain one at
#'   <https://api.census.gov/data/key_signup.html>. To set up, use
#'   `tidycensus::census_api_key("YOUR KEY GOES HERE")`, or include it as an
#'   argument.
#' @param geometry Default as `FALSE` for retrieving census data without spatial
#'   information. Set as `TRUE` for retrieving census data with an additional
#'   `geometry` column containing simple feature geometry.
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
#' @seealso [find_svi()] for retrieving census data and calculating SVI for
#'   multiple year-state pairs at a Census geographic level. [get_census_data()]
#'   (with `exp = TRUE`) and [get_svi_x()] for separate functions for data
#'   retrieval and SVI calculation at a customized geographic level.
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
#'
#'   find_svi_x(
#'     year = 2020,
#'     geography = "county",
#'     xwalk = cty_cz_2020_xwalk
#'     )
#'
#' @importFrom rlang .data
#' @export
#'
find_svi_x  <- function(
  year,
  geography,
  state = NULL,
  county = NULL,
  key = NULL,
  geometry = FALSE,
  xwalk)
{
  if (!length(year) == 1) {
    cli::cli_abort(c(
      "x" = "Multiple years are not valid input for `year`.",
      "i" = "Please supply one year between 2012-2022."
    ))
  }


  year_valid <- 2012:2022
  if (!(year %in% year_valid)) {
    cli::cli_abort(c(
      "x" = "{year} is not a valid input for `year`.",
      "i" = "Years available for census data retrieval: 2012-2022."
    ))
  }


  state_valid <- state_valid

  state_valid_chr <- state_valid %>%
    dplyr::select("st_abbr", "st_name") %>%
    unlist(use.names = FALSE)
  state_valid_chr_us <- c("US", state_valid_chr)

  state_valid_dbl <- state_valid %>%
    dplyr::select("fips_code") %>%
    unlist(use.names = FALSE)

  if (inherits(state, "numeric")) {
    if (any(!(state %in% state_valid_dbl))) {
      cli::cli_abort(c(
        "x" = "One or more elements of {state} is not a valid input for `state`.",
        "i" = "Use `state_valid` for a table of valid input (besides 'US'). Note state full names/abbreviations are characters; FIPS codes are numbers."
      ))
    }
  }

  if(inherits(state, "character")) {
    if (any(!(state %in% state_valid_chr_us))) {
      cli::cli_abort(c(
        "x" = "One or more elements of {state} is not a valid input for `state`.",
        "i" = "Use `state_valid` for a table of valid input (besides 'US'). Note state full names/abbreviations are characters; FIPS codes are numbers."
      ))
    }
  }

  census_data <- findSVI::get_census_data(
    year = year,
    geography = geography,
    state = state,
    county = county,
    key = key,
    geometry = geometry,
    exp = TRUE
  )
  cli::cli_alert_success("Finished retrieving census data for {state} {county} {year} at {geography} level")

  results <- findSVI::get_svi_x(
    year = year,
    data = census_data,
    xwalk = xwalk
  )

  return(results)
}
