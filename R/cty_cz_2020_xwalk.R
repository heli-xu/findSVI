#' Relationship file (crosswalk) between US counties and commuting zones
#'
#' This table contains GEOIDs for US counties and the commuting zones they are
#' nested in. Commuting zones can be used to study regional economy with
#' considerations of urban-rural interconnections across state lines. For
#' details refer to papers by  Fowler Jensen and Rhubart
#' (2016) <doi:10.1007/s11113-016-9386-0> and Fowler
#' (2024) <doi:10.17605/OSF.IO/J256U>.
#'
#' @name cty_cz_2020_xwalk
#' @format ## A data frame with 2 columns and 3222 rows:
#' \describe{
#'   \item{GEOID}{US county FIPS code.}
#'   \item{GEOID2}{Commuting zone ID for the year 2020.}
#'
#' }
#' @source https://sites.psu.edu/psucz/data/
"cty_cz_2020_xwalk"
