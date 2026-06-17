##code to prepare 2023-2024 census_variables and variable_e_ep_calculation datasets(.rda)
##crosswalks variables from 2022 to 2023, 2024 equivalents

library(tidycensus) # needs an API key
library(tidyverse)

update_findsvi <- function(
  census_variables,
  variable_calculation,
  from,
  to
) {
  acs_datasets <- c("acs5", "acs5/profile", "acs5/subject")
  census_vars_from <- census_variables |>
    unlist() |>
    unname()

  census_var_def_acs_from <- acs_datasets |>
    map(\(dataset) {
      load_variables(year = from, dataset = dataset)
    }) |>
    bind_rows()
  census_var_def_acs_from_filtered <- census_var_def_acs_from |>
    filter(name %in% str_remove(census_vars_from, "E$"))
  census_var_def_acs_to <- acs_datasets |>
    map(\(dataset) {
      load_variables(year = to, dataset = dataset)
    }) |>
    bind_rows()
  census_var_def_acs_to_filtered <- census_var_def_acs_to |>
    filter(name %in% str_remove(census_vars_from, "E$"))

  unmatched_vars <- census_var_def_acs_from_filtered |>
    anti_join(census_var_def_acs_to_filtered, join_by(label, concept))
  correct_match_vars <- unmatched_vars |>
    inner_join(
      census_var_def_acs_to,
      join_by(label, concept),
      suffix = c("_from", "_to"),
      relationship = "one-to-one"
    )
  if (any(is.na(correct_match_vars[["name_to"]]))) {
    cli::cli_alert("Could not find a match for at least one variable.")
  }
  correction_map <- correct_match_vars |>
    mutate(across(starts_with("name"), \(col) str_c(col, "E"))) |>
    select(starts_with("name")) |>
    deframe()

  census_variables_to <- census_variables |>
    map(\(theme) {
      str_replace_all(theme, correction_map)
    })

  new_calc_col <- paste0("x", to, "_table_field_calculation")
  old_calc_col <- paste0("x", from, "_table_field_calculation")
  new_var_col <- paste0("x", to, "_variable_name")
  old_var_col <- paste0("x", from, "_variable_name")

  variable_calculation_to <- variable_calculation |>
    mutate(
      !!new_calc_col := str_replace_all(
        .data[[old_calc_col]],
        correction_map
      ),
      .keep = "unused"
    ) |>
    rename(!!new_var_col := all_of(old_var_col))

  list(
    census_variables = census_variables_to,
    variable_calculations = variable_calculation_to
  )
}

# 2023
result_2023 <- update_findsvi(findSVI::census_variables_exp_2022, findSVI::variable_cal_exp_2022, from = 2022, to = 2023)
census_variables_exp_2023 <- result_2023$census_variables
variable_cal_exp_2023 <- result_2023$variable_calculations

usethis::use_data(census_variables_exp_2023, overwrite = TRUE)
usethis::use_data(variable_cal_exp_2023, overwrite = TRUE)

result_2023 <- update_findsvi(findSVI::census_variables_2022, findSVI::variable_e_ep_calculation_2022, from = 2022, to = 2023)
census_variables_2023 <- result_2023$census_variables
variable_e_ep_calculation_2023 <- result_2023$variable_calculations

usethis::use_data(census_variables_2023, overwrite = TRUE)
usethis::use_data(variable_e_ep_calculation_2023, overwrite = TRUE)

# 2024
result_2024 <- update_findsvi(findSVI::census_variables_exp_2022, findSVI::variable_cal_exp_2022, from = 2022, to = 2024)
census_variables_exp_2024 <- result_2024$census_variables
variable_cal_exp_2024 <- result_2024$variable_calculations

usethis::use_data(census_variables_exp_2024, overwrite = TRUE)
usethis::use_data(variable_cal_exp_2024, overwrite = TRUE)

result_2023 <- update_findsvi(findSVI::census_variables_2022, findSVI::variable_e_ep_calculation_2022, from = 2022, to = 2024)
census_variables_2024 <- result_2024$census_variables
variable_e_ep_calculation_2024 <- result_2024$variable_calculations

usethis::use_data(census_variables_2024, overwrite = TRUE)
usethis::use_data(variable_e_ep_calculation_2024, overwrite = TRUE)
