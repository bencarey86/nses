.impute_income <- function(bg_df, tracts_df) {
    tracts_df <- tracts_df |>
        dplyr::select("tract_id", "tract_median_household_income" = "median_household_income")
    bg_df <- bg_df |>
        # Left join tract median income to block group data
        dplyr::left_join(tracts_df, by = "tract_id") |>
        # Impute inadequate estimates for median income in block group data
        dplyr::mutate("median_household_income" = dplyr::case_when(
            is.na(median_household_income) ~ tract_median_household_income,
            TRUE ~ median_household_income
        )) |>
        dplyr::select(-tract_median_household_income)
}
