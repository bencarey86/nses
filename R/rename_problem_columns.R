.rename_problem_columns <- function(df) {
    df <- df |>
        dplyr::rename(
            "median_housing_value" = "median_value_dollars",
            "median_household_income" = dplyr::contains("median_household_income_in_the_past_12_months")
        )
}
