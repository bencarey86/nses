.compute_standardized_income <- function(df) {
    df <- df |>
        dplyr::mutate(
            "standardized_median_income" =
                stats::pnorm(
                    median_household_income,
                    mean(median_household_income, na.rm = TRUE),
                    stats::sd(median_household_income, na.rm = TRUE)
                ) * 100
        )
}
