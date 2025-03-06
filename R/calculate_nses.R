.calculate_nses_index <- function(df) {
    df <- df |>
        dplyr::mutate(nses = 50 +
            (-0.07 * percent_crowded) +
            (0.08 * standardized_median_housing_value) +
            (-0.10 * percent_poverty) +
            (0.11 * standardized_median_income) +
            (0.10 * percent_high_education) +
            (-0.11 * percent_low_education) +
            (-0.08 * percent_unemployed)) |>
        dplyr::mutate(nses_quartile = dplyr::ntile(nses, 4))
}
