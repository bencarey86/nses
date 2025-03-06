.compute_housing_value <- function(df) {
    df <- df |>
        dplyr::mutate(
            "standardized_median_housing_value" =
                stats::pnorm(
                    median_housing_value,
                    mean = mean(median_housing_value, na.rm = TRUE),
                    sd = stats::sd(median_housing_value, na.rm = TRUE)
                ) * 100
        )
}
