.compute_nses_variables <- function(df) {
    df <- df |>
        .compute_education() |>
        .compute_standardized_income() |>
        .compute_poverty() |>
        .compute_unemployment() |>
        .compute_housing_value() |>
        .compute_occupant_density()
}
