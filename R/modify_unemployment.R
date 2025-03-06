.compute_unemployment <- function(df) {
    df <- df |>
        dplyr::mutate(
            "percent_unemployed" =
                in_labor_force_civilian_labor_force_unemployed / in_labor_force * 100
        )
}
