.compute_poverty <- function(df) {
    df <- df |>
        dplyr::mutate(
            "percent_poverty" =
                income_in_the_past_12_months_below_poverty_level /
                    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement *
                    100
        )
}
