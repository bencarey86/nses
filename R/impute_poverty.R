.impute_poverty <- function(bg_df, tracts_df) {
    tracts_df <- tracts_df |>
        dplyr::select("tract_id", "tract_poverty" = "percent_poverty")
    bg_df |>
        dplyr::left_join(tracts_df, by = "tract_id") |>
        dplyr::mutate(
            "percent_poverty" = dplyr::case_when(
                total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement == 0 ~ tract_poverty,
                TRUE ~ percent_poverty
            )
        ) |>
        dplyr::select(-"tract_poverty")
}
