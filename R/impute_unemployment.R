.impute_unemployment <- function(bg_df, tracts_df) {
    tracts_df <- tracts_df |>
        dplyr::select("tract_id", "tract_unemployed" = "percent_unemployed")
    bg_df |>
        dplyr::left_join(tracts_df, by = "tract_id") |>
        dplyr::mutate(
            "percent_unemployed" = dplyr::case_when(
                in_labor_force == 0 ~ tract_unemployed,
                TRUE ~ percent_unemployed
            )
        ) |>
        dplyr::select(-"tract_unemployed")
}
