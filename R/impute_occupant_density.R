.impute_occupant_density <- function(bg_df, tracts_df) {
    tracts_df <- tracts_df |>
        dplyr::select("tract_id", "tract_density" = "percent_crowded")
    bg_df |>
        dplyr::left_join(tracts_df, by = "tract_id") |>
        dplyr::mutate("percent_crowded" = dplyr::case_when(
            total_tenure_by_occupants_per_room == 0 ~ tract_density,
            TRUE ~ percent_crowded
        )) |>
        # Drop tract estimate column
        dplyr::select(-"tract_density")
}
