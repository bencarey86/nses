.impute_housing_value <- function(bg_df, tracts_df) {
    tracts_df <- tracts_df |>
        dplyr::select("tract_id", "tract_median_housing_value" = "median_housing_value")

    bg_df <- bg_df |>
        dplyr::left_join(tracts_df, by = "tract_id") |>
        dplyr::mutate(median_housing_value = dplyr::case_when(
            is.na(median_housing_value) ~ tract_median_housing_value,
            TRUE ~ median_housing_value
        )) |>
        dplyr::select(-tract_median_housing_value)
}
