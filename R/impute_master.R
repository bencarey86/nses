.impute_inadequate_values <- function(bg_df, tracts_df) {
    bg_df |>
        .impute_housing_value(tracts_df = tracts_df) |>
        .impute_income(tracts_df = tracts_df)
}

.impute_universe_pop_0 <- function(bg_df, tracts_df) {
    bg_df |>
        .impute_education(tracts_df = tracts_df) |>
        .impute_occupant_density(tracts_df = tracts_df) |>
        .impute_poverty(tracts_df = tracts_df) |>
        .impute_unemployment(tracts_df = tracts_df)
}
