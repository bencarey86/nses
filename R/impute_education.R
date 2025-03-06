#' Impute education data for block groups with universe population 0
#'
#' This function imputes the education data for block groups with universe population 0.
#' It should be performed on the block group data frame after NSES has been calculated.
#'
#' @param bg_df A data frame with block group data
#' @param tracts_df A data frame with tract data
#' @return A data frame with imputed education data
#' @keywords internal
.impute_education <- function(bg_df, tracts_df) {
    tracts_df <- tracts_df |>
        dplyr::select(
            "tract_id",
            "tract_low" = "percent_low_education",
            "tract_high" = "percent_high_education"
        )
    bg_df |>
        dplyr::left_join(tracts_df, by = "tract_id") |>
        dplyr::mutate(
            percent_low_education = dplyr::case_when(
                total_educational_attainment_for_the_population_25_years_and_over == 0 ~ tract_low,
                TRUE ~ percent_low_education
            ),
            percent_high_education = dplyr::case_when(
                total_educational_attainment_for_the_population_25_years_and_over == 0 ~ tract_high,
                TRUE ~ percent_high_education
            )
        ) |>
        dplyr::select(-tract_low, -tract_high)
}
