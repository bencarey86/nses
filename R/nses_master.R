#' Calculate NSES index and quartiles for all block groups in all 50 states and Washington, D.C.
#'
#' This function calculates the NSES index and quartiles for all block groups in all 50 states and Washington, D.C.
#' It takes in two data frames of US Census Data: one with block group data and one with tract data.
#' The function imputes missing values for block groups with values from the corresponding census tract.
#'
#' @param bg_df A data frame of US Census block group data
#' @param tracts_df A data frame of US Census census tract data
#' @param columns A character vector of column names to include in the output data frame, along with NSES columns
#' @return A data frame of US Census block group data with NSES index and quartiles
#' @export
get_nses <- function(bg_df, tracts_df, columns = c("state", "tract_id", "bg_id", "geo_id", "total_total_population")) {
    tracts_df <- tracts_df |>
        .filter_states() |>
        .rename_problem_columns() |>
        .make_inadequate_estimate_na() |>
        .compute_nses_variables()
    bg_df |>
        .filter_states() |>
        .rename_problem_columns() |>
        .make_inadequate_estimate_na() |>
        .impute_inadequate_values(tracts_df = tracts_df) |>
        .compute_nses_variables() |>
        .impute_universe_pop_0(tracts_df = tracts_df) |>
        .calculate_nses_index() |>
        .select_columns(columns = columns)
}
