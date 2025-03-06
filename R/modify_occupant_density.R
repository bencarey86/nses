.compute_occupant_density <- function(df) {
    df <- df |>
        dplyr::mutate(
            "percent_crowded" =
                rowSums(
                    dplyr::across(dplyr::contains(c(
                        "occupied_1_01",
                        "occupied_1_51",
                        "occupied_2_01"
                    )))
                ) /
                    total_tenure_by_occupants_per_room * 100
        )
}
