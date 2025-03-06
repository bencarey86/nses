.select_columns <- function(df, columns) {
    df <- df |>
        dplyr::select(
            dplyr::any_of(columns),
            dplyr::contains(c("median", "percent", "nses"))
        )
}
