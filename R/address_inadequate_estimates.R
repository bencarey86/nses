.make_inadequate_estimate_na <- function(df) {
    inadequate_estimate_char <- c(
        -666666666, -999999999, -888888888, -222222222, -333333333, -555555555
    )
    df <- df |>
        dplyr::mutate(
            dplyr::across(
                .cols = dplyr::everything(),
                .fns = ~ ifelse(.x %in% inadequate_estimate_char, NA_real_, .x)
            )
        )
}
