.filter_states <- function(df) {
    state_codes <- nses_states$code
    df |>
        dplyr::filter(state %in% state_codes)
}
