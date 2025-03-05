.compute_low_education <- function(df) {
    df <- df |>
        dplyr::mutate(
            "low_education" = no_schooling_completed + nursery_school + kindergarten + x1st_grade +
                x2nd_grade + x3rd_grade + x4th_grade + x5th_grade + x6th_grade + x7th_grade +
                x8th_grade + x9th_grade + x10th_grade + x11th_grade + x12th_grade_no_diploma
        ) |>
        dplyr::mutate(
            "percent_low_education" =
                low_education /
                    total_educational_attainment_for_the_population_25_years_and_over *
                    100
        )
    return(df)
}

.compute_high_education <- function(df) {
    df <- df |>
        dplyr::mutate(
            "high_education" = bachelors_degree + masters_degree + professional_school_degree +
                doctorate_degree
        ) |>
        dplyr::mutate(
            "percent_high_education" =
                high_education /
                    total_educational_attainment_for_the_population_25_years_and_over *
                    100
        )
    return(df)
}

.compute_education <- function(df) {
    df <- df |>
        .compute_low_education() |>
        .compute_high_education()
    return(df)
}
