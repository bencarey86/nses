test_that("NSES index and quartiles are calculated correctly", {
  df <- data.frame(
    percent_crowded = c(15, 10, 5, 30),
    standardized_median_housing_value = c(15.9, 50, 84.1, 3),
    percent_poverty = c(15, 10, 5, 20),
    standardized_median_income = c(15.9, 50, 84.1, 3),
    percent_high_education = c(20, 40, 60, 10),
    percent_low_education = c(60, 40, 20, 70),
    percent_unemployed = c(15, 10, 5, 20)
  )
  expected_df <- df
  expected_df$nses <- c(
    (50 + (-0.07 * 15) + (0.08 * 15.9) + (-0.10 * 15) + (0.11 * 15.9) + (0.10 * 20) +
      (-0.11 * 60) + (-0.08 * 15)),
    (50 + (-0.07 * 10) + (0.08 * 50) + (-0.10 * 10) + (0.11 * 50) + (0.10 * 40) +
      (-0.11 * 40) + (-0.08 * 10)),
    (50 + (-0.07 * 5) + (0.08 * 84.1) + (-0.10 * 5) + (0.11 * 84.1) + (0.10 * 60) +
      (-0.11 * 20) + (-0.08 * 5)),
    (50 + (-0.07 * 30) + (0.08 * 3) + (-0.10 * 20) + (0.11 * 3) + (0.10 * 10) +
      (-0.11 * 70) + (-0.08 * 20))
  )
  expected_df$nses_quartile <- c(2, 3, 4, 1)
  expect_equal(.calculate_nses_index(df), expected_df)
})
