test_that("Percent poverty is correctly calcuated", {
  df <- data.frame(
    income_in_the_past_12_months_below_poverty_level = c(5, 5, 3),
    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement = c(100, 200, 300)
  )
  expected_df <- data.frame(
    income_in_the_past_12_months_below_poverty_level = c(5, 5, 3),
    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement = c(100, 200, 300),
    percent_poverty = c(5, 2.5, 1)
  )
  testthat::expect_equal(.compute_poverty(df), expected_df)
})
