test_that("Standardized median housing values are calculated correctly", {
  median_housing_value <- sample(10000:999999, 100, replace = TRUE)
  standardized_med_housing_value <- stats::pnorm(
    median_housing_value,
    mean = mean(median_housing_value, na.rm = TRUE),
    sd = stats::sd(median_housing_value, na.rm = TRUE)
  ) * 100
  df <- data.frame(
    median_housing_value = median_housing_value
  )
  expected_df <- data.frame(
    median_housing_value = median_housing_value,
    standardized_median_housing_value = standardized_med_housing_value
  )
  expect_equal(.compute_housing_value(df), expected_df)
})
