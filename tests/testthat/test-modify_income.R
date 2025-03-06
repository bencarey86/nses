test_that("Standardized incomes are computed correctly", {
  median_income <- sample(30000:200000, 30)
  standardized_income <- pnorm(
    median_income,
    mean(median_income, na.rm = TRUE),
    sd(median_income, na.rm = TRUE)
  ) * 100
  df <- data.frame(
    median_household_income =
      median_income
  )
  expected_df <- data.frame(
    median_household_income =
      median_income,
    standardized_median_income = standardized_income
  )
  expect_equal(.compute_standardized_income(df), expected_df)
})
