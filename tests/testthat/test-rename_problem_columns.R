test_that("Problem columns are renamed correctly", {
  df <- data.frame(
    median_value_dollars = c(100000, 200000, 300000, 400000),
    median_household_income_in_the_past_12_months_2022 = c(10000, 20000, 30000, 40000)
  )
  expected_df <- data.frame(
    median_housing_value = c(100000, 200000, 300000, 400000),
    median_household_income = c(10000, 20000, 30000, 40000)
  )
  expect_equal(.rename_problem_columns(df), expected_df)
})
