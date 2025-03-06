test_that("impute_med_inc correctly imputes values for all inadequate estimates", {
  bg_df <- data.frame(
    tract_id = c("001", "002", "003", "004"),
    median_household_income = c(NA, 100, NA, 100)
  )
  tracts_df <- data.frame(
    tract_id = c("001", "002", "003", "004"),
    median_household_income = c(9999, 200, 8888, 400)
  )
  expected_df <- data.frame(
    tract_id = c("001", "002", "003", "004"),
    median_household_income = c(9999, 100, 8888, 100)
  )
  expect_equal(.impute_income(bg_df, tracts_df), expected_df)
})
