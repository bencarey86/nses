test_that("imput_med_house_val correctly imputes values for all inadequate estimates", {
  bg_df <- data.frame(
    tract_id = c("001", "002", "003", "004"),
    median_housing_value = c(NA, 50, NA, 100)
  )
  tracts_df <- data.frame(
    tract_id = c("001", "002", "003", "004"),
    median_housing_value = c(100000, 200000, 300000, 400000)
  )
  expected_df <- data.frame(
    tract_id = c("001", "002", "003", "004"),
    median_housing_value = c(100000, 50, 300000, 100)
  )
  expect_equal(.impute_housing_value(bg_df, tracts_df), expected_df)
})
