test_that("Poverty status is imputed correctly", {
  bg_df <- data.frame(
    tract_id = c("001", "002", "003"),
    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement = c(0, 100, 150),
    percent_poverty = c(NA, NA, 50)
  )
  tracts_df <- data.frame(
    tract_id = c("001", "002", "003"),
    percent_poverty = c(10, 20, 30)
  )
  expected_df <- data.frame(
    tract_id = c("001", "002", "003"),
    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement = c(0, 100, 150),
    percent_poverty = c(10, NA, 50)
  )
  expect_equal(.impute_poverty(bg_df, tracts_df), expected_df)
})
