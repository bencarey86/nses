test_that("Unemployment status is imputed correctly", {
  bg_df <- data.frame(
    tract_id = c("001", "002", "003"),
    in_labor_force = c(0, 200, 300),
    percent_unemployed = c(NA, NA, 50)
  )
  tracts_df <- data.frame(
    tract_id = c("001", "002", "003"),
    percent_unemployed = c(10, 20, 30)
  )
  expected_df <- data.frame(
    tract_id = c("001", "002", "003"),
    in_labor_force = c(0, 200, 300),
    percent_unemployed = c(10, NA, 50)
  )
  expect_equal(.impute_unemployment(bg_df, tracts_df), expected_df)
})
