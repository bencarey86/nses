test_that("Unemployment data are modified correctly", {
  df <- data.frame(
    in_labor_force_civilian_labor_force_unemployed = c(5, 10, 15),
    in_labor_force = c(100, 100, 100)
  )
  expected_df <- data.frame(
    in_labor_force_civilian_labor_force_unemployed = c(5, 10, 15),
    in_labor_force = c(100, 100, 100),
    percent_unemployed = c(5, 10, 15)
  )
  expect_equal(.compute_unemployment(df), expected_df)
})
