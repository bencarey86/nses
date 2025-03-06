test_that("Education data are imputed correctly", {
  bg_df <- data.frame(
    tract_id = c("001", "002", "003"),
    total_educational_attainment_for_the_population_25_years_and_over = c(0, 100, 150),
    percent_low_education = c(NA, 10, 20),
    percent_high_education = c(NA, 20, 10)
  )
  tracts_df <- data.frame(
    tract_id = c("001", "002", "003"),
    percent_low_education = c(10, 20, 30),
    percent_high_education = c(20, 40, 60)
  )
  expected_df <- data.frame(
    tract_id = c("001", "002", "003"),
    total_educational_attainment_for_the_population_25_years_and_over = c(0, 100, 150),
    percent_low_education = c(10, 10, 20),
    percent_high_education = c(20, 20, 10)
  )
  expect_equal(.impute_education(bg_df, tracts_df), expected_df)
})
