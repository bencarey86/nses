test_that("Occupant density data are correctly imputed", {
  bg_df <- data.frame(
    tract_id = c("001", "002", "003"),
    total_tenure_by_occupants_per_room = c(0, 100, 150),
    percent_crowded = c(NA, NA, 50)
  )
  tracts_df <- data.frame(
    tract_id = c("001", "002", "003"),
    percent_crowded = c(10, 20, 30)
  )
  expected_df <- data.frame(
    tract_id = c("001", "002", "003"),
    total_tenure_by_occupants_per_room = c(0, 100, 150),
    percent_crowded = c(10, NA, 50)
  )
  expect_equal(.impute_occupant_density(bg_df, tracts_df), expected_df)
})
