test_that("Columns are selected as desired", {
  df <- data.frame(
    state = c("CA", "CA", "CA"),
    tract_id = c(1, 2, 3),
    bg_id = c(1, 2, 3),
    geo_id = c(1, 2, 3),
    total_total_population = c(100, 200, 300),
    median_income = c(1000, 2000, 3000),
    percent_poverty = c(10, 20, 30),
    nses_index = c(1, 2, 3),
    other_column = c(1, 2, 3)
  )
  expected_df <- data.frame(
    state = c("CA", "CA", "CA"),
    tract_id = c(1, 2, 3),
    bg_id = c(1, 2, 3),
    geo_id = c(1, 2, 3),
    total_total_population = c(100, 200, 300),
    median_income = c(1000, 2000, 3000),
    percent_poverty = c(10, 20, 30),
    nses_index = c(1, 2, 3)
  )
  expect_equal(
    .select_columns(df, c("state", "tract_id", "bg_id", "geo_id", "total_total_population")),
    expected_df
  )
})
