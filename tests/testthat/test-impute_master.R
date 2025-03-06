test_that("Inadequate values are correctly imputed by master function", {
  bg_df <- data.frame(
    tract_id = c("001", "002", "003", "004"),
    median_housing_value = c(NA, 50, NA, 100),
    median_household_income = c(NA, 100, NA, 100)
  )
  tracts_df <- data.frame(
    tract_id = c("001", "002", "003", "004"),
    median_housing_value = c(100000, 200000, 300000, 400000),
    median_household_income = c(9999, 200, 8888, 400)
  )
  expected_df <- data.frame(
    tract_id = c("001", "002", "003", "004"),
    median_housing_value = c(100000, 50, 300000, 100),
    median_household_income = c(9999, 100, 8888, 100)
  )
  expect_equal(.impute_inadequate_values(bg_df, tracts_df), expected_df)
})

test_that("Values with universe population 0 are correctly imputed", {
  bg_df <- data.frame(
    tract_id = c("001", "002", "003"),
    total_educational_attainment_for_the_population_25_years_and_over = c(0, 100, 150),
    percent_low_education = c(NA, 10, 20),
    percent_high_education = c(NA, 20, 10),
    total_tenure_by_occupants_per_room = c(0, 100, 150),
    percent_crowded = c(NA, NA, 50),
    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement = c(0, 100, 150),
    percent_poverty = c(NA, NA, 50),
    in_labor_force = c(0, 200, 300),
    percent_unemployed = c(NA, NA, 50)
  )
  tracts_df <- data.frame(
    tract_id = c("001", "002", "003"),
    percent_low_education = c(10, 20, 30),
    percent_high_education = c(20, 40, 60),
    percent_crowded = c(10, 20, 30),
    percent_poverty = c(10, 20, 30),
    percent_unemployed = c(10, 20, 30)
  )
  expected_df <- data.frame(
    tract_id = c("001", "002", "003"),
    total_educational_attainment_for_the_population_25_years_and_over = c(0, 100, 150),
    percent_low_education = c(10, 10, 20),
    percent_high_education = c(20, 20, 10),
    total_tenure_by_occupants_per_room = c(0, 100, 150),
    percent_crowded = c(10, NA, 50),
    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement = c(0, 100, 150),
    percent_poverty = c(10, NA, 50),
    in_labor_force = c(0, 200, 300),
    percent_unemployed = c(10, NA, 50)
  )
  expect_equal(.impute_universe_pop_0(bg_df, tracts_df), expected_df)
})
