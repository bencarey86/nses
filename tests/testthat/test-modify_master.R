test_that("All NSES data modifications are correctly performed", {
  df <- data.frame(
    total_educational_attainment_for_the_population_25_years_and_over = c(100, 200, 300),
    no_schooling_completed = c(5, 10, 15),
    nursery_school = c(5, 10, 15),
    kindergarten = c(5, 10, 15),
    x1st_grade = c(5, 10, 15),
    x2nd_grade = c(5, 10, 15),
    x3rd_grade = c(5, 10, 15),
    x4th_grade = c(5, 10, 15),
    x5th_grade = c(5, 10, 15),
    x6th_grade = c(5, 10, 15),
    x7th_grade = c(5, 10, 15),
    x8th_grade = c(5, 10, 15),
    x9th_grade = c(5, 10, 15),
    x10th_grade = c(5, 10, 15),
    x11th_grade = c(5, 10, 15),
    x12th_grade_no_diploma = c(5, 10, 15),
    bachelors_degree = c(5, 10, 15),
    masters_degree = c(5, 10, 15),
    professional_school_degree = c(5, 10, 15),
    doctorate_degree = c(5, 10, 15),
    median_household_income = c(100, 200, 300),
    income_in_the_past_12_months_below_poverty_level = c(5, 10, 15),
    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement = c(100, 200, 300),
    in_labor_force_civilian_labor_force_unemployed = c(5, 10, 15),
    in_labor_force = c(100, 200, 300),
    median_housing_value = c(100, 200, 300),
    owner_occupied_1_01_to_1_50_occupants_per_room = c(5, 10, 15),
    owner_occupied_1_51_to_2_00_occupants_per_room = c(5, 10, 15),
    owner_occupied_2_01_or_more_occupants_per_room = c(5, 10, 15),
    renter_occupied_1_01_to_1_50_occupants_per_room = c(5, 10, 15),
    renter_occupied_1_51_to_2_00_occupants_per_room = c(5, 10, 15),
    renter_occupied_2_01_or_more_occupants_per_room = c(5, 10, 15),
    total_tenure_by_occupants_per_room = c(100, 100, 100)
  )
  expected_df <- df
  expected_df$low_education <- c(75, 150, 225)
  expected_df$percent_low_education <- c(75, 75, 75)
  expected_df$high_education <- c(20, 40, 60)
  expected_df$percent_high_education <- c(20, 20, 20)
  expected_df$standardized_median_income <- c(15.8655254, 50.00000, 84.1344746)
  expected_df$percent_poverty <- c(5, 5, 5)
  expected_df$percent_unemployed <- c(5, 5, 5)
  expected_df$standardized_median_housing_value <- c(15.8655254, 50.00000, 84.1344746)
  expected_df$percent_crowded <- c(30, 60, 90)
  expect_equal(.compute_nses_variables(df), expected_df)
})
