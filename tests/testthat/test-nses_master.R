test_that("Master get_nses function correctly calculates NSES", {
  tracts_df <- data.frame(
    state = c("01", "02", "08", "04", "06", "72"),
    tract_id = c(1, 2, 3, 4, 6, 7),
    bg_id = c(1, 2, 3, 4, 6, 7),
    geo_id = c(1, 2, 3, 4, 6, 7),
    total_total_population = c(100, 200, 300, 400, 600, 700),
    no_schooling_completed = c(10, 10, 10, 10, 10, 10),
    nursery_school = c(10, 10, 10, 10, 10, 10),
    kindergarten = c(10, 10, 10, 10, 10, 10),
    x1st_grade = c(10, 10, 10, 10, 10, 10),
    x2nd_grade = c(10, 10, 10, 10, 10, 10),
    x3rd_grade = c(10, 10, 10, 10, 10, 10),
    x4th_grade = c(10, 10, 10, 10, 10, 10),
    x5th_grade = c(10, 10, 10, 10, 10, 10),
    x6th_grade = c(10, 10, 10, 10, 10, 10),
    x7th_grade = c(10, 10, 10, 10, 10, 10),
    x8th_grade = c(10, 10, 10, 10, 10, 10),
    x9th_grade = c(10, 10, 10, 10, 10, 10),
    x10th_grade = c(10, 10, 10, 10, 10, 10),
    x11th_grade = c(10, 10, 10, 10, 10, 10),
    x12th_grade_no_diploma = c(10, 10, 10, 10, 10, 10),
    bachelors_degree = c(10, 20, 30, 40, 60, 70),
    masters_degree = c(10, 20, 30, 40, 60, 70),
    professional_school_degree = c(10, 20, 30, 40, 60, 70),
    doctorate_degree = c(10, 20, 30, 40, 60, 70),
    total_educational_attainment_for_the_population_25_years_and_over = c(200, 250, 300, 350, 450, 500),
    median_household_income_in_the_past_12_months_2022 = c(100000, 200000, 300000, 400000, -666666666, 700000),
    income_in_the_past_12_months_below_poverty_level = c(25, 20, 17, 15, 5, 2),
    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement = c(100, 100, 100, 100, 100, 100),
    in_labor_force_civilian_labor_force_unemployed = c(25, 20, 17, 15, 5, 2),
    in_labor_force = c(100, 100, 100, 100, 100, 100),
    median_value_dollars = c(100000, 200000, 300000, 400000, -666666666, 700000),
    owner_occupied_1_01_to_1_50_occupants_per_room = c(10, 9, 8, 7, 5, 4),
    owner_occupied_1_51_to_2_00_occupants_per_room = c(10, 9, 8, 7, 5, 4),
    owner_occupied_2_01_or_more_occupants_per_room = c(10, 9, 8, 7, 5, 4),
    renter_occupied_1_01_to_1_50_occupants_per_room = c(10, 9, 8, 7, 5, 4),
    renter_occupied_1_51_to_2_00_occupants_per_room = c(10, 9, 8, 7, 5, 4),
    renter_occupied_2_01_or_more_occupants_per_room = c(10, 9, 8, 7, 5, 4),
    total_tenure_by_occupants_per_room = c(100, 100, 100, 100, 100, 100)
  )

  transformed_tracts_df <- tracts_df |>
    dplyr::filter(state != "72") |>
    dplyr::rename(
      median_household_income = median_household_income_in_the_past_12_months_2022,
      median_housing_value = median_value_dollars
    ) |>
    dplyr::mutate(
      percent_low_education = c((150 / 200) * 100, (150 / 250) * 100, (150 / 300) * 100, (150 / 350) * 100, (150 / 450) * 100),
      percent_high_education = c((40 / 200) * 100, (80 / 250) * 100, (120 / 300) * 100, (160 / 350) * 100, (240 / 450) * 100),
      median_household_income = c(100000, 200000, 300000, 400000, NA),
      standardized_median_income = c(
        stats::pnorm(10000, mean = mean(median_household_income, na.rm = TRUE), sd = sd(median_household_income, na.rm = TRUE)),
        stats::pnorm(20000, mean = mean(median_household_income, na.rm = TRUE), sd = sd(median_household_income, na.rm = TRUE)),
        stats::pnorm(30000, mean = mean(median_household_income, na.rm = TRUE), sd = sd(median_household_income, na.rm = TRUE)),
        stats::pnorm(40000, mean = mean(median_household_income, na.rm = TRUE), sd = sd(median_household_income, na.rm = TRUE)),
        NA
      ),
      percent_poverty = c(25, 20, 17, 15, 5),
      percent_unemployed = c(25, 20, 17, 15, 5),
      median_housing_value = c(100000, 200000, 300000, 400000, NA),
      standardized_median_housing_value = c(
        stats::pnorm(100000, mean = mean(median_housing_value, na.rm = TRUE), sd = sd(median_housing_value, na.rm = TRUE)),
        stats::pnorm(200000, mean = mean(median_housing_value, na.rm = TRUE), sd = sd(median_housing_value, na.rm = TRUE)),
        stats::pnorm(300000, mean = mean(median_housing_value, na.rm = TRUE), sd = sd(median_housing_value, na.rm = TRUE)),
        stats::pnorm(400000, mean = mean(median_housing_value, na.rm = TRUE), sd = sd(median_housing_value, na.rm = TRUE)),
        NA
      ),
      percent_crowded = c(60 / 100, 54 / 100, 48 / 100, 42 / 100, 30 / 100),
      nses = c(
        (50 +
          (-0.07 * 60) +
          (0.08 * stats::pnorm(
            100000,
            mean = mean(median_housing_value, na.rm = TRUE),
            sd = sd(median_housing_value, na.rm = TRUE)
          )) +
          (-0.10 * 25) +
          (0.11 * stats::pnorm(
            10000,
            mean = mean(median_household_income, na.rm = TRUE),
            sd = sd(median_household_income, na.rm = TRUE)
          )) +
          (0.10 * 40 / 200 * 100) +
          (-0.11 * 150 / 200 * 100) +
          (-0.08 * 25)
        ),
        (50 +
          (-0.07 * 54) +
          (0.08 * stats::pnorm(
            200000,
            mean = mean(median_housing_value, na.rm = TRUE),
            sd = sd(median_housing_value, na.rm = TRUE)
          )) +
          (-0.10 * 20) +
          (0.11 * stats::pnorm(
            20000,
            mean = mean(median_household_income, na.rm = TRUE),
            sd = sd(median_household_income, na.rm = TRUE)
          )) +
          (0.10 * 80 / 250 * 100) +
          (-0.11 * 150 / 250 * 100) +
          (-0.08 * 20)
        ),
        (50 +
          (-0.07 * 48) +
          (0.08 * stats::pnorm(
            300000,
            mean = mean(median_housing_value, na.rm = TRUE),
            sd = sd(median_housing_value, na.rm = TRUE)
          )) +
          (-0.10 * 17) +
          (0.11 * stats::pnorm(
            30000,
            mean = mean(median_household_income, na.rm = TRUE),
            sd = sd(median_household_income, na.rm = TRUE)
          )) +
          (0.10 * 120 / 300 * 100) +
          (-0.11 * 150 / 300 * 100) +
          (-0.08 * 17)
        ),
        (50 +
          (-0.07 * 42) +
          (0.08 * stats::pnorm(
            400000,
            mean = mean(median_housing_value, na.rm = TRUE),
            sd = sd(median_housing_value, na.rm = TRUE)
          )) +
          (-0.10 * 15) +
          (0.11 * stats::pnorm(
            40000,
            mean = mean(median_household_income, na.rm = TRUE),
            sd = sd(median_household_income, na.rm = TRUE)
          )) +
          (0.10 * 160 / 400 * 100) +
          (-0.11 * 200 / 300 * 100) +
          (-0.08 * 15)
        ),
        NA
      ),
      nses_quartile = c(1, 2, 3, 4, NA)
    )

  bg_df <- data.frame(
    state = c("01", "02", "08", "04", "06", "72"),
    tract_id = c(1, 2, 3, 4, 6, 7),
    bg_id = c(1, 2, 3, 4, 6, 7),
    geo_id = c(1, 2, 3, 4, 6, 7),
    total_total_population = c(100, 200, 300, 400, 600, 700),
    no_schooling_completed = c(0, 10, 10, 10, 10, 10),
    nursery_school = c(0, 10, 10, 10, 10, 10),
    kindergarten = c(0, 10, 10, 10, 10, 10),
    x1st_grade = c(0, 10, 10, 10, 10, 10),
    x2nd_grade = c(0, 10, 10, 10, 10, 10),
    x3rd_grade = c(0, 10, 10, 10, 10, 10),
    x4th_grade = c(0, 10, 10, 10, 10, 10),
    x5th_grade = c(0, 10, 10, 10, 10, 10),
    x6th_grade = c(0, 10, 10, 10, 10, 10),
    x7th_grade = c(0, 10, 10, 10, 10, 10),
    x8th_grade = c(0, 10, 10, 10, 10, 10),
    x9th_grade = c(0, 10, 10, 10, 10, 10),
    x10th_grade = c(0, 10, 10, 10, 10, 10),
    x11th_grade = c(0, 10, 10, 10, 10, 10),
    x12th_grade_no_diploma = c(0, 10, 10, 10, 10, 10),
    bachelors_degree = c(0, 20, 30, 40, 60, 70),
    masters_degree = c(0, 20, 30, 40, 60, 70),
    professional_school_degree = c(0, 20, 30, 40, 60, 70),
    doctorate_degree = c(0, 20, 30, 40, 60, 70),
    total_educational_attainment_for_the_population_25_years_and_over = c(0, 250, 300, 350, 450, 500),
    median_household_income_in_the_past_12_months_2022 = c(100000, 200000, -666666666, 400000, -666666666, 700000),
    income_in_the_past_12_months_below_poverty_level = c(25, 0, 17, 15, 5, 2),
    total_poverty_status_of_individuals_in_the_past_12_months_by_living_arrangement = c(100, 0, 100, 100, 100, 100),
    in_labor_force_civilian_labor_force_unemployed = c(25, 20, 0, 15, 5, 2),
    in_labor_force = c(100, 100, 0, 100, 100, 100),
    median_value_dollars = c(100000, 200000, -666666666, 400000, -666666666, 700000),
    owner_occupied_1_01_to_1_50_occupants_per_room = c(0, 9, 8, 7, 5, 4),
    owner_occupied_1_51_to_2_00_occupants_per_room = c(0, 9, 8, 7, 5, 4),
    owner_occupied_2_01_or_more_occupants_per_room = c(0, 9, 8, 7, 5, 4),
    renter_occupied_1_01_to_1_50_occupants_per_room = c(0, 9, 8, 7, 5, 4),
    renter_occupied_1_51_to_2_00_occupants_per_room = c(0, 9, 8, 7, 5, 4),
    renter_occupied_2_01_or_more_occupants_per_room = c(0, 9, 8, 7, 5, 4),
    total_tenure_by_occupants_per_room = c(0, 100, 100, 100, 100, 100)
  )

  transformed_bg_df <- bg_df |>
    dplyr::filter(state != "72") |>
    dplyr::rename(
      median_household_income = median_household_income_in_the_past_12_months_2022,
      median_housing_value = median_value_dollars
    ) |>
    dplyr::mutate(
      percent_low_education = c((150 / 200) * 100, (150 / 250) * 100, (150 / 300) * 100, (150 / 350) * 100, (150 / 450) * 100),
      percent_high_education = c((40 / 200) * 100, (80 / 250) * 100, (120 / 300) * 100, (160 / 350) * 100, (240 / 450) * 100),
      median_household_income = c(100000, 200000, 300000, 400000, NA),
      standardized_median_income = c(
        stats::pnorm(100000, mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE), sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)) * 100,
        stats::pnorm(200000, mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE), sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)) * 100,
        stats::pnorm(300000, mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE), sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)) * 100,
        stats::pnorm(400000, mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE), sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)) * 100,
        NA
      ),
      percent_poverty = c(25, 20, 17, 15, 5),
      percent_unemployed = c(25, 20, 17, 15, 5),
      median_housing_value = c(100000, 200000, 300000, 400000, NA),
      standardized_median_housing_value = c(
        stats::pnorm(100000, mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE), sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)) * 100,
        stats::pnorm(200000, mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE), sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)) * 100,
        stats::pnorm(300000, mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE), sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)) * 100,
        stats::pnorm(400000, mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE), sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)) * 100,
        NA
      ),
      percent_crowded = c(60, 54, 48, 42, 30),
      nses = c(
        (50 +
          (-0.07 * 60) +
          (0.08 * stats::pnorm(
            100000,
            mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE),
            sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)
          ) * 100) +
          (-0.10 * 25) +
          (0.11 * stats::pnorm(
            100000,
            mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE),
            sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)
          ) * 100) +
          (0.10 * 40 / 200 * 100) +
          (-0.11 * 150 / 200 * 100) +
          (-0.08 * 25)
        ),
        (50 +
          (-0.07 * 54) +
          (0.08 * stats::pnorm(
            200000,
            mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE),
            sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)
          ) * 100) +
          (-0.10 * 20) +
          (0.11 * stats::pnorm(
            200000,
            mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE),
            sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)
          ) * 100) +
          (0.10 * 80 / 250 * 100) +
          (-0.11 * 150 / 250 * 100) +
          (-0.08 * 20)
        ),
        (50 +
          (-0.07 * 48) +
          (0.08 * stats::pnorm(
            300000,
            mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE),
            sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)
          ) * 100) +
          (-0.10 * 17) +
          (0.11 * stats::pnorm(
            300000,
            mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE),
            sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)
          ) * 100) +
          (0.10 * 120 / 300 * 100) +
          (-0.11 * 150 / 300 * 100) +
          (-0.08 * 17)
        ),
        (50 +
          (-0.07 * 42) +
          (0.08 * stats::pnorm(
            400000,
            mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE),
            sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)
          ) * 100) +
          (-0.10 * 15) +
          (0.11 * stats::pnorm(
            400000,
            mean = mean(c(100000, 200000, 300000, 400000), na.rm = TRUE),
            sd = sd(c(100000, 200000, 300000, 400000), na.rm = TRUE)
          ) * 100) +
          (0.10 * 160 / 350 * 100) +
          (-0.11 * 150 / 350 * 100) +
          (-0.08 * 15)
        ),
        NA
      ),
      nses_quartile = c(1, 2, 3, 4, NA)
    ) |>
    dplyr::select(
      dplyr::any_of(c("state", "tract_id", "bg_id", "geo_id", "total_total_population")),
      dplyr::contains(c("median", "percent", "nses"))
    )
  expect_equal(get_nses(bg_df, tracts_df, c("state", "tract_id", "bg_id", "geo_id", "total_total_population")), transformed_bg_df)
})
