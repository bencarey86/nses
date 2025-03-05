test_that("Low eduction computed correctly", {
  df <- data.frame(
    no_schooling_completed = c(1, 2, 3),
    nursery_school = c(1, 2, 3),
    kindergarten = c(1, 2, 3),
    x1st_grade = c(1, 2, 3),
    x2nd_grade = c(1, 2, 3),
    x3rd_grade = c(1, 2, 3),
    x4th_grade = c(1, 2, 3),
    x5th_grade = c(1, 2, 3),
    x6th_grade = c(1, 2, 3),
    x7th_grade = c(1, 2, 3),
    x8th_grade = c(1, 2, 3),
    x9th_grade = c(1, 2, 3),
    x10th_grade = c(1, 2, 3),
    x11th_grade = c(1, 2, 3),
    x12th_grade_no_diploma = c(1, 2, 3),
    total_educational_attainment_for_the_population_25_years_and_over = c(30, 60, 90)
  )
  expected_df <- data.frame(
    no_schooling_completed = c(1, 2, 3),
    nursery_school = c(1, 2, 3),
    kindergarten = c(1, 2, 3),
    x1st_grade = c(1, 2, 3),
    x2nd_grade = c(1, 2, 3),
    x3rd_grade = c(1, 2, 3),
    x4th_grade = c(1, 2, 3),
    x5th_grade = c(1, 2, 3),
    x6th_grade = c(1, 2, 3),
    x7th_grade = c(1, 2, 3),
    x8th_grade = c(1, 2, 3),
    x9th_grade = c(1, 2, 3),
    x10th_grade = c(1, 2, 3),
    x11th_grade = c(1, 2, 3),
    x12th_grade_no_diploma = c(1, 2, 3),
    total_educational_attainment_for_the_population_25_years_and_over = c(30, 60, 90),
    low_education = c(15, 30, 45),
    percent_low_education = c(50, 50, 50)
  )
  expect_equal(.compute_low_education(df), expected_df)
})

test_that("High eduction computed correctly", {
  df <- data.frame(
    bachelors_degree = c(1, 2, 3),
    masters_degree = c(1, 2, 3),
    professional_school_degree = c(1, 2, 3),
    doctorate_degree = c(1, 2, 3),
    total_educational_attainment_for_the_population_25_years_and_over = c(40, 40, 48)
  )
  expected_df <- data.frame(
    bachelors_degree = c(1, 2, 3),
    masters_degree = c(1, 2, 3),
    professional_school_degree = c(1, 2, 3),
    doctorate_degree = c(1, 2, 3),
    total_educational_attainment_for_the_population_25_years_and_over = c(40, 40, 48),
    high_education = c(4, 8, 12),
    percent_high_education = c(10, 20, 25)
  )
  expect_equal(.compute_high_education(df), expected_df)
})

test_that("Master function computes both high and low education correctly", {
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
    doctorate_degree = c(5, 10, 15)
  )
  expected_df <- data.frame(
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
    low_education = c(75, 150, 225),
    percent_low_education = c(75, 75, 75),
    high_education = c(20, 40, 60),
    percent_high_education = c(20, 20, 20)
  )
  expect_equal(.compute_education(df), expected_df)
})
