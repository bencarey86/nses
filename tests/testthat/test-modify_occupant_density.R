test_that("High occupant density is calculated correctly", {
  df <- data.frame(
    owner_occupied_1_01_to_1_50_occupants_per_room = c(5, 10, 15),
    owner_occupied_1_51_to_2_00_occupants_per_room = c(5, 10, 15),
    owner_occupied_2_01_or_more_occupants_per_room = c(5, 10, 15),
    renter_occupied_1_01_to_1_50_occupants_per_room = c(5, 10, 15),
    renter_occupied_1_51_to_2_00_occupants_per_room = c(5, 10, 15),
    renter_occupied_2_01_or_more_occupants_per_room = c(5, 10, 15),
    total_tenure_by_occupants_per_room = c(100, 100, 100)
  )

  # Expected output
  expected_df <- data.frame(
    owner_occupied_1_01_to_1_50_occupants_per_room = c(5, 10, 15),
    owner_occupied_1_51_to_2_00_occupants_per_room = c(5, 10, 15),
    owner_occupied_2_01_or_more_occupants_per_room = c(5, 10, 15),
    renter_occupied_1_01_to_1_50_occupants_per_room = c(5, 10, 15),
    renter_occupied_1_51_to_2_00_occupants_per_room = c(5, 10, 15),
    renter_occupied_2_01_or_more_occupants_per_room = c(5, 10, 15),
    total_tenure_by_occupants_per_room = c(100, 100, 100),
    percent_crowded = c(30, 60, 90)
  )
  expect_equal(.compute_occupant_density(df), expected_df)
})
