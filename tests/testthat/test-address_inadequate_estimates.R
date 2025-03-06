test_that("Inadequate estimates are removed from all columns", {
  df <- data.frame(
    a = c(1, 2, 3, 4, 5, 6),
    b = c(-666666666, 2, 3, 4, 5, 6),
    c = c(1, -222222222, 3, 4, 5, 6),
    d = c(1, 2, -333333333, 4, 5, 6),
    e = c(1, 2, 3, -555555555, 5, 6),
    f = c(1, 2, 3, 4, -888888888, 6),
    g = c(1, 2, 3, 4, 5, -999999999)
  )
  expected_df <- data.frame(
    a = c(1, 2, 3, 4, 5, 6),
    b = c(NA, 2, 3, 4, 5, 6),
    c = c(1, NA, 3, 4, 5, 6),
    d = c(1, 2, NA, 4, 5, 6),
    e = c(1, 2, 3, NA, 5, 6),
    f = c(1, 2, 3, 4, NA, 6),
    g = c(1, 2, 3, 4, 5, NA)
  )
  expect_equal(.make_inadequate_estimate_na(df), expected_df)
})
