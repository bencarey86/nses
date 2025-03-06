test_that("States are filtered appropriately", {
  df <- data.frame(
    state = c("01", "02", "10", "20", "30", "40", "50", "72", "78")
  )
  expect_equal(
    .filter_states(df),
    data.frame(
      state = c("01", "02", "10", "20", "30", "40", "50")
    )
  )
})
