test_that("dur_entry_conflict evaluates correctly", {
  durs <- list(
    tl1 = c(10, 15, 30, 0),
    tl2 = c(10, 15, 0, 0),
    tl3 = c(10, 15, 90, 0),
    tl4 = c(10, 15, 0, 0)
  )

  probs_nc <- list(
    tl1 = c(0.2, 0.5, 1, 0),
    tl2 = c(0.8, 0.5, 0, 0),
    tl3 = c(0.2, 0.5, 1, 0),
    tl4 = c(0.8, 0.5, 0, 0)
  )

  probs_c <- list(
    tl1 = c(1, 0.5, 1, 0),
    tl2 = c(0, 0.5, 0, 0),
    tl3 = c(0.2, 0.5, 1, 0),
    tl4 = c(0.8, 0.5, 0, 0)
  )

  expect_equal(dur_entry_conflict(durs, probs_nc), FALSE)
  expect_equal(dur_entry_conflict(durs, probs_c), TRUE)
})
