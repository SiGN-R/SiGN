test_that("s_plus_dur returns correct duration when sig = TRUE", {
  expect_equal(s_plus_dur(TRUE, 10, 20, 0.5, 0.3), 10)
  expect_equal(s_plus_dur(TRUE, 10, 20, 0.2, 0.5), 20)
  expect_equal(s_plus_dur(TRUE, 5, 5, 0.5, 0.5), 5)  # equal rates, returns first
})

test_that("s_plus_dur returns NA_real_ when sig = FALSE", {
  expect_true(is.na(s_plus_dur(FALSE, 10, 20, 0.5, 0.3)))
  expect_type(s_plus_dur(FALSE, 10, 20, 0.5, 0.3), "double")
})

test_that("s_plus_dur handles zero durations or probabilities gracefully", {
  expect_equal(s_plus_dur(TRUE, 10, 20, 0, 0.5), 20)
  expect_equal(s_plus_dur(TRUE, 10, 20, 0.5, 0), 10)
  expect_true(is.na(s_plus_dur(TRUE, 10, 20, 0, 0)))
  expect_true(is.na(s_plus_dur(TRUE, 0, 10, 0.5, 0.5)))
  expect_type(s_plus_dur(TRUE, 10, 20, 0, 0), "double")
  expect_type(s_plus_dur(TRUE, 0, 10, 0.5, 0.5), "double")
})

test_that("s_plus_dur handles ties gracefully", {
  expect_equal(s_plus_dur(TRUE, 20, 10, 0.5, 0.5), 10)
  expect_equal(s_plus_dur(TRUE, 10, 10, 0.5, 0.5), 10)
})

test_that("s_plus_dur vectorises correctly", {
  sig_vec <- c(TRUE, TRUE, FALSE)
  dur1_vec <- c(10, 10, 10)
  dur2_vec <- c(20, 20, 20)
  p1_vec <- c(0.5, 0.2, 0.5)
  p2_vec <- c(0.3, 0.5, 0.3)

  result <- s_plus_dur(sig_vec, dur1_vec, dur2_vec, p1_vec, p2_vec)
  expect_equal(result, c(10, 20, NA_real_))
})
