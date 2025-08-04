test_that("dr_bonus returns correct value when sig = TRUE", {
  result <- dr_bonus(
    sig = TRUE,
    big_t = 20,
    s_plus_tr_p = 0.8,
    s_plus_dur = 10,
    tr_p = 0.5,
    tl_dur = 12
  )
  # Expected: (20 * 0.8) - 10 - (20 * 0.5) + 12 = 16 - 10 - 10 + 12 = 8
  expect_equal(result, 8)
})

test_that("dr_bonus returns 0 when sig = FALSE", {
  result <- dr_bonus(
    sig = FALSE,
    big_t = 20,
    s_plus_tr_p = 0.8,
    s_plus_dur = 10,
    tr_p = 0.5,
    tl_dur = 12
  )
  expect_equal(result, 0)
})

test_that("dr_bonus handles vectorised input correctly", {
  result <- dr_bonus(
    sig = c(TRUE, FALSE),
    big_t = c(20, 20),
    s_plus_tr_p = c(0.8, 0.7),
    s_plus_dur = c(10, 9),
    tr_p = c(0.5, 0.5),
    tl_dur = c(12, 12)
  )
  # First: (20 * 0.8) - 10 - (20 * 0.5) + 12 = 8
  # Second: sig = FALSE => 0
  expect_equal(result, c(8, 0))
})

test_that("dr_bonus handles zero durations and probabilities", {
  result <- dr_bonus(
    sig = TRUE,
    big_t = 0,
    s_plus_tr_p = 0,
    s_plus_dur = 0,
    tr_p = 0,
    tl_dur = 0
  )
  # Expected: 0 * 0 - 0 - 0 * 0 + 0 = 0
  expect_equal(result, 0)
})
