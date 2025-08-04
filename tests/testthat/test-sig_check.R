test_that("returns FALSE when durations and reinforcement probabilities are equal", {
  expect_false(sig_check(10, 10, 0.5, 0.5))
})

test_that("returns TRUE when reinforcement probabilities differ", {
  expect_true(sig_check(10, 10, 0.5, 0.0))
})

test_that("returns TRUE when durations differ", {
  expect_true(sig_check(10, 5, 0.5, 0.5))
})

test_that("returns FALSE when either duration is zero", {
  expect_false(sig_check(0, 10, 0.5, 0.0))
  expect_false(sig_check(10, 0, 0.5, 0.0))
  expect_false(sig_check(0, 0, 0.5, 0.0))
})

test_that("vectorised input returns correct logical vector", {
  result <- sig_check(
    dur1 = c(10, 10, 0, 5),
    dur2 = c(10, 5, 10, 5),
    rein_p1 = c(0.5, 0.5, 0.5, 0.5),
    rein_p2 = c(0.5, 0.5, 0.0, 0.5)
  )
  expect_equal(result, c(FALSE, TRUE, FALSE, FALSE))
})

test_that("partial recycling produces warning", {
  warns <- capture_warnings(
    sig_check(c(10, 10, 10), c(10, 20), c(0.5, 0.5, 0.5), c(0.5, 0.0))
  )
  expect_true(length(warns) > 0)
})
