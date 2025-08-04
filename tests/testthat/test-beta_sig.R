test_that("beta returns 1 when use_beta or sig is FALSE", {
  expect_equal(
    beta_sig(FALSE, TRUE, "VI", "VI", 10, 20, 5),
    1
  )

  expect_equal(
    beta_sig(TRUE, FALSE, "VI", "VI", 10, 20, 5),
    1
  )

  expect_equal(
    beta_sig(FALSE, FALSE, "VI", "VI", 10, 20, 5),
    1
  )
})

test_that("beta computes correct value for VI-VI schedules", {
  result <- beta_sig(TRUE, TRUE, "VI", "VI", 10, 20, 5)
  expect_equal(round(result, 3), round(log10(1 + 5 / (1 / (1 / 10 + 1 / 20))), 3))
})

test_that("beta uses il_dur1 when schedules are not both VI", {
  # Use il_dur1 = 10
  # beta = log10(1 + 5 / 10) = log10(1.5) ≈ 0.176
  result <- beta_sig(TRUE, TRUE, "FR", "VI", 10, 20, 5)
  expect_equal(round(result, 3), round(log10(1 + 5 / 10), 3))
})
