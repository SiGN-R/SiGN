test_that("pred_SiGN returns correct values for standard input", {
  # Equal reinforcement and delay reduction → 0.5
  expect_equal(pred_SiGN(r1 = 1, r2 = 1, cr1 = 0.5, cr2 = 0.5), 0.5)

  # Different reinforcement and delay reduction → 0.8
  expect_equal(pred_SiGN(r1 = 2, r2 = 1, cr1 = 0.4, cr2 = 0.2), 0.8)

  # Lower DR for alt1 → closer to 0
  expect_equal(pred_SiGN(r1 = 1, r2 = 1, cr1 = 0.2, cr2 = 0.8), 0.2)
})

test_that("pred_SiGN handles edge cases with DR sign reversal", {
  # cr1 > 0, cr2 < 0 → force prediction to 1
  expect_equal(pred_SiGN(r1 = 1, r2 = 1, cr1 = 0.5, cr2 = -0.1), 1)

  # cr1 < 0, cr2 > 0 → force prediction to 0
  expect_equal(pred_SiGN(r1 = 1, r2 = 1, cr1 = -0.1, cr2 = 0.5), 0)
})

test_that("pred_SiGN is vectorised", {
  r1 <- c(1, 1)
  r2 <- c(1, 1)
  cr1 <- c(0.5, -0.1)
  cr2 <- c(0.5, 0.5)

  result <- pred_SiGN(r1, r2, cr1, cr2)
  expect_equal(result, c(0.5, 0))
})

test_that("pred_SiGN handles zero delay reductions", {
  # If both DRs are 0, result is NaN due to 0/0
  result <- suppressWarnings(pred_SiGN(1, 1, 0, 0))
  expect_true(is.nan(result))
})
