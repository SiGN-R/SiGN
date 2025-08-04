test_that("ccc returns correct value for known input", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(1.1, 1.9, 3.2, 4.1, 4.8)

  # Match Lin's original method
  expected <- 0.994303469704816
  result <- round(ccc(x, y, bias_correction = FALSE), 15)

  expect_equal(result, expected)
})


test_that("ccc is symmetric", {
  x <- c(10, 20, 30)
  y <- c(12, 18, 31)

  expect_equal(ccc(x, y), ccc(y, x))
  expect_equal(
    ccc(x, y, bias_correction = TRUE),
    ccc(y, x, bias_correction = TRUE)
  )
})

test_that("ccc returns 1 for identical vectors", {
  x <- c(0.1, 0.5, 0.9)

  expect_equal(ccc(x, x), 1)
  expect_equal(ccc(x, x, bias_correction = TRUE), 1)
})

test_that("ccc throws errors for invalid input", {
  expect_error(ccc(1:5, 1:4), "same length")
  expect_error(ccc(letters[1:5], 1:5), "must be numeric")
  expect_error(ccc(1:5, as.factor(1:5)), "must be numeric")
  expect_error(ccc(1, 1), "At least two observations")
})

test_that("ccc returns a value between -1 and 1", {
  set.seed(123)
  x <- rnorm(100)
  y <- 0.9 * x + rnorm(100, sd = 0.1)

  result <- ccc(x, y)
  result2 <- ccc(x, y, bias_correction = TRUE)

  expect_true(is.numeric(result))
  expect_gte(result, -1)
  expect_lte(result, 1)

  expect_gte(result2, -1)
  expect_lte(result2, 1)
})
