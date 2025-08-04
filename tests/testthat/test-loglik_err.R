test_that("loglik_err() returns expected output for scalar phi", {
  obs <- c(0.2, 0.5, 0.8)
  pred <- c(0.25, 0.55, 0.75)
  phi <- 10

  loglik <- loglik_err(obs, pred, phi, neg = FALSE)
  negloglik <- loglik_err(obs, pred, phi, neg = TRUE)

  expect_type(loglik, "double")
  expect_lt(negloglik, 0)  # Negative log-likelihood should be < 0
  expect_equal(negloglik, -loglik, tolerance = 1e-8)
})

test_that("loglik_err() handles vectorised phi", {
  obs <- c(0.3, 0.6)
  pred <- c(0.35, 0.55)
  phi <- c(10, 20)

  result <- loglik_err(obs, pred, phi)
  expect_type(result, "double")
  expect_length(result, 1)
})

test_that("loglik_err() throws error on length mismatch", {
  obs <- c(0.2, 0.4)
  pred <- c(0.3, 0.5)
  phi <- c(10, 20, 30)  # Too long

  expect_error(loglik_err(obs, pred, phi))
})

test_that("loglik_err() handles boundary values safely", {
  obs <- c(0, 1, 0.9999)
  pred <- c(1, 0, 0.0001)
  phi <- 5

  result <- loglik_err(obs, pred, phi)
  expect_type(result, "double")
  expect_true(is.finite(result))
})

test_that("loglik_err() throws error with NA input", {
  obs <- c(0.2, NA)
  pred <- c(0.3, 0.5)
  phi <- 10

  expect_error(loglik_err(obs, pred, phi))
})

test_that("loglik_err() returns finite value", {
  set.seed(123)
  obs <- runif(10, 0.01, 0.99)
  pred <- runif(10, 0.01, 0.99)
  phi <- 25

  ll <- loglik_err(obs, pred, phi)
  expect_true(is.finite(ll))
})
