test_that("choice_mod_eval returns correct structure", {
  obs <- c(0.2, 0.4, 0.6, 0.8)
  pred <- c(0.25, 0.35, 0.65, 0.75)

  result <- suppressWarnings(choice_mod_eval(obs, pred))

  expect_s3_class(result, "choice_mod_eval")
  expect_named(result, c("desc_stats", "info_criteria", "residuals"))
  expect_true(is.data.frame(result$desc_stats))
  expect_true(is.data.frame(result$info_criteria))
  expect_true(is.numeric(result$residuals))
})

test_that("summary contains expected columns", {
  obs <- c(0.2, 0.3, 0.5, 0.7)
  pred <- c(0.25, 0.28, 0.48, 0.72)

  result <- suppressWarnings(choice_mod_eval(obs, pred))
  expect_true(all(c("n", "r_squared", "mean_bias", "rmse", "mae", "median_ae", "ccc") %in% names(result$desc_stats)))
})

test_that("info_criteria contains phi, logLik, AIC, BIC", {
  obs <- c(0.2, 0.3, 0.5, 0.7)
  pred <- c(0.25, 0.28, 0.48, 0.72)

  result <- suppressWarnings(choice_mod_eval(obs, pred))
  expect_true(all(c("n_parameters","phi", "logLik", "AIC", "BIC") %in%
                    names(result$info_criteria)))
})

test_that("residuals are calculated correctly", {
  obs <- c(0.1, 0.2, 0.3)
  pred <- c(0.15, 0.18, 0.29)

  result <- suppressWarnings(choice_mod_eval(obs, pred))
  expected <- obs - pred
  expect_equal(result$residuals, expected)
})

test_that("errors are thrown for mismatched lengths", {
  expect_error(choice_mod_eval(1:5, 1:4), "different lengths")
})

test_that("warning/message is triggered for small sample size", {
  obs <- c(0.2, 0.3)
  pred <- c(0.25, 0.35)

  expect_message(
    suppressWarnings(choice_mod_eval(obs, pred))
    )
})

test_that("epsilon affects log-likelihood", {
  obs <- c(0, 0.02, 0.03, 0.04)
  pred <- c(0.01, 0.02, 0.03, 0.04)

  ll1 <- suppressWarnings(choice_mod_eval(obs, pred, epsilon = 0.001)$info_criteria$logLik)
  ll2 <- suppressWarnings(choice_mod_eval(obs, pred, epsilon = 0.0001)$info_criteria$logLik)

  expect_false(isTRUE(all.equal(ll1, ll2)))  # Log-likelihood should differ

})

test_that("k affects AIC", {
  obs <- c(0.01, 0.02, 0.03, 0.04)
  pred <- c(0.01, 0.02, 0.03, 0.04)

  ll1 <- suppressWarnings(choice_mod_eval(obs, pred, k = 0)$info_criteria$AIC)
  ll2 <- suppressWarnings(choice_mod_eval(obs, pred, k = 1)$info_criteria$AIC)

  expect_false(isTRUE(all.equal(ll1, ll2)))

})

test_that("k affects BIC", {
  obs <- c(0.01, 0.02, 0.03, 0.04)
  pred <- c(0.01, 0.02, 0.03, 0.04)

  ll1 <- suppressWarnings(choice_mod_eval(obs, pred, k = 0)$info_criteria$BIC)
  ll2 <- suppressWarnings(choice_mod_eval(obs, pred, k = 1)$info_criteria$BIC)

  expect_false(isTRUE(all.equal(ll1, ll2)))

})

test_that("Warning is triggered when n/k <= 10", {
  # Simulate small dataset
  observed <- rep(0.5, 20)         # n = 20
  predicted <- rep(0.5, 20)
  k <- 3                           # n/k = 6.67, should trigger warning

  expect_warning(
    choice_mod_eval(observed, predicted, k = k)
  )
})
