#' Fit a Generalised SiGN Model and Evaluate Fit Quality
#'
#' This function fits a generalised version of the SiGN model to observed
#' choice proportions using non-linear least squares via `nlsLM()`, and
#' evaluates model fit using both descriptive and likelihood-based metrics.
#' A beta-distributed error model is used to estimate the log-likelihood and
#' compute AIC and BIC values. The model includes three free parameters:
#' bias (\code{b}), sensitivity to reinforcement rate (\code{k_r}), and
#' sensitivity to delay reduction (\code{k_d}).
#'
#' @param params A list of parameter inputs compatible with the `SiGN()`
#' function, such as the output from `choice_params()`. Must be of the same
#' length as \code{observed}.
#' @param observed A numeric vector of observed choice proportions, one per condition.
#' Values must lie strictly between 0 and 1.
#' @param b Starting value for the bias parameter (default is 1).
#' @param k_r Starting value for the reinforcement rate sensitivity parameter
#' (default is 1).
#' @param k_d Starting value for the delay reduction sensitivity parameter
#' (default is 1).
#' @param epsilon A small positive value used to prevent undefined operations
#' (e.g., division by zero). Used when computing delay reductions or for
#' numerical stability in likelihood calculations.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return A list of class \code{"SiGN_gen_eval"} with the following components:
#' \describe{
#'   \item{param_est}{A data frame of estimated model parameters (\code{b},
#'   \code{k_r}, \code{k_d}).}
#'   \item{desc_stats}{Descriptive statistics summarizing model fit (e.g.,
#'   RMSE, R-squared, MAE, CCC).}
#'   \item{info_criteria}{A data frame with the estimated \code{phi}, total
#'   log-likelihood, AIC, and BIC.}
#'   \item{mod_info}{The full model object returned by \code{nlsLM()}.}
#'   \item{phi_optim_result}{The result from \code{optimize()} used to estimate
#'   the beta model precision parameter.}
#'   \item{residuals}{Vector of residuals (observed minus predicted choice
#'   proportions).}
#'   \item{details}{A data frame of all intermediate values, including
#'   predictions and components of the SiGN model.}
#' }
#'
#' @details
#' The function assumes that each observed choice proportion is independent
#' and follows a beta distribution centred around the predicted value from the
#' generalised SiGN model. A single precision parameter (\code{phi}) is estimated
#' per model fit. AIC and BIC are based on four free parameters (three from the
#' model, one from the error distribution). Residual checks are recommended.
#' For additional details regarding the Beta error model, see [choice_mod_eval].
#'
#' @seealso \code{\link[minpack.lm]{nlsLM}}, [choice_mod_eval], [SiGN],
#' [choice_params], [loglik_err]
#'
#' @examples
#' params <- do.call(choice_params, as.list(subopt_avian[9:24]))
#' result <- SiGN_gen_eval(params, subopt_avian$cp, b = 1, k_r = 1, k_d = 1)
#' print(result)
#' @importFrom stats coef predict
#' @importFrom minpack.lm nls.lm.control
#' @export
#-------------------------------------------------------------------------------
SiGN_gen_eval <- function(params, observed,
                          b = 1, k_r = 1, k_d = 1,
                          epsilon = 1e-3, ...) {
  # Throw error if observed is incorrect length
  if (max(lengths(params)) != length(observed)) {
    stop(sprintf(
      "Model parameters and observed values are different lengths: %d vs. %d",
      max(lengths(params)), length(observed)
    ))
  }

  if (any(observed < 0 | observed > 1)) {
    stop("Observed values must be strictly between 0 and 1.")
  }

  if (b == 1 && k_r == 1 && k_d == 1) {
    message("\u2139 Default starting values (1, 1, 1) are being used. For better results, consider setting values that match the patterns in your data - poor starting points can trip up the fitting algorithm.")
  }

  # Storage list
  vals <- as.list(SiGN(params)$details)
  vals[1] <- NULL
  vals$cp_obs <- observed
  vals$cr_a <- pmax(vals$cr_a, epsilon)
  vals$cr_b <- pmax(vals$cr_b, epsilon)

  # Function for generalized version's model
  SiGN_gen <- function(b, k_r, k_d, r_a, r_b, cr_a, cr_b) {
    (b * (r_a^k_r * cr_a^k_d)) /
      (b * (r_a^k_r * cr_a^k_d) + r_b^k_r * cr_b^k_d)
  }

  fit <- minpack.lm::nlsLM(
    cp_obs ~ SiGN_gen(b, k_r, k_d, r_a, r_b, cr_a, cr_b),
    data = vals,
    start = list(b = b, k_r = k_r, k_d = k_d),
    lower = c(0, 0, 0),
    control = nls.lm.control(
      maxiter = 1000,
      ftol = 1e-8,
      ptol = 1e-8,
      maxfev = 5000
    )
  )

  # Predictions
  vals$cp_fit <- predict(fit) # predictions

  # Parameter estimates
  params <- coef(fit)
  b <- params["b"]
  k_r <- params["k_r"]
  k_d <- params["k_d"]

  # Descriptive model stats
  residuals <- vals$cp_obs - vals$cp_fit
  ss_res_fit <- sum(residuals^2)
  ss_tot_fit <- sum((vals$cp_obs - mean(vals$cp_obs))^2)

  # General Stats
  n <- length(vals$cp_obs)

  if (n < 30) {
    message("\u2139 Sample size is small (n < 30); AIC and BIC may be unstable or less reliable for model comparison.")
  }

  rsq <- 1 - (ss_res_fit / ss_tot_fit)
  mean_bias <- mean(vals$cp_fit - vals$cp_obs)
  rmse <- sqrt(mean(residuals^2))
  mean_ae <- mean(abs(residuals))
  med_ae <- median(abs(residuals))
  ccc_val <- ccc(vals$cp_fit, vals$cp_obs, ...)

  # Fancy Stats
  # Estimate phi
  optim_result <- optimize(loglik_err,
    interval = c(1, 200),
    observed = vals$cp_obs,
    predicted = vals$cp_fit,
    neg = TRUE,
    epsilon = epsilon
  )

  phi_est <- optim_result$minimum

  # Total Log-Likelihood
  ll <- loglik_err(
    observed = vals$cp_obs,
    predicted = vals$cp_fit,
    phi = phi_est,
    neg = FALSE,
    epsilon = epsilon
  )

  # AIC & BIC
  k <- 4
  aic <- 2 * k - 2 * ll
  bic <- -2 * ll + k * log(n)


  # Output
  param_est <- data.frame(
    b = b,
    k_r = k_r,
    k_d = k_d
  )

  desc_stats <- data.frame(
    n = n,
    r_squared = rsq,
    mean_bias = mean_bias,
    rmse = rmse,
    mae = mean_ae,
    median_ae = med_ae,
    ccc = ccc_val
  )

  ic_df <- data.frame(
    n_parameters = k,
    phi = phi_est,
    logLik = ll,
    AIC = aic,
    BIC = bic
  )

  # Return structured output
  output <- list(
    param_est = param_est,
    desc_stats = desc_stats,
    info_criteria = ic_df,
    mod_info = fit,
    phi_optim_result = optim_result,
    residuals = residuals,
    details = as.data.frame(vals)
  )

  class(output) <- "SiGN_gen_eval"
  return(output)
}

#' @export
print.SiGN_gen_eval <- function(x, ...) {
  cat("$param_est\n")
  print(x$param_est)

  cat("\n$desc_stats\n")
  print(x$desc_stats)

  cat("\n$info_criteria\n")
  print(x$info_criteria)
}
