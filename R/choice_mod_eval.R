#' Evaluate Model Fit with Descriptive and Likelihood-Based Metrics
#'
#' Computes a variety of descriptive and likelihood-based statistics for
#' evaluating the fit of a model to observed data. This includes common
#' descriptive error metrics, a concordance coefficient, and information
#' criteria (AIC, BIC) based on a beta-distributed error model.
#'
#' @param observed A numeric vector of observed values (e.g., choice
#' proportions). Values must lie within the closed interval `[0, 1]`.
#' @param predicted A numeric vector of predicted values. Must be the same
#' length as `observed`, and also constrained to the closed interval `[0, 1]`.
#' @param k An integer specifying the number of free parameters that were used
#' to generate the `predicted` values. The function incorporates an error
#' model that adds one to this value (see Details).
#' @param epsilon A small continuity correction used to constrain values
#' strictly within the open interval (0, 1). Defaults to `0.001`. This is
#' necessary for the beta error model, which is undefined at 0 and 1.
#' @param ... Additional arguments passed to internal functions.
#'
#' @returns An object of class `"choice_mod_eval"` containing:
#' \describe{
#'   \item{\code{desc_stats}}{A data frame of descriptive fit statistics: sample
#'   size, R-squared, mean bias, RMSE, MAE, median absolute error, and the
#'   concordance correlation coefficient.}
#'   \item{\code{info_criteria}}{A data frame listing the parameter amount,
#'   estimated \eqn{\phi} parameter value (precision of the beta distribution),
#'   log-likelihood, AIC, and BIC.}
#'   \item{\code{residuals}}{A numeric vector of residuals (observed -
#'   predicted), not printed by default.}
#' }
#'
#' @details
#'
#' The residual-based coefficient of determination (\eqn{R^2}) is calculated
#' as:
#' \deqn{R^2 = 1 - \frac{\sum_i (y_i - \hat{y}_i)^2}{\sum_i (y_i - \bar{y})^2}}
#' where \eqn{y_i} are the observed values, \eqn{\hat{y}_i} are the predicted
#' values, and \eqn{\bar{y}} is the mean of the observed values.
#'
#' While this form of \eqn{R^2} is widely used as a descriptive measure of
#' fit, it should be interpreted with caution in the context of nonlinear models.
#' Unlike in linear regression, \eqn{R^2} does not have a clear interpretation
#' in terms of explained variance, nor is it guaranteed to fall between 0 and 1.
#' In particular, it can take on negative values when the model fits worse than
#' the mean. As such, it is best used here as a rough, supplementary indicator
#' of model performance rather than a definitive measure of fit—effectively,
#' a "pseudo-\eqn{R^2}."
#'
#' For the likelihood-based metrics in `$info_criteria`, a beta-distributed
#' error model for choice proportions is incorporated. This adds one fitted
#' parameter (\eqn{\phi}), which is shared across all observations.
#' Consequently, AIC and BIC values are computed as having `k` + 1 free
#' parameters.
#'
#' The error model assumes that each observed value \eqn{y_i} is
#' drawn independently from a Beta distribution with a mean equal to the
#' choice model's prediction \eqn{\mu_i} and a precision parameter \eqn{\phi}:
#' \deqn{y_i \sim \text{Beta}(\alpha_i, \beta_i)}
#' with:
#' \deqn{\alpha_i = \mu_i \cdot \phi,\quad \beta_i = (1 - \mu_i) \cdot \phi}
#' where \eqn{\mu_i} is the predicted value and \eqn{\phi} is the precision
#' parameter.
#'
#' The total log-likelihood is computed as:
#' \deqn{\log L = \sum_i \log \left[ \text{Beta}(y_i | \alpha_i, \beta_i) \right]}
#'
#' The AIC and BIC are then computed as:
#' \deqn{\text{AIC} = 2k - 2\log L,\quad \text{BIC} = \log(n) \cdot k - 2\log L}
#' where \eqn{k = 1} is the number of estimated parameters and \eqn{n} is the number of observations.
#'
#' Observed and predicted values are assumed to lie strictly within the unit
#' interval (0, 1). Values near 0 or 1 are adjusted using the `epsilon`
#' parameter to avoid undefined behaviour under the beta distribution.
#'
#' The choice of `epsilon` can strongly influence the log-likelihood,
#' particularly when values approach 0 or 1. Users are encouraged to check the
#' sensitivity of the output to this value if model selection is a focus.
#'
#' When the sample size is less than 30, a message is printed to remind users
#' that information criteria may be unstable or overly sensitive in
#' small-sample settings.
#'
#' @seealso
#' Comprehensive guidance on interpreting the outputs and applying this
#' function is provided in the following companion vignettes:
#' `vignette("eval_descriptive", package = "SiGN")`
#' `vignette("eval_info-theoretic", package = "SiGN")`
#'
#' @examples
#' obs <- c(0.2, 0.4, 0.6, 0.8)
#' pred <- c(0.25, 0.35, 0.65, 0.75)
#' result <- choice_mod_eval(obs, pred)
#' result
#' result$residuals  # Access residuals directly
#' @importFrom stats median optimize dbeta
#' @export
#-------------------------------------------------------------------------------
choice_mod_eval <- function(observed, predicted, k = 0, epsilon = 0.001, ...) {
  # Throw error if obs is incorrect length
  if (length(observed) != length(predicted)) {
    stop(sprintf(
      "Observed and predicted values are different lengths: %d vs. %d",
      length(observed), length(predicted)
    ))
  }

  residuals <- observed - predicted
  ss_res_fit <- sum(residuals^2)
  ss_tot_fit <- sum((observed - mean(observed))^2)

  # General Stats
  n <- length(observed)

  if (n < 30) {
    message("\u2139 Sample size is small (n < 30); AIC and BIC may be unstable or less reliable for model comparison.")
  }

  rsq <- 1 - (ss_res_fit / ss_tot_fit)
  mean_bias <- mean(predicted - observed)
  rmse <- sqrt(mean(residuals^2))
  mean_ae <- mean(abs(residuals))
  med_ae <- median(abs(residuals))
  ccc_val <- ccc(predicted, observed, ...)

  # Fancy Stats
  # Estimate phi
  optim_result <- optimize(loglik_err,
    interval = c(1, 200),
    observed = observed, predicted = predicted, neg = TRUE, epsilon = epsilon
  )
  phi_est <- optim_result$minimum

  # Total Log-Likelihood
  ll <- loglik_err(observed, predicted, phi = phi_est,
                   neg = FALSE, epsilon = epsilon)

  # AIC & BIC
  k <- k + 1
  aic <- 2*k - 2 * ll
  bic <- -2 * ll + k * log(n)

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

  structure(
    list(
      desc_stats = desc_stats,
      info_criteria = ic_df,
      residuals = residuals),
    class = "choice_mod_eval"
  )

}

#' @export
print.choice_mod_eval <- function(x, ...) {
  cat("$desc_stats\n")
  print(x$desc_stats)
  cat("\n$info_criteria\n")
  print(x$info_criteria)
  cat("\nUse `object$residuals` to access the residuals.\n")
  invisible(x)
}


