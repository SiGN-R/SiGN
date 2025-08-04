#' Compute (Negative) Log-Likelihood for Beta Error Model
#'
#' Calculates the log-likelihood (or negative log-likelihood) of a set of
#' observed values under a Beta-distributed error model.
#'
#' @param observed A numeric vector of observed values (e.g., proportions),
#' each in the range `[0, 1]`.
#' @param predicted A numeric vector of predicted values from the model, also
#' in `[0, 1]`.
#' @param phi A scalar or numeric vector specifying the precision
#' (concentration) parameter(s) of the Beta distribution. If a vector, it must
#' be the same length as `observed` and `predicted`.
#' @param neg Logical. If `TRUE` (default), returns the negative
#' log-likelihood (useful for optimisation). If `FALSE`, returns the
#' log-likelihood.
#' @param epsilon A small numeric value used to constrain `observed` and
#' `predicted` away from 0 and 1, ensuring numerical stability (default
#' is `1e-3`). See Details.
#'
#' @returns A single numeric value: the negative log-likelihood or
#' log-likelihood of the data under the Beta error model.
#'
#' @details
#' This function calculates the (log) likelihood of observed choice proportions under a
#' Beta-distributed error model. Since the Beta distribution is only defined on the open interval
#' (0, 1), a small continuity correction (`epsilon`) is applied to both the `observed`
#' and `predicted` values to avoid zero or one values that would result in undefined or infinite
#' log-likelihoods.
#'
#' The choice of `epsilon` can substantially affect the resulting log-likelihood, particularly
#' when any values are near 0 or 1. This is due to the steep curvature of the Beta distribution near
#' the boundaries, especially when the shape parameters are less than 1. Smaller values of
#' `epsilon` reduce the size of the correction, but can introduce large penalties to the
#' likelihood if the data approach the boundaries.
#'
#' For this reason, it is important to choose an `epsilon` value that balances numerical
#' stability and minimal distortion of the data. A default of `epsilon = 0.001` is typically
#' sufficient for behavioural data, but users are encouraged to check the sensitivity of their results when values are
#' close to 0 or 1.
#'
#' @note
#' If the observed data include values near 0 or 1, the log-likelihood may
#' be highly sensitive to the choice of `epsilon`. See Details.
#'
#' @examples
#' # Scalar phi
#' loglik_err(c(0.3, 0.6), c(0.25, 0.55), phi = 20)
#'
#' # Vectorised phi
#' loglik_err(c(0.3, 0.6), c(0.25, 0.55), phi = c(15, 25))
#'
#' # Return log-likelihood instead of negative log-likelihood
#' loglik_err(c(0.3, 0.6), c(0.25, 0.55), phi = 20, neg = FALSE)
#'
#' @export
#-------------------------------------------------------------------------------
loglik_err <- function(observed, predicted, phi, neg = TRUE, epsilon = 1e-3) {
  # Check lengths
  n <- length(observed)

  if (!(length(predicted) == n && (length(phi) == 1 || length(phi) == n))) {
    stop("`phi` must be either length 1 or the same length as `observed` and `predicted`.")
  }

  if (anyNA(c(observed, predicted, phi))) {
    stop("Inputs must not contain NA values.")
  }

  # if (any(observed < epsilon | observed > 1 - epsilon)) {
  #  warning("Some observed values are near 0 or 1; log-likelihood is sensitive to 'epsilon'.")
  #}

  # Continuity correction
  y <- pmin(pmax(observed, epsilon), 1 - epsilon)
  mu <- pmin(pmax(predicted, epsilon), 1 - epsilon)

  # Recycle phi if needed
  if (length(phi) == 1) {
    phi <- rep(phi, n)
  }

  # Compute shape parameters
  alpha_shape <- mu * phi
  beta_shape  <- (1 - mu) * phi

  # Compute log-likelihood
  log_lik <- sum(dbeta(y, alpha_shape, beta_shape, log = TRUE))

  if (neg) {
    return(-log_lik)
  } else {
    return(log_lik)
  }
}
