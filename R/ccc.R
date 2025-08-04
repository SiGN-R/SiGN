#' Concordance Correlation Coefficient (CCC)
#'
#' Computes the Concordance Correlation Coefficient (CCC) between two numeric
#' vectors.
#' The CCC assesses the agreement between two sets of measurements by combining
#' measures of both precision (correlation) and accuracy (closeness to the
#' identity line).
#'
#' @param x A numeric vector of predicted or measured values.
#' @param y A numeric vector of observed or reference values. Must be the same
#' length as `x`.
#' @param bias_correction Logical. If \code{TRUE}, applies a bias correction by
#' using sample variance and covariance estimators (i.e., dividing by
#' \eqn{n - 1} instead of \eqn{n}).
#'
#' @returns A numeric value between -1 and 1 indicating the level of concordance.
#' A value of 1 indicates perfect agreement.
#'
#' @references Lawrence I-Kuei Lin. (1989). A Concordance Correlation Coefficient to Evaluate Reproducibility. *Biometrics*, *45*(1), 255–268. \url{https://doi.org/10.2307/2532051}
#'
#' @examples
#' x <- c(1, 2, 3, 4, 5)
#' y <- c(1.1, 1.9, 3.2, 4.1, 4.8)
#' ccc(x, y)                          # Unbiased sample-based method
#' ccc(x, y, bias_correction = FALSE) # Lin's original population method
#' @importFrom stats var cov
#' @export
ccc <- function(x, y, bias_correction = TRUE) {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("Both 'x' and 'y' must be numeric vectors.")
  }
  if (length(x) != length(y)) {
    stop("'x' and 'y' must be of the same length.")
  }
  n <- length(x)
  if (n < 2) {
    stop("At least two observations are required.")
  }

  mx <- mean(x)
  my <- mean(y)

  if (bias_correction) {
    s2x <- var(x)
    s2y <- var(y)
    sxy <- cov(x, y)
  } else {
    # Use population estimators: multiply sample by (n-1)/n
    s2x <- var(x) * (n - 1) / n
    s2y <- var(y) * (n - 1) / n
    sxy <- cov(x, y) * (n - 1) / n
  }

  numerator <- 2 * sxy
  denominator <- s2x + s2y + (mx - my)^2

  return(numerator / denominator)
}
