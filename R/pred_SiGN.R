#' Compute SiGN Prediction
#'
#' A helper function compute the probability of selecting the first alternative
#' according to the SiGN (Signal for Good News; Dunn et al. 2024) and by
#' extension DRH (Delay Reduction Hypothesis; Squires & Fantino, 1971).
#'
#' @param r1 Numeric vector of terminal reinforcement rates for alternative 1.
#' @param r2 Numeric vector of terminal reinforcement rates for alternative 2.
#' @param cr1 Numeric vector of \eqn{\delta} values for alternative 1.
#' @param cr2 Numeric vector of \eqn{\delta} values for alternative 2.
#'
#'  @details
#'  The underlying calculation is described in Dunn et al. (2024), with
#'  Equation 7 providing the core formulation.
#'
#'
#' @returns
#' A numeric vector of predicted choice probabilities for alternative 1.
#'
#' @references
#' Dunn, R. M., Pisklak, J. M., McDevitt, M. A., & Spetch, M. L. (2024).
#' Suboptimal choice: A review and quantification of the signal for good news
#' (SiGN) model. *Psychological Review*. *131*(1), 58-78.
#' \url{https://doi.org/10.1037/rev0000416}
#'
#' Squires, N., Fantino, E. (1971). A model for choice in simple concurrent and
#' concurrent-chains schedules. *Journal of the Experimental Analysis of
#' Behavior*. *15*(1), 27 - 38. \url{https://doi.org/10.1901/jeab.1971.15-27}
#'
#' @examples
#' pred_SiGN(r1 = 0.05, r2 = 0.05, cr1 = 12, cr2 = 0.6)
#' pred_SiGN(r1 = 0.05, r2 = 0.01, cr1 = 30, cr2 = -30)  # returns 1 (edge case)
#' @export
#-------------------------------------------------------------------------------
pred_SiGN <- function(r1, r2, cr1, cr2) {
  pred <- (r1 * cr1) / (r1 * cr1 + r2 * cr2)

  # Set edge cases
  pred[cr1 > 0 & cr2 < 0] <- 1
  pred[cr1 < 0 & cr2 > 0] <- 0

  pred
}
