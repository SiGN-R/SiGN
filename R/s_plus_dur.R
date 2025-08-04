#' Duration of the S+ Terminal Link
#'
#' Given two terminal link stimuli, identifies the one with
#' the greater reinforcement probability per unit of time (i.e., higher
#' \eqn{p/t} ratio), and returns its duration. This is interpreted as the
#'  \eqn{S^{+}} duration within a signalled alternative.
#'
#' @param sig Logical. Indicates whether the terminal link stimuli are assumed
#' to be discriminable. See [sig_check()].
#' @param tl_dur1 Numeric. Duration of the first terminal link.
#' @param tl_dur2 Numeric. Duration of the second terminal link.
#' @param tr_p1 Numeric. Probability of terminal (primary/unconditional)
#' reinforcement following the first terminal link.
#' @param tr_p2 Numeric. Probability of terminal (primary/unconditional)
#' reinforcement following the second terminal link.
#'
#' @returns A numeric vector containing the duration of the identified \eqn{S^{+}} terminal link, or `NA` if undefined.
#'
#' @details
#' If both terminal reinforcement probabilities are 0, or if either
#' terminal link duration is 0, the function returns `NA`. If the choice
#' alternative is not signalled (`sig == FALSE`), `NA` is also returned.
#'
#' @seealso [s_delta()], [s_plus_tr_p()]
#'
#' @examples
#' s_plus_dur(TRUE, 10, 20, 0.5, 0.8)
#' s_plus_dur(TRUE, 10, 20, 0, 0) # Returns NA
#' s_plus_dur(FALSE, 10, 20, 0.5, 0.8) # Returns NA
#' @export
#-------------------------------------------------------------------------------
s_plus_dur <- function(sig, tl_dur1, tl_dur2, tr_p1, tr_p2) {
  invalid <- (tr_p1 == 0 & tr_p2 == 0) | (tl_dur1 == 0 | tl_dur2 == 0)
  pt1 <- tr_p1 / tl_dur1
  pt2 <- tr_p2 / tl_dur2

  result <- ifelse(pt1 > pt2, tl_dur1, tl_dur2)
  result[invalid | !sig] <- NA_real_
  result
}
