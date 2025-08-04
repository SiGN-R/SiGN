#' Bonus Delay Reduction
#'
#' A helper function to compute bonus delay reduction on a alternative.
#'
#' @param sig Logical vector indicating whether the choice alternative is
#' signalled.
#' @param big_t Numeric vector of the overall average programmed time to
#' terminal reinforcement from the start of the initial links (excluding
#' time spent in a intertrial interval). See [SiGN()].
#' @param s_plus_tr_p Numeric vector of terminal reinforcement probabilities
#' for an alternative's \eqn{S^{+}} terminal link. See [s_plus_tr_p()].
#' @param s_plus_dur Numeric vector of durations associated with an
#' alternative's \eqn{S^{+}} terminal link. See [s_plus_dur()].
#' @param tr_p Numeric vector of overall terminal reinforcement probabilities
#' for the alternative.
#' @param tl_dur Numeric vector of average terminal link durations for the
#' alternative.
#'
#' @returns
#' A numeric vector representing the bonus delay reduction.
#'
#' @details
#' If the alternative is not signalled (`sig == FALSE`), the bonus is set to 0.
#' @examples
#' dr_bonus(
#'   sig = TRUE,
#'   big_t = 20,
#'   s_plus_tr_p = 1,
#'   s_plus_dur = 10,
#'   tr_p = 0.2,
#'   tl_dur = 10
#' )
#' @export
#-------------------------------------------------------------------------------
dr_bonus <- function(sig, big_t, s_plus_tr_p, s_plus_dur, tr_p, tl_dur) {
  ifelse(sig,
    big_t * s_plus_tr_p - s_plus_dur - big_t * tr_p + tl_dur,
    0
  )
}
