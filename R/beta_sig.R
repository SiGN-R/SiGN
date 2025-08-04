#' Compute the β (beta) parameter for the SiGN model
#'
#' Computes the β adjustment for signalled alternatives, reflecting the
#' balance between conditional and terminal (i.e., primary/unconditional)
#' reinforcement, as described in Equation 6 of Dunn et al. (2024).
#'
#' @param use_beta Logical vector indicating whether the β parameter
#' should be applied.
#' @param sig Logical vector indicating whether the alternative is signalled.
#' See [sig_check].
#' @param sched1 Character vector indicating the schedule type for the first
#' alternative's initial link. See Details.
#'  (e.g., `"VI"`, `"FR"`).
#' @param sched2 Character vector indicating the schedule type for the second
#' alternative's initial link. See Details.
#' @param il_dur1 Numeric vector of durations for the first alternative's
#' initial link.
#' @param il_dur2 Numeric vector of durations for the second alternative's
#' initial link.
#' @param s_plus_dur Numeric vector of durations associated with alternative
#' 1's \eqn{S^{+}} stimulus.
#' @param base A positive real number specifying the base of
#' the logarithm used for β. Defaults to 10.
#'
#' @details
#' Although the initial link durations for both alternatives are provided as
#' arguments, the function computes β specifically for the first initial
#' link.
#'
#' If both initial links are variable interval (VI) schedules, a common initial
#' link duration is computed: \deqn{1 / (1 / d_1 + 1 / d_2)}
#' Otherwise, the duration of the first initial link is used.
#'
#' See [choice_params()] for additional details appropriate on
#' schedule selection.
#'
#' @returns
#' A numeric vector containing the β values.
#'
#' @references
#' Dunn, R. M., Pisklak, J. M., McDevitt, M. A., & Spetch, M. L. (2024).
#' Suboptimal choice: A review and quantification of the signal for good news
#' (SiGN) model. *Psychological Review*. *131*(1), 58-78.
#' \url{https://doi.org/10.1037/rev0000416}
#'
#'
#' @examples
#' beta_sig(
#'   use_beta = TRUE,
#'   sig = TRUE,
#'   sched1 = "FR",
#'   sched2 = "FR",
#'   il_dur1 = 1,
#'   il_dur2 = 1,
#'   s_plus_dur = 10
#' )
#' @export
#-------------------------------------------------------------------------------
beta_sig <- function(use_beta, sig,
                 sched1, sched2,
                 il_dur1, il_dur2,
                 s_plus_dur,
                 base = 10) {
  # Adjust il duration if common VI
  eff_il <- ifelse(sched1 == "VI" & sched2 == "VI",
    1 / (1 / il_dur1 + 1 / il_dur2),
    il_dur1
  )

  apply_beta <- sig & use_beta
  ifelse(apply_beta,
    log(1 + s_plus_dur / eff_il, base = base),
    1
  )
}
