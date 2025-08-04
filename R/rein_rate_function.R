#' Compute Rate of (Terminal) Reinforcement
#'
#' Calculates the terminal reinforcement rate given initial link (IL)
#' durations, terminal link (TL) duration, terminal reinforcement probability,
#' and schedule types. Optionally computes a common effective IL
#' duration when both schedules are variable interval (VI).
#'
#' @usage
#' r(
#'  il_dur1, il_dur2,
#'  tl_dur1, tr_p,
#'  il_sched1, il_sched2,
#'  common_il_dur = FALSE
#'  )
#'
#' @param il_dur1 Duration of the first alternative's IL.
#' @param il_dur2 Duration of the second alternative's IL.
#' @param tl_dur1 Duration of the first alternative's TL.
#' @param tr_p Probability of terminal reinforcement.
#' @param il_sched1 Schedule type for the initial links. (e.g., `"VI"`
#'  or `"FR"`). See [choice_params()] for additional details about inputting
#'  schedule types.
#' @param il_sched2 Schedule type for the second alternative's initial link.
#' @param common_il_dur Logical; if `TRUE`, a common initial link (IL) duration
#' is computed, but only when both IL schedules are `"VI"`. If `FALSE`, the
#' reinforcement rate is computed using only the first alternative's IL
#' duration, regardless of schedule type.
#'
#' @returns A numeric value (or vector) giving the computed terminal
#' reinforcement rate.
#'
#' @details
#' When using vectorised inputs, arguments of differing lengths will be
#' recycled according to R's standard vector recycling rules. If lengths are
#' not compatible (i.e., not clean multiples), a warning may be issued.
#' See [base::Arithmetic()] for details.
#'
#'
#' @seealso [choice_params()]
#'
#' @examples
#' r(il_dur1 = 90, il_dur2 = 30, tl_dur1 = 30, tr_p = 1.0,
#'   il_sched1 = "VI", il_sched2 = "VI", common_il_dur = TRUE)
#'
#' r(il_dur1 = 90, il_dur2 = NULL, tl_dur1 = 30, tr_p = 1.0,
#'   il_sched1 = "VI", il_sched2 = "VI", common_il_dur = FALSE)
#' @export
#-------------------------------------------------------------------------------
r <- function(il_dur1, il_dur2, tl_dur1, tr_p,
              il_sched1, il_sched2,
              common_il_dur = FALSE) {

  # Effective IL duration:
  eff_il <- ifelse(
    common_il_dur & il_sched1 == "VI" & il_sched2 == "VI",
    1 / (1/il_dur1 + 1/il_dur2),
    il_dur1
  )

  # Reinforcement Rate
  rate <- tr_p / (eff_il + tl_dur1)

  # When common_il_dur is TRUE but schedules aren't both VI, force NA
  rate[common_il_dur & (il_sched1 != "VI" | il_sched2 != "VI")] <- NA_real_

  # When reinforce prob is not between 0 and 1, force NA
  rate[tr_p < 0 | tr_p > 1] <- NA_real_

  rate
}
