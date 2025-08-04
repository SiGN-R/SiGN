#' Determine Whether a Terminal Link Alternative Is Signalled
#'
#' Evaluates whether an alternative can be considered "signalled" on the basis
#' of its two terminal link (TL) durations and reinforcement probabilities.
#'
#' @param dur1 A numeric vector giving the durations of an alternative's first
#' TL.
#' @param dur2 A numeric vector giving the durations of an alternative's second
#' TL.
#' @param rein_p1 A numeric vector giving the terminal reinforcement
#' probabilities for an alternative's first TL.
#' @param rein_p2 A numeric vector giving the terminal reinforcement
#' probabilities for an alternative's second TL.
#'
#' @returns A logical vector indicating whether the choice alternative can be
#' considered signalled.
#'
#' @details
#' The durations and probabilities input should correspond to only one choice
#' alternative.
#'
#' A choice alternative is treated as signalled if both terminal link (TL)
#' durations are non-zero and either the durations or the terminal
#' reinforcement probabilities differ between the two TLs.
#'
#' This definition assumes that differences in duration or reinforcement
#' probability imply discriminability between the stimuli associated with each
#' TL. For example, if one TL lasts 10 seconds and the other 20 seconds, and
#' both have a reinforcement probability of 1, it is assumed that the
#' organism can fully discriminate between them—e.g., the TLs may be visually
#' distinct (such as different colours) and thus are recognisable from the
#' moment of onset. See also [choice_params()]
#'
#' @examples
#' sig_check(10, 10, 0.5, 0.5)  # FALSE — same duration and probability
#' sig_check(10, 10, 0.5, 0.0)  # TRUE  — same duration, different probability
#' sig_check(10, 5, 0.5, 0.5)   # TRUE  — different durations
#' sig_check(0, 10, 0.5, 0.5)   # FALSE — one duration is zero
#' @export
#-------------------------------------------------------------------------------
sig_check <- function(dur1, dur2, rein_p1, rein_p2) {
  # An alternative is only "signalled" if:
  # 1. Both TL durations are non-zero
  # 2. Either duration or reinforcement probability differs between TLs.
  dur_valid <- dur1 != 0 & dur2 != 0
  differs   <- dur1 != dur2 | rein_p1 != rein_p2

  dur_valid & differs
}
