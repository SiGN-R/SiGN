#' Check for Duration and Entry Probability Conflicts
#'
#' This function checks for conflicts between terminal link durations and their
#'  corresponding entry probabilities. Specifically, it returns `TRUE` if
#'  any of the specified durations are greater than zero while their
#'  associated entry probabilities are equal to zero. This typically indicates
#'  a logical inconsistency in the parameters for a choice alternative.
#'
#' @param durs A list of 4 numeric vectors representing terminal-link durations.
#' @param probs A list of 4 numeric vectors representing terminal-link
#' entry probabilities.
#'
#' @returns A logical value: `TRUE` if any duration > 0 is paired with an entry
#'  probability of 0 and `FALSE` otherwise.
#'
#' @examples
#' durs <- list(
#' tl1 = c(10, 15, 30, 0),
#' tl2 = c(10, 15, 0,  0),
#' tl3 = c(10, 15, 90, 0),
#' tl4 = c(10, 15, 0,  0)
#' )
#' # List of probabilities with no duration conflict
#' probs_nc <- list(
#' tl1 = c(0.2, 0.5, 1, 0),
#' tl2 = c(0.8, 0.5, 0, 0),
#' tl3 = c(0.2, 0.5, 1, 0),
#' tl4 = c(0.8, 0.5, 0, 0)
#' )
#' # List of probabilities with duration conflict
#' probs_c <- list(
#' tl1 = c(1, 0.5,   1, 0),
#' tl2 = c(0, 0.5,   0, 0),
#' tl3 = c(0.2, 0.5, 1, 0),
#' tl4 = c(0.8, 0.5, 0, 0)
#' )
#'
#' dur_entry_conflict(durs, probs_nc)
#' dur_entry_conflict(durs, probs_c)
#' @export
#-------------------------------------------------------------------------------
dur_entry_conflict <- function(durs, probs){
  d <- matrix(unlist(durs), ncol = 4, byrow = FALSE)
  p <- matrix(unlist(probs), ncol = 4, byrow = FALSE)
  any((d > 0) & (p == 0))
}
