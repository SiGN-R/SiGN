#' Validate Schedule Type Input
#'
#' Check whether pairs of schedule types input are valid. Specifically, each
#' pair must be either "VI" "VI" or "FR", "FR".
#'
#' @param x A list of 2 character vectors, where each vector contains schedule
#' labels. See [choice_params()] for additional details about inputting
#'  schedule types.
#'
#' @returns A logical value: `TRUE` if all pairs contain valid and matching
#' schedule types.
#'
#' @seealso [choice_params()]
#'
#' @examples
#' # Valid
#' valid_sched_input(list(c("VI", "VI", "FR"), c("VI", "VI", "FR")))
#' # Invalid
#' valid_sched_input(list(c("FR", "VI", "FR"), c("VI", "VI", "FR")))
#' @export
#-------------------------------------------------------------------------------
valid_sched_input <- function(x) {
  mat <- matrix(unlist(x), ncol = 2, byrow = FALSE)
  all(apply(mat, MARGIN = 1, function(row) all(row == "VI") || all(row == "FR")))
}
