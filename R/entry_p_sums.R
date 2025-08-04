#' Sum Entry Probabilities Across Four Terminal Links
#'
#' This function takes a list of 4 numeric vectors (each representing entry
#' probabilities for a terminal link) and returns the sum of the probabilities
#' across the 4 vectors.
#'
#' @param x A list of 4 numeric vectors with an equal number of elements.
#'
#' @returns A numeric vector of sums, where each element corresponds to
#' the sum of the four probabilities.
#'
#'
#' @examples
#' entry_p_sums(list(
#'   a = c(0.2, 0.5, 1),
#'   b = c(0.8, 0.5, 0),
#'   c = c(0.2, 0.5, 1),
#'   d = c(0.8, 0.5, 1)
#' ))
#' @export
#-------------------------------------------------------------------------------
entry_p_sums <- function(x) {
  p_grid <- matrix(unlist(x), ncol = 4, byrow = FALSE)
  rowSums(p_grid)
}
