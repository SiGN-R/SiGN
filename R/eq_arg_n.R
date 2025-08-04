#' Equal Argument Check
#'
#' This function checks whether all elements in a list have and equal length.
#' It is useful when validating argument consistency in list-based input,
#' especially when recycling or vectorised operations depend on uniform lengths.
#'
#' @param x A list of vectors.
#'
#' @returns A logical scalar: `TRUE` if all elements have the same length,
#'   `FALSE` otherwise.
#' @export
#'
#' @examples
#' eq_arg_n(list(1:3, 4:6))      # TRUE
#' eq_arg_n(list(1:3, 4:5))      # FALSE
#' eq_arg_n(list("a", "b", "c")) # TRUE
#' @export
#-------------------------------------------------------------------------------
eq_arg_n <- function(x) {all(lengths(x) == lengths(x)[1])}
