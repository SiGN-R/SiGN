#' List Recycle
#'
#' Recycle list elements to equal length.
#'
#' @param x A named or unnamed list where each element is a vector. Elements
#' may vary in length.
#'
#' @returns A list where each element has length equal to the maximum length
#' of the original list elements. Shorter elements are repeated to match this
#' length.
#'
#' @examples
#' recycle_list(list(a = 1:3, b = 5))
#'
#' recycle_list(list(a = 1:7, b = c("Pigeon", "Pigeon", "Rat")))
#' @export
#-------------------------------------------------------------------------------
recycle_list <- function(x) {
  max_len <- max(lengths(x))
  lapply(x, function(el) if (length(el) == max_len) el else rep_len(el, max_len))
}
