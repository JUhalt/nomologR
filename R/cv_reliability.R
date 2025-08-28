#' Reliability (CR, omega) from a lavaan fit
#' @param fit lavaan object
#' @export
cv_reliability <- function(fit) {
  # TODO: use semTools::reliability and compute AVE/CR
  tibble::tibble(scale = character(), CR = numeric(), AVE = numeric())
}
