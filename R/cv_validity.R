#' Convergent & discriminant validity (AVE, HTMT, Fornell-Larcker)
#' @param fit lavaan object
#' @export
cv_validity <- function(fit) {
  # TODO: compute HTMT (semTools::htmt), Fornell-Larcker matrix, flag issues
  list(AVE = NULL, HTMT = NULL, FL = NULL)
}
