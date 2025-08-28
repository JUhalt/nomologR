#' Nomological validity (structural model)
#' @param meas_fit lavaan CFA fit
#' @param nomo_model lavaan syntax for structural relations
#' @param data data.frame
#' @export
cv_nomological <- function(meas_fit, nomo_model, data) {
  # TODO: extract factor scores or refit with structure; evaluate hypothesized paths
  list(paths = NULL, suggestions = "Implement SEM via lavaan::sem using fixed measurement")
}
