#' Confirmatory factor analysis helper
#' @param meas_model lavaan syntax for measurement model only
#' @param data data.frame
#' @param cutoffs list from [cv_defaults()]
#' @export
cv_cfa <- function(meas_model, data, cutoffs = cv_defaults()) {
  # TODO: run lavaan::cfa and evaluate against cutoffs$fit_targets and cfa_loading_min
  list(fit = NULL, loadings = NULL, suggestions = "Implement CFA with lavaan::cfa")
}
