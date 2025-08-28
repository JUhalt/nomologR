#' Multi-group measurement invariance
#' @param meas_model lavaan syntax
#' @param data data.frame
#' @param group string column name for groups
#' @export
cv_invariance <- function(meas_model, data, group) {
  # TODO: pass to semTools::measurementInvariance and summarize ΔCFI etc.
  list(summary = NULL)
}
