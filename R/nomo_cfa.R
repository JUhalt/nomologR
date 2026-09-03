#' Guided confirmatory factor analysis
#'
#' The planned implementation will delegate estimation to `lavaan`, choose or
#' recommend estimators appropriate to indicator type, and present global and
#' local fit as evidence rather than as a binary pass/fail verdict.
#'
#' @param model A researcher-specified `lavaan` measurement model.
#' @param data A data frame containing the indicators.
#' @param ordered Optional character vector of ordered indicators.
#' @param estimator Optional `lavaan` estimator.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return During the development series this function is a documented API stub.
#' @export
nomo_cfa <- function(model,
                     data,
                     ordered = NULL,
                     estimator = NULL,
                     guidance = nomo_defaults()) {
  nomo_not_implemented("nomo_cfa", "Milestone 4")
}
