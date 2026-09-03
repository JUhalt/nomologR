#' Evaluate measurement invariance across groups or occasions
#'
#' The planned implementation will use current `semTools` infrastructure such as
#' `measEq.syntax()` for configural, metric, scalar, and strict invariance. It will
#' not rely on the deprecated `measurementInvariance()` convenience function.
#'
#' @param model A `lavaan` measurement model.
#' @param data A data frame.
#' @param group Character scalar naming the grouping variable.
#' @param ordered Optional character vector of ordered indicators.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return During the development series this function is a documented API stub.
#' @export
nomo_invariance <- function(model,
                            data,
                            group,
                            ordered = NULL,
                            guidance = nomo_defaults()) {
  nomo_not_implemented("nomo_invariance", "Milestone 7")
}
