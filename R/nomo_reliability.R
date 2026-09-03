#' Model-based reliability evidence
#'
#' The planned implementation will emphasize omega/composite reliability using
#' current `semTools` infrastructure such as `compRelSEM()`. Coefficient alpha
#' will be available as a familiar secondary statistic with its assumptions made
#' explicit. AVE is intentionally handled by [nomo_validity()], not here.
#'
#' @param fit A fitted measurement model.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return During the development series this function is a documented API stub.
#' @export
nomo_reliability <- function(fit, guidance = nomo_defaults()) {
  nomo_not_implemented("nomo_reliability", "Milestone 5")
}
