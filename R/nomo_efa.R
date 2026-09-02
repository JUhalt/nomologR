#' Guided exploratory factor analysis
#'
#' The planned implementation will use oblique solutions by default, respect the
#' indicator type, and report item-level evidence without automatic deletion.
#'
#' @param data A data frame containing candidate items.
#' @param items Optional character vector identifying item columns.
#' @param factors Number of factors to extract.
#' @param rotation Rotation method; planned default is `"oblimin"`.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return During the development series this function is a documented API stub.
#' @export
nomo_efa <- function(data,
                     items = NULL,
                     factors,
                     rotation = "oblimin",
                     guidance = nomo_defaults()) {
  nomo_not_implemented("nomo_efa", "Milestone 3")
}
