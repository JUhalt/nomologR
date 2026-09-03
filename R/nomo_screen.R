#' Audit item-level data before factor modeling
#'
#' `nomo_screen()` will provide item- and case-level diagnostics without
#' automatically removing or modifying observations.
#'
#' @param data A data frame containing candidate items.
#' @param items Optional character vector identifying item columns.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return During the development series this function is a documented API stub.
#' @export
nomo_screen <- function(data, items = NULL, guidance = nomo_defaults()) {
  nomo_not_implemented("nomo_screen", "Milestone 1")
}
