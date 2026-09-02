#' Evaluate a theory-specified nomological network
#'
#' The planned implementation will fit the full latent structural model when
#' item-level measurement is available and compare observed relations with
#' expectations supplied by [nomo_hypotheses()]. Post-hoc changes will be
#' explicitly labeled as exploratory.
#'
#' @param model A `lavaan` structural model containing the measurement model.
#' @param data A data frame.
#' @param hypotheses Optional theoretical expectations from [nomo_hypotheses()].
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return During the development series this function is a documented API stub.
#' @export
nomo_network <- function(model,
                         data,
                         hypotheses = NULL,
                         guidance = nomo_defaults()) {
  nomo_not_implemented("nomo_network", "Milestone 6")
}
