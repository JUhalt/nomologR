#' Run the guided nomologR workflow
#'
#' `nomo_run()` will eventually coordinate the component functions while pausing
#' at consequential researcher decisions. It will never silently remove items or
#' respecify a model.
#'
#' @param data A data frame.
#' @param scales A named list defining candidate scale items.
#' @param mode Planned output mode: `"teaching"` or `"research"`.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return During the development series this function is a documented API stub.
#' @export
nomo_run <- function(data,
                     scales,
                     mode = c("teaching", "research"),
                     guidance = nomo_defaults()) {
  mode <- match.arg(mode)
  nomo_not_implemented("nomo_run", "Milestone 8")
}
