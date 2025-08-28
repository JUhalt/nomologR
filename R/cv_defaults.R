#' Default decision thresholds for the nomologr pipeline
#'
#' @return A named list of cutoffs and targets used throughout the pipeline.
#' @export
cv_defaults <- function() {
  list(
    item_total_min = 0.30,
    efa_loading_min = 0.40,
    efa_crossloading_max = 0.30,
    efa_communal_min = 0.40,
    cfa_loading_min = 0.50,
    fit_targets = list(CFI = 0.95, TLI = 0.95, RMSEA = 0.06, SRMR = 0.08),
    CR_min = 0.70,
    AVE_min = 0.50,
    HTMT_max = 0.85
  )
}
