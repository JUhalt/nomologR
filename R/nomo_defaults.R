#' Default guidance settings for nomologR
#'
#' The numerical values returned by this function are teaching/reference points,
#' not universal pass/fail criteria. They are intended to trigger inspection and
#' explanation in downstream `nomologR` functions.
#'
#' @param profile Guidance profile. Only `"teaching"` is implemented during the
#'   initial development series.
#'
#' @return A named list of guidance settings.
#' @export
nomo_defaults <- function(profile = "teaching") {
  profile <- match.arg(profile, choices = "teaching")

  list(
    profile = profile,
    item_total_reference = 0.30,
    response_concentration_reference = 0.80,
    nzv_frequency_ratio_reference = 19,
    nzv_percent_unique_reference = 10,
    factor_small_n_reference = 100L,
    factor_kmo_review_reference = 0.60,
    factor_kmo_concern_reference = 0.50,
    factor_parallel_iterations = 100L,
    factor_parallel_quantile = 0.95,
    efa_loading_reference = 0.40,
    efa_crossloading_reference = 0.30,
    efa_communality_reference = 0.40,
    cfa_loading_reference = 0.50,
    fit_reference = list(
      cfi = 0.95,
      tli = 0.95,
      rmsea = 0.06,
      srmr = 0.08
    ),
    reliability_reference = 0.70,
    ave_reference = 0.50,
    htmt_reference = 0.85,
    auto_delete = FALSE,
    auto_respecify = FALSE,
    interpretation = paste(
      "Reference values trigger discussion; they do not automatically",
      "determine item retention, model respecification, or validity."
    )
  )
}
