#' Default guidance settings for nomologR
#'
#' The numerical values returned by this function are teaching/reference points,
#' not universal pass/fail criteria. They are intended to trigger inspection and
#' explanation in downstream `nomologR` functions.
#'
#' @param profile Guidance profile. `"teaching"`, the only profile, holds
#'   reference values commonly taught in measurement courses.
#'
#' @details
#' Each value is a commonly taught reference point from the literature cited in
#' the corresponding analysis function (for example, [nomo_efa()],
#' [nomo_cfa()], [nomo_reliability()], and [nomo_validity()]). Changing a
#' reference changes which evidence is flagged for review; it never deletes
#' items, respecifies models, or declares validity.
#'
#' `long_string_min_items` is a count of items, not a reference value for a
#' statistic: [nomo_screen()] flags a case on long-string only when at least
#' that many items are screened, because half the length of a shorter item set
#' is a run that attentive respondents give often.
#'
#' `factor_cd_population`, `factor_cd_samples`, and `factor_cd_alpha` are the
#' simulation settings for the comparison-data criterion of [nomo_factors()]:
#' the size of each simulated population, the number of samples drawn per
#' candidate structure, and the significance level of the test that stops
#' adding factors. The defaults (5000 cases, 100 samples, .30) are smaller than
#' those of [EFAtools::efa_cd()] (10000 cases, 500 samples) to keep the run
#' short; raise them for a final analysis.
#'
#' `auto_delete` and `auto_respecify` are always `FALSE`. They record a design
#' rule rather than switch a feature: nomologR never deletes an item or
#' respecifies a model on its own. [nomo_factors()] and [nomo_efa()] stop with
#' an explanation when either is set to `TRUE`.
#'
#' @return A named list of guidance settings.
#' @export
#'
#' @examples
#' guidance <- nomo_defaults()
#' guidance$efa_loading_reference
#' guidance$fit_reference
#'
#' # A deliberately stricter loading reference flags more items for review.
#' stricter <- nomo_defaults()
#' stricter$efa_loading_reference <- 0.60
#' efa <- nomo_efa(nomo_demo_continuous, factors = 2, guidance = stricter)
#' efa$item_summary[, c("item", "primary_loading", "attention")]
nomo_defaults <- function(profile = "teaching") {
  profile <- nomo_match_arg(profile, choices = "teaching")

  list(
    profile = profile,
    item_total_reference = 0.30,
    response_concentration_reference = 0.80,
    nzv_frequency_ratio_reference = 19,
    nzv_percent_unique_reference = 10,
    long_string_min_items = 20L,
    factor_small_n_reference = 100L,
    factor_kmo_review_reference = 0.60,
    factor_kmo_concern_reference = 0.50,
    factor_parallel_iterations = 100L,
    factor_parallel_quantile = 0.95,
    factor_cd_population = 5000L,
    factor_cd_samples = 100L,
    factor_cd_alpha = 0.30,
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


# `auto_delete` and `auto_respecify` are not switches: nomologR never deletes
# an item or respecifies a model. A guidance list that sets either to TRUE
# would otherwise be accepted and silently ignored (#145, defaults-dead-fields).
nomo_defaults_check_safeguards <- function(guidance) {
  set <- c("auto_delete", "auto_respecify")[c(
    isTRUE(guidance$auto_delete), isTRUE(guidance$auto_respecify)
  )]
  if (length(set)) {
    stop(
      sprintf(
        paste(
          "%s cannot be `TRUE`: nomologR never deletes an item or respecifies",
          "a model on its own. Flags in the results mark what to review."
        ),
        nomo_present_or(sprintf("`guidance$%s`", set))
      ),
      call. = FALSE
    )
  }
  invisible(guidance)
}
