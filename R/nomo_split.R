#' Create a reproducible calibration/validation split
#'
#' `nomo_split()` creates an explicit random split for workflows in which EFA
#' and CFA should be evaluated on different observations when the available
#' sample permits it. The function does not claim that sample splitting is
#' always preferable: dividing a modest sample reduces precision in both
#' subsets, and an external validation sample is generally stronger evidence
#' when one is available.
#'
#' The caller's random-number-generator state is restored after the split so
#' that using `nomo_split()` does not silently alter later stochastic analyses.
#'
#' The split depends on the seed and on the random-number generator in use,
#' which `rng_kind` records: the same seed under another [RNGkind()], such as
#' `"L'Ecuyer-CMRG"` or `sample.kind = "Rounding"`, gives a different split. To
#' reproduce a split, set the recorded kinds with `RNGkind()` before calling
#' `nomo_split()` with the same seed, or keep `assignment`.
#'
#' @param data A non-empty data frame.
#' @param validation_prop Proportion of rows assigned to the validation sample.
#'   Must be strictly between 0 and 1.
#' @param seed Integer seed used, with the random-number generator recorded in
#'   `rng_kind`, to make the split reproducible.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return A `nomo_split` object. The fields to read are:
#'
#'   * `calibration` and `validation`: the two samples.
#'   * `assignment`: which sample each row went to.
#'   * `n_total`, `n_calibration`, `n_validation`, and
#'     `validation_prop_realized`.
#'   * `seed`, `rng_kind`, and `decision_log`. `rng_kind` is the
#'     random-number generator, normal, and sample kinds of [RNGkind()] that
#'     drew the split.
#'
#'   Other fields record the call, the settings used, and intermediate engine
#'   results. They may change between releases and are not part of the stable
#'   interface (see `?nomologR`).
#'
#'   `print()` shows the sizes of the two samples, the proportion requested and
#'   realized, the seed, and the random-number generator.
#'
#' @references
#' Fokkema, M., & Greiff, S. (2017). How performing PCA and CFA on the same
#' data equals trouble: Overfitting in the assessment of internal structure and
#' some editorial thoughts on it. *European Journal of Psychological
#' Assessment, 33*(6), 399-402. \doi{10.1027/1015-5759/a000460}
#'
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
#' modifications in covariance structure analysis: The problem of
#' capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.
#' \doi{10.1037/0033-2909.111.3.490}
#'
#' @export
#'
#' @examples
#' split <- nomo_split(nomo_demo_continuous, validation_prop = 0.40, seed = 2026)
#' split
#' nrow(split$calibration)
#' nrow(split$validation)
nomo_split <- function(data,
                       validation_prop = 0.50,
                       seed = 1234L,
                       guidance = nomo_defaults()) {
  if (!is.data.frame(data) || nrow(data) < 2L) {
    stop("`data` must be a data frame with at least two rows.", call. = FALSE)
  }
  if (!is.numeric(validation_prop) || length(validation_prop) != 1L ||
      is.na(validation_prop) || !is.finite(validation_prop) ||
      validation_prop <= 0 || validation_prop >= 1) {
    stop("`validation_prop` must be one number strictly between 0 and 1.", call. = FALSE)
  }
  if (!nomo_is_whole_number(seed)) {
    stop("`seed` must be one finite integer.", call. = FALSE)
  }
  if (!is.list(guidance) ||
      !"factor_small_n_reference" %in% names(guidance)) {
    stop(
      "`guidance` must include `factor_small_n_reference` from `nomo_defaults()`.",
      call. = FALSE
    )
  }
  nomo_defaults_check_safeguards(guidance)

  n <- nrow(data)
  n_validation <- as.integer(round(n * validation_prop))
  n_validation <- max(1L, min(n - 1L, n_validation))
  n_calibration <- n - n_validation

  rng_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (rng_exists) {
    rng_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (rng_exists) {
      assign(".Random.seed", rng_before, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(as.integer(seed))
  # The seed reproduces the split only under the same generator, so the
  # generator is recorded with it (#145). The first three kinds are the
  # documented ones; R-devel's RNGkind() adds a fourth that sample.int() does
  # not use.
  rng_kind <- RNGkind()[1:3]
  validation_rows <- sort(sample.int(n, size = n_validation, replace = FALSE))
  calibration_rows <- setdiff(seq_len(n), validation_rows)

  assignment <- tibble::tibble(
    row = seq_len(n),
    sample = ifelse(
      seq_len(n) %in% validation_rows,
      "validation",
      "calibration"
    )
  )

  log <- nomo_log_new()
  log <- nomo_log_add(
    log,
    stage = "sample_split",
    object = "sample",
    metric = "validation_proportion",
    value = n_validation / n,
    reference = paste(
      "Independent calibration/validation samples can reduce capitalization",
      "on chance, but splitting also reduces precision in each subset"
    ),
    severity = "info",
    observation = sprintf(
      "%d rows were assigned to calibration and %d to validation using seed %d with RNGkind() %s.",
      n_calibration, n_validation, as.integer(seed), paste(rng_kind, collapse = ", ")
    ),
    recommendation = paste(
      "Use the calibration subset for exploratory/model-development work and",
      "reserve validation rows for a prespecified confirmatory model when that",
      "design is substantively and statistically defensible."
    )
  )

  small_ref <- suppressWarnings(as.numeric(guidance$factor_small_n_reference)[1L])
  if (is.finite(small_ref) &&
      (n_calibration < small_ref || n_validation < small_ref)) {
    log <- nomo_log_add(
      log,
      stage = "sample_split",
      object = "sample",
      metric = "split_sample_size",
      value = min(n_calibration, n_validation),
      reference = paste(
        "The configured small-N reference is a review prompt, not a universal",
        "minimum sample-size rule"
      ),
      severity = "review",
      observation = sprintf(
        "At least one split contains fewer than %d rows.",
        as.integer(small_ref)
      ),
      recommendation = paste(
        "Consider whether an internal split sacrifices too much precision.",
        "Alternatives include a larger sample, external replication, or clearly",
        "labeling the CFA as same-sample rather than pretending independence."
      )
    )
  }

  out <- list(
    call = match.call(),
    seed = as.integer(seed),
    rng_kind = rng_kind,
    validation_prop_requested = validation_prop,
    validation_prop_realized = n_validation / n,
    n_total = n,
    n_calibration = n_calibration,
    n_validation = n_validation,
    calibration_rows = calibration_rows,
    validation_rows = validation_rows,
    assignment = assignment,
    calibration = data[calibration_rows, , drop = FALSE],
    validation = data[validation_rows, , drop = FALSE],
    decision_log = log
  )
  class(out) <- c("nomo_split", "list")
  out
}


# TRUE for one finite whole number that fits R's integer range. Testing the
# range first keeps as.integer() from turning 1e10 into NA, which made the
# argument checks fail with a base-R error rather than their own (#145).
nomo_is_whole_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    abs(x) <= .Machine$integer.max && x == round(x)
}


#' @export
print.nomo_split <- function(x, ...) {
  nomo_present_header("nomo_split", "Calibration and validation split")
  nomo_present_facts(c(
    sprintf("Rows: %d", x$n_total),
    sprintf("Calibration: %d", x$n_calibration),
    sprintf("Validation: %d", x$n_validation)
  ))
  # A proportion, without the leading zero, and the realized one with the
  # decimals that tell it from the requested one.
  nomo_present_facts(c(
    sprintf("Validation proportion: %s requested, %s realized",
            nomo_present_stat(x$validation_prop_requested, "proportion"),
            nomo_present_stat(x$validation_prop_realized, "proportion",
                              reference = x$validation_prop_requested)),
    sprintf("Seed: %d", x$seed),
    sprintf("Generator: %s", paste(x$rng_kind, collapse = ", "))
  ))
  cat("\n")
  nomo_present_text(
    "Use splitting only when the gain in independence justifies the loss of precision."
  )
  nomo_present_pointer("x$assignment", "the sample each row went to")
  invisible(x)
}
