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
#' @param data A non-empty data frame.
#' @param validation_prop Proportion of rows assigned to the validation sample.
#'   Must be strictly between 0 and 1.
#' @param seed Integer seed used to make the split reproducible.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return A `nomo_split` object containing `calibration`, `validation`, a
#'   row-level `assignment` table, split sizes, the seed, and a decision log.
#' @export
#'
#' @examples
#' split <- nomo_split(mtcars, validation_prop = 0.40, seed = 2026)
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
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) ||
      !is.finite(seed) || seed != as.integer(seed)) {
    stop("`seed` must be one finite integer.", call. = FALSE)
  }
  if (!is.list(guidance) ||
      !"factor_small_n_reference" %in% names(guidance)) {
    stop(
      "`guidance` must include `factor_small_n_reference` from `nomo_defaults()`.",
      call. = FALSE
    )
  }

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
      "%d rows were assigned to calibration and %d to validation using seed %d.",
      n_calibration, n_validation, as.integer(seed)
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


#' @export
print.nomo_split <- function(x, ...) {
  cat("<nomo_split>\n")
  cat(sprintf(
    "Rows: %d total | %d calibration | %d validation\n",
    x$n_total, x$n_calibration, x$n_validation
  ))
  cat(sprintf(
    "Validation proportion: %.3f requested | %.3f realized | Seed: %d\n",
    x$validation_prop_requested,
    x$validation_prop_realized,
    x$seed
  ))
  cat(
    "Use splitting only when the gain in independence justifies the loss of precision.\n"
  )
  invisible(x)
}
