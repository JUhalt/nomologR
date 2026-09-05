# Confirmatory factor analysis ------------------------------------------------

#' Guided confirmatory factor analysis
#'
#' `nomo_cfa()` fits a researcher-specified confirmatory factor model with
#' `lavaan::cfa()` and adds a transparent guidance layer around estimator
#' choice, convergence, standardized loadings, global fit, local residuals,
#' Heywood diagnostics, and modification indices.
#'
#' The fitted `lavaan` object is retained unchanged in `$fit`. `nomologR` does
#' not free parameters, add residual covariances, delete indicators, or refit a
#' different model in response to fit statistics or modification indices.
#'
#' @param model A researcher-specified `lavaan` measurement-model syntax string
#'   or an object created by [nomo_model()].
#' @param data A data frame containing the model indicators.
#' @param ordered Optional character vector naming binary/ordinal indicators.
#'   When supplied and `estimator = NULL`, `nomo_cfa()` explicitly requests
#'   `WLSMV`.
#' @param estimator Optional `lavaan` estimator. For continuous indicators,
#'   leaving this as `NULL` preserves `lavaan::cfa()`'s default. Robust ML
#'   variants such as `"MLR"` remain explicit researcher choices.
#' @param missing Optional `lavaan` missing-data option such as `"fiml"` for
#'   continuous ML models or `"pairwise"` where supported.
#' @param std.lv Logical. Passed directly to `lavaan::cfa()`.
#' @param control Optional named list passed to lavaan's optimizer through
#'   `lavaan::cfa(control = ...)`. This is an advanced troubleshooting/reproducibility
#'   option; non-default optimizer controls are recorded in the decision log.
#' @param modification_indices Logical; if `TRUE`, compute modification indices
#'   as quarantined diagnostics. They never trigger automatic respecification.
#' @param mi_top Number of largest modification indices to retain in the compact
#'   `$top_modification_indices` view. The full table remains available in
#'   `$modification_indices`.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return A `nomo_cfa` object containing the unchanged `lavaan` fit plus
#'   standardized loadings, factor correlations, fit evidence, residual
#'   diagnostics, Heywood checks, modification indices, engine warnings, and a
#'   decision log.
#' @export
#'
#' @examples
#' \dontrun{
#' model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' out <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
#' summary(out)
#' }
nomo_cfa <- function(model,
                     data,
                     ordered = NULL,
                     estimator = NULL,
                     missing = NULL,
                     std.lv = FALSE,
                     control = NULL,
                     modification_indices = TRUE,
                     mi_top = 10L,
                     guidance = nomo_defaults()) {
  if (inherits(model, "nomo_model")) model <- as.character(model)

  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !nzchar(trimws(model))) {
    stop("`model` must be one non-empty lavaan model-syntax string.", call. = FALSE)
  }
  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }

  if (is.null(ordered)) {
    ordered <- character()
  } else {
    if (!is.character(ordered) || anyNA(ordered) ||
        any(!nzchar(trimws(ordered)))) {
      stop("`ordered` must be NULL or a character vector of indicator names.", call. = FALSE)
    }
    ordered <- unique(trimws(ordered))
    missing_ordered <- setdiff(ordered, names(data))
    if (length(missing_ordered)) {
      stop(
        sprintf(
          "Ordered indicator(s) not found in `data`: %s.",
          paste(missing_ordered, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(estimator)) {
    if (!is.character(estimator) || length(estimator) != 1L || is.na(estimator) ||
        !nzchar(trimws(estimator))) {
      stop("`estimator` must be NULL or one non-empty character value.", call. = FALSE)
    }
    estimator <- toupper(trimws(estimator))
  }

  if (!is.null(missing)) {
    if (!is.character(missing) || length(missing) != 1L || is.na(missing) ||
        !nzchar(trimws(missing))) {
      stop("`missing` must be NULL or one non-empty character value.", call. = FALSE)
    }
    missing <- trimws(missing)
  }

  if (!is.logical(std.lv) || length(std.lv) != 1L || is.na(std.lv)) {
    stop("`std.lv` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(control)) {
    if (!is.list(control) || is.null(names(control)) ||
        anyNA(names(control)) || any(!nzchar(trimws(names(control))))) {
      stop("`control` must be NULL or a named list of lavaan optimizer controls.", call. = FALSE)
    }
  }
  if (!is.logical(modification_indices) || length(modification_indices) != 1L ||
      is.na(modification_indices)) {
    stop("`modification_indices` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(mi_top) || length(mi_top) != 1L || is.na(mi_top) ||
      !is.finite(mi_top) || mi_top < 0 || mi_top != as.integer(mi_top)) {
    stop("`mi_top` must be a non-negative integer.", call. = FALSE)
  }
  mi_top <- as.integer(mi_top)
  nomo_cfa_validate_guidance(guidance)

  if (length(ordered) && !is.null(estimator) && grepl("^ML", estimator)) {
    stop(
      paste0(
        "ML-family estimators are not supported here with declared ordered ",
        "indicators. Leave `estimator = NULL` for WLSMV or choose a ",
        "categorical-data estimator supported by lavaan."
      ),
      call. = FALSE
    )
  }

  if (length(ordered) && !is.null(missing) &&
      tolower(missing) %in% c("ml", "fiml", "ml.x", "fiml.x")) {
    stop(
      paste0(
        "FIML is not supported by lavaan for declared ordered indicators. ",
        "Use an ordered-data missingness option supported by your estimator ",
        "or address missingness before this model."
      ),
      call. = FALSE
    )
  }

  estimator_source <- if (length(ordered) && is.null(estimator)) {
    "ordered_default"
  } else if (is.null(estimator)) {
    "lavaan_default"
  } else {
    "researcher"
  }

  estimator_requested <- estimator
  if (length(ordered) && is.null(estimator_requested)) estimator_requested <- "WLSMV"

  fit_args <- list(model = model, data = data, std.lv = std.lv)
  if (length(ordered)) fit_args$ordered <- ordered
  if (!is.null(estimator_requested)) fit_args$estimator <- estimator_requested
  if (!is.null(missing)) fit_args$missing <- missing
  if (!is.null(control)) fit_args$control <- control

  engine_warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      do.call(lavaan::cfa, fit_args),
      warning = function(w) {
        engine_warnings <<- unique(c(engine_warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(paste0("CFA estimation failed: ", conditionMessage(e)), call. = FALSE)
    }
  )

  options <- tryCatch(lavaan::lavInspect(fit, "options"), error = function(e) list())
  engine_estimator <- if (!is.null(options$estimator)) {
    toupper(as.character(options$estimator)[1L])
  } else if (!is.null(estimator_requested)) {
    toupper(estimator_requested)
  } else {
    NA_character_
  }

  estimator_label <- if (!is.null(estimator_requested)) {
    toupper(estimator_requested)
  } else {
    engine_estimator
  }

  converged <- isTRUE(tryCatch(
    lavaan::lavInspect(fit, "converged"),
    error = function(e) FALSE
  ))

  n_used_raw <- tryCatch(lavaan::lavInspect(fit, "nobs"), error = function(e) NA_real_)
  n_used <- suppressWarnings(sum(as.numeric(unlist(n_used_raw)), na.rm = TRUE))
  if (!length(n_used) || !is.finite(n_used) || n_used <= 0) n_used <- NA_real_

  n_dropped <- if (is.finite(n_used)) max(0, nrow(data) - n_used) else NA_real_
  pct_dropped <- if (is.finite(n_dropped) && nrow(data) > 0L) {
    n_dropped / nrow(data)
  } else {
    NA_real_
  }
  sample_summary <- tibble::tibble(
    n_input = nrow(data),
    n_used = n_used,
    n_dropped = n_dropped,
    pct_dropped = pct_dropped
  )

  fit_measures_all <- tryCatch(lavaan::fitMeasures(fit), error = function(e) numeric())
  fit_evidence <- nomo_cfa_fit_evidence(fit_measures_all, guidance = guidance)

  parameter_estimates <- tryCatch(
    tibble::as_tibble(lavaan::parameterEstimates(fit, standardized = TRUE, ci = TRUE)),
    error = function(e) tibble::tibble()
  )

  standardized_solution <- tryCatch(
    tibble::as_tibble(lavaan::standardizedSolution(fit, type = "std.all")),
    error = function(e) tibble::tibble()
  )

  standardized_loadings <- nomo_cfa_loadings(standardized_solution, guidance = guidance)
  latent_names <- if (nrow(standardized_loadings)) {
    unique(standardized_loadings$factor)
  } else {
    tryCatch(as.character(lavaan::lavNames(fit, type = "lv")), error = function(e) character())
  }
  observed_names <- if (nrow(standardized_loadings)) {
    unique(standardized_loadings$item)
  } else {
    tryCatch(as.character(lavaan::lavNames(fit, type = "ov")), error = function(e) character())
  }

  factor_correlations <- nomo_cfa_factor_correlations(standardized_solution, latent_names)
  heywood <- nomo_cfa_heywood(
    parameter_estimates = parameter_estimates,
    standardized_solution = standardized_solution,
    latent_names = latent_names,
    observed_names = observed_names
  )

  residual_warnings <- character()
  residual_object <- tryCatch(
    withCallingHandlers(
      lavaan::lavResiduals(fit, type = "cor.bentler", summary = TRUE),
      warning = function(w) {
        residual_warnings <<- unique(c(residual_warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  residual_matrix <- nomo_cfa_residual_matrix(residual_object, "cov")
  residual_z_matrix <- nomo_cfa_residual_matrix(residual_object, "cov.z")
  residual_pairs <- nomo_cfa_residual_pairs(residual_matrix)
  residual_z_pairs <- nomo_cfa_residual_pairs(residual_z_matrix)

  mi_warnings <- character()
  mi_error <- NULL
  if (modification_indices && converged) {
    modification_table <- tryCatch(
      withCallingHandlers(
        tibble::as_tibble(
          lavaan::modificationIndices(fit, standardized = TRUE, sort. = TRUE)
        ),
        warning = function(w) {
          mi_warnings <<- unique(c(mi_warnings, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        mi_error <<- conditionMessage(e)
        tibble::tibble()
      }
    )
  } else {
    modification_table <- tibble::tibble()
  }

  top_modification_indices <- if (nrow(modification_table) && mi_top > 0L) {
    utils::head(modification_table, mi_top)
  } else {
    modification_table[0, , drop = FALSE]
  }

  all_warnings <- unique(c(engine_warnings, residual_warnings, mi_warnings))

  decision_log <- nomo_cfa_decision_log(
    estimator_label = estimator_label,
    engine_estimator = engine_estimator,
    estimator_source = estimator_source,
    ordered = ordered,
    missing = missing,
    control = control,
    data_n = nrow(data),
    n_used = n_used,
    converged = converged,
    warnings = all_warnings,
    fit_evidence = fit_evidence,
    loadings = standardized_loadings,
    heywood = heywood,
    residual_pairs = residual_pairs,
    modification_indices = modification_table,
    modification_indices_requested = modification_indices,
    mi_error = mi_error
  )

  out <- list(
    call = match.call(),
    model = model,
    data_n = nrow(data),
    n_used = n_used,
    n_dropped = n_dropped,
    pct_dropped = pct_dropped,
    sample_summary = sample_summary,
    ordered = ordered,
    estimator = estimator_label,
    estimator_engine = engine_estimator,
    estimator_source = estimator_source,
    missing = if (is.null(missing)) {
      if (!is.null(options$missing)) as.character(options$missing)[1L] else NA_character_
    } else missing,
    std.lv = std.lv,
    control = control,
    converged = converged,
    engine_warnings = all_warnings,
    fit = fit,
    fit_measures_all = fit_measures_all,
    fit_evidence = fit_evidence,
    parameter_estimates = parameter_estimates,
    standardized_solution = standardized_solution,
    standardized_loadings = standardized_loadings,
    factor_correlations = factor_correlations,
    heywood = heywood,
    heywood_detected = nrow(heywood) > 0L,
    residuals = residual_object,
    residual_matrix = residual_matrix,
    residual_z_matrix = residual_z_matrix,
    residual_pairs = residual_pairs,
    residual_z_pairs = residual_z_pairs,
    modification_indices = modification_table,
    top_modification_indices = top_modification_indices,
    modification_indices_requested = modification_indices,
    decision_log = decision_log,
    guidance = guidance
  )
  class(out) <- c("nomo_cfa", "list")
  out
}


nomo_cfa_validate_guidance <- function(guidance) {
  if (!is.list(guidance)) {
    stop("`guidance` must be a list returned by `nomo_defaults()`.", call. = FALSE)
  }
  needed <- c("cfa_loading_reference", "fit_reference")
  missing_needed <- setdiff(needed, names(guidance))
  if (length(missing_needed)) {
    stop(
      sprintf(
        "`guidance` is missing required setting(s): %s.",
        paste(missing_needed, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  fit_needed <- c("cfi", "tli", "rmsea", "srmr")
  if (!is.list(guidance$fit_reference) ||
      any(!fit_needed %in% names(guidance$fit_reference))) {
    stop("`guidance$fit_reference` must contain cfi, tli, rmsea, and srmr.", call. = FALSE)
  }
  invisible(TRUE)
}


nomo_cfa_first_measure <- function(measures, candidates) {
  if (!length(measures)) return(list(value = NA_real_, variant = NA_character_))
  for (candidate in candidates) {
    if (candidate %in% names(measures)) {
      value <- suppressWarnings(as.numeric(measures[[candidate]])[1L])
      if (length(value) && is.finite(value)) {
        return(list(value = value, variant = candidate))
      }
    }
  }
  list(value = NA_real_, variant = NA_character_)
}


nomo_cfa_fit_evidence <- function(measures, guidance) {
  refs <- guidance$fit_reference
  chi <- nomo_cfa_first_measure(measures, c("chisq.scaled", "chisq"))
  df <- nomo_cfa_first_measure(measures, c("df.scaled", "df"))
  p <- nomo_cfa_first_measure(measures, c("pvalue.scaled", "pvalue"))
  cfi <- nomo_cfa_first_measure(measures, c("cfi.robust", "cfi.scaled", "cfi"))
  tli <- nomo_cfa_first_measure(measures, c("tli.robust", "tli.scaled", "tli"))
  rmsea <- nomo_cfa_first_measure(measures, c("rmsea.robust", "rmsea.scaled", "rmsea"))
  rmsea_lo <- nomo_cfa_first_measure(
    measures,
    c("rmsea.ci.lower.robust", "rmsea.ci.lower.scaled", "rmsea.ci.lower")
  )
  rmsea_hi <- nomo_cfa_first_measure(
    measures,
    c("rmsea.ci.upper.robust", "rmsea.ci.upper.scaled", "rmsea.ci.upper")
  )
  srmr <- nomo_cfa_first_measure(measures, c("srmr"))

  values <- c(
    chi_square = chi$value,
    df = df$value,
    p_value = p$value,
    CFI = cfi$value,
    TLI = tli$value,
    RMSEA = rmsea$value,
    RMSEA_CI_lower = rmsea_lo$value,
    RMSEA_CI_upper = rmsea_hi$value,
    SRMR = srmr$value
  )
  variants <- c(
    chi$variant, df$variant, p$variant, cfi$variant, tli$variant,
    rmsea$variant, rmsea_lo$variant, rmsea_hi$variant, srmr$variant
  )
  reference <- c(
    NA_real_, NA_real_, NA_real_, refs$cfi, refs$tli, refs$rmsea,
    NA_real_, NA_real_, refs$srmr
  )
  direction <- c(
    "information", "information", "information", "higher", "higher",
    "lower", "information", "information", "lower"
  )
  attention <- rep("info", length(values))
  attention[!is.finite(values)] <- "unavailable"
  review_higher <- names(values) %in% c("CFI", "TLI") &
    is.finite(values) & values < reference
  review_lower <- names(values) %in% c("RMSEA", "SRMR") &
    is.finite(values) & values > reference
  attention[review_higher | review_lower] <- "review"

  explanation <- rep(
    "Descriptive model-fit information; interpret with estimator, sample, local fit, and theory.",
    length(values)
  )
  for (metric in c("CFI", "TLI")) {
    idx <- names(values) == metric
    explanation[idx] <- ifelse(
      attention[idx] == "review",
      "Below the configured teaching reference; inspect global and localized model strain.",
      "At or above the configured teaching reference; this does not establish model correctness."
    )
  }
  for (metric in c("RMSEA", "SRMR")) {
    idx <- names(values) == metric
    explanation[idx] <- ifelse(
      attention[idx] == "review",
      "Above the configured teaching reference; inspect global and localized model strain.",
      "At or below the configured teaching reference; this does not establish model correctness."
    )
  }

  tibble::tibble(
    metric = names(values),
    value = as.numeric(values),
    variant = variants,
    reference = as.numeric(reference),
    direction = direction,
    attention = attention,
    explanation = explanation
  )
}


nomo_cfa_loadings <- function(standardized_solution, guidance) {
  empty <- tibble::tibble(
    factor = character(), item = character(), loading = numeric(), se = numeric(),
    z = numeric(), p_value = numeric(), ci_lower = numeric(), ci_upper = numeric(),
    attention = character(), explanation = character()
  )
  if (!inherits(standardized_solution, "data.frame") || !nrow(standardized_solution) ||
      !all(c("lhs", "op", "rhs", "est.std") %in% names(standardized_solution))) {
    return(empty)
  }
  rows <- standardized_solution[standardized_solution$op == "=~", , drop = FALSE]
  if (!nrow(rows)) return(empty)

  get_col <- function(name) {
    if (name %in% names(rows)) suppressWarnings(as.numeric(rows[[name]])) else rep(NA_real_, nrow(rows))
  }
  loading <- get_col("est.std")
  attention <- rep("KEEP", length(loading))
  explanation <- rep("No configured standardized-loading review flag was triggered.", length(loading))
  weak <- is.finite(loading) & abs(loading) < guidance$cfa_loading_reference
  extreme <- is.finite(loading) & abs(loading) > 1 + 1e-6
  unavailable <- !is.finite(loading)
  attention[weak] <- "REVIEW"
  explanation[weak] <- paste0(
    "Absolute standardized loading is below the configured teaching reference of ",
    format(guidance$cfa_loading_reference, trim = TRUE),
    "; inspect item content, precision, and model specification."
  )
  attention[extreme] <- "STRONG REVIEW"
  explanation[extreme] <- paste(
    "Absolute standardized loading exceeds 1; inspect for a Heywood/improper",
    "solution, model misspecification, sampling instability, or identification issues."
  )
  attention[unavailable] <- "REVIEW"
  explanation[unavailable] <- paste(
    "A finite standardized loading was not available; inspect convergence,",
    "identification, and the underlying lavaan output."
  )

  tibble::tibble(
    factor = as.character(rows$lhs),
    item = as.character(rows$rhs),
    loading = loading,
    se = get_col("se"),
    z = get_col("z"),
    p_value = get_col("pvalue"),
    ci_lower = get_col("ci.lower"),
    ci_upper = get_col("ci.upper"),
    attention = attention,
    explanation = explanation
  )
}


nomo_cfa_factor_correlations <- function(standardized_solution, latent_names) {
  empty <- tibble::tibble(
    factor1 = character(), factor2 = character(), correlation = numeric(),
    se = numeric(), z = numeric(), p_value = numeric(),
    ci_lower = numeric(), ci_upper = numeric()
  )
  if (!inherits(standardized_solution, "data.frame") || !nrow(standardized_solution) ||
      !length(latent_names) ||
      !all(c("lhs", "op", "rhs", "est.std") %in% names(standardized_solution))) {
    return(empty)
  }
  keep <- standardized_solution$op == "~~" &
    standardized_solution$lhs != standardized_solution$rhs &
    standardized_solution$lhs %in% latent_names &
    standardized_solution$rhs %in% latent_names
  rows <- standardized_solution[keep, , drop = FALSE]
  if (!nrow(rows)) return(empty)
  get_col <- function(name) {
    if (name %in% names(rows)) suppressWarnings(as.numeric(rows[[name]])) else rep(NA_real_, nrow(rows))
  }
  tibble::tibble(
    factor1 = as.character(rows$lhs),
    factor2 = as.character(rows$rhs),
    correlation = get_col("est.std"),
    se = get_col("se"),
    z = get_col("z"),
    p_value = get_col("pvalue"),
    ci_lower = get_col("ci.lower"),
    ci_upper = get_col("ci.upper")
  )
}


nomo_cfa_heywood <- function(parameter_estimates, standardized_solution,
                             latent_names, observed_names) {
  out <- tibble::tibble(
    object = character(), issue = character(), value = numeric(),
    severity = character(), explanation = character()
  )

  if (inherits(parameter_estimates, "data.frame") && nrow(parameter_estimates) &&
      all(c("lhs", "op", "rhs", "est") %in% names(parameter_estimates))) {
    variance_rows <- parameter_estimates$op == "~~" &
      parameter_estimates$lhs == parameter_estimates$rhs
    observed_negative <- variance_rows &
      parameter_estimates$lhs %in% observed_names &
      is.finite(parameter_estimates$est) & parameter_estimates$est < -1e-8
    latent_negative <- variance_rows &
      parameter_estimates$lhs %in% latent_names &
      is.finite(parameter_estimates$est) & parameter_estimates$est < -1e-8

    if (any(observed_negative)) {
      idx <- which(observed_negative)
      out <- dplyr::bind_rows(
        out,
        tibble::tibble(
          object = as.character(parameter_estimates$lhs[idx]),
          issue = "negative_observed_residual_variance",
          value = as.numeric(parameter_estimates$est[idx]),
          severity = "concern",
          explanation = paste(
            "Negative observed residual variance is an improper-solution",
            "signal; inspect identification, sampling instability, and model specification."
          )
        )
      )
    }
    if (any(latent_negative)) {
      idx <- which(latent_negative)
      out <- dplyr::bind_rows(
        out,
        tibble::tibble(
          object = as.character(parameter_estimates$lhs[idx]),
          issue = "negative_latent_variance",
          value = as.numeric(parameter_estimates$est[idx]),
          severity = "concern",
          explanation = paste(
            "Negative latent variance is an improper-solution signal;",
            "inspect identification, sampling instability, and model specification."
          )
        )
      )
    }
  }

  if (inherits(standardized_solution, "data.frame") && nrow(standardized_solution) &&
      all(c("lhs", "op", "rhs", "est.std") %in% names(standardized_solution))) {
    extreme <- standardized_solution$op == "=~" &
      is.finite(standardized_solution$est.std) &
      abs(standardized_solution$est.std) > 1 + 1e-6
    if (any(extreme)) {
      idx <- which(extreme)
      out <- dplyr::bind_rows(
        out,
        tibble::tibble(
          object = paste0(standardized_solution$lhs[idx], " =~ ", standardized_solution$rhs[idx]),
          issue = "standardized_loading_beyond_one",
          value = as.numeric(standardized_solution$est.std[idx]),
          severity = "concern",
          explanation = paste(
            "Absolute standardized loading greater than one can signal an",
            "improper/Heywood solution or severe model strain."
          )
        )
      )
    }

    impossible_latent_correlation <- standardized_solution$op == "~~" &
      standardized_solution$lhs != standardized_solution$rhs &
      standardized_solution$lhs %in% latent_names &
      standardized_solution$rhs %in% latent_names &
      is.finite(standardized_solution$est.std) &
      abs(standardized_solution$est.std) > 1 + 1e-6

    if (any(impossible_latent_correlation)) {
      idx <- which(impossible_latent_correlation)
      out <- dplyr::bind_rows(
        out,
        tibble::tibble(
          object = paste0(
            standardized_solution$lhs[idx],
            " ~~ ",
            standardized_solution$rhs[idx]
          ),
          issue = "latent_correlation_beyond_one",
          value = as.numeric(standardized_solution$est.std[idx]),
          severity = "concern",
          explanation = paste(
            "Absolute standardized latent correlation greater than one is",
            "inadmissible and signals an improper solution or severe model strain."
          )
        )
      )
    }
  }
  out
}


nomo_cfa_residual_matrix <- function(residual_object, component = "cov") {
  if (is.null(residual_object)) return(matrix(numeric(), nrow = 0L, ncol = 0L))
  if (is.list(residual_object) && component %in% names(residual_object) &&
      is.matrix(residual_object[[component]])) {
    return(as.matrix(residual_object[[component]]))
  }
  if (is.list(residual_object) && length(residual_object) == 1L &&
      is.list(residual_object[[1L]]) && component %in% names(residual_object[[1L]]) &&
      is.matrix(residual_object[[1L]][[component]])) {
    return(as.matrix(residual_object[[1L]][[component]]))
  }
  matrix(numeric(), nrow = 0L, ncol = 0L)
}


nomo_cfa_residual_pairs <- function(residual_matrix) {
  if (!is.matrix(residual_matrix) || nrow(residual_matrix) < 2L || ncol(residual_matrix) < 2L) {
    return(tibble::tibble(
      item1 = character(), item2 = character(),
      residual = numeric(), abs_residual = numeric()
    ))
  }
  idx <- which(lower.tri(residual_matrix), arr.ind = TRUE)
  values <- residual_matrix[idx]
  item1 <- if (!is.null(rownames(residual_matrix))) rownames(residual_matrix)[idx[, 1L]] else paste0("V", idx[, 1L])
  item2 <- if (!is.null(colnames(residual_matrix))) colnames(residual_matrix)[idx[, 2L]] else paste0("V", idx[, 2L])
  out <- tibble::tibble(
    item1 = item1,
    item2 = item2,
    residual = as.numeric(values),
    abs_residual = abs(as.numeric(values))
  )
  out <- out[is.finite(out$abs_residual), , drop = FALSE]
  out[order(out$abs_residual, decreasing = TRUE), , drop = FALSE]
}


nomo_cfa_decision_log <- function(estimator_label, engine_estimator,
                                  estimator_source, ordered, missing, control = NULL,
                                  data_n, n_used, converged, warnings, fit_evidence,
                                  loadings, heywood, residual_pairs,
                                  modification_indices,
                                  modification_indices_requested,
                                  mi_error) {
  log <- nomo_log_new()
  estimator_observation <- switch(
    estimator_source,
    ordered_default = paste0(
      "Ordered indicators were declared, so WLSMV was requested explicitly. ",
      "lavaan may report DWLS as the parameter-estimation engine because ",
      "WLSMV uses diagonal weighting for estimation with robust test/SE corrections."
    ),
    lavaan_default = "No estimator was forced by nomologR; lavaan's continuous-data default was retained.",
    researcher = "The researcher supplied the estimator explicitly."
  )

  log <- nomo_log_add(
    log, stage = "cfa", object = "model", metric = "estimator",
    value = NA_real_,
    reference = "Estimator choice should match indicator type and inferential goals",
    severity = "info",
    observation = paste0(
      "Reported estimator: ", ifelse(is.na(estimator_label), "unavailable", estimator_label),
      "; engine estimator: ", ifelse(is.na(engine_estimator), "unavailable", engine_estimator),
      ". ", estimator_observation
    ),
    recommendation = paste(
      "Confirm that estimator assumptions match the indicators, missingness,",
      "and intended inference."
    )
  )

  log <- nomo_log_add(
    log, stage = "cfa",
    object = if (length(ordered)) paste(ordered, collapse = ", ") else "model",
    metric = "ordered_indicators", value = length(ordered),
    reference = "Binary/ordinal outcomes should be declared rather than inferred solely from integer storage",
    severity = "info",
    observation = if (length(ordered)) {
      sprintf("%d indicator(s) were declared ordered.", length(ordered))
    } else "No indicators were explicitly declared ordered.",
    recommendation = "Confirm the measurement level of all indicators before interpreting the model."
  )

  if (!is.null(missing)) {
    log <- nomo_log_add(
      log, stage = "cfa", object = "sample", metric = "missing_data_option",
      value = NA_real_, reference = "Missing-data handling is part of the model specification",
      severity = "info", observation = paste("lavaan missing-data option:", missing),
      recommendation = "Document the missingness assumptions and estimator compatibility."
    )
  }

  if (!is.null(control)) {
    control_text <- paste(
      paste0(names(control), "=", vapply(control, function(x) {
        paste(as.character(x), collapse = ",")
      }, character(1))),
      collapse = "; "
    )
    log <- nomo_log_add(
      log, stage = "cfa", object = "optimizer", metric = "optimizer_control",
      value = length(control),
      reference = "Non-default optimizer controls should be explicit and reproducible",
      severity = "review",
      observation = paste("Researcher-supplied lavaan optimizer control:", control_text),
      recommendation = paste(
        "Use optimizer controls for transparent troubleshooting, not to force",
        "a substantively questionable model to appear acceptable."
      )
    )
  }

  if (is.finite(n_used)) {
    n_dropped <- max(0, data_n - n_used)
    log <- nomo_log_add(
      log, stage = "cfa", object = "sample", metric = "cases_used",
      value = n_used,
      reference = "Case retention should remain visible because missing-data handling can change the analyzed sample",
      severity = if (n_dropped > 0) "review" else "info",
      observation = if (n_dropped > 0) {
        sprintf(
          "%d of %d input cases were used; %d case(s) were not used by the fitted model.",
          as.integer(n_used), as.integer(data_n), as.integer(n_dropped)
        )
      } else {
        sprintf("All %d input cases were used by the fitted model.", as.integer(data_n))
      },
      recommendation = if (n_dropped > 0) {
        paste(
          "Confirm that case loss follows the intended missing-data strategy",
          "and assess whether the analyzed sample differs meaningfully from the input sample."
        )
      } else {
        "Document the fitted model's sample and missing-data strategy."
      }
    )
  }

  log <- nomo_log_add(
    log, stage = "cfa", object = "model", metric = "convergence",
    value = as.numeric(converged),
    reference = "A converged admissible solution is required before substantive interpretation",
    severity = if (converged) "info" else "concern",
    observation = if (converged) "lavaan reported convergence." else "lavaan did not report convergence; fit and parameter summaries are not trustworthy.",
    recommendation = if (converged) {
      "Continue to global/local fit and parameter diagnostics."
    } else {
      "Inspect model identification, starting values, data quality, and the underlying lavaan fit before interpreting results."
    }
  )

  if (length(warnings)) {
    for (warning_text in warnings) {
      log <- nomo_log_add(
        log, stage = "cfa", object = "engine", metric = "engine_warning",
        value = NA_real_, reference = "Engine warnings must remain visible",
        severity = "review", observation = warning_text,
        recommendation = "Inspect the warning in context before interpreting model fit."
      )
    }
  }

  if (inherits(fit_evidence, "data.frame") && nrow(fit_evidence)) {
    reviews <- fit_evidence$attention == "review"
    if (any(reviews)) {
      for (i in which(reviews)) {
        row <- fit_evidence[i, , drop = FALSE]
        log <- nomo_log_add(
          log, stage = "cfa", object = "model",
          metric = paste0("fit_", tolower(row$metric)), value = row$value,
          reference = paste("Configured teaching reference:", format(row$reference, trim = TRUE)),
          severity = "review", observation = row$explanation,
          recommendation = paste(
            "Inspect estimator, sample size, localized residuals, parameter",
            "estimates, and theoretical coherence rather than using a single cutoff."
          )
        )
      }
    }

    df_rows <- fit_evidence[
      fit_evidence$metric == "df" & is.finite(fit_evidence$value),
      , drop = FALSE
    ]
    if (nrow(df_rows) && df_rows$value[[1L]] <= 0) {
      log <- nomo_log_add(
        log, stage = "cfa", object = "model", metric = "degrees_of_freedom",
        value = df_rows$value[[1L]],
        reference = "Global fit evidence requires an overidentified model with positive degrees of freedom",
        severity = "review",
        observation = paste(
          "The model has non-positive degrees of freedom; ordinary global-fit",
          "indices cannot provide a meaningful test of model restriction."
        ),
        recommendation = paste(
          "Inspect model identification and whether the specified measurement",
          "model is empirically testable before interpreting global fit."
        )
      )
    }
  }

  if (inherits(loadings, "data.frame") && nrow(loadings)) {
    flagged <- loadings$attention != "KEEP"
    if (any(flagged)) {
      for (i in which(flagged)) {
        row <- loadings[i, , drop = FALSE]
        log <- nomo_log_add(
          log, stage = "cfa", object = row$item, metric = "standardized_loading",
          value = row$loading,
          reference = "Configured loading reference is a review prompt, not a deletion rule",
          severity = if (row$attention == "STRONG REVIEW") "concern" else "review",
          observation = row$explanation,
          recommendation = paste(
            "Interpret the loading with item content, uncertainty, residuals,",
            "factor definition, and independent validation."
          )
        )
      }
    }
  }

  if (inherits(heywood, "data.frame") && nrow(heywood)) {
    for (i in seq_len(nrow(heywood))) {
      row <- heywood[i, , drop = FALSE]
      log <- nomo_log_add(
        log, stage = "cfa", object = row$object, metric = row$issue,
        value = row$value,
        reference = "Proper CFA solutions should not contain inadmissible variances, loadings, or latent correlations",
        severity = "concern", observation = row$explanation,
        recommendation = paste(
          "Do not repair the model mechanically; inspect identification,",
          "sampling instability, data quality, and substantive misspecification."
        )
      )
    }
  }

  if (inherits(residual_pairs, "data.frame") && nrow(residual_pairs)) {
    log <- nomo_log_add(
      log, stage = "cfa",
      object = paste0(residual_pairs$item1[[1L]], " ~ ", residual_pairs$item2[[1L]]),
      metric = "largest_residual_correlation",
      value = residual_pairs$residual[[1L]],
      reference = "Localized residuals identify where model-implied relationships miss observed relationships",
      severity = "info",
      observation = paste("Largest absolute residual correlation:", format(residual_pairs$abs_residual[[1L]], digits = 3)),
      recommendation = paste(
        "Inspect localized strain substantively; a residual does not by itself",
        "authorize a correlated error or cross-loading."
      )
    )
  }

  if (modification_indices_requested) {
    log <- nomo_log_add(
      log, stage = "cfa", object = "model", metric = "modification_indices",
      value = nrow(modification_indices),
      reference = "Modification indices are post-hoc diagnostics, not respecification instructions",
      severity = "info",
      observation = if (is.null(mi_error)) {
        sprintf("%d modification-index candidate(s) were retained for inspection.", nrow(modification_indices))
      } else paste("Modification indices were unavailable:", mi_error),
      recommendation = paste(
        "Treat large indices as hypotheses about localized strain; require",
        "substantive justification and ideally independent confirmation before changing the model."
      )
    )
  }

  log <- nomo_log_add(
    log, stage = "cfa", object = "model", metric = "automatic_respecification",
    value = 0,
    reference = "nomologR does not alter researcher-specified CFA models automatically",
    severity = "info",
    observation = "No parameters were freed and no alternative model was fit automatically.",
    recommendation = paste(
      "Any model change should be researcher-specified, documented, and",
      "distinguished as confirmatory or post hoc."
    )
  )
  log
}
