#' Guided exploratory factor analysis
#'
#' `nomo_efa()` estimates a common-factor exploratory factor analysis while
#' keeping the consequential analytical choices visible. Oblique rotation is
#' the default, item-level diagnostics are framed as prompts for inspection,
#' and no item is automatically deleted or model silently refit.
#'
#' The factor count may be supplied directly as a positive integer or by passing
#' a `nomo_factors` object. When a `nomo_factors` object is supplied, its primary
#' parallel-analysis suggestion is used as the requested EFA factor count and,
#' unless overridden, its item set, modeling types, correlation model, and
#' missing-data strategy are carried forward.
#'
#' @param data A data frame containing candidate items.
#' @param items Optional character vector identifying item columns. If `factors`
#'   is a `nomo_factors` object and `items = NULL`, its item set is inherited.
#'   Otherwise all columns are used when `items = NULL`.
#' @param factors A positive integer number of factors to extract, or a
#'   `nomo_factors` object. For a `nomo_factors` object, the selected
#'   parallel-analysis factor count is used.
#' @param rotation Rotation passed to [psych::fa()]. The default is `"oblimin"`.
#'   Orthogonal rotations are allowed but are recorded as a researcher choice.
#' @param fm Common-factor extraction method passed to [psych::fa()]. The
#'   default is `"minres"`. Supported values are `"minres"`, `"uls"`, `"ols"`,
#'   `"wls"`, `"gls"`, `"pa"`, `"ml"`, `"minchi"`, `"minrank"`, `"alpha"`,
#'   and `"old.min"`.
#' @param correlation Optional correlation strategy: `"auto"`, `"pearson"`,
#'   `"polychoric"`, `"tetrachoric"`, or `"mixed"`. If `NULL`, the value is
#'   inherited from a supplied `nomo_factors` object when possible; otherwise
#'   `"auto"` is used.
#' @param types Optional named character vector declaring selected items as
#'   `"continuous"`, `"ordinal"`, or `"binary"`. If `NULL`, modeling types are
#'   inherited from a supplied `nomo_factors` object when possible; otherwise
#'   they are inferred using the same conservative rules as [nomo_factors()].
#' @param missing Optional missing-data strategy: `"pairwise"` or `"complete"`.
#'   If `NULL`, the value is inherited from a supplied `nomo_factors` object
#'   when possible; otherwise `"pairwise"` is used.
#' @param smooth Optional logical. If `NULL` (default), smoothing is inherited
#'   from a supplied `nomo_factors` object when that object explicitly used
#'   smoothing; otherwise `FALSE`. If `FALSE`, a non-positive-definite
#'   correlation matrix stops the analysis. If `TRUE`, [psych::cor.smooth()] is
#'   used explicitly and the intervention is recorded.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return An object of class `nomo_efa` containing the fitted EFA, tidy pattern
#'   and structure matrices, factor correlations, item diagnostics,
#'   communalities/uniquenesses, residual diagnostics, adequacy information,
#'   and a decision log.
#'
#' @examples
#' set.seed(2026)
#' f1 <- rnorm(250)
#' f2 <- 0.35 * f1 + sqrt(1 - 0.35^2) * rnorm(250)
#' dat <- data.frame(
#'   a1 = .75 * f1 + rnorm(250, sd = .65),
#'   a2 = .70 * f1 + rnorm(250, sd = .70),
#'   a3 = .80 * f1 + rnorm(250, sd = .60),
#'   a4 = .72 * f1 + rnorm(250, sd = .68),
#'   b1 = .75 * f2 + rnorm(250, sd = .65),
#'   b2 = .70 * f2 + rnorm(250, sd = .70),
#'   b3 = .80 * f2 + rnorm(250, sd = .60),
#'   b4 = .72 * f2 + rnorm(250, sd = .68)
#' )
#' efa <- nomo_efa(dat, factors = 2)
#' efa
#' summary(efa)
#'
#' @export
nomo_efa <- function(data,
                     items = NULL,
                     factors,
                     rotation = "oblimin",
                     fm = "minres",
                     correlation = NULL,
                     types = NULL,
                     missing = NULL,
                     smooth = NULL,
                     guidance = nomo_defaults()) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0L || ncol(data) == 0L) {
    stop("`data` must contain at least one row and one column.", call. = FALSE)
  }
  if (!is.list(guidance)) {
    stop(
      "`guidance` must be a list, typically returned by `nomo_defaults()`.",
      call. = FALSE
    )
  }
  if (!is.character(rotation) || length(rotation) != 1L ||
      is.na(rotation) || rotation == "") {
    stop("`rotation` must be a single non-empty character value.", call. = FALSE)
  }
  if (!is.character(fm) || length(fm) != 1L || is.na(fm) || fm == "") {
    stop("`fm` must be a single non-empty character value.", call. = FALSE)
  }
  supported_fm <- c(
    "minres", "uls", "ols", "wls", "gls", "pa",
    "ml", "minchi", "minrank", "alpha", "old.min"
  )
  if (!fm %in% supported_fm) {
    stop(
      sprintf(
        paste0(
          "Unsupported extraction method `fm = \"%s\"`. ",
          "Use one of: %s."
        ),
        fm,
        paste(supported_fm, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!is.null(smooth) &&
      (!is.logical(smooth) || length(smooth) != 1L || is.na(smooth))) {
    stop("`smooth` must be `NULL`, `TRUE`, or `FALSE`.", call. = FALSE)
  }

  types_supplied_by_user <- !is.null(types)
  types_inherited_from_factors <- FALSE

  inherited <- inherits(factors, "nomo_factors")
  factor_source <- if (inherited) "nomo_factors" else "researcher"
  factor_context <- NULL

  if (inherited) {
    factor_object <- factors
    k <- factor_object$parallel$n_factors
    if (is.null(k) || length(k) != 1L || is.na(k) || k < 1L) {
      stop(
        paste(
          "The supplied `nomo_factors` object does not contain a positive",
          "parallel-analysis factor suggestion. Supply `factors` as a positive integer."
        ),
        call. = FALSE
      )
    }

    if (is.null(items)) {
      items <- factor_object$items
    }
    if (is.null(correlation)) {
      correlation <- factor_object$correlation
    }
    if (is.null(missing)) {
      missing <- factor_object$missing
    }
    if (is.null(types) && !is.null(factor_object$modeling_types)) {
      inherited_types <- stats::setNames(
        factor_object$modeling_types$model_type,
        factor_object$modeling_types$item
      )
      types <- inherited_types[names(inherited_types) %in% items]
      types_inherited_from_factors <- TRUE
    }
    if (is.null(smooth)) {
      smooth <- isTRUE(factor_object$smoothed)
    }

    plausible <- suppressWarnings(
      as.integer(unlist(factor_object$plausible_factors, use.names = FALSE))
    )
    plausible <- sort(unique(plausible[is.finite(plausible) & plausible >= 1L]))
    factor_context <- list(
      primary_parallel = as.integer(k),
      plausible_factors = plausible,
      recommendation = if (is.null(factor_object$recommendation)) {
        ""
      } else {
        as.character(factor_object$recommendation)
      }
    )
  } else {
    k <- factors
  }

  if (is.null(smooth)) {
    smooth <- FALSE
  }

  if (length(k) != 1L || is.na(k) || !is.numeric(k) ||
      k < 1 || abs(k - round(k)) > sqrt(.Machine$double.eps)) {
    stop("`factors` must be a positive integer or a `nomo_factors` object.", call. = FALSE)
  }
  k <- as.integer(k)

  if (is.null(items)) {
    items <- names(data)
  }
  if (!is.character(items) || length(items) < 3L ||
      anyNA(items) || any(items == "") || anyDuplicated(items)) {
    stop(
      "`items` must identify at least three unique, non-missing item columns.",
      call. = FALSE
    )
  }
  missing_items <- setdiff(items, names(data))
  if (length(missing_items)) {
    stop(
      sprintf("Unknown item column%s: %s.",
              if (length(missing_items) == 1L) "" else "s",
              paste(missing_items, collapse = ", ")),
      call. = FALSE
    )
  }
  if (k >= length(items)) {
    stop("`factors` must be smaller than the number of analyzed items.", call. = FALSE)
  }

  correlation <- if (is.null(correlation)) "auto" else correlation
  correlation <- match.arg(
    correlation,
    choices = c("auto", "pearson", "polychoric", "tetrachoric", "mixed")
  )
  missing <- if (is.null(missing)) "pairwise" else missing
  missing <- match.arg(missing, choices = c("pairwise", "complete"))

  selected <- data[items]

  hard_bad <- vapply(
    selected,
    function(x) {
      observed <- x[!is.na(x)]
      length(observed) == 0L || length(unique(observed)) < 2L
    },
    logical(1)
  )
  if (any(hard_bad)) {
    stop(
      sprintf(
        "EFA requires nonconstant observed data. Inspect: %s.",
        paste(items[hard_bad], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  screen_types <- vapply(selected, nomo_screen_item_type, character(1))
  item_types <- nomo_factors_model_types(
    selected = selected,
    items = items,
    screen_types = screen_types,
    types = types
  )

  if (types_inherited_from_factors && !types_supplied_by_user) {
    inherited_rows <- item_types$source == "user_override"
    item_types$source[inherited_rows] <- "inherited_from_nomo_factors"
  }

  analysis_data <- nomo_factors_numeric_data(selected, item_types)

  if (missing == "complete") {
    complete <- stats::complete.cases(analysis_data)
    analysis_data <- analysis_data[complete, , drop = FALSE]
    if (nrow(analysis_data) < max(3L, k + 1L)) {
      stop(
        "Too few complete cases remain for the requested EFA.",
        call. = FALSE
      )
    }
  }

  pairwise_n <- nomo_factors_pairwise_n(analysis_data)
  off_diag_n <- pairwise_n[row(pairwise_n) != col(pairwise_n)]
  min_pairwise_n <- if (length(off_diag_n)) min(off_diag_n) else nrow(analysis_data)
  if (min_pairwise_n < 3L) {
    stop(
      "At least one item pair has fewer than three jointly observed cases.",
      call. = FALSE
    )
  }

  correlation_method <- nomo_factors_choose_correlation(
    requested = correlation,
    model_types = item_types$model_type
  )
  corr <- suppressWarnings(
    nomo_factors_correlation(
      x = analysis_data,
      model_types = item_types$model_type,
      method = correlation_method,
      use = if (missing == "pairwise") "pairwise" else "complete"
    )
  )

  if (any(!is.finite(corr))) {
    stop(
      paste(
        "The estimated correlation matrix contains non-finite values.",
        "Inspect sparse categories, missingness, or coding before EFA."
      ),
      call. = FALSE
    )
  }

  original_min_eigen <- min(
    eigen(corr, symmetric = TRUE, only.values = TRUE)$values
  )
  smoothed <- FALSE
  if (original_min_eigen <= 1e-08) {
    if (!smooth) {
      stop(
        paste(
          "The observed correlation matrix is not positive definite.",
          "Inspect redundant items, sparse categories, missingness, or coding.",
          "If smoothing is substantively justified, rerun with `smooth = TRUE`."
        ),
        call. = FALSE
      )
    }
    corr <- suppressWarnings(psych::cor.smooth(corr))
    smoothed <- TRUE
  }

  common_n_available <- missing == "complete" || !anyNA(analysis_data)
  fa_args <- list(
    r = corr,
    nfactors = k,
    rotate = rotation,
    fm = fm,
    residuals = TRUE,
    warnings = FALSE
  )
  if (common_n_available) {
    fa_args$n.obs <- nrow(analysis_data)
  }

  fit <- tryCatch(
    suppressWarnings(
      suppressMessages(
        do.call(psych::fa, fa_args)
      )
    ),
    error = function(e) {
      stop(
        sprintf("EFA estimation failed: %s", conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  pattern <- unclass(fit$loadings)
  pattern <- as.matrix(pattern)
  rownames(pattern) <- items

  # Keep engine-specific factor labels (for example, MR1/MR2) inside `fit`.
  # Public nomologR outputs use neutral F1...Fk labels without reordering or
  # flipping the solution, so presentation does not leak extraction-engine names.
  factor_names <- paste0("F", seq_len(ncol(pattern)))
  colnames(pattern) <- factor_names

  phi <- fit$Phi
  if (is.null(phi)) {
    phi <- diag(ncol(pattern))
    dimnames(phi) <- list(colnames(pattern), colnames(pattern))
    oblique <- FALSE
  } else {
    phi <- as.matrix(phi)
    dimnames(phi) <- list(colnames(pattern), colnames(pattern))
    oblique <- TRUE
  }

  structure <- if (!is.null(fit$Structure)) {
    as.matrix(unclass(fit$Structure))
  } else {
    pattern %*% phi
  }
  rownames(structure) <- items
  colnames(structure) <- colnames(pattern)

  h2 <- if (!is.null(fit$communality)) {
    as.numeric(fit$communality)
  } else if (!is.null(fit$communalities)) {
    as.numeric(fit$communalities)
  } else {
    rowSums((pattern %*% phi) * pattern)
  }
  uniqueness <- if (!is.null(fit$uniquenesses)) {
    as.numeric(fit$uniquenesses)
  } else {
    1 - h2
  }
  complexity <- if (!is.null(fit$complexity)) {
    as.numeric(fit$complexity)
  } else {
    nomo_efa_complexity(pattern)
  }

  item_summary <- nomo_efa_item_summary(
    pattern = pattern,
    communality = h2,
    uniqueness = uniqueness,
    complexity = complexity,
    guidance = guidance
  )

  reproduced <- pattern %*% phi %*% t(pattern)
  dimnames(reproduced) <- dimnames(corr)
  residual <- corr - reproduced
  diag(residual) <- 0
  off_diag <- residual[row(residual) != col(residual)]
  rmsr <- sqrt(mean(off_diag^2))

  residual_pairs <- nomo_efa_residual_pairs(residual)

  kmo <- nomo_factors_kmo(corr, items)
  bartlett <- nomo_factors_bartlett(
    corr = corr,
    n = nrow(analysis_data),
    available = common_n_available
  )

  extraction_note <- nomo_efa_extraction_note(
    fm = fm,
    correlation_method = correlation_method
  )
  sample_adequacy <- list(
    n_cases = nrow(analysis_data),
    n_items = length(items),
    cases_per_item = nrow(analysis_data) / length(items),
    min_pairwise_n = min_pairwise_n,
    kmo = kmo,
    bartlett = bartlett
  )

  decision_log <- nomo_efa_log(
    k = k,
    factor_source = factor_source,
    factor_context = factor_context,
    rotation = rotation,
    fm = fm,
    extraction_note = extraction_note,
    correlation_method = correlation_method,
    item_types = item_types,
    missing = missing,
    min_pairwise_n = min_pairwise_n,
    smoothed = smoothed,
    original_min_eigen = original_min_eigen,
    item_summary = item_summary,
    rmsr = rmsr,
    kmo = kmo,
    n_cases = nrow(analysis_data),
    n_items = length(items),
    guidance = guidance
  )

  out <- list(
    call = match.call(),
    n_cases = nrow(analysis_data),
    n_items = length(items),
    items = items,
    n_factors = k,
    factor_source = factor_source,
    factor_context = factor_context,
    rotation = rotation,
    oblique = oblique,
    fm = fm,
    extraction_note = extraction_note,
    item_types = item_types,
    modeling_types = item_types,
    correlation_requested = correlation,
    correlation_method = correlation_method,
    correlation = correlation_method,
    correlation_matrix = corr,
    pairwise_n = pairwise_n,
    min_pairwise_n = min_pairwise_n,
    missing = missing,
    smoothed = smoothed,
    original_min_eigen = original_min_eigen,
    fit = fit,
    pattern_matrix = pattern,
    structure_matrix = structure,
    factor_correlations = phi,
    communalities = stats::setNames(h2, items),
    uniquenesses = stats::setNames(uniqueness, items),
    complexity = stats::setNames(complexity, items),
    item_summary = item_summary,
    reproduced_correlations = reproduced,
    residual_matrix = residual,
    residual_pairs = residual_pairs,
    rmsr = rmsr,
    kmo = kmo,
    bartlett = bartlett,
    sample_adequacy = sample_adequacy,
    decision_log = decision_log,
    guidance = guidance
  )
  class(out) <- c("nomo_efa", "list")
  out
}


nomo_efa_extraction_note <- function(fm, correlation_method) {
  fm_lower <- tolower(fm)

  if (fm_lower %in% c("minres", "uls", "minchi")) {
    if (correlation_method %in% c("polychoric", "tetrachoric", "mixed")) {
      return(paste(
        sprintf("Extraction `%s` is applied to a %s correlation matrix.", fm, correlation_method),
        "This is a common-factor solution and avoids treating numeric category codes as",
        "continuous Pearson indicators, but the extraction label itself should not be",
        "described as an ordinal-data estimator."
      ))
    }
    return(paste(
      sprintf("Extraction `%s` is a common-factor method suitable for exploratory structure.", fm),
      "Estimator choice remains a researcher decision rather than an automatic normality rule."
    ))
  }

  if (fm_lower == "ml") {
    return(paste(
      "Maximum-likelihood extraction was researcher-selected.",
      "Likelihood-based interpretation should be considered alongside distributional",
      "assumptions and the correlation model."
    ))
  }

  paste(
    sprintf("Extraction `%s` was researcher-selected.", fm),
    "Document why this common-factor extraction method is appropriate for the indicators."
  )
}


nomo_efa_complexity <- function(pattern) {
  sq <- pattern^2
  numerator <- rowSums(sq)^2
  denominator <- rowSums(sq^2)
  out <- numerator / denominator
  out[!is.finite(out)] <- NA_real_
  out
}


nomo_efa_item_summary <- function(pattern,
                                  communality,
                                  uniqueness,
                                  complexity,
                                  guidance) {
  load_ref <- guidance$efa_loading_reference
  cross_ref <- guidance$efa_crossloading_reference
  communality_ref <- guidance$efa_communality_reference

  if (is.null(load_ref)) load_ref <- 0.40
  if (is.null(cross_ref)) cross_ref <- 0.30
  if (is.null(communality_ref)) communality_ref <- 0.40

  rows <- lapply(seq_len(nrow(pattern)), function(i) {
    vals <- pattern[i, ]
    ord <- order(abs(vals), decreasing = TRUE)
    primary_idx <- ord[[1L]]
    secondary_idx <- if (length(ord) >= 2L) ord[[2L]] else NA_integer_
    primary <- vals[[primary_idx]]
    secondary <- if (is.na(secondary_idx)) NA_real_ else vals[[secondary_idx]]

    weak_primary <- abs(primary) < load_ref
    cross_loading <- !is.na(secondary) && abs(secondary) >= cross_ref
    low_communality <- communality[[i]] < communality_ref
    flags <- c(weak_primary, cross_loading, low_communality)
    n_flags <- sum(flags, na.rm = TRUE)

    attention <- if (n_flags >= 2L) {
      "STRONG REVIEW"
    } else if (n_flags == 1L) {
      "REVIEW"
    } else {
      "KEEP"
    }

    reasons <- character()
    if (weak_primary) {
      reasons <- c(
        reasons,
        sprintf(
          "primary loading |%.2f| is below the %.2f teaching reference",
          primary, load_ref
        )
      )
    }
    if (cross_loading) {
      reasons <- c(
        reasons,
        sprintf(
          "secondary loading |%.2f| meets/exceeds the %.2f cross-loading reference",
          secondary, cross_ref
        )
      )
    }
    if (low_communality) {
      reasons <- c(
        reasons,
        sprintf(
          "communality %.2f is below the %.2f teaching reference",
          communality[[i]], communality_ref
        )
      )
    }
    if (!length(reasons)) {
      reasons <- "no numeric teaching-reference flags; retain substantive review"
    }

    tibble::tibble(
      item = rownames(pattern)[[i]],
      primary_factor = colnames(pattern)[[primary_idx]],
      primary_loading = as.numeric(primary),
      secondary_factor = if (is.na(secondary_idx)) NA_character_ else colnames(pattern)[[secondary_idx]],
      secondary_loading = as.numeric(secondary),
      loading_gap = if (is.na(secondary)) NA_real_ else abs(primary) - abs(secondary),
      communality = as.numeric(communality[[i]]),
      uniqueness = as.numeric(uniqueness[[i]]),
      complexity = as.numeric(complexity[[i]]),
      weak_primary = weak_primary,
      cross_loading = cross_loading,
      low_communality = low_communality,
      attention = attention,
      explanation = paste(reasons, collapse = "; ")
    )
  })

  dplyr::bind_rows(rows)
}


nomo_efa_residual_pairs <- function(residual) {
  p <- ncol(residual)
  if (p < 2L) {
    return(
      tibble::tibble(
        item1 = character(),
        item2 = character(),
        residual = numeric(),
        abs_residual = numeric()
      )
    )
  }

  idx <- which(upper.tri(residual), arr.ind = TRUE)
  residual_values <- residual[idx]

  out <- tibble::tibble(
    item1 = rownames(residual)[idx[, 1]],
    item2 = colnames(residual)[idx[, 2]],
    residual = as.numeric(residual_values),
    abs_residual = abs(as.numeric(residual_values))
  )
  out[order(out$abs_residual, decreasing = TRUE), , drop = FALSE]
}


nomo_efa_log <- function(k,
                         factor_source,
                         factor_context,
                         rotation,
                         fm,
                         extraction_note,
                         correlation_method,
                         item_types,
                         missing,
                         min_pairwise_n,
                         smoothed,
                         original_min_eigen,
                         item_summary,
                         rmsr,
                         kmo,
                         n_cases,
                         n_items,
                         guidance) {
  log <- nomo_log_new()

  log <- nomo_log_add(
    log,
    stage = "efa",
    object = "model",
    metric = "n_factors",
    value = k,
    reference = "Factor count is researcher controlled",
    severity = "info",
    observation = sprintf(
      "%d-factor EFA requested via %s.",
      k,
      if (factor_source == "nomo_factors") "nomo_factors() handoff" else "direct specification"
    ),
    recommendation = "Compare neighboring plausible solutions when retention evidence is ambiguous."
  )

  if (!is.null(factor_context) &&
      length(factor_context$plausible_factors) > 0L &&
      any(factor_context$plausible_factors != k)) {
    log <- nomo_log_add(
      log,
      stage = "efa",
      object = "model",
      metric = "retention_ambiguity",
      value = k,
      reference = "Retention evidence identifies solutions to investigate; it does not prove dimensionality",
      severity = "review",
      observation = sprintf(
        "The M2 handoff selected %d factor%s for this EFA, while the broader plausible set includes: %s.",
        k,
        if (k == 1L) "" else "s",
        paste(factor_context$plausible_factors, collapse = ", ")
      ),
      recommendation = "Fit and compare substantively plausible neighboring EFA solutions rather than treating the handoff as proof."
    )
  }

  log <- nomo_log_add(
    log,
    stage = "efa",
    object = "model",
    metric = "extraction_rotation",
    value = NA_real_,
    reference = "Common-factor extraction; oblique rotation is the default",
    severity = if (tolower(rotation) %in% c("varimax", "quartimax", "equamax")) "review" else "info",
    observation = paste(
      sprintf("Extraction = %s; rotation = %s.", fm, rotation),
      extraction_note
    ),
    recommendation = if (tolower(rotation) %in% c("varimax", "quartimax", "equamax")) {
      "Orthogonal rotation was researcher-selected; justify the assumption that factors are uncorrelated."
    } else {
      "Interpret the pattern matrix first and examine factor correlations."
    }
  )

  log <- nomo_log_add(
    log,
    stage = "efa",
    object = "correlation_matrix",
    metric = "correlation_method",
    value = NA_real_,
    reference = "Indicator type should inform the correlation model",
    severity = "info",
    observation = sprintf(
      "Correlation method = %s; missing-data handling = %s; minimum pairwise N = %d.",
      correlation_method, missing, min_pairwise_n
    ),
    recommendation = "Confirm modeling types and missing-data handling are substantively appropriate."
  )

  inherited_types <- item_types$item[
    item_types$source == "inherited_from_nomo_factors"
  ]
  if (length(inherited_types)) {
    log <- nomo_log_add(
      log,
      stage = "efa",
      object = paste(inherited_types, collapse = ", "),
      metric = "modeling_types_inherited",
      value = length(inherited_types),
      reference = "Carry forward explicit M2 modeling decisions unless the researcher overrides them",
      severity = "info",
      observation = paste(
        "Modeling types were inherited from the supplied `nomo_factors` object for:",
        paste(inherited_types, collapse = ", ")
      ),
      recommendation = "Confirm that the M2 item set and measurement-level decisions remain appropriate for this EFA."
    )
  }

  overrides <- item_types$item[item_types$source == "user_override"]
  if (length(overrides)) {
    log <- nomo_log_add(
      log,
      stage = "efa",
      object = paste(overrides, collapse = ", "),
      metric = "modeling_type_override",
      value = length(overrides),
      reference = "Researcher-specified modeling interpretation",
      severity = "info",
      observation = paste(
        "Explicit modeling-type overrides were used for:",
        paste(overrides, collapse = ", ")
      ),
      recommendation = "Document why these indicators were treated at the declared measurement level."
    )
  }

  if (smoothed) {
    log <- nomo_log_add(
      log,
      stage = "efa",
      object = "correlation_matrix",
      metric = "smoothing",
      value = original_min_eigen,
      reference = "No silent positive-definite repair",
      severity = "review",
      observation = "The observed correlation matrix was explicitly smoothed before EFA.",
      recommendation = "Report the smoothing and investigate redundancy, sparse categories, missingness, or coding."
    )
  }

  for (i in seq_len(nrow(item_summary))) {
    row <- item_summary[i, ]
    if (row$attention == "KEEP") next

    log <- nomo_log_add(
      log,
      stage = "efa",
      object = row$item,
      metric = "item_structure_review",
      value = abs(row$primary_loading),
      reference = sprintf(
        "loading %.2f; cross-loading %.2f; communality %.2f are teaching references",
        guidance$efa_loading_reference,
        guidance$efa_crossloading_reference,
        guidance$efa_communality_reference
      ),
      severity = if (row$attention == "STRONG REVIEW") "concern" else "review",
      observation = row$explanation,
      recommendation = paste(
        "Inspect theory/content coverage, wording, redundancy, cross-loading,",
        "communality, and factor interpretability. Do not delete automatically."
      )
    )
  }

  log <- nomo_log_add(
    log,
    stage = "efa",
    object = "model",
    metric = "rmsr",
    value = rmsr,
    reference = "Residual size is descriptive evidence, not a binary fit rule",
    severity = "info",
    observation = sprintf("Off-diagonal RMSR = %.3f.", rmsr),
    recommendation = "Inspect the largest localized residuals alongside the loading pattern."
  )

  if (isTRUE(kmo$available)) {
    kmo_severity <- if (kmo$overall < guidance$factor_kmo_concern_reference) {
      "concern"
    } else if (kmo$overall < guidance$factor_kmo_review_reference) {
      "review"
    } else {
      "info"
    }
    log <- nomo_log_add(
      log,
      stage = "efa",
      object = "sample",
      metric = "kmo",
      value = kmo$overall,
      reference = "KMO is supporting adequacy evidence, not an item-retention rule",
      severity = kmo_severity,
      observation = sprintf("Overall KMO = %.3f.", kmo$overall),
      recommendation = "Use KMO with the broader structural evidence rather than as a stand-alone gate."
    )
  }

  small_n_ref <- guidance$factor_small_n_reference
  if (is.null(small_n_ref)) small_n_ref <- 100L
  if (n_cases < small_n_ref) {
    log <- nomo_log_add(
      log,
      stage = "efa",
      object = "sample",
      metric = "sample_size",
      value = n_cases,
      reference = sprintf("%d cases is a teaching review reference, not a universal minimum", small_n_ref),
      severity = "review",
      observation = sprintf(
        "%d cases were analyzed for %d items (%.1f cases per item).",
        n_cases, n_items, n_cases / n_items
      ),
      recommendation = paste(
        "Evaluate sample adequacy jointly with communalities, loading magnitude,",
        "factor overdetermination, KMO, convergence, and stability; do not use a",
        "single cases-per-item rule as a pass/fail threshold."
      )
    )
  }

  log
}
