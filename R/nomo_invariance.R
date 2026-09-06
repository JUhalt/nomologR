# Measurement invariance -------------------------------------------------------

nomo_invariance_sequences <- function(ordered = character(),
                                      category_table = NULL) {
  if (!length(ordered)) {
    return(list(
      sequence = c("configural", "metric", "scalar", "strict"),
      constraints = list(
        configural = character(),
        metric = "loadings",
        scalar = c("loadings", "intercepts"),
        strict = c("loadings", "intercepts", "residuals")
      ),
      type = "continuous",
      identification_note = paste(
        "Continuous indicators use the conventional configural, metric,",
        "scalar, and strict sequence."
      )
    ))
  }

  if (is.null(category_table) || !nrow(category_table)) {
    stop("Ordered indicators require observed category information.", call. = FALSE)
  }

  counts <- category_table$categories

  if (any(counts == 2L)) {
    return(list(
      sequence = c("configural", "strong", "strict"),
      constraints = list(
        configural = character(),
        strong = c("thresholds", "loadings", "intercepts"),
        strict = c("thresholds", "loadings", "intercepts", "residuals")
      ),
      type = if (all(counts == 2L)) {
        "ordered_binary"
      } else {
        "ordered_with_binary"
      },
      identification_note = paste(
        "At least one binary indicator is present. Under Wu-Estabrook",
        "identification, threshold, loading, and intercept restrictions cannot",
        "be treated as independent sequential tests for binary indicators, so",
        "the first equality step is labeled strong and imposes them together."
      )
    ))
  }

  if (any(counts == 3L)) {
    return(list(
      sequence = c("configural", "metric", "scalar", "strict"),
      constraints = list(
        configural = character(),
        metric = c("thresholds", "loadings"),
        scalar = c("thresholds", "loadings", "intercepts"),
        strict = c("thresholds", "loadings", "intercepts", "residuals")
      ),
      type = if (all(counts == 3L)) {
        "ordered_three_category"
      } else {
        "ordered_with_three_category"
      },
      identification_note = paste(
        "At least one three-category indicator is present. Threshold equality",
        "is required for identification and is therefore imposed together with",
        "the loading-equality step rather than presented as an independent",
        "threshold-invariance test."
      )
    ))
  }

  list(
    sequence = c("configural", "thresholds", "metric", "scalar", "strict"),
    constraints = list(
      configural = character(),
      thresholds = "thresholds",
      metric = c("thresholds", "loadings"),
      scalar = c("thresholds", "loadings", "intercepts"),
      strict = c("thresholds", "loadings", "intercepts", "residuals")
    ),
    type = "ordered_polytomous",
    identification_note = paste(
      "Threshold invariance is evaluated before loading invariance under",
      "Wu-Estabrook identification because all declared ordered indicators",
      "have at least four observed categories."
    )
  )
}


nomo_invariance_validate_levels <- function(levels, sequence) {
  if (is.null(levels)) return(sequence)

  if (!is.character(levels) || !length(levels) || anyNA(levels)) {
    stop("`levels` must be NULL or a non-empty character vector.", call. = FALSE)
  }

  levels <- tolower(trimws(levels))
  if (any(!levels %in% sequence) || anyDuplicated(levels)) {
    stop(
      sprintf(
        "`levels` may contain only: %s.",
        paste(sequence, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  expected <- sequence[seq_along(levels)]
  if (!identical(levels, expected)) {
    stop(
      paste0(
        "`levels` must form an ordered prefix of ",
        paste(sequence, collapse = " -> "),
        "."
      ),
      call. = FALSE
    )
  }

  levels
}


nomo_invariance_ordered_categories <- function(data, ordered) {
  if (!length(ordered)) {
    return(tibble::tibble(
      item = character(),
      categories = integer()
    ))
  }

  categories <- vapply(
    ordered,
    function(item) {
      x <- data[[item]]
      x <- x[!is.na(x)]
      length(unique(x))
    },
    integer(1)
  )

  tibble::tibble(
    item = ordered,
    categories = unname(categories)
  )
}


nomo_invariance_validate_ordered_categories <- function(category_table) {
  if (!nrow(category_table)) return(invisible(TRUE))

  if (any(category_table$categories < 2L)) {
    bad <- category_table$item[category_table$categories < 2L]
    stop(
      sprintf(
        "Ordered indicator(s) have fewer than two observed categories: %s.",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


nomo_invariance_first_measure <- function(measures, candidates) {
  if (!length(measures)) return(NA_real_)

  for (candidate in candidates) {
    if (candidate %in% names(measures)) {
      value <- suppressWarnings(as.numeric(measures[[candidate]])[1L])
      if (length(value) && is.finite(value)) return(value)
    }
  }

  NA_real_
}


nomo_invariance_fit_row <- function(level,
                                    constraints,
                                    fit,
                                    warnings = character(),
                                    error = NULL,
                                    partial_requested = character()) {
  partial_label <- if (length(partial_requested)) {
    paste(partial_requested, collapse = "; ")
  } else {
    ""
  }

  if (is.null(fit)) {
    return(
      tibble::tibble(
        level = level,
        constraints = if (length(constraints)) {
          paste(constraints, collapse = ", ")
        } else {
          "none"
        },
        partial_requested = partial_label,
        status = "fit_error",
        converged = FALSE,
        chisq = NA_real_,
        df = NA_real_,
        pvalue = NA_real_,
        cfi = NA_real_,
        rmsea = NA_real_,
        srmr = NA_real_,
        delta_cfi = NA_real_,
        delta_rmsea = NA_real_,
        delta_srmr = NA_real_,
        lrt_chisq = NA_real_,
        lrt_df = NA_real_,
        lrt_p = NA_real_,
        warnings = paste(warnings, collapse = " | "),
        error = if (is.null(error)) "" else as.character(error)
      )
    )
  }

  converged <- isTRUE(tryCatch(
    lavaan::lavInspect(fit, "converged"),
    error = function(e) FALSE
  ))

  measures <- tryCatch(
    lavaan::fitMeasures(fit),
    error = function(e) numeric()
  )

  tibble::tibble(
    level = level,
    constraints = if (length(constraints)) {
      paste(constraints, collapse = ", ")
    } else {
      "none"
    },
    partial_requested = partial_label,
    status = if (converged) "estimated" else "not_converged",
    converged = converged,
    chisq = nomo_invariance_first_measure(
      measures,
      c("chisq.scaled", "chisq")
    ),
    df = nomo_invariance_first_measure(
      measures,
      c("df.scaled", "df")
    ),
    pvalue = nomo_invariance_first_measure(
      measures,
      c("pvalue.scaled", "pvalue")
    ),
    cfi = nomo_invariance_first_measure(
      measures,
      c("cfi.robust", "cfi.scaled", "cfi")
    ),
    rmsea = nomo_invariance_first_measure(
      measures,
      c("rmsea.robust", "rmsea.scaled", "rmsea")
    ),
    srmr = nomo_invariance_first_measure(
      measures,
      "srmr"
    ),
    delta_cfi = NA_real_,
    delta_rmsea = NA_real_,
    delta_srmr = NA_real_,
    lrt_chisq = NA_real_,
    lrt_df = NA_real_,
    lrt_p = NA_real_,
    warnings = paste(warnings, collapse = " | "),
    error = ""
  )
}


nomo_invariance_lrt <- function(previous_fit, current_fit) {
  warnings <- character()

  tab <- tryCatch(
    withCallingHandlers(
      as.data.frame(lavaan::lavTestLRT(previous_fit, current_fit)),
      warning = function(w) {
        warnings <<- unique(c(warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )

  if (is.null(tab) || nrow(tab) < 2L) {
    return(list(
      chisq = NA_real_,
      df = NA_real_,
      p = NA_real_,
      warnings = warnings
    ))
  }

  last <- tab[nrow(tab), , drop = FALSE]
  nms <- names(last)

  pick <- function(pattern) {
    hit <- grep(pattern, nms, ignore.case = TRUE)
    if (!length(hit)) return(NA_real_)
    value <- suppressWarnings(as.numeric(last[[hit[[1L]]]])[1L])
    if (!length(value) || !is.finite(value)) NA_real_ else value
  }

  list(
    chisq = pick("chisq.*diff|diff.*chisq"),
    df = pick("^df.*diff|diff.*df"),
    p = pick("pr\\(>chisq\\)|p.*value|pvalue"),
    warnings = warnings
  )
}


nomo_invariance_add_comparison <- function(current_row,
                                           previous_row,
                                           previous_fit,
                                           current_fit) {
  current_row$delta_cfi <- current_row$cfi - previous_row$cfi
  current_row$delta_rmsea <- current_row$rmsea - previous_row$rmsea
  current_row$delta_srmr <- current_row$srmr - previous_row$srmr

  lrt <- nomo_invariance_lrt(previous_fit, current_fit)
  current_row$lrt_chisq <- lrt$chisq
  current_row$lrt_df <- lrt$df
  current_row$lrt_p <- lrt$p

  list(row = current_row, warnings = lrt$warnings)
}


nomo_invariance_validate_partial <- function(partial, sequence) {
  if (is.null(partial)) return(invisible(TRUE))

  if (!inherits(partial, "nomo_partial")) {
    stop("`partial` must be NULL or an object created by `nomo_partial()`.", call. = FALSE)
  }

  bad <- setdiff(unique(partial$releases$level), sequence)
  if (length(bad)) {
    stop(
      sprintf(
        paste0(
          "Partial-invariance release level(s) are not part of this model's ",
          "sequence: %s."
        ),
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


nomo_invariance_partial_for_level <- function(partial, level, sequence) {
  if (is.null(partial) || identical(level, "configural")) {
    return(character())
  }

  current_index <- match(level, sequence)
  release_index <- match(partial$releases$level, sequence)

  unique(
    partial$releases$syntax[
      is.finite(release_index) & release_index <= current_index
    ]
  )
}


nomo_invariance_partial_cumulative <- function(partial, levels, sequence) {
  rows <- lapply(
    levels,
    function(level) {
      requested <- nomo_invariance_partial_for_level(
        partial = partial,
        level = level,
        sequence = sequence
      )

      tibble::tibble(
        level = level,
        requested_release = if (length(requested)) {
          paste(requested, collapse = "; ")
        } else {
          ""
        }
      )
    }
  )

  dplyr::bind_rows(rows)
}


nomo_invariance_score_test <- function(fit, level) {
  if (is.null(fit) || identical(level, "configural")) {
    return(list(
      table = tibble::tibble(),
      raw = NULL,
      warning = character(),
      error = NULL
    ))
  }

  warnings <- character()
  error <- NULL

  score <- tryCatch(
    withCallingHandlers(
      lavaan::lavTestScore(
        fit,
        univariate = TRUE,
        cumulative = FALSE,
        epc = TRUE,
        standardized = TRUE
      ),
      warning = function(w) {
        warnings <<- unique(c(warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(score) || is.null(score$uni)) {
    return(list(
      table = tibble::tibble(),
      raw = score,
      warning = warnings,
      error = error
    ))
  }

  uni <- as.data.frame(score$uni)
  if (!nrow(uni)) {
    return(list(
      table = tibble::tibble(),
      raw = score,
      warning = warnings,
      error = error
    ))
  }

  get_numeric <- function(candidates) {
    hit <- intersect(candidates, names(uni))
    if (!length(hit)) return(rep(NA_real_, nrow(uni)))
    suppressWarnings(as.numeric(uni[[hit[[1L]]]]))
  }

  constraint <- if (all(c("lhs", "op", "rhs") %in% names(uni))) {
    paste(uni$lhs, uni$op, uni$rhs)
  } else {
    rn <- row.names(uni)
    if (is.null(rn) || !length(rn)) {
      paste0("constraint_", seq_len(nrow(uni)))
    } else {
      rn
    }
  }

  tab <- tibble::tibble(
    level = level,
    constraint_index = seq_len(nrow(uni)),
    constraint = as.character(constraint),
    score_x2 = get_numeric(c("X2", "Chisq", "chisq")),
    df = get_numeric(c("df", "Df")),
    p_value = get_numeric(c("p.value", "pvalue", "Pr(>Chisq)")),
    diagnostic_only = TRUE
  )

  tab <- tab[order(tab$score_x2, decreasing = TRUE, na.last = TRUE), , drop = FALSE]

  list(
    table = tab,
    raw = score,
    warning = warnings,
    error = error
  )
}


nomo_invariance_decision_log <- function(group,
                                         groups,
                                         requested_levels,
                                         fit_evidence,
                                         estimator,
                                         missing,
                                         ID.fac,
                                         ID.cat,
                                         parameterization,
                                         ordered,
                                         category_table,
                                         sequence_note,
                                         partial = NULL,
                                         localize = TRUE,
                                         score_diagnostics = NULL) {
  log <- nomo_log_new()

  log <- nomo_log_add(
    log,
    stage = "invariance",
    object = group,
    metric = "grouping_variable",
    reference = paste(groups, collapse = ", "),
    severity = "info",
    observation = sprintf(
      "Measurement invariance was evaluated across %d observed groups.",
      length(groups)
    ),
    recommendation = paste(
      "Interpret comparisons only to the level supported by measurement",
      "evidence and the intended substantive comparison."
    )
  )

  log <- nomo_log_add(
    log,
    stage = "invariance",
    object = "model",
    metric = "requested_levels",
    reference = paste(requested_levels, collapse = " -> "),
    severity = "info",
    observation = paste0(
      "Requested sequence: ",
      paste(requested_levels, collapse = " -> "),
      "."
    ),
    recommendation = sequence_note
  )

  if (length(ordered)) {
    log <- nomo_log_add(
      log,
      stage = "invariance",
      object = "model",
      metric = "ordered_identification",
      reference = paste0(ID.cat, " / ", parameterization),
      severity = "info",
      observation = paste(
        sprintf(
          "%d ordered indicator(s) were modeled using `%s` categorical identification.",
          length(ordered),
          ID.cat
        ),
        sequence_note
      ),
      recommendation = sequence_note
    )

    cat_ref <- paste0(
      category_table$item,
      "=",
      category_table$categories,
      collapse = ", "
    )

    log <- nomo_log_add(
      log,
      stage = "invariance",
      object = "ordered_indicators",
      metric = "observed_categories",
      reference = cat_ref,
      severity = "info",
      observation = paste0("Observed category counts: ", cat_ref, "."),
      recommendation = "Keep category structure visible when reporting invariance."
    )
  }

  if (!is.null(estimator)) {
    log <- nomo_log_add(
      log,
      stage = "invariance",
      object = "model",
      metric = "estimator",
      reference = estimator,
      severity = "info",
      observation = sprintf("Estimator `%s` was used.", estimator),
      recommendation = "Interpret fit-change evidence in light of the estimator."
    )
  }

  if (!is.null(missing)) {
    log <- nomo_log_add(
      log,
      stage = "invariance",
      object = "model",
      metric = "missing",
      reference = missing,
      severity = "info",
      observation = sprintf("Missing-data option `%s` was requested.", missing),
      recommendation = "Keep the missing-data strategy consistent across levels."
    )
  }

  log <- nomo_log_add(
    log,
    stage = "invariance",
    object = "model",
    metric = "factor_identification",
    reference = ID.fac,
    severity = "info",
    observation = sprintf("Factor identification uses `%s`.", ID.fac),
    recommendation = "Identification is explicit and held consistent across levels."
  )

  if (!is.null(partial) && nrow(partial$releases)) {
    for (i in seq_len(nrow(partial$releases))) {
      row <- partial$releases[i, , drop = FALSE]
      log <- nomo_log_add(
        log,
        stage = "invariance_partial",
        object = row$syntax[[1L]],
        metric = "researcher_requested_release",
        reference = row$level[[1L]],
        severity = "info",
        observation = sprintf(
          "Researcher requested release `%s` beginning at the %s level.",
          row$syntax[[1L]],
          row$level[[1L]]
        ),
        recommendation = paste(
          "Rationale:",
          row$rationale[[1L]],
          "The release is never selected automatically by nomologR."
        )
      )
    }
  }

  for (i in seq_len(nrow(fit_evidence))) {
    row <- fit_evidence[i, , drop = FALSE]
    severity <- if (identical(row$status[[1L]], "estimated")) "info" else "concern"

    log <- nomo_log_add(
      log,
      stage = "invariance",
      object = row$level[[1L]],
      metric = "invariance_level",
      severity = severity,
      observation = if (identical(row$status[[1L]], "estimated")) {
        sprintf(
          "%s model estimated with equality constraints on: %s.",
          tools::toTitleCase(row$level[[1L]]),
          row$constraints[[1L]]
        )
      } else {
        sprintf(
          "%s model status: %s.",
          tools::toTitleCase(row$level[[1L]]),
          row$status[[1L]]
        )
      },
      recommendation = if (identical(row$status[[1L]], "estimated")) {
        paste(
          "Inspect absolute fit, change in fit, parameter behavior, and localized",
          "strain together. No single change index is treated as a pass/fail rule."
        )
      } else {
        paste(
          "Investigate the model/syntax before imposing additional equality",
          "constraints."
        )
      }
    )
  }

  if (isTRUE(localize) && !is.null(score_diagnostics)) {
    n_local <- sum(vapply(
      score_diagnostics,
      function(x) nrow(x$table),
      integer(1)
    ))

    log <- nomo_log_add(
      log,
      stage = "invariance",
      object = "equality_constraints",
      metric = "score_diagnostics",
      value = n_local,
      reference = "diagnostic only",
      severity = if (n_local > 0L) "review" else "info",
      observation = sprintf(
        "%d univariate equality-constraint score diagnostic(s) were retained.",
        n_local
      ),
      recommendation = paste(
        "Use these diagnostics to localize strain, not to authorize automatic",
        "constraint release. Partial invariance requires an explicit researcher",
        "specification and rationale."
      )
    )
  }

  log
}


#' Evaluate measurement invariance across groups
#'
#' `nomo_invariance()` evaluates increasingly constrained multi-group CFA models
#' while retaining every generated `semTools::measEq.syntax()` object and every
#' fitted lavaan model.
#'
#' The sequence is adapted to the observed category structure. Continuous
#' indicators use configural -> metric -> scalar -> strict. Ordered indicators
#' with four or more categories permit a separate threshold step. Three-category
#' indicators require threshold equality as part of the metric step. When any
#' binary indicator is present, threshold, loading, and intercept restrictions
#' are imposed together at the first equality step (`strong`) because those
#' restrictions cannot be treated as independent tests under Wu-Estabrook
#' identification.
#'
#' Partial invariance is researcher controlled. Supply an object from
#' `nomo_partial()` to request specific equality-constraint releases. Releases
#' are carried forward to more restrictive levels and their rationales are
#' retained. `nomo_invariance()` never searches for a combination of releases
#' that makes a fit rule pass.
#'
#' When `localize = TRUE`, univariate score tests for equality constraints are
#' retained as diagnostic evidence. They are explicitly not used to modify the
#' fitted model.
#'
#' @param model A researcher-specified lavaan measurement-model syntax string or
#'   an object created by `nomo_model()`.
#' @param data A non-empty data frame.
#' @param group Character scalar naming the grouping variable in `data`.
#' @param ordered Optional character vector naming ordered indicators.
#' @param levels Optional invariance levels. `NULL` uses the sequence implied by
#'   the indicator category structure.
#' @param partial Optional researcher-specified partial-invariance releases from
#'   `nomo_partial()`.
#' @param localize Logical. If `TRUE`, retain equality-constraint score tests as
#'   diagnostic evidence. Diagnostics never trigger automatic release/refitting.
#' @param estimator Optional lavaan estimator. Ordered models default to WLSMV.
#' @param missing Optional lavaan missing-data option.
#' @param ID.fac Factor-identification method passed to
#'   `semTools::measEq.syntax()`. `"std.lv"` is the default.
#' @param ID.cat Ordered-indicator identification method. Wu-Estabrook is the
#'   default.
#' @param parameterization Lavaan categorical parameterization. `"theta"` is
#'   the default for ordered indicators.
#' @param guidance Guidance settings from `nomo_defaults()`.
#'
#' @return A `nomo_invariance` object containing generated syntax objects/text,
#'   fitted models, fit/change evidence, category information, partial-invariance
#'   provenance, score diagnostics, and a decision log.
#' @export
nomo_invariance <- function(model,
                            data,
                            group,
                            ordered = NULL,
                            levels = NULL,
                            partial = NULL,
                            localize = TRUE,
                            estimator = NULL,
                            missing = NULL,
                            ID.fac = "std.lv",
                            ID.cat = "Wu.Estabrook.2016",
                            parameterization = "theta",
                            guidance = nomo_defaults()) {
  if (inherits(model, "nomo_model")) model <- as.character(model)

  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !nzchar(trimws(model))) {
    stop("`model` must be one non-empty lavaan measurement-model string.", call. = FALSE)
  }

  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }

  if (!is.character(group) || length(group) != 1L || is.na(group) ||
      !nzchar(trimws(group))) {
    stop("`group` must be one non-empty column name.", call. = FALSE)
  }
  group <- trimws(group)

  if (!group %in% names(data)) {
    stop(sprintf("Grouping variable `%s` was not found in `data`.", group), call. = FALSE)
  }

  if (anyNA(data[[group]])) {
    stop(
      "`group` contains missing values. Resolve or explicitly exclude them before invariance testing.",
      call. = FALSE
    )
  }

  groups <- unique(as.character(data[[group]]))
  if (length(groups) < 2L) {
    stop("Measurement invariance requires at least two observed groups.", call. = FALSE)
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

  if (!is.logical(localize) || length(localize) != 1L || is.na(localize)) {
    stop("`localize` must be TRUE or FALSE.", call. = FALSE)
  }

  category_table <- nomo_invariance_ordered_categories(data, ordered)
  nomo_invariance_validate_ordered_categories(category_table)

  sequence_info <- nomo_invariance_sequences(
    ordered = ordered,
    category_table = category_table
  )
  levels <- nomo_invariance_validate_levels(
    levels = levels,
    sequence = sequence_info$sequence
  )
  nomo_invariance_validate_partial(partial, sequence_info$sequence)

  if (!is.null(estimator)) {
    if (!is.character(estimator) || length(estimator) != 1L ||
        is.na(estimator) || !nzchar(trimws(estimator))) {
      stop("`estimator` must be NULL or one non-empty character value.", call. = FALSE)
    }
    estimator <- toupper(trimws(estimator))
  }

  if (!is.null(missing)) {
    if (!is.character(missing) || length(missing) != 1L ||
        is.na(missing) || !nzchar(trimws(missing))) {
      stop("`missing` must be NULL or one non-empty character value.", call. = FALSE)
    }
    missing <- trimws(missing)
  }

  if (!is.character(ID.fac) || length(ID.fac) != 1L ||
      is.na(ID.fac) || !nzchar(trimws(ID.fac))) {
    stop("`ID.fac` must be one non-empty character value.", call. = FALSE)
  }
  ID.fac <- trimws(ID.fac)

  if (!is.character(ID.cat) || length(ID.cat) != 1L ||
      is.na(ID.cat) || !nzchar(trimws(ID.cat))) {
    stop("`ID.cat` must be one non-empty character value.", call. = FALSE)
  }
  ID.cat <- trimws(ID.cat)

  if (!is.character(parameterization) || length(parameterization) != 1L ||
      is.na(parameterization) || !nzchar(trimws(parameterization))) {
    stop("`parameterization` must be one non-empty character value.", call. = FALSE)
  }
  parameterization <- tolower(trimws(parameterization))

  if (!is.list(guidance)) {
    stop("`guidance` must be a list returned by `nomo_defaults()`.", call. = FALSE)
  }

  if (length(ordered) &&
      !identical(tolower(ID.fac), "std.lv") &&
      grepl("^wu", tolower(ID.cat))) {
    stop(
      "Wu-Estabrook categorical identification should use `ID.fac = \"std.lv\"`.",
      call. = FALSE
    )
  }

  if (length(ordered) && !is.null(estimator) && grepl("^ML", estimator)) {
    stop(
      paste0(
        "ML-family estimators are not supported here with declared ordered ",
        "indicators. Leave `estimator = NULL` for WLSMV or select a ",
        "categorical-data estimator supported by lavaan."
      ),
      call. = FALSE
    )
  }

  if (length(ordered) && !is.null(missing) &&
      tolower(missing) %in% c("ml", "fiml", "ml.x", "fiml.x")) {
    stop(
      "FIML is not supported by lavaan for declared ordered indicators.",
      call. = FALSE
    )
  }

  estimator_requested <- estimator
  estimator_source <- if (length(ordered) && is.null(estimator_requested)) {
    estimator_requested <- "WLSMV"
    "ordered_default"
  } else if (is.null(estimator_requested)) {
    "lavaan_default"
  } else {
    "researcher"
  }

  constraints <- sequence_info$constraints

  syntax <- list()
  syntax_text <- list()
  fits <- list()
  fit_measures <- list()
  engine_warnings <- list()
  comparison_warnings <- list()
  evidence_rows <- list()
  score_diagnostics <- list()

  partial_requested <- nomo_invariance_partial_cumulative(
    partial = partial,
    levels = levels,
    sequence = sequence_info$sequence
  )

  previous_level <- NULL

  for (level in levels) {
    equality <- constraints[[level]]

    partial_for_level <- nomo_invariance_partial_for_level(
      partial = partial,
      level = level,
      sequence = sequence_info$sequence
    )

    syntax_args <- list(
      configural.model = model,
      data = data,
      group = group,
      group.equal = if (length(equality)) equality else "",
      group.partial = if (length(partial_for_level)) partial_for_level else "",
      ID.fac = ID.fac,
      meanstructure = TRUE,
      return.fit = FALSE
    )

    if (length(ordered)) {
      syntax_args$ordered <- ordered
      syntax_args$parameterization <- parameterization
      syntax_args$ID.cat <- ID.cat
    }

    syn <- tryCatch(
      do.call(semTools::measEq.syntax, syntax_args),
      error = function(e) {
        stop(
          sprintf(
            "Could not generate %s invariance syntax: %s",
            level,
            conditionMessage(e)
          ),
          call. = FALSE
        )
      }
    )

    syntax[[level]] <- syn
    syntax_text[[level]] <- as.character(syn)

    warnings <- character()
    fit_error <- NULL

    fit_args <- list(
      model = syntax_text[[level]],
      data = data,
      group = group
    )

    if (length(ordered)) {
      fit_args$ordered <- ordered
      fit_args$parameterization <- parameterization
    }

    if (!is.null(estimator_requested)) {
      fit_args$estimator <- estimator_requested
    }
    if (!is.null(missing)) fit_args$missing <- missing

    fit <- tryCatch(
      withCallingHandlers(
        do.call(lavaan::cfa, fit_args),
        warning = function(w) {
          warnings <<- unique(c(warnings, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        fit_error <<- conditionMessage(e)
        NULL
      }
    )

    fits[[level]] <- fit
    engine_warnings[[level]] <- warnings

    row <- nomo_invariance_fit_row(
      level = level,
      constraints = equality,
      fit = fit,
      warnings = warnings,
      error = fit_error,
      partial_requested = partial_for_level
    )

    if (!is.null(fit)) {
      fit_measures[[level]] <- tryCatch(
        lavaan::fitMeasures(fit),
        error = function(e) numeric()
      )
    } else {
      fit_measures[[level]] <- numeric()
    }

    if (!is.null(previous_level) &&
        !is.null(fit) &&
        isTRUE(row$converged[[1L]]) &&
        !is.null(fits[[previous_level]]) &&
        isTRUE(evidence_rows[[previous_level]]$converged[[1L]])) {
      comp <- nomo_invariance_add_comparison(
        current_row = row,
        previous_row = evidence_rows[[previous_level]],
        previous_fit = fits[[previous_level]],
        current_fit = fit
      )
      row <- comp$row
      comparison_warnings[[level]] <- comp$warnings
    } else {
      comparison_warnings[[level]] <- character()
    }

    evidence_rows[[level]] <- row

    if (isTRUE(localize) && !is.null(fit) && isTRUE(row$converged[[1L]])) {
      score_diagnostics[[level]] <- nomo_invariance_score_test(fit, level)
    } else {
      score_diagnostics[[level]] <- list(
        table = tibble::tibble(),
        raw = NULL,
        warning = character(),
        error = NULL
      )
    }

    if (is.null(fit) || !isTRUE(row$converged[[1L]])) {
      break
    }

    previous_level <- level
  }

  fit_evidence <- dplyr::bind_rows(evidence_rows)

  local_strain <- dplyr::bind_rows(
    lapply(score_diagnostics, function(x) x$table)
  )

  decision_log <- nomo_invariance_decision_log(
    group = group,
    groups = groups,
    requested_levels = levels,
    fit_evidence = fit_evidence,
    estimator = estimator_requested,
    missing = missing,
    ID.fac = ID.fac,
    ID.cat = if (length(ordered)) ID.cat else NA_character_,
    parameterization = if (length(ordered)) {
      parameterization
    } else {
      NA_character_
    },
    ordered = ordered,
    category_table = category_table,
    sequence_note = sequence_info$identification_note,
    partial = partial,
    localize = localize,
    score_diagnostics = score_diagnostics
  )

  out <- list(
    call = match.call(),
    model = model,
    data_n = nrow(data),
    group = group,
    groups = groups,
    indicator_type = sequence_info$type,
    identification_note = sequence_info$identification_note,
    ordered = ordered,
    ordered_categories = category_table,
    requested_levels = levels,
    completed_levels = fit_evidence$level,
    constraints = constraints[fit_evidence$level],
    partial = partial,
    partial_requested = partial_requested,
    localize = localize,
    score_diagnostics = score_diagnostics,
    local_strain = local_strain,
    estimator = if (is.null(estimator_requested)) {
      NA_character_
    } else {
      estimator_requested
    },
    estimator_source = estimator_source,
    missing = missing,
    ID.fac = ID.fac,
    ID.cat = if (length(ordered)) ID.cat else NA_character_,
    parameterization = if (length(ordered)) {
      parameterization
    } else {
      NA_character_
    },
    syntax = syntax,
    syntax_text = syntax_text,
    fits = fits,
    fit_measures = fit_measures,
    fit_evidence = fit_evidence,
    engine_warnings = engine_warnings,
    comparison_warnings = comparison_warnings,
    decision_log = decision_log,
    guidance = guidance
  )

  class(out) <- c("nomo_invariance", "list")
  out
}
