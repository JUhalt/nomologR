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


# A release in lavaan syntax, parsed as semTools parses `group.partial`.
nomo_invariance_parse_release <- function(release) {
  rows <- tryCatch(
    lavaan::lavParseModelString(release, as.data.frame. = TRUE),
    error = function(e) NULL
  )
  if (!NROW(rows)) {
    stop(
      sprintf(
        paste0(
          "Release `%s` is not lavaan parameter syntax such as `F =~ x2`, ",
          "`x3 ~ 1`, `u2 | t1`, or `x1 ~~ x1`."
        ),
        release
      ),
      call. = FALSE
    )
  }
  rows
}


# The model's loadings, as factor and item, against which releases are checked.
# A model lavaan cannot parse is left to lavaan, which explains the error.
nomo_invariance_model_structure <- function(model) {
  pt <- lavaan::lavaanify(model)
  loadings <- pt[pt$op == "=~", , drop = FALSE]
  tibble::tibble(factor = loadings$lhs, item = loadings$rhs)
}


# Each release must name a parameter of `model` that some level holds equal
# (#145): semTools ignores a release that matches nothing, which had left the
# model fully constrained while the fit table, summary, and log reported the
# release. A release applies from the level that first holds its parameter type
# equal. Declared earlier, it changes nothing before that level, so it is
# moved there and the log says so. Declared later, the level before would hold
# the parameter equal and the next free it, so the two models would not be
# nested and their difference test would be invalid; that is an error.
# Returns the releases at the levels they apply from, and the declared levels.
nomo_invariance_validate_partial <- function(partial, sequence_info, structure,
                                             hint = "") {
  if (is.null(partial)) return(list(partial = NULL, declared = NULL))

  if (!inherits(partial, "nomo_partial")) {
    stop("`partial` must be NULL or an object created by `nomo_partial()`.", call. = FALSE)
  }

  sequence <- sequence_info$sequence
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

  types <- c("=~" = "loadings", "~1" = "intercepts", "|" = "thresholds",
             "~~" = "residuals")
  nouns <- c(loadings = "a loading", intercepts = "an intercept",
             thresholds = "a threshold", residuals = "a residual variance")
  indicators <- setdiff(structure$item, structure$factor)
  declared <- partial$releases$level

  for (i in seq_len(nrow(partial$releases))) {
    release <- partial$releases$syntax[[i]]
    rows <- nomo_invariance_parse_release(release)
    named <- ifelse(
      rows$op == "=~",
      paste(rows$lhs, rows$rhs) %in% paste(structure$factor, structure$item),
      rows$lhs %in% indicators & (rows$op != "~~" | rows$rhs == rows$lhs)
    )
    type <- unique(unname(types[rows$op]))
    if (anyNA(type) || length(type) != 1L || !all(named)) {
      stop(
        sprintf(
          paste0(
            "Release `%s` does not name a loading (`F =~ x2`), intercept ",
            "(`x3 ~ 1`), threshold (`u2 | t1`), or residual variance ",
            "(`x1 ~~ x1`) of an indicator in `model`, the parameters the ",
            "levels hold equal.%s"
          ),
          release, hint
        ),
        call. = FALSE
      )
    }

    holds <- vapply(
      sequence,
      function(level) type %in% sequence_info$constraints[[level]],
      logical(1)
    )
    if (!any(holds)) {
      stop(
        sprintf(
          "Release `%s` frees %s, and no level of this sequence holds %s equal.",
          release, nouns[[type]], type
        ),
        call. = FALSE
      )
    }
    first <- sequence[holds][[1L]]
    if (match(declared[[i]], sequence) > match(first, sequence)) {
      stop(
        sprintf(
          paste0(
            "Release `%s` frees %s, and %s are first held equal at the %s ",
            "level, so declare it at %s rather than %s: released only from %s ",
            "on, the %s model would not be nested in the model before it."
          ),
          release, nouns[[type]], type, first, first, declared[[i]],
          declared[[i]], declared[[i]]
        ),
        call. = FALSE
      )
    }
    partial$releases$level[[i]] <- first
  }

  list(partial = partial, declared = declared)
}


# semTools ignores a release it cannot apply, such as a marker loading fixed at
# 1 or a threshold fixed for identification. So each release applied at a level
# must free a parameter in the generated syntax: one estimated in some group or
# occasion and not labeled equal across them (#145). `names_map` gives, across
# occasions, each item's and factor's names in the fitted model.
nomo_invariance_check_releases <- function(syntax, releases, level, ngroups,
                                           names_map = NULL,
                                           parameterization = NULL) {
  if (!length(releases)) return(invisible(TRUE))
  # Without ordered indicators there is no parameterization to pass.
  args <- list(syntax, ngroups = ngroups)
  args$parameterization <- parameterization
  pt <- suppressWarnings(do.call(lavaan::lavaanify, args))
  expand <- function(name) {
    if (name %in% names(names_map)) names_map[[name]] else name
  }
  for (release in releases) {
    rows <- nomo_invariance_parse_release(release)
    freed <- vapply(seq_len(nrow(rows)), function(i) {
      hit <- pt$op == rows$op[[i]] &
        pt$lhs %in% expand(rows$lhs[[i]]) &
        pt$rhs %in% expand(rows$rhs[[i]]) &
        (rows$op[[i]] != "~~" | pt$lhs == pt$rhs)
      any(pt$free[hit] > 0L) && length(unique(pt$label[hit])) > 1L
    }, logical(1))
    if (!all(freed)) {
      stop(
        sprintf(
          paste0(
            "Release `%s` frees no parameter at the %s level: the generated ",
            "model fixes it or still holds it equal, as for a marker loading ",
            "or a threshold fixed for identification. Remove the release."
          ),
          release, level
        ),
        call. = FALSE
      )
    }
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
    # A non-empty data.frame always has row names, including automatic ones.
    row.names(uni)
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
                                         score_diagnostics = NULL,
                                         design = "groups",
                                         ordered_detected = character(),
                                         partial_declared = partial$releases$level) {
  log <- nomo_log_new()
  across_occasions <- identical(design, "occasions")

  log <- nomo_log_add(
    log,
    stage = "invariance",
    object = group,
    metric = if (across_occasions) "occasions" else "grouping_variable",
    reference = paste(groups, collapse = ", "),
    severity = "info",
    observation = sprintf(
      if (across_occasions) {
        "Measurement invariance was evaluated across %d occasions of the same measures."
      } else {
        "Measurement invariance was evaluated across %d observed groups."
      },
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
          "%s %s modeled using `%s` categorical identification.",
          nomo_present_count(length(ordered), "ordered indicator"),
          nomo_present_noun(length(ordered), "was", "were"),
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
  log <- nomo_ordered_detected_log(log, ordered_detected, "invariance")

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
      declared <- partial_declared[[i]]
      log <- nomo_log_add(
        log,
        stage = "invariance_partial",
        object = row$syntax[[1L]],
        metric = "researcher_requested_release",
        reference = row$level[[1L]],
        severity = "info",
        observation = if (identical(declared, row$level[[1L]])) {
          sprintf(
            "Researcher requested release `%s` beginning at the %s level.",
            row$syntax[[1L]],
            row$level[[1L]]
          )
        } else {
          sprintf(
            paste(
              "Researcher requested release `%s` at the %s level. No level",
              "holds its parameters equal before %s, so it applies from there."
            ),
            row$syntax[[1L]], declared, row$level[[1L]]
          )
        },
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


# Checks and normalizes the options nomo_invariance() and
# nomo_invariance_longitudinal() share, and chooses the invariance sequence.
# With `model`, model indicators stored as ordered factors are treated as
# declared, since lavaan fits them as categorical whether or not `ordered` names
# them (#145); the longitudinal function detects them by item before it calls
# this and passes no `model`. `structure` holds the model's loadings, as
# factor and item, against which partial releases are checked; it is evaluated
# only when there are releases.
nomo_invariance_prepare <- function(data,
                                    ordered,
                                    levels,
                                    partial,
                                    localize,
                                    estimator,
                                    missing,
                                    ID.fac,
                                    ID.cat,
                                    parameterization,
                                    guidance,
                                    model = NULL,
                                    structure = NULL,
                                    release_hint = "") {
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

  ordered_detected <- character()
  if (!is.null(model)) {
    found <- nomo_ordered_indicators(model, data, ordered)
    ordered <- found$ordered
    ordered_detected <- found$detected
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
  releases <- nomo_invariance_validate_partial(
    partial, sequence_info, structure, release_hint
  )

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

  list(
    ordered = ordered,
    ordered_detected = ordered_detected,
    category_table = category_table,
    sequence_info = sequence_info,
    levels = levels,
    partial = releases$partial,
    partial_declared = releases$declared,
    missing = missing,
    ID.fac = ID.fac,
    ID.cat = ID.cat,
    parameterization = parameterization,
    estimator_requested = estimator_requested,
    estimator_source = estimator_source
  )
}


# Fits each requested level in turn and stops at the first that fails or does
# not converge. `syntax_base` and `fit_base` hold the measEq.syntax() and lavaan
# arguments every level shares; `equal` and `release` name the measEq.syntax()
# arguments that carry a level's equality constraints and partial releases:
# group.equal and group.partial across groups, long.equal and long.partial
# across occasions. `ngroups` and `names_map` let each release be checked in
# the generated syntax (see nomo_invariance_check_releases()).
nomo_invariance_fit_levels <- function(levels,
                                       constraints,
                                       partial,
                                       sequence_info,
                                       syntax_base,
                                       fit_base,
                                       equal,
                                       release,
                                       localize,
                                       ngroups = 1L,
                                       names_map = NULL) {
  syntax <- list()
  syntax_text <- list()
  fits <- list()
  fit_measures <- list()
  engine_warnings <- list()
  comparison_warnings <- list()
  evidence_rows <- list()
  score_diagnostics <- list()

  previous_level <- NULL

  for (level in levels) {
    equality <- constraints[[level]]

    partial_for_level <- nomo_invariance_partial_for_level(
      partial = partial,
      level = level,
      sequence = sequence_info$sequence
    )

    syntax_args <- syntax_base
    syntax_args[[equal]] <- if (length(equality)) equality else ""
    syntax_args[[release]] <- if (length(partial_for_level)) partial_for_level else ""

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
    nomo_invariance_check_releases(
      syntax_text[[level]], partial_for_level, level, ngroups, names_map,
      fit_base$parameterization
    )

    warnings <- character()
    fit_error <- NULL

    fit_args <- c(list(model = syntax_text[[level]]), fit_base)

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

  list(
    syntax = syntax,
    syntax_text = syntax_text,
    fits = fits,
    fit_measures = fit_measures,
    engine_warnings = engine_warnings,
    comparison_warnings = comparison_warnings,
    score_diagnostics = score_diagnostics,
    fit_evidence = dplyr::bind_rows(evidence_rows)
  )
}


# The measEq.syntax() and lavaan arguments shared by every level.
nomo_invariance_engine_args <- function(syntax_base,
                                        fit_base,
                                        ordered,
                                        parameterization,
                                        ID.cat,
                                        estimator,
                                        missing) {
  if (length(ordered)) {
    syntax_base$ordered <- ordered
    syntax_base$parameterization <- parameterization
    syntax_base$ID.cat <- ID.cat
    fit_base$ordered <- ordered
    fit_base$parameterization <- parameterization
  }
  if (!is.null(estimator)) fit_base$estimator <- estimator
  if (!is.null(missing)) fit_base$missing <- missing
  list(syntax = syntax_base, fit = fit_base)
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
#' Each release must name a loading, intercept, threshold, or residual variance
#' of an indicator in `model`, and must free that parameter in the generated
#' model: `semTools::measEq.syntax()` ignores a release it cannot match, so a
#' misspelled name, or a marker loading fixed at 1, is an error rather than a
#' fully constrained model reported as partial. A release applies from the
#' level that first holds its parameter type equal (loadings at `metric`,
#' intercepts at `scalar`, and so on, as the sequence for the indicators sets
#' them). Declared at an earlier level, where it would change nothing, it is
#' moved to that first level and the decision log says so. Declared at a later
#' level, it is an error, because the earlier model would hold the parameter
#' equal and the later one free it, so the two would not be nested.
#'
#' When `localize = TRUE`, univariate score tests for equality constraints are
#' retained as diagnostic evidence. They are explicitly not used to modify the
#' fitted model.
#'
#' @param model A researcher-specified lavaan measurement-model syntax string or
#'   an object created by `nomo_model()`.
#' @param data A non-empty data frame.
#' @param group Character scalar naming the grouping variable in `data`.
#' @param ordered Optional character vector naming ordered indicators. Model
#'   indicators stored as ordered factors are modeled as ordered whether or not
#'   they are named here, since lavaan fits them as categorical; the result's
#'   `ordered` includes them, and the decision log lists them for review.
#' @param levels Optional invariance levels. `NULL` uses the sequence implied by
#'   the indicator category structure.
#' @param partial Optional researcher-specified partial-invariance releases from
#'   `nomo_partial()`.
#' @param localize Logical. If `TRUE`, retain equality-constraint score tests as
#'   diagnostic evidence. Diagnostics never trigger automatic release/refitting.
#' @param estimator Optional lavaan estimator. Ordered models default to WLSMV.
#' @param missing Optional lavaan missing-data option.
#' @section Latent means:
#' Once intercepts are invariant, fully or with researcher-specified partial
#' releases, groups can be compared on the construct itself (Byrne, Shavelson,
#' & Muthén, 1989). This is the structured-means form of known-groups evidence:
#' a difference theory predicts between groups that differ on the construct.
#' At each level that holds intercepts equal, `latent_means` gives each group's
#' latent means relative to the reference group, the first group, which the
#' default `ID.fac = "std.lv"` fixes at a mean of 0 and a variance of 1. Each
#' mean is then a difference in the reference group's latent standard
#' deviations, the effect size Hancock (2001) describes. Under another
#' identification the table is empty. Where the intercepts are not invariant,
#' the means are not comparable until the non-invariant intercepts are
#' released with [nomo_partial()] (Vandenberg & Lance, 2000).
#'
#' @param ID.fac Factor-identification method passed to
#'   `semTools::measEq.syntax()`. `"std.lv"` is the default.
#' @param ID.cat Ordered-indicator identification method passed to
#'   `semTools::measEq.syntax()`. Wu-Estabrook is the default.
#' @param parameterization Lavaan categorical parameterization. `"theta"` is
#'   the default for ordered indicators.
#' @param guidance Guidance settings from `nomo_defaults()`.
#'
#' @return A `nomo_invariance` object. The fields to read are:
#'
#'   * `groups`, `requested_levels`, and `completed_levels`.
#'   * `fit_evidence`: one row per level, with its fit, its change from the
#'     level before, the likelihood-ratio test, and any warning or error.
#'   * `local_strain`: score diagnostics for each equality constraint, which
#'     localize strain without releasing anything.
#'   * `partial`: the researcher-specified releases, when given, each at the
#'     level it applies from.
#'   * `latent_means`: at each level that holds intercepts equal, each group's
#'     latent means relative to the reference group, in the reference group's
#'     latent standard deviations, with their intervals. See **Latent means**.
#'   * `fits`: the fitted `lavaan` model at each level.
#'   * `syntax_text`: the model syntax at each level.
#'   * `engine_warnings`: warnings `lavaan` raised at each level.
#'   * `indicator_type`, `identification_note`, and `decision_log`.
#'
#'   Other fields record the call, the settings used, and intermediate engine
#'   results. They may change between releases and are not part of the stable
#'   interface (see `?nomologR`).
#'
#' @references
#' Historical foundations:
#'
#' Jöreskog, K. G. (1971). Simultaneous factor analysis in several
#' populations. *Psychometrika, 36*(4), 409-426. \doi{10.1007/BF02291366}
#'
#' Hancock, G. R. (2001). Effect size, power, and sample size determination
#' for structured means modeling and MIMIC approaches to between-groups
#' hypothesis testing of means on a single latent construct. *Psychometrika,
#' 66*(3), 373-388. \doi{10.1007/BF02294440}
#'
#' Meredith, W. (1993). Measurement invariance, factor analysis and factorial
#' invariance. *Psychometrika, 58*(4), 525-543. \doi{10.1007/BF02294825}
#'
#' Vandenberg, R. J., & Lance, C. E. (2000). A review and synthesis of the
#' measurement invariance literature: Suggestions, practices, and
#' recommendations for organizational research. *Organizational Research
#' Methods, 3*(1), 4-70. \doi{10.1177/109442810031002}
#'
#' Change-in-fit evidence:
#'
#' Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of
#' measurement invariance. *Structural Equation Modeling, 14*(3), 464-504.
#' \doi{10.1080/10705510701301834}
#'
#' Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit indexes
#' for testing measurement invariance. *Structural Equation Modeling, 9*(2),
#' 233-255. \doi{10.1207/S15328007SEM0902_5}
#'
#' Putnick, D. L., & Bornstein, M. H. (2016). Measurement invariance
#' conventions and reporting: The state of the art and future directions for
#' psychological research. *Developmental Review, 41*, 71-90.
#' \doi{10.1016/j.dr.2016.06.004}
#'
#' Ordered-categorical indicators:
#'
#' Svetina, D., Rutkowski, L., & Rutkowski, D. (2020). Multiple-group
#' invariance with categorical outcomes using updated guidelines: An
#' illustration using Mplus and the lavaan/semTools packages. *Structural
#' Equation Modeling, 27*(1), 111-130. \doi{10.1080/10705511.2019.1602776}
#'
#' Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor
#' analysis models of different levels of invariance for ordered categorical
#' outcomes. *Psychometrika, 81*(4), 1014-1045.
#' \doi{10.1007/s11336-016-9506-0}
#'
#' @seealso [nomo_partial()] for researcher-specified releases.
#'
#' @examples
#' inv <- nomo_invariance(
#'   "Agency =~ ag1 + ag2 + ag3 + ag4",
#'   data = nomo_demo_network,
#'   group = "group",
#'   levels = c("configural", "metric", "scalar")
#' )
#' inv
#' nomo_table(inv, "fit")
#' head(nomo_table(inv, "local_strain"))
#'
#' \donttest{
#' # A release is a documented researcher decision, never an automatic search.
#' release <- nomo_partial(
#'   level = "scalar",
#'   syntax = "ag3 ~ 1",
#'   rationale = "Prior evidence anticipated a mode difference in ag3 wording."
#' )
#' inv_partial <- nomo_invariance(
#'   "Agency =~ ag1 + ag2 + ag3 + ag4",
#'   data = nomo_demo_network,
#'   group = "group",
#'   levels = c("configural", "metric", "scalar"),
#'   partial = release
#' )
#' nomo_table(inv_partial, "fit")
#' nomo_table(inv_partial, "partial")
#' }
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

  prepared <- nomo_invariance_prepare(
    data = data,
    ordered = ordered,
    levels = levels,
    partial = partial,
    localize = localize,
    estimator = estimator,
    missing = missing,
    ID.fac = ID.fac,
    ID.cat = ID.cat,
    parameterization = parameterization,
    guidance = guidance,
    model = model,
    structure = nomo_invariance_model_structure(model)
  )
  ordered <- prepared$ordered
  category_table <- prepared$category_table
  sequence_info <- prepared$sequence_info
  levels <- prepared$levels
  partial <- prepared$partial
  missing <- prepared$missing
  ID.fac <- prepared$ID.fac
  ID.cat <- prepared$ID.cat
  parameterization <- prepared$parameterization
  estimator_requested <- prepared$estimator_requested
  estimator_source <- prepared$estimator_source
  # After the argument checks, so their more specific messages come first.
  nomo_check_model_variables(model, data)

  constraints <- sequence_info$constraints

  partial_requested <- nomo_invariance_partial_cumulative(
    partial = partial,
    levels = levels,
    sequence = sequence_info$sequence
  )

  engine <- nomo_invariance_engine_args(
    syntax_base = list(
      configural.model = model,
      data = data,
      group = group,
      ID.fac = ID.fac,
      meanstructure = TRUE,
      return.fit = FALSE
    ),
    fit_base = list(data = data, group = group),
    ordered = ordered,
    parameterization = parameterization,
    ID.cat = ID.cat,
    estimator = estimator_requested,
    missing = missing
  )
  run <- nomo_invariance_fit_levels(
    levels = levels,
    constraints = constraints,
    partial = partial,
    sequence_info = sequence_info,
    syntax_base = engine$syntax,
    fit_base = engine$fit,
    equal = "group.equal",
    release = "group.partial",
    localize = localize,
    ngroups = length(groups)
  )
  syntax <- run$syntax
  syntax_text <- run$syntax_text
  fits <- run$fits
  fit_measures <- run$fit_measures
  engine_warnings <- run$engine_warnings
  comparison_warnings <- run$comparison_warnings
  score_diagnostics <- run$score_diagnostics
  fit_evidence <- run$fit_evidence

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
    score_diagnostics = score_diagnostics,
    ordered_detected = prepared$ordered_detected,
    partial_declared = prepared$partial_declared
  )

  latent_means <- nomo_invariance_latent_means(
    fits = fits,
    constraints = constraints,
    fit_evidence = fit_evidence,
    ID.fac = ID.fac
  )
  decision_log <- dplyr::bind_rows(
    decision_log,
    nomo_invariance_latent_means_log(latent_means)
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
    ordered_detected = prepared$ordered_detected,
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
    latent_means = latent_means,
    engine_warnings = engine_warnings,
    comparison_warnings = comparison_warnings,
    decision_log = decision_log,
    guidance = guidance
  )

  class(out) <- c("nomo_invariance", "list")
  out
}


# Latent means ----------------------------------------------------------------------
#
# The structured-means comparison of groups on the construct: at each level
# whose intercepts are held equal, fully or with the researcher's partial
# releases, the groups' latent means can be compared (Byrne, Shavelson, &
# Muthén, 1989). Under std.lv identification the reference group's latent means
# are 0 and its variances 1, so each mean is a difference in the reference
# group's latent standard deviations, the effect size Hancock (2001) describes.
nomo_invariance_latent_means <- function(fits, constraints, fit_evidence, ID.fac) {
  out <- tibble::tibble(
    level = character(),
    group = character(),
    reference_group = character(),
    factor = character(),
    estimate = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    p_value = numeric()
  )
  if (!identical(tolower(ID.fac), "std.lv")) return(out)

  converged <- fit_evidence$level[fit_evidence$converged %in% TRUE]
  rows <- lapply(converged, function(level) {
    if (!"intercepts" %in% constraints[[level]]) return(NULL)
    fit <- fits[[level]]
    pe <- lavaan::parameterEstimates(fit, ci = TRUE)
    latent <- lavaan::lavNames(fit, type = "lv")
    means <- pe[pe$op == "~1" & pe$lhs %in% latent & pe$group > 1L, , drop = FALSE]
    labels <- lavaan::lavInspect(fit, "group.label")
    tibble::tibble(
      level = level,
      group = labels[means$group],
      reference_group = labels[[1L]],
      factor = means$lhs,
      estimate = means$est,
      se = means$se,
      ci_lower = means$ci.lower,
      ci_upper = means$ci.upper,
      p_value = means$pvalue
    )
  })
  dplyr::bind_rows(out, rows)
}


nomo_invariance_latent_means_log <- function(latent_means) {
  log <- nomo_log_new()
  if (!nrow(latent_means)) return(log)
  # The most constrained level with latent means, the one a comparison would
  # usually be reported from.
  level <- latent_means$level[[nrow(latent_means)]]
  rows <- latent_means[latent_means$level == level, , drop = FALSE]
  number <- function(x) nomo_present_number(x, 2L)
  nomo_log_add(
    log, stage = "invariance", object = "latent_means",
    metric = "latent_means",
    value = nrow(rows),
    reference = paste(
      "Latent means are comparable only with invariant intercepts, fully or",
      "partially (Byrne, Shavelson, & Muth\u00e9n, 1989)"
    ),
    severity = "info",
    observation = sprintf(
      "At the %s level, latent means relative to %s, in its latent standard deviations: %s.",
      level, rows$reference_group[[1L]],
      paste0(
        rows$factor, " in ", rows$group, " ", number(rows$estimate), " [",
        number(rows$ci_lower), ", ", number(rows$ci_upper), "]",
        collapse = "; "
      )
    ),
    recommendation = paste(
      "As known-groups evidence, compare each difference with the one theory",
      "predicts (Hancock, 2001). If the intercepts are not invariant, release",
      "those the local-strain table points to with nomo_partial() before",
      "interpreting the means (Vandenberg & Lance, 2000)."
    )
  )
}
