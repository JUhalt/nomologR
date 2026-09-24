# Theory-specified nomological network -----------------------------------------

nomo_network_model_table <- function(model) {
  tryCatch(
    as.data.frame(
      lavaan::lavaanify(
        model = model,
        model.type = "sem",
        meanstructure = TRUE,
        auto = TRUE
      )
    ),
    error = function(e) {
      stop(
        paste0(
          "Could not parse `model` as lavaan SEM syntax: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}


nomo_network_relation_present <- function(partable, relation) {
  if (!nrow(partable)) return(FALSE)

  if (identical(relation$relation_type, "directed")) {
    return(any(
      partable$op == "~" &
        partable$lhs == relation$target &
        partable$rhs == relation$source
    ))
  }

  any(
    partable$op == "~~" &
      (
        (partable$lhs == relation$source & partable$rhs == relation$target) |
          (partable$lhs == relation$target & partable$rhs == relation$source)
      )
  )
}


nomo_network_relation_syntax <- function(relation) {
  if (identical(relation$relation_type, "directed")) {
    paste(relation$target, "~", relation$source)
  } else {
    paste(relation$source, "~~", relation$target)
  }
}


nomo_network_prepare_model <- function(model, hypotheses, add_missing) {
  partable <- nomo_network_model_table(model)

  latent <- unique(partable$lhs[partable$op == "=~"])
  latent <- latent[nzchar(latent)]

  h <- hypotheses$hypotheses
  additions <- vector("list", nrow(h))

  for (i in seq_len(nrow(h))) {
    hyp_row <- as.list(h[i, , drop = FALSE])

    present <- nomo_network_relation_present(partable, hyp_row)
    syntax <- nomo_network_relation_syntax(hyp_row)

    additions[[i]] <- tibble::tibble(
      id = hyp_row$id,
      relation = hyp_row$relation,
      syntax = syntax,
      already_in_model = present,
      added_from_hypothesis = isTRUE(add_missing) && !present,
      origin = hyp_row$origin
    )
  }

  additions <- dplyr::bind_rows(additions)
  lines <- additions$syntax[additions$added_from_hypothesis]

  full_model <- if (length(lines)) {
    paste(
      c(
        model,
        "",
        "# Theory-specified nomological relations added by nomologR",
        lines
      ),
      collapse = "\n"
    )
  } else {
    model
  }

  list(
    full_model = full_model,
    additions = additions,
    latent = latent,
    original_partable = partable
  )
}


nomo_network_validate_nodes <- function(hypotheses, latent, data) {
  nodes <- unique(c(
    hypotheses$hypotheses$source,
    hypotheses$hypotheses$target
  ))

  available <- unique(c(latent, names(data)))
  unknown <- setdiff(nodes, available)

  if (length(unknown)) {
    stop(
      paste0(
        "The following hypothesis node(s) are neither latent variables in ",
        "`model` nor observed columns in `data`: ",
        paste(unknown, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


nomo_network_fit_measure <- function(measures, candidates) {
  if (!length(measures)) return(NA_real_)

  for (candidate in candidates) {
    if (candidate %in% names(measures)) {
      value <- suppressWarnings(as.numeric(measures[[candidate]])[1L])
      if (length(value) && is.finite(value)) return(value)
    }
  }

  NA_real_
}


nomo_network_fit_evidence <- function(fit) {
  measures <- tryCatch(
    lavaan::fitMeasures(fit),
    error = function(e) numeric()
  )

  tibble::tibble(
    chisq = nomo_network_fit_measure(measures, c("chisq.scaled", "chisq")),
    df = nomo_network_fit_measure(measures, c("df.scaled", "df")),
    pvalue = nomo_network_fit_measure(
      measures,
      c("pvalue.scaled", "pvalue")
    ),
    cfi = nomo_network_fit_measure(
      measures,
      c("cfi.robust", "cfi.scaled", "cfi")
    ),
    tli = nomo_network_fit_measure(
      measures,
      c("tli.robust", "tli.scaled", "tli")
    ),
    rmsea = nomo_network_fit_measure(
      measures,
      c("rmsea.robust", "rmsea.scaled", "rmsea")
    ),
    srmr = nomo_network_fit_measure(measures, "srmr")
  )
}


nomo_network_match_index <- function(tab, hypothesis) {
  if (!nrow(tab)) return(integer())

  if (identical(hypothesis$relation_type, "directed")) {
    return(which(
      tab$op == "~" &
        tab$lhs == hypothesis$target &
        tab$rhs == hypothesis$source
    ))
  }

  which(
    tab$op == "~~" &
      (
        (tab$lhs == hypothesis$source & tab$rhs == hypothesis$target) |
          (tab$lhs == hypothesis$target & tab$rhs == hypothesis$source)
      )
  )
}


nomo_network_value <- function(row, candidates) {
  for (candidate in candidates) {
    if (candidate %in% names(row)) {
      value <- suppressWarnings(as.numeric(row[[candidate]])[1L])
      if (length(value) && is.finite(value)) return(value)
    }
  }
  NA_real_
}


nomo_network_in_region <- function(value,
                                   lower,
                                   upper,
                                   lower_inclusive,
                                   upper_inclusive) {
  if (!is.finite(value)) return(NA)

  lower_ok <- if (is.infinite(lower) && lower < 0) {
    TRUE
  } else if (isTRUE(lower_inclusive)) {
    value >= lower
  } else {
    value > lower
  }

  upper_ok <- if (is.infinite(upper) && upper > 0) {
    TRUE
  } else if (isTRUE(upper_inclusive)) {
    value <= upper
  } else {
    value < upper
  }

  lower_ok && upper_ok
}


nomo_network_interval_within_region <- function(ci_lower,
                                                ci_upper,
                                                lower,
                                                upper,
                                                lower_inclusive,
                                                upper_inclusive) {
  if (!is.finite(ci_lower) || !is.finite(ci_upper)) return(NA)

  lower_ok <- if (is.infinite(lower) && lower < 0) {
    TRUE
  } else if (isTRUE(lower_inclusive)) {
    ci_lower >= lower
  } else {
    ci_lower > lower
  }

  upper_ok <- if (is.infinite(upper) && upper > 0) {
    TRUE
  } else if (isTRUE(upper_inclusive)) {
    ci_upper <= upper
  } else {
    ci_upper < upper
  }

  lower_ok && upper_ok
}


nomo_network_interval_overlaps_region <- function(ci_lower,
                                                  ci_upper,
                                                  lower,
                                                  upper) {
  if (!is.finite(ci_lower) || !is.finite(ci_upper)) return(NA)

  left <- if (is.infinite(lower) && lower < 0) -Inf else lower
  right <- if (is.infinite(upper) && upper > 0) Inf else upper

  ci_upper >= left && ci_lower <= right
}


nomo_network_direction_correct <- function(estimate, prediction) {
  if (!is.finite(estimate)) return(NA)
  if (identical(prediction, "positive")) return(estimate > 0)
  if (identical(prediction, "negative")) return(estimate < 0)
  NA
}


nomo_network_classify <- function(hypothesis,
                                  estimate,
                                  ci_lower,
                                  ci_upper,
                                  converged) {
  if (!isTRUE(converged)) {
    return(list(
      concordance = "not_evaluable",
      interpretation = paste(
        "The fitted network did not converge, so this theoretical relation",
        "is not interpreted."
      )
    ))
  }

  if (!is.finite(estimate)) {
    return(list(
      concordance = "not_evaluable",
      interpretation = paste(
        "No estimable model parameter could be matched to this theoretical",
        "relation."
      )
    ))
  }

  prediction <- hypothesis$prediction

  if (identical(prediction, "negligible") &&
      !isTRUE(hypothesis$magnitude_specified)) {
    return(list(
      concordance = "not_confirmable_without_sesoi",
      interpretation = paste(
        "Theory predicted a negligible relation but no quantitative negligible",
        "region was supplied. A non-significant p-value is not treated as",
        "confirmation of negligibility."
      )
    ))
  }

  inside <- nomo_network_in_region(
    estimate,
    hypothesis$lower,
    hypothesis$upper,
    hypothesis$lower_inclusive,
    hypothesis$upper_inclusive
  )

  ci_inside <- nomo_network_interval_within_region(
    ci_lower,
    ci_upper,
    hypothesis$lower,
    hypothesis$upper,
    hypothesis$lower_inclusive,
    hypothesis$upper_inclusive
  )

  overlap <- nomo_network_interval_overlaps_region(
    ci_lower,
    ci_upper,
    hypothesis$lower,
    hypothesis$upper
  )

  if (isTRUE(ci_inside)) {
    return(list(
      concordance = "concordant",
      interpretation = paste(
        "The estimate and its confidence interval fall within the",
        "researcher-specified theoretical region."
      )
    ))
  }

  if (isTRUE(inside)) {
    return(list(
      concordance = "directionally_concordant_imprecise",
      interpretation = paste(
        "The point estimate falls within the predicted region, but its",
        "confidence interval extends outside that region."
      )
    ))
  }

  if (prediction %in% c("positive", "negative")) {
    direction_ok <- nomo_network_direction_correct(estimate, prediction)

    if (isTRUE(direction_ok) && isTRUE(hypothesis$magnitude_specified)) {
      return(list(
        concordance = "direction_concordant_below_magnitude",
        interpretation = paste(
          "The estimate has the predicted direction but does not meet the",
          "researcher-specified magnitude boundary."
        )
      ))
    }
  }

  if (isTRUE(overlap)) {
    return(list(
      concordance = "inconclusive",
      interpretation = paste(
        "The point estimate is outside the predicted region, but the",
        "confidence interval still overlaps values compatible with theory."
      )
    ))
  }

  list(
    concordance = "inconsistent",
    interpretation = paste(
      "The estimate and confidence interval do not overlap the",
      "researcher-specified theoretical region."
    )
  )
}


nomo_network_node_type <- function(node, latent, data) {
  if (node %in% latent) return("latent")
  if (node %in% names(data)) return("observed")
  "unknown"
}


nomo_network_scope <- function(source_type, target_type, relation_type) {
  if (identical(relation_type, "directed")) {
    if (identical(source_type, "latent") && identical(target_type, "observed")) {
      return("latent_to_observed_outcome")
    }
    if (identical(source_type, "observed") && identical(target_type, "latent")) {
      return("observed_to_latent")
    }
    if (identical(source_type, "latent") && identical(target_type, "latent")) {
      return("latent_structural")
    }
    return("observed_structural")
  }

  if (identical(source_type, "latent") && identical(target_type, "latent")) {
    return("latent_association")
  }
  if ("latent" %in% c(source_type, target_type)) {
    return("latent_observed_association")
  }
  "observed_association"
}


nomo_network_measurement_context <- function(fit,
                                             standardized_solution,
                                             parameter_estimates,
                                             fit_evidence,
                                             converged,
                                             warnings,
                                             guidance) {
  latent <- tryCatch(
    as.character(lavaan::lavNames(fit, type = "lv")),
    error = function(e) character()
  )

  loading_rows <- standardized_solution[
    standardized_solution$op == "=~",
    ,
    drop = FALSE
  ]

  loading_table <- if (nrow(loading_rows)) {
    loading_est <- vapply(
      seq_len(nrow(loading_rows)),
      function(i) {
        nomo_network_value(
          loading_rows[i, , drop = FALSE],
          c("est.std", "std.all")
        )
      },
      numeric(1)
    )

    loading_ref <- suppressWarnings(
      as.numeric(guidance$cfa_loading_reference)[1L]
    )
    if (!length(loading_ref) || !is.finite(loading_ref)) loading_ref <- NA_real_

    tibble::tibble(
      factor = loading_rows$lhs,
      item = loading_rows$rhs,
      loading = loading_est,
      absolute_loading = abs(loading_est),
      review_reference = loading_ref,
      attention = if (is.finite(loading_ref)) {
        ifelse(
          is.finite(loading_est) & abs(loading_est) < loading_ref,
          "review",
          "info"
        )
      } else {
        "info"
      }
    )
  } else {
    tibble::tibble(
      factor = character(),
      item = character(),
      loading = numeric(),
      absolute_loading = numeric(),
      review_reference = numeric(),
      attention = character()
    )
  }

  variance_rows <- parameter_estimates[
    parameter_estimates$op == "~~" &
      parameter_estimates$lhs == parameter_estimates$rhs,
    ,
    drop = FALSE
  ]

  improper <- if (nrow(variance_rows)) {
    est <- suppressWarnings(as.numeric(variance_rows$est))
    tibble::tibble(
      variable = variance_rows$lhs,
      estimate = est,
      negative_variance = is.finite(est) & est < 0
    )
  } else {
    tibble::tibble(
      variable = character(),
      estimate = numeric(),
      negative_variance = logical()
    )
  }

  refs <- guidance$fit_reference
  fit_flags <- character()

  if (is.list(refs) && nrow(fit_evidence)) {
    if ("cfi" %in% names(refs) &&
        is.finite(fit_evidence$cfi[[1L]]) &&
        fit_evidence$cfi[[1L]] < refs$cfi) {
      fit_flags <- c(fit_flags, "CFI below configured review reference")
    }
    if ("tli" %in% names(refs) &&
        is.finite(fit_evidence$tli[[1L]]) &&
        fit_evidence$tli[[1L]] < refs$tli) {
      fit_flags <- c(fit_flags, "TLI below configured review reference")
    }
    if ("rmsea" %in% names(refs) &&
        is.finite(fit_evidence$rmsea[[1L]]) &&
        fit_evidence$rmsea[[1L]] > refs$rmsea) {
      fit_flags <- c(fit_flags, "RMSEA above configured review reference")
    }
    if ("srmr" %in% names(refs) &&
        is.finite(fit_evidence$srmr[[1L]]) &&
        fit_evidence$srmr[[1L]] > refs$srmr) {
      fit_flags <- c(fit_flags, "SRMR above configured review reference")
    }
  }

  low_loading_n <- sum(loading_table$attention == "review", na.rm = TRUE)
  negative_variance_n <- sum(improper$negative_variance, na.rm = TRUE)

  attention <- if (!isTRUE(converged) || negative_variance_n > 0L) {
    "concern"
  } else if (low_loading_n > 0L || length(fit_flags) || length(warnings)) {
    "review"
  } else {
    "info"
  }

  notes <- character()
  if (!isTRUE(converged)) notes <- c(notes, "network model did not converge")
  if (negative_variance_n > 0L) {
    notes <- c(
      notes,
      sprintf("%d negative variance estimate(s)", negative_variance_n)
    )
  }
  if (low_loading_n > 0L) {
    notes <- c(
      notes,
      sprintf(
        "%d loading(s) below the configured absolute review reference",
        low_loading_n
      )
    )
  }
  notes <- c(notes, fit_flags)
  if (length(warnings)) {
    notes <- c(notes, sprintf("%d captured engine warning(s)", length(warnings)))
  }
  if (!length(notes)) {
    notes <- "no configured measurement-context review signal was triggered"
  }

  summary <- tibble::tibble(
    attention = attention,
    converged = isTRUE(converged),
    latent_constructs = length(latent),
    loading_review_flags = low_loading_n,
    negative_variance_flags = negative_variance_n,
    global_fit_review_flags = length(fit_flags),
    engine_warning_count = length(warnings),
    observation = paste(notes, collapse = "; ")
  )

  list(
    summary = summary,
    loadings = loading_table,
    variances = improper,
    fit_reference_flags = fit_flags
  )
}


nomo_network_equivalence_ci <- function(estimate, se, alpha) {
  if (!is.finite(estimate) || !is.finite(se) || se < 0) {
    return(c(lower = NA_real_, upper = NA_real_))
  }

  critical <- stats::qnorm(1 - alpha)
  c(
    lower = estimate - critical * se,
    upper = estimate + critical * se
  )
}


nomo_network_hypothesis_evidence <- function(hypotheses,
                                             parameter_estimates,
                                             standardized_solution,
                                             converged,
                                             latent,
                                             data,
                                             measurement_context,
                                             equivalence_alpha = 0.05) {
  h <- hypotheses$hypotheses
  rows <- vector("list", nrow(h))

  measurement_attention <- measurement_context$summary$attention[[1L]]
  measurement_observation <- measurement_context$summary$observation[[1L]]

  for (i in seq_len(nrow(h))) {
    hyp <- as.list(h[i, , drop = FALSE])

    idx_u <- nomo_network_match_index(parameter_estimates, hyp)
    idx_s <- nomo_network_match_index(standardized_solution, hyp)

    u <- if (length(idx_u)) {
      parameter_estimates[idx_u[[1L]], , drop = FALSE]
    } else {
      parameter_estimates[0, , drop = FALSE]
    }

    s <- if (length(idx_s)) {
      standardized_solution[idx_s[[1L]], , drop = FALSE]
    } else {
      standardized_solution[0, , drop = FALSE]
    }

    est_u <- if (nrow(u)) nomo_network_value(u, "est") else NA_real_
    se_u <- if (nrow(u)) nomo_network_value(u, "se") else NA_real_
    p_u <- if (nrow(u)) {
      nomo_network_value(u, c("pvalue", "p.value"))
    } else {
      NA_real_
    }
    lo_u <- if (nrow(u)) {
      nomo_network_value(u, c("ci.lower", "ci.lower.std"))
    } else {
      NA_real_
    }
    hi_u <- if (nrow(u)) {
      nomo_network_value(u, c("ci.upper", "ci.upper.std"))
    } else {
      NA_real_
    }

    est_s <- if (nrow(s)) {
      nomo_network_value(s, c("est.std", "std.all"))
    } else {
      NA_real_
    }
    se_s <- if (nrow(s)) nomo_network_value(s, "se") else NA_real_
    p_s <- if (nrow(s)) {
      nomo_network_value(s, c("pvalue", "p.value"))
    } else {
      NA_real_
    }
    lo_s <- if (nrow(s)) nomo_network_value(s, "ci.lower") else NA_real_
    hi_s <- if (nrow(s)) nomo_network_value(s, "ci.upper") else NA_real_

    use_standardized <- identical(hyp$scale, "standardized")

    estimate <- if (use_standardized) est_s else est_u
    se <- if (use_standardized) se_s else se_u
    p_value <- if (use_standardized) p_s else p_u
    ci_lower <- if (use_standardized) lo_s else lo_u
    ci_upper <- if (use_standardized) hi_s else hi_u

    eq_ci <- c(lower = NA_real_, upper = NA_real_)
    if (identical(hyp$prediction, "negligible") &&
        isTRUE(hyp$magnitude_specified)) {
      eq_ci <- nomo_network_equivalence_ci(
        estimate = estimate,
        se = se,
        alpha = equivalence_alpha
      )
    }

    classify_lower <- if (
      identical(hyp$prediction, "negligible") &&
        isTRUE(hyp$magnitude_specified)
    ) {
      eq_ci[["lower"]]
    } else {
      ci_lower
    }

    classify_upper <- if (
      identical(hyp$prediction, "negligible") &&
        isTRUE(hyp$magnitude_specified)
    ) {
      eq_ci[["upper"]]
    } else {
      ci_upper
    }

    classified <- nomo_network_classify(
      hypothesis = hyp,
      estimate = estimate,
      ci_lower = classify_lower,
      ci_upper = classify_upper,
      converged = converged
    )

    equivalence_supported <- if (
      identical(hyp$prediction, "negligible") &&
        isTRUE(hyp$magnitude_specified) &&
        is.finite(eq_ci[["lower"]]) &&
        is.finite(eq_ci[["upper"]])
    ) {
      isTRUE(
        nomo_network_interval_within_region(
          eq_ci[["lower"]],
          eq_ci[["upper"]],
          hyp$lower,
          hyp$upper,
          hyp$lower_inclusive,
          hyp$upper_inclusive
        )
      )
    } else {
      NA
    }

    confirmatory_status <- if (identical(hyp$origin, "post_hoc")) {
      "post_hoc_exploratory"
    } else {
      "a_priori"
    }

    source_type <- nomo_network_node_type(
      hyp$source,
      latent = latent,
      data = data
    )
    target_type <- nomo_network_node_type(
      hyp$target,
      latent = latent,
      data = data
    )

    base_interpretation <- classified$interpretation

    if (identical(hyp$prediction, "negligible") &&
        isTRUE(hyp$magnitude_specified) &&
        is.finite(eq_ci[["lower"]]) &&
        is.finite(eq_ci[["upper"]])) {
      base_interpretation <- paste(
        base_interpretation,
        sprintf(
          paste0(
            "Negligibility is evaluated using the %.1f%% normal-approximation ",
            "equivalence CI corresponding to alpha = %.3f, not by p > .05."
          ),
          100 * (1 - 2 * equivalence_alpha),
          equivalence_alpha
        )
      )
    }

    if (identical(confirmatory_status, "post_hoc_exploratory")) {
      base_interpretation <- paste(
        base_interpretation,
        "Because this expectation was labeled post hoc, the result is",
        "reported as exploratory rather than confirmatory."
      )
    }

    if (!identical(measurement_attention, "info")) {
      base_interpretation <- paste(
        base_interpretation,
        "Measurement context also requires review:",
        measurement_observation
      )
    }

    rows[[i]] <- tibble::tibble(
      id = hyp$id,
      relation = hyp$relation,
      source = hyp$source,
      target = hyp$target,
      source_type = source_type,
      target_type = target_type,
      evidence_scope = nomo_network_scope(
        source_type,
        target_type,
        hyp$relation_type
      ),
      relation_type = hyp$relation_type,
      prediction = hyp$prediction,
      theoretical_region = hyp$region,
      scale = hyp$scale,
      origin = hyp$origin,
      estimate = estimate,
      se = se,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      p_value = p_value,
      equivalence_alpha = if (
        identical(hyp$prediction, "negligible") &&
          isTRUE(hyp$magnitude_specified)
      ) {
        equivalence_alpha
      } else {
        NA_real_
      },
      equivalence_ci_lower = eq_ci[["lower"]],
      equivalence_ci_upper = eq_ci[["upper"]],
      equivalence_supported = equivalence_supported,
      estimate_unstandardized = est_u,
      estimate_standardized = est_s,
      concordance = classified$concordance,
      confirmatory_status = confirmatory_status,
      measurement_attention = measurement_attention,
      interpretation = base_interpretation
    )
  }

  dplyr::bind_rows(rows)
}


nomo_network_fit_once <- function(model_fitted,
                                  model_relations,
                                  hypotheses,
                                  data,
                                  ordered,
                                  estimator_requested,
                                  estimator_source,
                                  missing,
                                  std.lv,
                                  control,
                                  guidance,
                                  equivalence_alpha,
                                  sample_role) {
  fit_args <- list(
    model = model_fitted,
    data = data,
    std.lv = std.lv
  )
  if (length(ordered)) fit_args$ordered <- ordered
  if (!is.null(estimator_requested)) fit_args$estimator <- estimator_requested
  if (!is.null(missing)) fit_args$missing <- missing
  if (!is.null(control)) fit_args$control <- control

  engine_warnings <- character()
  fit <- tryCatch(
    withCallingHandlers(
      do.call(lavaan::sem, fit_args),
      warning = function(w) {
        engine_warnings <<- unique(c(engine_warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(
        paste0(
          "Nomological-network estimation failed in the ",
          sample_role,
          " sample: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  converged <- isTRUE(tryCatch(
    lavaan::lavInspect(fit, "converged"),
    error = function(e) FALSE
  ))

  parameter_estimates <- tryCatch(
    tibble::as_tibble(
      lavaan::parameterEstimates(
        fit,
        standardized = TRUE,
        ci = TRUE
      )
    ),
    error = function(e) tibble::tibble()
  )

  standardized_solution <- tryCatch(
    tibble::as_tibble(
      lavaan::standardizedSolution(
        fit,
        type = "std.all",
        se = TRUE,
        zstat = TRUE,
        pvalue = TRUE,
        ci = TRUE
      )
    ),
    error = function(e) tibble::tibble()
  )

  fit_evidence <- nomo_network_fit_evidence(fit)

  latent <- tryCatch(
    as.character(lavaan::lavNames(fit, type = "lv")),
    error = function(e) character()
  )

  measurement_context <- nomo_network_measurement_context(
    fit = fit,
    standardized_solution = standardized_solution,
    parameter_estimates = parameter_estimates,
    fit_evidence = fit_evidence,
    converged = converged,
    warnings = engine_warnings,
    guidance = guidance
  )

  hypothesis_evidence <- nomo_network_hypothesis_evidence(
    hypotheses = hypotheses,
    parameter_estimates = parameter_estimates,
    standardized_solution = standardized_solution,
    converged = converged,
    latent = latent,
    data = data,
    measurement_context = measurement_context,
    equivalence_alpha = equivalence_alpha
  )

  list(
    sample_role = sample_role,
    model_fitted = model_fitted,
    data_n = nrow(data),
    converged = converged,
    engine_warnings = engine_warnings,
    fit = fit,
    fit_evidence = fit_evidence,
    parameter_estimates = parameter_estimates,
    standardized_solution = standardized_solution,
    measurement_context = measurement_context,
    hypothesis_evidence = hypothesis_evidence,
    estimator = if (is.null(estimator_requested)) {
      NA_character_
    } else {
      estimator_requested
    },
    estimator_source = estimator_source
  )
}


# Which side of zero a confidence interval lies on: "positive" when it lies
# entirely above zero, "negative" when entirely below, and NA when it includes
# zero or is unavailable. An unavailable interval can never establish a side.
nomo_network_interval_side <- function(ci_lower, ci_upper) {
  if (length(ci_lower) != 1L || length(ci_upper) != 1L ||
      !is.finite(ci_lower) || !is.finite(ci_upper)) {
    return(NA_character_)
  }
  if (ci_lower > 0) return("positive")
  if (ci_upper < 0) return("negative")
  NA_character_
}


nomo_network_row_value <- function(row, column) {
  if (!column %in% names(row)) return(NA_real_)
  value <- row[[column]][[1L]]
  if (is.null(value)) NA_real_ else as.numeric(value)
}


# Classifies a change in sign between samples for a directional prediction.
#
# Opposite point estimates alone are not a reversal: two estimates scattered
# around a null relation will differ in sign about half the time. A reversal is
# claimed only when each sample, on its own, places the relation on its side of
# zero -- both confidence intervals exclude zero, in opposite directions. When
# only one interval excludes zero, the other sample failed to replicate that
# direction without establishing the opposite one. When neither does, the sign
# change is within sampling uncertainty.
nomo_network_sign_change <- function(primary_row, validation_row) {
  side_a <- nomo_network_interval_side(
    nomo_network_row_value(primary_row, "ci_lower"),
    nomo_network_row_value(primary_row, "ci_upper")
  )
  side_b <- nomo_network_interval_side(
    nomo_network_row_value(validation_row, "ci_lower"),
    nomo_network_row_value(validation_row, "ci_upper")
  )

  intervals_missing <- any(!is.finite(c(
    nomo_network_row_value(primary_row, "ci_lower"),
    nomo_network_row_value(primary_row, "ci_upper"),
    nomo_network_row_value(validation_row, "ci_lower"),
    nomo_network_row_value(validation_row, "ci_upper")
  )))

  if (!is.na(side_a) && !is.na(side_b) && side_a != side_b) {
    return(list(
      status = "sign_reversal",
      interpretation = paste(
        "The relation changes sign across samples, and both confidence",
        "intervals exclude zero on opposite sides, so each sample on its own",
        "supports a relation in a different direction. This is a",
        "substantively important replication discrepancy."
      )
    ))
  }

  if (!is.na(side_a) || !is.na(side_b)) {
    established <- if (!is.na(side_a)) "primary" else "validation"
    other <- if (!is.na(side_a)) "validation" else "primary"
    return(list(
      status = "direction_not_replicated",
      interpretation = sprintf(
        paste(
          "The point estimates have opposite signs, but only the %s sample's",
          "confidence interval excludes zero. The %s sample does not support",
          "the direction found in the %s sample, but it does not establish the",
          "opposite direction either: the direction was not replicated, which",
          "is not the same as a reversal."
        ),
        established, other, established
      )
    ))
  }

  interpretation <- paste(
    "The point estimates have opposite signs, but neither sample's confidence",
    "interval excludes zero, so neither sample distinguishes the relation from",
    "zero. The sign change is compatible with sampling variability around a",
    "small or null relation and is not evidence of a reversal. A claim that",
    "the relation is negligible needs a negligible() prediction with a",
    "researcher-specified equivalence region."
  )
  if (intervals_missing) {
    interpretation <- paste(
      interpretation,
      "Confidence intervals were unavailable for at least one sample, so a",
      "reversal could not be established."
    )
  }

  list(status = "sign_change_within_uncertainty", interpretation = interpretation)
}


nomo_network_replication_evidence <- function(primary, validation) {
  a <- primary$hypothesis_evidence
  b <- validation$hypothesis_evidence

  rows <- vector("list", nrow(a))

  supportive <- c(
    "concordant",
    "directionally_concordant_imprecise",
    "direction_concordant_below_magnitude"
  )

  for (i in seq_len(nrow(a))) {
    aa <- a[i, , drop = FALSE]
    bb <- b[b$id == aa$id[[1L]], , drop = FALSE]

    if (!nrow(bb)) {
      rows[[i]] <- tibble::tibble(
        id = aa$id[[1L]],
        relation = aa$relation[[1L]],
        prediction = aa$prediction[[1L]],
        primary_estimate = aa$estimate[[1L]],
        validation_estimate = NA_real_,
        estimate_shift = NA_real_,
        primary_concordance = aa$concordance[[1L]],
        validation_concordance = "not_evaluable",
        replication_status = "not_evaluable",
        interpretation = "The validation sample did not yield a matching relation."
      )
      next
    }

    est_a <- aa$estimate[[1L]]
    est_b <- bb$estimate[[1L]]
    con_a <- aa$concordance[[1L]]
    con_b <- bb$concordance[[1L]]
    prediction <- aa$prediction[[1L]]

    status <- "mixed_or_inconclusive"
    interpretation <- paste(
      "The two samples provide a mixed or inconclusive pattern for this",
      "theory-specified relation."
    )

    if (!is.finite(est_a) || !is.finite(est_b) ||
        con_a == "not_evaluable" || con_b == "not_evaluable") {
      status <- "not_evaluable"
      interpretation <- "At least one sample did not yield an evaluable relation."
    } else if (prediction %in% c("positive", "negative") &&
               sign(est_a) != 0 &&
               sign(est_b) != 0 &&
               sign(est_a) != sign(est_b)) {
      sign_change <- nomo_network_sign_change(aa, bb)
      status <- sign_change$status
      interpretation <- sign_change$interpretation
    } else if (identical(con_a, "concordant") &&
               identical(con_b, "concordant")) {
      status <- "replicated_concordance"
      interpretation <- paste(
        "The theoretical region is supported in both the primary and",
        "validation samples."
      )
    } else if (con_a %in% supportive && identical(con_b, "inconsistent")) {
      status <- "not_replicated"
      interpretation <- paste(
        "The primary sample was compatible with the prediction, but the",
        "validation sample was inconsistent with it."
      )
    } else if (identical(con_a, "inconsistent") && con_b %in% supportive) {
      status <- "unstable"
      interpretation <- paste(
        "The samples disagree materially: the primary sample was inconsistent",
        "while the validation sample was compatible with the prediction."
      )
    } else if (identical(con_a, "inconsistent") &&
               identical(con_b, "inconsistent")) {
      status <- "replicated_inconsistency"
      interpretation <- paste(
        "Both samples are inconsistent with the researcher-specified",
        "theoretical region."
      )
    } else if (prediction %in% c("positive", "negative") &&
               sign(est_a) == sign(est_b)) {
      status <- "direction_replicated_but_uncertain"
      interpretation <- paste(
        "The estimated direction is the same across samples, but uncertainty",
        "or magnitude evidence prevents stronger replication language."
      )
    }

    rows[[i]] <- tibble::tibble(
      id = aa$id[[1L]],
      relation = aa$relation[[1L]],
      prediction = prediction,
      primary_estimate = est_a,
      validation_estimate = est_b,
      estimate_shift = est_b - est_a,
      primary_concordance = con_a,
      validation_concordance = con_b,
      replication_status = status,
      interpretation = interpretation
    )
  }

  dplyr::bind_rows(rows)
}


nomo_network_decision_log <- function(model_additions,
                                      hypotheses_evidence,
                                      converged,
                                      warnings,
                                      estimator,
                                      ordered,
                                      measurement_context,
                                      replication_evidence = NULL,
                                      sample_role = "primary") {
  log <- nomo_log_new()

  log <- nomo_log_add(
    log,
    stage = "network",
    object = sample_role,
    metric = "convergence",
    value = as.numeric(converged),
    reference = "TRUE",
    severity = if (isTRUE(converged)) "info" else "concern",
    observation = if (isTRUE(converged)) {
      sprintf("The theory-specified SEM converged in the %s sample.", sample_role)
    } else {
      sprintf(
        "The theory-specified SEM did not converge in the %s sample.",
        sample_role
      )
    },
    recommendation = if (isTRUE(converged)) {
      paste(
        "Interpret theoretical relations together with measurement quality,",
        "model fit, uncertainty, and the a-priori/post-hoc distinction."
      )
    } else {
      "Investigate estimation/model problems before interpreting theory."
    }
  )

  measurement_row <- measurement_context$summary[1L, , drop = FALSE]
  log <- nomo_log_add(
    log,
    stage = "network",
    object = sample_role,
    metric = "measurement_context",
    value = measurement_row$loading_review_flags[[1L]],
    reference = "measurement evidence interpreted jointly",
    severity = measurement_row$attention[[1L]],
    observation = measurement_row$observation[[1L]],
    recommendation = paste(
      "Do not attribute nomological strain to theory alone when the",
      "measurement model also requires review."
    )
  )

  added <- model_additions[model_additions$added_from_hypothesis, , drop = FALSE]
  if (nrow(added)) {
    for (i in seq_len(nrow(added))) {
      log <- nomo_log_add(
        log,
        stage = "network",
        object = added$relation[[i]],
        metric = "theory_path_added",
        severity = "info",
        observation = sprintf(
          "Theory-specified relation `%s` was added to the fitted model as `%s`.",
          added$relation[[i]],
          added$syntax[[i]]
        ),
        recommendation = paste(
          "This relation came from the explicit hypothesis object rather than",
          "from post-estimation model search."
        )
      )
    }
  }

  if (length(ordered)) {
    log <- nomo_log_add(
      log,
      stage = "network",
      object = sample_role,
      metric = "ordered_indicators",
      reference = paste(ordered, collapse = ", "),
      severity = "info",
      observation = sprintf(
        "%d ordered indicator(s) were declared.",
        length(ordered)
      ),
      recommendation = "Interpret SEM estimates using the categorical-data estimator."
    )
  }

  if (!is.null(estimator)) {
    log <- nomo_log_add(
      log,
      stage = "network",
      object = sample_role,
      metric = "estimator",
      reference = estimator,
      severity = "info",
      observation = sprintf("Estimator `%s` was requested.", estimator),
      recommendation = "Keep the estimator visible in methods/reporting."
    )
  }

  if (length(warnings)) {
    log <- nomo_log_add(
      log,
      stage = "network",
      object = sample_role,
      metric = "engine_warnings",
      value = length(warnings),
      severity = "review",
      observation = paste(warnings, collapse = " | "),
      recommendation = "Review estimation warnings before substantive interpretation."
    )
  }

  for (i in seq_len(nrow(hypotheses_evidence))) {
    row <- hypotheses_evidence[i, , drop = FALSE]
    severity <- if (row$concordance[[1L]] %in% c(
      "inconsistent", "not_evaluable"
    )) {
      "concern"
    } else if (row$concordance[[1L]] %in% c(
      "directionally_concordant_imprecise",
      "direction_concordant_below_magnitude",
      "inconclusive",
      "not_confirmable_without_sesoi"
    )) {
      "review"
    } else {
      "info"
    }

    log <- nomo_log_add(
      log,
      stage = "network",
      object = row$relation[[1L]],
      metric = "theory_concordance",
      value = row$estimate[[1L]],
      reference = row$theoretical_region[[1L]],
      severity = severity,
      observation = row$interpretation[[1L]],
      recommendation = paste(
        "Treat this as one piece of construct-validity evidence; distinguish",
        "theory strain from imprecision and measurement-model problems."
      )
    )
  }

  if (!is.null(replication_evidence) && nrow(replication_evidence)) {
    for (i in seq_len(nrow(replication_evidence))) {
      row <- replication_evidence[i, , drop = FALSE]
      severity <- if (row$replication_status[[1L]] %in% c(
        "sign_reversal",
        "direction_not_replicated",
        "not_replicated",
        "unstable",
        "replicated_inconsistency"
      )) {
        "concern"
      } else if (row$replication_status[[1L]] %in% c(
        "mixed_or_inconclusive",
        "direction_replicated_but_uncertain",
        "sign_change_within_uncertainty",
        "not_evaluable"
      )) {
        "review"
      } else {
        "info"
      }

      log <- nomo_log_add(
        log,
        stage = "network_replication",
        object = row$relation[[1L]],
        metric = "replication_status",
        value = row$estimate_shift[[1L]],
        reference = row$replication_status[[1L]],
        severity = severity,
        observation = row$interpretation[[1L]],
        recommendation = paste(
          "Treat validation-sample evidence as a replication check rather than",
          "using the primary sample alone to establish the network claim."
        )
      )
    }
  }

  log
}


nomo_network_validate_data <- function(data, label) {
  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop(
      sprintf("`%s` must be a non-empty data frame.", label),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Evaluate a theory-specified nomological network
#'
#' `nomo_network()` combines a researcher-specified measurement/SEM model with a
#' machine-readable object from `nomo_hypotheses()`. Hypothesized relations that
#' are not already present in `model` can be added transparently before fitting,
#' so the theory object itself can define the structural portion of the network.
#'
#' Directed `A -> B` hypotheses map to lavaan regression paths `B ~ A`.
#' Association `A <-> B` hypotheses map to covariance paths `A ~~ B`.
#'
#' For quantitative `negligible(within = ...)` predictions, `nomo_network()`
#' evaluates the SESOI using a normal-approximation equivalence confidence
#' interval. With the default `equivalence_alpha = .05`, this is a 90 percent
#' interval, corresponding to the usual two one-sided tests logic. A bare
#' `negligible()` prediction remains non-confirmable from `p > .05`.
#'
#' The function can also fit the same prespecified model in a validation sample.
#' Pass `validation_data` explicitly, or pass a `nomo_split` object as `data` to
#' use its calibration and validation subsets. No model relation is added or
#' removed on the basis of validation results.
#'
#' @section Replication status when the sign changes:
#' When a directional prediction's primary and validation point estimates have
#' opposite signs, `replication_status` is decided by the 95 percent confidence
#' intervals, not by the point estimates alone:
#'
#' * `"sign_reversal"`: both intervals exclude zero, on opposite sides. Each
#'   sample on its own supports a relation in a different direction.
#' * `"direction_not_replicated"`: exactly one interval excludes zero. The
#'   other sample does not support that direction, but it does not establish
#'   the opposite direction either.
#' * `"sign_change_within_uncertainty"`: neither interval excludes zero, or an
#'   interval is unavailable. Neither sample distinguishes the relation from
#'   zero, and the sign change is compatible with sampling variability around a
#'   small or null relation.
#'
#' Point estimates scattered around a null relation differ in sign about half
#' the time, so a sign change without interval evidence is not treated as a
#' substantive discrepancy. Requiring both intervals to exclude zero is the
#' interval counterpart of each sample separately rejecting a zero relation in
#' its own direction. The rule does not turn a non-significant result into
#' evidence of no relation: that claim needs a `negligible(within = ...)`
#' prediction with a researcher-specified equivalence region (Lakens, Scheel, &
#' Isager, 2018).
#'
#' @param model One non-empty lavaan SEM/measurement-model syntax string or an
#'   object created by `nomo_model()`.
#' @param data A non-empty data frame or a `nomo_split` object. For a
#'   `nomo_split`, the calibration subset is the primary sample and the
#'   validation subset is reserved for replication.
#' @param hypotheses A `nomo_hypotheses` object.
#' @param validation_data Optional independent validation data frame. Do not use
#'   this together with a `nomo_split` object.
#' @param add_missing Logical. If `TRUE` (default), theory-specified relations
#'   absent from `model` are appended transparently before estimation.
#' @param ordered Optional character vector naming ordered indicators.
#' @param estimator Optional lavaan estimator. When ordered indicators are
#'   declared and `estimator = NULL`, WLSMV is requested.
#' @param missing Optional lavaan missing-data option.
#' @param std.lv Logical passed to `lavaan::sem()`.
#' @param control Optional optimizer-control list passed to `lavaan::sem()`.
#' @param equivalence_alpha One number strictly between 0 and .5. For
#'   quantitative negligible predictions, the equivalence confidence level is
#'   `1 - 2 * equivalence_alpha`.
#' @param guidance Guidance settings returned by `nomo_defaults()`.
#'
#' @section Relationships estimated between observed variables:
#' A hypothesis whose endpoints are latent variables is estimated with their
#' measurement error modelled. When an endpoint is an observed variable, that
#' error enters unmodelled. If the observed variable is a composite of several
#' items, such as a sum, mean, or factor score, the estimated relationship
#' carries the discrepancy [nomo_scores()] reports as correlational accuracy,
#' which can be substantial and runs in either direction depending on the
#' scoring method and the model.
#'
#' `nomo_network()` cannot tell from the model syntax whether an observed
#' variable is a composite or a single measured quantity, so it does not guess.
#' It classifies each hypothesis by whether its endpoints are latent, and the
#' decision log discloses observed endpoints: for review when both ends are
#' observed, and for information when one is. A single measured variable, such
#' as a criterion recorded without items, is not a composite, and the
#' disclosure says so.
#'
#' Where the observed variables are composites, modelling their items as
#' indicators of latent variables removes the discrepancy, and
#' `lavaan::sam()` estimates the structural relationships after the
#' measurement model (Rosseel & Loh, 2024).
#'
#' Where they are factor scores and the hypothesis is a linear regression,
#' Skrondal and Laake (2001) showed that one scoring design gives consistent
#' estimates of the regression coefficients: regression-method scores for the
#' predictors and Bartlett scores for the outcome, each block scored from a
#' measurement model of its own. Scoring both with the same method, or scoring
#' all the factors from one model, does not, and
#' `vignette("scoring", package = "nomologR")` shows both failures. The
#' standard errors still treat the scores as observed, and Skrondal and Laake
#' note that corrected ones may require resampling. The result does not extend
#' to nonlinear models. No correction is applied automatically.
#'
#' @return A `nomo_network` object retaining the primary fitted SEM, optional
#'   validation fit, measurement context, relation-level theory evidence,
#'   replication evidence, and decision log.
#'
#' @references
#' Anderson, J. C., & Gerbing, D. W. (1988). Structural equation modeling in
#' practice: A review and recommended two-step approach. *Psychological
#' Bulletin, 103*(3), 411-423. \doi{10.1037/0033-2909.103.3.411}
#'
#' Cronbach, L. J., & Meehl, P. E. (1955). Construct validity in psychological
#' tests. *Psychological Bulletin, 52*(4), 281-302. \doi{10.1037/h0040957}
#'
#' Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing for
#' psychological research: A tutorial. *Advances in Methods and Practices in
#' Psychological Science, 1*(2), 259-269. \doi{10.1177/2515245918770963}
#'
#' Messick, S. (1995). Validity of psychological assessment: Validation of
#' inferences from persons' responses and performances as scientific inquiry
#' into score meaning. *American Psychologist, 50*(9), 741-749.
#' \doi{10.1037/0003-066X.50.9.741}
#'
#' Rosseel, Y., & Loh, W. W. (2024). A structural after measurement approach
#' to structural equation modeling. *Psychological Methods, 29*(3), 561-588.
#' \doi{10.1037/met0000503}
#'
#' Schuirmann, D. J. (1987). A comparison of the two one-sided tests procedure
#' and the power approach for assessing the equivalence of average
#' bioavailability. *Journal of Pharmacokinetics and Biopharmaceutics, 15*(6),
#' 657-680. \doi{10.1007/BF01068419}
#'
#' Skrondal, A., & Laake, P. (2001). Regression among factor scores.
#' *Psychometrika, 66*(4), 563-575. \doi{10.1007/BF02296196}
#'
#' @examples
#' model <- nomo_model(list(
#'   Agency = c("ag1", "ag2", "ag3", "ag4"),
#'   Persistence = c("pe1", "pe2", "pe3", "pe4"),
#'   SocialDesirability = c("sd1", "sd2", "sd3")
#' ))
#'
#' h <- nomo_hypotheses(
#'   "Agency -> Persistence" = positive(min = .20),
#'   "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15)),
#'   "Agency -> Performance" = positive()
#' )
#'
#' net <- nomo_network(model, data = nomo_demo_network, hypotheses = h)
#' net
#' nomo_table(net, "hypotheses")
#'
#' \donttest{
#' # Evaluate the same prespecified network in calibration and validation rows
#' s <- nomo_split(nomo_demo_network, validation_prop = 0.40, seed = 2026)
#' net_rep <- nomo_network(model, data = s, hypotheses = h)
#' nomo_table(net_rep, "replication")
#' }
#' @export
nomo_network <- function(model,
                         data,
                         hypotheses,
                         validation_data = NULL,
                         add_missing = TRUE,
                         ordered = NULL,
                         estimator = NULL,
                         missing = NULL,
                         std.lv = TRUE,
                         control = NULL,
                         equivalence_alpha = 0.05,
                         guidance = nomo_defaults()) {
  if (inherits(model, "nomo_model")) model <- as.character(model)

  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !nzchar(trimws(model))) {
    stop("`model` must be one non-empty lavaan model string.", call. = FALSE)
  }

  split_source <- "none"
  if (inherits(data, "nomo_split")) {
    if (!is.null(validation_data)) {
      stop(
        "Do not supply `validation_data` when `data` is already a `nomo_split`.",
        call. = FALSE
      )
    }
    primary_data <- data$calibration
    validation_data <- data$validation
    primary_role <- "calibration"
    split_source <- "nomo_split"
  } else {
    primary_data <- data
    primary_role <- "primary"
  }

  nomo_network_validate_data(primary_data, "data")
  if (!is.null(validation_data)) {
    nomo_network_validate_data(validation_data, "validation_data")
  }

  if (!inherits(hypotheses, "nomo_hypotheses")) {
    stop("`hypotheses` must be created by `nomo_hypotheses()`.", call. = FALSE)
  }

  if (!is.logical(add_missing) || length(add_missing) != 1L ||
      is.na(add_missing)) {
    stop("`add_missing` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(std.lv) || length(std.lv) != 1L || is.na(std.lv)) {
    stop("`std.lv` must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.numeric(equivalence_alpha) || length(equivalence_alpha) != 1L ||
      is.na(equivalence_alpha) || !is.finite(equivalence_alpha) ||
      equivalence_alpha <= 0 || equivalence_alpha >= 0.5) {
    stop("`equivalence_alpha` must be one number strictly between 0 and .5.", call. = FALSE)
  }

  if (!is.list(guidance)) {
    stop("`guidance` must be a list returned by `nomo_defaults()`.", call. = FALSE)
  }

  if (is.null(ordered)) {
    ordered <- character()
  } else {
    if (!is.character(ordered) || anyNA(ordered) ||
        any(!nzchar(trimws(ordered)))) {
      stop("`ordered` must be NULL or a character vector of indicator names.", call. = FALSE)
    }
    ordered <- unique(trimws(ordered))

    absent_primary <- setdiff(ordered, names(primary_data))
    if (length(absent_primary)) {
      stop(
        sprintf(
          "Ordered indicator(s) not found in primary data: %s.",
          paste(absent_primary, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (!is.null(validation_data)) {
      absent_validation <- setdiff(ordered, names(validation_data))
      if (length(absent_validation)) {
        stop(
          sprintf(
            "Ordered indicator(s) not found in validation data: %s.",
            paste(absent_validation, collapse = ", ")
          ),
          call. = FALSE
        )
      }
    }
  }

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

  prepared <- nomo_network_prepare_model(
    model = model,
    hypotheses = hypotheses,
    add_missing = add_missing
  )

  nomo_network_validate_nodes(
    hypotheses = hypotheses,
    latent = prepared$latent,
    data = primary_data
  )
  if (!is.null(validation_data)) {
    nomo_network_validate_nodes(
      hypotheses = hypotheses,
      latent = prepared$latent,
      data = validation_data
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

  primary <- nomo_network_fit_once(
    model_fitted = prepared$full_model,
    model_relations = prepared$additions,
    hypotheses = hypotheses,
    data = primary_data,
    ordered = ordered,
    estimator_requested = estimator_requested,
    estimator_source = estimator_source,
    missing = missing,
    std.lv = std.lv,
    control = control,
    guidance = guidance,
    equivalence_alpha = equivalence_alpha,
    sample_role = primary_role
  )

  validation <- NULL
  replication_evidence <- tibble::tibble()

  if (!is.null(validation_data)) {
    validation <- nomo_network_fit_once(
      model_fitted = prepared$full_model,
      model_relations = prepared$additions,
      hypotheses = hypotheses,
      data = validation_data,
      ordered = ordered,
      estimator_requested = estimator_requested,
      estimator_source = estimator_source,
      missing = missing,
      std.lv = std.lv,
      control = control,
      guidance = guidance,
      equivalence_alpha = equivalence_alpha,
      sample_role = "validation"
    )

    replication_evidence <- nomo_network_replication_evidence(
      primary,
      validation
    )
  }

  decision_log <- nomo_network_decision_log(
    model_additions = prepared$additions,
    hypotheses_evidence = primary$hypothesis_evidence,
    converged = primary$converged,
    warnings = primary$engine_warnings,
    estimator = estimator_requested,
    ordered = ordered,
    measurement_context = primary$measurement_context,
    replication_evidence = replication_evidence,
    sample_role = primary_role
  )

  decision_log <- dplyr::bind_rows(
    decision_log,
    nomo_network_endpoint_log(hypotheses, primary$fit)
  )

  if (!is.null(validation)) {
    validation_log <- nomo_network_decision_log(
      model_additions = prepared$additions[0, , drop = FALSE],
      hypotheses_evidence = validation$hypothesis_evidence,
      converged = validation$converged,
      warnings = validation$engine_warnings,
      estimator = estimator_requested,
      ordered = ordered,
      measurement_context = validation$measurement_context,
      replication_evidence = NULL,
      sample_role = "validation"
    )
    decision_log <- dplyr::bind_rows(decision_log, validation_log)
  }

  out <- list(
    call = match.call(),
    model_original = model,
    model_fitted = prepared$full_model,
    model_relations = prepared$additions,
    hypotheses = hypotheses,
    sample_role = primary_role,
    split_source = split_source,
    data_n = primary$data_n,
    validation_n = if (is.null(validation)) NA_integer_ else validation$data_n,
    ordered = ordered,
    estimator = primary$estimator,
    estimator_source = primary$estimator_source,
    missing = missing,
    std.lv = std.lv,
    control = control,
    equivalence_alpha = equivalence_alpha,
    equivalence_confidence = 1 - 2 * equivalence_alpha,
    converged = primary$converged,
    engine_warnings = primary$engine_warnings,
    fit = primary$fit,
    fit_evidence = primary$fit_evidence,
    parameter_estimates = primary$parameter_estimates,
    standardized_solution = primary$standardized_solution,
    measurement_context = primary$measurement_context,
    hypothesis_evidence = primary$hypothesis_evidence,
    validation = validation,
    replication_evidence = replication_evidence,
    decision_log = decision_log,
    guidance = guidance
  )

  class(out) <- c("nomo_network", "list")
  out
}



# Observed endpoints -----------------------------------------------------------
#
# The network cannot tell from its syntax whether an observed variable is a
# composite of items or a single measured quantity, so it does not guess. It
# classifies each hypothesis by whether its endpoints are latent, and discloses
# what an observed endpoint means: its measurement error enters unmodelled, and
# if it is a composite, the relationship carries the discrepancy nomo_scores()
# reports as correlational accuracy.
nomo_network_endpoint_log <- function(hypotheses, fit) {
  log <- nomo_log_new()

  latent <- tryCatch(
    as.character(lavaan::lavNames(fit, type = "lv")),
    error = function(e) character()
  )
  # nomo_network() has already validated the hypotheses, so the table has a
  # row for each, with its source and target.
  table <- as.data.frame(nomo_table(hypotheses))

  source_latent <- table$source %in% latent
  target_latent <- table$target %in% latent
  both_observed <- !source_latent & !target_latent
  mixed <- xor(source_latent, target_latent)

  observed_names <- function(rows) {
    sort(unique(c(
      table$source[rows & !source_latent],
      table$target[rows & !target_latent]
    )))
  }

  consequence <- paste(
    "Observed variables enter the network with their measurement error",
    "unmodelled. If any of them is a composite of several items (a sum, mean,",
    "or factor score), the estimated relationship carries the discrepancy",
    "nomo_scores() reports as correlational accuracy, which can be substantial",
    "and runs in either direction depending on the scoring method and the",
    "model. A single measured variable, such as a criterion recorded without",
    "items, is not a composite, and this does not apply to it."
  )
  remedy <- paste(
    "Where these are composites, model their items as indicators of latent",
    "variables instead, which removes the discrepancy; lavaan::sam() estimates",
    "the structural relationships after the measurement model (Rosseel & Loh,",
    "2024). Where they are factor scores and the hypothesis is a linear",
    "regression, Skrondal and Laake (2001) showed that regression-method scores",
    "for the predictors and Bartlett scores for the outcome, each block scored",
    "from a measurement model of its own, give consistent estimates of the",
    "regression coefficients; scores from one model containing both, or from",
    "the same method for both, do not, and the standard errors here still treat",
    "the scores as observed."
  )

  if (any(both_observed)) {
    log <- nomo_log_add(
      log, stage = "network", object = "hypotheses",
      metric = "observed_endpoints",
      value = sum(both_observed),
      reference = "nomo_scores(): correlational accuracy",
      severity = "review",
      observation = sprintf(
        "%s relate%s observed variables to each other: %s.",
        paste(table$id[both_observed], collapse = ", "),
        if (sum(both_observed) == 1L) "s" else "",
        paste(observed_names(both_observed), collapse = ", ")
      ),
      recommendation = paste(consequence, remedy)
    )
  }

  if (any(mixed)) {
    log <- nomo_log_add(
      log, stage = "network", object = "hypotheses",
      metric = "mixed_endpoints",
      value = sum(mixed),
      reference = "nomo_scores(): correlational accuracy",
      severity = "info",
      observation = sprintf(
        "%s relate%s a latent variable to an observed one: %s.",
        paste(table$id[mixed], collapse = ", "),
        if (sum(mixed) == 1L) "s" else "",
        paste(observed_names(mixed), collapse = ", ")
      ),
      recommendation = paste(consequence, remedy)
    )
  }

  log
}
