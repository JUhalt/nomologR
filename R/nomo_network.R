# Theory-specified nomological network -----------------------------------------

nomo_network_model_table <- function(model, fixed.x = FALSE) {
  tryCatch(
    as.data.frame(
      lavaan::lavaanify(
        model = model,
        model.type = "sem",
        meanstructure = TRUE,
        auto = TRUE,
        fixed.x = fixed.x
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

  # The covariance of two fixed exogenous covariates is held at its sample
  # value rather than estimated, so it does not count as present.
  any(
    partable$op == "~~" &
      partable$exo == 0L &
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


nomo_network_compose_model <- function(model, lines) {
  if (!length(lines)) return(model)

  paste(
    c(
      model,
      "",
      "# Theory-specified nomological relations added by nomologR",
      lines
    ),
    collapse = "\n"
  )
}


# An association cannot be estimated beside a directed path between the same
# two variables. A path added in the direction opposite to another gives a
# reciprocal pair, which is not identified without further restrictions, so
# none is added. A reciprocal pair the researcher wrote in `model` is fitted
# as written: it can be identified, by instruments for example, and only the
# researcher's model says whether it is.
nomo_network_check_pairs <- function(partable, h, added) {
  regressions <- partable[partable$op == "~", , drop = FALSE]

  for (i in seq_len(nrow(h))) {
    source <- h$source[[i]]
    target <- h$target[[i]]
    reverse <- any(regressions$lhs == source & regressions$rhs == target)
    forward <- any(regressions$lhs == target & regressions$rhs == source)

    if (identical(h$relation_type[[i]], "association") && (reverse || forward)) {
      stop(
        sprintf(
          paste(
            "Hypothesis `%s` is an association, but the model to be fitted has a",
            "directed path between `%s` and `%s`. A pair of variables can carry",
            "a directed path or an association, not both. Keep the relation the",
            "theory predicts."
          ),
          h$relation[[i]], source, target
        ),
        call. = FALSE
      )
    }

    if (added[[i]] && reverse) {
      # The opposite path is in the researcher's model unless another
      # hypothesis adds it.
      other <- added & h$relation_type == "directed" &
        h$source == target & h$target == source
      opposite <- if (any(other)) {
        "another hypothesis adds"
      } else {
        "the model already has"
      }
      stop(
        sprintf(
          paste(
            "Hypothesis `%s` would add a path opposite to one %s between `%s`",
            "and `%s`. A reciprocal pair is not identified without further",
            "restrictions, so `nomo_network()` does not add one. Keep one",
            "direction, or write both paths in `model` together with what",
            "identifies them."
          ),
          h$relation[[i]], opposite, source, target
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}


nomo_network_prepare_model <- function(model, hypotheses, add_missing) {
  partable <- nomo_network_model_table(model)

  latent <- unique(partable$lhs[partable$op == "=~"])
  latent <- latent[nzchar(latent)]

  h <- hypotheses$hypotheses
  rows <- lapply(seq_len(nrow(h)), function(i) as.list(h[i, , drop = FALSE]))
  syntax <- vapply(rows, nomo_network_relation_syntax, character(1))
  directed <- h$relation_type == "directed"

  # A directed path is in the model only when the researcher wrote it.
  present <- vapply(
    rows,
    function(row) nomo_network_relation_present(partable, row),
    logical(1)
  )
  added <- isTRUE(add_missing) & directed & !present

  # An association is judged against the model that will be fitted: the
  # researcher's model with the hypothesized paths in place, under the settings
  # lavaan::sem() fits it with. A covariance lavaan adds between two exogenous
  # factors is no longer added once a hypothesized path makes one of them an
  # outcome, so judging it against the researcher's model alone would leave
  # the hypothesis without a parameter (#145). lavaan warns here when the
  # model gives an exogenous covariate a variance or covariance; the fit
  # repeats that warning, and it is recorded there.
  fitted_table <- suppressWarnings(nomo_network_model_table(
    nomo_network_compose_model(model, syntax[added]),
    fixed.x = TRUE
  ))
  nomo_network_check_pairs(fitted_table, h, added)

  present[!directed] <- vapply(
    rows[!directed],
    function(row) nomo_network_relation_present(fitted_table, row),
    logical(1)
  )
  added <- isTRUE(add_missing) & !present

  additions <- tibble::tibble(
    id = h$id,
    relation = h$relation,
    syntax = syntax,
    already_in_model = present,
    added_from_hypothesis = added,
    origin = h$origin
  )

  full_model <- nomo_network_compose_model(model, syntax[added])
  changes <- nomo_network_model_changes(model, full_model, h)

  list(
    full_model = full_model,
    additions = additions,
    changes = changes,
    latent = latent,
    original_partable = partable
  )
}


# The covariances a model estimates between two different variables, free or
# held at their sample values (fixed exogenous covariates), as sorted pairs.
nomo_network_covariance_pairs <- function(partable) {
  rows <- partable$op == "~~" & partable$lhs != partable$rhs &
    (partable$free > 0L | partable$exo == 1L)
  data.frame(
    lhs = partable$lhs[rows],
    rhs = partable$rhs[rows],
    key = vapply(which(rows), function(i) {
      paste(sort(c(partable$lhs[[i]], partable$rhs[[i]])), collapse = "\r")
    }, character(1)),
    stringsAsFactors = FALSE
  )
}


# Relations the added paths change beyond the hypotheses themselves (#145).
# lavaan::sem() covaries exogenous factors with one another, holds observed
# exogenous predictors' covariances at their sample values, and covaries the
# residuals of outcomes that predict no other variable. It relates no other
# pair that the syntax does not join: not a factor and an observed
# predictor, not an outcome and an exogenous variable, and not an outcome that
# predicts another variable and a variable it does not predict. So a path that
# makes a variable an outcome fixes to zero each covariance it had with an
# exogenous variable the path does not come from, two outcomes can gain a
# residual covariance nobody wrote, and a variable the hypotheses bring into
# the model is related only as those rules allow. Each
# changes the model the hypotheses are tested in, and the test of the
# structural restrictions counts each zero, so all are recorded; the relations
# the hypotheses name are not. A composite modeled as a single indicator is a
# factor in the model fitted, related as lavaan relates any factor, so
# `composites` are read as factors in both models.
nomo_network_model_changes <- function(model, full_model, h, composites = character()) {
  structure_of <- function(syntax) {
    table <- suppressWarnings(nomo_network_model_table(syntax, fixed.x = TRUE))
    observed <- lavaan::lavNames(table, "ov")
    used <- intersect(composites, observed)
    if (!length(used)) return(table)
    indicators <- vapply(used, nomo_network_single_name, character(1), taken = observed)
    suppressWarnings(nomo_network_model_table(
      paste(c(syntax, sprintf("%s =~ %s", used, indicators)), collapse = "\n"),
      fixed.x = TRUE
    ))
  }
  given_table <- structure_of(model)
  fitted_table <- structure_of(full_model)

  given <- nomo_network_covariance_pairs(given_table)
  fitted <- nomo_network_covariance_pairs(fitted_table)
  hypothesized <- vapply(seq_len(nrow(h)), function(i) {
    paste(sort(c(h$source[[i]], h$target[[i]])), collapse = "\r")
  }, character(1))

  # The outcomes of the fitted model, named in the explanation: a covariance
  # lavaan no longer adds has one at an end. A covariance lavaan adds is
  # recorded when it joins two outcomes' residuals; one between a variable the
  # hypotheses bring in and another exogenous variable is the unrestricted
  # treatment of exogenous variables, not a change.
  outcomes <- unique(fitted_table$lhs[fitted_table$op == "~"])
  dropped <- given[!given$key %in% c(fitted$key, hypothesized), , drop = FALSE]
  freed <- fitted[!fitted$key %in% c(given$key, hypothesized) &
                    fitted$lhs %in% outcomes & fitted$rhs %in% outcomes, , drop = FALSE]

  # The pairs the fitted model does not join that the model as given could
  # not restrict, because a variable of the pair is not in it. A pair of the
  # model's own variables that is not joined is its own restriction, or one
  # dropped above.
  given_variables <- nomo_network_open_pairs(given_table)$structural
  open <- nomo_network_open_pairs(fitted_table)$pairs
  new_ends <- lapply(seq_len(nrow(open)), function(i) {
    setdiff(c(open$lhs[[i]], open$rhs[[i]]), given_variables)
  })
  unrelated <- open[lengths(new_ends) > 0L & !open$key %in% hypothesized, , drop = FALSE]
  new_ends <- new_ends[lengths(new_ends) > 0L & !open$key %in% hypothesized]

  lhs <- c(dropped$lhs, unrelated$lhs, freed$lhs)
  rhs <- c(dropped$rhs, unrelated$rhs, freed$rhs)

  tibble::tibble(
    relation = sprintf("%s <-> %s", lhs, rhs),
    lhs = lhs,
    rhs = rhs,
    change = rep(c("fixed_to_zero", "not_estimated", "added_by_lavaan"),
                 c(nrow(dropped), nrow(unrelated), nrow(freed))),
    outcome = vapply(seq_along(lhs), function(i) {
      paste(intersect(c(lhs[[i]], rhs[[i]]), outcomes), collapse = " and ")
    }, character(1)),
    new_variable = c(
      rep("", nrow(dropped)),
      vapply(new_ends, paste, character(1), collapse = " and "),
      rep("", nrow(freed))
    )
  )
}


nomo_network_nodes <- function(hypotheses) {
  unique(c(hypotheses$hypotheses$source, hypotheses$hypotheses$target))
}


# lavaan reads a name only as letters, digits, dots, and underscores, not
# starting with a digit. A column such as `Job Sat` is otherwise reported by
# the engine as missing from the data, which it is not (#145). Checked before
# the model is composed, which would fail on such a name first.
nomo_network_validate_names <- function(hypotheses) {
  nodes <- nomo_network_nodes(hypotheses)
  unusable <- nodes[!grepl("^[[:alpha:]._][[:alnum:]._]*$", nodes)]
  if (length(unusable)) {
    n <- length(unusable)
    stop(
      sprintf(
        paste(
          "%s cannot be %s in lavaan model syntax, which allows only letters,",
          "digits, dots, and underscores, not starting with a digit. Rename the",
          "%s in `data` and in the hypotheses."
        ),
        nomo_present_or(paste0("`", unusable, "`"), "and"),
        nomo_present_noun(n, "a variable name", "variable names"),
        nomo_present_noun(n, "column", "columns")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


nomo_network_validate_nodes <- function(hypotheses, latent, data) {
  nodes <- nomo_network_nodes(hypotheses)
  available <- unique(c(latent, names(data)))
  unknown <- setdiff(nodes, available)

  if (length(unknown)) {
    stop(
      sprintf(
        "%s neither %s in `model` nor %s in `data`: %s.",
        nomo_present_noun(length(unknown), "This hypothesis node is",
                          "These hypothesis nodes are"),
        nomo_present_noun(length(unknown), "a latent variable", "latent variables"),
        nomo_present_noun(length(unknown), "an observed column", "observed columns"),
        paste(unknown, collapse = ", ")
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


# The version of an index the fit evidence reports, from the first candidate
# lavaan gives a finite value for: "standard", "scaled", or "robust" (#145).
nomo_network_fit_version <- function(measures, candidates) {
  for (candidate in candidates) {
    if (is.finite(nomo_network_fit_measure(measures, candidate))) {
      return(if (grepl("robust", candidate)) {
        "robust"
      } else if (grepl("scaled", candidate)) {
        "scaled"
      } else {
        "standard"
      })
    }
  }
  NA_character_
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
    srmr = nomo_network_fit_measure(measures, "srmr"),
    chisq_version = nomo_network_fit_version(measures, c("chisq.scaled", "chisq")),
    index_version = nomo_network_index_version(measures)
  )
}


# CFI, TLI, and RMSEA come from the same candidates, so they share a version
# unless lavaan leaves one of them out; then the version is "mixed".
nomo_network_index_version <- function(measures) {
  versions <- unique(stats::na.omit(c(
    nomo_network_fit_version(measures, c("cfi.robust", "cfi.scaled", "cfi")),
    nomo_network_fit_version(measures, c("tli.robust", "tli.scaled", "tli")),
    nomo_network_fit_version(measures, c("rmsea.robust", "rmsea.scaled", "rmsea"))
  )))
  if (length(versions) > 1L) return("mixed")
  if (length(versions)) versions else NA_character_
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

  # A converged fit can still lack standard errors, usually because the model
  # is not identified. Without an interval nothing can be said about where the
  # relation lies, so the point estimate is not compared with the prediction.
  if (!is.finite(ci_lower) || !is.finite(ci_upper)) {
    return(list(
      concordance = "not_evaluable",
      interpretation = paste(
        "A point estimate was obtained, but its standard error was",
        "unavailable, so uncertainty could not be evaluated and the estimate",
        "is not compared with the prediction."
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
        "region was supplied. A non-significant p value is not treated as",
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

  direction_ok <- isTRUE(nomo_network_direction_correct(estimate, prediction))
  magnitude_missed <- direction_ok && isTRUE(hypothesis$magnitude_specified)

  # The estimate is outside the region. With the predicted direction and a
  # researcher-specified magnitude, it missed that magnitude on one side:
  # beyond the bound it had to stay within, or short of the bound it had to
  # reach.
  above <- abs(estimate) > max(abs(c(hypothesis$lower, hypothesis$upper)))
  missed <- sprintf(
    if (above) {
      "larger in magnitude than the researcher-specified region %s allows"
    } else {
      "smaller in magnitude than the researcher-specified region %s requires"
    },
    hypothesis$region
  )

  if (!isTRUE(overlap)) {
    return(list(
      concordance = "inconsistent",
      interpretation = if (magnitude_missed) {
        paste0(
          "The estimate has the predicted direction, but the estimate and its ",
          "whole confidence interval are ", missed, "."
        )
      } else {
        paste(
          "The estimate and confidence interval do not overlap the",
          "researcher-specified theoretical region."
        )
      }
    ))
  }

  if (magnitude_missed) {
    return(list(
      concordance = if (above) {
        "direction_concordant_above_magnitude"
      } else {
        "direction_concordant_below_magnitude"
      },
      interpretation = paste0(
        "The estimate has the predicted direction but is ", missed,
        ". Its confidence interval still overlaps that region."
      )
    ))
  }

  list(
    concordance = "inconclusive",
    interpretation = paste(
      "The point estimate is outside the predicted region, but the",
      "confidence interval still overlaps values compatible with theory."
    )
  )
}


nomo_network_node_type <- function(node, latent, data) {
  if (node %in% latent) return("latent")
  if (node %in% names(data)) return("observed")
  "unknown"
}


nomo_network_scope <- function(source_type,
                               target_type,
                               relation_type,
                               residual = FALSE) {
  # An association with an endpoint the fitted model predicts is estimated on
  # what the predictors leave of that endpoint, whatever the endpoints' types.
  if (isTRUE(residual)) return("residual_association")

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


# The measurement evidence that qualifies the structural evidence: loadings,
# variances, convergence, engine warnings, and the fit of the measurement model
# alone. `fit_evidence` is that fit; `measurement_fit` says where it came from
# (see nomo_network_measurement_fit()), and "not_computed" judges no fit.
nomo_network_measurement_context <- function(fit,
                                             standardized_solution,
                                             parameter_estimates,
                                             fit_evidence,
                                             converged,
                                             warnings,
                                             guidance,
                                             measurement_fit = "fitted") {
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

  # The fit judged here is the measurement model's own, never the network's:
  # misfit the structural restrictions add is theory strain, and the model-fit
  # stream reports it (#145). `measurement_fit` says where `fit_evidence` came
  # from; a context computed for a refit judges no fit at all.
  status <- if (!length(latent)) "no_latent" else measurement_fit
  fit_flags <- if (status %in% c("fitted", "same")) {
    nomo_network_fit_flags(fit_evidence, guidance$fit_reference)
  } else {
    character()
  }

  low_loading_n <- sum(loading_table$attention == "review", na.rm = TRUE)
  negative_variance_n <- sum(improper$negative_variance, na.rm = TRUE)

  # A converged fit whose information matrix cannot be inverted returns its
  # estimates without standard errors: no estimated parameter has one.
  standard_errors <- suppressWarnings(
    as.numeric(as.data.frame(parameter_estimates)$se)
  )
  no_standard_errors <- isTRUE(converged) && nrow(parameter_estimates) > 0L &&
    !any(is.finite(standard_errors) & standard_errors > 0)

  attention <- if (!isTRUE(converged) || negative_variance_n > 0L ||
                   no_standard_errors) {
    "concern"
  } else if (low_loading_n > 0L || length(fit_flags) || length(warnings)) {
    "review"
  } else {
    "info"
  }

  # Each note is a sentence that names the values and the references, so the
  # flag can be judged where it is printed (#145).
  notes <- character()
  if (!isTRUE(converged)) notes <- c(notes, "The network model did not converge.")
  if (no_standard_errors) {
    notes <- c(
      notes,
      paste(
        "The standard errors could not be computed, which usually means the",
        "model is not identified."
      )
    )
  }
  if (negative_variance_n > 0L) {
    negative <- improper[improper$negative_variance, , drop = FALSE]
    notes <- c(notes, sprintf(
      "%s: %s.",
      nomo_present_noun(negative_variance_n, "Negative variance estimate",
                        "Negative variance estimates"),
      paste0(negative$variable, " (", nomo_present_stat(negative$estimate, "estimate"),
             ")", collapse = ", ")
    ))
  }
  if (low_loading_n > 0L) {
    low <- loading_table[loading_table$attention == "review", , drop = FALSE]
    reference <- low$review_reference[[1L]]
    notes <- c(notes, sprintf(
      "%s below the review reference of %s in absolute value: %s.",
      nomo_present_noun(low_loading_n, "Standardized loading", "Standardized loadings"),
      nomo_present_stat(reference, "loading"),
      paste0(low$item, " (", nomo_present_stat(low$loading, "loading", reference = reference),
             ")", collapse = ", ")
    ))
  }
  if (length(fit_flags)) {
    notes <- c(notes, sprintf(
      "In the measurement model alone, %s.", nomo_present_or(fit_flags, "and")
    ))
  }
  if (identical(status, "no_latent")) {
    notes <- c(notes, paste(
      "The model has no latent variables, so there is no measurement model to",
      "evaluate."
    ))
  }
  if (identical(status, "failed")) {
    notes <- c(notes, paste(
      "The measurement model alone could not be fitted, so its fit is not part",
      "of this context."
    ))
  }
  if (length(warnings)) {
    notes <- c(notes, sprintf(
      "Engine %s captured: %d (see the decision log).",
      nomo_present_noun(length(warnings), "warning", "warnings"), length(warnings)
    ))
  }
  if (!length(notes)) {
    notes <- "No measurement-context flag was raised."
  }

  summary <- tibble::tibble(
    attention = attention,
    converged = isTRUE(converged),
    latent_constructs = length(latent),
    loading_review_flags = low_loading_n,
    negative_variance_flags = negative_variance_n,
    global_fit_review_flags = length(fit_flags),
    engine_warning_count = length(warnings),
    observation = paste(notes, collapse = " ")
  )

  list(
    summary = summary,
    loadings = loading_table,
    variances = improper,
    fit_reference_flags = fit_flags
  )
}


# Each fit index beyond its configured review reference, as a clause that names
# the value and the reference in the same format: "TLI 0.939 is below 0.950".
nomo_network_fit_flags <- function(fit_evidence, refs) {
  if (!is.list(refs) || !is.data.frame(fit_evidence) || !nrow(fit_evidence)) {
    return(character())
  }
  checks <- list(
    cfi = c("CFI", "fit_bounded", "below"), tli = c("TLI", "fit", "below"),
    rmsea = c("RMSEA", "fit", "above"), srmr = c("SRMR", "fit", "above")
  )
  flags <- character()
  for (index in names(checks)) {
    reference <- suppressWarnings(as.numeric(refs[[index]])[1L])
    value <- if (index %in% names(fit_evidence)) fit_evidence[[index]][[1L]] else NA_real_
    if (!length(reference) || !is.finite(reference) || !is.finite(value)) next
    check <- checks[[index]]
    beyond <- if (check[[3L]] == "below") value < reference else value > reference
    if (beyond) {
      flags <- c(flags, sprintf(
        "%s %s is %s %s", check[[1L]],
        nomo_present_stat(value, check[[2L]], reference = reference), check[[3L]],
        nomo_present_stat(reference, check[[2L]])
      ))
    }
  }
  flags
}


# The measurement model alone (#145): the fitted network with its structural
# part saturated, which is how Anderson and Gerbing (1988) separate the two
# steps. Each pair of structural variables -- the factors that are not
# themselves indicators, and the observed variables outside the factor
# definitions -- that no path or covariance joins gains a free covariance. A
# pair joined by a path never gains one, so in an acyclic model the latent
# covariances become unrestricted: the model fits as the measurement model with
# every structural variable correlated does, and the network is nested in it.
# Everything else in the model, its labels and constraints included, is kept
# as written.
nomo_network_saturating_lines <- function(partable) {
  open <- nomo_network_open_pairs(partable)$pairs
  sprintf("%s ~~ %s", open$lhs, open$rhs)
}


# The structural variables of a parameter table, and the pairs of them that no
# path or covariance joins, sorted within and across pairs. A covariance held
# at its sample value or fixed in the syntax joins its pair: only a pair the
# model says nothing about is open.
nomo_network_open_pairs <- function(partable) {
  partable <- as.data.frame(partable)
  indicators <- unique(partable$rhs[partable$op == "=~"])
  factors <- unique(partable$lhs[partable$op == "=~"])
  joined_rows <- partable$op == "~" | (partable$op == "~~" & partable$lhs != partable$rhs)
  variables <- unique(c(partable$lhs[partable$op %in% c("~", "~~")],
                        partable$rhs[partable$op %in% c("~", "~~")]))
  structural <- setdiff(unique(c(factors, variables)), indicators)
  pairs <- if (length(structural) < 2L) {
    matrix(character(), nrow = 2L)
  } else {
    utils::combn(sort(structural), 2L)
  }

  lhs <- partable$lhs[joined_rows]
  rhs <- partable$rhs[joined_rows]
  joined <- c(paste(lhs, rhs), paste(rhs, lhs))
  open <- !paste(pairs[1L, ], pairs[2L, ]) %in% joined
  list(
    structural = structural,
    pairs = data.frame(
      lhs = pairs[1L, open], rhs = pairs[2L, open],
      key = paste(pairs[1L, open], pairs[2L, open], sep = "\r"),
      stringsAsFactors = FALSE
    )
  )
}


# The measurement model alone, fitted with the network's settings, and the
# test of the structural restrictions against it. `status` is "same" when the
# structural part is already saturated, so the network is its own measurement
# model; "no_latent" when there is no measurement model; and "failed" when it
# cannot be fitted or is not nested as expected.
nomo_network_measurement_fit <- function(fit, fit_args, fit_evidence) {
  none <- tibble::tibble(chisq_diff = NA_real_, df_diff = NA_real_,
                         p_value = NA_real_, method = NA_character_)
  out <- list(status = "failed", fit_evidence = fit_evidence[0, , drop = FALSE],
              structural_test = none, lines = character())
  latent <- tryCatch(as.character(lavaan::lavNames(fit, type = "lv")),
                     error = function(e) character())
  if (!length(latent)) {
    out$status <- "no_latent"
    return(out)
  }
  partable <- tryCatch(lavaan::parTable(fit), error = function(e) NULL)
  if (is.null(partable)) return(out)
  lines <- nomo_network_saturating_lines(partable)
  out$lines <- lines
  if (!length(lines)) {
    out$status <- "same"
    out$fit_evidence <- fit_evidence
    out$structural_test$df_diff <- 0
    return(out)
  }

  fit_args$model <- paste(
    c(fit_args$model, "", "# Structural part saturated by nomologR", lines),
    collapse = "\n"
  )
  saturated <- tryCatch(
    suppressWarnings(do.call(lavaan::sem, fit_args)),
    error = function(e) NULL
  )
  if (is.null(saturated) ||
      !isTRUE(tryCatch(lavaan::lavInspect(saturated, "converged"),
                       error = function(e) FALSE))) {
    return(out)
  }
  evidence <- nomo_network_fit_evidence(saturated)
  df_diff <- fit_evidence$df[[1L]] - evidence$df[[1L]]
  if (!is.finite(df_diff) || df_diff <= 0) return(out)
  out$status <- "fitted"
  out$fit_evidence <- evidence

  # lavaan conditions on exogenous covariates with categorical outcomes;
  # a covariate the saturated model correlates is no longer conditioned on,
  # and the two statistics are then not comparable.
  conditional <- function(f) isTRUE(lavaan::lavInspect(f, "options")$conditional.x)
  if (conditional(fit) != conditional(saturated)) return(out)
  test <- tryCatch(
    suppressWarnings(lavaan::lavTestLRT(fit, saturated)),
    error = function(e) NULL
  )
  if (is.null(test) || nrow(test) < 2L) return(out)
  heading <- paste(attr(test, "heading"), collapse = " ")
  out$structural_test <- tibble::tibble(
    chisq_diff = as.numeric(test[["Chisq diff"]][[2L]]),
    df_diff = as.numeric(test[["Df diff"]][[2L]]),
    p_value = as.numeric(test[["Pr(>Chisq)"]][[2L]]),
    method = if (grepl("method = \"", heading, fixed = TRUE)) {
      sub(".*method = \"([^\"]+)\".*", "\\1", heading)
    } else {
      "standard"
    }
  )
  out
}


# The fit of the network model as fitted, against the configured references.
# It is a stream of its own (#145): misfit in the measurement model alone
# belongs to the measurement context, and misfit the structural restrictions
# add is strain on the theory's structure.
nomo_network_model_fit_context <- function(fit_evidence, measurement, guidance) {
  flags <- nomo_network_fit_flags(fit_evidence, guidance$fit_reference)
  test <- measurement$structural_test
  test_text <- if (is.data.frame(test) && nrow(test) && is.finite(test$chisq_diff[[1L]])) {
    nomo_present_chisq(test$chisq_diff[[1L]], test$df_diff[[1L]], test$p_value[[1L]],
                       delta = TRUE)
  } else {
    ""
  }
  measurement_flags <- nomo_network_fit_flags(measurement$fit_evidence, guidance$fit_reference)

  observation <- if (!length(flags)) {
    "No fit index of the network model is beyond its review reference."
  } else {
    paste0("In the network model, ", nomo_present_or(flags, "and"), ".")
  }
  if (length(flags)) {
    observation <- paste(observation, switch(
      measurement$status,
      fitted = if (length(measurement_flags)) {
        paste0(
          "The measurement model alone is flagged as well (see the measurement ",
          "context)", if (nzchar(test_text)) paste0(", and the structural ",
          "restrictions add ", test_text) else "", "."
        )
      } else {
        paste0(
          "The measurement model alone meets the references",
          if (nzchar(test_text)) paste0(", and the structural restrictions add ", test_text) else "",
          ", so the misfit is in the structural part of the network."
        )
      },
      same = paste(
        "The structural part adds no restriction to the measurement model, so",
        "the misfit is in the measurement model."
      ),
      no_latent = paste(
        "The model has no latent variables, so the misfit is in its",
        "structural restrictions."
      ),
      paste(
        "The measurement model alone could not be fitted, so the misfit is not",
        "attributed to the measurement or the structural part."
      )
    ))
  }

  # The advice follows the attribution: only misfit the measurement model
  # does not share is strain on the theory's structure.
  recommendation <- if (!length(flags)) {
    paste(
      "Report the fit of the network model beside that of the measurement",
      "model alone, so that misfit of either kind stays visible."
    )
  } else {
    switch(
      measurement$status,
      fitted = if (length(measurement_flags)) {
        paste(
          "Review the measurement context first: misfit the measurement model",
          "alone shows is a measurement problem. Only the misfit the structural",
          "restrictions add is strain on the theory's structure (Anderson &",
          "Gerbing, 1988)."
        )
      } else {
        paste(
          "Misfit beyond the measurement model's comes from the structural",
          "restrictions, such as the relations the hypothesized paths fix to zero.",
          "Read it as strain on the theory's structure, not as a measurement",
          "problem (Anderson & Gerbing, 1988)."
        )
      },
      same = paste(
        "Review the measurement context before the hypotheses: this misfit is a",
        "measurement problem, not strain on the theory's structure."
      ),
      no_latent = paste(
        "Check the relations the model fixes to zero, those the hypothesized",
        "paths fix included, against the theory before interpreting the",
        "estimates."
      ),
      paste(
        "Fit the measurement model alone, for example with nomo_cfa(), before",
        "reading the misfit as a measurement problem or as strain on the",
        "theory's structure."
      )
    )
  }

  list(
    attention = if (length(flags)) "review" else "info",
    flags = flags,
    observation = observation,
    recommendation = recommendation
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
                                             equivalence_alpha = 0.05,
                                             extra_variance = NULL) {
  h <- hypotheses$hypotheses
  rows <- vector("list", nrow(h))

  measurement_attention <- measurement_context$summary$attention[[1L]]
  measurement_observation <- measurement_context$summary$observation[[1L]]

  # Variables the fitted model predicts: an outcome of a directed path, and an
  # indicator of a factor, which the loading predicts (a first-order factor
  # under a higher-order one included). A covariance involving one of them is
  # a covariance of its residual. A fit without a parameter table has none.
  fitted <- as.data.frame(parameter_estimates)
  predicted <- unique(as.character(c(
    fitted$lhs[fitted$op == "~"],
    fitted$rhs[fitted$op == "=~"]
  )))

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

    # The uncertainty in a single indicator's reliability, added to the
    # estimate's variance as Oberski and Satorra (2013) derive. The interval
    # and p value follow from the larger standard error.
    added <- if (hyp$id %in% names(extra_variance)) extra_variance[[hyp$id]] else 0
    if (added > 0 && is.finite(se)) {
      se <- sqrt(se^2 + added)
      critical <- stats::qnorm(0.975)
      ci_lower <- estimate - critical * se
      ci_upper <- estimate + critical * se
      p_value <- 2 * stats::pnorm(-abs(estimate / se))
    } else {
      added <- 0
    }

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

    # A relation without an estimate is not labeled: nothing was estimated,
    # as a residual or otherwise, and it keeps its ordinary scope.
    labeled <- identical(hyp$relation_type, "association") && is.finite(estimate)
    residual_of <- if (labeled) {
      intersect(c(hyp$source, hyp$target), predicted)
    } else {
      character()
    }

    if (length(residual_of)) {
      base_interpretation <- paste(
        base_interpretation,
        sprintf(
          paste(
            "The fitted model also predicts %s from other variables, by a",
            "directed path or a factor loading, so this estimate is a residual",
            "association: the association that remains after those predictors,",
            "not the overall association between `%s` and `%s`."
          ),
          paste0("`", residual_of, "`", collapse = " and "),
          hyp$source, hyp$target
        )
      )
    }

    if (identical(hyp$prediction, "negligible") &&
        isTRUE(hyp$magnitude_specified) &&
        is.finite(eq_ci[["lower"]]) &&
        is.finite(eq_ci[["upper"]])) {
      base_interpretation <- paste(
        base_interpretation,
        sprintf(
          paste(
            "Negligibility is evaluated using the normal-approximation",
            "equivalence interval (%s, alpha = %s), not by p > .05."
          ),
          nomo_present_ci_label(1 - 2 * equivalence_alpha),
          nomo_present_level(equivalence_alpha)
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
        "The measurement context also requires review.",
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
        hyp$relation_type,
        residual = length(residual_of) > 0L
      ),
      relation_type = hyp$relation_type,
      prediction = hyp$prediction,
      theoretical_region = hyp$region,
      scale = hyp$scale,
      origin = hyp$origin,
      estimate = estimate,
      se = se,
      se_reliability_added = sqrt(added),
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
                                  sample_role,
                                  measurement_fit = FALSE) {
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

  n_used <- nomo_network_cases_used(fit)

  # The measurement model alone is fitted for the samples reported, not for
  # the refits a sensitivity analysis makes.
  measurement <- if (isTRUE(measurement_fit) && converged) {
    nomo_network_measurement_fit(fit, fit_args, fit_evidence)
  } else {
    list(
      status = "not_computed", fit_evidence = fit_evidence[0, , drop = FALSE],
      structural_test = tibble::tibble(chisq_diff = NA_real_, df_diff = NA_real_,
                                       p_value = NA_real_, method = NA_character_),
      lines = character()
    )
  }

  measurement_context <- nomo_network_measurement_context(
    fit = fit,
    standardized_solution = standardized_solution,
    parameter_estimates = parameter_estimates,
    fit_evidence = measurement$fit_evidence,
    converged = converged,
    warnings = engine_warnings,
    guidance = guidance,
    measurement_fit = measurement$status
  )
  measurement_context$fit <- measurement$fit_evidence
  measurement_context$structural_test <- measurement$structural_test
  measurement_context$fit_status <- measurement$status
  model_fit <- nomo_network_model_fit_context(fit_evidence, measurement, guidance)

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
    n_used = n_used,
    converged = converged,
    engine_warnings = engine_warnings,
    fit = fit,
    fit_evidence = fit_evidence,
    model_fit = model_fit,
    parameter_estimates = parameter_estimates,
    standardized_solution = standardized_solution,
    measurement_context = measurement_context,
    hypothesis_evidence = hypothesis_evidence,
    estimator = if (is.null(estimator_requested)) {
      nomo_network_fitted_estimator(fit)
    } else {
      estimator_requested
    },
    estimator_source = estimator_source
  )
}


# The cases lavaan analyzed, which listwise deletion can make fewer than the
# rows supplied (#145); NA when the fit cannot say.
nomo_network_cases_used <- function(fit) {
  n <- suppressWarnings(sum(as.numeric(unlist(tryCatch(
    lavaan::lavInspect(fit, "nobs"),
    error = function(e) NA_real_
  )))))
  if (is.finite(n) && n > 0) n else NA_real_
}


# The estimator lavaan chose for a fit made without one requested, so the
# result records what was fitted. lavaan reports WLSMV as DWLS with a
# scaled-and-shifted test statistic.
nomo_network_fitted_estimator <- function(fit) {
  options <- tryCatch(
    lavaan::lavInspect(fit, "options"),
    error = function(e) list()
  )
  estimator <- toupper(as.character(options$estimator)[1L])
  wlsmv <- identical(estimator, "DWLS") && "scaled.shifted" %in% options$test
  if (wlsmv) "WLSMV" else estimator
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


# Whether an estimate has the sign opposite to a directional prediction. A
# negligible prediction, or a missing estimate, has no direction to oppose.
nomo_network_opposite_direction <- function(prediction, estimate) {
  predicted <- c(positive = 1, negative = -1)[as.character(prediction)]
  !is.na(predicted) & is.finite(estimate) & sign(estimate) == -predicted
}


nomo_network_replication_evidence <- function(primary, validation) {
  a <- primary$hypothesis_evidence
  b <- validation$hypothesis_evidence

  rows <- vector("list", nrow(a))

  # Statuses whose interval reaches the theoretical region.
  supportive <- c(
    "concordant",
    "directionally_concordant_imprecise",
    "direction_concordant_below_magnitude",
    "direction_concordant_above_magnitude"
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
      # The same sign in both samples is a replicated direction only when it is
      # the predicted one; the stored status covers both, so the
      # interpretation and the log say which (#145).
      interpretation <- if (nomo_network_opposite_direction(prediction, est_a)) {
        paste(
          "Both samples estimate the relation in the direction opposite to the",
          "prediction. The shared direction is not a replication of the",
          "prediction; uncertainty prevents a stronger statement against it."
        )
      } else {
        paste(
          "The estimated direction is the same across samples, but uncertainty",
          "or magnitude evidence prevents stronger replication language."
        )
      }
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
                                      sample_role = "primary",
                                      ordered_detected = character(),
                                      model_fit = NULL,
                                      cases = NULL,
                                      model_changes = NULL,
                                      stage = "network") {
  log <- nomo_log_new()

  log <- nomo_log_add(
    log,
    stage = stage,
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
        "model fit, uncertainty, and the a priori/post hoc distinction."
      )
    } else {
      "Investigate estimation/model problems before interpreting theory."
    }
  )

  # The cases analyzed, as nomo_cfa() records them: listwise deletion can drop
  # rows without a word from lavaan (#145).
  if (!is.null(cases) && is.finite(cases$n_used)) {
    dropped <- max(0, cases$data_n - cases$n_used)
    log <- nomo_log_add(
      log,
      stage = stage,
      object = sample_role,
      metric = "cases_used",
      value = cases$n_used,
      reference = paste(
        "Case retention stays visible because missing-data handling can change",
        "the analyzed sample"
      ),
      severity = if (dropped > 0) "review" else "info",
      observation = if (dropped > 0) {
        sprintf(
          "%d of %d rows of the %s sample were analyzed; %s %s dropped by %s.",
          as.integer(cases$n_used), as.integer(cases$data_n), sample_role,
          nomo_present_count(as.integer(dropped), "row"),
          nomo_present_noun(dropped, "was", "were"),
          if (is.null(cases$missing)) "listwise deletion" else {
            sprintf("lavaan's handling of missing values (missing = \"%s\")", cases$missing)
          }
        )
      } else {
        sprintf("All %d rows of the %s sample were analyzed.",
                as.integer(cases$data_n), sample_role)
      },
      recommendation = if (dropped > 0) {
        paste(
          "Confirm that the case loss follows the intended missing-data strategy.",
          "With continuous indicators, missing = \"fiml\" uses every case that has",
          "data; nomo_missing() compares the strategies."
        )
      } else {
        "Report the number of cases analyzed with the network's estimates."
      }
    )
  }

  measurement_row <- measurement_context$summary[1L, , drop = FALSE]
  log <- nomo_log_add(
    log,
    stage = stage,
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

  # The fit of the network model, apart from the measurement model's (#145).
  if (!is.null(model_fit)) {
    log <- nomo_log_add(
      log,
      stage = stage,
      object = sample_role,
      metric = "model_fit",
      value = length(model_fit$flags),
      reference = "Fit references in nomo_defaults()$fit_reference",
      severity = model_fit$attention,
      observation = model_fit$observation,
      recommendation = model_fit$recommendation
    )
  }

  # Relations the added paths fixed to zero or lavaan added (#145). A relation
  # of a variable the hypotheses bring into the model is a restriction too.
  changes <- if (is.null(model_changes)) data.frame() else model_changes
  for (i in seq_len(nrow(changes))) {
    kind <- changes$change[[i]]
    fixed <- kind %in% c("fixed_to_zero", "not_estimated")
    outcome <- strsplit(changes$outcome[[i]], " and ", fixed = TRUE)[[1L]]
    log <- nomo_log_add(
      log,
      stage = stage,
      object = changes$relation[[i]],
      metric = if (fixed) "relation_constrained" else "relation_auto_freed",
      value = 0,
      reference = switch(
        kind,
        fixed_to_zero = "Free in the model as given",
        not_estimated = "Absent from the model as given",
        "In neither the model nor the hypotheses"
      ),
      severity = if (fixed) "review" else "info",
      observation = if (identical(kind, "fixed_to_zero")) {
        sprintf(
          paste(
            "`%s`, estimated in the model as given, is fixed to zero in the fitted",
            "model: with the hypothesized paths, %s %s, and lavaan does not",
            "covary an outcome's residual with a variable that does not predict",
            "it."
          ),
          changes$relation[[i]],
          nomo_present_or(paste0("`", outcome, "`"), "and"),
          nomo_present_noun(length(outcome), "is an outcome", "are outcomes")
        )
      } else if (fixed) {
        sprintf(
          paste(
            "`%s` is fixed to zero in the fitted model: the hypotheses bring %s",
            "into the model, and neither they nor lavaan's defaults relate the",
            "two."
          ),
          changes$relation[[i]],
          nomo_present_or(paste0(
            "`", strsplit(changes$new_variable[[i]], " and ", fixed = TRUE)[[1L]], "`"
          ), "and")
        )
      } else {
        sprintf(
          paste(
            "`%s` is estimated in the fitted model although neither the model",
            "nor the hypotheses name it: lavaan covaries the residuals of the",
            "outcomes the hypothesized paths create."
          ),
          changes$relation[[i]]
        )
      },
      recommendation = if (fixed) {
        paste(
          "Fixing a relation to zero is a restriction of the network, and its",
          "misfit counts against the theory's structure. If the theory allows",
          "the relation, add it as a hypothesis or write it in `model`."
        )
      } else {
        paste(
          "The hypothesis estimates are conditional on this relation. If the",
          "theory excludes it, fix it to zero in `model`."
        )
      }
    )
  }

  added <- model_additions[model_additions$added_from_hypothesis, , drop = FALSE]
  if (nrow(added)) {
    for (i in seq_len(nrow(added))) {
      log <- nomo_log_add(
        log,
        stage = stage,
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
      stage = stage,
      object = sample_role,
      metric = "ordered_indicators",
      reference = paste(ordered, collapse = ", "),
      severity = "info",
      observation = sprintf(
        "%s %s declared.",
        nomo_present_count(length(ordered), "ordered indicator"),
        nomo_present_noun(length(ordered), "was", "were")
      ),
      recommendation = "Interpret SEM estimates using the categorical-data estimator."
    )
  }
  log <- nomo_ordered_detected_log(log, ordered_detected, "network")

  if (!is.null(estimator)) {
    log <- nomo_log_add(
      log,
      stage = stage,
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
      stage = stage,
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
      "direction_concordant_above_magnitude",
      "inconclusive",
      "not_confirmable_without_sesoi"
    )) {
      "review"
    } else {
      "info"
    }

    log <- nomo_log_add(
      log,
      stage = stage,
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
      # Both samples against the predicted direction is no replication of it.
      opposite <- identical(row$replication_status[[1L]], "direction_replicated_but_uncertain") &&
        all(c("prediction", "primary_estimate") %in% names(row)) &&
        isTRUE(nomo_network_opposite_direction(row$prediction[[1L]], row$primary_estimate[[1L]]))
      severity <- if (opposite || row$replication_status[[1L]] %in% c(
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
#' are not already present in `model` can be added before fitting, so the
#' theory object itself can define the structural portion of the network. Each
#' addition, and each relation the additions change, is recorded.
#'
#' Directed `A -> B` hypotheses map to lavaan regression paths `B ~ A`.
#' Association `A <-> B` hypotheses map to covariance paths `A ~~ B`.
#'
#' A directed path is in the model when `model` writes it. An association is
#' judged against the model that will be fitted, which is `model` with the
#' hypothesized directed paths in place: it is in the model when `model` writes
#' it or when lavaan adds it there automatically, as it does between exogenous
#' factors. `model_relations` records which relations were already in the
#' model and which were added.
#'
#' An added path changes more than its own relation. `lavaan::sem()` covaries
#' exogenous factors with one another, holds the covariances of observed
#' exogenous predictors at their sample values, and covaries the residuals of
#' outcomes that predict no other variable. It relates no other pair of
#' variables that neither `model` nor the hypotheses join: not a factor and
#' an observed predictor, not an outcome and an exogenous variable, and not an
#' outcome that predicts another variable and a variable it does not predict.
#' So once a path makes a variable an outcome, every relation it had with an
#' exogenous variable other than its predictors is fixed to zero, although
#' `model` estimated it, and two outcomes can gain a residual covariance that
#' neither `model` nor the hypotheses name. A variable the hypotheses bring
#' into the model is related only as these rules allow: an observed predictor
#' of a factor is uncorrelated with the exogenous factors, for example. These
#' zeros are restrictions of the network, and their misfit counts against the
#' theory's structure. `model_changes` lists each relation fixed to zero
#' although `model` estimated it (`"fixed_to_zero"`), each relation of a
#' variable the hypotheses bring in that the fitted model fixes to zero
#' (`"not_estimated"`), and each residual covariance of two outcomes that
#' lavaan added (`"added_by_lavaan"`). The decision log records the first two
#' for review (`relation_constrained`) and the third for information
#' (`relation_auto_freed`). A relation the theory allows belongs in the
#' hypotheses or in `model`.
#'
#' When the fitted model also predicts an endpoint of an association, `A ~~ B`
#' is a covariance between residuals. The model predicts a variable when a
#' directed path points to it, and when it is an indicator of a factor, which
#' includes a factor that loads on a higher-order factor. The estimate is then
#' the association that remains after those predictors, a residual
#' association, and it can differ from the overall association in size and in
#' sign. Such a hypothesis has `evidence_scope` `"residual_association"`, its
#' interpretation says so, and the decision log flags it for review. If the
#' theory concerns the overall association, evaluate it in a model that does
#' not predict those variables.
#'
#' A pair of variables carries a directed path or an association, not both:
#' `nomo_network()` stops when an association hypothesis names two variables
#' that the model to be fitted joins with a directed path. It also stops when
#' a hypothesis would add a path opposite to another, because a reciprocal
#' pair is not identified without further restrictions. A reciprocal pair
#' that `model` itself writes is fitted as written, and hypotheses about
#' either direction are evaluated; whether that model is identified, by
#' instruments for example, is for the researcher to establish.
#'
#' For quantitative `negligible(within = ...)` predictions, `nomo_network()`
#' evaluates the smallest effect size of interest (SESOI), the region the
#' researcher treats as negligible, using a normal-approximation equivalence
#' confidence interval. With the default `equivalence_alpha = .05`, this is a
#' 90 percent interval, corresponding to the usual two one-sided tests logic.
#' A bare `negligible()` prediction remains non-confirmable from `p > .05`.
#'
#' The function can also fit the same prespecified model in a validation sample.
#' Pass `validation_data` explicitly, or pass a `nomo_split` object as `data` to
#' use its calibration and validation subsets. No model relation is added or
#' removed on the basis of validation results. The validation sample's rows in
#' the decision log have the stage `"network_validation"`, so they are never
#' read as a second entry for the same relation.
#'
#' Rows with a missing value are dropped by lavaan's default listwise
#' deletion. `n_used` records the cases analyzed, the output reports them
#' beside the rows supplied ("Cases: 479 of 800"), and the decision log flags
#' the loss for review (`cases_used`). With continuous indicators,
#' `missing = "fiml"` uses every case that has data.
#'
#' `print()` shows each hypothesis's concordance, estimate, and the interval
#' its concordance was judged on, the replication evidence, and what is
#' flagged about the cases, the measurement context, the model fit, and the
#' relations the added paths changed. `summary()` adds the fit of the network
#' and of the measurement model alone, the changed relations, each prediction
#' in full, the concordance counts, and every flag with its recommendation.
#'
#' @section Measurement context and model fit:
#' The fit of the network model mixes two sources of misfit, the measurement
#' model and the structural restrictions, which Anderson and Gerbing (1988)
#' separate by fitting the measurement model first. `nomo_network()` also fits
#' the measurement model alone: the network with its structural part
#' saturated, in which every pair of factors and observed variables outside
#' the factor definitions that no path or covariance joins is allowed to
#' covary. The measurement context judges the measurement model's own fit,
#' with its loadings and variances. The model fit, a separate decision-log row
#' (`model_fit`), judges the network's fit and, when the measurement model
#' meets the references, attributes the misfit to the structural part. The
#' difference test between the two (`structural_test`, a Delta chi-square from
#' `lavaan::lavTestLRT()`, scaled as the estimator requires) tests the
#' structural restrictions: the pairs the measurement model alone frees, those
#' `model` itself leaves out as well as those `model_changes` lists as fixed
#' to zero. A model without latent variables has no measurement model to
#' judge.
#'
#' The fit indices are the robust versions when lavaan reports them, and the
#' chi-square is the scaled one; `chisq_version` and `index_version` in
#' `fit_evidence` say which, and `summary()` prints them.
#'
#' @section Concordance:
#' `concordance` in `hypothesis_evidence` compares each estimate and its
#' confidence interval with the theoretical region of its prediction, on the
#' scale the prediction names (standardized by default). For `positive()` and
#' `negative()` predictions the interval is the 95 percent confidence
#' interval. For `negligible(within = ...)` it is the equivalence interval,
#' with confidence `1 - 2 * equivalence_alpha` (90 percent by default). A
#' bound given as `min`, `max`, or `within` belongs to the region; zero, the
#' bound of a prediction of direction alone, does not. The values, in the
#' order the rules are applied:
#'
#' * `"not_evaluable"`: the model did not converge, no parameter of the fitted
#'   model matches the relation, or the estimate has no standard error, as
#'   happens when the model is not identified. The estimate is not compared
#'   with the prediction.
#' * `"not_confirmable_without_sesoi"`: a bare `negligible()` prediction, which
#'   gives no region to compare the interval with.
#' * `"concordant"`: the whole interval lies inside the region.
#' * `"directionally_concordant_imprecise"`: the estimate lies inside the
#'   region, and its interval extends outside it.
#' * `"inconsistent"`: the estimate and its whole interval lie outside the
#'   region. A relation with the predicted sign is inconsistent when its
#'   interval excludes the predicted magnitude.
#' * `"direction_concordant_below_magnitude"`: for a `positive()` or
#'   `negative()` prediction with `min` or `max`, the estimate has the
#'   predicted sign and is smaller in magnitude than the region requires, and
#'   its interval reaches the region.
#' * `"direction_concordant_above_magnitude"`: the same, with an estimate
#'   larger in magnitude than the region allows.
#' * `"inconclusive"`: any other estimate outside the region whose interval
#'   reaches the region, such as an estimate of the wrong sign whose interval
#'   includes values of the predicted sign.
#'
#' The decision log records `"concordant"` for information, `"inconsistent"`
#' and `"not_evaluable"` as concerns, and the other values for review. None is
#' a verdict on validity: each is one piece of evidence, read with the
#' measurement context, the fit, and whether the prediction was made a priori.
#' Printed output and plots show the values in short words, such as "In
#' region, imprecise" for `"directionally_concordant_imprecise"`, which also
#' reads for a `negligible()` prediction; the stored values do not change.
#'
#' @section Evidence scope:
#' `evidence_scope` in `hypothesis_evidence` names what kind of parameter the
#' estimate is. For a directed path: `"latent_structural"` (factor to factor),
#' `"latent_to_observed_outcome"`, `"observed_to_latent"`, and
#' `"observed_structural"`. For an association: `"latent_association"`,
#' `"latent_observed_association"`, `"observed_association"`, and
#' `"residual_association"` when the fitted model predicts an endpoint, by a
#' directed path or a factor loading, whatever the endpoints are. A relation
#' without an estimate keeps the scope its endpoints give it.
#'
#' @section Replication status:
#' With a validation sample, `replication_status` in `replication_evidence`
#' compares the two samples' estimates and concordance. The values, in the
#' order the rules are applied:
#'
#' * `"not_evaluable"`: the relation is `"not_evaluable"` or has no estimate
#'   in at least one sample.
#' * `"sign_reversal"`, `"direction_not_replicated"`, and
#'   `"sign_change_within_uncertainty"`: a `positive()` or `negative()`
#'   prediction whose two point estimates have opposite signs. See
#'   **Replication status when the sign changes**.
#' * `"replicated_concordance"`: `"concordant"` in both samples.
#' * `"not_replicated"`: compatible with the prediction in the primary sample
#'   and `"inconsistent"` in the validation sample. Compatible means
#'   `"concordant"`, `"directionally_concordant_imprecise"`,
#'   `"direction_concordant_below_magnitude"`, or
#'   `"direction_concordant_above_magnitude"`, the values whose interval
#'   reaches the region.
#' * `"unstable"`: `"inconsistent"` in the primary sample and compatible with
#'   the prediction in the validation sample.
#' * `"replicated_inconsistency"`: `"inconsistent"` in both samples.
#' * `"direction_replicated_but_uncertain"`: a `positive()` or `negative()`
#'   prediction whose estimates have the same sign in both samples, without
#'   meeting a rule above. When that shared sign is the opposite of the
#'   prediction, the interpretation says so, and printed output labels it
#'   "Opposite in both" rather than "Same direction".
#' * `"mixed_or_inconclusive"`: any other pattern.
#'
#' The decision log records `"replicated_concordance"` for information;
#' `"sign_reversal"`, `"direction_not_replicated"`, `"not_replicated"`,
#' `"unstable"`, `"replicated_inconsistency"`, and a
#' `"direction_replicated_but_uncertain"` opposite to the prediction as
#' concerns; and the other values for review.
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
#'   absent from `model` are appended transparently before estimation: the
#'   directed paths first, then the associations the model still lacks.
#' @param ordered Optional character vector naming ordered indicators. A
#'   variable of the fitted model stored as an ordered factor, in `data` or in
#'   `validation_data`, is treated as declared whether or not it is named
#'   here, because lavaan fits such a column as ordered-categorical either
#'   way. The decision log lists the columns found this way for review. A
#'   column that is an ordered factor in only one of the two samples is
#'   declared ordered in both, so both are fitted with the same estimator;
#'   the sample that stores it as numbers is then fitted with a
#'   categorical-data estimator, and its estimates differ from a fit that
#'   treats the column as continuous. An exogenous covariate is the
#'   exception: lavaan does not model a covariate as ordered-categorical, so
#'   one stored as an ordered factor is refused. Supply it as a numeric
#'   column or as dummy-coded columns.
#' @param estimator Optional lavaan estimator. When ordered indicators are
#'   declared and `estimator = NULL`, WLSMV is requested.
#' @param missing Optional lavaan missing-data option.
#' @param std.lv Logical passed to `lavaan::sem()`. It also sets the metric of
#'   an unstandardized estimate that involves a latent variable: with `TRUE`,
#'   each factor's variance, or residual variance for an outcome, is 1; with
#'   `FALSE`, each factor takes the units of its first indicator. See `scale`
#'   in [nomo_expectations]. The decision log names the identification for
#'   each such hypothesis (`unstandardized_metric`).
#' @param control Optional optimizer-control list passed to `lavaan::sem()`.
#' @param equivalence_alpha One number strictly between 0 and .5. For
#'   quantitative negligible predictions, the equivalence confidence level is
#'   `1 - 2 * equivalence_alpha`.
#' @param single_indicators Optional composites to model as single-indicator
#'   latent variables: a named list, or a named numeric vector, whose names are
#'   observed variables that `model` uses and whose values are reliabilities,
#'   given as one number or as a [nomo_single_indicator()] record. See
#'   **Single indicators**.
#' @param guidance Guidance settings returned by `nomo_defaults()`.
#'
#' @section Relationships estimated between observed variables:
#' A hypothesis whose endpoints are latent variables is estimated with their
#' measurement error modeled. When an endpoint is an observed variable, that
#' error enters unmodeled. If the observed variable is a composite of several
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
#' Where the observed variables are composites, modeling their items as
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
#' Where they are composites whose reliability is known, `single_indicators`
#' corrects them (see **Single indicators**).
#'
#' @section Single indicators:
#' A composite named in `single_indicators` becomes the one indicator of a
#' latent variable with the composite's name, so the model syntax and the
#' hypotheses are unchanged. Its error variance is fixed at
#' \eqn{(1 - \rho)\sigma^2}, the reliability's complement times the
#' composite's variance in the data being fitted, and the relationships it
#' enters are corrected for its unreliability (Bollen, 1989; Savalei, 2019).
#' [nomo_single_indicator()] explains the method's origin and evidence, and
#' which reliability to use.
#'
#' The correction is only as good as the reliability, and Savalei (2019) found
#' that misestimating it by more than about .05 costs accuracy. So each
#' hypothesis is refitted with each composite's reliability .05 and .10 lower
#' and higher, one composite at a time, and `single_indicator_sensitivity`
#' records the estimates, intervals, and concordance at each. The decision log
#' flags any hypothesis whose concordance changes across that range.
#'
#' When a reliability's standard error is supplied, the standard errors,
#' intervals, and concordance of the hypotheses add its uncertainty, following
#' Oberski and Satorra (2013): the variance of an estimate gains its squared
#' rate of change in the reliability times the reliability's variance. The rate
#' of change is taken from the refits within .05 of the reliability. Without a
#' standard error, the log says that the standard errors treat the reliability
#' as known.
#'
#' @return A `nomo_network` object. The fields to read are:
#'
#'   * `hypothesis_evidence`: one row per hypothesis, with its prediction,
#'     estimate, interval, `concordance` with the prediction (see
#'     **Concordance**), `evidence_scope` (see **Evidence scope**), and
#'     interpretation.
#'   * `replication_evidence`: the same comparison in `validation_data`, when
#'     given, with its `replication_status` (see **Replication status**).
#'   * `fit_evidence`: global fit of the fitted model, with `chisq_version`
#'     and `index_version` naming the version of the chi-square and of CFI,
#'     TLI, and RMSEA reported: `"standard"`, `"scaled"`, or `"robust"`.
#'   * `parameter_estimates` and `standardized_solution`: `lavaan`'s parameter
#'     tables, as tibbles.
#'   * `measurement_context`: the loadings, the variances, and the fit of the
#'     measurement model alone (`fit`), with the test of the structural
#'     restrictions against it (`structural_test`), which qualify the
#'     structural evidence. See **Measurement context and model fit**.
#'   * `model_fitted` and `model_relations`: the syntax fitted, and for each
#'     hypothesis whether its relation was already in the model to be fitted
#'     (`already_in_model`) or was added (`added_from_hypothesis`).
#'   * `model_changes`: each relation the added paths fixed to zero or lavaan
#'     added (`change`), with the outcomes among its variables (`outcome`)
#'     and the variables the hypotheses brought into the model
#'     (`new_variable`).
#'   * `data_n` and `n_used`: the rows of the primary sample and the cases the
#'     fit analyzed; `validation_n` and `validation_n_used` for the validation
#'     sample.
#'   * `fit`: the `lavaan` fit, and `validation`, the validation fit, when
#'     given.
#'   * `single_indicators`: one row per composite modeled as a single
#'     indicator, with its reliability, the reliability's standard error,
#'     coefficient, and source, the composite's variance, and the error
#'     variance fixed. Empty when `single_indicators` is not used.
#'   * `single_indicator_sensitivity`: each hypothesis's estimate, interval,
#'     and concordance with each composite's reliability shifted by up to .10.
#'   * `converged`, `engine_warnings`, and `decision_log`.
#'
#'   `measurement_attention` in `hypothesis_evidence`, `attention` in the
#'   `summary` and `loadings` tables of `measurement_context`, and `severity`
#'   in `decision_log` are `"info"`, `"review"`, or `"concern"`. See
#'   **Conventions in returned tables** in `?nomologR`.
#'
#'   Other fields record the call, the settings used, and intermediate engine
#'   results. They may change between releases and are not part of the stable
#'   interface (see `?nomologR`).
#'
#' @references
#' Anderson, J. C., & Gerbing, D. W. (1988). Structural equation modeling in
#' practice: A review and recommended two-step approach. *Psychological
#' Bulletin, 103*(3), 411-423. \doi{10.1037/0033-2909.103.3.411}
#'
#' Bollen, K. A. (1989). *Structural equations with latent variables*. Wiley.
#' \doi{10.1002/9781118619179}
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
#' Oberski, D. L., & Satorra, A. (2013). Measurement error models with
#' uncertainty about the error variance. *Structural Equation Modeling, 20*(3),
#' 409-428. \doi{10.1080/10705511.2013.797820}
#'
#' Rosseel, Y., & Loh, W. W. (2024). A structural after measurement approach
#' to structural equation modeling. *Psychological Methods, 29*(3), 561-588.
#' \doi{10.1037/met0000503}
#'
#' Savalei, V. (2019). A comparison of several approaches for controlling
#' measurement error in small samples. *Psychological Methods, 24*(3), 352-370.
#' \doi{10.1037/met0000181}
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
#'
#' # A composite corrected for its unreliability: the Persistence mean, with
#' # omega from its own measurement model.
#' dat <- nomo_demo_network
#' dat$persistence <- rowMeans(dat[c("pe1", "pe2", "pe3", "pe4")])
#' rel <- nomo_reliability(nomo_cfa("P =~ pe1 + pe2 + pe3 + pe4", dat))
#' net_si <- nomo_network(
#'   "Agency =~ ag1 + ag2 + ag3 + ag4",
#'   data = dat,
#'   hypotheses = nomo_hypotheses("Agency -> persistence" = positive()),
#'   single_indicators = list(persistence = nomo_single_indicator(rel))
#' )
#' nomo_table(net_si, "single_indicators")
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
                         single_indicators = NULL,
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
  nomo_defaults_check_safeguards(guidance)

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

  nomo_network_validate_names(hypotheses)
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
  # After the argument checks, so their more specific messages come first.
  nomo_check_model_variables(model, primary_data)
  if (!is.null(validation_data)) {
    nomo_check_model_variables(model, validation_data, "validation_data")
  }

  # lavaan fits a column stored as an ordered factor as categorical whether or
  # not `ordered` names it, so such columns of the fitted model are treated as
  # declared, in either sample (#145).
  ordered_named <- ordered
  ordered_detected <- character()
  for (sample_data in list(primary_data, validation_data)) {
    ordered_detected <- union(
      ordered_detected,
      nomo_ordered_indicators(prepared$full_model, sample_data, ordered_named)$detected
    )
  }

  # That holds for the variables lavaan models, not for an exogenous covariate.
  # lavaan conditions on a covariate: beside ordered outcomes it uses the codes
  # of an ordered factor as numbers, and with continuous outcomes only, its
  # estimation fails (lavaan 0.7.2). Declaring the covariate would record an
  # ordered-categorical model that was not fitted, so it is refused and the
  # researcher chooses its coding.
  covariates <- lavaan::lavNames(
    suppressWarnings(nomo_network_model_table(prepared$full_model, fixed.x = TRUE)),
    "ov.x"
  )
  ordered_covariates <- intersect(ordered_detected, covariates)
  if (length(ordered_covariates)) {
    stop(
      sprintf(
        paste(
          "Exogenous covariate(s) stored as ordered factors: %s. lavaan does not",
          "model an exogenous covariate as ordered-categorical, so these columns",
          "cannot be treated as declared in `ordered`. Supply each as a numeric",
          "column or as dummy-coded columns."
        ),
        paste(ordered_covariates, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  ordered <- c(ordered_named, ordered_detected)
  detected_note <- if (length(ordered_detected)) {
    sprintf(
      " Stored as ordered factors and treated as declared: %s.",
      paste(ordered_detected, collapse = ", ")
    )
  } else {
    ""
  }

  if (length(ordered) && !is.null(estimator) && grepl("^ML", estimator)) {
    stop(
      paste0(
        "ML-family estimators are not supported here with declared ordered ",
        "indicators. Leave `estimator = NULL` for WLSMV or select a ",
        "categorical-data estimator supported by lavaan.",
        detected_note
      ),
      call. = FALSE
    )
  }

  if (length(ordered) && !is.null(missing) &&
      tolower(missing) %in% c("ml", "fiml", "ml.x", "fiml.x")) {
    stop(
      paste0(
        "FIML is not supported by lavaan for declared ordered indicators.",
        detected_note
      ),
      call. = FALSE
    )
  }

  single <- nomo_network_single_spec(
    single_indicators = single_indicators,
    data = primary_data,
    validation_data = validation_data,
    ordered = ordered_named,
    prepared = prepared
  )
  # A composite modeled as a single indicator becomes a factor, which changes
  # the relations lavaan adds for it.
  if (nrow(single)) {
    prepared$changes <- nomo_network_model_changes(
      model, prepared$full_model, hypotheses$hypotheses, single$variable
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

  # One sample fitted with the composites in `spec` as single indicators; with
  # none, the model and data are fitted as given. The samples reported also fit
  # their measurement model alone; the sensitivity refits do not need it.
  fit_sample <- function(spec, sample_data, sample_role, measurement_fit = FALSE) {
    applied <- nomo_network_single_apply(
      prepared$full_model, sample_data, spec, missing, estimator_requested,
      ordered
    )
    list(
      result = nomo_network_fit_once(
        model_fitted = applied$model,
        model_relations = prepared$additions,
        hypotheses = hypotheses,
        data = applied$data,
        ordered = ordered,
        estimator_requested = estimator_requested,
        estimator_source = estimator_source,
        missing = missing,
        std.lv = std.lv,
        control = control,
        guidance = guidance,
        equivalence_alpha = equivalence_alpha,
        sample_role = sample_role,
        measurement_fit = measurement_fit
      ),
      model = applied$model,
      data = applied$data,
      table = applied$table
    )
  }

  primary_sample <- fit_sample(single, primary_data, primary_role, measurement_fit = TRUE)
  primary <- primary_sample$result

  sensitivity <- nomo_network_single_sensitivity(
    spec = single,
    base = primary,
    refit = function(spec) fit_sample(spec, primary_data, primary_role)$result
  )
  primary <- nomo_network_single_reevaluate(
    primary, hypotheses, primary_sample$data, sensitivity$extra, equivalence_alpha
  )

  validation <- NULL
  replication_evidence <- tibble::tibble()

  if (!is.null(validation_data)) {
    validation_sample <- fit_sample(single, validation_data, "validation", measurement_fit = TRUE)
    validation <- validation_sample$result

    # The validation sample's standard errors add the reliabilities'
    # uncertainty too, from refits of its own.
    uncertain <- which(is.finite(single$se))
    if (length(uncertain)) {
      validation_extra <- nomo_network_single_sensitivity(
        spec = single,
        base = validation,
        refit = function(spec) fit_sample(spec, validation_data, "validation")$result,
        shifts = c(-0.05, 0.05),
        perturb = uncertain
      )$extra
      validation <- nomo_network_single_reevaluate(
        validation, hypotheses, validation_sample$data, validation_extra,
        equivalence_alpha
      )
    }

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
    sample_role = primary_role,
    ordered_detected = ordered_detected,
    model_fit = primary$model_fit,
    cases = list(n_used = primary$n_used, data_n = primary$data_n, missing = missing),
    model_changes = prepared$changes
  )

  decision_log <- dplyr::bind_rows(
    decision_log,
    nomo_network_residual_log(primary$hypothesis_evidence),
    nomo_network_endpoint_log(hypotheses, primary$fit),
    nomo_network_metric_log(hypotheses, primary$fit, std.lv),
    nomo_network_single_log(primary_sample$table, sensitivity$table)
  )

  # The validation sample's rows have a stage of their own, so a relation's
  # evidence in the two samples is never read as two entries for one (#145).
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
      sample_role = "validation",
      model_fit = validation$model_fit,
      cases = list(n_used = validation$n_used, data_n = validation$data_n, missing = missing),
      stage = "network_validation"
    )
    decision_log <- dplyr::bind_rows(decision_log, validation_log)
  }

  out <- list(
    call = match.call(),
    model_original = model,
    model_fitted = primary_sample$model,
    model_relations = prepared$additions,
    model_changes = prepared$changes,
    hypotheses = hypotheses,
    sample_role = primary_role,
    split_source = split_source,
    data_n = primary$data_n,
    n_used = primary$n_used,
    validation_n = if (is.null(validation)) NA_integer_ else validation$data_n,
    validation_n_used = if (is.null(validation)) NA_real_ else validation$n_used,
    ordered = ordered,
    ordered_detected = ordered_detected,
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
    single_indicators = primary_sample$table,
    single_indicator_sensitivity = sensitivity$table,
    decision_log = decision_log,
    guidance = guidance
  )

  class(out) <- c("nomo_network", "list")
  out
}



# Residual associations --------------------------------------------------------
#
# `A <-> B` is fitted as the covariance `A ~~ B`. When the fitted model also
# predicts A or B, lavaan estimates that covariance between residuals, so the
# estimate is what remains of the association after the predictors and can
# differ from the overall association in size and in sign. The estimate is
# kept and labeled: its evidence scope is "residual_association", its
# interpretation says so, and the log asks for review.
nomo_network_residual_log <- function(hypothesis_evidence) {
  log <- nomo_log_new()
  # Only a relation that has an estimate is said to be estimated.
  residual <- hypothesis_evidence[
    hypothesis_evidence$evidence_scope == "residual_association" &
      is.finite(hypothesis_evidence$estimate),
    ,
    drop = FALSE
  ]

  for (i in seq_len(nrow(residual))) {
    log <- nomo_log_add(
      log,
      stage = "network",
      object = residual$relation[[i]],
      metric = "residual_association",
      value = residual$estimate[[i]],
      reference = "An association with a predicted endpoint is a residual covariance",
      severity = "review",
      observation = sprintf(
        paste(
          "`%s` is estimated as a residual association, because the fitted",
          "model predicts at least one of its endpoints from other variables,",
          "by a directed path or a factor loading. The estimate is the",
          "association that remains after those predictors, not the overall",
          "association between the two variables."
        ),
        residual$relation[[i]]
      ),
      recommendation = paste(
        "Read the prediction against a residual association, which can differ",
        "from the overall association in size and in sign. If the theory",
        "concerns the overall association, evaluate it in a model that does",
        "not predict these variables."
      )
    )
  }

  log
}


# The metric of an unstandardized latent estimate (#145) ------------------------
#
# A latent variable has no raw units: an unstandardized bound on a relation
# that involves one is judged in the metric its identification sets, which
# `std.lv` chooses. The same bound can be met under one identification and
# missed under the other, so the log names the one used.
nomo_network_metric_log <- function(hypotheses, fit, std.lv) {
  log <- nomo_log_new()
  latent <- tryCatch(
    as.character(lavaan::lavNames(fit, type = "lv")),
    error = function(e) character()
  )
  h <- hypotheses$hypotheses
  rows <- which(h$scale == "unstandardized" & (h$source %in% latent | h$target %in% latent))
  for (i in rows) {
    nodes <- intersect(c(h$source[[i]], h$target[[i]]), latent)
    log <- nomo_log_add(
      log, stage = "network", object = h$relation[[i]],
      metric = "unstandardized_metric",
      reference = sprintf("std.lv = %s", isTRUE(std.lv)),
      severity = "info",
      observation = sprintf(
        "`%s` is judged on the unstandardized scale, whose metric for %s is set by the identification: %s.",
        h$relation[[i]], nomo_present_or(paste0("`", nodes, "`"), "and"),
        if (isTRUE(std.lv)) {
          "with std.lv = TRUE, a factor's variance, or an outcome's residual variance, is 1"
        } else {
          "with std.lv = FALSE, each factor takes the units of its first indicator"
        }
      ),
      recommendation = paste(
        "Report the identification with an unstandardized bound: the same bound",
        "can be met under one identification and missed under the other."
      )
    )
  }
  log
}


# Observed endpoints -----------------------------------------------------------
#
# The network cannot tell from its syntax whether an observed variable is a
# composite of items or a single measured quantity, so it does not guess. It
# classifies each hypothesis by whether its endpoints are latent, and discloses
# what an observed endpoint means: its measurement error enters unmodeled, and
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
    "unmodeled. If any of them is a composite of several items (a sum, mean,",
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
    "the scores as observed. Where they are composites whose reliability is",
    "known, `single_indicators` models each as a single-indicator latent",
    "variable with its error variance fixed at (1 - reliability) x variance",
    "(Bollen, 1989; Savalei, 2019)."
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
