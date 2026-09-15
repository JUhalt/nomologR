# Measurement-model comparison ------------------------------------------------

nomo_compare_labels <- function(models, exprs) {
  labels <- names(models)
  if (is.null(labels)) labels <- rep("", length(models))
  fallback <- vapply(
    exprs,
    function(e) paste(deparse(e, width.cutoff = 60L), collapse = ""),
    character(1)
  )
  labels[!nzchar(labels)] <- fallback[!nzchar(labels)]

  if (anyDuplicated(labels)) {
    stop(
      paste(
        "Model labels must be unique. Name the models, for example",
        "`nomo_compare(full = cfa_full, reduced = cfa_reduced, rationale = ...)`."
      ),
      call. = FALSE
    )
  }

  labels
}


nomo_compare_measure <- function(measures, name) {
  if (!length(measures) || !name %in% names(measures)) return(NA_real_)
  value <- suppressWarnings(as.numeric(measures[[name]])[1L])
  if (length(value) && is.finite(value)) value else NA_real_
}


nomo_compare_observed_names <- function(fit) {
  sort(unique(as.character(tryCatch(
    lavaan::lavNames(fit, type = "ov"),
    error = function(e) character()
  ))))
}


nomo_compare_data <- function(fit) {
  out <- tryCatch(lavaan::lavInspect(fit, "data"), error = function(e) NULL)
  if (is.list(out) && !is.matrix(out)) out <- out[[1L]]
  if (is.null(out)) return(NULL)
  as.data.frame(out)
}


nomo_compare_validate_models <- function(models) {
  if (length(models) < 2L) {
    stop("`nomo_compare()` needs at least two `nomo_cfa` objects.", call. = FALSE)
  }

  not_cfa <- names(models)[!vapply(models, inherits, logical(1), what = "nomo_cfa")]
  if (length(not_cfa)) {
    stop(
      sprintf(
        "Every model must be a `nomo_cfa` object. Not a `nomo_cfa`: %s.",
        paste(not_cfa, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  not_converged <- names(models)[!vapply(models, function(m) isTRUE(m$converged), logical(1))]
  if (length(not_converged)) {
    stop(
      sprintf(
        "Models must converge before they are compared. Not converged: %s.",
        paste(not_converged, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  describe <- function(values) {
    paste(sprintf("%s = %s", names(values), values), collapse = "; ")
  }

  estimators <- vapply(models, function(m) as.character(m$estimator)[1L], character(1))
  if (length(unique(estimators)) != 1L) {
    stop(
      sprintf(
        paste(
          "Models must use the same estimator (%s). Test statistics and fit",
          "indices from different estimators are not comparable; refit the",
          "models with one estimator."
        ),
        describe(estimators)
      ),
      call. = FALSE
    )
  }

  missing_modes <- vapply(models, function(m) as.character(m$missing)[1L], character(1))
  if (length(unique(missing_modes)) != 1L) {
    stop(
      sprintf(
        "Models must use the same missing-data handling (%s). Refit them with one approach.",
        describe(missing_modes)
      ),
      call. = FALSE
    )
  }

  observed <- lapply(models, function(m) nomo_compare_observed_names(m$fit))
  reference_observed <- observed[[1L]]

  cases <- lapply(models, function(m) {
    tryCatch(lavaan::lavInspect(m$fit, "case.idx"), error = function(e) NULL)
  })
  if (any(vapply(cases, is.null, logical(1)))) {
    stop("Could not confirm which cases each model used, so the models cannot be compared.", call. = FALSE)
  }

  data <- lapply(models, function(m) nomo_compare_data(m$fit))
  if (any(vapply(data, is.null, logical(1)))) {
    stop("Could not retrieve the data each model used, so the models cannot be compared.", call. = FALSE)
  }

  for (nm in names(models)[-1L]) {
    if (!identical(as.integer(unlist(cases[[nm]])), as.integer(unlist(cases[[1L]])))) {
      stop(
        sprintf(
          paste(
            "Models `%s` and `%s` were not fitted to the same cases (%s vs %s cases).",
            "Comparisons require the same cases; check subsetting and",
            "missing-data handling."
          ),
          names(models)[[1L]], nm,
          length(unlist(cases[[1L]])), length(unlist(cases[[nm]]))
        ),
        call. = FALSE
      )
    }

    shared <- intersect(reference_observed, observed[[nm]])
    if (!length(shared)) {
      stop(
        sprintf("Models `%s` and `%s` share no observed variables.", names(models)[[1L]], nm),
        call. = FALSE
      )
    }

    a <- as.matrix(data[[1L]][, shared, drop = FALSE])
    b <- as.matrix(data[[nm]][, shared, drop = FALSE])
    if (!isTRUE(all.equal(unname(a), unname(b), check.attributes = FALSE))) {
      stop(
        sprintf(
          paste(
            "Models `%s` and `%s` were not fitted to the same data: values of",
            "shared observed variables differ. Fit both models to the same data."
          ),
          names(models)[[1L]], nm
        ),
        call. = FALSE
      )
    }

    ordered_a <- intersect(shared, models[[1L]]$ordered)
    ordered_b <- intersect(shared, models[[nm]]$ordered)
    if (!setequal(ordered_a, ordered_b)) {
      stop(
        sprintf(
          "Models `%s` and `%s` treat shared indicators differently (ordered vs continuous).",
          names(models)[[1L]], nm
        ),
        call. = FALSE
      )
    }
  }

  list(observed = observed)
}


nomo_compare_fixed_zero_loadings <- function(fit) {
  pt <- tryCatch(as.data.frame(lavaan::parTable(fit)), error = function(e) NULL)
  if (is.null(pt) || !nrow(pt) || !all(c("op", "free", "est") %in% names(pt))) {
    return(tibble::tibble(factor = character(), item = character()))
  }
  rows <- pt$op == "=~" & pt$free == 0L & is.finite(pt$est) & abs(pt$est) < 1e-12
  tibble::tibble(factor = as.character(pt$lhs[rows]), item = as.character(pt$rhs[rows]))
}


nomo_compare_model_row <- function(label, cfa, observed) {
  measures <- cfa$fit_measures_all
  fit_evidence <- cfa$fit_evidence

  pick <- function(metric) {
    value <- fit_evidence$value[fit_evidence$metric == metric]
    if (length(value) && is.finite(value[[1L]])) value[[1L]] else NA_real_
  }
  variant <- function(metric) {
    value <- fit_evidence$variant[fit_evidence$metric == metric]
    if (length(value)) as.character(value[[1L]]) else NA_character_
  }

  # Compute every value before building the tibble: tibble() evaluates columns
  # sequentially, so a new column would otherwise mask a same-named variable.
  values <- list(
    model = label,
    estimator = as.character(cfa$estimator),
    n_used = as.numeric(cfa$n_used),
    observed_variables = length(observed),
    npar = nomo_compare_measure(measures, "npar"),
    df = nomo_compare_measure(measures, "df"),
    chisq = pick("chi_square"),
    cfi = pick("CFI"),
    tli = pick("TLI"),
    rmsea = pick("RMSEA"),
    srmr = pick("SRMR"),
    aic = nomo_compare_measure(measures, "aic"),
    bic = nomo_compare_measure(measures, "bic"),
    fit_index_variant = variant("CFI"),
    fixed_zero_loadings = nrow(nomo_compare_fixed_zero_loadings(cfa$fit))
  )
  tibble::as_tibble(values)
}


nomo_compare_nesting <- function(reference_fit,
                                 other_fit,
                                 same_variables,
                                 df_reference,
                                 df_other,
                                 declared) {
  out <- list(
    nested = FALSE,
    check = "not_applicable",
    relation = "different_variables",
    note = paste(
      "The models contain different observed variables, so they describe",
      "different data. A difference test and information criteria are not",
      "comparable. To test whether an item is needed, keep it in both models",
      "and fix its loading to zero in the reduced model."
    ),
    conflict = FALSE,
    warnings = character()
  )
  if (!same_variables) return(out)

  check <- "not_run"
  check_nested <- NA
  equivalent <- FALSE
  warnings <- character()
  check_error <- ""

  if (!identical(declared, "no")) {
    net <- tryCatch(
      withCallingHandlers(
        semTools::net(reference_fit, other_fit),
        warning = function(w) {
          warnings <<- unique(c(warnings, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) e
    )

    if (inherits(net, "error")) {
      check <- "unavailable"
      check_error <- conditionMessage(net)
    } else {
      test <- methods::slot(net, "test")
      dfs <- as.numeric(methods::slot(net, "df"))
      if (!is.matrix(test) || nrow(test) != 2L || length(dfs) != 2L) {
        check <- "unavailable"
        check_error <- "The nesting check returned an unexpected result."
      } else if (dfs[[1L]] == dfs[[2L]]) {
        check_nested <- test[2L, 1L]
        equivalent <- isTRUE(check_nested)
      } else {
        check_nested <- test[which.max(dfs), which.min(dfs)]
      }

      if (!identical(check, "unavailable")) {
        check <- if (is.na(check_nested)) {
          "unavailable"
        } else if (isTRUE(equivalent)) {
          "equivalent"
        } else if (isTRUE(check_nested)) {
          "nested"
        } else {
          "not_nested"
        }
        if (is.na(check_nested)) {
          check_error <- "A model did not converge when fitted to the other model's implied moments."
        }
      }
    }
  }

  nested <- switch(
    declared,
    no = FALSE,
    yes = if (identical(check, "not_nested")) FALSE else TRUE,
    auto = isTRUE(check_nested)
  )
  conflict <- identical(declared, "yes") && identical(check, "not_nested")

  relation <- if (isTRUE(equivalent)) {
    "equivalent"
  } else if (!nested) {
    "non_nested"
  } else if (df_other > df_reference) {
    "more_constrained"
  } else if (df_other < df_reference) {
    "less_constrained"
  } else {
    "equivalent"
  }

  note <- switch(
    check,
    nested = "The nesting check (Bentler & Satorra, 2010) confirms that the models are nested.",
    equivalent = paste(
      "The models are equivalent: they have the same degrees of freedom and",
      "imply the same covariance structure, so the data cannot distinguish",
      "them. Choose between them on theoretical grounds."
    ),
    not_nested = if (conflict) {
      paste(
        "The models were declared nested, but the nesting check (Bentler &",
        "Satorra, 2010) indicates that they are not. The difference test is not",
        "reported; review the model specifications."
      )
    } else {
      "The nesting check (Bentler & Satorra, 2010) indicates that the models are not nested, so a chi-square difference test does not apply."
    },
    unavailable = if (identical(declared, "yes")) {
      paste0(
        "Nesting could not be checked automatically (", check_error, "). ",
        "The models are treated as nested because the researcher declared them nested."
      )
    } else {
      paste0(
        "Nesting could not be checked automatically (", check_error, "). ",
        "If one model is obtained from the other by fixing or constraining ",
        "parameters, set `nested = \"yes\"` to request the difference test."
      )
    },
    not_run = "The researcher declared the models non-nested; no difference test was requested.",
    ""
  )

  list(
    nested = nested,
    check = check,
    relation = relation,
    note = note,
    conflict = conflict,
    warnings = warnings
  )
}


nomo_compare_lrt_value <- function(row, pattern) {
  nms <- names(row)
  hit <- grep(pattern, nms, ignore.case = TRUE)
  if (!length(hit)) return(NA_real_)
  value <- suppressWarnings(as.numeric(row[[hit[[1L]]]])[1L])
  if (length(value) && is.finite(value)) value else NA_real_
}


nomo_compare_difference_test <- function(reference_fit, other_fit, method, estimator) {
  warnings <- character()
  args <- list(reference_fit, other_fit)
  if (!identical(method, "default")) args$method <- method

  tab <- tryCatch(
    withCallingHandlers(
      do.call(lavaan::lavTestLRT, args),
      warning = function(w) {
        warnings <<- unique(c(warnings, trimws(gsub("[[:space:]]+", " ", conditionMessage(w)))))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )

  empty <- list(
    available = FALSE,
    statistic = NA_real_,
    df = NA_real_,
    p_value = NA_real_,
    method = NA_character_,
    label = NA_character_,
    note = "",
    warnings = warnings
  )

  if (inherits(tab, "error")) {
    empty$note <- paste("lavaan could not compute the difference test:", conditionMessage(tab))
    return(empty)
  }

  heading <- paste(attr(tab, "heading"), collapse = " ")
  method_hit <- regmatches(heading, regexpr('method = "[^"]+"', heading))
  method_used <- if (length(method_hit)) {
    sub('method = "([^"]+)"', "\\1", method_hit)
  } else {
    "standard"
  }
  heading_lines <- trimws(strsplit(heading, "\n", fixed = TRUE)[[1L]])
  heading_lines <- heading_lines[nzchar(heading_lines)]
  label <- if (length(heading_lines)) heading_lines[[1L]] else "Chi-Squared Difference Test"

  tab <- as.data.frame(tab)
  last <- tab[nrow(tab), , drop = FALSE]
  statistic <- nomo_compare_lrt_value(last, "chisq.*diff|diff.*chisq")
  df_diff <- nomo_compare_lrt_value(last, "^df.*diff|diff.*df")
  p_value <- nomo_compare_lrt_value(last, "pr\\(>chisq\\)|p.*value|pvalue")

  available <- is.finite(statistic) && statistic >= 0 &&
    is.finite(df_diff) && df_diff > 0 && is.finite(p_value)

  note <- ""
  if (!available) {
    reason <- if (is.finite(statistic) && statistic < 0) {
      "the scaled difference statistic is negative"
    } else {
      "lavaan did not return a finite difference statistic"
    }
    note <- paste0("The difference test is unavailable: ", reason, ".")
    if (length(warnings)) {
      note <- paste0(note, " lavaan reported: ", paste(warnings, collapse = " | "), ".")
    }
    if (identical(method, "default") && !estimator %in% c("ML", "WLSMV", "DWLS", "WLS", "ULS")) {
      note <- paste(
        note,
        "For robust ML estimators, `method = \"satorra.bentler.2010\"` was",
        "designed to keep the scaling factor positive (Satorra & Bentler, 2010);",
        "if you use it, report the method explicitly."
      )
    }
    statistic <- NA_real_
    p_value <- NA_real_
  }

  list(
    available = available,
    statistic = statistic,
    df = df_diff,
    p_value = p_value,
    method = method_used,
    label = label,
    note = note,
    warnings = warnings
  )
}


nomo_compare_format_p <- function(p) {
  if (!is.finite(p)) return("p unavailable")
  if (p < 0.001) return("p < .001")
  paste0("p = ", sub("^0", "", formatC(p, format = "f", digits = 3)))
}


nomo_compare_interpretation <- function(row) {
  relation_text <- switch(
    row$relation,
    more_constrained = sprintf(
      "`%s` is nested within `%s` and has %s more degree(s) of freedom (additional constraints).",
      row$model, row$reference, format(row$df_difference, trim = TRUE)
    ),
    less_constrained = sprintf(
      "`%s` has %s fewer degree(s) of freedom than `%s` (it estimates additional parameters), and `%s` is nested within it.",
      row$model, format(abs(row$df_difference), trim = TRUE), row$reference, row$reference
    ),
    equivalent = sprintf("`%s` and `%s` are equivalent models.", row$model, row$reference),
    non_nested = sprintf("`%s` and `%s` are not nested.", row$model, row$reference),
    different_variables = sprintf(
      "`%s` and `%s` contain different observed variables.",
      row$model, row$reference
    )
  )

  test_text <- if (isTRUE(row$test_available)) {
    paste0(
      row$test, ": chi-square difference = ",
      formatC(row$chisq_diff, format = "f", digits = 2),
      ", df = ", format(row$df_diff, trim = TRUE), ", ",
      nomo_compare_format_p(row$p_value), ". ",
      "A small p-value indicates that the extra constraints are not fully ",
      "consistent with the data; with large samples, even small ",
      "misspecifications produce small p-values."
    )
  } else if (nzchar(row$test_note)) {
    row$test_note
  } else {
    ""
  }

  fit_text <- if (any(is.finite(c(row$delta_cfi, row$delta_tli, row$delta_rmsea, row$delta_srmr)))) {
    fmt <- function(x) if (is.finite(x)) sprintf("%+.3f", x) else "unavailable"
    sprintf(
      "Change in fit (`%s` minus `%s`): CFI %s, TLI %s, RMSEA %s, SRMR %s.",
      row$model, row$reference, fmt(row$delta_cfi), fmt(row$delta_tli),
      fmt(row$delta_rmsea), fmt(row$delta_srmr)
    )
  } else {
    ""
  }

  ic_text <- if (isTRUE(row$ic_available)) {
    sprintf(
      "AIC %+.1f and BIC %+.1f (`%s` minus `%s`); lower values favor a model for these data, and only differences are interpretable.",
      row$delta_aic, row$delta_bic, row$model, row$reference
    )
  } else if (nzchar(row$ic_note)) {
    row$ic_note
  } else {
    ""
  }

  pieces <- c(relation_text, test_text, fit_text, ic_text)
  paste(
    c(pieces[nzchar(pieces)], "No model is selected automatically; read this evidence with theory and the recorded rationale."),
    collapse = " "
  )
}


nomo_compare_quietly <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- unique(c(warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  list(value = value, warnings = warnings)
}


nomo_compare_evidence <- function(label, model, guidance) {
  zero <- nomo_compare_fixed_zero_loadings(model$fit)
  zero_note <- function(construct) {
    items <- zero$item[zero$factor == construct]
    if (!length(items)) return("")
    paste0(
      "Loading(s) fixed to zero for ", paste(items, collapse = ", "),
      " keep those item(s) in this composite; the coefficient does not describe a shortened scale."
    )
  }

  rows <- list()

  rel <- nomo_compare_quietly(nomo_reliability(model, guidance = guidance))
  if (inherits(rel$value, "error")) {
    rows[[length(rows) + 1L]] <- tibble::tibble(
      model = label, construct = NA_character_, metric = "reliability",
      estimate = NA_real_, note = paste("Unavailable:", conditionMessage(rel$value))
    )
  } else if (nrow(rel$value$evidence)) {
    ev <- rel$value$evidence
    rows[[length(rows) + 1L]] <- tibble::tibble(
      model = label,
      construct = as.character(ev$construct),
      metric = as.character(ev$metric),
      estimate = as.numeric(ev$estimate),
      note = vapply(as.character(ev$construct), zero_note, character(1))
    )
  }

  val <- nomo_compare_quietly(nomo_validity(model, htmt = "htmt2", guidance = guidance))
  if (inherits(val$value, "error")) {
    rows[[length(rows) + 1L]] <- tibble::tibble(
      model = label, construct = NA_character_, metric = "validity",
      estimate = NA_real_, note = paste("Unavailable:", conditionMessage(val$value))
    )
  } else {
    if (nrow(val$value$ave)) {
      ave <- val$value$ave
      rows[[length(rows) + 1L]] <- tibble::tibble(
        model = label,
        construct = as.character(ave$construct),
        metric = "AVE",
        estimate = as.numeric(ave$estimate),
        note = vapply(as.character(ave$construct), zero_note, character(1))
      )
    }
    if (nrow(val$value$htmt2)) {
      h <- val$value$htmt2
      rows[[length(rows) + 1L]] <- tibble::tibble(
        model = label,
        construct = paste(h$construct_1, "vs", h$construct_2),
        metric = "HTMT2",
        estimate = as.numeric(h$estimate),
        note = ""
      )
    }
  }

  if (!length(rows)) {
    return(tibble::tibble(
      model = character(), construct = character(), metric = character(),
      estimate = numeric(), note = character()
    ))
  }
  dplyr::bind_rows(rows)
}


nomo_compare_loadings <- function(models) {
  long <- lapply(names(models), function(nm) {
    sl <- models[[nm]]$standardized_loadings
    if (!nrow(sl)) return(NULL)
    data.frame(
      factor = as.character(sl$factor),
      item = as.character(sl$item),
      model = nm,
      loading = as.numeric(sl$loading),
      stringsAsFactors = FALSE
    )
  })
  long <- do.call(rbind, long)
  if (is.null(long) || !nrow(long)) return(tibble::tibble())

  out <- unique(long[, c("factor", "item")])
  keys <- paste(out$factor, out$item, sep = "\r")
  for (nm in names(models)) {
    part <- long[long$model == nm, , drop = FALSE]
    out[[nm]] <- part$loading[match(keys, paste(part$factor, part$item, sep = "\r"))]
  }
  rownames(out) <- NULL
  tibble::as_tibble(out)
}


nomo_compare_references <- function() {
  tibble::tibble(
    topic = c(
      "nested_difference_tests",
      "nesting_check",
      "change_in_fit",
      "information_criteria",
      "post_hoc_respecification"
    ),
    citation = c(
      "Satorra (2000); Satorra & Bentler (2001, 2010)",
      "Bentler & Satorra (2010)",
      "Cheung & Rensvold (2002); Chen (2007)",
      "Akaike (1974); Schwarz (1978); Raftery (1995); Burnham & Anderson (2004)",
      "MacCallum, Roznowski, & Necowitz (1992)"
    ),
    purpose = c(
      "Use difference tests that match the estimator; robust estimators need scaled differences rather than subtracting robust chi-square values.",
      "Check nesting from the models' implied moments instead of assuming it from syntax.",
      "Report several changes in fit without treating any single cutoff as decisive.",
      "Compare non-nested models of the same data with information criteria, interpreting differences rather than raw values.",
      "Label comparisons prompted by modification indices or residuals as post hoc and confirm them in independent data."
    )
  )
}


#' Compare confirmatory measurement models
#'
#' `nomo_compare()` places two or more fitted [nomo_cfa()] models side by side
#' and reports the evidence that bears on the comparison: a difference test
#' matched to the estimator when the models are nested, changes in global fit
#' indices, information criteria when they are defined, and standardized
#' loadings, reliability, and construct-separation evidence for each model. A
#' researcher rationale is required and recorded. The function never selects a
#' "winning" model.
#'
#' @details
#' **Requirements.** All models must be converged `nomo_cfa` objects fitted with
#' the same estimator and missing-data handling to the same cases and data.
#' Otherwise the comparison is refused with an explanation, because test
#' statistics and fit indices would not be comparable.
#'
#' **Nesting.** With `nested = "auto"`, nesting is checked from the models'
#' implied moments with [semTools::net()] (Bentler & Satorra, 2010). The
#' difference test is reported only for nested models. If the check cannot run
#' (for example for some categorical models), set `nested = "yes"` when one
#' model is obtained from the other by fixing or constraining parameters. A
#' declaration that the check contradicts is recorded as a concern and no test
#' is reported.
#'
#' **Difference tests.** Tests come from [lavaan::lavTestLRT()]: the ordinary
#' chi-square difference test for ML, the scaled difference test for robust ML
#' estimators (Satorra & Bentler, 2001), and the scaled-and-shifted test for
#' categorical estimators such as WLSMV (Satorra, 2000). The method lavaan used
#' is reported. A test that lavaan cannot compute, or a negative scaled
#' statistic, is reported as unavailable with lavaan's message rather than
#' replaced by a different method; `method` can request an alternative
#' explicitly.
#'
#' **Information criteria.** AIC and BIC are reported for likelihood-based
#' estimators when the models contain the same observed variables. They are not
#' defined for WLSMV and are reported as unavailable, not substituted.
#'
#' **Removing an item.** Models with different observed variables describe
#' different data, so neither a difference test nor information criteria
#' applies; only descriptive evidence is shown. To test whether an item is
#' needed, keep it in both models and fix its loading to zero in the reduced
#' model (for example `B =~ b1 + b2 + b3 + b4 + 0*b5`). Reliability for such a
#' model still includes the zero-loading item in the composite, and the
#' evidence table says so.
#'
#' @param ... Two or more `nomo_cfa` objects. Argument names become model
#'   labels, for example `nomo_compare(full = cfa_full, reduced = cfa_reduced,
#'   rationale = "...")`.
#' @param rationale Required character scalar recording why these models are
#'   compared (for example, the theoretical question each model represents).
#' @param origin Whether the comparison was planned before seeing results
#'   (`"a_priori"`) or prompted by results such as modification indices or
#'   residuals (`"post_hoc"`). Post-hoc comparisons are flagged in the decision
#'   log.
#' @param nested `"auto"` (default) checks nesting automatically; `"yes"`
#'   declares the models nested; `"no"` declares them non-nested and skips the
#'   difference test.
#' @param reference The model every other model is compared with, as an index
#'   or label. Defaults to the first model.
#' @param method Difference-test method passed to [lavaan::lavTestLRT()].
#'   `"default"` lets lavaan choose the method appropriate to the estimator.
#' @param evidence Logical. If `TRUE` (default), compute side-by-side
#'   reliability ([nomo_reliability()]) and convergent/discriminant
#'   ([nomo_validity()]) evidence for each model.
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return A `nomo_compare` object containing `$models` (fit and information
#'   criteria for each model), `$comparisons` (nesting, difference test, changes
#'   in fit, and interpretation for each model against the reference),
#'   `$loadings` (standardized loadings side by side), `$evidence`
#'   (reliability, AVE, and HTMT2 by model), the fitted `nomo_cfa` objects in
#'   `$fits`, references, and a decision log.
#'
#' @references
#' Akaike, H. (1974). A new look at the statistical model identification.
#' *IEEE Transactions on Automatic Control, 19*(6), 716-723.
#' \doi{10.1109/TAC.1974.1100705}
#'
#' Bentler, P. M., & Satorra, A. (2010). Testing model nesting and equivalence.
#' *Psychological Methods, 15*(2), 111-123. \doi{10.1037/a0019625}
#'
#' Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference:
#' Understanding AIC and BIC in model selection. *Sociological Methods &
#' Research, 33*(2), 261-304. \doi{10.1177/0049124104268644}
#'
#' Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of
#' measurement invariance. *Structural Equation Modeling, 14*(3), 464-504.
#' \doi{10.1080/10705510701301834}
#'
#' Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit indexes
#' for testing measurement invariance. *Structural Equation Modeling, 9*(2),
#' 233-255. \doi{10.1207/S15328007SEM0902_5}
#'
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
#' modifications in covariance structure analysis: The problem of
#' capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.
#' \doi{10.1037/0033-2909.111.3.490}
#'
#' Raftery, A. E. (1995). Bayesian model selection in social research.
#' *Sociological Methodology, 25*, 111-163. \doi{10.2307/271063}
#'
#' Satorra, A. (2000). Scaled and adjusted restricted tests in multi-sample
#' analysis of moment structures. In *Innovations in multivariate statistical
#' analysis* (pp. 233-247). Springer. \doi{10.1007/978-1-4615-4603-0_17}
#'
#' Satorra, A., & Bentler, P. M. (2001). A scaled difference chi-square test
#' statistic for moment structure analysis. *Psychometrika, 66*(4), 507-514.
#' \doi{10.1007/BF02296192}
#'
#' Satorra, A., & Bentler, P. M. (2010). Ensuring positiveness of the scaled
#' difference chi-square test statistic. *Psychometrika, 75*(2), 243-248.
#' \doi{10.1007/s11336-009-9135-y}
#'
#' Schwarz, G. (1978). Estimating the dimension of a model. *The Annals of
#' Statistics, 6*(2), 461-464. \doi{10.1214/aos/1176344136}
#'
#' @seealso [nomo_cfa()] to fit the models.
#'
#' @examples
#' full <- nomo_cfa(
#'   "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
#'   data = nomo_demo_continuous
#' )
#' # Keep b5 in the data but fix its loading to zero to test whether it is needed
#' no_b5 <- nomo_cfa(
#'   "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + 0*b5",
#'   data = nomo_demo_continuous
#' )
#'
#' cmp <- nomo_compare(
#'   full = full,
#'   no_b5 = no_b5,
#'   rationale = "Evaluate whether the weakly loading item b5 contributes to factor B."
#' )
#' cmp
#' nomo_table(cmp, "comparisons")
#'
#' \donttest{
#' summary(cmp)
#' plot(cmp, type = "loadings")
#' }
#' @export
nomo_compare <- function(...,
                         rationale,
                         origin = c("a_priori", "post_hoc"),
                         nested = c("auto", "yes", "no"),
                         reference = 1L,
                         method = "default",
                         evidence = TRUE,
                         guidance = nomo_defaults()) {
  models <- list(...)
  exprs <- as.list(substitute(list(...)))[-1L]
  labels <- nomo_compare_labels(models, exprs)
  names(models) <- labels

  if (missing(rationale) || !is.character(rationale) || length(rationale) != 1L ||
      is.na(rationale) || !nzchar(trimws(rationale))) {
    stop(
      paste(
        "`rationale` is required: record why these models are compared (for",
        "example, the theoretical question each model represents)."
      ),
      call. = FALSE
    )
  }
  rationale <- trimws(rationale)
  origin <- match.arg(origin)
  nested <- match.arg(nested)

  if (!is.character(method) || length(method) != 1L || is.na(method) || !nzchar(trimws(method))) {
    stop("`method` must be one non-empty character value.", call. = FALSE)
  }
  if (!is.logical(evidence) || length(evidence) != 1L || is.na(evidence)) {
    stop("`evidence` must be TRUE or FALSE.", call. = FALSE)
  }
  nomo_cfa_validate_guidance(guidance)

  context <- nomo_compare_validate_models(models)

  reference_index <- if (is.character(reference) && length(reference) == 1L) {
    match(reference, labels)
  } else if (is.numeric(reference) && length(reference) == 1L &&
             is.finite(reference) && reference == as.integer(reference)) {
    as.integer(reference)
  } else {
    NA_integer_
  }
  if (is.na(reference_index) || reference_index < 1L || reference_index > length(models)) {
    stop(
      sprintf(
        "`reference` must be one model index or label: %s.",
        paste(labels, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  reference_label <- labels[[reference_index]]

  model_rows <- dplyr::bind_rows(lapply(labels, function(nm) {
    nomo_compare_model_row(nm, models[[nm]], context$observed[[nm]])
  }))

  reference_row <- model_rows[model_rows$model == reference_label, , drop = FALSE]
  reference_model <- models[[reference_label]]
  estimator <- as.character(reference_model$estimator)

  comparison_rows <- list()
  engine_warnings <- list()
  conflicts <- character()

  for (nm in setdiff(labels, reference_label)) {
    other_row <- model_rows[model_rows$model == nm, , drop = FALSE]
    same_variables <- identical(context$observed[[nm]], context$observed[[reference_label]])

    nesting <- nomo_compare_nesting(
      reference_fit = reference_model$fit,
      other_fit = models[[nm]]$fit,
      same_variables = same_variables,
      df_reference = reference_row$df,
      df_other = other_row$df,
      declared = nested
    )
    if (isTRUE(nesting$conflict)) conflicts <- c(conflicts, nm)

    test <- list(
      available = FALSE, statistic = NA_real_, df = NA_real_, p_value = NA_real_,
      method = NA_character_, label = NA_character_, note = nesting$note, warnings = character()
    )
    if (isTRUE(nesting$nested) && nesting$relation %in% c("more_constrained", "less_constrained")) {
      test <- nomo_compare_difference_test(reference_model$fit, models[[nm]]$fit, method, estimator)
      if (isTRUE(test$available) && nzchar(nesting$note)) {
        test$note <- nesting$note
      } else if (!isTRUE(test$available) && nzchar(nesting$note)) {
        test$note <- paste(nesting$note, test$note)
      }
    }
    engine_warnings[[nm]] <- unique(c(nesting$warnings, test$warnings))

    ic_available <- same_variables && is.finite(reference_row$aic) && is.finite(other_row$aic)
    ic_note <- if (ic_available) {
      ""
    } else if (!same_variables) {
      "Information criteria are not comparable because the models contain different observed variables."
    } else {
      paste(
        "Information criteria are not defined for this estimator (no likelihood)",
        "and are reported as unavailable rather than substituted."
      )
    }

    # Build from precomputed values: tibble() evaluates columns sequentially,
    # so a column such as `test` would otherwise mask the `test` result list.
    test_available <- isTRUE(test$available)
    row <- tibble::as_tibble(list(
      model = nm,
      reference = reference_label,
      relation = nesting$relation,
      nested_declared = nested,
      nesting_check = nesting$check,
      nested = isTRUE(nesting$nested),
      df_difference = other_row$df - reference_row$df,
      test = if (test_available) test$label else NA_character_,
      method = if (test_available) test$method else NA_character_,
      chisq_diff = test$statistic,
      df_diff = test$df,
      p_value = test$p_value,
      test_available = test_available,
      test_note = test$note,
      delta_cfi = other_row$cfi - reference_row$cfi,
      delta_tli = other_row$tli - reference_row$tli,
      delta_rmsea = other_row$rmsea - reference_row$rmsea,
      delta_srmr = other_row$srmr - reference_row$srmr,
      delta_aic = if (ic_available) other_row$aic - reference_row$aic else NA_real_,
      delta_bic = if (ic_available) other_row$bic - reference_row$bic else NA_real_,
      ic_available = ic_available,
      ic_note = ic_note
    ))
    row$interpretation <- nomo_compare_interpretation(row)
    comparison_rows[[nm]] <- row
  }
  comparisons <- dplyr::bind_rows(comparison_rows)

  evidence_table <- if (evidence) {
    dplyr::bind_rows(lapply(labels, function(nm) {
      nomo_compare_evidence(nm, models[[nm]], guidance)
    }))
  } else {
    tibble::tibble(
      model = character(), construct = character(), metric = character(),
      estimate = numeric(), note = character()
    )
  }

  log <- nomo_log_new()
  log <- nomo_log_add(
    log,
    stage = "compare",
    object = "comparison",
    metric = "researcher_rationale",
    reference = "MacCallum, Roznowski, & Necowitz (1992)",
    severity = "info",
    observation = sprintf(
      "%d models compared with reference `%s` (%s comparison).",
      length(models), reference_label, gsub("_", "-", origin)
    ),
    recommendation = paste(
      "Interpret difference tests, changes in fit, information criteria, and",
      "measurement evidence together with theory."
    ),
    decision = "no model selected automatically",
    rationale = rationale
  )

  if (identical(origin, "post_hoc")) {
    log <- nomo_log_add(
      log,
      stage = "compare",
      object = "comparison",
      metric = "comparison_origin",
      reference = "MacCallum, Roznowski, & Necowitz (1992)",
      severity = "review",
      observation = paste(
        "The comparison was specified after seeing results (for example,",
        "prompted by modification indices or residuals)."
      ),
      recommendation = paste(
        "Report the comparison as post hoc and, where feasible, confirm the",
        "retained model in independent data (for example with `nomo_split()`)."
      ),
      rationale = "Data-driven respecification capitalizes on chance."
    )
  }

  if (nrow(comparisons)) {
    for (i in seq_len(nrow(comparisons))) {
      cmp <- comparisons[i, , drop = FALSE]
      conflict <- cmp$model %in% conflicts
      log <- nomo_log_add(
        log,
        stage = "compare",
        object = cmp$model,
        metric = "model_comparison",
        value = if (isTRUE(cmp$test_available)) cmp$chisq_diff else NA_real_,
        reference = "Satorra & Bentler (2001, 2010); Bentler & Satorra (2010); Cheung & Rensvold (2002); Chen (2007)",
        severity = if (conflict) "concern" else if (isTRUE(cmp$test_available) || cmp$relation %in% c("non_nested", "equivalent")) "info" else "review",
        observation = cmp$interpretation,
        recommendation = if (conflict) {
          "Review the model specifications; the declared nesting is not supported by the nesting check."
        } else {
          "Weigh the statistical evidence with theory and measurement evidence before retaining either model."
        },
        rationale = "Model comparison informs, but does not replace, a researcher decision."
      )
    }
  }

  out <- list(
    call = match.call(),
    rationale = rationale,
    origin = origin,
    nested_requested = nested,
    method_requested = method,
    reference = reference_label,
    estimator = estimator,
    models = model_rows,
    comparisons = comparisons,
    loadings = nomo_compare_loadings(models),
    evidence = evidence_table,
    engine_warnings = engine_warnings,
    fits = models,
    references = nomo_compare_references(),
    decision_log = log,
    guidance = guidance
  )
  class(out) <- c("nomo_compare", "list")
  out
}
