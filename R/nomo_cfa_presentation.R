# Presentation methods for nomo_cfa -----------------------------------------
#
# print() gives the cases, the estimator, convergence, the fit indices, and the
# number of flags; summary() gives the evidence behind them and every flag with
# its explanation. The helpers below are shared with the nomo_compare() and
# nomo_esem() outputs, which report the same indices.


# Abbreviations ------------------------------------------------------------------

# Every abbreviation an output shows is defined once in it (guide point 23).
# Each definition reads after "CFI = " in a note; a key starts it with a
# capital.
nomo_cfa_glossary <- c(
  CFI = "comparative fit index",
  TLI = "Tucker-Lewis index",
  RMSEA = "root mean square error of approximation",
  SRMR = "standardized root mean square residual",
  CI = "confidence interval",
  SE = "standard error",
  df = "degrees of freedom",
  MI = "modification index, the expected drop in the chi-square if the parameter were freed",
  EPC = "expected parameter change if the parameter were freed",
  "Std. EPC" = "the expected parameter change with all variables standardized",
  AIC = "Akaike information criterion",
  BIC = "Bayesian information criterion",
  AVE = "average variance extracted",
  HTMT2 = "heterotrait-monotrait ratio of correlations, geometric-mean version",
  ESEM = "exploratory structural equation modeling",
  CFA = "confirmatory factor analysis",
  ML = "maximum likelihood",
  MLR = "maximum likelihood with robust standard errors and a scaled test statistic",
  MLM = paste("maximum likelihood with robust standard errors and the",
              "Satorra-Bentler scaled test statistic"),
  MLMV = paste("maximum likelihood with robust standard errors and a mean- and",
               "variance-adjusted test statistic"),
  MLMVS = paste("maximum likelihood with robust standard errors and a mean- and",
                "variance-adjusted (Satterthwaite) test statistic"),
  MLF = "maximum likelihood with first-order standard errors",
  WLSMV = paste("diagonally weighted least squares with robust standard errors",
                "and a mean- and variance-adjusted test statistic"),
  WLSM = paste("diagonally weighted least squares with robust standard errors",
               "and a mean-adjusted test statistic"),
  DWLS = "diagonally weighted least squares",
  WLS = "weighted least squares",
  ULS = "unweighted least squares",
  ULSMV = paste("unweighted least squares with robust standard errors and a",
                "mean- and variance-adjusted test statistic"),
  GLS = "generalized least squares"
)


# The definitions of the abbreviations given, in that order, for
# nomo_present_key(). Unknown abbreviations are left out.
nomo_cfa_key_entries <- function(abbr) {
  abbr <- unique(abbr[abbr %in% names(nomo_cfa_glossary)])
  text <- unname(nomo_cfa_glossary[abbr])
  stats::setNames(paste0(toupper(substr(text, 1L, 1L)), substring(text, 2L)), abbr)
}


# The same definitions as one note, after a blank line, for print():
# "CFI = comparative fit index; TLI = Tucker-Lewis index."
nomo_cfa_present_key_note <- function(abbr) {
  abbr <- unique(abbr[abbr %in% names(nomo_cfa_glossary)])
  if (!length(abbr)) return(invisible(NULL))
  cat("\n")
  nomo_present_text(paste(abbr, "=", nomo_cfa_glossary[abbr], collapse = "; "), ".")
}


# The estimator and the engine's estimator, for the key.
nomo_cfa_estimator_abbr <- function(x) {
  unique(stats::na.omit(c(x$estimator, x$estimator_engine)))
}


# Numbers -------------------------------------------------------------------------

# CFI cannot exceed 1 and loses its leading zero; TLI, RMSEA, and SRMR keep it
# (guide point 8).
nomo_cfa_fit_kind <- function(metric) {
  ifelse(metric == "CFI", "fit_bounded", "fit")
}


# Each fit index in its kind, with the decisions that tell a flagged value from
# its reference when `reference` is given (guide point 9).
nomo_cfa_fit_number <- function(metric, value, reference = NULL) {
  vapply(seq_along(value), function(i) {
    nomo_present_stat(value[[i]], nomo_cfa_fit_kind(metric[[i]]),
                      reference = if (is.null(reference)) NULL else reference[[i]])
  }, character(1))
}


# Formats for a fit table in the column names of lavaan::fitMeasures(), as the
# nomo_compare() and nomo_esem() model tables use them.
nomo_cfa_fit_formats <- function() {
  fit <- function(v) nomo_present_stat(v, "fit")
  list(
    npar = function(v) nomo_present_stat(v, "count"),
    chisq = function(v) nomo_present_stat(v, "stat"),
    df = function(v) nomo_present_stat(v, "df"),
    pvalue = nomo_present_p,
    cfi = function(v) nomo_present_stat(v, "fit_bounded"),
    tli = fit, rmsea = fit, srmr = fit,
    aic = function(v) nomo_present_stat(v, "ic"),
    bic = function(v) nomo_present_stat(v, "ic")
  )
}


# Facts ---------------------------------------------------------------------------

# "Cases: 500", or "Cases: 473 of 500 used" when not every row was analyzed
# (guide point 2).
nomo_cfa_cases_fact <- function(n_used, data_n) {
  if (isTRUE(n_used == data_n)) return(sprintf("Cases: %s", nomo_present_stat(data_n, "count")))
  sprintf("Cases: %s of %s used", nomo_present_stat(n_used, "count"),
          nomo_present_stat(data_n, "count"))
}


# Shared by print() and summary(): case use, estimator, and convergence.
nomo_cfa_present_facts <- function(x, detail = FALSE) {
  cases <- nomo_cfa_cases_fact(x$n_used, x$data_n)
  if (isTRUE(detail) && is.finite(x$n_dropped) && x$n_dropped > 0) {
    cases <- sprintf("%s (%d not used, %s)", cases, as.integer(x$n_dropped),
                     nomo_present_percent(x$pct_dropped, base = x$data_n))
  }
  estimator <- paste0(
    "Estimator: ", if (is.na(x$estimator)) nomo_present_missing else x$estimator,
    if (!is.na(x$estimator_engine) && !is.na(x$estimator) &&
        !identical(x$estimator_engine, x$estimator)) {
      sprintf(" (engine: %s)", x$estimator_engine)
    } else {
      ""
    }
  )
  nomo_present_facts(c(
    cases,
    estimator,
    paste0("Converged: ", if (isTRUE(x$converged)) "yes" else "no"),
    if (length(x$ordered)) sprintf("Ordered indicators: %d", length(x$ordered)) else ""
  ))
}


# Global fit indices test a model only when it has positive degrees of freedom.
# A just-identified model (df = 0) reproduces the covariances by construction,
# and a model with negative df is not identified; either way the indices are
# not evidence of fit. NULL when df is positive or unknown.
nomo_cfa_df_problem <- function(fit_evidence) {
  df <- fit_evidence$value[fit_evidence$metric == "df"]
  if (!length(df) || !is.finite(df[[1L]]) || df[[1L]] > 0) return(NULL)
  df <- df[[1L]]
  if (df == 0) {
    list(
      label = "just identified",
      explanation = paste(
        "The model is just identified (df = 0): it reproduces the observed",
        "covariances by construction, so its fit indices cannot test it."
      )
    )
  } else {
    list(
      label = "not identified",
      explanation = sprintf(paste(
        "The model is not identified (df = %s): it has more free parameters",
        "than the observed covariances can determine, so neither its estimates",
        "nor its fit can be interpreted."
      ), nomo_present_df(df))
    )
  }
}


# Versions of the fit statistics ----------------------------------------------------

# The version of a fit index lavaan reported: standard, scaled, or robust.
nomo_cfa_fit_version <- function(variant) {
  ifelse(grepl("robust", variant), "robust",
         ifelse(grepl("scaled", variant), "scaled", ""))
}


# The scaled test lavaan computed, in words: "Yuan-Bentler scaled" for MLR,
# "scaled and shifted" for WLSMV. Empty for the standard test.
nomo_cfa_test_label <- function(fit) {
  test <- tryCatch(as.character(lavaan::lavInspect(fit, "options")$test),
                   error = function(e) character())
  test <- setdiff(test, c("standard", "none", "default"))
  if (!length(test)) return("")
  words <- c(
    satorra.bentler = "Satorra-Bentler scaled",
    yuan.bentler = "Yuan-Bentler scaled",
    yuan.bentler.mplus = "Yuan-Bentler scaled",
    mean.var.adjusted = "mean- and variance-adjusted",
    scaled.shifted = "scaled and shifted",
    mean.adjusted = "mean-adjusted"
  )
  if (test[[1L]] %in% names(words)) words[[test[[1L]]]] else gsub(".", " ", test[[1L]], fixed = TRUE)
}


# One sentence saying which chi-square and which versions of the indices are
# shown (#145): "The chi-square is the Yuan-Bentler scaled test statistic, and
# CFI, TLI, and RMSEA are robust values." Empty when every value is standard.
# `variants` is named by metric (chi_square, CFI, TLI, RMSEA).
nomo_cfa_versions_note <- function(variants, test = "") {
  version <- stats::setNames(nomo_cfa_fit_version(variants), names(variants))
  parts <- character()
  if (nzchar(version[["chi_square"]])) {
    parts <- sprintf("the chi-square is the %s test statistic",
                     if (nzchar(test)) test else "scaled")
  }
  indices <- version[names(version) != "chi_square" & nzchar(version)]
  for (v in unique(indices)) {
    named <- names(indices)[indices == v]
    parts <- c(parts, sprintf("%s %s %s", nomo_present_or(named, "and"),
                              nomo_present_noun(length(named), "is a", "are"),
                              nomo_present_noun(length(named), paste(v, "value"),
                                                paste(v, "values"))))
  }
  if (!length(parts)) return("")
  text <- if (length(parts) == 2L) paste(parts, collapse = ", and ") else nomo_present_or(parts, "and")
  paste0(toupper(substr(text, 1L, 1L)), substring(text, 2L), ".")
}


# The fit indices as facts, "CFI .973", "TLI 0.965", named by index. With
# `tag = TRUE` a robust or scaled value carries its version: "CFI .930 (robust)".
nomo_cfa_fit_parts <- function(fit_evidence, metrics = c("CFI", "TLI", "RMSEA", "SRMR"),
                               tag = FALSE) {
  fit <- fit_evidence[fit_evidence$metric %in% metrics & is.finite(fit_evidence$value), ,
                      drop = FALSE]
  out <- paste(fit$metric, nomo_cfa_fit_number(fit$metric, fit$value))
  if (isTRUE(tag)) {
    version <- nomo_cfa_fit_version(fit$variant)
    out <- ifelse(nzchar(version), paste0(out, " (", version, ")"), out)
  }
  stats::setNames(out, fit$metric)
}


# The variants of the CFA's chi-square and indices, named for the note above.
nomo_cfa_variants <- function(fit_evidence) {
  metrics <- c("chi_square", "CFI", "TLI", "RMSEA")
  stats::setNames(fit_evidence$variant[match(metrics, fit_evidence$metric)], metrics)
}


# Flags ---------------------------------------------------------------------------

# Every flag the analysis raised, as units with a status and an explanation:
# convergence, cases not used, the decision log's own review rows, the degrees
# of freedom, fit indices beyond their references, flagged loadings, improper
# solutions, and lavaan's warnings. print() counts them and summary() lists
# them. A model that did not converge has no estimates to flag.
nomo_cfa_flagged <- function(x) {
  unit <- character()
  status <- character()
  text <- character()
  add <- function(u, s, t) {
    unit <<- c(unit, u)
    status <<- c(status, s)
    text <<- c(text, t)
  }
  converged <- isTRUE(x$converged)
  if (!converged) {
    add("Convergence", "concern", paste(
      "lavaan did not report convergence, so the fit and parameter values are",
      "not estimates. Inspect model identification, starting values, data",
      "quality, and the underlying lavaan fit before interpreting results."
    ))
  }
  if (is.finite(x$n_dropped) && x$n_dropped > 0) {
    add("Cases", "review", sprintf(
      paste("%s of %s input cases were used. Confirm that the loss follows the",
            "intended missing-data strategy."),
      nomo_present_stat(x$n_used, "count"), nomo_present_stat(x$data_n, "count")
    ))
  }
  log <- x$decision_log
  own <- log[log$metric %in% c("optimizer_control", "scaled_test_unavailable", "ordered_detected"), ,
             drop = FALSE]
  units <- c(optimizer_control = "Optimizer", scaled_test_unavailable = "Scaled test")
  for (i in seq_len(nrow(own))) {
    add(if (own$metric[[i]] %in% names(units)) units[[own$metric[[i]]]] else own$object[[i]],
        own$severity[[i]], own$observation[[i]])
  }

  if (converged) {
    problem <- nomo_cfa_df_problem(x$fit_evidence)
    if (!is.null(problem)) {
      df <- x$fit_evidence$value[x$fit_evidence$metric == "df"][[1L]]
      add("Degrees of freedom", if (df < 0) "concern" else "review", problem$explanation)
    }
    fit <- x$fit_evidence[x$fit_evidence$attention == "review", , drop = FALSE]
    for (i in seq_len(nrow(fit))) {
      add(fit$metric[[i]], "review", sprintf(
        "The value, %s, is %s the teaching reference of %s; inspect global and localized model strain.",
        nomo_cfa_fit_number(fit$metric[[i]], fit$value[[i]], fit$reference[[i]]),
        if (identical(fit$direction[[i]], "higher")) "below" else "above",
        nomo_cfa_fit_number(fit$metric[[i]], fit$reference[[i]])
      ))
    }
    loadings <- x$standardized_loadings
    loadings <- loadings[nzchar(nomo_present_flag(loadings$attention)), , drop = FALSE]
    for (i in seq_len(nrow(loadings))) {
      add(paste(loadings$item[[i]], "on", loadings$factor[[i]]), loadings$attention[[i]],
          loadings$explanation[[i]])
    }
    # A loading above 1 is already flagged with the loadings.
    heywood <- x$heywood[x$heywood$issue != "standardized_loading_beyond_one", , drop = FALSE]
    for (i in seq_len(nrow(heywood))) {
      add(heywood$object[[i]], heywood$severity[[i]], heywood$explanation[[i]])
    }
  }
  for (warning_text in x$engine_warnings) {
    add("lavaan", "review", trimws(gsub("[[:space:]]+", " ", warning_text)))
  }
  data.frame(unit = unit, status = status, text = text, stringsAsFactors = FALSE)
}


# Print ---------------------------------------------------------------------------

#' @export
print.nomo_cfa <- function(x, ...) {
  nomo_present_header("nomo_cfa", "Confirmatory factor analysis")
  nomo_cfa_present_facts(x)
  shown <- nomo_cfa_estimator_abbr(x)

  if (!isTRUE(x$converged)) {
    nomo_present_text(
      "The model did not converge, so its loadings and fit are not estimates ",
      "and are not flagged."
    )
  } else {
    problem <- nomo_cfa_df_problem(x$fit_evidence)
    parts <- nomo_cfa_fit_parts(x$fit_evidence)
    if (!is.null(problem)) {
      nomo_present_facts(sprintf(
        "Fit: not testable (df = %s, %s)",
        nomo_present_df(x$fit_evidence$value[x$fit_evidence$metric == "df"][[1L]]),
        problem$label
      ))
      shown <- c(shown, "df")
    } else if (length(parts)) {
      nomo_present_facts(c(paste0("Fit: ", parts[[1L]]), parts[-1L]))
      variants <- nomo_cfa_variants(x$fit_evidence)
      variants[["chi_square"]] <- NA_character_
      note <- nomo_cfa_versions_note(variants)
      if (nzchar(note)) nomo_present_text(note)
      shown <- c(shown, names(parts))
    }
  }

  flagged <- nomo_cfa_flagged(x)
  nomo_present_facts(c(
    sprintf("Loadings: %d", nrow(x$standardized_loadings)),
    paste0("Flags: ", nomo_present_flag_counts(flagged$status)),
    if (isTRUE(x$converged) && nrow(x$heywood)) {
      sprintf("Improper-solution signals: %d", nrow(x$heywood))
    } else {
      ""
    },
    if (length(x$engine_warnings)) sprintf("Engine warnings: %d", length(x$engine_warnings)) else ""
  ))
  nomo_present_text("No parameter was freed and no model was refit automatically.")
  nomo_cfa_present_key_note(shown)
  nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"fit\")"),
    c("the evidence and each flag", "every fit index")
  )
  invisible(x)
}


#' Summarize a guided confirmatory factor analysis
#'
#' @param object A `nomo_cfa` object.
#' @param ... Unused.
#'
#' @return An object of class `summary_nomo_cfa`. Printing it shows the
#'   chi-square test and fit indices with their references, the standardized
#'   loadings, the factor correlations, any improper solution, the largest
#'   residual correlations and modification indices, and each flag with its
#'   explanation.
#' @export
summary.nomo_cfa <- function(object, ...) {
  correlations <- object$factor_correlations
  # A correlation the model fixes, such as the zeros of a bifactor model, is
  # shown as fixed, not as an estimate with an interval (#145).
  correlations$fixed <- nomo_cfa_fixed_correlations(correlations, object$fit)
  out <- list(
    data_n = object$data_n,
    n_used = object$n_used,
    n_dropped = object$n_dropped,
    pct_dropped = object$pct_dropped,
    estimator = object$estimator,
    estimator_engine = object$estimator_engine,
    test = nomo_cfa_test_label(object$fit),
    ordered = object$ordered,
    missing = object$missing,
    converged = object$converged,
    engine_warnings = object$engine_warnings,
    fit_evidence = object$fit_evidence,
    standardized_loadings = object$standardized_loadings,
    factor_correlations = correlations,
    heywood = object$heywood,
    largest_residuals = utils::head(object$residual_pairs, 5L),
    top_modification_indices = utils::head(object$top_modification_indices, 5L),
    decision_log = object$decision_log
  )
  class(out) <- c("summary_nomo_cfa", "list")
  out
}


# Which factor correlations the model fixes rather than estimates, from
# lavaan's parameter table.
nomo_cfa_fixed_correlations <- function(correlations, fit) {
  pt <- tryCatch(as.data.frame(lavaan::parTable(fit)), error = function(e) NULL)
  if (is.null(pt) || !nrow(correlations)) return(rep(FALSE, nrow(correlations)))
  fixed <- pt[pt$op == "~~" & pt$lhs != pt$rhs & pt$free == 0L, , drop = FALSE]
  pairs <- c(paste(fixed$lhs, fixed$rhs, sep = "\r"), paste(fixed$rhs, fixed$lhs, sep = "\r"))
  paste(correlations$factor1, correlations$factor2, sep = "\r") %in% pairs
}


#' @export
print.summary_nomo_cfa <- function(x, ...) {
  nomo_present_header("nomo_cfa", "Confirmatory factor analysis", summary = TRUE)
  nomo_cfa_present_facts(x, detail = TRUE)
  shown <- nomo_cfa_estimator_abbr(x)

  if (!isTRUE(x$converged)) {
    nomo_present_section("Not converged")
    nomo_present_text(
      "lavaan did not report convergence, so the loadings, correlations, ",
      "residuals, and fit are the optimizer's last values, not estimates. They ",
      "are not shown or flagged; x$fit holds lavaan's output.",
      indent = 2L
    )
  } else {
    shown <- c(shown, nomo_cfa_present_fit(x))
    shown <- c(shown, nomo_cfa_present_loadings(x))
    shown <- c(shown, nomo_cfa_present_correlations(x$factor_correlations))
    nomo_cfa_present_improper(x$heywood)
    nomo_cfa_present_residuals(x$largest_residuals)
    shown <- c(shown, nomo_cfa_present_mi(x$top_modification_indices))
  }

  flagged <- nomo_cfa_flagged(x)
  nomo_present_flagged(unit = flagged$unit, status = flagged$status, text = flagged$text)
  nomo_present_key(nomo_cfa_key_entries(shown))
  cat("\n")
  nomo_present_text(
    "Global fit, local strain, and parameter estimates are evidence to ",
    "interpret together; no single cutoff establishes model validity."
  )
  nomo_present_pointer(
    c("nomo_table(x, \"decision_log\")", "x$fit"),
    c("the decision log", "lavaan's full output")
  )
  invisible(x)
}


# The chi-square test, the versions shown, and the fit indices beside their
# references. Returns the abbreviations it showed.
nomo_cfa_present_fit <- function(x) {
  nomo_present_section("Global fit")
  fe <- x$fit_evidence
  shown <- character()
  problem <- nomo_cfa_df_problem(fe)
  if (!is.null(problem)) {
    nomo_present_text(problem$explanation, indent = 2L)
    shown <- "df"
  }
  value <- function(m) {
    v <- fe$value[fe$metric == m]
    if (length(v)) v[[1L]] else NA_real_
  }
  chisq <- nomo_present_chisq(value("chi_square"), value("df"), value("p_value"))
  if (nzchar(chisq)) nomo_present_text(chisq, indent = 2L)

  indices <- fe[fe$metric %in% c("CFI", "TLI", "RMSEA", "SRMR"), , drop = FALSE]
  if (!any(is.finite(indices$value))) {
    # Without values the table would list only the references.
    nomo_present_text("No fit index is available.", indent = 2L)
    return(shown)
  }
  note <- nomo_cfa_versions_note(nomo_cfa_variants(fe), x$test)
  if (nzchar(note)) nomo_present_text(note, indent = 2L)
  indices$interval <- ifelse(
    indices$metric == "RMSEA",
    nomo_present_ci(value("RMSEA_CI_lower"), value("RMSEA_CI_upper"), kind = "fit"),
    ""
  )
  indices$flag <- nomo_present_status(indices$attention)
  # Each row in its index's kind, and a flagged value with the decimals that
  # tell it from its reference. The reference sits beside the value, so a
  # narrow console drops the interval first.
  nomo_present_table(
    indices,
    c("Index" = "metric", "Value" = "value", "Reference" = "reference",
      "90% CI" = "interval", "Flag" = "flag"),
    formats = list(
      value = function(v) nomo_cfa_fit_number(indices$metric, v, indices$reference),
      reference = function(v) nomo_cfa_fit_number(indices$metric, v)
    ),
    more = "nomo_table(x, \"fit\")"
  )
  nomo_present_text(
    "References are teaching values for review, not cutoffs.", indent = 2L
  )
  c(shown, indices$metric[is.finite(indices$value)], "CI")
}


nomo_cfa_present_loadings <- function(x) {
  loadings <- x$standardized_loadings
  nomo_present_section("Standardized loadings")
  if (!nrow(loadings)) {
    nomo_present_text("No standardized loadings are available.", indent = 2L)
    return(character())
  }
  loadings$interval <- nomo_present_ci(loadings$ci_lower, loadings$ci_upper, kind = "loading")
  loadings$flag <- nomo_present_status(loadings$attention)
  # In a higher-order model a first-order factor is the indicator of a
  # second-order loading, so the column is "Indicator" rather than "Item".
  nomo_present_table(
    loadings,
    c("Factor" = "factor", "Indicator" = "item", "Loading" = "loading",
      "SE" = "se", "95% CI" = "interval", "Flag" = "flag"),
    formats = list(loading = function(v) nomo_present_stat(v, "loading"),
                   se = function(v) nomo_present_stat(v, "estimate")),
    more = "nomo_table(x, \"loadings\")"
  )
  if (!any(nzchar(loadings$flag))) {
    nomo_present_text("No loading was flagged for review.", indent = 2L)
  }
  c("SE", "CI")
}


nomo_cfa_present_correlations <- function(correlations) {
  if (!nrow(correlations)) return(character())
  nomo_present_section("Factor correlations")
  estimated <- correlations[!correlations$fixed, , drop = FALSE]
  fixed <- correlations[correlations$fixed, , drop = FALSE]
  if (nrow(estimated)) {
    estimated$interval <- nomo_present_ci(estimated$ci_lower, estimated$ci_upper, kind = "r")
    nomo_present_table(
      estimated,
      c("Factor 1" = "factor1", "Factor 2" = "factor2", "r" = "correlation",
        "95% CI" = "interval"),
      formats = list(correlation = function(v) nomo_present_stat(v, "r")),
      more = "nomo_table(x, \"factor_correlations\")"
    )
  }
  if (nrow(fixed)) {
    nomo_present_text(
      if (nrow(estimated)) "Fixed" else "Every factor correlation is fixed",
      if (isTRUE(all(fixed$correlation == 0))) " at 0" else "",
      " by the model, so not estimated: ",
      nomo_present_or(paste(fixed$factor1, "with", fixed$factor2), "and"), ".",
      indent = 2L
    )
  }
  if (nrow(estimated)) "CI" else character()
}


# Short enough that a 40-column console keeps the signal beside its parameter.
nomo_cfa_issue_label <- function(issue) {
  labels <- c(
    negative_observed_residual_variance = "Negative residual variance",
    negative_latent_variance = "Negative latent variance",
    standardized_loading_beyond_one = "Loading above 1",
    latent_correlation_beyond_one = "Correlation above 1"
  )
  out <- unname(labels[issue])
  plain <- gsub("_", " ", issue)
  ifelse(is.na(out), paste0(toupper(substr(plain, 1L, 1L)), substring(plain, 2L)), out)
}


nomo_cfa_present_improper <- function(heywood) {
  nomo_present_section("Improper solutions")
  if (!nrow(heywood)) {
    nomo_present_text(
      "No improper-solution signal, such as a negative residual variance, ",
      "was detected.", indent = 2L
    )
    return(invisible(NULL))
  }
  heywood$signal <- nomo_cfa_issue_label(heywood$issue)
  kind <- ifelse(heywood$issue == "standardized_loading_beyond_one", "loading",
                 ifelse(heywood$issue == "latent_correlation_beyond_one", "r", "estimate"))
  shown <- vapply(seq_len(nrow(heywood)), function(i) {
    nomo_present_stat(heywood$value[[i]], kind[[i]])
  }, character(1))
  nomo_present_table(
    heywood,
    c("Parameter" = "object", "Signal" = "signal", "Value" = "value"),
    formats = list(value = function(v) shown),
    more = "nomo_table(x, \"heywood\")"
  )
}


nomo_cfa_present_residuals <- function(residuals) {
  if (!nrow(residuals)) return(invisible(NULL))
  nomo_present_section("Largest residual correlations")
  nomo_present_table(
    residuals,
    c("Item 1" = "item1", "Item 2" = "item2", "Residual" = "residual"),
    formats = list(residual = function(v) nomo_present_stat(v, "estimate")),
    more = "nomo_table(x, \"residuals\")"
  )
}


nomo_cfa_present_mi <- function(mi) {
  if (!nrow(mi)) return(character())
  nomo_present_section("Modification indices (diagnostic only)")
  mi$parameter <- paste(mi$lhs, mi$op, mi$rhs)
  estimate <- function(v) nomo_present_stat(v, "estimate")
  nomo_present_table(
    mi,
    c("Parameter" = "parameter", "MI" = "mi", "EPC" = "epc", "Std. EPC" = "sepc.all"),
    formats = list(mi = function(v) nomo_present_stat(v, "stat"), epc = estimate,
                   sepc.all = estimate),
    more = "nomo_table(x, \"modification_indices\")"
  )
  nomo_present_text(
    "Modification indices locate strain. They do not authorize freeing a ",
    "parameter, and nomologR never does so automatically.", indent = 2L
  )
  c("MI", "EPC", "Std. EPC")
}


# Plots ---------------------------------------------------------------------------

#' Plot confirmatory factor-analysis evidence
#'
#' @param x A `nomo_cfa` object.
#' @param type Plot type: `"loadings"`, `"fit"`, `"residuals"`, or
#'   `"modification_indices"`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object. Flags are drawn with the package's status
#'   shapes and colors (filled circle for no flag, open circle for review,
#'   filled square for concern), with a legend whenever a flag is drawn.
#' @export
plot.nomo_cfa <- function(x,
                          type = c("loadings", "fit", "residuals", "modification_indices"),
                          ...) {
  type <- nomo_match_arg(type)

  if (type == "loadings") {
    dat <- x$standardized_loadings
    if (!nrow(dat)) stop("No standardized loadings are available to plot.", call. = FALSE)
    dat$item <- factor(dat$item, levels = rev(unique(dat$item)))
    dat$status <- nomo_plot_status(dat$attention)
    ref <- x$guidance$cfa_loading_reference
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = loading, y = item, shape = status, colour = status)) +
        ggplot2::geom_vline(xintercept = c(-ref, ref), linetype = 2) +
        ggplot2::geom_point(size = 2.8, na.rm = TRUE) +
        nomo_plot_status_scales(dat$status, name = "Flag") +
        ggplot2::facet_wrap(stats::as.formula("~ factor"), scales = "free_y") +
        nomo_plot_labs(
          title = "CFA standardized loadings",
          subtitle = paste0(
            "Dashed lines mark the teaching reference for an absolute loading, ",
            nomo_present_stat(ref, "loading"), ". Factor direction depends on the model."
          ),
          x = "Standardized loading", y = NULL, shape = "Flag", colour = "Flag",
          caption = paste(
            "A reference prompts inspection; it does not delete indicators or",
            "validate the model."
          )
        ) + ggplot2::theme_minimal()
    )
  }

  if (type == "fit") {
    dat <- x$fit_evidence[
      x$fit_evidence$metric %in% c("CFI", "TLI", "RMSEA", "SRMR"), , drop = FALSE
    ]
    dat <- dat[is.finite(dat$value), , drop = FALSE]
    if (!nrow(dat)) stop("No global fit evidence is available to plot.", call. = FALSE)
    references <- paste(dat$metric, nomo_cfa_fit_number(dat$metric, dat$reference),
                        collapse = ", ")
    dat$metric <- factor(dat$metric, levels = c("CFI", "TLI", "RMSEA", "SRMR"))
    # CFI and TLI sit near 1 and RMSEA and SRMR near 0, so one axis squashed
    # the second pair against zero; each pair gets its own panel and scale (#89).
    dat$panel <- factor(
      ifelse(dat$metric %in% c("CFI", "TLI"), "Higher values favor fit",
             "Lower values favor fit"),
      levels = c("Higher values favor fit", "Lower values favor fit")
    )
    dat$status <- nomo_plot_status(dat$attention)
    # The reference is a dashed mark, so the cross stays the "not computed"
    # status it is in every plot (#144).
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = metric, y = value)) +
        ggplot2::geom_errorbar(
          ggplot2::aes(ymin = reference, ymax = reference),
          width = 0.5, linetype = 2, na.rm = TRUE
        ) +
        ggplot2::geom_segment(
          ggplot2::aes(x = metric, xend = metric, y = reference, yend = value),
          colour = "grey60", na.rm = TRUE
        ) +
        ggplot2::geom_point(ggplot2::aes(shape = status, colour = status), size = 3,
                            na.rm = TRUE) +
        nomo_plot_status_scales(dat$status, name = "Flag") +
        ggplot2::facet_wrap(stats::as.formula("~ panel"), scales = "free") +
        nomo_plot_labs(
          title = "CFA global fit evidence",
          subtitle = "Points are observed values; dashed marks are teaching references.",
          x = NULL, y = "Fit index", shape = "Flag", colour = "Flag",
          caption = paste0(
            "References: ", references, ". They are review prompts, not cutoffs; ",
            "interpret global fit with local strain, estimator, sample, and theory."
          )
        ) + ggplot2::theme_minimal()
    )
  }

  if (type == "residuals") {
    mat <- x$residual_matrix
    if (!is.matrix(mat) || nrow(mat) < 2L) {
      stop("No residual-correlation matrix is available to plot.", call. = FALSE)
    }
    idx <- which(lower.tri(mat), arr.ind = TRUE)
    dat <- data.frame(
      item1 = colnames(mat)[idx[, 2L]],
      item2 = rownames(mat)[idx[, 1L]],
      residual = as.numeric(mat[idx]),
      stringsAsFactors = FALSE
    )
    dat$item1 <- factor(dat$item1, levels = colnames(mat))
    dat$item2 <- factor(dat$item2, levels = rev(rownames(mat)))
    max_abs <- max(abs(dat$residual), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = item1, y = item2, fill = residual)) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(midpoint = 0, limits = c(-max_abs, max_abs)) +
        nomo_plot_labs(
          title = "CFA localized residual correlations",
          subtitle = "Unique off-diagonal residual pairs",
          x = NULL, y = NULL, fill = "Residual",
          caption = paste(
            "Localized residuals identify strain; they do not automatically",
            "authorize correlated errors, cross-loadings, or other model changes."
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    )
  }

  dat <- x$top_modification_indices
  if (!nrow(dat) || !"mi" %in% names(dat)) {
    stop("No modification indices are available to plot.", call. = FALSE)
  }
  dat$label <- paste(dat$lhs, dat$op, dat$rhs)
  dat <- dat[order(dat$mi, decreasing = FALSE), , drop = FALSE]
  dat$label <- factor(dat$label, levels = dat$label)

  ggplot2::ggplot(dat, ggplot2::aes(x = mi, y = label)) +
    ggplot2::geom_col() +
    nomo_plot_labs(
      title = "Largest CFA modification indices",
      subtitle = "Post hoc diagnostic evidence only",
      x = "Modification index", y = NULL,
      caption = paste(
        "A large modification index proposes a parameter worth investigating.",
        "It is not permission to respecify the model automatically."
      )
    ) + ggplot2::theme_minimal()
}


utils::globalVariables(c(
  "loading", "item", "factor", "status", "metric", "value", "reference",
  "item1", "item2", "residual", "label", "mi", "panel"
))
