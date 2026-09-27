# Presentation methods for nomo_cfa -----------------------------------------

# Shared by print() and summary(): case use, estimator, and convergence.
nomo_cfa_present_facts <- function(x, detail = FALSE) {
  cases <- sprintf(
    "Cases: %s of %d used",
    if (is.finite(x$n_used)) format(x$n_used, trim = TRUE) else "unknown",
    x$data_n
  )
  if (isTRUE(detail) && is.finite(x$n_dropped) && x$n_dropped > 0) {
    cases <- sprintf("%s (%d not used, %.1f%%)", cases, as.integer(x$n_dropped),
                     100 * x$pct_dropped)
  }
  estimator <- paste0(
    "Estimator: ", if (is.na(x$estimator)) "unknown" else x$estimator,
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
    paste0("Converged: ", if (isTRUE(x$converged)) "yes" else "NO"),
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
      ), format(df, trim = TRUE))
    )
  }
}


# The version of a fit index lavaan reported: standard, scaled, or robust.
nomo_cfa_fit_version <- function(variant) {
  ifelse(grepl("robust", variant), "robust",
         ifelse(grepl("scaled", variant), "scaled", ""))
}


#' @export
print.nomo_cfa <- function(x, ...) {
  nomo_present_header("nomo_cfa", "Confirmatory factor analysis")
  nomo_cfa_present_facts(x)

  fit <- x$fit_evidence[
    x$fit_evidence$metric %in% c("CFI", "TLI", "RMSEA", "SRMR") &
      is.finite(x$fit_evidence$value), , drop = FALSE
  ]
  problem <- nomo_cfa_df_problem(x$fit_evidence)
  if (!is.null(problem)) {
    nomo_present_facts(sprintf(
      "Fit: not testable (df = %s, %s)",
      format(x$fit_evidence$value[x$fit_evidence$metric == "df"][[1L]], trim = TRUE),
      problem$label
    ))
  } else if (nrow(fit)) {
    version <- nomo_cfa_fit_version(fit$variant)
    nomo_present_facts(c(
      paste0("Fit: ", fit$metric[[1L]], " ", nomo_present_number(fit$value[[1L]]),
             ifelse(nzchar(version[[1L]]), paste0(" (", version[[1L]], ")"), "")),
      paste0(fit$metric[-1L], " ", nomo_present_number(fit$value[-1L]),
             ifelse(nzchar(version[-1L]), paste0(" (", version[-1L], ")"), ""))
    ))
  }

  nomo_present_facts(c(
    sprintf("Loadings: %d", nrow(x$standardized_loadings)),
    paste0("Flags: ", nomo_present_flag_counts(x$standardized_loadings$attention)),
    if (nrow(x$heywood)) sprintf("Improper-solution signals: %d", nrow(x$heywood)) else "",
    if (length(x$engine_warnings)) {
      sprintf("Engine warnings: %d (see the decision log)", length(x$engine_warnings))
    } else {
      ""
    }
  ))
  nomo_present_text(
    "No parameter was freed and no model was refit automatically. ",
    "summary() shows the evidence."
  )
  invisible(x)
}


#' Summarize a guided confirmatory factor analysis
#'
#' @param object A `nomo_cfa` object.
#' @param ... Unused.
#'
#' @return An object of class `summary_nomo_cfa`.
#' @export
summary.nomo_cfa <- function(object, ...) {
  out <- list(
    data_n = object$data_n,
    n_used = object$n_used,
    n_dropped = object$n_dropped,
    pct_dropped = object$pct_dropped,
    estimator = object$estimator,
    estimator_engine = object$estimator_engine,
    ordered = object$ordered,
    missing = object$missing,
    converged = object$converged,
    engine_warnings = object$engine_warnings,
    fit_evidence = object$fit_evidence,
    standardized_loadings = object$standardized_loadings,
    factor_correlations = object$factor_correlations,
    heywood = object$heywood,
    largest_residuals = utils::head(object$residual_pairs, 5L),
    top_modification_indices = utils::head(object$top_modification_indices, 5L),
    decision_log = object$decision_log
  )
  class(out) <- c("summary_nomo_cfa", "list")
  out
}


#' @export
print.summary_nomo_cfa <- function(x, ...) {
  nomo_present_header("nomo_cfa", "Confirmatory factor analysis", summary = TRUE)
  nomo_cfa_present_facts(x, detail = TRUE)

  nomo_present_section("Global fit")
  fe <- x$fit_evidence
  problem <- nomo_cfa_df_problem(fe)
  if (!is.null(problem)) nomo_present_text(problem$explanation, indent = 2L)
  value <- function(m) {
    v <- fe$value[fe$metric == m]
    if (length(v)) v[[1L]] else NA_real_
  }
  if (is.finite(value("chi_square"))) {
    p <- nomo_present_p_clause(value("p_value"))
    nomo_present_text(
      sprintf("chi-square(%s) = %s%s", format(value("df"), trim = TRUE),
              nomo_present_number(value("chi_square"), 2L),
              if (nzchar(p)) paste0(", ", p) else ""),
      indent = 2L
    )
  }
  indices <- fe[fe$metric %in% c("CFI", "TLI", "RMSEA", "SRMR"), , drop = FALSE]
  if (!any(is.finite(indices$value))) {
    # Without values the table would list only the references.
    nomo_present_text(
      "No fit index is available",
      if (!isTRUE(x$converged)) ": the model did not converge." else ".",
      indent = 2L
    )
  } else {
    indices$interval <- ifelse(
      indices$metric == "RMSEA",
      nomo_present_ci(value("RMSEA_CI_lower"), value("RMSEA_CI_upper")),
      ""
    )
    indices$version <- nomo_cfa_fit_version(indices$variant)
    nomo_present_table(
      indices,
      c("Index" = "metric", "Value" = "value", "90% CI" = "interval",
        "Reference" = "reference", "Version" = "version"),
      more = "nomo_table(x, \"fit\")"
    )
    nomo_present_text(
      "References are teaching values for review, not cutoffs.", indent = 2L
    )
  }

  loadings <- x$standardized_loadings
  nomo_present_section("Standardized loadings")
  if (nrow(loadings)) {
    loadings$interval <- nomo_present_ci(loadings$ci_lower, loadings$ci_upper)
    loadings$flag <- nomo_present_flag(loadings$attention)
    nomo_present_table(
      loadings,
      c("Factor" = "factor", "Item" = "item", "Loading" = "loading",
        "SE" = "se", "95% CI" = "interval", "Flag" = "flag"),
      more = "nomo_table(x, \"loadings\")"
    )
    flagged <- loadings[nzchar(loadings$flag), , drop = FALSE]
    if (nrow(flagged)) {
      nomo_present_section("Flagged loadings")
      nomo_present_bullets(sprintf(
        "%s on %s (%s): %s", flagged$item, flagged$factor, flagged$flag,
        flagged$explanation
      ))
    } else {
      nomo_present_text("No loading was flagged for review.", indent = 2L)
    }
  } else {
    nomo_present_text("No standardized loadings are available.", indent = 2L)
  }

  if (nrow(x$factor_correlations)) {
    nomo_present_section("Factor correlations")
    fc <- x$factor_correlations
    fc$interval <- nomo_present_ci(fc$ci_lower, fc$ci_upper)
    nomo_present_table(
      fc,
      c("Factor 1" = "factor1", "Factor 2" = "factor2", "r" = "correlation",
        "95% CI" = "interval")
    )
  }

  nomo_present_section("Improper solutions")
  if (nrow(x$heywood)) {
    nomo_present_bullets(sprintf(
      "%s: %s (%s). %s", x$heywood$object, x$heywood$issue,
      nomo_present_number(x$heywood$value), x$heywood$explanation
    ))
  } else {
    nomo_present_text(
      "No improper-solution signal, such as a negative residual variance, ",
      "was detected.", indent = 2L
    )
  }

  if (nrow(x$largest_residuals)) {
    nomo_present_section("Largest residual correlations")
    nomo_present_table(
      x$largest_residuals,
      c("Item 1" = "item1", "Item 2" = "item2", "Residual" = "residual"),
      more = "nomo_table(x, \"residuals\")"
    )
  }

  if (nrow(x$top_modification_indices)) {
    nomo_present_section("Modification indices (diagnostic only)")
    mi <- x$top_modification_indices
    mi$parameter <- paste(mi$lhs, mi$op, mi$rhs)
    nomo_present_table(
      mi,
      c("Parameter" = "parameter", "MI" = "mi", "EPC" = "epc",
        "Std. EPC" = "sepc.all"),
      formats = list(mi = function(v) nomo_present_number(v, 2L)),
      more = "nomo_table(x, \"modification_indices\")"
    )
    nomo_present_text(
      "Modification indices locate strain. They do not authorize freeing a ",
      "parameter, and nomologR never does so automatically.", indent = 2L
    )
  }

  if (length(x$engine_warnings)) {
    nomo_present_section("Engine warnings")
    nomo_present_bullets(x$engine_warnings)
  }

  cat("\n")
  nomo_present_text(
    "Global fit, local strain, and parameter estimates are evidence to ",
    "interpret together; no single cutoff establishes model validity."
  )
  invisible(x)
}


#' Plot confirmatory factor-analysis evidence
#'
#' @param x A `nomo_cfa` object.
#' @param type Plot type: `"loadings"`, `"fit"`, `"residuals"`, or
#'   `"modification_indices"`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object.
#' @export
plot.nomo_cfa <- function(x,
                          type = c("loadings", "fit", "residuals", "modification_indices"),
                          ...) {
  type <- nomo_match_arg(type)

  if (type == "loadings") {
    dat <- x$standardized_loadings
    if (!nrow(dat)) stop("No standardized loadings are available to plot.", call. = FALSE)
    dat$item <- factor(dat$item, levels = rev(unique(dat$item)))
    dat$flag <- nomo_present_flag_legend(dat$attention)
    ref <- x$guidance$cfa_loading_reference
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = loading, y = item, shape = flag)) +
        ggplot2::geom_vline(xintercept = c(-ref, ref), linetype = 2) +
        ggplot2::geom_point(size = 2.8, na.rm = TRUE) +
        ggplot2::scale_shape_manual(values = nomo_present_flag_shapes, drop = TRUE) +
        ggplot2::facet_wrap(stats::as.formula("~ factor"), scales = "free_y") +
        nomo_plot_labs(
          title = "CFA standardized loadings",
          subtitle = paste(
            "Dashed lines mark the configured absolute loading review reference.",
            "Factor direction remains researcher/model dependent."
          ),
          x = "Standardized loading", y = NULL, shape = "Flag",
          caption = paste(
            "Reference values trigger inspection; they do not automatically",
            "delete indicators or validate the model."
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
    dat$metric <- factor(dat$metric, levels = c("CFI", "TLI", "RMSEA", "SRMR"))
    # CFI and TLI sit near 1 and RMSEA and SRMR near 0, so one axis squashed
    # the second pair against zero; each pair gets its own panel and scale (#89).
    dat$panel <- factor(
      ifelse(dat$metric %in% c("CFI", "TLI"), "Higher values favor fit",
             "Lower values favor fit"),
      levels = c("Higher values favor fit", "Lower values favor fit")
    )
    dat$flag <- nomo_present_flag_legend(dat$attention)
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = metric, y = value, shape = flag)) +
      ggplot2::geom_segment(
        ggplot2::aes(x = metric, xend = metric, y = reference, yend = value),
        na.rm = TRUE
      ) +
      ggplot2::geom_point(size = 3, na.rm = TRUE) +
      ggplot2::geom_point(ggplot2::aes(y = reference), shape = 4, size = 3, na.rm = TRUE) +
      ggplot2::facet_wrap(stats::as.formula("~ panel"), scales = "free") +
      ggplot2::scale_shape_manual(values = nomo_present_flag_shapes, drop = TRUE) +
      nomo_plot_labs(
        title = "CFA global fit evidence",
        subtitle = "Points are observed values; x-marks are teaching references.",
        x = NULL, y = "Fit index", shape = "Flag",
        caption = paste(
          "Cutoffs are reference points, not pass/fail laws.",
          "Interpret global fit with local strain, estimator, sample, and theory."
        )
      ) + ggplot2::theme_minimal()
    if (length(unique(dat$flag)) == 1L) p <- p + ggplot2::guides(shape = "none")
    return(p)
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
      subtitle = "Post-hoc diagnostic evidence only",
      x = "Modification index", y = NULL,
      caption = paste(
        "A large MI proposes a parameter worth investigating.",
        "It is not permission to respecify the model automatically."
      )
    ) + ggplot2::theme_minimal()
}


utils::globalVariables(c(
  "loading", "item", "factor", "flag", "metric", "value", "reference",
  "item1", "item2", "residual", "label", "mi", "panel"
))
