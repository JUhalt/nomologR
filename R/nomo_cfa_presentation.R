# Presentation methods for nomo_cfa -----------------------------------------

#' @export
print.nomo_cfa <- function(x, ...) {
  cat("<nomo_cfa>\n")
  cat(sprintf(
    "Cases: %s used of %d | Estimator: %s",
    if (is.finite(x$n_used)) format(x$n_used, trim = TRUE) else "unknown",
    x$data_n,
    ifelse(is.na(x$estimator), "unknown", x$estimator)
  ))
  if (!is.na(x$estimator_engine) && !is.na(x$estimator) &&
      !identical(x$estimator_engine, x$estimator)) {
    cat(sprintf(" (engine: %s)", x$estimator_engine))
  }
  cat("\n")
  cat(sprintf(
    "Converged: %s | Ordered indicators: %d | Heywood flags: %d\n",
    if (x$converged) "yes" else "NO",
    length(x$ordered), nrow(x$heywood)
  ))

  fit_show <- x$fit_evidence[
    x$fit_evidence$metric %in% c("CFI", "TLI", "RMSEA", "SRMR"),
    c("metric", "value"), drop = FALSE
  ]
  available <- is.finite(fit_show$value)
  if (any(available)) {
    pieces <- paste0(
      fit_show$metric[available], "=",
      formatC(fit_show$value[available], format = "f", digits = 3)
    )
    cat("Global fit: ", paste(pieces, collapse = " | "), "\n", sep = "")
  }

  counts <- table(factor(
    x$standardized_loadings$attention,
    levels = c("KEEP", "REVIEW", "STRONG REVIEW")
  ))
  cat(sprintf(
    "Loading review: %d KEEP | %d REVIEW | %d STRONG REVIEW\n",
    counts[["KEEP"]], counts[["REVIEW"]], counts[["STRONG REVIEW"]]
  ))
  if (length(x$engine_warnings)) {
    cat(sprintf(
      "Captured engine warnings: %d (inspect `$engine_warnings` / decision log)\n",
      length(x$engine_warnings)
    ))
  }
  cat("No parameters were automatically freed and no model was automatically refit.\n")
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
  cat("nomologR confirmatory factor analysis\n")
  cat(sprintf(
    "%s cases used of %d | Estimator: %s",
    if (is.finite(x$n_used)) format(x$n_used, trim = TRUE) else "unknown",
    x$data_n,
    ifelse(is.na(x$estimator), "unknown", x$estimator)
  ))
  if (!is.na(x$estimator_engine) && !is.na(x$estimator) &&
      !identical(x$estimator_engine, x$estimator)) {
    cat(sprintf(" (engine: %s)", x$estimator_engine))
  }
  cat("\n")
  cat(sprintf(
    "Converged: %s | Ordered indicators: %d\n",
    if (x$converged) "yes" else "NO", length(x$ordered)
  ))
  if (is.finite(x$n_dropped) && x$n_dropped > 0) {
    cat(sprintf(
      "Case use: %d not used (%.1f%% of input)\n",
      as.integer(x$n_dropped),
      100 * x$pct_dropped
    ))
  }
  cat("\n")

  cat("Global fit evidence\n")
  print(
    x$fit_evidence[, c("metric", "value", "variant", "reference", "attention")],
    row.names = FALSE
  )

  cat("\nStandardized loadings\n")
  loading_cols <- c(
    "factor", "item", "loading", "se", "ci_lower", "ci_upper", "attention"
  )
  print(x$standardized_loadings[, loading_cols, drop = FALSE], row.names = FALSE)

  flagged <- x$standardized_loadings[
    x$standardized_loadings$attention != "KEEP", , drop = FALSE
  ]
  if (!nrow(flagged)) {
    cat("\nNo configured standardized-loading review flags were triggered.\n")
  } else {
    cat("\nLoading flags requiring inspection\n")
    print(
      flagged[, c("factor", "item", "attention", "explanation"), drop = FALSE],
      row.names = FALSE, width = Inf
    )
  }

  if (nrow(x$factor_correlations)) {
    cat("\nFactor correlations\n")
    print(
      x$factor_correlations[, c(
        "factor1", "factor2", "correlation", "ci_lower", "ci_upper"
      ), drop = FALSE],
      row.names = FALSE
    )
  }

  if (nrow(x$heywood)) {
    cat("\nImproper-solution / Heywood signals\n")
    print(x$heywood, row.names = FALSE, width = Inf)
  } else {
    cat("\nNo configured Heywood/improper-solution signal was detected.\n")
  }

  if (nrow(x$largest_residuals)) {
    cat("\nLargest localized residual correlations\n")
    print(x$largest_residuals, row.names = FALSE)
  }

  if (nrow(x$top_modification_indices)) {
    cat("\nTop modification indices - diagnostic only\n")
    mi_cols <- intersect(
      c("lhs", "op", "rhs", "mi", "epc", "sepc.all"),
      names(x$top_modification_indices)
    )
    print(x$top_modification_indices[, mi_cols, drop = FALSE], row.names = FALSE)
    cat("Modification indices do not authorize automatic respecification.\n")
  }

  if (length(x$engine_warnings)) {
    cat("\nCaptured engine warnings\n")
    for (w in x$engine_warnings) cat("- ", w, "\n", sep = "")
  }

  cat(
    "\nInterpretation rule: global fit, local strain, and parameter estimates ",
    "are evidence to interpret together; no single cutoff establishes model ",
    "validity.\n", sep = ""
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
  type <- match.arg(type)

  if (type == "loadings") {
    dat <- x$standardized_loadings
    if (!nrow(dat)) stop("No standardized loadings are available to plot.", call. = FALSE)
    dat$item <- factor(dat$item, levels = rev(unique(dat$item)))
    ref <- x$guidance$cfa_loading_reference
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = loading, y = item, shape = attention)) +
        ggplot2::geom_vline(xintercept = c(-ref, ref), linetype = 2) +
        ggplot2::geom_point(size = 2.8, na.rm = TRUE) +
        ggplot2::facet_wrap(stats::as.formula("~ factor"), scales = "free_y") +
        ggplot2::labs(
          title = "CFA standardized loadings",
          subtitle = paste(
            "Dashed lines mark the configured absolute loading review reference.",
            "Factor direction remains researcher/model dependent."
          ),
          x = "Standardized loading", y = NULL, shape = "Review",
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
    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = metric, y = value, shape = attention)) +
        ggplot2::geom_segment(
          ggplot2::aes(x = metric, xend = metric, y = reference, yend = value),
          na.rm = TRUE
        ) +
        ggplot2::geom_point(size = 3, na.rm = TRUE) +
        ggplot2::geom_point(ggplot2::aes(y = reference), shape = 4, size = 3, na.rm = TRUE) +
        ggplot2::labs(
          title = "CFA global fit evidence",
          subtitle = paste(
            "Points are observed values; x-marks are teaching references.",
            "CFI/TLI favor higher values; RMSEA/SRMR favor lower values."
          ),
          x = NULL, y = "Fit index", shape = "Review",
          caption = paste(
            "Cutoffs are reference points, not pass/fail laws.",
            "Interpret global fit with local strain, estimator, sample, and theory."
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
        ggplot2::labs(
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
    ggplot2::labs(
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
  "loading", "item", "factor", "attention", "metric", "value", "reference",
  "item1", "item2", "residual", "label", "mi"
))
