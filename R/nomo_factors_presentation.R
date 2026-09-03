# Tidy-evaluation pronoun used inside ggplot2 aesthetics.
.data <- NULL


#' Print factor-retention evidence
#'
#' @param x A `nomo_factors` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return `x`, invisibly.
#' @export
print.nomo_factors <- function(x, ...) {
  cat("<nomo_factors>\n")
  cat(sprintf(
    "Cases: %d | Items: %d | Correlation: %s\n",
    x$n_cases,
    x$n_items,
    x$correlation_method
  ))
  cat(sprintf(
    "Parallel analysis: %d | MAP (original): %d | KMO: %s\n",
    x$parallel$n_factors,
    x$map$n_factors,
    if (isTRUE(x$kmo$available)) sprintf("%.3f", x$kmo$overall) else "unavailable"
  ))
  if (isTRUE(x$smoothed)) {
    cat("Correlation matrix: explicitly smoothed after non-PD diagnosis\n")
  }
  cat(x$recommendation, "\n")
  invisible(x)
}


#' Summarize factor-retention evidence
#'
#' @param object A `nomo_factors` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return An object of class `summary_nomo_factors`.
#' @export
summary.nomo_factors <- function(object, ...) {
  evidence <- object$evidence

  kmo_display <- if (isTRUE(object$kmo$available)) {
    sprintf("%.3f", object$kmo$overall)
  } else {
    "unavailable"
  }

  bartlett_display <- if (isTRUE(object$bartlett$available)) {
    sprintf(
      "chi-square(%d) = %.2f, p %s",
      as.integer(object$bartlett$df),
      object$bartlett$chisq,
      nomo_format_p(object$bartlett$p_value)
    )
  } else {
    object$bartlett$reason
  }

  adequacy <- tibble::tibble(
    metric = c("KMO", "Bartlett"),
    value = c(
      if (isTRUE(object$kmo$available)) object$kmo$overall else NA_real_,
      if (isTRUE(object$bartlett$available)) object$bartlett$p_value else NA_real_
    ),
    available = c(
      isTRUE(object$kmo$available),
      isTRUE(object$bartlett$available)
    ),
    display = c(kmo_display, bartlett_display)
  )

  out <- list(
    n_cases = object$n_cases,
    n_items = object$n_items,
    correlation_method = object$correlation_method,
    smoothed = object$smoothed,
    evidence = evidence,
    adequacy = adequacy,
    item_kmo = object$kmo$item,
    plausible_factors = object$plausible_factors,
    recommendation = object$recommendation,
    decision_log = object$decision_log
  )

  class(out) <- c("summary_nomo_factors", "list")
  out
}


#' Print a factor-retention summary
#'
#' @param x A `summary_nomo_factors` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return `x`, invisibly.
#' @export
print.summary_nomo_factors <- function(x, ...) {
  cat("<summary_nomo_factors>\n")
  cat(sprintf(
    "Cases: %d | Items: %d | Correlation: %s%s\n",
    x$n_cases,
    x$n_items,
    x$correlation_method,
    if (isTRUE(x$smoothed)) " | SMOOTHED" else ""
  ))

  cat("\nRetention evidence:\n")
  print(x$evidence, n = Inf, width = Inf)

  cat("\nSupporting adequacy evidence:\n")
  print(x$adequacy[c("metric", "display")], n = Inf, width = Inf)

  cat("\nSynthesis:\n")
  cat(x$recommendation, "\n")
  cat(
    "\nFactor counts are candidates for investigation, not automatic dimensionality verdicts.\n"
  )
  cat(
    "Common-factor eigenvalues come from a reduced common-variance matrix; later values can be negative.\n"
  )

  invisible(x)
}


#' Plot factor-retention evidence
#'
#' @param x A `nomo_factors` object.
#' @param y Unused; included for the base `plot()` generic.
#' @param type Plot type: `"retention"` compares observed common-factor
#'   eigenvalues with the parallel-analysis null reference; `"scree"` displays
#'   observed component and common-factor eigenvalues; `"evidence"` compares the
#'   factor counts suggested by parallel analysis and original MAP; `"kmo"`
#'   displays item-level KMO/MSA values.
#' @param show_values Logical; label values where useful.
#' @param ... Additional arguments, currently ignored.
#'
#' @return A `ggplot` object.
#' @export
plot.nomo_factors <- function(x,
                              y = NULL,
                              type = c(
                                "retention",
                                "scree",
                                "evidence",
                                "kmo"
                              ),
                              show_values = TRUE,
                              ...) {
  type <- match.arg(type)

  if (!is.logical(show_values) || length(show_values) != 1L || is.na(show_values)) {
    stop("`show_values` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  if (type == "retention") {
    d <- x$parallel$table
    observed_label <- "Observed common-factor eigenvalue"
    null_label <- sprintf(
      "Null %.0fth percentile",
      100 * x$parallel$quantile
    )

    long <- dplyr::bind_rows(
      tibble::tibble(
        factor = d$factor,
        series = observed_label,
        eigenvalue = d$observed_eigenvalue
      ),
      tibble::tibble(
        factor = d$factor,
        series = null_label,
        eigenvalue = d$random_reference
      )
    )
    long$series <- factor(
      long$series,
      levels = c(observed_label, null_label)
    )

    return(
      ggplot2::ggplot(
        long,
        ggplot2::aes(
          x = .data$factor,
          y = .data$eigenvalue,
          linetype = .data$series,
          shape = .data$series
        )
      ) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 2.3) +
        ggplot2::scale_linetype_manual(
          values = stats::setNames(c("solid", "dashed"), c(observed_label, null_label))
        ) +
        ggplot2::scale_shape_manual(
          values = stats::setNames(c(16, 17), c(observed_label, null_label))
        ) +
        ggplot2::scale_x_continuous(breaks = d$factor) +
        ggplot2::labs(
          title = "Parallel-analysis retention evidence",
          subtitle = sprintf(
            "%d factor%s retained by the parallel-analysis stopping rule",
            x$parallel$n_factors,
            if (x$parallel$n_factors == 1L) "" else "s"
          ),
          caption = paste0(
            "Observed common-factor eigenvalues are compared with a null reference.\n",
            "Later common-factor eigenvalues can be negative."
          ),
          x = "Factor index",
          y = "Eigenvalue",
          linetype = NULL,
          shape = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom")
    )
  }

  if (type == "scree") {
    d <- x$scree
    common_label <- "Common factor"
    component_label <- "Component"

    long <- dplyr::bind_rows(
      tibble::tibble(
        index = d$index,
        series = common_label,
        eigenvalue = d$factor_eigenvalue
      ),
      tibble::tibble(
        index = d$index,
        series = component_label,
        eigenvalue = d$component_eigenvalue
      )
    )
    long$series <- factor(
      long$series,
      levels = c(common_label, component_label)
    )

    return(
      ggplot2::ggplot(
        long,
        ggplot2::aes(
          x = .data$index,
          y = .data$eigenvalue,
          linetype = .data$series,
          shape = .data$series
        )
      ) +
        ggplot2::geom_hline(yintercept = 0, linewidth = 0.4) +
        ggplot2::geom_line(linewidth = 0.8) +
        ggplot2::geom_point(size = 2.3) +
        ggplot2::scale_linetype_manual(
          values = c("Common factor" = "solid", "Component" = "dashed")
        ) +
        ggplot2::scale_shape_manual(
          values = c("Common factor" = 16, "Component" = 17)
        ) +
        ggplot2::scale_x_continuous(breaks = d$index) +
        ggplot2::labs(
          title = "Observed scree information",
          subtitle = "Use the shape as complementary evidence; do not automate the elbow",
          caption = paste0(
            "Common-factor and component eigenvalues answer related but different questions.\n",
            "Parallel analysis uses the common-factor series."
          ),
          x = "Index",
          y = "Eigenvalue",
          linetype = NULL,
          shape = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(legend.position = "bottom")
    )
  }

  if (type == "evidence") {
    d <- x$evidence
    d$method <- factor(d$method, levels = d$method)
    d$method_display <- factor(
      paste0(as.character(d$method), "\n(", d$role, ")"),
      levels = paste0(as.character(d$method), "\n(", d$role, ")")
    )

    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(x = .data$method_display, y = .data$n_factors)
    ) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::scale_y_continuous(
        breaks = seq.int(0, max(c(1L, d$n_factors)), by = 1),
        expand = ggplot2::expansion(mult = c(0, 0.12))
      ) +
      ggplot2::labs(
        title = "Factor-retention evidence by method",
        subtitle = "Agreement strengthens a candidate solution; disagreement invites comparison",
        x = NULL,
        y = "Suggested factor count"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    if (show_values) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data$n_factors),
        vjust = -0.4
      )
    }

    return(p)
  }

  if (!isTRUE(x$kmo$available)) {
    stop("Item-level KMO/MSA values are unavailable for this object.", call. = FALSE)
  }

  d <- x$kmo$item

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = stats::reorder(.data$item, .data$msa),
      y = .data$msa
    )
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::geom_hline(
      yintercept = x$kmo$overall,
      linetype = "dashed",
      linewidth = 0.6
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Item-level KMO / measure of sampling adequacy",
      subtitle = sprintf("Dashed line = overall KMO (%.3f)", x$kmo$overall),
      x = NULL,
      y = "MSA"
    ) +
    ggplot2::theme_minimal(base_size = 12)

  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.2f", .data$msa)),
      hjust = -0.15,
      size = 3.3
    ) +
      ggplot2::scale_y_continuous(
        limits = c(0, max(1, max(d$msa, na.rm = TRUE) * 1.08))
      )
  }

  p
}


nomo_format_p <- function(p) {
  if (length(p) != 1L || is.na(p)) {
    return("unavailable")
  }
  if (p < 0.001) {
    return("< .001")
  }
  paste0("= ", formatC(p, format = "f", digits = 3))
}
