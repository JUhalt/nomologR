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
  nomo_present_header("nomo_factors", "Factor-retention evidence")
  nomo_present_facts(c(
    nomo_factors_cases_text(x$n_cases, x$min_pairwise_n),
    sprintf("Items: %d", x$n_items),
    sprintf("Correlation: %s", x$correlation_method)
  ))

  n_available <- sum(x$criterion_status$status == "available")
  n_skipped <- sum(x$criterion_status$status == "skipped")
  n_families <- if (!is.null(x$family_evidence)) nrow(x$family_evidence) else n_available
  nomo_present_facts(c(
    sprintf("Criterion set: %s", x$criterion_set),
    sprintf("Available methods: %d", n_available),
    sprintf("Families: %d", n_families),
    sprintf("Skipped: %d", n_skipped)
  ))

  nomo_present_facts(c(
    sprintf("Parallel analysis (%s): %d", x$parallel$rule, x$parallel$n_factors),
    sprintf("MAP TR2/TR4: %d/%d", x$map$n_factors_original, x$map$n_factors_revised),
    paste0("KMO: ", if (isTRUE(x$kmo$available)) {
      nomo_present_number(x$kmo$overall)
    } else {
      "unavailable"
    })
  ))

  if (isTRUE(x$smoothed)) {
    nomo_present_text("Correlation matrix: explicitly smoothed after non-PD diagnosis.")
  }

  nomo_present_text(x$recommendation)
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
    min_pairwise_n = object$min_pairwise_n,
    n_items = object$n_items,
    correlation_method = object$correlation_method,
    correlation = object$correlation_method,
    modeling_types = object$item_types,
    criterion_set = object$criterion_set,
    parallel_rule = object$parallel_rule,
    smoothed = object$smoothed,
    evidence = object$evidence,
    criterion_status = object$criterion_status,
    parallel_sensitivity = object$parallel$sensitivity,
    family_evidence = object$family_evidence,
    concordance = object$concordance,
    family_concordance = object$family_concordance,
    method_concordance = object$method_concordance,
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
  nomo_present_header("nomo_factors", "Factor-retention evidence", summary = TRUE)
  nomo_present_facts(c(
    nomo_factors_cases_text(x$n_cases, x$min_pairwise_n),
    sprintf("Items: %d", x$n_items),
    sprintf("Correlation: %s", x$correlation_method),
    sprintf("Criteria: %s", x$criterion_set),
    if (isTRUE(x$smoothed)) "Correlation matrix smoothed" else ""
  ))

  nomo_present_section("Retention evidence")
  nomo_present_table(
    x$evidence,
    c("Method" = "method", "Factors" = "n_factors", "Role" = "role")
  )

  sensitivity <- x$parallel_sensitivity
  if (is.data.frame(sensitivity) && nrow(sensitivity)) {
    nomo_present_section("Parallel-analysis rule sensitivity")
    sensitivity$chosen <- ifelse(sensitivity$selected, "selected", "")
    nomo_present_table(
      sensitivity,
      c("Rule" = "rule", "Factors" = "n_factors", "Used" = "chosen")
    )
  }

  qualified <- x$criterion_status[
    x$criterion_status$status == "available" &
      nzchar(x$criterion_status$qualification),
    ,
    drop = FALSE
  ]
  if (nrow(qualified) > 0L) {
    nomo_present_section("Criteria available with qualification")
    nomo_present_bullets(paste0(qualified$method, ": ", qualified$qualification))
  }

  skipped <- x$criterion_status[x$criterion_status$status == "skipped", , drop = FALSE]
  if (nrow(skipped) > 0L) {
    nomo_present_section("Criteria requested but not run")
    nomo_present_bullets(paste0(skipped$method, ": ", skipped$reason))
  }

  if (is.data.frame(x$concordance) && nrow(x$concordance) > 0L) {
    nomo_present_section("Concordance across criterion families")
    nomo_present_table(
      x$concordance,
      c("Factors" = "n_factors", "Families" = "n_families", "Which" = "families")
    )
  }

  nomo_present_section("Supporting adequacy evidence")
  nomo_present_bullets(paste0(x$adequacy$metric, ": ", x$adequacy$display))

  nomo_present_section("Synthesis")
  nomo_present_text(x$recommendation, indent = 2L)

  cat("\n")
  nomo_present_text(
    "Factor counts are candidates for investigation, not automatic ",
    "dimensionality verdicts. Common-factor eigenvalues come from a reduced ",
    "common-variance matrix; later values can be negative."
  )

  invisible(x)
}


#' Plot factor-retention evidence
#'
#' @param x A `nomo_factors` object.
#' @param y Unused; included for the base `plot()` generic.
#' @param type Plot type. `"retention"` compares observed common-factor
#'   eigenvalues with the selected parallel-analysis reference;
#'   `"parallel_rules"` compares PA factor counts under mean, percentile, and
#'   Crawford rules; `"scree"` displays component and common-factor eigenvalues;
#'   `"map"` displays original TR2 and revised TR4 MAP curves; `"evidence"`
#'   compares available retention criteria; `"concordance"` groups related
#'   variants into criterion families before showing support for each factor
#'   count; and `"kmo"` displays
#'   item-level KMO/MSA values.
#' @param show_values Logical; label values where useful.
#' @param ... Additional arguments, currently ignored.
#'
#' @return A `ggplot` object.
#' @export
plot.nomo_factors <- function(x,
                              y = NULL,
                              type = c(
                                "retention",
                                "parallel_rules",
                                "scree",
                                "map",
                                "evidence",
                                "concordance",
                                "kmo"
                              ),
                              show_values = TRUE,
                              ...) {
  type <- nomo_match_arg(type)

  if (!is.logical(show_values) || length(show_values) != 1L || is.na(show_values)) {
    stop("`show_values` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  if (type == "retention") {
    d <- x$parallel$table
    observed_label <- "Observed common-factor eigenvalue"
    rule_label <- switch(
      x$parallel$rule,
      percentile = sprintf("Null %.0fth percentile", 100 * x$parallel$quantile),
      mean = "Null mean",
      crawford = sprintf(
        "Crawford: %.0fth percentile first, mean thereafter",
        100 * x$parallel$quantile
      )
    )

    long <- dplyr::bind_rows(
      tibble::tibble(
        factor = d$factor,
        series = observed_label,
        eigenvalue = d$observed_eigenvalue
      ),
      tibble::tibble(
        factor = d$factor,
        series = rule_label,
        eigenvalue = d$random_reference
      )
    )
    long$series <- factor(long$series, levels = c(observed_label, rule_label))

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
          values = stats::setNames(c("solid", "dashed"), c(observed_label, rule_label))
        ) +
        ggplot2::scale_shape_manual(
          values = stats::setNames(c(16, 17), c(observed_label, rule_label))
        ) +
        ggplot2::scale_x_continuous(breaks = d$factor) +
        nomo_plot_labs(
          title = "Parallel-analysis retention evidence",
          subtitle = sprintf(
            "%d factor%s retained by the %s stopping rule",
            x$parallel$n_factors,
            if (x$parallel$n_factors == 1L) "" else "s",
            x$parallel$rule
          ),
          caption = paste0(
            "Observed common-factor eigenvalues are compared with the selected null reference.\n",
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

  if (type == "parallel_rules") {
    d <- x$parallel$sensitivity
    rule_labels <- c(
      percentile = "Percentile",
      mean = "Mean",
      crawford = "Crawford"
    )
    display <- unname(rule_labels[d$rule])
    display[d$selected] <- paste0(display[d$selected], "\n(selected)")
    d$rule_display <- factor(display, levels = display)

    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(x = .data$rule_display, y = .data$n_factors)
    ) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::scale_y_continuous(
        breaks = seq.int(0, max(c(1L, d$n_factors)), by = 1),
        expand = ggplot2::expansion(mult = c(0, 0.14))
      ) +
      nomo_plot_labs(
        title = "Parallel-analysis rule sensitivity",
        subtitle = sprintf("Selected rule: %s", x$parallel$rule),
        caption = "The selected rule is labeled; alternatives are sensitivity evidence, not competing p-values.",
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
    long$series <- factor(long$series, levels = c(common_label, component_label))

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
        nomo_plot_labs(
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

  if (type == "map") {
    d <- x$map$table
    long <- dplyr::bind_rows(
      tibble::tibble(
        n_factors = d$n_factors,
        criterion = "Original MAP (TR2)",
        value = d$map_original,
        minimum = d$minimum_original
      ),
      tibble::tibble(
        n_factors = d$n_factors,
        criterion = "Revised MAP (TR4)",
        value = d$map_revised,
        minimum = d$minimum_revised
      )
    )
    long$criterion <- factor(
      long$criterion,
      levels = c("Original MAP (TR2)", "Revised MAP (TR4)")
    )

    p <- ggplot2::ggplot(
      long,
      ggplot2::aes(
        x = .data$n_factors,
        y = .data$value
      )
    ) +
      ggplot2::geom_line(linewidth = 0.8, na.rm = TRUE) +
      ggplot2::geom_point(size = 2.2, na.rm = TRUE) +
      ggplot2::geom_point(
        data = long[long$minimum & is.finite(long$value), , drop = FALSE],
        size = 4,
        stroke = 1.2,
        na.rm = TRUE
      ) +
      ggplot2::facet_wrap(
        stats::as.formula("~ criterion"),
        ncol = 1,
        scales = "free_y"
      ) +
      ggplot2::scale_x_continuous(breaks = d$n_factors) +
      nomo_plot_labs(
        title = "Velicer MAP sensitivity",
        subtitle = sprintf(
          "Original TR2 minimum: %d | Revised TR4 minimum: %d",
          x$map$n_factors_original,
          x$map$n_factors_revised
        ),
        caption = paste0(
          "The highlighted minimum is the suggested count for each MAP variant; smaller values are preferred.\n",
          "TR2 and TR4 have different numerical scales, so the panels use separate y-axes."
        ),
        x = "Partialled components / candidate factor count",
        y = "MAP criterion value"
      ) +
      ggplot2::theme_minimal(base_size = 12)

    return(p)
  }

  if (type == "evidence") {
    d <- x$evidence
    # Keep method names compact on the axis. Roles remain visible in the
    # evidence table and are explained in the plot caption.
    display <- d$method
    d$method_display <- factor(display, levels = rev(display))

    evidence_caption <- if (any(d$role == "legacy")) {
      paste0(
        "Parallel analysis is primary; other displayed methods are complementary or extended evidence.\n",
        "Legacy criteria are context only. Related variants are grouped by family in concordance; methods are not independent votes."
      )
    } else {
      paste0(
        "Parallel analysis is primary; other displayed methods are complementary or extended evidence.\n",
        "Related variants are grouped by family in concordance; methods are not independent votes."
      )
    }

    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(y = .data$method_display, x = .data$n_factors)
    ) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::scale_x_continuous(
        breaks = seq.int(0, max(c(1L, d$n_factors)), by = 1),
        expand = ggplot2::expansion(mult = c(0, 0.14))
      ) +
      nomo_plot_labs(
        title = "Factor-retention evidence by method",
        subtitle = paste0(
          "Agreement strengthens a candidate solution;\n",
          "disagreement invites comparison rather than averaging"
        ),
        caption = evidence_caption,
        x = "Suggested factor count",
        y = NULL
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(hjust = 0),
        plot.subtitle = ggplot2::element_text(hjust = 0)
      )

    if (show_values) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data$n_factors),
        hjust = -0.4
      )
    }
    return(p)
  }

  if (type == "concordance") {
    d <- x$family_concordance
    if (is.null(d)) {
      d <- x$concordance
    }
    if (!is.data.frame(d) || nrow(d) == 0L) {
      stop(
        "No internally consistent criterion-family recommendations are available to plot.",
        call. = FALSE
      )
    }

    p <- ggplot2::ggplot(
      d,
      ggplot2::aes(x = factor(.data$n_factors), y = .data$n_families)
    ) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::scale_y_continuous(
        breaks = seq.int(0, max(c(1L, d$n_families)), by = 1),
        expand = ggplot2::expansion(mult = c(0, 0.14))
      ) +
      nomo_plot_labs(
        title = "Retention-evidence concordance by criterion family",
        subtitle = paste0(
          "Related variants such as MAP TR2/TR4 are grouped before concordance is summarized;\n",
          "internally split families remain visible in the tables rather than being forced into one count"
        ),
        caption = paste0(
          "Family counts summarize convergence without double-counting closely related variants.\n",
          "Criterion families are related evidence, not independent votes or proof of dimensionality."
        ),
        x = "Suggested factor count",
        y = "Number of criterion families"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(hjust = 0),
        plot.subtitle = ggplot2::element_text(hjust = 0)
      )

    if (show_values) {
      p <- p + ggplot2::geom_text(
        ggplot2::aes(label = .data$n_families),
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
    nomo_plot_labs(
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
