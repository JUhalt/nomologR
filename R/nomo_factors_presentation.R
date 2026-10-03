# Tidy-evaluation pronoun used inside ggplot2 aesthetics.
.data <- NULL


#' Print factor-retention evidence
#'
#' `print()` shows the count each main criterion suggests, the sampling
#' adequacy, and the synthesis; `summary()` adds the evidence by method, the
#' parallel-analysis rule sensitivity, the criteria that did not run and why,
#' and the concordance across criterion families.
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
    sprintf("Correlation: %s", nomo_factors_correlation_label(x$correlation_method))
  ))

  counts <- nomo_factors_method_counts(x)
  nomo_present_facts(c(
    sprintf("Criterion set: %s", x$criterion_set),
    sprintf("Methods run: %s", counts$methods),
    sprintf("Families: %d", counts$families),
    sprintf("Not run: %d", counts$skipped),
    paste0("Flags: ", nomo_present_flag_counts(nomo_factors_flag_rows(x$decision_log)$severity))
  ))

  # Revised MAP is shown only when the criterion set asked for it (#145,
  # factors-7).
  revised <- nomo_factors_ran(x, "map_revised")
  nomo_present_facts(c(
    sprintf("Parallel analysis (%s rule): %d", x$parallel$rule, x$parallel$n_factors),
    if (revised) {
      sprintf("MAP: %d (TR2), %d (TR4)", x$map$n_factors_original, x$map$n_factors_revised)
    } else {
      sprintf("MAP: %d (TR2)", x$map$n_factors_original)
    },
    paste0("KMO: ", nomo_factors_kmo_text(x$kmo))
  ))

  if (isTRUE(x$smoothed)) {
    nomo_present_text(
      "The correlation matrix was explicitly smoothed because it was not ",
      "positive definite."
    )
  }
  nomo_present_text(paste(
    "MAP = Velicer's minimum average partial criterion,",
    if (revised) "original (TR2) and revised (TR4);" else "original (TR2);",
    "KMO = Kaiser-Meyer-Olkin measure of sampling adequacy."
  ))

  cat("\n")
  nomo_present_text(nomo_factors_bind_calls(x$recommendation))
  nomo_present_pointer("summary(x)", "the evidence by method and the criteria that did not run")
  invisible(x)
}


# The decision-log rows a reader should look at again: review and concern
# rows, except those the summary already shows in their own section (the
# synthesis, the EKC qualification, and the legacy Kaiser rule).
nomo_factors_flag_rows <- function(log) {
  empty <- data.frame(metric = character(), severity = character(),
                      observation = character(), recommendation = character())
  if (!is.data.frame(log) || !all(names(empty) %in% names(log))) return(empty)
  shown <- c("retention_family_concordance", "ekc", "kaiser")
  log[log$severity %in% c("review", "concern") & !log$metric %in% shown, , drop = FALSE]
}


# Whether a criterion ran (status "available"). An object without a status
# table counts it as run.
nomo_factors_ran <- function(x, criterion) {
  status <- x$criterion_status
  if (!is.data.frame(status) || !nrow(status)) return(TRUE)
  any(status$criterion == criterion & status$status == "available")
}


# The counts in the print: the methods that ran, with a legacy rule counted
# apart because the synthesis leaves it out (#145, efa-7), the criterion
# families, and the methods requested but not run.
nomo_factors_method_counts <- function(x) {
  status <- x$criterion_status
  ran <- status$criterion[status$status == "available"]
  legacy <- sum(x$evidence$role[x$evidence$criterion %in% ran] == "legacy")
  recommended <- length(ran) - legacy
  list(
    methods = if (legacy > 0L) {
      sprintf("%d (+%d legacy)", recommended, legacy)
    } else {
      as.character(recommended)
    },
    families = if (is.null(x$family_evidence)) recommended else nrow(x$family_evidence),
    skipped = sum(status$status == "skipped")
  )
}


# A call named in the synthesis, such as nomo_table(x, "criteria"), is kept on
# one line when the text is wrapped.
nomo_factors_bind_calls <- function(text) {
  gsub("(\\w+\\(x,) ", paste0("\\1", nomo_present_nbsp), text)
}


# The overall KMO as printed: two decimals, without the leading zero.
nomo_factors_kmo_text <- function(kmo) {
  if (isTRUE(kmo$available)) {
    nomo_present_stat(kmo$overall, "proportion")
  } else {
    "not computed"
  }
}


#' Summarize factor-retention evidence
#'
#' @param object A `nomo_factors` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return An object of class `summary_nomo_factors`. Printing it shows the
#'   evidence by method, the parallel-analysis rule sensitivity, the criteria
#'   that ran with a qualification or did not run, the concordance across
#'   criterion families, the supporting adequacy evidence, and the synthesis.
#' @export
summary.nomo_factors <- function(object, ...) {
  kmo_display <- if (isTRUE(object$kmo$available)) {
    nomo_present_stat(object$kmo$overall, "proportion")
  } else {
    "unavailable"
  }

  bartlett_display <- if (isTRUE(object$bartlett$available)) {
    paste0(
      nomo_present_chisq(object$bartlett$chisq, object$bartlett$df,
                         object$bartlett$p_value),
      nomo_factors_bartlett_qualifier(object$correlation_method)
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
    sprintf("Correlation: %s", nomo_factors_correlation_label(x$correlation_method)),
    sprintf("Criterion set: %s", x$criterion_set),
    if (isTRUE(x$smoothed)) "Correlation matrix: smoothed" else ""
  ))

  nomo_present_section("Retention evidence")
  evidence <- x$evidence
  evidence$role_shown <- nomo_present_status(evidence$role)
  nomo_present_table(
    evidence,
    c("Method" = "method", "Factors" = "n_factors", "Role" = "role_shown"),
    more = "nomo_table(x, \"evidence\")"
  )

  sensitivity <- x$parallel_sensitivity
  if (is.data.frame(sensitivity) && nrow(sensitivity)) {
    nomo_present_section("Parallel-analysis rule sensitivity")
    sensitivity$rule_shown <- nomo_present_status(sensitivity$rule)
    sensitivity$chosen <- ifelse(sensitivity$selected, "Selected", "")
    nomo_present_table(
      sensitivity,
      c("Rule" = "rule_shown", "Factors" = "n_factors", "Used" = "chosen"),
      more = "x$parallel$sensitivity"
    )
  }

  status <- x$criterion_status
  qualified <- status[status$status == "available" & nzchar(status$qualification), , drop = FALSE]
  if (nrow(qualified) > 0L) {
    nomo_present_section("Criteria available with qualification")
    nomo_present_bullets(paste0(qualified$method, ": ", qualified$qualification))
  }

  skipped <- status[status$status == "skipped", , drop = FALSE]
  if (nrow(skipped) > 0L) {
    nomo_present_section("Criteria requested but not run")
    nomo_present_bullets(paste0(skipped$method, ": ", skipped$reason))
  }

  if (is.data.frame(x$concordance) && nrow(x$concordance) > 0L) {
    nomo_present_section("Concordance across criterion families")
    nomo_present_table(
      x$concordance,
      c("Factors" = "n_factors", "Families" = "n_families", "Which" = "families"),
      more = "nomo_table(x, \"concordance\")"
    )
  }

  # Bartlett's reason for not running is a sentence of its own.
  nomo_present_section("Supporting adequacy evidence")
  adequacy <- x$adequacy
  kmo <- adequacy$metric == "KMO"
  nomo_present_bullets(ifelse(
    kmo,
    paste0("KMO: ", ifelse(adequacy$available, adequacy$display,
                           "not computed; the correlation matrix could not be inverted.")),
    ifelse(adequacy$available, paste0("Bartlett's test: ", adequacy$display),
           adequacy$display)
  ))

  # Review and concern rows of the decision log, with what to do about each.
  flags <- nomo_factors_flag_rows(x$decision_log)
  nomo_present_flagged(unit = NULL, status = flags$severity,
                       text = paste(flags$observation, flags$recommendation))

  nomo_present_section("Synthesis")
  nomo_present_text(nomo_factors_bind_calls(x$recommendation), indent = 2L)

  nomo_present_key(nomo_factors_key(status), title = "Abbreviations")

  cat("\n")
  nomo_present_text(
    "Factor counts are candidates for investigation, not automatic ",
    "dimensionality verdicts. Common-factor eigenvalues come from a reduced ",
    "common-variance matrix; later values can be negative."
  )
  nomo_present_pointer("nomo_table(x, \"criteria\")", "the status of every requested criterion")
  invisible(x)
}


# The abbreviations a factor-retention summary shows, each defined once
# (guide point 23): MAP and KMO always, the others when their criterion was
# requested.
nomo_factors_key <- function(status) {
  requested <- if (is.data.frame(status)) status$criterion else character()
  entries <- c(
    MAP = paste(
      "Minimum average partial criterion (Velicer). TR2, the original, averages",
      "squared partial correlations; TR4, the revised, averages fourth powers."
    ),
    KMO = "Kaiser-Meyer-Olkin measure of sampling adequacy.",
    EKC = "Empirical Kaiser criterion.",
    NEST = "Next eigenvalue sufficiency test.",
    CAF = "Common part accounted for, the fit index Hull compares."
  )
  shown <- c("MAP", "KMO", c("EKC", "NEST", "CAF")[c("ekc", "nest", "hull") %in% requested])
  entries[shown]
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

  # One base size for every plot in the package (#145, clarity-17); the text
  # is wrapped by nomo_plot_labs(), so no label carries a hard line break.
  theme <- ggplot2::theme_minimal(base_size = 11)

  if (type == "retention") {
    d <- x$parallel$table
    observed_label <- "Observed common-factor eigenvalue"
    percentile <- nomo_factors_percentile(x$parallel$quantile)
    rule_label <- switch(
      x$parallel$rule,
      percentile = paste("Null", percentile),
      mean = "Null mean",
      crawford = sprintf("Crawford: null %s first, mean thereafter", percentile)
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
            "%s retained by the %s stopping rule",
            nomo_present_count(x$parallel$n_factors, "factor"),
            x$parallel$rule
          ),
          caption = paste(
            "Observed common-factor eigenvalues are compared with the selected null",
            "reference. Later common-factor eigenvalues can be negative."
          ),
          x = "Factor index",
          y = "Eigenvalue",
          linetype = NULL,
          shape = NULL
        ) +
        theme +
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
        caption = paste(
          "The selected rule is labeled; the alternatives are sensitivity evidence,",
          "not competing p values."
        ),
        x = NULL,
        y = "Suggested factor count"
      ) +
      theme

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
          caption = paste(
            "Common-factor and component eigenvalues answer related but different",
            "questions. Parallel analysis uses the common-factor series."
          ),
          x = "Index",
          y = "Eigenvalue",
          linetype = NULL,
          shape = NULL
        ) +
        theme +
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
        title = "Velicer's minimum average partial (MAP) criteria",
        subtitle = sprintf(
          "Original TR2 minimum: %d | Revised TR4 minimum: %d",
          x$map$n_factors_original,
          x$map$n_factors_revised
        ),
        caption = paste(
          "TR2 averages the squared partial correlations and TR4 their fourth powers.",
          "The highlighted minimum is the count each variant suggests; smaller values",
          "are preferred. The two have different scales, so the panels use separate",
          "y-axes."
        ),
        x = "Partialled components / candidate factor count",
        y = "MAP criterion value"
      ) +
      theme

    return(p)
  }

  if (type == "evidence") {
    d <- x$evidence
    # Keep method names compact on the axis. Roles remain visible in the
    # evidence table and are explained in the plot caption.
    display <- d$method
    d$method_display <- factor(display, levels = rev(display))

    abbreviations <- c(
      "MAP = minimum average partial (TR2, original; TR4, revised)",
      if (any(d$criterion == "nest")) "NEST = next eigenvalue sufficiency test",
      if (any(d$criterion == "hull")) "CAF = common part accounted for"
    )
    evidence_caption <- paste(
      "Parallel analysis is primary; the other methods are complementary or extended",
      "evidence.",
      if (any(d$role == "legacy")) "Legacy criteria are context only.",
      "Related variants are grouped by family in concordance; methods are not",
      "independent votes.",
      paste0(paste(abbreviations, collapse = "; "), ".")
    )

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
        subtitle = paste(
          "Agreement strengthens a candidate solution; disagreement invites",
          "comparison rather than averaging"
        ),
        caption = evidence_caption,
        x = "Suggested factor count",
        y = NULL
      ) +
      theme +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(hjust = 0),
        plot.subtitle = ggplot2::element_text(hjust = 0),
        plot.caption.position = "plot",
        plot.title.position = "plot"
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
        subtitle = paste(
          "Related variants, such as the two minimum average partial (MAP)",
          "criteria, are grouped before concordance is summarized; a family split",
          "between counts stays in the tables rather than being forced into one"
        ),
        caption = paste(
          "Family counts summarize convergence without double-counting closely",
          "related variants. Criterion families are related evidence, not",
          "independent votes or proof of dimensionality."
        ),
        x = "Suggested factor count",
        y = "Number of criterion families"
      ) +
      theme +
      ggplot2::theme(
        plot.caption = ggplot2::element_text(hjust = 0),
        plot.subtitle = ggplot2::element_text(hjust = 0),
        plot.caption.position = "plot",
        plot.title.position = "plot"
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

  # The values sit in a column past the longest bar and the reference line, so
  # the dashed line never strikes through them (#145, clarity-32).
  d <- x$kmo$item
  label_at <- max(c(d$msa, x$kmo$overall), na.rm = TRUE) + 0.04
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
    ggplot2::scale_y_continuous(
      limits = c(0, max(1, label_at + 0.1)),
      labels = nomo_plot_bounded_labels
    ) +
    nomo_plot_labs(
      title = "Measure of sampling adequacy (MSA) by item",
      subtitle = sprintf(
        "Dashed line: the overall Kaiser-Meyer-Olkin (KMO) value, %s",
        nomo_present_stat(x$kmo$overall, "proportion")
      ),
      x = NULL,
      y = "MSA"
    ) +
    theme

  if (show_values) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(y = label_at, label = nomo_present_stat(.data$msa, "proportion")),
      hjust = 0,
      size = 3.3
    )
  }

  p
}
