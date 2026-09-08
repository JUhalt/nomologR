# Nomological-network presentation --------------------------------------------

nomo_network_pretty_status <- function(x) {
  lookup <- c(
    concordant = "Concordant",
    directionally_concordant_imprecise = "Directionally concordant / imprecise",
    direction_concordant_below_magnitude = "Direction concordant / below magnitude",
    inconclusive = "Inconclusive",
    inconsistent = "Inconsistent",
    not_confirmable_without_sesoi = "Not confirmable without SESOI",
    not_evaluable = "Not evaluable",
    replicated_concordance = "Replicated concordance",
    mixed_or_inconclusive = "Mixed / inconclusive",
    not_replicated = "Not replicated",
    unstable = "Unstable",
    replicated_inconsistency = "Replicated inconsistency",
    direction_replicated_but_uncertain = "Direction replicated / uncertain",
    sign_reversal = "Sign reversal",
    post_hoc_exploratory = "Post-hoc / exploratory",
    a_priori = "A priori"
  )

  out <- unname(lookup[as.character(x)])
  missing <- is.na(out)
  out[missing] <- gsub("_", " ", as.character(x)[missing], fixed = TRUE)
  out
}


nomo_network_concordance_levels <- function() {
  c(
    "Inconsistent",
    "Not evaluable",
    "Not confirmable without SESOI",
    "Inconclusive",
    "Direction concordant / below magnitude",
    "Directionally concordant / imprecise",
    "Concordant"
  )
}


nomo_network_theory_plot_data <- function(x) {
  dat <- x$hypothesis_evidence

  h <- x$hypotheses$hypotheses[, c(
    "id",
    "lower",
    "upper",
    "lower_inclusive",
    "upper_inclusive"
  ), drop = FALSE]

  idx <- match(dat$id, h$id)

  dat$theory_lower <- h$lower[idx]
  dat$theory_upper <- h$upper[idx]
  dat$theory_lower_inclusive <- h$lower_inclusive[idx]
  dat$theory_upper_inclusive <- h$upper_inclusive[idx]

  finite_values <- c(
    dat$ci_lower[is.finite(dat$ci_lower)],
    dat$ci_upper[is.finite(dat$ci_upper)],
    dat$estimate[is.finite(dat$estimate)],
    dat$theory_lower[is.finite(dat$theory_lower)],
    dat$theory_upper[is.finite(dat$theory_upper)],
    0
  )

  # `finite_values` always contains the explicit zero reference above, so
  # the empty-vector branch is unreachable by construction.
  plot_min <- min(finite_values, na.rm = TRUE)
  plot_max <- max(finite_values, na.rm = TRUE)
  span <- plot_max - plot_min
  if (!is.finite(span) || span <= 0) span <- 1
  plot_min <- plot_min - .10 * span
  plot_max <- plot_max + .10 * span

  dat$theory_lower_plot <- ifelse(
    is.finite(dat$theory_lower),
    dat$theory_lower,
    plot_min
  )
  dat$theory_upper_plot <- ifelse(
    is.finite(dat$theory_upper),
    dat$theory_upper,
    plot_max
  )

  list(
    data = dat,
    limits = c(plot_min, plot_max)
  )
}


#' @export
print.nomo_network <- function(x, ...) {
  cat("<nomo_network>\n")
  cat(sprintf(
    "%s sample: N = %d | Converged: %s\n",
    tools::toTitleCase(x$sample_role),
    x$data_n,
    x$converged
  ))

  if (!is.na(x$validation_n)) {
    cat(sprintf("Validation sample: N = %d\n", x$validation_n))
  }

  added <- sum(x$model_relations$added_from_hypothesis)
  cat(sprintf(
    "Theory relations: %d | Added transparently to model: %d\n",
    x$hypotheses$n,
    added
  ))

  measurement <- x$measurement_context$summary[1L, , drop = FALSE]
  cat(sprintf(
    "Measurement context: %s | %s\n\n",
    toupper(measurement$attention[[1L]]),
    measurement$observation[[1L]]
  ))

  show <- x$hypothesis_evidence[, c(
    "id",
    "relation",
    "prediction",
    "theoretical_region",
    "estimate",
    "ci_lower",
    "ci_upper",
    "concordance",
    "confirmatory_status"
  ), drop = FALSE]

  show$concordance <- nomo_network_pretty_status(show$concordance)
  show$confirmatory_status <- nomo_network_pretty_status(
    show$confirmatory_status
  )

  print(show, n = Inf, width = Inf)

  if (nrow(x$replication_evidence)) {
    cat("\nReplication evidence\n")
    rep_show <- x$replication_evidence[, c(
      "id",
      "relation",
      "primary_estimate",
      "validation_estimate",
      "replication_status"
    ), drop = FALSE]
    rep_show$replication_status <- nomo_network_pretty_status(
      rep_show$replication_status
    )
    print(rep_show, n = Inf, width = Inf)
  }

  cat(
    "\nInterpretation rule: theory concordance, uncertainty, measurement ",
    "quality, and replication are distinct evidence streams. Statistical ",
    "significance alone is not a validity verdict.\n",
    sep = ""
  )

  invisible(x)
}


#' @export
summary.nomo_network <- function(object, ...) {
  counts <- as.data.frame(table(
    object$hypothesis_evidence$concordance,
    useNA = "ifany"
  ))
  names(counts) <- c("concordance", "n")

  replication_counts <- if (nrow(object$replication_evidence)) {
    tmp <- as.data.frame(table(
      object$replication_evidence$replication_status,
      useNA = "ifany"
    ))
    names(tmp) <- c("replication_status", "n")
    tibble::as_tibble(tmp)
  } else {
    tibble::tibble()
  }

  out <- list(
    sample_role = object$sample_role,
    data_n = object$data_n,
    validation_n = object$validation_n,
    converged = object$converged,
    fit_evidence = object$fit_evidence,
    measurement_context = object$measurement_context,
    hypothesis_evidence = object$hypothesis_evidence,
    concordance_counts = tibble::as_tibble(counts),
    replication_evidence = object$replication_evidence,
    replication_counts = replication_counts,
    model_relations = object$model_relations,
    decision_log = object$decision_log
  )

  class(out) <- c("summary_nomo_network", "list")
  out
}


#' @export
print.summary_nomo_network <- function(x, ...) {
  cat("nomologR theory-specified network summary\n")
  cat(sprintf(
    "%s N: %d | Converged: %s\n",
    tools::toTitleCase(x$sample_role),
    x$data_n,
    x$converged
  ))
  if (!is.na(x$validation_n)) {
    cat(sprintf("Validation N: %d\n", x$validation_n))
  }

  cat("\nMeasurement context\n")
  print(x$measurement_context$summary, n = Inf, width = Inf)

  cat("\nModel fit evidence\n")
  print(x$fit_evidence, n = Inf, width = Inf)

  cat("\nHypothesis evidence\n")
  h_show <- x$hypothesis_evidence[, c(
    "id",
    "relation",
    "evidence_scope",
    "estimate",
    "ci_lower",
    "ci_upper",
    "concordance",
    "measurement_attention",
    "confirmatory_status"
  ), drop = FALSE]
  h_show$concordance <- nomo_network_pretty_status(h_show$concordance)
  h_show$confirmatory_status <- nomo_network_pretty_status(
    h_show$confirmatory_status
  )
  print(h_show, n = Inf, width = Inf)

  cat("\nConcordance counts\n")
  count_show <- x$concordance_counts
  count_show$concordance <- nomo_network_pretty_status(
    count_show$concordance
  )
  print(count_show, n = Inf, width = Inf)

  if (nrow(x$replication_evidence)) {
    cat("\nReplication evidence\n")
    rep_show <- x$replication_evidence
    rep_show$replication_status <- nomo_network_pretty_status(
      rep_show$replication_status
    )
    print(rep_show, n = Inf, width = Inf)
  }

  invisible(x)
}


#' Plot nomological-network evidence
#'
#' @param x A `nomo_network` object.
#' @param type Plot type: `"effects"`, `"concordance"`, `"fit"`, or
#'   `"replication"`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object.
#' @export
plot.nomo_network <- function(
    x,
    type = c("effects", "concordance", "fit", "replication"),
    ...) {
  type <- match.arg(type)

  if (type == "effects") {
    prepared <- nomo_network_theory_plot_data(x)
    dat <- prepared$data
    limits <- prepared$limits

    dat <- dat[
      is.finite(dat$estimate) &
        is.finite(dat$ci_lower) &
        is.finite(dat$ci_upper),
      ,
      drop = FALSE
    ]
    if (!nrow(dat)) {
      stop(
        "No finite hypothesis estimates and confidence intervals are available.",
        call. = FALSE
      )
    }

    dat$relation <- factor(dat$relation, levels = rev(dat$relation))
    dat$concordance_display <- nomo_network_pretty_status(dat$concordance)

    return(
      ggplot2::ggplot(dat, ggplot2::aes(y = relation)) +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = theory_lower_plot,
            xend = theory_upper_plot,
            yend = relation
          ),
          linewidth = 5,
          alpha = .16,
          lineend = "butt"
        ) +
        ggplot2::geom_vline(xintercept = 0, linetype = 2) +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = ci_lower,
            xend = ci_upper,
            yend = relation
          ),
          linewidth = .6,
          na.rm = TRUE
        ) +
        ggplot2::geom_point(
          ggplot2::aes(
            x = estimate,
            shape = concordance_display
          ),
          size = 3,
          na.rm = TRUE
        ) +
        ggplot2::coord_cartesian(xlim = limits, clip = "off") +
        ggplot2::scale_x_continuous(
          expand = ggplot2::expansion(mult = c(.02, .04))
        ) +
        ggplot2::labs(
          title = "Theory-specified nomological effects",
          subtitle = paste(
            "Points are estimates; thin lines are confidence intervals.",
            "\nThick background segments show the prespecified theory-compatible region."
          ),
          x = "Estimate on the hypothesis-specified scale",
          y = NULL,
          shape = "Theory evidence",
          caption = paste(
            "Theory regions are researcher specified; measurement context and",
            "\na-priori/post-hoc status remain separate evidence streams."
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.margin = ggplot2::margin(8, 18, 8, 8)
        )
    )
  }

  if (type == "concordance") {
    dat <- x$hypothesis_evidence
    if (!nrow(dat)) {
      stop("No hypothesis evidence is available to plot.", call. = FALSE)
    }

    dat$concordance_display <- nomo_network_pretty_status(dat$concordance)
    dat$concordance_display <- factor(
      dat$concordance_display,
      levels = nomo_network_concordance_levels()
    )
    dat$relation <- factor(
      dat$relation,
      levels = rev(dat$relation)
    )

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(
          x = concordance_display,
          y = relation
        )
      ) +
        ggplot2::geom_point(size = 3.4) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::labs(
          title = "Relation-level nomological evidence",
          subtitle = paste(
            "Each theory-specified relation is shown at its current",
            "evidence classification."
          ),
          x = "Theory-evidence classification",
          y = NULL,
          caption = paste(
            "This is not a package-level validity score.",
            "Inspect estimates, uncertainty, measurement context, and",
            "replication alongside the classification."
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            angle = 35,
            hjust = 1
          )
        )
    )
  }

  if (type == "fit") {
    refs <- x$guidance$fit_reference
    dat <- data.frame(
      metric = factor(
        c("CFI", "TLI", "RMSEA", "SRMR"),
        levels = c("CFI", "TLI", "RMSEA", "SRMR")
      ),
      value = c(
        x$fit_evidence$cfi[[1L]],
        x$fit_evidence$tli[[1L]],
        x$fit_evidence$rmsea[[1L]],
        x$fit_evidence$srmr[[1L]]
      ),
      reference = c(
        refs$cfi,
        refs$tli,
        refs$rmsea,
        refs$srmr
      ),
      panel_x = 1
    )
    dat <- dat[is.finite(dat$value), , drop = FALSE]

    if (!nrow(dat)) {
      stop("No finite global fit evidence is available.", call. = FALSE)
    }

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = panel_x, y = value)
      ) +
        ggplot2::geom_segment(
          ggplot2::aes(
            x = panel_x,
            xend = panel_x,
            y = reference,
            yend = value
          ),
          na.rm = TRUE
        ) +
        ggplot2::geom_point(size = 3, na.rm = TRUE) +
        ggplot2::geom_point(
          ggplot2::aes(y = reference),
          shape = 4,
          size = 3,
          na.rm = TRUE
        ) +
        ggplot2::facet_wrap(
          stats::as.formula("~ metric"),
          scales = "free_y",
          nrow = 1
        ) +
        ggplot2::scale_x_continuous(
          breaks = NULL,
          expand = ggplot2::expansion(mult = .25)
        ) +
        ggplot2::labs(
          title = "Nomological-network global fit evidence",
          subtitle = paste(
            "Points are observed values; x-marks are configured teaching",
            "references. Each metric uses its own scale."
          ),
          x = NULL,
          y = "Fit index",
          caption = paste(
            "Reference values trigger inspection; they are not pass/fail laws.",
            "\nInterpret global fit with local strain, estimator, sample, and theory."
          )
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (!nrow(x$replication_evidence)) {
    stop(
      "No validation sample was fitted, so replication evidence is unavailable.",
      call. = FALSE
    )
  }

  dat <- x$replication_evidence
  dat <- dat[
    is.finite(dat$primary_estimate) &
      is.finite(dat$validation_estimate),
    ,
    drop = FALSE
  ]

  if (!nrow(dat)) {
    stop("No finite replication estimates are available to plot.", call. = FALSE)
  }

  dat$replication_display <- nomo_network_pretty_status(
    dat$replication_status
  )

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = primary_estimate,
      y = validation_estimate,
      shape = replication_display
    )
  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(
      data = dat,
      mapping = ggplot2::aes(
        x = primary_estimate,
        y = validation_estimate,
        label = relation
      ),
      inherit.aes = FALSE,
      nudge_y = .018,
      check_overlap = TRUE
    ) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = c(.12, .20))
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(.12, .20))
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(
      title = "Primary versus validation nomological effects",
      subtitle = "The dashed identity line represents identical estimates across samples.",
      x = "Primary estimate",
      y = "Validation estimate",
      shape = "Replication evidence",
      caption = paste(
        "Replication status reflects theory concordance and stability;",
        "\nthe validation model is not respecified from primary-sample results."
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(8, 24, 12, 16)
    )
}
