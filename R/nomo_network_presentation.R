# Nomological-network presentation --------------------------------------------

nomo_network_pretty_status <- function(x) {
  lookup <- c(
    concordant = "Concordant",
    directionally_concordant_imprecise = "Directionally concordant / imprecise",
    direction_concordant_below_magnitude = "Direction concordant / below magnitude",
    direction_concordant_above_magnitude = "Direction concordant / above magnitude",
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
    direction_not_replicated = "Direction not replicated",
    sign_change_within_uncertainty = "Sign change within uncertainty",
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
    "Direction concordant / above magnitude",
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


# Shared by print() and summary(): the hypothesis evidence, and replication.
nomo_network_present_evidence <- function(evidence, replication) {
  show <- evidence
  show$interval <- nomo_present_ci(show$ci_lower, show$ci_upper)
  show$concordance_shown <- nomo_network_pretty_status(show$concordance)
  nomo_present_section("Hypothesis evidence")
  nomo_present_table(
    show,
    c("ID" = "id", "Relation" = "relation", "Estimate" = "estimate",
      "95% CI" = "interval", "Concordance" = "concordance_shown"),
    more = "nomo_table(x, \"hypotheses\")"
  )

  if (nrow(replication)) {
    rep <- replication
    rep$status_shown <- nomo_network_pretty_status(rep$replication_status)
    nomo_present_section("Replication evidence")
    nomo_present_table(
      rep,
      c("ID" = "id", "Relation" = "relation", "Primary" = "primary_estimate",
        "Validation" = "validation_estimate", "Replication" = "status_shown"),
      more = "nomo_table(x, \"replication\")"
    )
  }
}



# Single indicators, for print() and summary(): the reliability behind each
# composite modeled as a single indicator, and how the evidence moves with it.
nomo_network_present_single <- function(single, sensitivity, detail = FALSE) {
  if (!is.data.frame(single) || !nrow(single)) return(invisible(NULL))
  if (!isTRUE(detail)) {
    nomo_present_facts(paste0(
      "Single indicator: ", single$variable, " (reliability ",
      nomo_present_number(single$reliability, 3L), ", ", single$coefficient, ")"
    ))
    return(invisible(NULL))
  }

  # Numbers stay numeric, so they are right-aligned like the variances; a
  # missing standard error is shown as "-", and the column is left out when
  # no composite has one.
  nomo_present_section("Single indicators")
  nomo_present_table(
    single,
    c("Composite" = "variable", "Coefficient" = "coefficient",
      "Reliability" = "reliability", "SE" = "se", "Variance" = "variance",
      "Error variance" = "error_variance"),
    more = "nomo_table(x, \"single_indicators\")"
  )

  wide <- nomo_network_sensitivity_wide(sensitivity)
  nomo_present_section("Sensitivity to the reliability")
  nomo_present_table(
    wide,
    c("Composite" = "variable", "ID" = "id", "-.10" = "minus_10",
      "-.05" = "minus_05", "Given" = "given", "+.05" = "plus_05",
      "+.10" = "plus_10", "Concordance" = "concordance"),
    more = "nomo_table(x, \"sensitivity\")"
  )
  nomo_present_text(
    "Estimates with each composite's reliability shifted by the amount shown, ",
    "one composite at a time.",
    indent = 2L
  )
}


# The sensitivity table with one row per composite and hypothesis, and the
# estimate at each shift of the reliability in its own column.
nomo_network_sensitivity_wide <- function(sensitivity) {
  keys <- unique(as.data.frame(sensitivity)[c("variable", "id")])
  columns <- c(minus_10 = -0.10, minus_05 = -0.05, given = 0, plus_05 = 0.05,
               plus_10 = 0.10)
  for (column in names(columns)) {
    keys[[column]] <- vapply(seq_len(nrow(keys)), function(i) {
      hit <- sensitivity$variable == keys$variable[[i]] &
        sensitivity$id == keys$id[[i]] &
        abs(sensitivity$shift - columns[[column]]) < 1e-9
      if (any(hit)) sensitivity$estimate[hit][[1L]] else NA_real_
    }, numeric(1))
  }
  keys$concordance <- vapply(seq_len(nrow(keys)), function(i) {
    seen <- sensitivity$concordance[
      sensitivity$variable == keys$variable[[i]] & sensitivity$id == keys$id[[i]]
    ]
    if (length(unique(seen)) > 1L) "changes" else "unchanged"
  }, character(1))
  keys
}

# Shared facts: sample, convergence, and the relations added from hypotheses.
nomo_network_present_facts <- function(x) {
  nomo_present_facts(c(
    sprintf("%s sample: N = %d", tools::toTitleCase(x$sample_role), x$data_n),
    if (!is.na(x$validation_n)) sprintf("Validation sample: N = %d", x$validation_n) else "",
    paste0("Converged: ", if (isTRUE(x$converged)) "yes" else "NO")
  ))
}


#' @export
print.nomo_network <- function(x, ...) {
  nomo_present_header("nomo_network", "Nomological network")
  nomo_network_present_facts(x)
  nomo_present_facts(c(
    sprintf("Theory relations: %d", x$hypotheses$n),
    sprintf("Added to the model from hypotheses: %d",
            sum(x$model_relations$added_from_hypothesis))
  ))
  nomo_network_present_single(
    x[["single_indicators"]], x[["single_indicator_sensitivity"]]
  )

  measurement <- x$measurement_context$summary[1L, , drop = FALSE]
  flag <- nomo_present_flag(measurement$attention[[1L]])
  nomo_present_text(
    "Measurement context", if (nzchar(flag)) paste0(" (", flag, ")") else "", ": ",
    measurement$observation[[1L]]
  )

  nomo_network_present_evidence(x$hypothesis_evidence, x$replication_evidence)

  cat("\n")
  nomo_present_text(
    "Theory concordance, uncertainty, measurement quality, and replication are ",
    "distinct evidence streams. Statistical significance alone is not a ",
    "validity verdict."
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
    single_indicators = object[["single_indicators"]],
    single_indicator_sensitivity = object[["single_indicator_sensitivity"]],
    decision_log = object$decision_log
  )

  class(out) <- c("summary_nomo_network", "list")
  out
}


#' @export
print.summary_nomo_network <- function(x, ...) {
  nomo_present_header("nomo_network", "Nomological network", summary = TRUE)
  nomo_network_present_facts(x)

  nomo_present_section("Measurement context")
  mc <- x$measurement_context$summary[1L, , drop = FALSE]
  flag <- nomo_present_flag(mc$attention[[1L]])
  nomo_present_text(paste(c(
    paste0("Flag: ", if (nzchar(flag)) flag else "none"),
    sprintf("Constructs: %s", format(mc$latent_constructs, trim = TRUE)),
    sprintf("Loading flags: %s", format(mc$loading_review_flags, trim = TRUE)),
    sprintf("Negative variances: %s", format(mc$negative_variance_flags, trim = TRUE)),
    sprintf("Global-fit flags: %s", format(mc$global_fit_review_flags, trim = TRUE)),
    sprintf("Engine warnings: %s", format(mc$engine_warning_count, trim = TRUE))
  ), collapse = " | "), indent = 2L)
  nomo_present_text(mc$observation, indent = 2L)

  nomo_present_section("Model fit")
  fit <- x$fit_evidence[1L, , drop = FALSE]
  if (nrow(fit) && all(c("chisq", "df") %in% names(fit)) && is.finite(fit$chisq)) {
    p <- nomo_present_p_clause(fit$pvalue)
    nomo_present_text(
      sprintf("chi-square(%s) = %s%s", format(fit$df, trim = TRUE),
              nomo_present_number(fit$chisq, 2L), if (nzchar(p)) paste0(", ", p) else ""),
      indent = 2L
    )
  }
  indices <- intersect(c("cfi", "tli", "rmsea", "srmr"), names(fit))
  shown <- indices[vapply(indices, function(nm) is.finite(fit[[nm]]), logical(1))]
  if (length(shown)) {
    nomo_present_text(paste(
      toupper(shown), nomo_present_number(unlist(fit[shown])), collapse = " | "
    ), indent = 2L)
  }

  nomo_network_present_evidence(x$hypothesis_evidence, x$replication_evidence)
  nomo_network_present_single(
    x[["single_indicators"]], x[["single_indicator_sensitivity"]], detail = TRUE
  )

  nomo_present_section("Predictions and context")
  context <- x$hypothesis_evidence
  context$scope_shown <- gsub("_", " ", context$evidence_scope)
  context$measurement_shown <- nomo_present_flag(context$measurement_attention)
  context$status_shown <- nomo_network_pretty_status(context$confirmatory_status)
  nomo_present_table(
    context,
    c("ID" = "id", "Prediction" = "prediction", "Region" = "theoretical_region",
      "Evidence scope" = "scope_shown", "Measurement" = "measurement_shown",
      "Status" = "status_shown"),
    more = "nomo_table(x, \"hypotheses\")"
  )

  counts <- x$concordance_counts
  nomo_present_section("Concordance")
  nomo_present_text(paste(
    nomo_network_pretty_status(counts$concordance), counts$n, collapse = " | "
  ), indent = 2L)

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
  type <- nomo_match_arg(type)

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
        nomo_plot_labs(
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
        nomo_plot_labs(
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
        nomo_plot_labs(
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
    nomo_plot_labs(
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
