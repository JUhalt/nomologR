# Presentation methods for reliability evidence -------------------------------

nomo_reliability_summary_table <- function(x) {
  constructs <- x$item_type_context[, c("construct", "indicator_type"), drop = FALSE]
  if (!nrow(constructs)) return(tibble::tibble())

  omega <- x$evidence[
    x$evidence$metric == "omega",
    intersect(
      c("construct", "block", "estimate", "attention", "ci_lower", "ci_upper", "ci_n_success"),
      names(x$evidence)
    ),
    drop = FALSE
  ]
  names(omega)[names(omega) == "estimate"] <- "omega"
  names(omega)[names(omega) == "attention"] <- "omega_attention"
  names(omega)[names(omega) == "ci_lower"] <- "omega_ci_lower"
  names(omega)[names(omega) == "ci_upper"] <- "omega_ci_upper"
  names(omega)[names(omega) == "ci_n_success"] <- "omega_ci_n_success"

  alpha <- x$evidence[
    x$evidence$metric == "alpha",
    intersect(
      c("construct", "block", "estimate", "ci_lower", "ci_upper", "ci_n_success"),
      names(x$evidence)
    ),
    drop = FALSE
  ]
  names(alpha)[names(alpha) == "estimate"] <- "alpha"
  names(alpha)[names(alpha) == "ci_lower"] <- "alpha_ci_lower"
  names(alpha)[names(alpha) == "ci_upper"] <- "alpha_ci_upper"
  names(alpha)[names(alpha) == "ci_n_success"] <- "alpha_ci_n_success"

  if (nrow(omega)) {
    out <- merge(constructs, omega, by = "construct", all.x = TRUE, sort = FALSE)
  } else {
    out <- constructs
    out$block <- "overall"
    out$omega <- NA_real_
    out$omega_attention <- "unavailable"
  }

  if (nrow(alpha)) {
    out <- merge(out, alpha, by = c("construct", "block"), all.x = TRUE, sort = FALSE)
  } else {
    out$alpha <- NA_real_
  }

  for (nm in c(
    "omega_ci_lower", "omega_ci_upper", "alpha_ci_lower", "alpha_ci_upper"
  )) if (!nm %in% names(out)) out[[nm]] <- NA_real_
  for (nm in c("omega_ci_n_success", "alpha_ci_n_success")) {
    if (!nm %in% names(out)) out[[nm]] <- NA_integer_
  }

  status <- x$alpha_status[, c("construct", "available", "score_scale"), drop = FALSE]
  names(status)[names(status) == "available"] <- "alpha_available"
  names(status)[names(status) == "score_scale"] <- "alpha_scale"
  out <- merge(out, status, by = "construct", all.x = TRUE, sort = FALSE)

  out$omega_scale <- ifelse(
    out$indicator_type == "ordered",
    if (isTRUE(x$ordinal_scale)) "observed_ordinal" else "latent_response",
    "observed_continuous"
  )

  omega_attention <- tolower(ifelse(
    is.na(out$omega_attention), "unavailable", out$omega_attention
  ))
  out$signal <- ifelse(
    omega_attention == "concern", "concern",
    ifelse(omega_attention == "review", "review", "info")
  )

  # In the order the model defines the constructs, as `omega` lists them;
  # merge() does not keep it (#145).
  out <- out[order(
    match(out$construct, constructs$construct),
    match(out$block, unique(out$block))
  ), , drop = FALSE]

  tibble::as_tibble(out[, c(
    "construct", "block", "indicator_type",
    "omega", "omega_ci_lower", "omega_ci_upper", "omega_ci_n_success",
    "alpha", "alpha_ci_lower", "alpha_ci_upper", "alpha_ci_n_success",
    "alpha_available", "omega_scale", "alpha_scale", "signal"
  ), drop = FALSE])
}


# A bootstrap interval's heading: "95% CI", the level from the object.
nomo_reliability_ci_label <- function(ci_status) {
  level <- if (is.data.frame(ci_status) && nrow(ci_status)) ci_status$level[[1L]] else 0.95
  nomo_present_ci_label(level)
}


# A construct as a flagged unit, with its group or level when there are several.
nomo_reliability_unit <- function(construct, block) {
  ifelse(block == "overall", construct, paste0(construct, " in ", block))
}


# One sentence per flagged coefficient: "Omega .68 is below the review
# reference .70."
nomo_reliability_flag_text <- function(metric, estimate, reference) {
  label <- ifelse(metric == "alpha", "Alpha", "Omega")
  shown <- nomo_present_stat(estimate, "reliability", reference = reference)
  ifelse(
    !is.finite(estimate), paste(label, "could not be computed."),
    ifelse(
      estimate < 0 | estimate > 1,
      sprintf("%s %s is outside the 0 to 1 range of a reliability.", label, shown),
      sprintf("%s %s is below the review reference %s.", label, shown,
              nomo_present_stat(reference, "reliability"))
    )
  )
}


#' @export
print.nomo_reliability <- function(x, ...) {
  tab <- nomo_reliability_summary_table(x)
  reference <- x$guidance$reliability_reference
  nomo_present_header("nomo_reliability", "Reliability")
  nomo_present_facts(c(
    sprintf("Constructs: %d", length(unique(tab$construct))),
    "Primary coefficient: model-based omega",
    paste0("Review reference: ", nomo_present_stat(reference, "reliability"))
  ))

  finite_omega <- tab$omega[is.finite(tab$omega)]
  if (length(finite_omega)) {
    range <- nomo_present_stat(range(finite_omega), "reliability")
    nomo_present_facts(c(
      paste0("Omega: ", if (identical(range[[1L]], range[[2L]])) {
        range[[1L]]
      } else {
        paste(range, collapse = " to ")
      }),
      paste0("Omega flags: ", nomo_present_flag_counts(tab$signal))
    ))
  }

  if (isTRUE(x$include_alpha)) {
    nomo_present_facts(sprintf(
      "Alpha: %d of %s, as a secondary coefficient",
      sum(x$alpha_status$available), nomo_present_count(nrow(x$alpha_status), "construct")
    ))
  }

  if (!is.null(x$ci_status) && identical(x$ci_status$method[[1L]], "bootstrap")) {
    # Absent from status tables created before worker counts were recorded,
    # such as a saved object from an earlier version.
    workers <- if ("workers" %in% names(x$ci_status)) x$ci_status$workers else NULL
    used <- x$ci_status$min_successful_draws[[1L]]
    nomo_present_facts(c(
      paste0("Uncertainty: ", nomo_reliability_ci_label(x$ci_status),
             ", percentile bootstrap"),
      sprintf("Draws: %d requested", x$ci_status$requested_draws[[1L]]),
      if (is.finite(used)) sprintf("Fewest usable draws: %d", used) else "",
      if (length(workers) && is.finite(workers[[1L]]) && workers[[1L]] > 1L) {
        sprintf("Workers: %d", workers[[1L]])
      } else {
        ""
      }
    ))
    if (nzchar(x$ci_status$reason[[1L]])) {
      nomo_present_text("Bootstrap note: ", x$ci_status$reason[[1L]])
    }
  } else {
    nomo_present_text(
      "Uncertainty: point estimates only; use `ci = \"bootstrap\"` for interval estimates."
    )
  }

  if (isTRUE(x$model_strain) || isTRUE(x$improper_solution)) {
    nomo_present_text(
      "Measurement-model context: review the CFA before treating reliability as stable evidence."
    )
  }

  # Omega's flags, as counted above; alpha's are in summary().
  flagged <- tab[tab$signal %in% c("review", "concern"), , drop = FALSE]
  nomo_present_flagged(
    unit = nomo_reliability_unit(flagged$construct, flagged$block), status = flagged$signal,
    text = nomo_reliability_flag_text(rep("omega", nrow(flagged)), flagged$omega, reference)
  )

  cat("\n")
  nomo_present_text("Reference values guide review; they are not pass/fail reliability rules.")
  nomo_present_pointer("summary(x)", "each construct's omega and alpha")
  invisible(x)
}


#' Summarize model-based reliability evidence
#'
#' @param object A `nomo_reliability` object.
#' @param ... Unused.
#' @return An object of class `summary_nomo_reliability`. Printed, it shows each
#'   construct's omega and alpha with their intervals, score scale, and flag,
#'   why any alpha was not computed, and the reason for each flag.
#' @export
summary.nomo_reliability <- function(object, ...) {
  out <- list(
    table = nomo_reliability_summary_table(object),
    alpha_status = object$alpha_status,
    ci_status = object$ci_status,
    reference = object$guidance$reliability_reference,
    evidence = object$evidence,
    model_strain = object$model_strain,
    improper_solution = object$improper_solution,
    fit_context = object$fit_context,
    references = object$references,
    decision_log = object$decision_log
  )
  class(out) <- c("summary_nomo_reliability", "list")
  out
}


#' @export
print.summary_nomo_reliability <- function(x, ...) {
  reference <- x$reference
  nomo_present_header("nomo_reliability", "Reliability", summary = TRUE)
  nomo_present_facts(c(
    sprintf("Constructs: %d", length(unique(x$table$construct))),
    paste0("Review reference: ", nomo_present_stat(reference, "reliability"))
  ))
  nomo_present_section("Coefficients")
  ci_label <- nomo_reliability_ci_label(x$ci_status)
  if (nrow(x$table)) {
    show <- x$table
    coefficient <- function(v) nomo_present_stat(v, "reliability", reference = reference)
    show$omega_ci <- nomo_present_ci(show$omega_ci_lower, show$omega_ci_upper,
                                     kind = "reliability")
    show$alpha_ci <- nomo_present_ci(show$alpha_ci_lower, show$alpha_ci_upper,
                                     kind = "reliability")
    show$scale_shown <- gsub("_", " ", show$omega_scale)
    show$flag <- nomo_present_status(show$signal)
    columns <- c("Construct" = "construct", "Block" = "block",
                 "Indicators" = "indicator_type", "Omega" = "omega", "CI" = "omega_ci",
                 "Alpha" = "alpha", "CI" = "alpha_ci", "Omega scale" = "scale_shown",
                 "Flag" = "flag")
    names(columns)[names(columns) == "CI"] <- ci_label
    # A construct without omega, such as a single-indicator factor, has no block.
    columns <- nomo_present_drop_constant(columns, "Block", stats::na.omit(show$block))
    # With continuous indicators the scale is always the observed score's, as
    # the Indicators column already says; ordered indicators keep it.
    continuous <- all(show$omega_scale == "observed_continuous")
    columns <- columns[!(names(columns) == "Omega scale" & continuous)]
    nomo_present_table(
      show, columns,
      formats = list(omega = coefficient, alpha = coefficient),
      more = "nomo_table(x, \"coefficients\")"
    )
  } else {
    nomo_present_text("No reliability coefficients are available.", indent = 2L)
  }

  unavailable <- x$alpha_status[
    x$alpha_status$requested & !x$alpha_status$available,
    , drop = FALSE
  ]
  if (nrow(unavailable)) {
    nomo_present_section("Alpha not computed")
    nomo_present_bullets(sprintf(
      "%s (%s indicators, %s scale): %s", unavailable$construct,
      unavailable$indicator_type, gsub("_", " ", unavailable$score_scale),
      unavailable$reason
    ))
  }

  # Each flagged coefficient, with what to look at; omega's and alpha's.
  evidence <- x$evidence
  if (is.data.frame(evidence) && nrow(evidence)) {
    log <- x$decision_log
    advice <- log$recommendation[match(
      paste(evidence$construct, evidence$metric),
      paste(log$object, log$metric)
    )]
    nomo_present_flagged(
      unit = nomo_reliability_unit(evidence$construct, evidence$block),
      status = evidence$attention,
      text = paste(
        nomo_reliability_flag_text(evidence$metric, evidence$estimate, reference),
        ifelse(is.na(advice), "", advice)
      )
    )
  }

  if (any(is.finite(x$table$omega_ci_lower) | is.finite(x$table$alpha_ci_lower))) {
    nomo_present_key(stats::setNames(
      "Percentile bootstrap confidence interval",
      sub("^[0-9.]+% ", "", ci_label)
    ))
  }

  cat("\n")
  if (!is.null(x$ci_status) &&
      !identical(x$ci_status$method[[1L]], "bootstrap")) {
    nomo_present_text(
      "Sampling uncertainty was not bootstrapped. For report-ready intervals, ",
      "rerun with `ci = \"bootstrap\"`."
    )
  }

  if (isTRUE(x$model_strain) || isTRUE(x$improper_solution)) {
    nomo_present_text(
      "Measurement-model context requires review: reliability is conditional ",
      "on the fitted CFA."
    )
  }

  nomo_present_text(
    "Omega is primary for the congeneric CFA workflow; alpha is secondary and ",
    "assumption-dependent. Reliability contributes score-precision evidence, ",
    "not construct validity."
  )
  nomo_present_pointer("x$decision_log", "the reasoning behind each coefficient")
  invisible(x)
}


nomo_plot_x_limits <- function(values, conceptual = c(0, 1), step = 0.05) {
  values <- values[is.finite(values)]
  lo <- conceptual[[1L]]
  hi <- conceptual[[2L]]
  if (length(values)) {
    lo <- min(lo, values)
    hi <- max(hi, values)
  }
  lo <- floor(lo / step) * step
  hi <- ceiling(hi / step) * step
  c(lo, hi)
}


#' Plot model-based reliability evidence
#'
#' @param x A `nomo_reliability` object.
#' @param type Plot type. `"coefficients"`, the only one, shows omega and alpha
#'   for each construct.
#' @param ... Unused.
#' @return A `ggplot2` object with one row per construct and coefficient, omega
#'   drawn larger than alpha. Each point's shape and color show its flag, with
#'   a legend whenever a coefficient is flagged. The conceptual 0-to-1
#'   reliability range is shown by default and expands only if an estimate or
#'   interval lies outside it.
#' @export
plot.nomo_reliability <- function(x, type = "coefficients", ...) {
  # Every plot method takes `type`, as the shared style asks (#144).
  type <- nomo_match_arg(type, "coefficients")
  dat <- x$evidence
  dat <- dat[is.finite(dat$estimate), , drop = FALSE]
  if (!nrow(dat)) {
    stop("No finite reliability coefficients are available to plot.", call. = FALSE)
  }

  if (!"ci_lower" %in% names(dat)) dat$ci_lower <- NA_real_
  if (!"ci_upper" %in% names(dat)) dat$ci_upper <- NA_real_

  # One row per construct and coefficient, named on the axis, so the shape is
  # free to carry the flag (#144).
  dat$metric <- factor(dat$metric, levels = c("omega", "alpha"))
  constructs <- unique(as.character(dat$construct))
  rows <- as.vector(t(outer(constructs, c("omega", "alpha"), paste, sep = ": ")))
  rows <- rows[rows %in% paste(dat$construct, dat$metric, sep = ": ")]
  levels_y <- rev(rows)
  dat$y_plot <- match(paste(dat$construct, dat$metric, sep = ": "), levels_y)
  dat$flag <- nomo_plot_status(dat$attention)

  ref <- x$guidance$reliability_reference
  xlim <- nomo_plot_x_limits(c(dat$estimate, dat$ci_lower, dat$ci_upper))

  omega_dat <- dat[as.character(dat$metric) == "omega", , drop = FALSE]
  alpha_dat <- dat[as.character(dat$metric) == "alpha", , drop = FALSE]

  omega_ci <- omega_dat[
    is.finite(omega_dat$ci_lower) & is.finite(omega_dat$ci_upper),
    ,
    drop = FALSE
  ]
  alpha_ci <- alpha_dat[
    is.finite(alpha_dat$ci_lower) & is.finite(alpha_dat$ci_upper),
    ,
    drop = FALSE
  ]
  reference_shown <- nomo_present_stat(ref, "reliability")

  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(xintercept = ref, linetype = 2) +
    ggplot2::geom_segment(
      data = omega_ci,
      ggplot2::aes(x = ci_lower, xend = ci_upper, y = y_plot, yend = y_plot),
      linewidth = 0.9
    ) +
    ggplot2::geom_segment(
      data = alpha_ci,
      ggplot2::aes(x = ci_lower, xend = ci_upper, y = y_plot, yend = y_plot),
      linewidth = 0.55
    ) +
    ggplot2::geom_point(
      data = omega_dat,
      ggplot2::aes(x = estimate, y = y_plot, shape = flag, colour = flag),
      size = 3.4
    ) +
    ggplot2::geom_point(
      data = alpha_dat,
      ggplot2::aes(x = estimate, y = y_plot, shape = flag, colour = flag),
      size = 2.5
    ) +
    nomo_plot_status_scales(dat$flag) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(levels_y),
      labels = levels_y
    ) +
    ggplot2::scale_x_continuous(labels = nomo_plot_bounded_labels) +
    ggplot2::coord_cartesian(xlim = xlim) +
    nomo_plot_labs(
      title = "Reliability evidence",
      subtitle = if (any(is.finite(dat$ci_lower) & is.finite(dat$ci_upper))) {
        paste0(
          nomo_reliability_ci_label(x$ci_status),
          " (percentile bootstrap); dashed line = review reference (",
          reference_shown, ")."
        )
      } else {
        paste0(
          "Dashed line = review reference (", reference_shown,
          "); bootstrap CIs are optional."
        )
      },
      x = "Reliability coefficient",
      y = NULL,
      caption = "Omega is primary and drawn larger; alpha is secondary. Reference values guide review; they are not pass/fail rules."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line()
    )

  if (length(unique(dat$block)) > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ block"))
  }
  p
}


utils::globalVariables(c(
  "estimate", "construct", "metric", "block", "ci_lower", "ci_upper", "y_plot", "flag"
))
