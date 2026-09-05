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

  tibble::as_tibble(out[, c(
    "construct", "block", "indicator_type",
    "omega", "omega_ci_lower", "omega_ci_upper", "omega_ci_n_success",
    "alpha", "alpha_ci_lower", "alpha_ci_upper", "alpha_ci_n_success",
    "alpha_available", "omega_scale", "alpha_scale", "signal"
  ), drop = FALSE])
}


nomo_reliability_ci_string <- function(est, lo, hi) {
  if (!is.finite(est)) return(NA_character_)
  if (is.finite(lo) && is.finite(hi)) {
    sprintf("%.3f [%.3f, %.3f]", est, lo, hi)
  } else sprintf("%.3f", est)
}


#' @export
print.nomo_reliability <- function(x, ...) {
  tab <- nomo_reliability_summary_table(x)
  cat("<nomo_reliability>\n")
  cat(sprintf(
    "Constructs: %d | Primary coefficient: model-based omega | Review reference: %s\n",
    length(unique(tab$construct)),
    format(x$guidance$reliability_reference, trim = TRUE)
  ))

  if (nrow(tab)) {
    finite_omega <- tab$omega[is.finite(tab$omega)]
    if (length(finite_omega)) {
      cat(sprintf(
        "Omega range: %.3f to %.3f | Construct review signals: %d\n",
        min(finite_omega), max(finite_omega), sum(tab$signal != "info")
      ))
    }
  }

  if (isTRUE(x$include_alpha)) {
    cat(sprintf(
      "Alpha: %d of %d construct(s) available as a secondary coefficient\n",
      sum(x$alpha_status$available), nrow(x$alpha_status)
    ))
  }

  if (!is.null(x$ci_status) && identical(x$ci_status$method[[1L]], "bootstrap")) {
    cat(sprintf(
      "Uncertainty: %.1f%% percentile bootstrap CI | %d requested draws",
      100 * x$ci_status$level[[1L]], x$ci_status$requested_draws[[1L]]
    ))
    if (is.finite(x$ci_status$min_successful_draws[[1L]])) {
      cat(sprintf(" | minimum successful draws: %d",
                  x$ci_status$min_successful_draws[[1L]]))
    }
    cat("\n")
    if (nzchar(x$ci_status$reason[[1L]])) {
      cat("Bootstrap note: ", x$ci_status$reason[[1L]], "\n", sep = "")
    }
  } else {
    cat("Uncertainty: point estimates only; use `ci = \"bootstrap\"` for interval estimates.\n")
  }

  if (isTRUE(x$model_strain) || isTRUE(x$improper_solution)) {
    cat("Measurement-model context: REVIEW before treating reliability as stable evidence.\n")
  }
  cat("Reference values guide review; they are not pass/fail reliability rules.\n")
  invisible(x)
}


#' Summarize model-based reliability evidence
#'
#' @param object A `nomo_reliability` object.
#' @param ... Unused.
#' @return An object of class `summary_nomo_reliability`.
#' @export
summary.nomo_reliability <- function(object, ...) {
  out <- list(
    table = nomo_reliability_summary_table(object),
    alpha_status = object$alpha_status,
    ci_status = object$ci_status,
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
  cat("nomologR reliability evidence\n")
  if (nrow(x$table)) {
    show <- x$table
    display <- tibble::tibble(
      construct = show$construct,
      block = show$block,
      indicator_type = show$indicator_type,
      omega = mapply(
        nomo_reliability_ci_string,
        show$omega, show$omega_ci_lower, show$omega_ci_upper,
        USE.NAMES = FALSE
      ),
      alpha = mapply(
        nomo_reliability_ci_string,
        show$alpha, show$alpha_ci_lower, show$alpha_ci_upper,
        USE.NAMES = FALSE
      ),
      omega_scale = show$omega_scale,
      signal = show$signal
    )
    print(display, row.names = FALSE)
    if (any(is.finite(show$omega_ci_lower) & is.finite(show$omega_ci_upper))) {
      cat("Bracketed values are bootstrap confidence intervals.\n")
    }
  } else {
    cat("No reliability coefficients are available.\n")
  }

  unavailable <- x$alpha_status[
    x$alpha_status$requested & !x$alpha_status$available,
    , drop = FALSE
  ]
  if (nrow(unavailable)) {
    cat("\nSecondary alpha unavailable for:\n")
    print(
      unavailable[, c("construct", "indicator_type", "score_scale", "reason"), drop = FALSE],
      row.names = FALSE, width = Inf
    )
  }

  if (!is.null(x$ci_status) &&
      !identical(x$ci_status$method[[1L]], "bootstrap")) {
    cat(
      "\nSampling uncertainty was not bootstrapped. ",
      "For report-ready intervals, rerun with `ci = \"bootstrap\"`.\n",
      sep = ""
    )
  }

  if (isTRUE(x$model_strain) || isTRUE(x$improper_solution)) {
    cat("\nMeasurement-model context requires review: reliability is conditional on the fitted CFA.\n")
  }

  cat(
    "\nInterpretation rule: omega is primary for the congeneric CFA workflow; ",
    "alpha is secondary and assumption-dependent. Reliability contributes ",
    "score-precision evidence, not construct validity.\n",
    sep = ""
  )
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
#' @param ... Unused.
#' @return A `ggplot2` object. The conceptual 0-to-1 reliability range is shown
#'   by default and expands only if an estimate or interval lies outside it.
#' @export
plot.nomo_reliability <- function(x, ...) {
  dat <- x$evidence
  dat <- dat[is.finite(dat$estimate), , drop = FALSE]
  if (!nrow(dat)) {
    stop("No finite reliability coefficients are available to plot.", call. = FALSE)
  }

  if (!"ci_lower" %in% names(dat)) dat$ci_lower <- NA_real_
  if (!"ci_upper" %in% names(dat)) dat$ci_upper <- NA_real_

  dat$metric <- factor(dat$metric, levels = c("omega", "alpha"))
  levels_y <- rev(unique(as.character(dat$construct)))
  dat$y_base <- match(as.character(dat$construct), levels_y)
  dat$y_plot <- dat$y_base + ifelse(as.character(dat$metric) == "omega", 0.09, -0.09)

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
      ggplot2::aes(x = estimate, y = y_plot, shape = metric),
      size = 3.4
    ) +
    ggplot2::geom_point(
      data = alpha_dat,
      ggplot2::aes(x = estimate, y = y_plot, shape = metric),
      size = 2.5
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(levels_y),
      labels = levels_y
    ) +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::labs(
      title = "Reliability evidence",
      subtitle = if (any(is.finite(dat$ci_lower) & is.finite(dat$ci_upper))) {
        paste0(
          format(100 * x$ci_level, trim = TRUE),
          "% bootstrap CIs; dashed line = review reference (",
          format(ref, trim = TRUE), ")."
        )
      } else {
        paste0(
          "Dashed line = review reference (", format(ref, trim = TRUE),
          "); bootstrap CIs are optional."
        )
      },
      x = "Reliability coefficient",
      y = NULL,
      shape = "Coefficient",
      caption = "Reference values guide review; they are not pass/fail rules."
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line()
    )

  if (length(unique(as.character(dat$metric))) == 1L) {
    p <- p + ggplot2::guides(shape = "none")
  }
  if (length(unique(dat$block)) > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ block"))
  }
  p
}


utils::globalVariables(c(
  "estimate", "construct", "metric", "block", "ci_lower", "ci_upper", "y_plot"
))
