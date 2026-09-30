# Measurement-invariance presentation -----------------------------------------

# Labels use ASCII so figures render on every graphics device. Non-ASCII
# symbols such as the Greek capital delta or arrows are dropped or mangled by
# the default pdf() device, and R CMD check treats that as an error.
nomo_invariance_metric_label <- function(x) {
  lookup <- c(
    delta_cfi = "Delta CFI",
    delta_rmsea = "Delta RMSEA",
    delta_srmr = "Delta SRMR"
  )
  out <- unname(lookup[as.character(x)])
  missing <- is.na(out)
  out[missing] <- as.character(x)[missing]
  out
}


# Across occasions, `occasion_map` translates each occasion's column and factor
# names back to the item or factor in the one-occasion model, and the occasion
# takes the place of the group.
nomo_invariance_parameter_label <- function(pt, row, group_labels, occasion_map = NULL) {
  lhs <- as.character(pt$lhs[[row]])
  op <- as.character(pt$op[[row]])
  rhs <- as.character(pt$rhs[[row]])
  occasion <- NULL
  if (!is.null(occasion_map)) {
    k <- match(if (identical(op, "=~")) rhs else lhs, occasion_map$name)
    occasion <- if (is.na(k)) "" else occasion_map$occasion[[k]]
    generic <- function(name) {
      k <- match(name, occasion_map$name)
      if (is.na(k)) name else occasion_map$generic[[k]]
    }
    lhs <- generic(lhs)
    rhs <- generic(rhs)
  }

  base <- if (identical(op, "=~")) {
    paste0("Loading: ", lhs, " -> ", rhs)
  } else if (identical(op, "~1")) {
    paste0("Intercept: ", lhs)
  } else if (identical(op, "|")) {
    paste0("Threshold: ", lhs, " | ", rhs)
  } else if (identical(op, "~~") && identical(lhs, rhs)) {
    paste0("Residual variance: ", lhs)
  } else if (identical(op, "~~")) {
    paste0("Covariance: ", lhs, " <-> ", rhs)
  } else {
    paste(lhs, op, rhs)
  }
  if (!is.null(occasion)) return(list(base = base, group = occasion))

  group_id <- suppressWarnings(as.integer(pt$group[[row]]))
  group_name <- if (
    is.finite(group_id) &&
      group_id >= 1L &&
      group_id <= length(group_labels)
  ) {
    group_labels[[group_id]]
  } else if (is.finite(group_id) && group_id > 0L) {
    paste0("Group ", group_id)
  } else {
    ""
  }

  list(base = base, group = group_name)
}


nomo_invariance_pretty_constraint <- function(constraint, fit, occasion_map = NULL) {
  if (is.null(fit) ||
      !is.character(constraint) ||
      length(constraint) != 1L ||
      is.na(constraint) ||
      !nzchar(constraint)) {
    return(constraint)
  }

  parts <- strsplit(
    constraint,
    "\\s*==\\s*",
    perl = TRUE
  )[[1L]]

  if (length(parts) != 2L) return(constraint)

  pt <- tryCatch(
    as.data.frame(lavaan::parTable(fit)),
    error = function(e) NULL
  )

  if (is.null(pt) || !nrow(pt)) return(constraint)

  plabel_col <- if ("plabel" %in% names(pt)) {
    "plabel"
  } else if ("label" %in% names(pt)) {
    "label"
  } else {
    NULL
  }

  if (is.null(plabel_col)) return(constraint)

  left <- which(as.character(pt[[plabel_col]]) == trimws(parts[[1L]]))
  right <- which(as.character(pt[[plabel_col]]) == trimws(parts[[2L]]))

  if (!length(left) || !length(right)) return(constraint)

  group_labels <- tryCatch(
    as.character(lavaan::lavInspect(fit, "group.label")),
    error = function(e) character()
  )

  a <- nomo_invariance_parameter_label(
    pt,
    left[[1L]],
    group_labels,
    occasion_map
  )
  b <- nomo_invariance_parameter_label(
    pt,
    right[[1L]],
    group_labels,
    occasion_map
  )

  if (identical(a$base, b$base)) {
    if (nzchar(a$group) || nzchar(b$group)) {
      group_text <- paste(
        c(a$group, b$group)[nzchar(c(a$group, b$group))],
        collapse = " vs. "
      )
      return(paste0(a$base, " (", group_text, ")"))
    }
    return(a$base)
  }

  left_label <- if (nzchar(a$group)) {
    paste0(a$base, " [", a$group, "]")
  } else {
    a$base
  }
  right_label <- if (nzchar(b$group)) {
    paste0(b$base, " [", b$group, "]")
  } else {
    b$base
  }

  paste(left_label, "=", right_label)
}


nomo_invariance_local_strain_display <- function(x) {
  dat <- x$local_strain
  if (!nrow(dat)) return(dat)
  occasion_map <- if (identical(x$design, "occasions")) {
    named <- c(x$long_items, x$long_factors)
    tibble::tibble(
      name = unname(unlist(named)),
      generic = rep(names(named), lengths(named)),
      occasion = rep(x$occasions, length(named))
    )
  }

  dat$constraint_display <- vapply(
    seq_len(nrow(dat)),
    function(i) {
      level <- as.character(dat$level[[i]])
      fit <- x$fits[[level]]
      nomo_invariance_pretty_constraint(
        dat$constraint[[i]],
        fit,
        occasion_map
      )
    },
    character(1)
  )

  dat
}


# Levels that were not estimated, did not converge, or raised warnings, named
# with what went wrong; a table column would only say that something did.
nomo_invariance_present_problems <- function(fit) {
  bad <- fit[
    fit$status != "estimated" | !fit$converged | nzchar(fit$warnings) | nzchar(fit$error),
    , drop = FALSE
  ]
  if (!nrow(bad)) return(invisible(NULL))
  nomo_present_section("Levels to review")
  nomo_present_bullets(vapply(seq_len(nrow(bad)), function(i) {
    parts <- c(
      bad$status[[i]],
      if (isFALSE(bad$converged[[i]])) "did not converge" else "",
      bad$error[[i]],
      if (nzchar(bad$warnings[[i]])) paste("warnings:", bad$warnings[[i]]) else ""
    )
    paste0(bad$level[[i]], ": ", paste(parts[nzchar(parts)], collapse = "; "))
  }, character(1)))
}


#' @export
print.nomo_invariance <- function(x, ...) {
  if (identical(x$design, "occasions")) {
    nomo_present_header("nomo_invariance_longitudinal", "Measurement invariance across occasions")
    across <- sprintf("Occasions: %s", paste(x$occasions, collapse = ", "))
  } else {
    nomo_present_header("nomo_invariance", "Measurement invariance")
    across <- sprintf("Grouping variable: %s (%d groups: %s)", x$group, length(x$groups),
                      paste(x$groups, collapse = ", "))
  }
  nomo_present_facts(c(across, sprintf("Indicators: %s", x$indicator_type)))

  if (length(x$ordered)) {
    nomo_present_facts(c(
      sprintf("Ordered identification: %s", x$ID.cat),
      sprintf("parameterization: %s", x$parameterization)
    ))
  }

  nomo_present_facts(sprintf("Requested: %s", paste(x$requested_levels, collapse = " -> ")))
  nomo_present_facts(sprintf("Completed: %s", paste(x$completed_levels, collapse = " -> ")))

  if (!is.null(x$partial) && x$partial$n > 0L) {
    nomo_present_facts(sprintf("Researcher-specified partial releases: %d", x$partial$n))
  }

  signed <- function(v) nomo_present_signed(v)
  cat("\n")
  nomo_present_table(
    x$fit_evidence,
    c("Level" = "level", "CFI" = "cfi", "RMSEA" = "rmsea", "SRMR" = "srmr",
      "CFI change" = "delta_cfi", "RMSEA change" = "delta_rmsea",
      "LRT p" = "lrt_p"),
    formats = list(delta_cfi = signed, delta_rmsea = signed, lrt_p = nomo_present_p),
    more = "summary(x)"
  )
  nomo_invariance_present_problems(x$fit_evidence)

  if (nrow(x$local_strain)) {
    nomo_present_text(
      sprintf("Localized equality-constraint diagnostics retained: %d", nrow(x$local_strain))
    )
  }

  cat("\n")
  nomo_present_text(
    "Fit changes and score diagnostics are evidence. They are not pass/fail ",
    "rules, and nomologR never frees a parameter because of them."
  )

  invisible(x)
}


#' @export
summary.nomo_invariance <- function(object, ...) {
  local_display <- nomo_invariance_local_strain_display(object)
  # The ten largest across all levels. The diagnostics come in level order, so
  # the first ten had been the metric level's alone, and a scalar-level strain
  # (an intercept) never reached the summary.
  top_local <- if (nrow(local_display)) {
    utils::head(local_display[order(-local_display$score_x2), , drop = FALSE], 10L)
  } else {
    tibble::tibble()
  }

  out <- list(
    design = object$design,
    group = object$group,
    groups = object$groups,
    indicator_type = object$indicator_type,
    identification_note = object$identification_note,
    ordered = object$ordered,
    ordered_categories = object$ordered_categories,
    requested_levels = object$requested_levels,
    completed_levels = object$completed_levels,
    fit_evidence = object$fit_evidence,
    partial = object$partial,
    partial_requested = object$partial_requested,
    top_local_strain = top_local,
    latent_means = object[["latent_means"]],
    decision_log = object$decision_log,
    note = paste(
      "No single delta-CFI, delta-RMSEA, delta-SRMR, chi-square difference,",
      "or score diagnostic is treated as a universal invariance rule."
    )
  )
  class(out) <- c("summary_nomo_invariance", "list")
  out
}


#' @export
print.summary_nomo_invariance <- function(x, ...) {
  across_occasions <- identical(x$design, "occasions")
  if (across_occasions) {
    nomo_present_header(
      "nomo_invariance_longitudinal", "Measurement invariance across occasions",
      summary = TRUE
    )
  } else {
    nomo_present_header("nomo_invariance", "Measurement invariance", summary = TRUE)
  }
  nomo_present_facts(c(
    sprintf("Indicators: %s", x$indicator_type),
    sprintf(if (across_occasions) "Occasions: %s" else "Groups: %s",
            paste(x$groups, collapse = ", "))
  ))
  nomo_present_facts(sprintf(
    "Levels completed: %s", paste(x$completed_levels, collapse = " -> ")
  ))

  nomo_present_section("Identification and sequence")
  nomo_present_text(x$identification_note, indent = 2L)

  if (nrow(x$ordered_categories)) {
    nomo_present_section("Observed ordered categories")
    cats <- x$ordered_categories
    nomo_present_table(cats, stats::setNames(names(cats), gsub("_", " ", names(cats))),
                       more = "nomo_table(x, \"categories\")")
  }

  fit <- x$fit_evidence
  nomo_present_section("Fit by level")
  nomo_present_table(
    fit,
    c("Level" = "level", "Constraints" = "constraints", "Chi-square" = "chisq",
      "df" = "df", "p" = "pvalue", "CFI" = "cfi", "RMSEA" = "rmsea", "SRMR" = "srmr"),
    formats = list(chisq = function(v) nomo_present_number(v, 2L),
                   df = function(v) format(v, trim = TRUE), pvalue = nomo_present_p),
    more = "nomo_table(x, \"fit\")"
  )
  signed <- function(v) nomo_present_signed(v)
  nomo_present_section("Changes from the preceding level")
  nomo_present_table(
    fit[-1L, , drop = FALSE],
    c("Level" = "level", "CFI change" = "delta_cfi", "RMSEA change" = "delta_rmsea",
      "SRMR change" = "delta_srmr", "LRT chi-square" = "lrt_chisq", "df" = "lrt_df",
      "p" = "lrt_p"),
    formats = list(delta_cfi = signed, delta_rmsea = signed, delta_srmr = signed,
                   lrt_chisq = function(v) nomo_present_number(v, 2L),
                   lrt_df = function(v) format(v, trim = TRUE), lrt_p = nomo_present_p),
    more = "nomo_table(x, \"fit\")"
  )
  nomo_invariance_present_problems(fit)

  if (!is.null(x$partial) && x$partial$n > 0L) {
    nomo_present_section("Researcher-specified partial invariance")
    rel <- x$partial$releases
    nomo_present_bullets(sprintf(
      "%s (%s): %s. %s", rel$release_id, rel$level, rel$syntax, rel$rationale
    ))
  }

  means <- x[["latent_means"]]
  if (is.data.frame(means) && nrow(means)) {
    shown <- means
    shown$interval <- nomo_present_ci(shown$ci_lower, shown$ci_upper, 2L)
    nomo_present_section(if (across_occasions) {
      sprintf("Latent change from %s (its latent SD)", shown$reference_occasion[[1L]])
    } else {
      sprintf("Latent means relative to %s (its latent SD)", shown$reference_group[[1L]])
    })
    nomo_present_table(
      shown,
      c("Level" = "level", "Group" = "group", "Occasion" = "occasion", "Factor" = "factor",
        "Difference" = "estimate", "95% CI" = "interval", "p" = "p_value"),
      formats = list(estimate = function(v) nomo_present_number(v, 2L),
                     p_value = nomo_present_p),
      more = "nomo_table(x, \"latent_means\")"
    )
    nomo_present_text(
      "Comparable only with invariant intercepts, full or partial.", indent = 2L
    )
  }

  if (nrow(x$top_local_strain)) {
    nomo_present_section("Largest equality-constraint score diagnostics (diagnostic only)")
    nomo_present_table(
      x$top_local_strain,
      c("Level" = "level", "Constraint" = "constraint_display",
        "Score" = "score_x2", "df" = "df", "p" = "p_value"),
      formats = list(score_x2 = function(v) nomo_present_number(v, 2L),
                     df = function(v) format(v, trim = TRUE), p_value = nomo_present_p),
      more = "nomo_table(x, \"local_strain\")"
    )
  }

  cat("\n")
  nomo_present_text(x$note)
  invisible(x)
}


#' Plot measurement-invariance evidence
#'
#' @param x A `nomo_invariance` object.
#' @param type Plot type: `"fit"`, `"change"`, or `"local_strain"`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object.
#' @export
plot.nomo_invariance <- function(
    x,
    type = c("fit", "change", "local_strain"),
    ...) {
  type <- nomo_match_arg(type)

  if (type == "fit") {
    pieces <- lapply(
      c("cfi", "rmsea", "srmr"),
      function(metric) {
        data.frame(
          level = x$fit_evidence$level,
          metric = toupper(metric),
          value = x$fit_evidence[[metric]]
        )
      }
    )
    dat <- do.call(rbind, pieces)
    dat <- dat[is.finite(dat$value), , drop = FALSE]

    if (!nrow(dat)) {
      stop("No finite invariance fit evidence is available.", call. = FALSE)
    }

    dat$level <- factor(dat$level, levels = x$completed_levels)

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(
          x = level,
          y = value,
          group = 1
        )
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::facet_wrap(
          stats::as.formula("~ metric"),
          scales = "free_y"
        ) +
        nomo_plot_labs(
          title = "Measurement-invariance fit across levels",
          subtitle = paste(
            "Absolute fit should be read alongside change evidence",
            "and localized strain."
          ),
          x = "Invariance level",
          y = "Fit index",
          caption = "No single fit index determines invariance."
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (type == "change") {
    pieces <- lapply(
      c("delta_cfi", "delta_rmsea", "delta_srmr"),
      function(metric) {
        data.frame(
          level = x$fit_evidence$level,
          metric = nomo_invariance_metric_label(metric),
          value = x$fit_evidence[[metric]]
        )
      }
    )
    dat <- do.call(rbind, pieces)
    dat <- dat[is.finite(dat$value), , drop = FALSE]

    if (!nrow(dat)) {
      stop(
        "No finite change-in-fit evidence is available; at least two completed levels are needed.",
        call. = FALSE
      )
    }

    dat$metric <- factor(
      dat$metric,
      levels = c("Delta CFI", "Delta RMSEA", "Delta SRMR")
    )
    dat$level <- factor(dat$level, levels = x$completed_levels)

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(
          x = level,
          y = value,
          group = 1
        )
      ) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::geom_line() +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::facet_wrap(
          stats::as.formula("~ metric"),
          scales = "free_y"
        ) +
        nomo_plot_labs(
          title = "Change in fit as equality constraints accumulate",
          subtitle = "Zero means no change from the preceding fitted level.",
          x = "Invariance level",
          y = "Change in fit index",
          caption = paste(
            "For CFI, decreases indicate worsening fit; for RMSEA/SRMR,",
            "increases indicate worsening fit.",
            "\nInterpret magnitude contextually; no universal cutoff is imposed."
          )
        ) +
        ggplot2::theme_minimal()
    )
  }

  dat <- nomo_invariance_local_strain_display(x)
  dat <- dat[is.finite(dat$score_x2), , drop = FALSE]

  if (!nrow(dat)) {
    stop(
      "No equality-constraint score diagnostics are available to plot.",
      call. = FALSE
    )
  }

  dat <- utils::head(
    dat[order(dat$score_x2, decreasing = TRUE), , drop = FALSE],
    20L
  )
  dat$constraint_display <- factor(
    dat$constraint_display,
    levels = rev(unique(dat$constraint_display))
  )

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = score_x2,
      y = constraint_display,
      shape = level
    )
  ) +
    ggplot2::geom_point(size = 2.8) +
    nomo_plot_labs(
      title = "Largest equality-constraint score diagnostics",
      subtitle = paste(
        "Higher score statistics localize strain in fitted equality constraints.",
        "\nThey do not authorize automatic partial invariance."
      ),
      x = "Score-test chi-square",
      y = NULL,
      shape = "Level",
      caption = paste(
        "Any constraint release must be explicitly researcher specified",
        "\nand documented with a rationale."
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.margin = ggplot2::margin(8, 16, 8, 8)
    )
}
