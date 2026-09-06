# Measurement-invariance presentation -----------------------------------------

nomo_invariance_metric_label <- function(x) {
  lookup <- c(
    delta_cfi = "\u0394CFI",
    delta_rmsea = "\u0394RMSEA",
    delta_srmr = "\u0394SRMR"
  )
  out <- unname(lookup[as.character(x)])
  missing <- is.na(out)
  out[missing] <- as.character(x)[missing]
  out
}


nomo_invariance_parameter_label <- function(pt, row, group_labels) {
  lhs <- as.character(pt$lhs[[row]])
  op <- as.character(pt$op[[row]])
  rhs <- as.character(pt$rhs[[row]])

  base <- if (identical(op, "=~")) {
    paste0("Loading: ", lhs, " \u2192 ", rhs)
  } else if (identical(op, "~1")) {
    paste0("Intercept: ", lhs)
  } else if (identical(op, "|")) {
    paste0("Threshold: ", lhs, " | ", rhs)
  } else if (identical(op, "~~") && identical(lhs, rhs)) {
    paste0("Residual variance: ", lhs)
  } else if (identical(op, "~~")) {
    paste0("Covariance: ", lhs, " \u2194 ", rhs)
  } else {
    paste(lhs, op, rhs)
  }

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


nomo_invariance_pretty_constraint <- function(constraint, fit) {
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
    group_labels
  )
  b <- nomo_invariance_parameter_label(
    pt,
    right[[1L]],
    group_labels
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

  dat$constraint_display <- vapply(
    seq_len(nrow(dat)),
    function(i) {
      level <- as.character(dat$level[[i]])
      fit <- x$fits[[level]]
      nomo_invariance_pretty_constraint(
        dat$constraint[[i]],
        fit
      )
    },
    character(1)
  )

  dat
}


#' @export
print.nomo_invariance <- function(x, ...) {
  cat("<nomo_invariance>\n")
  cat(sprintf(
    "Grouping variable: %s (%d groups: %s)\n",
    x$group,
    length(x$groups),
    paste(x$groups, collapse = ", ")
  ))
  cat(sprintf("Indicator treatment: %s\n", x$indicator_type))

  if (length(x$ordered)) {
    cat(sprintf(
      "Ordered identification: %s | parameterization: %s\n",
      x$ID.cat,
      x$parameterization
    ))
  }

  cat(sprintf(
    "Requested: %s\n",
    paste(x$requested_levels, collapse = " -> ")
  ))
  cat(sprintf(
    "Completed: %s\n",
    paste(x$completed_levels, collapse = " -> ")
  ))

  if (!is.null(x$partial) && x$partial$n > 0L) {
    cat(sprintf(
      "Researcher-specified partial releases: %d\n",
      x$partial$n
    ))
  }

  cat("\n")
  print(
    x$fit_evidence[, c(
      "level",
      "status",
      "constraints",
      "partial_requested",
      "cfi",
      "rmsea",
      "srmr",
      "delta_cfi",
      "delta_rmsea",
      "delta_srmr",
      "lrt_p"
    ), drop = FALSE],
    n = Inf,
    width = Inf
  )

  if (nrow(x$local_strain)) {
    cat(sprintf(
      "\nLocalized equality-constraint diagnostics retained: %d\n",
      nrow(x$local_strain)
    ))
  }

  cat(
    "\nInterpretation rule: fit changes and score diagnostics are evidence, ",
    "not universal pass/fail rules, not automatic pass/fail decisions, ",
    "and not automatic parameter-freeing rules.\n",
    sep = ""
  )

  invisible(x)
}


#' @export
summary.nomo_invariance <- function(object, ...) {
  local_display <- nomo_invariance_local_strain_display(object)
  top_local <- if (nrow(local_display)) {
    utils::head(local_display, 10L)
  } else {
    tibble::tibble()
  }

  out <- list(
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
  cat("nomologR measurement-invariance summary\n")
  cat(sprintf("Indicator treatment: %s\n", x$indicator_type))
  cat(sprintf(
    "Groups: %s\n",
    paste(x$groups, collapse = ", ")
  ))
  cat(sprintf(
    "Levels completed: %s\n\n",
    paste(x$completed_levels, collapse = " -> ")
  ))

  cat("Identification/sequence note\n")
  cat(x$identification_note, "\n\n")

  if (nrow(x$ordered_categories)) {
    cat("Observed ordered categories\n")
    print(x$ordered_categories, n = Inf, width = Inf)
    cat("\n")
  }

  cat("Fit and change evidence\n")
  print(x$fit_evidence, n = Inf, width = Inf)

  if (!is.null(x$partial) && x$partial$n > 0L) {
    cat("\nResearcher-specified partial invariance\n")
    print(x$partial$releases, n = Inf, width = Inf)
  }

  if (nrow(x$top_local_strain)) {
    cat("\nLargest equality-constraint score diagnostics - diagnostic only\n")
    cols <- intersect(
      c(
        "level",
        "constraint_display",
        "score_x2",
        "df",
        "p_value",
        "diagnostic_only"
      ),
      names(x$top_local_strain)
    )
    print(
      x$top_local_strain[, cols, drop = FALSE],
      n = Inf,
      width = Inf
    )
  }

  cat("\n", x$note, "\n", sep = "")
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
  type <- match.arg(type)

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
        ggplot2::labs(
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
      levels = c("\u0394CFI", "\u0394RMSEA", "\u0394SRMR")
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
        ggplot2::labs(
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
    ggplot2::labs(
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
