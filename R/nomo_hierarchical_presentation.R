# Presentation for nomo_hierarchical ------------------------------------------

nomo_hierarchical_structure_label <- function(x) {
  if (identical(x$structure, "higher_order")) "Higher-order" else "Bifactor"
}


#' @export
print.nomo_hierarchical <- function(x, digits = 3, ...) {
  cat("<nomo_hierarchical>\n")
  cat(sprintf(
    "%s model | general factor: %s | group factors: %s\n",
    nomo_hierarchical_structure_label(x),
    x$general,
    paste(names(x$groups), collapse = ", ")
  ))
  cat(sprintf(
    "Estimand: %s\n\n",
    if (identical(x$estimand, "latent_response")) {
      "latent-response composite (ordered indicators)"
    } else {
      "unit-weighted observed composite"
    }
  ))

  idx <- x$indices[, c("index", "estimate")]
  idx$estimate <- round(idx$estimate, digits)
  cat("Total score\n")
  print(idx, n = Inf, width = Inf)

  sub <- x$subscales[, c(
    "subscale", "n_items", "omega_subscale", "omega_hierarchical_subscale"
  )]
  sub$omega_subscale <- round(sub$omega_subscale, digits)
  sub$omega_hierarchical_subscale <- round(sub$omega_hierarchical_subscale, digits)
  cat("\nSubscales\n")
  print(sub, n = Inf, width = Inf)

  flagged <- x$notes[x$notes$severity %in% c("review", "concern"), , drop = FALSE]
  if (nrow(flagged)) {
    cat("\nNotes\n")
    cat(paste0("- [", flagged$severity, "] ", flagged$note), sep = "\n")
  }

  cat("\nNo index is treated as a pass/fail threshold; see nomo_table(x, \"indices\").\n")
  invisible(x)
}


#' @export
summary.nomo_hierarchical <- function(object, ...) {
  out <- list(
    structure = object$structure,
    general = object$general,
    estimand = object$estimand,
    indices = object$indices,
    subscales = object$subscales,
    notes = object$notes
  )
  class(out) <- c("summary_nomo_hierarchical", "list")
  out
}


#' @export
print.summary_nomo_hierarchical <- function(x, ...) {
  cat(sprintf(
    "%s model evaluation (general factor: %s)\n\n",
    if (identical(x$structure, "higher_order")) "Higher-order" else "Bifactor",
    x$general
  ))
  for (i in seq_len(nrow(x$indices))) {
    cat(sprintf("- %s = %.3f: %s\n",
                x$indices$index[[i]], x$indices$estimate[[i]],
                x$indices$interpretation[[i]]))
  }
  cat("\nSubscales\n")
  for (i in seq_len(nrow(x$subscales))) {
    s <- x$subscales[i, ]
    cat(sprintf(
      "- %s (%d items): omega subscale = %.3f, omega hierarchical subscale = %.3f\n",
      s$subscale, s$n_items, s$omega_subscale, s$omega_hierarchical_subscale
    ))
  }
  if (nrow(x$notes)) {
    cat("\nNotes\n")
    cat(paste0("- [", x$notes$severity, "] ", x$notes$note), sep = "\n")
  }
  invisible(x)
}


#' Plot hierarchical measurement evidence
#'
#' @param x A `nomo_hierarchical` object.
#' @param type `"variance"` (default) shows, for the total score and each
#'   subscale composite, how its variance divides among the general factor, the
#'   group factor, and everything else. `"loadings"` shows each item's
#'   standardized general and group loadings.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#' @export
plot.nomo_hierarchical <- function(x, type = c("variance", "loadings"), ...) {
  type <- match.arg(type)

  if (identical(type, "loadings")) {
    dat <- x$loadings
    long <- rbind(
      data.frame(item = dat$item, source = "General", loading = dat$general_loading),
      data.frame(item = dat$item, source = "Group", loading = dat$group_loading)
    )
    long$item <- factor(long$item, levels = rev(dat$item))
    long$source <- factor(long$source, levels = c("General", "Group"))

    return(
      ggplot2::ggplot(long, ggplot2::aes(x = loading, y = item, shape = source)) +
        ggplot2::geom_vline(xintercept = 0, linetype = 2) +
        ggplot2::geom_point(size = 2.6) +
        ggplot2::labs(
          title = "Standardized general and group loadings",
          subtitle = sprintf("%s model, general factor: %s",
                             nomo_hierarchical_structure_label(x), x$general),
          x = "Standardized loading",
          y = NULL,
          shape = "Source"
        ) +
        ggplot2::theme_minimal()
    )
  }

  omega_t <- x$indices$estimate[x$indices$index == "omega_total"]
  omega_h <- x$indices$estimate[x$indices$index == "omega_hierarchical"]
  composites <- c("Total score", x$subscales$subscale)
  general <- c(omega_h, x$subscales$omega_subscale - x$subscales$omega_hierarchical_subscale)
  group <- c(omega_t - omega_h, x$subscales$omega_hierarchical_subscale)
  other <- 1 - general - group

  long <- data.frame(
    composite = rep(composites, 3L),
    source = rep(c("General factor", "Group factor(s)", "Other variance"),
                 each = length(composites)),
    share = c(general, group, other)
  )
  long$composite <- factor(long$composite, levels = rev(composites))
  long$source <- factor(
    long$source,
    levels = c("Other variance", "Group factor(s)", "General factor")
  )

  ggplot2::ggplot(long, ggplot2::aes(x = share, y = composite, fill = source)) +
    ggplot2::geom_col(width = .65, colour = "grey30", linewidth = .2) +
    ggplot2::scale_fill_manual(values = c(
      "General factor" = "grey25",
      "Group factor(s)" = "grey60",
      "Other variance" = "grey92"
    )) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::labs(
      title = "Where each composite's variance comes from",
      subtitle = paste(
        "General = omega hierarchical;",
        "general + group = omega; the remainder is not common-factor variance"
      ),
      x = "Proportion of composite variance",
      y = NULL,
      fill = NULL
    ) +
    ggplot2::theme_minimal()
}


#' @export
nomo_table.nomo_hierarchical <- function(
    x,
    type = c("indices", "subscales", "loadings", "notes", "decision_log"),
    ...) {
  type <- match.arg(type)

  if (type == "indices") return(x$indices)
  if (type == "subscales") return(x$subscales)
  if (type == "loadings") return(x$loadings)
  if (type == "notes") return(x$notes)
  x$decision_log
}
