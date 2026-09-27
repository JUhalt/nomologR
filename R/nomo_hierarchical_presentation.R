# Presentation for nomo_hierarchical ------------------------------------------

nomo_hierarchical_structure_label <- function(x) {
  if (identical(x$structure, "higher_order")) "Higher-order" else "Bifactor"
}


# Index names as readers know them: "omega hierarchical", "ECV", "PUC".
nomo_hierarchical_index_label <- function(index) {
  out <- gsub("_", " ", index)
  out[index %in% c("ecv", "puc")] <- toupper(index[index %in% c("ecv", "puc")])
  out
}


#' @export
print.nomo_hierarchical <- function(x, digits = 3, ...) {
  number <- function(v) nomo_present_number(v, digits)
  nomo_present_header("nomo_hierarchical", "Hierarchical model evaluation")
  nomo_present_facts(c(
    sprintf("%s model", nomo_hierarchical_structure_label(x)),
    sprintf("General factor: %s", x$general),
    sprintf("Group factors: %s", paste(names(x$groups), collapse = ", "))
  ))
  nomo_present_facts(sprintf(
    "Estimand: %s",
    if (identical(x$estimand, "latent_response")) {
      "latent-response composite (ordered indicators)"
    } else {
      "unit-weighted observed composite"
    }
  ))

  idx <- x$indices
  idx$label <- nomo_hierarchical_index_label(idx$index)
  nomo_present_section("Total score")
  nomo_present_table(
    idx, c("Index" = "label", "Estimate" = "estimate"),
    formats = list(estimate = number)
  )

  nomo_present_section("Subscales")
  nomo_present_table(
    x$subscales,
    c("Subscale" = "subscale", "Items" = "n_items",
      "Omega subscale" = "omega_subscale",
      "Omega hierarchical subscale" = "omega_hierarchical_subscale"),
    formats = list(omega_subscale = number, omega_hierarchical_subscale = number)
  )

  if (!is.null(x$factors) && nrow(x$factors)) {
    nomo_present_section("Factor scores")
    nomo_present_table(
      x$factors,
      c("Factor" = "factor", "Role" = "role",
        "Determinacy" = "factor_determinacy", "Min competing r" = "min_competing_r",
        "Replicability H" = "construct_replicability"),
      formats = list(factor_determinacy = number, min_competing_r = number,
                     construct_replicability = number)
    )
  }

  flagged <- x$notes[x$notes$severity %in% c("review", "concern"), , drop = FALSE]
  if (nrow(flagged)) {
    nomo_present_section("Notes")
    nomo_present_notes(flagged)
  }

  cat("\n")
  nomo_present_text(
    "No index is treated as a pass/fail threshold; see nomo_table(x, \"indices\")."
  )
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
  nomo_present_header("nomo_hierarchical", "Hierarchical model evaluation",
                      summary = TRUE)
  nomo_present_facts(c(
    sprintf("%s model", nomo_hierarchical_structure_label(x)),
    sprintf("General factor: %s", x$general)
  ))

  nomo_present_section("Total score")
  nomo_present_bullets(sprintf(
    "%s = %s: %s", nomo_hierarchical_index_label(x$indices$index),
    nomo_present_number(x$indices$estimate), x$indices$interpretation
  ))

  nomo_present_section("Subscales")
  nomo_present_table(
    x$subscales,
    c("Subscale" = "subscale", "Items" = "n_items",
      "Omega subscale" = "omega_subscale",
      "Omega hierarchical subscale" = "omega_hierarchical_subscale")
  )

  if (nrow(x$notes)) {
    nomo_present_section("Notes")
    nomo_present_notes(x$notes)
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
        nomo_plot_labs(
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
    nomo_plot_labs(
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
    type = c(
      "indices", "subscales", "factors", "loadings", "notes", "decision_log"
    ),
    ...) {
  type <- match.arg(type)

  if (type == "indices") return(x$indices)
  if (type == "factors") return(x$factors)
  if (type == "subscales") return(x$subscales)
  if (type == "loadings") return(x$loadings)
  if (type == "notes") return(x$notes)
  x$decision_log
}
