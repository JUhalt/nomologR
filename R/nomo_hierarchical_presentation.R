# Presentation for nomo_hierarchical ------------------------------------------

nomo_hierarchical_structure_label <- function(x) {
  if (identical(x$structure, "higher_order")) "Higher-order" else "Bifactor"
}


# Index names as readers know them, in sentence case for a table cell:
# "Omega hierarchical", "ECV", "PUC".
nomo_hierarchical_index_label <- function(index) {
  labels <- c(
    omega_total = "Omega total",
    omega_hierarchical = "Omega hierarchical",
    omega_hierarchical_relative = "Omega hierarchical, relative",
    ecv = "ECV",
    puc = "PUC"
  )
  out <- unname(labels[index])
  out[is.na(out)] <- gsub("_", " ", index[is.na(out)])
  out
}


# The total-score omegas describe a composite; ECV and PUC describe the item
# set, so they are shown apart (#145).
nomo_hierarchical_item_set <- c("ecv", "puc")


# The precision of each column: omegas, determinacy, and H as reliabilities,
# shares as proportions, and the competing-scores correlation as an r (#144).
nomo_hierarchical_formats <- function(digits = NULL) {
  stat <- function(kind) function(v) nomo_present_stat(v, kind, digits = digits)
  list(
    estimate = stat("reliability"),
    share = stat("proportion"),
    omega_subscale = stat("reliability"),
    omega_hierarchical_subscale = stat("reliability"),
    factor_determinacy = stat("reliability"),
    min_competing_r = stat("r"),
    construct_replicability = stat("reliability")
  )
}


nomo_hierarchical_present_facts <- function(x) {
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
}


nomo_hierarchical_present_tables <- function(x, formats, indices = TRUE) {
  idx <- x$indices
  idx$label <- nomo_hierarchical_index_label(idx$index)
  # A proportion of the composite (omega) or of the items (ECV, PUC).
  idx$share <- idx$estimate
  if (isTRUE(indices)) {
    nomo_present_section("Total score")
    nomo_present_table(
      idx[!idx$index %in% nomo_hierarchical_item_set, , drop = FALSE],
      c("Index" = "label", "Estimate" = "estimate"),
      formats = formats,
      more = "nomo_table(x, \"indices\")"
    )
    nomo_present_section("Item set")
    nomo_present_table(
      idx[idx$index %in% nomo_hierarchical_item_set, , drop = FALSE],
      c("Index" = "label", "Estimate" = "share"),
      formats = formats,
      more = "nomo_table(x, \"indices\")"
    )
  }

  nomo_present_section("Subscales")
  nomo_present_table(
    x$subscales,
    c("Subscale" = "subscale", "Items" = "n_items",
      "Omega subscale" = "omega_subscale",
      "Omega hierarchical subscale" = "omega_hierarchical_subscale"),
    formats = formats,
    more = "nomo_table(x, \"subscales\")"
  )

  if (!is.null(x$factors) && nrow(x$factors)) {
    nomo_present_section("Factor scores")
    nomo_present_table(
      x$factors,
      c("Factor" = "factor", "Role" = "role",
        "Determinacy" = "factor_determinacy", "Min competing r" = "min_competing_r",
        "Replicability H" = "construct_replicability"),
      formats = formats,
      more = "nomo_table(x, \"factors\")"
    )
  }
}


nomo_hierarchical_present_key <- function(x) {
  key <- c(
    ECV = "Explained common variance: the share of the items' common variance that the general factor explains",
    PUC = "Percentage of uncontaminated correlations, shown as a proportion: the share of item correlations that reflect the general factor alone",
    "Min competing r" = "The lowest correlation two equally valid sets of factor scores could have, twice the squared determinacy minus one",
    "Replicability H" = "Construct replicability (Hancock & Mueller, 2001): how well a factor's own indicators, optimally weighted, define it"
  )
  if (is.null(x$factors) || !nrow(x$factors)) key <- key[1:2]
  nomo_present_key(key, title = "What these abbreviations mean")
}


# The notes' one-sentence form, for notes stored before it existed.
nomo_hierarchical_brief <- function(notes) {
  if ("brief" %in% names(notes)) notes$brief else notes$note
}


nomo_hierarchical_caveat <- function() {
  nomo_present_text(
    "These indices do not choose between a bifactor and a higher-order ",
    "structure, and no value is treated as a pass/fail threshold."
  )
}


#' @export
print.nomo_hierarchical <- function(x, digits = NULL, ...) {
  nomo_present_header("nomo_hierarchical", "Hierarchical model evaluation",
                      source = "Rodriguez, Reise, & Haviland (2016)")
  nomo_hierarchical_present_facts(x)
  nomo_hierarchical_present_tables(x, nomo_hierarchical_formats(digits))

  notes <- x$notes
  nomo_present_flagged(status = notes$severity, text = nomo_hierarchical_brief(notes))

  nomo_hierarchical_present_key(x)
  cat("\n")
  nomo_hierarchical_caveat()
  nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"factors\")"),
    c("each index's meaning with the notes in full", "every factor-score value")
  )
  invisible(x)
}


#' @export
summary.nomo_hierarchical <- function(object, ...) {
  out <- list(
    structure = object$structure,
    general = object$general,
    groups = object$groups,
    estimand = object$estimand,
    indices = object$indices,
    subscales = object$subscales,
    factors = object$factors,
    notes = object$notes
  )
  class(out) <- c("summary_nomo_hierarchical", "list")
  out
}


#' @export
print.summary_nomo_hierarchical <- function(x, ...) {
  nomo_present_header("nomo_hierarchical", "Hierarchical model evaluation",
                      summary = TRUE, source = "Rodriguez, Reise, & Haviland (2016)")
  nomo_hierarchical_present_facts(x)

  formats <- nomo_hierarchical_formats()
  idx <- x$indices
  bullets <- sprintf(
    "%s = %s: %s", nomo_hierarchical_index_label(idx$index),
    ifelse(idx$index %in% nomo_hierarchical_item_set,
           formats$share(idx$estimate), formats$estimate(idx$estimate)),
    idx$interpretation
  )
  nomo_present_section("Total score")
  nomo_present_bullets(bullets[!idx$index %in% nomo_hierarchical_item_set])
  nomo_present_section("Item set")
  nomo_present_bullets(bullets[idx$index %in% nomo_hierarchical_item_set])

  nomo_hierarchical_present_tables(x, formats, indices = FALSE)

  notes <- x$notes
  nomo_present_flagged(status = notes$severity, text = notes$note)
  info <- notes[!notes$severity %in% c("review", "concern"), , drop = FALSE]
  if (nrow(info)) {
    nomo_present_section("Notes")
    nomo_present_bullets(info$note)
  }

  nomo_hierarchical_present_key(x)
  cat("\n")
  nomo_hierarchical_caveat()
  nomo_present_pointer("nomo_table(x, \"decision_log\")", "the record of each index and note")
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
#' @return A `ggplot` object. A composite whose shares rest on a negative
#'   variance estimate is left out of the variance plot, and a loading that
#'   rests on one is left out of the loadings plot; the caption names what was
#'   left out.
#' @export
plot.nomo_hierarchical <- function(x, type = c("variance", "loadings"), ...) {
  type <- nomo_match_arg(type)

  if (identical(type, "loadings")) {
    dat <- x$loadings
    long <- rbind(
      data.frame(item = dat$item, source = "General", loading = dat$general_loading),
      data.frame(item = dat$item, source = "Group", loading = dat$group_loading)
    )
    # A loading that rests on a negative variance is NA; the caption names
    # what is left out, so nothing is dropped silently (#145).
    left_out <- long[!is.finite(long$loading), , drop = FALSE]
    long <- long[is.finite(long$loading), , drop = FALSE]
    if (!nrow(long)) {
      stop("No finite standardized loadings are available to plot; see `x$notes`.",
           call. = FALSE)
    }
    long$item <- factor(long$item, levels = rev(dat$item))
    long$source <- factor(long$source, levels = c("General", "Group"))
    caption <- if (nrow(left_out)) {
      pieces <- vapply(c("General", "Group"), function(s) {
        items <- left_out$item[left_out$source == s]
        if (length(items)) paste(tolower(s), "loadings of", paste(items, collapse = ", ")) else ""
      }, character(1))
      paste0(
        "Not drawn: ", nomo_present_or(pieces[nzchar(pieces)], "and"),
        ", which rest on a negative variance estimate (see x$notes)."
      )
    }

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
          shape = "Source",
          caption = caption
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
  # A composite with a share that rests on a negative variance is not drawn,
  # and the caption names it (#145).
  drawn <- is.finite(general) & is.finite(group)
  if (!any(drawn)) {
    stop(
      "No composite's variance shares can be drawn: each rests on a negative ",
      "variance estimate; see `x$notes`.",
      call. = FALSE
    )
  }
  caption <- if (!all(drawn)) {
    paste0(
      "Not drawn: ", nomo_present_or(composites[!drawn], "and"), ", whose ",
      "shares rest on a negative variance estimate (see x$notes)."
    )
  }

  long <- data.frame(
    composite = rep(composites[drawn], 3L),
    source = rep(c("General factor", "Group factors", "Other variance"),
                 each = sum(drawn)),
    share = c(general[drawn], group[drawn], other[drawn])
  )
  long$composite <- factor(long$composite, levels = rev(composites[drawn]))
  long$source <- factor(
    long$source,
    levels = c("Other variance", "Group factors", "General factor")
  )

  ggplot2::ggplot(long, ggplot2::aes(x = share, y = composite, fill = source)) +
    ggplot2::geom_col(width = .65, colour = "grey30", linewidth = .2) +
    ggplot2::scale_fill_manual(values = c(
      "General factor" = "grey25",
      "Group factors" = "grey60",
      "Other variance" = "grey92"
    )) +
    ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0),
                                labels = nomo_plot_bounded_labels) +
    ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
    nomo_plot_labs(
      title = "Where each composite's variance comes from",
      subtitle = paste(
        "General = omega hierarchical;",
        "general + group = omega; the remainder is not common-factor variance"
      ),
      x = "Proportion of composite variance",
      y = NULL,
      fill = NULL,
      caption = caption
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
  type <- nomo_match_arg(type)

  if (type == "indices") return(x$indices)
  if (type == "factors") return(x$factors)
  if (type == "subscales") return(x$subscales)
  if (type == "loadings") return(x$loadings)
  if (type == "notes") return(x$notes)
  x$decision_log
}
