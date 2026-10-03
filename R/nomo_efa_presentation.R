# Presentation methods for nomo_efa -----------------------------------------

#' @export
print.nomo_efa <- function(x, ...) {
  nomo_present_header("nomo_efa", "Exploratory factor analysis")
  nomo_efa_present_facts(x)
  nomo_present_facts(c(
    paste0("RMSR: ", nomo_present_stat(x$rmsr, "fit")),
    paste0("Item flags: ", nomo_present_flag_counts(x$item_summary$attention))
  ))
  nomo_efa_present_checks(x$decision_log)
  nomo_present_text(
    "RMSR = root mean square of the off-diagonal residual correlations. No ",
    "item was deleted and no model was refit automatically."
  )
  nomo_present_pointer("summary(x)", "the loadings and the reason for each flag")
  invisible(x)
}


# The settings, as print() and summary() show them. "from nomo_factors()" is
# the wording in both (#145, clarity-29): "handoff" names the contentvalidR
# object in this package.
nomo_efa_present_facts <- function(x) {
  source_text <- switch(
    as.character(x$factor_source),
    nomo_factors = "from nomo_factors()",
    researcher_with_nomo_factors_context = "researcher's choice after nomo_factors()",
    "researcher specified"
  )
  nomo_present_facts(c(
    nomo_factors_cases_text(x$n_cases, x$min_pairwise_n),
    sprintf("Items: %d", x$n_items),
    sprintf("Factors: %d (%s)", x$n_factors, source_text)
  ))
  nomo_present_facts(c(
    sprintf("Correlation: %s", nomo_factors_correlation_label(x$correlation)),
    sprintf("Extraction: %s", x$fm),
    sprintf("Rotation: %s", nomo_efa_rotation_text(x))
  ))
}


# The rotation as fitted (#145, efa-7): a one-factor solution is not rotated,
# whatever `rotation` was requested.
nomo_efa_rotation_text <- function(x) {
  if (isTRUE(x$n_factors < 2L)) return("not applicable (one factor)")
  if (identical(x$rotation, "none")) return("none (unrotated)")
  sprintf("%s (%s)", x$rotation, if (isTRUE(x$oblique)) "oblique" else "orthogonal")
}


# Problems with the solution itself, from the decision log: identification,
# Heywood cases, convergence, and engine messages (#145, efa-1 and efa-3). A
# summary shows a Heywood case with its item instead.
nomo_efa_solution_checks <- function(log,
                                     metrics = c("identification", "heywood", "convergence",
                                                 "engine_warnings")) {
  if (!nomo_efa_has_log(log)) {
    return(data.frame(severity = character(), note = character()))
  }
  rows <- log[log$metric %in% metrics, , drop = FALSE]
  rows <- rows[order(match(rows$severity, c("concern", "review", "info"))), , drop = FALSE]
  data.frame(severity = rows$severity, note = rows$observation, stringsAsFactors = FALSE)
}

nomo_efa_has_log <- function(log) {
  is.data.frame(log) && all(c("object", "metric", "severity", "observation") %in% names(log))
}

# The items with an improper communality, from the decision log.
nomo_efa_heywood_items <- function(log) {
  if (!nomo_efa_has_log(log)) return(character())
  log$object[log$metric == "heywood"]
}

nomo_efa_present_checks <- function(log) {
  checks <- nomo_efa_solution_checks(log)
  if (!nrow(checks)) return(invisible(NULL))
  nomo_present_section("Solution checks")
  nomo_present_notes(checks)
  cat("\n")
}


#' Summarize a guided exploratory factor analysis
#'
#' @param object A `nomo_efa` object.
#' @param ... Unused.
#'
#' @return An object of class `summary_nomo_efa`. Printing it shows the
#'   settings, the supporting adequacy evidence, each item's loadings and
#'   communality with the reason for every flag, the factor correlations, any
#'   problem with the solution, and the largest residual correlations.
#' @export
summary.nomo_efa <- function(object, ...) {
  out <- list(
    n_cases = object$n_cases,
    min_pairwise_n = object$min_pairwise_n,
    n_items = object$n_items,
    n_factors = object$n_factors,
    factor_source = object$factor_source,
    factor_context = object$factor_context,
    correlation = object$correlation,
    fm = object$fm,
    extraction_note = object$extraction_note,
    rotation = object$rotation,
    oblique = object$oblique,
    item_summary = object$item_summary,
    pattern_matrix = object$pattern_matrix,
    structure_matrix = object$structure_matrix,
    factor_correlations = object$factor_correlations,
    rmsr = object$rmsr,
    largest_residuals = utils::head(object$residual_pairs, 5L),
    kmo = object$kmo,
    bartlett = object$bartlett,
    sample_adequacy = object$sample_adequacy,
    decision_log = object$decision_log,
    guidance = object$guidance
  )
  class(out) <- c("summary_nomo_efa", "list")
  out
}

#' @export
print.summary_nomo_efa <- function(x, ...) {
  nomo_present_header("nomo_efa", "Exploratory factor analysis", summary = TRUE)
  nomo_efa_present_facts(x)
  nomo_present_facts(c(
    paste0("KMO: ", nomo_factors_kmo_text(x$kmo)),
    if (isTRUE(x$bartlett$available)) {
      paste0(
        "Bartlett's test: ",
        nomo_present_chisq(x$bartlett$chisq, x$bartlett$df, x$bartlett$p_value),
        nomo_factors_bartlett_qualifier(x$correlation)
      )
    } else {
      ""
    }
  ))

  # A value near its teaching reference shows the decimals that tell them
  # apart (guide point 9); loadings are compared in absolute value.
  load_ref <- nomo_null_default(x$guidance$efa_loading_reference, 0.40)
  cross_ref <- nomo_null_default(x$guidance$efa_crossloading_reference, 0.30)
  communality_ref <- nomo_null_default(x$guidance$efa_communality_reference, 0.40)
  loading <- function(ref) {
    function(v) nomo_present_stat(v, "loading", reference = sign(v) * ref)
  }
  items <- x$item_summary
  # An item with an improper communality is shown as a concern beside its
  # communality, which takes the decimals that tell it apart from 1 (#145,
  # efa-1). The stored flags are unchanged.
  heywood <- items$item %in% nomo_efa_heywood_items(x$decision_log)
  status <- ifelse(heywood, "concern", items$attention)
  text <- items$explanation
  text[heywood] <- trimws(paste(
    vapply(items$communality[heywood], function(h2) nomo_efa_heywood_text(NULL, h2),
           character(1)),
    ifelse(items$attention[heywood] == "KEEP", "", items$explanation[heywood])
  ))
  items$flag <- nomo_present_status(status)
  nomo_present_section("Item structure")
  nomo_present_table(
    items,
    c("Item" = "item", "Factor" = "primary_factor", "Loading" = "primary_loading",
      "Next factor" = "secondary_factor", "Loading" = "secondary_loading",
      "Communality" = "communality", "Flag" = "flag"),
    formats = list(
      primary_loading = loading(load_ref),
      secondary_loading = loading(cross_ref),
      communality = function(v) {
        nomo_present_stat(v, "proportion", reference = ifelse(heywood, 1, communality_ref))
      }
    ),
    more = "nomo_table(x, \"items\")"
  )
  if (!any(nzchar(items$flag))) {
    nomo_present_text("No item reached a teaching reference.", indent = 2L)
  }
  nomo_present_flagged(unit = items$item, status = status, text = text)

  nomo_present_section("Factor correlations")
  fc <- x$factor_correlations
  if (isTRUE(x$n_factors < 2L)) {
    nomo_present_text("Not applicable to a one-factor solution.", indent = 2L)
  } else if (!isTRUE(x$oblique)) {
    # An orthogonal or unrotated solution fixes them at 0; a table of zeros
    # would read as estimates (#145, efa-7).
    nomo_present_text(
      "Fixed at 0: ",
      if (identical(x$rotation, "none")) {
        "an unrotated solution"
      } else {
        sprintf("the orthogonal %s rotation", x$rotation)
      },
      " does not estimate them.",
      indent = 2L
    )
  } else {
    pairs <- which(lower.tri(fc), arr.ind = TRUE)
    nomo_present_table(
      data.frame(
        factor1 = colnames(fc)[pairs[, "col"]],
        factor2 = rownames(fc)[pairs[, "row"]],
        r = fc[pairs],
        stringsAsFactors = FALSE
      ),
      c("Factor 1" = "factor1", "Factor 2" = "factor2", "r" = "r"),
      formats = list(r = function(v) nomo_present_stat(v, "r")),
      more = "nomo_table(x, \"factor_correlations\")"
    )
  }

  checks <- nomo_efa_solution_checks(
    x$decision_log, metrics = c("identification", "convergence", "engine_warnings")
  )
  if (nrow(checks)) {
    nomo_present_section("Solution checks")
    nomo_present_notes(checks)
  }

  nomo_present_section("Largest residual correlations")
  nomo_present_text("RMSR: ", nomo_present_stat(x$rmsr, "fit"), indent = 2L)
  nomo_present_table(
    x$largest_residuals,
    c("Item 1" = "item1", "Item 2" = "item2", "Residual" = "residual"),
    formats = list(residual = function(v) nomo_present_stat(v, "r", digits = 3L)),
    more = "nomo_table(x, \"residuals\")"
  )

  nomo_present_key(c(
    KMO = "Kaiser-Meyer-Olkin measure of sampling adequacy.",
    RMSR = "Root mean square of the off-diagonal residual correlations."
  ), title = "Abbreviations")

  cat("\n")
  nomo_present_text(
    "Numerical references trigger inspection, not automatic deletion or ",
    "hidden refitting."
  )
  nomo_present_pointer(
    c("nomo_table(x, \"pattern\")", "nomo_table(x, \"decision_log\")"),
    c("the full pattern matrix", "every recorded decision")
  )
  invisible(x)
}

#' Plot exploratory factor-analysis evidence
#'
#' @param x A `nomo_efa` object.
#' @param type Plot type: `"pattern"`, `"items"`, `"residuals"`, or
#'   `"factor_correlations"`.
#' @param ... Unused.
#'
#' @return A `ggplot2` object.
#' @export
plot.nomo_efa <- function(x,
                          type = c(
                            "pattern",
                            "items",
                            "residuals",
                            "factor_correlations"
                          ),
                          ...) {
  type <- nomo_match_arg(type)

  if (type == "pattern") {
    dat <- as.data.frame(as.table(x$pattern_matrix), stringsAsFactors = FALSE)
    names(dat) <- c("item", "factor", "loading")
    dat$item <- factor(dat$item, levels = rev(x$items))

    max_abs <- max(abs(dat$loading), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
    dat$label_contrast <- abs(dat$loading) >= 0.55 * max_abs

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = factor, y = item, fill = loading)
      ) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(
          ggplot2::aes(
            label = nomo_present_stat(loading, "loading"),
            colour = label_contrast
          ),
          size = 3
        ) +
        ggplot2::scale_colour_manual(
          values = c(`FALSE` = "black", `TRUE` = "white"),
          guide = "none"
        ) +
        ggplot2::scale_fill_gradient2(
          midpoint = 0,
          limits = c(-max_abs, max_abs)
        ) +
        nomo_plot_labs(
          title = "EFA pattern matrix",
          subtitle = nomo_efa_solution_subtitle(x),
          x = NULL,
          y = NULL,
          fill = "Loading",
          caption = paste(
            "Diverging scale preserves loading sign.",
            "Reference values trigger review; they do not authorize automatic deletion."
          )
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }

  if (type == "items") {
    primary <- x$item_summary[, c("item", "primary_loading", "attention")]
    names(primary)[names(primary) == "primary_loading"] <- "loading"
    primary$loading_type <- "Primary"

    secondary <- x$item_summary[, c("item", "secondary_loading", "attention")]
    names(secondary)[names(secondary) == "secondary_loading"] <- "loading"
    secondary$loading_type <- "Secondary"

    dat <- dplyr::bind_rows(primary, secondary)
    dat$loading <- abs(dat$loading)
    dat <- dat[is.finite(dat$loading), , drop = FALSE]
    dat$item <- factor(dat$item, levels = rev(x$items))
    dat$loading_type <- factor(dat$loading_type, levels = c("Primary", "Secondary"))
    # The item's flag, by shape and color together (guide point 27); an
    # improper communality is a concern, as the summary shows it.
    heywood <- dat$item %in% nomo_efa_heywood_items(x$decision_log)
    dat$status <- nomo_plot_status(ifelse(heywood, "concern", dat$attention))

    load_ref <- nomo_null_default(x$guidance$efa_loading_reference, 0.40)
    cross_ref <- nomo_null_default(x$guidance$efa_crossloading_reference, 0.30)
    references <- data.frame(
      loading_type = factor(c("Primary", "Secondary"), levels = c("Primary", "Secondary")),
      x = c(load_ref, cross_ref)
    )
    references <- references[references$loading_type %in% dat$loading_type, , drop = FALSE]

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = loading, y = item, shape = status, colour = status)
      ) +
        ggplot2::geom_vline(
          data = references,
          ggplot2::aes(xintercept = x),
          linetype = "dashed",
          inherit.aes = FALSE
        ) +
        ggplot2::geom_point(size = 2.8) +
        ggplot2::facet_wrap(
          stats::as.formula("~ loading_type"),
          nrow = 1,
          labeller = ggplot2::as_labeller(c(Primary = "Primary loading",
                                            Secondary = "Secondary loading"))
        ) +
        nomo_plot_status_scales(dat$status, name = "Flag") +
        nomo_plot_labs(
          title = "Primary and secondary EFA loadings",
          subtitle = sprintf(
            paste(
              "Absolute loadings. Dashed lines mark the %s loading and %s",
              "cross-loading teaching references."
            ),
            nomo_present_stat(load_ref, "loading"),
            nomo_present_stat(cross_ref, "loading")
          ),
          x = "Absolute loading",
          y = NULL,
          caption = paste(
            "Each item's flag (review or concern) is explained in summary(x).",
            "No reference value deletes an item."
          )
        ) +
        ggplot2::theme_minimal(base_size = 11)
    )
  }

  if (type == "residuals") {
    mat <- x$residual_matrix
    idx <- which(lower.tri(mat), arr.ind = TRUE)

    dat <- data.frame(
      item1 = colnames(mat)[idx[, 2]],
      item2 = rownames(mat)[idx[, 1]],
      residual = as.numeric(mat[idx]),
      stringsAsFactors = FALSE
    )
    dat$item1 <- factor(dat$item1, levels = x$items)
    dat$item2 <- factor(dat$item2, levels = rev(x$items))

    max_abs <- max(abs(dat$residual), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = item1, y = item2, fill = residual)
      ) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_gradient2(
          midpoint = 0,
          limits = c(-max_abs, max_abs),
          labels = nomo_plot_bounded_labels
        ) +
        nomo_plot_labs(
          title = "EFA residual-correlation matrix",
          subtitle = sprintf(
            "Unique off-diagonal pairs | Root mean square residual (RMSR) = %s",
            nomo_present_stat(x$rmsr, "fit")
          ),
          x = NULL,
          y = NULL,
          fill = "Residual",
          caption = paste(
            "Only one triangle is shown because the residual matrix is symmetric.",
            "Inspect localized strain; no hidden refitting is performed."
          )
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    )
  }

  phi <- x$factor_correlations

  if (nrow(phi) < 2L) {
    return(
      ggplot2::ggplot() +
        ggplot2::annotate(
          "text",
          x = 0,
          y = 0,
          label = "One-factor solution: no interfactor correlations to display."
        ) +
        ggplot2::xlim(-1, 1) +
        ggplot2::ylim(-1, 1) +
        nomo_plot_labs(
          title = "EFA factor correlations",
          subtitle = "Interfactor correlations require at least two factors"
        ) +
        ggplot2::theme_void(base_size = 11)
    )
  }

  idx <- which(lower.tri(phi), arr.ind = TRUE)
  dat <- data.frame(
    factor1 = colnames(phi)[idx[, 2]],
    factor2 = rownames(phi)[idx[, 1]],
    correlation = as.numeric(phi[idx]),
    stringsAsFactors = FALSE
  )
  dat$factor1 <- factor(dat$factor1, levels = colnames(phi))
  dat$factor2 <- factor(dat$factor2, levels = rev(rownames(phi)))
  dat$label_contrast <- abs(dat$correlation) >= 0.55

  ggplot2::ggplot(
    dat,
    ggplot2::aes(x = factor1, y = factor2, fill = correlation)
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(
      ggplot2::aes(
        label = nomo_present_stat(correlation, "r"),
        colour = label_contrast
      ),
      size = 3
    ) +
    ggplot2::scale_colour_manual(
      values = c(`FALSE` = "black", `TRUE` = "white"),
      guide = "none"
    ) +
    ggplot2::scale_fill_gradient2(
      midpoint = 0,
      limits = c(-1, 1),
      labels = nomo_plot_bounded_labels
    ) +
    nomo_plot_labs(
      title = "EFA factor correlations",
      subtitle = if (isTRUE(x$oblique)) {
        "Unique interfactor correlations from the oblique solution"
      } else if (identical(x$rotation, "none")) {
        "Unrotated solution: the factors are uncorrelated by construction"
      } else {
        "Orthogonal rotation constrains interfactor correlations to zero"
      },
      x = NULL,
      y = NULL,
      fill = "Correlation",
      caption = "Diagonal 1.00 values and duplicate upper-triangle cells are omitted."
    ) +
    ggplot2::theme_minimal(base_size = 11)
}


# The pattern plot's subtitle names the solution that was fitted (#145, efa-7).
nomo_efa_solution_subtitle <- function(x) {
  if (isTRUE(x$n_factors < 2L)) {
    "One-factor solution: the loadings are not rotated"
  } else if (isTRUE(x$oblique)) {
    "Oblique solution: pattern coefficients are primary for factor interpretation"
  } else if (identical(x$rotation, "none")) {
    "Unrotated solution: the loadings are not rotated toward simple structure"
  } else {
    "Orthogonal solution: factor axes are constrained to be uncorrelated"
  }
}

utils::globalVariables(c(
  "item",
  "factor",
  "loading",
  "attention",
  "loading_type",
  "reference",
  "status",
  "x",
  "item1",
  "item2",
  "residual",
  "factor1",
  "factor2",
  "correlation",
  "label_contrast"
))
