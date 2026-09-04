# Presentation methods for nomo_efa -----------------------------------------

#' @export
print.nomo_efa <- function(x, ...) {
  source_text <- if (identical(x$factor_source, "nomo_factors")) {
    "nomo_factors() handoff"
  } else {
    "researcher specified"
  }

  cat("<nomo_efa>\n")
  cat(sprintf(
    "Cases: %d | Items: %d | Factors: %d (%s)\n",
    x$n_cases, x$n_items, x$n_factors, source_text
  ))
  cat(sprintf(
    "Correlation: %s | Extraction: %s | Rotation: %s\n",
    x$correlation, x$fm, x$rotation
  ))
  cat(sprintf("Off-diagonal RMSR: %.3f\n", x$rmsr))

  counts <- table(factor(
    x$item_summary$attention,
    levels = c("KEEP", "REVIEW", "STRONG REVIEW")
  ))
  cat(sprintf(
    "Item review: %d KEEP | %d REVIEW | %d STRONG REVIEW\n",
    counts[["KEEP"]], counts[["REVIEW"]], counts[["STRONG REVIEW"]]
  ))
  cat("No items were automatically deleted or refit.\n")
  invisible(x)
}

#' Summarize a guided exploratory factor analysis
#'
#' @param object A `nomo_efa` object.
#' @param ... Unused.
#'
#' @return An object of class `summary_nomo_efa`.
#' @export
summary.nomo_efa <- function(object, ...) {
  out <- list(
    n_cases = object$n_cases,
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
    decision_log = object$decision_log
  )
  class(out) <- c("summary_nomo_efa", "list")
  out
}

#' @export
print.summary_nomo_efa <- function(x, ...) {
  source_text <- if (identical(x$factor_source, "nomo_factors")) {
    "from nomo_factors()"
  } else {
    "researcher specified"
  }

  cat("nomologR exploratory factor analysis\n")
  cat(sprintf(
    "%d cases | %d items | %d factors (%s)\n",
    x$n_cases, x$n_items, x$n_factors, source_text
  ))
  cat(sprintf(
    "Correlation: %s | Extraction: %s | Rotation: %s\n",
    x$correlation, x$fm, x$rotation
  ))

  if (isTRUE(x$kmo$available)) {
    cat(sprintf("Supporting adequacy: KMO = %.3f", x$kmo$overall))
  } else {
    cat("Supporting adequacy: KMO unavailable")
  }
  if (isTRUE(x$bartlett$available)) {
    cat(sprintf(
      " | Bartlett chi-square(%g) = %.2f, p %s",
      x$bartlett$df,
      x$bartlett$chisq,
      if (x$bartlett$p_value < .001) "< .001" else {
        sprintf("= %.3f", x$bartlett$p_value)
      }
    ))
  }
  cat("\n\n")

  cat("Item-level structural review\n")
  compact <- x$item_summary[, c(
    "item", "primary_factor", "primary_loading",
    "secondary_factor", "secondary_loading",
    "communality", "attention"
  )]
  print(compact, row.names = FALSE)

  flagged <- x$item_summary[x$item_summary$attention != "KEEP", , drop = FALSE]
  if (nrow(flagged) == 0L) {
    cat("\nNo configured numeric EFA review flags were triggered.\n")
  } else {
    cat("\nItems requiring review\n")
    print(
      flagged[, c("item", "attention", "explanation")],
      row.names = FALSE,
      width = Inf
    )
  }

  if (x$n_factors > 1L) {
    cat("\nFactor correlations\n")
    print(round(x$factor_correlations, 3))
  } else {
    cat("\nFactor correlations: not applicable to a one-factor solution.\n")
  }

  cat(sprintf("\nOff-diagonal RMSR: %.3f\n", x$rmsr))
  if (nrow(x$largest_residuals)) {
    cat("\nLargest localized residuals\n")
    print(x$largest_residuals, row.names = FALSE)
  }

  cat(
    "\nInterpretation rule: numerical references trigger inspection, ",
    "not automatic deletion or hidden refitting.\n",
    sep = ""
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
  type <- match.arg(type)

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
            label = sprintf("%.2f", loading),
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
        ggplot2::labs(
          title = "EFA pattern matrix",
          subtitle = if (x$oblique) {
            "Oblique solution: pattern coefficients are primary for factor interpretation"
          } else {
            "Orthogonal solution: factor axes are constrained to be uncorrelated"
          },
          x = NULL,
          y = NULL,
          fill = "Loading",
          caption = paste(
            "Diverging scale preserves loading sign.",
            "Reference values trigger review; they do not authorize automatic deletion."
          )
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (type == "items") {
    primary <- x$item_summary[, c(
      "item", "primary_loading", "attention"
    )]
    names(primary)[names(primary) == "primary_loading"] <- "loading"
    primary$loading_type <- "Primary"

    secondary <- x$item_summary[, c(
      "item", "secondary_loading", "attention"
    )]
    names(secondary)[names(secondary) == "secondary_loading"] <- "loading"
    secondary$loading_type <- "Secondary"

    dat <- dplyr::bind_rows(primary, secondary)
    dat$loading <- abs(dat$loading)
    dat$item <- factor(dat$item, levels = rev(x$items))
    dat$loading_type <- factor(
      dat$loading_type,
      levels = c("Primary", "Secondary")
    )

    references <- data.frame(
      reference = factor(
        c("Primary review", "Cross-loading review"),
        levels = c("Primary review", "Cross-loading review")
      ),
      x = c(
        x$guidance$efa_loading_reference,
        x$guidance$efa_crossloading_reference
      )
    )

    return(
      ggplot2::ggplot(
        dat,
        ggplot2::aes(x = loading, y = item, shape = loading_type)
      ) +
        ggplot2::geom_vline(
          data = references,
          ggplot2::aes(xintercept = x, linetype = reference),
          inherit.aes = FALSE
        ) +
        ggplot2::geom_point(size = 2.8, na.rm = TRUE) +
        ggplot2::scale_shape_manual(
          values = c(Primary = 16, Secondary = 1)
        ) +
        ggplot2::labs(
          title = "Primary and secondary EFA loadings",
          subtitle = paste(
            "Filled circles are primary loadings; open circles are secondary loadings.",
            "Both teaching references are shown."
          ),
          x = "Absolute loading",
          y = NULL,
          shape = "Loading",
          linetype = "Teaching reference",
          caption = paste(
            "Item attention (KEEP / REVIEW / STRONG REVIEW) is reported in the item table.",
            "No threshold automatically deletes an item."
          )
        ) +
        ggplot2::theme_minimal()
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
          limits = c(-max_abs, max_abs)
        ) +
        ggplot2::labs(
          title = "EFA residual-correlation matrix",
          subtitle = sprintf(
            "Unique off-diagonal pairs only | RMSR = %.3f",
            x$rmsr
          ),
          x = NULL,
          y = NULL,
          fill = "Residual",
          caption = paste(
            "Only one triangle is shown because the residual matrix is symmetric.",
            "Inspect localized strain; no hidden refitting is performed."
          )
        ) +
        ggplot2::theme_minimal() +
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
        ggplot2::labs(
          title = "EFA factor correlations",
          subtitle = "Interfactor correlations require at least two factors"
        ) +
        ggplot2::theme_void()
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
        label = sprintf("%.2f", correlation),
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
      limits = c(-1, 1)
    ) +
    ggplot2::labs(
      title = "EFA factor correlations",
      subtitle = if (x$oblique) {
        "Unique interfactor correlations from the oblique solution"
      } else {
        "Orthogonal rotation constrains interfactor correlations to zero"
      },
      x = NULL,
      y = NULL,
      fill = "Correlation",
      caption = "Diagonal 1.00 values and duplicate upper-triangle cells are omitted."
    ) +
    ggplot2::theme_minimal()
}

utils::globalVariables(c(
  "item",
  "factor",
  "loading",
  "attention",
  "loading_type",
  "reference",
  "x",
  "item1",
  "item2",
  "residual",
  "factor1",
  "factor2",
  "correlation",
  "label_contrast"
))
