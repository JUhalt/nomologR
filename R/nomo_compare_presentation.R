# Presentation methods for nomo_compare -----------------------------------------

nomo_compare_relation_label <- function(relation) {
  labels <- c(
    more_constrained = "nested, more constrained",
    less_constrained = "nested, less constrained",
    equivalent = "equivalent",
    non_nested = "not nested",
    different_variables = "different observed variables"
  )
  out <- unname(labels[relation])
  out[is.na(out)] <- relation[is.na(out)]
  out
}


#' @export
print.nomo_compare <- function(x, ...) {
  cat("<nomo_compare>\n")
  cat(sprintf(
    "Models: %d | Reference: %s | Estimator: %s | Cases: %s | Origin: %s\n",
    nrow(x$models), x$reference, x$estimator,
    format(x$models$n_used[[1L]], trim = TRUE),
    gsub("_", "-", x$origin)
  ))
  cat("Rationale: ", x$rationale, "\n", sep = "")

  if (nrow(x$comparisons)) {
    cat(sprintf("\nCompared with `%s`:\n", x$reference))
    for (i in seq_len(nrow(x$comparisons))) {
      cmp <- x$comparisons[i, , drop = FALSE]
      test <- if (isTRUE(cmp$test_available)) {
        sprintf(
          "chi-square difference = %.2f, df = %s, %s",
          cmp$chisq_diff, format(cmp$df_diff, trim = TRUE),
          nomo_compare_format_p(cmp$p_value)
        )
      } else {
        "no difference test"
      }
      fit <- if (is.finite(cmp$delta_cfi)) {
        sprintf("dCFI %+.3f, dRMSEA %+.3f", cmp$delta_cfi, cmp$delta_rmsea)
      } else {
        "change in fit unavailable"
      }
      ic <- if (isTRUE(cmp$ic_available)) sprintf("dAIC %+.1f", cmp$delta_aic) else NULL
      cat(sprintf(
        "  - %s (%s): %s\n",
        cmp$model,
        nomo_compare_relation_label(cmp$relation),
        paste(c(test, fit, ic), collapse = "; ")
      ))
    }
  }

  cat("\nNo model was selected automatically. Use summary() for interpretations and measurement evidence.\n")
  invisible(x)
}


#' Summarize a measurement-model comparison
#'
#' @param object A `nomo_compare` object.
#' @param ... Unused.
#'
#' @return An object of class `summary_nomo_compare` containing model fit,
#'   comparisons with interpretations, side-by-side standardized loadings, and
#'   measurement evidence.
#' @export
summary.nomo_compare <- function(object, ...) {
  out <- list(
    rationale = object$rationale,
    origin = object$origin,
    reference = object$reference,
    models = object$models,
    comparisons = object$comparisons,
    loadings = object$loadings,
    evidence = object$evidence,
    references = object$references,
    decision_log = object$decision_log
  )
  class(out) <- c("summary_nomo_compare", "list")
  out
}


#' @export
print.summary_nomo_compare <- function(x, ...) {
  round_cols <- function(tab, cols, digits) {
    for (nm in intersect(cols, names(tab))) tab[[nm]] <- round(tab[[nm]], digits)
    tab
  }

  cat("nomologR measurement-model comparison\n")
  cat("Rationale: ", x$rationale, "\n", sep = "")
  cat("Origin: ", gsub("_", "-", x$origin), " | Reference model: ", x$reference, "\n", sep = "")

  cat("\nModel fit and information criteria\n")
  models <- round_cols(x$models, c("cfi", "tli", "rmsea", "srmr"), 3L)
  models <- round_cols(models, c("chisq", "aic", "bic"), 1L)
  print(models[, c(
    "model", "npar", "df", "chisq", "cfi", "tli", "rmsea", "srmr", "aic", "bic",
    "fixed_zero_loadings"
  ), drop = FALSE], row.names = FALSE, width = Inf)

  if (nrow(x$comparisons)) {
    cat("\nComparisons with the reference model\n")
    cmp <- round_cols(x$comparisons, c("chisq_diff"), 2L)
    cmp <- round_cols(cmp, c("p_value"), 4L)
    cmp <- round_cols(cmp, c("delta_cfi", "delta_tli", "delta_rmsea", "delta_srmr"), 3L)
    cmp <- round_cols(cmp, c("delta_aic", "delta_bic"), 1L)
    print(cmp[, c(
      "model", "relation", "nesting_check", "method", "chisq_diff", "df_diff",
      "p_value", "delta_cfi", "delta_rmsea", "delta_srmr", "delta_aic", "delta_bic"
    ), drop = FALSE], row.names = FALSE, width = Inf)

    cat("\nInterpretation\n")
    for (i in seq_len(nrow(x$comparisons))) {
      cat("- ", x$comparisons$interpretation[[i]], "\n", sep = "")
    }
  }

  if (nrow(x$loadings)) {
    cat("\nStandardized loadings by model\n")
    loadings <- x$loadings
    for (nm in setdiff(names(loadings), c("factor", "item"))) {
      loadings[[nm]] <- round(loadings[[nm]], 3L)
    }
    print(loadings, n = Inf, width = Inf)
  }

  if (nrow(x$evidence)) {
    cat("\nMeasurement evidence by model\n")
    evidence <- round_cols(x$evidence, "estimate", 3L)
    print(evidence[, c("model", "construct", "metric", "estimate"), drop = FALSE], n = Inf, width = Inf)
    notes <- unique(x$evidence$note[nzchar(x$evidence$note)])
    if (length(notes)) {
      cat("Notes:\n")
      for (note in notes) cat("- ", note, "\n", sep = "")
    }
  }

  cat(
    "\nNo model was selected automatically. Difference tests, changes in fit, ",
    "information criteria, and measurement evidence answer different questions; ",
    "read them together with theory and the recorded rationale.\n",
    sep = ""
  )
  invisible(x)
}


#' Plot a measurement-model comparison
#'
#' @param x A `nomo_compare` object.
#' @param type `"fit"` shows CFI, TLI, RMSEA, and SRMR for each model with the
#'   configured teaching references; `"loadings"` shows standardized loadings
#'   for each model side by side.
#' @param ... Unused.
#'
#' @return A `ggplot2` object.
#' @export
plot.nomo_compare <- function(x, type = c("fit", "loadings"), ...) {
  type <- match.arg(type)
  model_levels <- rev(x$models$model)

  if (type == "fit") {
    metrics <- c(CFI = "cfi", TLI = "tli", RMSEA = "rmsea", SRMR = "srmr")
    dat <- do.call(rbind, lapply(names(metrics), function(label) {
      data.frame(
        model = x$models$model,
        metric = label,
        value = x$models[[metrics[[label]]]],
        stringsAsFactors = FALSE
      )
    }))
    dat <- dat[is.finite(dat$value), , drop = FALSE]
    if (!nrow(dat)) stop("No finite fit indices are available to plot.", call. = FALSE)

    refs <- x$guidance$fit_reference
    ref_dat <- data.frame(
      metric = names(metrics),
      reference = c(refs$cfi, refs$tli, refs$rmsea, refs$srmr),
      stringsAsFactors = FALSE
    )
    dat$metric <- factor(dat$metric, levels = names(metrics))
    ref_dat$metric <- factor(ref_dat$metric, levels = names(metrics))
    dat$model <- factor(dat$model, levels = model_levels)

    return(
      ggplot2::ggplot(dat, ggplot2::aes(x = value, y = model)) +
        ggplot2::geom_vline(
          data = ref_dat,
          mapping = ggplot2::aes(xintercept = reference),
          linetype = 2
        ) +
        ggplot2::geom_point(size = 3) +
        ggplot2::facet_wrap(stats::as.formula("~ metric"), scales = "free_x") +
        ggplot2::labs(
          title = "Global fit across compared models",
          subtitle = "Dashed lines = teaching references; they are prompts, not decision rules.",
          x = NULL,
          y = NULL,
          caption = "No model is selected automatically."
        ) +
        ggplot2::theme_minimal()
    )
  }

  if (!nrow(x$loadings)) stop("No standardized loadings are available to plot.", call. = FALSE)
  model_cols <- setdiff(names(x$loadings), c("factor", "item"))
  dat <- do.call(rbind, lapply(model_cols, function(nm) {
    data.frame(
      factor = x$loadings$factor,
      item = x$loadings$item,
      model = nm,
      loading = x$loadings[[nm]],
      stringsAsFactors = FALSE
    )
  }))
  dat <- dat[is.finite(dat$loading), , drop = FALSE]
  if (!nrow(dat)) stop("No finite standardized loadings are available to plot.", call. = FALSE)
  dat$item <- factor(dat$item, levels = rev(unique(x$loadings$item)))
  dat$model <- factor(dat$model, levels = model_cols)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = loading, y = item, shape = model)) +
    ggplot2::geom_vline(xintercept = x$guidance$cfa_loading_reference, linetype = 2) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::facet_wrap(stats::as.formula("~ factor"), scales = "free_y") +
    ggplot2::coord_cartesian(xlim = nomo_plot_x_limits(dat$loading)) +
    ggplot2::labs(
      title = "Standardized loadings across compared models",
      subtitle = paste0(
        "Dashed line = teaching reference (",
        format(x$guidance$cfa_loading_reference, trim = TRUE), ")."
      ),
      x = "Standardized loading",
      y = NULL,
      shape = "Model",
      caption = "Loadings fixed to zero appear at 0."
    ) +
    ggplot2::theme_minimal()
  p
}

utils::globalVariables(c("value", "model", "metric", "reference", "loading", "item"))
