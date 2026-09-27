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
  nomo_present_header("nomo_compare", "Measurement-model comparison")
  nomo_present_facts(c(
    sprintf("Models: %d", nrow(x$models)),
    sprintf("Reference: %s", x$reference),
    sprintf("Estimator: %s", x$estimator),
    sprintf("Cases: %s", format(x$models$n_used[[1L]], trim = TRUE)),
    sprintf("Origin: %s", gsub("_", "-", x$origin))
  ))
  nomo_present_text("Rationale: ", x$rationale)

  if (nrow(x$comparisons)) {
    nomo_present_section(sprintf("Compared with `%s`", x$reference))
    nomo_present_bullets(vapply(seq_len(nrow(x$comparisons)), function(i) {
      cmp <- x$comparisons[i, , drop = FALSE]
      test <- if (isTRUE(cmp$test_available)) {
        sprintf(
          "chi-square difference = %s, df = %s, %s",
          nomo_present_number(cmp$chisq_diff, 2L), format(cmp$df_diff, trim = TRUE),
          nomo_compare_format_p(cmp$p_value)
        )
      } else {
        "no difference test"
      }
      fit <- if (is.finite(cmp$delta_cfi)) {
        sprintf("CFI change %s, RMSEA change %s", nomo_present_signed(cmp$delta_cfi),
                nomo_present_signed(cmp$delta_rmsea))
      } else {
        "change in fit unavailable"
      }
      ic <- if (isTRUE(cmp$ic_available)) {
        sprintf("AIC change %s", nomo_present_signed(cmp$delta_aic, 1L))
      } else {
        NULL
      }
      sprintf("%s (%s): %s", cmp$model, nomo_compare_relation_label(cmp$relation),
              paste(c(test, fit, ic), collapse = "; "))
    }, character(1)))
  }

  cat("\n")
  nomo_present_text(
    "No model was selected automatically. summary() shows interpretations and ",
    "measurement evidence."
  )
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
  nomo_present_header("nomo_compare", "Measurement-model comparison", summary = TRUE)
  nomo_present_text("Rationale: ", x$rationale)
  nomo_present_facts(c(
    sprintf("Origin: %s", gsub("_", "-", x$origin)),
    sprintf("Reference model: %s", x$reference)
  ))

  one_decimal <- function(v) nomo_present_number(v, 1L)
  nomo_present_section("Model fit")
  nomo_present_table(
    x$models,
    c("Model" = "model", "Parameters" = "npar", "df" = "df", "Chi-square" = "chisq",
      "CFI" = "cfi", "TLI" = "tli", "RMSEA" = "rmsea", "SRMR" = "srmr"),
    formats = list(npar = function(v) format(v, trim = TRUE),
                   df = function(v) format(v, trim = TRUE),
                   chisq = function(v) nomo_present_number(v, 2L)),
    more = "nomo_table(x, \"models\")"
  )
  nomo_present_section("Information criteria")
  nomo_present_table(
    x$models,
    c("Model" = "model", "AIC" = "aic", "BIC" = "bic",
      "Loadings fixed to zero" = "fixed_zero_loadings"),
    formats = list(aic = one_decimal, bic = one_decimal),
    more = "nomo_table(x, \"models\")"
  )

  if (nrow(x$comparisons)) {
    cmp <- x$comparisons
    cmp$relation_label <- nomo_compare_relation_label(cmp$relation)
    signed <- function(v) nomo_present_signed(v)
    nomo_present_section("Difference tests against the reference model")
    nomo_present_table(
      cmp,
      c("Model" = "model", "Relation" = "relation_label", "Check" = "nesting_check",
        "Method" = "method", "Chi-sq diff" = "chisq_diff", "df" = "df_diff",
        "p" = "p_value"),
      formats = list(chisq_diff = function(v) nomo_present_number(v, 2L),
                     df_diff = function(v) format(v, trim = TRUE),
                     p_value = nomo_present_p),
      more = "nomo_table(x, \"comparisons\")"
    )
    nomo_present_section("Changes in fit (model minus reference)")
    nomo_present_table(
      cmp,
      c("Model" = "model", "CFI" = "delta_cfi", "TLI" = "delta_tli",
        "RMSEA" = "delta_rmsea", "SRMR" = "delta_srmr", "AIC" = "delta_aic",
        "BIC" = "delta_bic"),
      formats = list(delta_cfi = signed, delta_tli = signed, delta_rmsea = signed,
                     delta_srmr = signed,
                     delta_aic = function(v) nomo_present_signed(v, 1L),
                     delta_bic = function(v) nomo_present_signed(v, 1L)),
      more = "nomo_table(x, \"comparisons\")"
    )

    nomo_present_section("Interpretation")
    nomo_present_bullets(x$comparisons$interpretation)
  }

  if (nrow(x$loadings)) {
    nomo_present_section("Standardized loadings by model")
    models <- setdiff(names(x$loadings), c("factor", "item"))
    nomo_present_table(
      x$loadings,
      c("Factor" = "factor", "Item" = "item", stats::setNames(models, models)),
      more = "nomo_table(x, \"loadings\")"
    )
  }

  if (nrow(x$evidence)) {
    nomo_present_section("Measurement evidence by model")
    ev <- x$evidence
    wide <- unique(ev[, c("construct", "metric"), drop = FALSE])
    key <- paste(wide$construct, wide$metric, sep = "\r")
    models <- unique(ev$model)
    columns <- c("Construct" = "construct", "Metric" = "metric")
    for (i in seq_along(models)) {
      rows <- ev[ev$model == models[[i]], , drop = FALSE]
      column <- paste0(".model_", i)
      wide[[column]] <- rows$estimate[match(key, paste(rows$construct, rows$metric, sep = "\r"))]
      columns <- c(columns, stats::setNames(column, models[[i]]))
    }
    nomo_present_table(wide, columns, more = "nomo_table(x, \"evidence\")")
    notes <- unique(ev$note[nzchar(ev$note)])
    if (length(notes)) nomo_present_bullets(notes)
  }

  cat("\n")
  nomo_present_text(
    "No model was selected automatically. Difference tests, changes in fit, ",
    "information criteria, and measurement evidence answer different questions; ",
    "read them together with theory and the recorded rationale."
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
        nomo_plot_labs(
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
    nomo_plot_labs(
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
