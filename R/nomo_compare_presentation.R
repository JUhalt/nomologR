# Presentation methods for nomo_compare -----------------------------------------
#
# print() gives one line per comparison with the reference model and the flags;
# summary() gives the tables behind them, the interpretations, and the
# measurement evidence of each model.

nomo_compare_relation_label <- function(relation) {
  labels <- c(
    more_constrained = "nested, more constrained",
    less_constrained = "nested, less constrained",
    equivalent = "equivalent",
    non_nested = "not nested",
    undetermined = "nesting not determined",
    different_variables = "different observed variables"
  )
  out <- unname(labels[relation])
  out[is.na(out)] <- relation[is.na(out)]
  out
}


# The models, the reference, the estimator, the cases, and the origin.
nomo_compare_present_facts <- function(x, data_n, summary = FALSE) {
  nomo_present_facts(c(
    if (summary) "" else sprintf("Models: %d", nrow(x$models)),
    sprintf("Reference: %s", x$reference),
    sprintf("Estimator: %s", x$estimator),
    nomo_cfa_cases_fact(x$models$n_used[[1L]], data_n),
    sprintf("Origin: %s", nomo_present_origin(x$origin))
  ))
}


# The flagged rows of the decision log: a post hoc origin, an improper
# solution, and a comparison whose test could not be reported. A comparison is
# explained by its note, which says why, rather than by its whole
# interpretation.
nomo_compare_flagged <- function(x, recommendation = FALSE) {
  log <- x$decision_log[x$decision_log$severity %in% c("review", "concern"), , drop = FALSE]
  text <- log$observation
  rows <- log$metric == "model_comparison"
  note <- x$comparisons$test_note[match(log$object[rows], x$comparisons$model)]
  text[rows] <- ifelse(nzchar(note), note, text[rows])
  if (isTRUE(recommendation)) text <- nomo_compare_then_recommend(text, log$recommendation)
  nomo_present_flagged(
    unit = ifelse(log$metric == "comparison_origin", "Origin", log$object),
    status = log$severity, text = text
  )
}


# An observation followed by its recommendation, as a summary's "Flagged"
# section gives them. The observation is ended with a full stop first, since a
# lavaan message kept as an observation or a note has none, and the shared
# helper adds one only at the very end (#145).
nomo_compare_then_recommend <- function(observation, recommendation) {
  observation <- trimws(observation)
  ended <- !nzchar(observation) | grepl("[.!?]$", observation)
  paste(ifelse(ended, observation, paste0(observation, ".")), recommendation)
}


# One comparison in a line: the difference test, the changes in CFI and RMSEA,
# and the change in AIC.
nomo_compare_line <- function(cmp) {
  test <- if (isTRUE(cmp$test_available)) {
    paste0(nomo_present_chisq(cmp$chisq_diff, cmp$df_diff, cmp$p_value, delta = TRUE),
           nomo_compare_method_note(cmp$method))
  } else {
    "no difference test"
  }
  fit <- if (is.finite(cmp$delta_cfi)) {
    sprintf("CFI change %s, RMSEA change %s",
            nomo_present_stat(cmp$delta_cfi, "fit_bounded", signed = TRUE),
            nomo_present_stat(cmp$delta_rmsea, "fit", signed = TRUE))
  } else {
    "change in fit unavailable"
  }
  ic <- if (isTRUE(cmp$ic_available)) {
    sprintf("AIC change %s", nomo_present_stat(cmp$delta_aic, "ic", signed = TRUE))
  } else {
    NULL
  }
  sprintf("%s (%s): %s", cmp$model, nomo_compare_relation_label(cmp$relation),
          paste(c(test, fit, ic), collapse = "; "))
}


#' @export
print.nomo_compare <- function(x, ...) {
  nomo_present_header("nomo_compare", "Measurement-model comparison")
  nomo_compare_present_facts(x, x$fits[[x$reference]]$data_n)
  nomo_present_text("Rationale: ", x$rationale)
  shown <- x$estimator

  if (nrow(x$comparisons)) {
    nomo_present_section(sprintf("Compared with %s", x$reference))
    nomo_present_bullets(vapply(seq_len(nrow(x$comparisons)), function(i) {
      nomo_compare_line(x$comparisons[i, , drop = FALSE])
    }, character(1)))
    shown <- c(shown, "CFI", "RMSEA", if (any(x$comparisons$ic_available)) "AIC")
  }

  nomo_compare_flagged(x)
  nomo_cfa_present_key_note(shown)
  cat("\n")
  nomo_present_text(
    "No model was selected automatically; read the comparison with theory and ",
    "the recorded rationale."
  )
  nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"comparisons\")"),
    c("the interpretations and measurement evidence", "every test")
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
#'   measurement evidence. Printing it shows these as tables, with each flag
#'   and its recommendation.
#' @export
summary.nomo_compare <- function(object, ...) {
  models <- object$models
  # The p value of each model's own chi-square, from its CFA.
  models$pvalue <- vapply(models$model, function(nm) {
    fe <- object$fits[[nm]]$fit_evidence
    fe$value[fe$metric == "p_value"][1L]
  }, numeric(1))
  reference <- object$fits[[object$reference]]
  out <- list(
    rationale = object$rationale,
    origin = object$origin,
    reference = object$reference,
    estimator = object$estimator,
    data_n = reference$data_n,
    version_note = nomo_cfa_versions_note(nomo_cfa_variants(reference$fit_evidence),
                                          nomo_cfa_test_label(reference$fit)),
    models = models,
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
  nomo_compare_present_facts(x, x$data_n, summary = TRUE)
  formats <- nomo_cfa_fit_formats()
  shown <- c(x$estimator, "CFI", "TLI", "RMSEA", "SRMR", "df")

  nomo_present_section("Model fit")
  nomo_present_table(
    x$models,
    c("Model" = "model", "Chi-square" = "chisq", "df" = "df", "p" = "pvalue",
      "CFI" = "cfi", "TLI" = "tli", "RMSEA" = "rmsea", "SRMR" = "srmr",
      "Parameters" = "npar"),
    formats = formats,
    more = "nomo_table(x, \"models\")"
  )
  if (nzchar(x$version_note)) nomo_present_text(x$version_note, indent = 2L)

  nomo_present_section("Information criteria")
  nomo_present_table(
    x$models,
    c("Model" = "model", "AIC" = "aic", "BIC" = "bic",
      "Loadings fixed to zero" = "fixed_zero_loadings"),
    formats = formats,
    more = "nomo_table(x, \"models\")"
  )
  if (any(is.finite(x$models$aic))) shown <- c(shown, "AIC", "BIC")

  if (nrow(x$comparisons)) {
    nomo_compare_present_tests(x$comparisons)
    nomo_present_section("Interpretation")
    nomo_present_bullets(x$comparisons$interpretation)
  }

  if (nrow(x$loadings)) {
    nomo_present_section("Standardized loadings by model")
    models <- setdiff(names(x$loadings), c("factor", "item"))
    loading <- function(v) nomo_present_stat(v, "loading")
    nomo_present_table(
      x$loadings,
      c("Factor" = "factor", "Indicator" = "item", stats::setNames(models, models)),
      formats = stats::setNames(rep(list(loading), length(models)), models),
      more = "nomo_table(x, \"loadings\")"
    )
  }

  if (nrow(x$evidence)) shown <- c(shown, nomo_compare_present_evidence(x$evidence))

  nomo_compare_flagged(x, recommendation = TRUE)
  nomo_present_key(nomo_cfa_key_entries(shown))
  cat("\n")
  nomo_present_text(
    "No model was selected automatically. Difference tests, changes in fit, ",
    "information criteria, and measurement evidence answer different questions; ",
    "read them together with theory and the recorded rationale."
  )
  nomo_present_pointer(
    c("nomo_table(x, \"decision_log\")", "x$fits"),
    c("the decision log", "each model's own analysis")
  )
  invisible(x)
}


# The difference tests, statistic, df, and p first so that they are the last
# columns dropped on a narrow console (#145), and the changes in fit.
nomo_compare_present_tests <- function(cmp) {
  cmp$relation_label <- nomo_compare_relation_label(cmp$relation)
  nomo_present_section("Difference tests against the reference model")
  nomo_present_table(
    cmp,
    c("Model" = "model", "Delta chi-square" = "chisq_diff", "df" = "df_diff",
      "p" = "p_value", "Relation" = "relation_label"),
    formats = list(chisq_diff = function(v) nomo_present_stat(v, "stat"),
                   df_diff = function(v) nomo_present_stat(v, "df"),
                   p_value = nomo_present_p),
    more = "nomo_table(x, \"comparisons\")"
  )
  methods <- unique(cmp$method[cmp$test_available])
  words <- vapply(methods, nomo_compare_method_words, character(1))
  if (any(nzchar(words))) {
    nomo_present_text("Method: ", nomo_present_or(unique(words[nzchar(words)]), "and"), ".",
                      indent = 2L)
  }

  signed <- function(kind) function(v) nomo_present_stat(v, kind, signed = TRUE)
  nomo_present_section("Changes in fit (model minus reference)")
  nomo_present_table(
    cmp,
    c("Model" = "model", "CFI" = "delta_cfi", "TLI" = "delta_tli",
      "RMSEA" = "delta_rmsea", "SRMR" = "delta_srmr", "AIC" = "delta_aic",
      "BIC" = "delta_bic"),
    formats = list(delta_cfi = signed("fit_bounded"), delta_tli = signed("fit"),
                   delta_rmsea = signed("fit"), delta_srmr = signed("fit"),
                   delta_aic = signed("ic"), delta_bic = signed("ic")),
    more = "nomo_table(x, \"comparisons\")"
  )
}


# The measurement evidence, one column per model. A model whose evidence is not
# available has no column, and its note, prefixed with its label, says why
# (#145). Returns the abbreviations shown.
nomo_compare_present_evidence <- function(ev) {
  nomo_present_section("Measurement evidence by model")
  available <- ev[!is.na(ev$construct), , drop = FALSE]
  wide <- unique(available[, c("construct", "metric"), drop = FALSE])
  key <- paste(wide$construct, wide$metric, sep = "\r")
  models <- unique(available$model)
  columns <- c("Construct" = "construct", "Metric" = "metric")
  formats <- list()
  for (i in seq_along(models)) {
    rows <- available[available$model == models[[i]], , drop = FALSE]
    column <- paste0(".model_", i)
    wide[[column]] <- rows$estimate[match(key, paste(rows$construct, rows$metric, sep = "\r"))]
    columns <- c(columns, stats::setNames(column, models[[i]]))
    formats[[column]] <- function(v) {
      ifelse(wide$metric == "HTMT2", nomo_present_stat(v, "htmt"),
             nomo_present_stat(v, "reliability"))
    }
  }
  # A pair is labeled with "vs.", as labels are (guide point 25).
  wide$construct <- sub(" vs ", " vs. ", wide$construct, fixed = TRUE)
  nomo_present_table(wide, columns, formats = formats, more = "nomo_table(x, \"evidence\")")
  # With no notes, the usual case, recycle0 gives no bullet rather than an
  # empty "- :".
  notes <- ev[nzchar(ev$note), , drop = FALSE]
  nomo_present_bullets(unique(paste0(notes$model, ": ", notes$note, recycle0 = TRUE)))
  intersect(c("AVE", "HTMT2"), wide$metric)
}


#' Plot a measurement-model comparison
#'
#' @param x A `nomo_compare` object.
#' @param type `"fit"` shows CFI, TLI, RMSEA, and SRMR for each model with the
#'   configured teaching references; `"loadings"` shows standardized loadings
#'   for each model side by side.
#' @param ... Unused.
#'
#' @return A `ggplot2` object. Models are told apart by color and by shapes
#'   that the package does not use for a status.
#' @export
plot.nomo_compare <- function(x, type = c("fit", "loadings"), ...) {
  type <- nomo_match_arg(type)
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
    named <- paste(ref_dat$metric, nomo_cfa_fit_number(ref_dat$metric, ref_dat$reference),
                   collapse = ", ")
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
          subtitle = paste0("Dashed lines are teaching references: ", named, "."),
          x = NULL,
          y = NULL,
          caption = "References are review prompts, not decision rules. No model is selected automatically."
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
  # Filled and open circles, filled squares, and crosses are the status shapes
  # (R/nomo_plot_helpers.R), so models take others: triangles, diamonds, and
  # their open forms.
  shapes <- rep_len(c(17, 18, 2, 5, 6, 25, 8, 3), length(model_cols))
  colours <- rep_len(c("#0072B2", "#009E73", "#CC79A7", "#56B4E9", "#000000", "#F0E442"),
                     length(model_cols))

  ggplot2::ggplot(dat, ggplot2::aes(x = loading, y = item, shape = model, colour = model)) +
    ggplot2::geom_vline(xintercept = x$guidance$cfa_loading_reference, linetype = 2) +
    ggplot2::geom_point(size = 3, position = ggplot2::position_dodge(width = 0.5)) +
    ggplot2::scale_shape_manual(values = stats::setNames(shapes, model_cols)) +
    ggplot2::scale_colour_manual(values = stats::setNames(colours, model_cols)) +
    ggplot2::facet_wrap(stats::as.formula("~ factor"), scales = "free_y") +
    ggplot2::coord_cartesian(xlim = nomo_plot_x_limits(dat$loading)) +
    nomo_plot_labs(
      title = "Standardized loadings across compared models",
      subtitle = paste0(
        "Dashed line: teaching reference, ",
        nomo_present_stat(x$guidance$cfa_loading_reference, "loading"), "."
      ),
      x = "Standardized loading",
      y = NULL,
      shape = "Model",
      colour = "Model",
      caption = "Loadings fixed to zero appear at 0."
    ) +
    ggplot2::theme_minimal()
}

utils::globalVariables(c("value", "model", "metric", "reference", "loading", "item"))
