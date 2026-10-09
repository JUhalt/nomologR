# Report-ready evidence tables -------------------------------------------------

#' Extract report-ready evidence tables
#'
#' `nomo_table()` returns compact tibbles intended for manuscripts, audit
#' appendices, teaching materials, and [nomo_report()]. It never rounds away
#' the underlying object: full engine results remain stored in the original
#' `nomologR` object.
#'
#' @details
#' Supported objects and `type` values. The first value listed is the default.
#'
#' * `nomo_screen`: `"items"`, `"distribution"`, `"cases"`,
#'   `"relationships"`, `"effort"`, `"decision_log"`; see [nomo_screen()].
#' * `nomo_factors`: `"evidence"`, `"criteria"`, `"adequacy"`,
#'   `"concordance"`, `"decision_log"`; see [nomo_factors()].
#' * `nomo_efa`: `"items"`, `"pattern"`, `"factor_correlations"`,
#'   `"residuals"`, `"decision_log"`. The pattern and factor-correlation
#'   matrices are returned as tables with the item or factor in the first
#'   column; see [nomo_efa()].
#' * `nomo_cfa`: `"fit"`, `"loadings"`, `"factor_correlations"`, `"heywood"`,
#'   `"residuals"`, `"modification_indices"`, `"decision_log"`; see
#'   [nomo_cfa()].
#' * `nomo_reliability`: `"coefficients"`, `"alpha_status"`, `"ci_status"`,
#'   `"decision_log"`; see [nomo_reliability()].
#' * `nomo_validity`: `"convergent"`, `"discriminant"`, `"htmt_status"`,
#'   `"decision_log"`. `"discriminant"` has one row per construct pair, with
#'   the latent correlation and the HTMT-family values together; see
#'   [nomo_validity()].
#' * `nomo_scores`: `"scores"`, `"diagnostics"`, `"unit_weighting"`,
#'   `"notes"`; see [nomo_scores()].
#' * `nomo_power`: `"power"` from [nomo_power_rmsea()]; `"summary"` (default)
#'   and `"parameters"` from [nomo_power_simulate()].
#' * `nomo_hypotheses`: the machine-readable hypothesis table (no `type`;
#'   any `type` given is an error).
#' * `nomo_network`: `"hypotheses"` (default), `"fit"`, `"measurement"`,
#'   `"relations"`, `"replication"`, `"single_indicators"`, `"sensitivity"`,
#'   `"decision_log"`. `"single_indicators"` and `"sensitivity"` describe the
#'   composites modeled as single indicators; see [nomo_network()].
#' * `nomo_retest`: `"icc"` (default), `"reliable_change"`, `"decision_log"`;
#'   see [nomo_retest()].
#' * `nomo_esem`: `"loadings"` (default), `"factor_correlations"`,
#'   `"models"`, `"comparisons"`, `"decision_log"`; see [nomo_esem()].
#' * `nomo_method_variance`: `"comparisons"` (default), `"models"`,
#'   `"loadings"`, `"reliability"`, `"correlations"`, `"decision_log"`; see
#'   [nomo_method_variance()].
#' * `nomo_invariance` and `nomo_invariance_longitudinal`: `"fit"` (default),
#'   `"categories"`, `"partial"`,
#'   `"local_strain"`, `"latent_means"`, `"decision_log"`. The local-strain table keeps lavaan's
#'   internal `constraint` label and adds a human-readable
#'   `constraint_display` column (for example, `Intercept: ag3 (online vs.
#'   paper)`).
#' * `nomo_compare`: `"comparisons"` (default), `"models"`, `"loadings"`,
#'   `"evidence"`, `"decision_log"`; see [nomo_compare()].
#' * `nomo_missing`: `"strategies"` (default), `"estimates"`, `"fit"`,
#'   `"reliability"`, `"pattern"`, `"variables"`, `"decision_log"`; see
#'   [nomo_missing()].
#' * `nomo_hierarchical`: `"indices"` (default), `"subscales"`, `"factors"`,
#'   `"loadings"`, `"notes"`, `"decision_log"`; see [nomo_hierarchical()].
#' * `nomo_run`: `"stages"` (default), `"requests"`, `"decisions"`,
#'   `"component_log"`, `"scales"`, `"recipe"`, `"settings"`, `"lineage"`,
#'   `"methods"`; see [nomo_run()] and [nomo_revise()]. `"lineage"` returns one
#'   row per revision, or no rows for a workflow that was never revised.
#'   `"methods"` returns the registry entries for the methods the run actually
#'   used, and is equivalent to `nomo_methods(x)`; see [nomo_methods()].
#'
#' @section Fit tables:
#' Fit tables name their indices in one of two ways, a known difference that
#' is kept as it is (see **Conventions in returned tables** in `?nomologR`).
#' The `nomo_cfa` and `nomo_missing` tables spell them `chi_square`,
#' `p_value`, `CFI`, and so on. The others use the names of
#' [lavaan::fitMeasures()]: `chisq`, `pvalue`, `cfi`, and so on.
#'
#' * `nomo_cfa`, `"fit"`: long by design, one row per index, named in
#'   `metric` (`chi_square`, `df`, `p_value`, `CFI`, `TLI`, `RMSEA`,
#'   `RMSEA_CI_lower`, `RMSEA_CI_upper`, `SRMR`), with its `value`,
#'   `reference`, and `attention` flag.
#' * `nomo_missing`, `"fit"`: one row per `strategy`, with `chi_square`, `df`,
#'   `p_value`, `CFI`, `TLI`, `RMSEA`, and `SRMR`.
#' * `nomo_esem`, `"models"`: one row per `model`, with `chisq`, `df`,
#'   `pvalue`, `cfi`, `tli`, `rmsea`, `srmr`, `aic`, and `bic`.
#' * `nomo_method_variance`, `"models"`: one row per `model`, with `chisq`,
#'   `df`, `pvalue`, `cfi`, `tli`, `rmsea`, and `srmr`.
#' * `nomo_compare`, `"models"`: one row per `model`, with `npar`, `df`,
#'   `chisq`, `cfi`, `tli`, `rmsea`, `srmr`, `aic`, and `bic`. The difference
#'   tests and their p values are in `"comparisons"`.
#' * `nomo_invariance`, `"fit"`: one row per `level`, with `chisq`, `df`,
#'   `pvalue`, `cfi`, `rmsea`, and `srmr`, the changes from the level before
#'   (`delta_cfi`, `delta_rmsea`, `delta_srmr`), and the likelihood-ratio test
#'   against it (`lrt_chisq`, `lrt_df`, `lrt_p`).
#' * `nomo_network`, `"fit"`: one row, with `chisq`, `df`, `pvalue`, `cfi`,
#'   `tli`, `rmsea`, and `srmr`.
#'
#' @param x A supported `nomologR` result object. An object of any other class
#'   is refused with the list of supported classes.
#' @param ... Additional arguments passed to methods, usually `type`.
#'
#' @return A tibble. A table the object has no rows for, such as `"partial"`
#'   for an invariance analysis without releases or `"replication"` for a
#'   network without validation data, has no rows and the table's usual
#'   columns.
#' @export
#'
#' @examples
#' h <- nomo_hypotheses(
#'   "Agency -> Persistence" = positive(min = .20),
#'   "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15))
#' )
#' nomo_table(h)
#'
#' inv <- nomo_invariance(
#'   "Agency =~ ag1 + ag2 + ag3 + ag4",
#'   data = nomo_demo_network,
#'   group = "group",
#'   levels = c("configural", "metric", "scalar")
#' )
#' nomo_table(inv, "fit")
#' head(nomo_table(inv, "local_strain"))
nomo_table <- function(x, ...) {
  UseMethod("nomo_table")
}


#' @export
nomo_table.default <- function(x, ...) {
  # The supported classes are read from the methods, so the list never goes
  # stale (#145).
  supported <- sub("nomo_table.", "", fixed = TRUE,
                   ls(asNamespace("nomologR"), pattern = "^nomo_table[.]nomo_"))
  stop(sprintf(
    "No nomo_table() is available for an object of class `%s`. Supported: %s.",
    class(x)[[1L]], paste(supported, collapse = ", ")
  ), call. = FALSE)
}


# A hypothesis set has one table, so a `type` given is refused rather than
# ignored (#145).
#' @export
nomo_table.nomo_hypotheses <- function(x, type = NULL, ...) {
  if (!is.null(type)) {
    stop(sprintf(
      "`type` must be NULL for a `nomo_hypotheses` object, which has one table, not %s.",
      paste(sprintf('"%s"', as.character(type)), collapse = ", ")
    ), call. = FALSE)
  }
  x$hypotheses
}


#' @export
nomo_table.nomo_network <- function(
    x,
    type = c(
      "hypotheses",
      "fit",
      "measurement",
      "relations",
      "replication",
      "single_indicators",
      "sensitivity",
      "decision_log"
    ),
    ...) {
  type <- nomo_match_arg(type)

  if (type == "hypotheses") {
    keep <- c(
      "id", "relation", "prediction", "theoretical_region", "scale",
      "estimate", "se", "ci_lower", "ci_upper", "p_value",
      "equivalence_ci_lower", "equivalence_ci_upper",
      "equivalence_supported", "concordance", "confirmatory_status",
      "evidence_scope", "measurement_attention"
    )
    return(x$hypothesis_evidence[, keep, drop = FALSE])
  }

  if (type == "fit") return(x$fit_evidence)

  if (type == "measurement") {
    return(x$measurement_context$summary)
  }

  if (type == "relations") return(x$model_relations)

  if (type == "replication") {
    # Without validation data the table has no rows but keeps its columns, so
    # code that selects or binds them still works (#145).
    if (!NROW(x$replication_evidence)) {
      return(tibble::tibble(
        id = character(), relation = character(), prediction = character(),
        primary_estimate = numeric(), validation_estimate = numeric(),
        estimate_shift = numeric(), primary_concordance = character(),
        validation_concordance = character(), replication_status = character(),
        interpretation = character()
      ))
    }
    return(x$replication_evidence)
  }

  if (type == "single_indicators") return(x[["single_indicators"]])
  if (type == "sensitivity") return(x[["single_indicator_sensitivity"]])

  x$decision_log
}


#' @export
nomo_table.nomo_retest <- function(
    x,
    type = c("icc", "reliable_change", "decision_log"),
    ...) {
  type <- nomo_match_arg(type)
  x[[type]]
}


#' @export
nomo_table.nomo_esem <- function(
    x,
    type = c("loadings", "factor_correlations", "models", "comparisons", "decision_log"),
    ...) {
  type <- nomo_match_arg(type)
  x[[type]]
}


#' @export
nomo_table.nomo_method_variance <- function(
    x,
    type = c("comparisons", "models", "loadings", "reliability", "correlations",
             "decision_log"),
    ...) {
  type <- nomo_match_arg(type)
  if (type == "loadings") return(x$method_loadings)
  x[[type]]
}


#' @export
nomo_table.nomo_invariance <- function(
    x,
    type = c(
      "fit",
      "categories",
      "partial",
      "local_strain",
      "latent_means",
      "decision_log"
    ),
    ...) {
  type <- nomo_match_arg(type)

  if (type == "fit") return(x$fit_evidence)
  if (type == "categories") return(x$ordered_categories)
  if (type == "latent_means") return(x[["latent_means"]])

  if (type == "partial") {
    # Without releases, the columns of nomo_partial()'s table with no rows.
    if (is.null(x$partial)) {
      return(tibble::tibble(release_id = character(), level = character(),
                            syntax = character(), rationale = character()))
    }
    return(x$partial$releases)
  }

  if (type == "local_strain") return(nomo_invariance_local_strain_display(x))

  x$decision_log
}


#' @export
nomo_table.nomo_compare <- function(
    x,
    type = c(
      "comparisons",
      "models",
      "loadings",
      "evidence",
      "decision_log"
    ),
    ...) {
  type <- nomo_match_arg(type)

  if (type == "comparisons") return(x$comparisons)
  if (type == "models") return(x$models)
  if (type == "loadings") return(x$loadings)
  if (type == "evidence") return(x$evidence)

  x$decision_log
}


# The measurement-stage objects expose the tables their report sections show,
# so nomo_table() and nomo_report() cannot drift apart (#89). A table an object
# does not hold comes back as an empty tibble.

#' @export
nomo_table.nomo_factors <- function(
    x,
    type = c("evidence", "criteria", "adequacy", "concordance", "decision_log"),
    ...) {
  type <- nomo_match_arg(type)
  s <- summary(x)
  tibble::as_tibble(switch(
    type,
    evidence = s$evidence,
    criteria = s$criterion_status,
    adequacy = s$adequacy,
    concordance = s$concordance,
    decision_log = s$decision_log
  ))
}


#' @export
nomo_table.nomo_efa <- function(
    x,
    type = c("items", "pattern", "factor_correlations", "residuals", "decision_log"),
    ...) {
  type <- nomo_match_arg(type)
  tibble::as_tibble(switch(
    type,
    items = x$item_summary,
    pattern = nomo_report_matrix_table(x$pattern_matrix, "item"),
    factor_correlations = nomo_report_matrix_table(x$factor_correlations, "factor"),
    residuals = x$residual_pairs,
    decision_log = x$decision_log
  ))
}


#' @export
nomo_table.nomo_cfa <- function(
    x,
    type = c(
      "fit", "loadings", "factor_correlations", "heywood", "residuals",
      "modification_indices", "decision_log"
    ),
    ...) {
  type <- nomo_match_arg(type)
  tibble::as_tibble(switch(
    type,
    fit = x$fit_evidence,
    loadings = x$standardized_loadings,
    factor_correlations = x$factor_correlations,
    heywood = x$heywood,
    residuals = x$residual_pairs,
    modification_indices = x$top_modification_indices,
    decision_log = x$decision_log
  ))
}


#' @export
nomo_table.nomo_reliability <- function(
    x,
    type = c("coefficients", "alpha_status", "ci_status", "decision_log"),
    ...) {
  type <- nomo_match_arg(type)
  s <- summary(x)
  tibble::as_tibble(switch(
    type,
    coefficients = s$table,
    alpha_status = s$alpha_status,
    ci_status = s$ci_status,
    decision_log = s$decision_log
  ))
}


#' @export
nomo_table.nomo_validity <- function(
    x,
    type = c("convergent", "discriminant", "htmt_status", "decision_log"),
    ...) {
  type <- nomo_match_arg(type)
  s <- summary(x)
  tibble::as_tibble(switch(
    type,
    convergent = s$convergent,
    discriminant = s$discriminant,
    htmt_status = s$htmt_status,
    decision_log = s$decision_log
  ))
}
