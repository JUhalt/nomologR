# Which registry methods a result object actually used ------------------------
#
# Every rule below reads a field the component already records, so a method is
# credited only when the object shows it ran. Nothing here infers a method from
# the stage alone: a run that skipped a criterion, suppressed alpha, or never
# requested Fornell-Larcker does not get credited with it, because the report's
# methods section has to describe what happened rather than what usually
# happens.

nomo_methods_used <- function(x, ...) {
  UseMethod("nomo_methods_used")
}


#' @export
nomo_methods_used.default <- function(x, ...) {
  stop(
    "`nomo_methods()` does not know how to read methods from an object of ",
    "class ", paste(class(x), collapse = "/"), ".",
    call. = FALSE
  )
}


# Screening -------------------------------------------------------------------

#' @export
nomo_methods_used.nomo_screen <- function(x, ...) {
  used <- c("missingness_audit", "response_distribution_audit")

  # Item-rest correlations exist only when enough items were scored to compute
  # a rest score; a single-item scale produces the audit but not the statistic.
  relationships <- x$relationship_summary
  if (is.data.frame(relationships) &&
      "corrected_item_rest_r" %in% names(relationships) &&
      any(is.finite(relationships$corrected_item_rest_r))) {
    used <- c(used, "item_rest_correlation")
    if (!is.null(x$guidance$item_total_reference)) {
      used <- c(used, "item_total_reference")
    }
  }

  summary_tbl <- x$item_summary
  if (is.data.frame(summary_tbl) && "near_zero_variance" %in% names(summary_tbl)) {
    used <- c(used, "near_zero_variance")
  }

  used
}


# Factor retention ------------------------------------------------------------

# criterion_status$criterion uses the same vocabulary as the registry ids for
# every criterion except the legacy rule, which the object calls "kaiser".
nomo_methods_factor_criterion_ids <- function() {
  c(
    parallel = "parallel_analysis",
    map_original = "map_original",
    map_revised = "map_revised",
    ekc = "ekc",
    nest = "nest",
    hull = "hull",
    comparison_data = "comparison_data",
    kaiser = "kaiser_guttman"
  )
}


#' @export
nomo_methods_used.nomo_factors <- function(x, ...) {
  used <- character()

  status <- x$criterion_status
  if (is.data.frame(status) && nrow(status)) {
    # A criterion whose assumptions did not hold is recorded as "skipped". It
    # was not used, so it is not cited.
    ran <- status$criterion[status$status == "available"]
    lookup <- nomo_methods_factor_criterion_ids()
    used <- c(used, unname(lookup[intersect(ran, names(lookup))]))
  }

  if (!is.null(x$scree)) used <- c(used, "scree")
  if (!is.null(x$kmo)) used <- c(used, "kmo")
  if (!is.null(x$bartlett)) used <- c(used, "bartlett")

  if (!is.null(x$correlation) &&
      x$correlation %in% c("polychoric", "tetrachoric", "mixed")) {
    used <- c(used, "categorical_correlations")
  }

  used
}


# Exploratory structure -------------------------------------------------------

#' @export
nomo_methods_used.nomo_efa <- function(x, ...) {
  used <- character()

  if (identical(x$fm, "minres")) used <- c(used, "minres_extraction")

  # A one-factor solution is not rotated at all, and `oblique` is then FALSE
  # because no factor correlation matrix was produced -- not because an
  # orthogonal rotation was chosen. Crediting a rotation method here would
  # describe a choice the researcher never made, so both are withheld unless a
  # rotation actually ran.
  rotated <- is.numeric(x$n_factors) && length(x$n_factors) == 1L &&
    !is.na(x$n_factors) && x$n_factors >= 2L
  if (rotated) {
    if (isTRUE(x$oblique)) {
      used <- c(used, "oblique_rotation")
    } else {
      used <- c(used, "orthogonal_rotation")
    }
  }

  if (is.data.frame(x$item_summary) && nrow(x$item_summary)) {
    used <- c(used, "loading_diagnostics")
    if (!is.null(x$guidance$efa_loading_reference)) {
      used <- c(used, "loading_reference")
    }
  }

  if (!is.null(x$correlation) &&
      x$correlation %in% c("polychoric", "tetrachoric", "mixed")) {
    used <- c(used, "categorical_correlations")
  }

  used
}


# Confirmatory measurement model ----------------------------------------------

#' @export
nomo_methods_used.nomo_cfa <- function(x, ...) {
  used <- character()

  ordered_used <- length(x$ordered) > 0L
  if (ordered_used) {
    used <- c(used, "wlsmv_cfa", "categorical_correlations")
  } else {
    used <- c(used, "ml_cfa")
  }

  fit <- x$fit_evidence
  if (is.data.frame(fit) && nrow(fit)) {
    metrics <- fit$metric
    if (any(c("chi_square", "p_value") %in% metrics)) {
      used <- c(used, "chisq_exact_fit")
    }
    if (any(c("CFI", "TLI") %in% metrics)) used <- c(used, "incremental_fit")
    if ("RMSEA" %in% metrics) used <- c(used, "rmsea_interval")
    if ("SRMR" %in% metrics) used <- c(used, "srmr")
    if (any(is.finite(fit$reference))) used <- c(used, "fit_cutoffs")
  }

  if (is.data.frame(x$residual_pairs) && nrow(x$residual_pairs)) {
    used <- c(used, "local_strain")
  }

  # Modification indices are computed only on request, and crediting them
  # matters: their presence is exactly what a reader should be able to see.
  if (isTRUE(x$modification_indices_requested)) {
    used <- c(used, "modification_indices")
  }

  if (!is.null(x$heywood)) used <- c(used, "improper_solutions")

  if (nomo_methods_is_fiml(x$missing)) used <- c(used, "fiml")

  structure <- nomo_methods_cfa_structure(x)
  if (identical(structure, "bifactor")) used <- c(used, "bifactor_model")
  if (identical(structure, "higher_order")) used <- c(used, "higher_order_model")

  used
}


# The structure is read from the fitted model because nomo_cfa() keeps model
# syntax as plain text. Anything that is not a recognizable bifactor or
# higher-order model is treated as ordinary first-order CFA.
nomo_methods_cfa_structure <- function(x) {
  if (is.null(x$fit)) return(NA_character_)
  tryCatch(
    {
      input <- nomo_hierarchical_input(x$fit)
      nomo_hierarchical_structure(input)$type
    },
    error = function(e) NA_character_
  )
}


#' @export
nomo_methods_used.nomo_hierarchical <- function(x, ...) {
  used <- c(
    if (identical(x$structure, "bifactor")) "bifactor_model" else "higher_order_model",
    "omega_hierarchical",
    "ecv",
    "puc"
  )
  if (is.data.frame(x$subscales) && nrow(x$subscales)) {
    used <- c(used, "omega_hierarchical_subscale")
  }

  # Credited from the values actually produced. A run whose reproduced matrix
  # was singular, or whose loadings made H undefined, reports NA and must not
  # be cited as having used the index.
  factors <- x$factors
  if (is.data.frame(factors) && nrow(factors)) {
    if (any(is.finite(factors$factor_determinacy))) {
      used <- c(used, "factor_determinacy")
    }
    if (any(is.finite(factors$construct_replicability))) {
      used <- c(used, "construct_replicability")
    }
  }

  if (identical(x$structure, "higher_order")) used <- c(used, "schmid_leiman")
  used
}


# Model comparison ------------------------------------------------------------

#' @export
nomo_methods_used.nomo_compare <- function(x, ...) {
  used <- character()

  comparisons <- x$comparisons
  if (is.data.frame(comparisons) && nrow(comparisons)) {
    # A difference test is credited only when it produced a result, and by
    # lavaan's own method value. Matching on text such as "satorra" would
    # credit the Satorra-Bentler scaled test for WLSMV comparisons, whose
    # scaled-and-shifted test lavaan labels "satorra.2000".
    if (all(c("method", "test_available") %in% names(comparisons))) {
      ran <- comparisons$method[
        !is.na(comparisons$method) & comparisons$test_available %in% TRUE
      ]
      lookup <- nomo_methods_difference_test_ids()
      used <- c(used, unname(lookup[intersect(unique(ran), names(lookup))]))
    }
    if (any(grepl("^delta_", names(comparisons)))) {
      used <- c(used, "delta_fit")
    }
    # AIC and BIC are undefined for some estimators, such as WLSMV, and are
    # then reported as unavailable rather than computed.
    if ("ic_available" %in% names(comparisons) &&
        any(comparisons$ic_available %in% TRUE)) {
      used <- c(used, "information_criteria")
    }
    if ("nesting_check" %in% names(comparisons) &&
        any(comparisons$nesting_check %in% c("nested", "equivalent", "not_nested"))) {
      used <- c(used, "nesting_check")
    }
  }

  used
}


# lavaan::lavTestLRT() method values mapped to registry ids.
nomo_methods_difference_test_ids <- function() {
  c(
    standard = "lrt_standard",
    default = "lrt_standard",
    satorra.bentler.2001 = "lrt_scaled",
    satorra.bentler.2010 = "lrt_scaled",
    satorra.2000 = "lrt_scaled_shifted"
  )
}


# lavaan treats "ml", "fiml", and "direct" (and their ".x" forms) as full
# information maximum likelihood, so crediting only the spelling "fiml" would
# miss researchers who wrote missing = "ml".
nomo_methods_is_fiml <- function(missing) {
  length(missing) == 1L && !is.na(missing) &&
    tolower(missing) %in% c("ml", "fiml", "direct", "ml.x", "fiml.x")
}


# Reliability -----------------------------------------------------------------

#' @export
nomo_methods_used.nomo_reliability <- function(x, ...) {
  used <- character()

  if (is.data.frame(x$omega) && nrow(x$omega)) used <- c(used, "omega")

  # Alpha can be requested and still be unavailable, for instance when the
  # estimand is an ordered-score scale. Only a computed estimate is cited.
  if (is.data.frame(x$alpha) && nrow(x$alpha) &&
      any(is.finite(x$alpha$estimate))) {
    used <- c(used, "alpha")
  }

  if (isTRUE(x$ordinal_scale) && length(x$ordered)) {
    used <- c(used, "omega_ordinal_scale")
  }

  status <- x$ci_status
  if (is.data.frame(status) && nrow(status) &&
      any(status$method == "bootstrap" & status$available)) {
    used <- c(used, "reliability_bootstrap_ci")
  }

  used
}


# Convergent and discriminant evidence ----------------------------------------

#' @export
nomo_methods_used.nomo_validity <- function(x, ...) {
  used <- character()

  if (is.data.frame(x$ave) && nrow(x$ave)) {
    used <- c(used, "standardized_loadings_ave")
  }

  status <- x$htmt_status
  if (is.data.frame(status) && nrow(status)) {
    available <- status$method[status$available]
    if ("HTMT2" %in% available) used <- c(used, "htmt2")
    if ("HTMT" %in% available) used <- c(used, "htmt")
  }

  # The comparison is produced only on request and is stored as a matrix.
  if (isTRUE(x$fornell_larcker_requested) && NROW(x$fornell_larcker) > 0L) {
    used <- c(used, "fornell_larcker")
  }

  if (is.data.frame(x$latent_correlations) && nrow(x$latent_correlations)) {
    used <- c(used, "latent_correlation_ci")
  }

  used
}


# Measurement invariance ------------------------------------------------------

#' @export
nomo_methods_used.nomo_invariance <- function(x, ...) {
  used <- c("multigroup_cfa")

  ordered_used <- length(x$ordered) > 0L
  if (ordered_used) {
    used <- c(used, "categorical_invariance", "categorical_correlations")
  } else {
    used <- c(used, "invariance_hierarchy")
  }

  fit <- x$fit_evidence
  if (is.data.frame(fit) && nrow(fit) > 1L && any(grepl("^delta_", names(fit)))) {
    used <- c(used, "invariance_delta_fit")
  }

  if (isTRUE(x$localize) &&
      (is.data.frame(x$score_diagnostics) || is.data.frame(x$local_strain))) {
    used <- c(used, "score_diagnostics")
  }

  if (!is.null(x$partial)) used <- c(used, "partial_invariance")

  used
}


# nomo_partial() specifies researcher releases; it fits nothing, so the
# multiple-group model is credited to the nomo_invariance() fit that uses it.
#' @export
nomo_methods_used.nomo_partial <- function(x, ...) {
  "partial_invariance"
}


# Nomological network ---------------------------------------------------------

#' @export
nomo_methods_used.nomo_network <- function(x, ...) {
  used <- c("nomological_network", "two_step_sem")

  hypotheses <- x$hypotheses
  if (inherits(hypotheses, "nomo_hypotheses")) {
    hypotheses <- hypotheses$hypotheses
  }
  if (is.data.frame(hypotheses) && nrow(hypotheses)) {
    if ("prediction" %in% names(hypotheses) &&
        any(hypotheses$prediction == "negligible", na.rm = TRUE)) {
      used <- c(used, "equivalence_testing")
    }
    if ("confirmatory_status" %in% names(hypotheses) ||
        "origin" %in% names(hypotheses)) {
      used <- c(used, "prediction_provenance")
    }
  }

  if (is.data.frame(x$replication_evidence) && nrow(x$replication_evidence)) {
    used <- c(used, "replication_same_model")
  }

  if (nomo_methods_is_fiml(x$missing)) used <- c(used, "fiml")

  used
}


#' @export
nomo_methods_used.nomo_hypotheses <- function(x, ...) {
  used <- "nomological_network"

  hypotheses <- x$hypotheses
  if (is.data.frame(hypotheses) && nrow(hypotheses)) {
    if ("prediction" %in% names(hypotheses) &&
        any(hypotheses$prediction == "negligible", na.rm = TRUE)) {
      used <- c(used, "equivalence_testing")
    }
    used <- c(used, "prediction_provenance")
  }

  used
}


# Sample design ---------------------------------------------------------------

#' @export
nomo_methods_used.nomo_split <- function(x, ...) {
  "holdout_split"
}


# The guided workflow ---------------------------------------------------------

# A run's methods are the union of its components' methods plus the workflow
# methods the run itself provides. Components are stored either as single
# objects or, for the per-scale stages, as named lists of them.
nomo_methods_used_component <- function(component) {
  if (is.null(component)) return(character())

  if (inherits(component, "nomo_screen") ||
      inherits(component, "nomo_factors") ||
      inherits(component, "nomo_efa") ||
      inherits(component, "nomo_cfa") ||
      inherits(component, "nomo_reliability") ||
      inherits(component, "nomo_validity") ||
      inherits(component, "nomo_invariance") ||
      inherits(component, "nomo_network") ||
      inherits(component, "nomo_compare") ||
      inherits(component, "nomo_partial")) {
    return(nomo_methods_used(component))
  }

  # Per-scale stages: a named list of component objects, one per scale.
  if (is.list(component)) {
    return(unlist(
      lapply(component, nomo_methods_used_component),
      use.names = FALSE
    ))
  }

  character()
}


#' @export
nomo_methods_used.nomo_run <- function(x, ...) {
  used <- c("staged_workflow", "decision_log")

  for (component in x$results) {
    used <- c(used, nomo_methods_used_component(component))
  }

  # Only a genuine calibration/validation design counts. "same_sample" is the
  # ordinary case and must not be credited as a holdout.
  if (identical(x$sample_design, "calibration_validation")) {
    used <- c(used, "holdout_split")
  }

  lineage <- nomo_run_lineage(x)
  if (is.data.frame(lineage) && nrow(lineage)) {
    used <- c(used, "revision_lineage")
  }

  if (!is.null(x$revision_comparison)) {
    used <- c(used, nomo_methods_used(x$revision_comparison))
  }

  unique(used)
}
