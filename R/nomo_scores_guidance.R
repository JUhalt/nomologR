# Notes ------------------------------------------------------------------------

nomo_scores_notes <- function(input, method, diagnostics, unit_weighting,
                              parallel) {
  notes <- tibble::tibble(
    topic = character(), severity = character(), note = character()
  )
  add <- function(notes, topic, severity, note) {
    tibble::add_row(notes, topic = topic, severity = severity, note = note)
  }

  unit <- method %in% c("sum", "mean")

  notes <- add(notes, "estimand", "info", paste(
    "A score is not the latent variable. Every method here produces an",
    "estimate whose correlation with its own factor is below one, reported as",
    "`validity` (Grice, 2001). Where a later question can be asked of the",
    "latent variables directly, asking it of scores replaces an unbiased",
    "answer with a biased one."
  ))

  if (unit) {
    notes <- add(notes, "unit_weighting", "info", paste(
      "Unit weighting is not a model-free calculation. McNeish and Wolf",
      "(2020) show that adding items assumes a parallel model: equal",
      "unstandardized loadings and equal residual variances. That assumption",
      "needs the same justification as any other measurement model."
    ))

    if (isTRUE(parallel$available) && is.finite(parallel$p_value)) {
      if (parallel$p_value < .05) {
        notes <- add(notes, "unit_weighting", "review", sprintf(
          paste(
            "The parallel model that unit weighting assumes fits worse than",
            "the model you fitted (chi-square difference %.2f on %s df, %s).",
            "The items are not interchangeable in the way adding them",
            "assumes. This does not forbid a sum score; it means the choice",
            "needs a reason beyond convenience, and that `validity` and",
            "`correlational_accuracy` describe what it costs."
          ),
          parallel$chisq_diff, format(parallel$df_diff, trim = TRUE),
          nomo_compare_format_p(parallel$p_value)
        ))
      } else {
        notes <- add(notes, "unit_weighting", "info", sprintf(
          paste(
            "The parallel model that unit weighting assumes does not fit",
            "worse than the model you fitted (chi-square difference %.2f on",
            "%s df, %s), so the constraints adding the items assumes are",
            "consistent with these data."
          ),
          parallel$chisq_diff, format(parallel$df_diff, trim = TRUE),
          nomo_compare_format_p(parallel$p_value)
        ))
      }
    } else if (nzchar(parallel$note)) {
      notes <- add(notes, "unit_weighting", "review", parallel$note)
    }

    notes <- add(notes, "reliability", "info", paste(
      "Coefficient alpha is the reliability coefficient for a unit-weighted",
      "scale; coefficient H belongs to optimally weighted scores (McNeish &",
      "Wolf, 2020). Reporting H for a sum score, or alpha for a weighted one,",
      "describes a scale that was not used."
    ))
  } else {
    notes <- add(notes, "indeterminacy", "info", paste(
      "Factor scores are indeterminate: infinitely many sets of scores are",
      "consistent with the same loadings (Grice, 2001). `validity` is the",
      "correlation between these scores and the factor they estimate, and",
      "Grice recommends that factor scores be evaluated before they are",
      "reported or used in later analyses, which is what this table is for."
    ))
  }

  low_validity <- diagnostics$factor[
    is.finite(diagnostics$validity) & diagnostics$validity < 0.90
  ]
  if (length(low_validity)) {
    notes <- add(notes, "validity", "review", paste0(
      "Validity is below .90 for ", paste(low_validity, collapse = ", "),
      ". Gorsuch (1983, p. 260) recommended at least .80, and above .90 if ",
      "the scores are to serve as adequate substitutes for the factors ",
      "themselves. Reported as his recommendation, not applied as a rule."
    ))
  }

  accurate <- is.finite(diagnostics$correlational_accuracy)
  if (any(accurate & abs(diagnostics$correlational_accuracy) >= 0.05)) {
    worst <- diagnostics[which.max(abs(diagnostics$correlational_accuracy)), ]
    notes <- add(notes, "correlational_accuracy", "concern", sprintf(
      paste(
        "Correlations among these scores do not reproduce the correlations",
        "among the factors: the largest discrepancy is %+.3f, for %s. A",
        "relationship estimated from these scores carries that much bias, and",
        "its direction is a property of the method and the model rather than a",
        "constant that can be corrected for. Where the question can be asked of",
        "the latent variables, ask it there. For a linear regression among",
        "factors, Skrondal and Laake (2001) showed a scoring design that gives",
        "consistent coefficients, and scores from one model containing every",
        "factor, like these, are not it: the predictors need regression-method",
        "scores and the outcome Bartlett scores, each from a measurement model",
        "of its own."
      ),
      worst$correlational_accuracy[[1L]], worst$factor[[1L]]
    ))
  }

  univocal <- is.finite(diagnostics$univocality)
  if (any(univocal & abs(diagnostics$univocality) >= 0.30)) {
    worst <- diagnostics[which.max(abs(diagnostics$univocality)), ]
    notes <- add(notes, "univocality", "review", sprintf(
      paste(
        "These scores also carry the other factors: the score for %s",
        "correlates %+.3f with a factor it does not represent (Grice, 2001). A",
        "score that is not univocal cannot be treated as though it measured its",
        "own factor alone."
      ),
      worst$factor[[1L]], worst$univocality[[1L]]
    ))
  }

  if (length(input$ordered)) {
    notes <- add(notes, "estimand", "review", sprintf(
      paste(
        "%d indicator(s) are ordered. These scores treat the latent-response",
        "variables as continuous; they are not the expected-a-posteriori scores",
        "an item-response model would produce, and the difference grows with",
        "fewer categories and more extreme thresholds."
      ),
      length(input$ordered)
    ))
  }

  if (length(input$cross_loaded)) {
    notes <- add(notes, "cross_loadings", "review", paste0(
      "Item(s) ", paste(input$cross_loaded, collapse = ", "),
      " load on more than one factor. A unit-weighted score assigns each such ",
      "item wholly to every factor it loads on, which counts it more than once; ",
      "a weighted score divides it according to the model."
    ))
  }

  heterogeneous <- unit_weighting$factor[
    is.finite(unit_weighting$loading_ratio) & unit_weighting$loading_ratio >= 2
  ]
  if (unit && length(heterogeneous)) {
    notes <- add(notes, "unit_weighting", "review", paste0(
      "The strongest standardized loading is at least twice the weakest for ",
      paste(heterogeneous, collapse = ", "),
      ". Adding those items gives the weakest indicator the same say as the ",
      "strongest, so two people with the same total can differ on the construct ",
      "by having endorsed different items."
    ))
  }

  notes
}


# User-facing ------------------------------------------------------------------

#' Score a measurement model, with the evidence for the scoring choice
#'
#' Computes scores from a fitted measurement model and reports what those
#' scores are and are not. Scoring is a modeling decision, and `nomo_scores()`
#' documents the decision rather than making it.
#'
#' @details
#' **Unit weighting is a model.** McNeish and Wolf (2020) show that adding
#' items is not a model-free arithmetic calculation but a *parallel* factor
#' model, assuming equal unstandardized loadings and equal residual variances.
#' For `method = "sum"` and `method = "mean"`, that constrained model is fitted
#' and compared with the model supplied, so a researcher can see whether the
#' assumption their sum score makes is consistent with their data.
#'
#' **A score is not the latent variable.** Grice (2001) evaluates factor scores
#' on three criteria, all reported here and all computed from the fitted model:
#'
#' * `validity`, the correlation between a score and the factor it estimates.
#'   For `method = "regression"` this equals the factor determinacy coefficient
#'   reported by [nomo_hierarchical()], because that method maximizes it.
#' * `univocality`, its correlation with the factors it does not represent.
#' * `correlational_accuracy`, how far correlations among scores sit from the
#'   correlations among the factors they stand in for.
#'
#' The third deserves attention before scores are used in later analyses. A
#' relationship estimated from scores carries that discrepancy as bias, and its
#' direction depends on the scoring method and the model rather than being a
#' constant that can be corrected for. Where a question can be asked of the
#' latent variables instead, asking it of scores replaces an unbiased answer
#' with a biased one.
#'
#' **One design recovers a regression.** For a linear regression among
#' factors, Skrondal and Laake (2001) proved that regression-method scores for
#' the predictors and Bartlett scores for the outcome, each block scored from a
#' measurement model of its own, give consistent estimates of the regression
#' coefficients. Both conditions matter: scoring every factor from one model,
#' or using the same method for both blocks, does not. Standard errors that
#' treat the scores as observed are not corrected by this, and the result does
#' not extend to nonlinear models. `vignette("scoring", package = "nomologR")`
#' works through the design.
#'
#' **Thresholds are context.** Gorsuch's (1983) recommendation that validity
#' reach .80, and above .90 for scores serving as substitutes for the factors
#' themselves, is reported where a value falls below it and is never applied as
#' a rule.
#'
#' The supplied data is never modified, and scores are computed only for the
#' cases the model used.
#'
#' @param fit A `nomo_cfa` object or fitted `lavaan` measurement model.
#'   Single-group, single-level, with no regressions among latent variables.
#' @param method Scoring method. `"sum"` and `"mean"` are unit weighted;
#'   `"regression"` and `"bartlett"` are model weighted.
#' @param guidance A `nomo_guidance` object from [nomo_defaults()].
#'
#' @return An object of class `nomo_scores`: the `scores` themselves, a
#'   `diagnostics` table of Grice's three criteria per factor, the
#'   `unit_weighting` evidence, `notes`, and the fitted model.
#'
#' @references
#' Gorsuch, R. L. (1983). *Factor analysis* (2nd ed.). Lawrence Erlbaum.
#'
#' Grice, J. W. (2001). Computing and evaluating factor scores.
#' *Psychological Methods, 6*(4), 430-450.
#' \doi{10.1037/1082-989X.6.4.430}
#'
#' McNeish, D., & Wolf, M. G. (2020). Thinking twice about sum scores.
#' *Behavior Research Methods, 52*(6), 2287-2305.
#' \doi{10.3758/s13428-020-01398-0}
#'
#' Skrondal, A., & Laake, P. (2001). Regression among factor scores.
#' *Psychometrika, 66*(4), 563-575. \doi{10.1007/BF02296196}
#'
#' @seealso [nomo_hierarchical()] for factor determinacy and construct
#'   replicability.
#'
#' @examples
#' model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
#'
#' # A sum score, with the parallel model it assumes fitted and compared
#' summed <- nomo_scores(cfa, method = "sum")
#' summed
#' summed$unit_weighting
#'
#' # Regression-method factor scores, with Grice's criteria
#' refined <- nomo_scores(cfa, method = "regression")
#' refined$diagnostics
#' head(refined$scores)
#'
#' @export
nomo_scores <- function(fit,
                        method = c("sum", "mean", "regression", "bartlett"),
                        guidance = nomo_defaults()) {
  method <- match.arg(method)
  input <- nomo_scores_input(fit)

  scores <- if (method %in% c("sum", "mean")) {
    nomo_scores_unit(input, method)
  } else {
    nomo_scores_refined(input, method)
  }

  parts <- nomo_scores_weights(input, method)
  properties <- nomo_scores_properties(parts)

  diagnostics <- tibble::tibble(
    factor = parts$factors,
    n_items = as.integer(vapply(
      parts$factors, function(f) length(input$factors[[f]]), integer(1),
      USE.NAMES = FALSE
    )),
    validity = unname(properties$validity),
    univocality = unname(properties$univocality),
    correlational_accuracy = unname(properties$accuracy)
  )

  unit_weighting <- nomo_scores_unit_weighting(input, parts)
  parallel <- if (method %in% c("sum", "mean")) {
    nomo_scores_parallel_test(input)
  } else {
    list(
      available = FALSE, chisq_diff = NA_real_, df_diff = NA_real_,
      p_value = NA_real_, note = ""
    )
  }

  out <- list(
    call = match.call(),
    method = method,
    weighting = if (method %in% c("sum", "mean")) "unit" else "model",
    factors = input$factors,
    ordered = input$ordered,
    scores = tibble::as_tibble(scores),
    diagnostics = diagnostics,
    unit_weighting = unit_weighting,
    parallel_test = parallel,
    score_correlations = properties$cor_scores,
    factor_correlations = properties$cor_factors,
    notes = nomo_scores_notes(
      input, method, diagnostics, unit_weighting, parallel
    ),
    fit = input$fit,
    guidance = guidance
  )
  class(out) <- c("nomo_scores", "list")
  out
}
