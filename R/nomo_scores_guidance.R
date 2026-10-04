# Notes ------------------------------------------------------------------------
#
# Each note's first sentence states what was found, so that print() can show
# that sentence and summary() the whole note. The references the notes apply
# are fixed values from the literature, not settings read from `guidance`:
# Gorsuch's (1983) .90 for validity, and .05 for a discrepancy between
# correlations, for correlational accuracy and for univocality alike.

# The size of a discrepancy between correlations that a note raises, for
# correlational accuracy and for univocality.
nomo_scores_discrepancy_reference <- 0.05


nomo_scores_notes <- function(input, method, diagnostics, unit_weighting,
                              parallel, properties, scores, opposite) {
  notes <- tibble::tibble(
    topic = character(), severity = character(), note = character()
  )
  add <- function(notes, topic, severity, note) {
    tibble::add_row(notes, topic = topic, severity = severity, note = note)
  }
  r <- function(v, signed = FALSE) nomo_present_stat(v, "r", signed = signed)

  unit <- method %in% c("sum", "mean")
  several <- nrow(diagnostics) > 1L

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
      test <- nomo_present_chisq(parallel$chisq_diff, parallel$df_diff,
                                 parallel$p_value, delta = TRUE)
      if (parallel$p_value < .05) {
        notes <- add(notes, "unit_weighting", "review", sprintf(
          paste(
            "The parallel model that unit weighting assumes fits worse than",
            "the model you fitted (%s). The items are not interchangeable in",
            "the way adding them assumes. This does not forbid a sum score; it",
            "means the choice needs a reason beyond convenience, and that %s",
            "what it costs."
          ),
          test,
          if (several) {
            "`validity` and `correlational_accuracy` describe"
          } else {
            "`validity` describes"
          }
        ))
      } else {
        notes <- add(notes, "unit_weighting", "info", sprintf(
          paste(
            "The parallel model that unit weighting assumes does not fit",
            "worse than the model you fitted (%s), so the constraints adding",
            "the items assumes are consistent with these data."
          ),
          test
        ))
      }
    } else if (nzchar(parallel$note)) {
      notes <- add(notes, "unit_weighting", "review", parallel$note)
    }

    # The reliability of a unit-weighted composite is omega from the fitted
    # model; alpha equals it only when the loadings are equal (#145).
    notes <- add(notes, "reliability", "info", paste(
      "The reliability of a unit-weighted score is omega computed from the",
      "fitted model, which nomo_reliability() reports; coefficient alpha",
      "equals it only when the items' loadings are equal (essential",
      "tau-equivalence). Coefficient H describes optimally weighted scores",
      "rather than a sum (McNeish & Wolf, 2020). Reporting H for a sum score,",
      "or a sum's omega or alpha for a weighted one, describes a scale that",
      "was not used."
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
      "Validity is below .90 for ", nomo_present_or(low_validity, "and"),
      ". Gorsuch (1983, p. 260) recommended at least .80, and above .90 if ",
      "the scores are to serve as adequate substitutes for the factors ",
      "themselves. Reported as his recommendation, not applied as a rule."
    ))
  }

  reference <- nomo_scores_discrepancy_reference
  accurate <- is.finite(diagnostics$correlational_accuracy)
  if (any(accurate & abs(diagnostics$correlational_accuracy) >= reference)) {
    size <- abs(diagnostics$correlational_accuracy)
    worst <- which.max(size)
    # A discrepancy belongs to a pair of factors and both report it, so the note
    # names the pair; picking one of two tied values would depend on rounding.
    pair <- diagnostics$factor[accurate & abs(size - size[[worst]]) < 1e-8]
    notes <- add(notes, "correlational_accuracy", "concern", sprintf(
      paste(
        "Correlations among these scores do not reproduce the correlations",
        "among the factors: the largest discrepancy is %s, between %s. A",
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
      nomo_present_stat(diagnostics$correlational_accuracy[[worst]], "r", signed = TRUE,
                        reference = sign(diagnostics$correlational_accuracy[[worst]]) *
                          reference),
      paste(pair, collapse = " and ")
    ))
  }

  # Univocality is judged against the factor correlations (#145): a score
  # reaches another factor through its own, by the factor correlation times
  # its validity, and only a departure from that is variance it takes from the
  # other factor directly.
  if (several) {
    judged <- nomo_scores_univocality_departure(properties)
    departure <- judged$departure
    if (any(abs(departure) >= reference, na.rm = TRUE)) {
      at <- which(abs(departure) == max(abs(departure), na.rm = TRUE), arr.ind = TRUE)[1L, ]
      other <- diagnostics$factor[[at[[1L]]]]
      own <- diagnostics$factor[[at[[2L]]]]
      notes <- add(notes, "univocality", "review", sprintf(
        paste(
          "The score for %s correlates %s with %s, a factor it does not",
          "represent, where through %s alone it would correlate %s, the factor",
          "correlation (%s) times the score's validity (%s). The difference,",
          "%s, is what the score takes from %s directly, so it is not",
          "univocal (Grice, 2001) and cannot be treated as though it measured",
          "%s alone."
        ),
        own, r(properties$cor_factor_scores[at[[1L]], at[[2L]]]), other,
        own, r(judged$through[at[[1L]], at[[2L]]]),
        r(properties$cor_factors[at[[1L]], at[[2L]]]),
        r(properties$validity[[at[[2L]]]]),
        r(departure[at[[1L]], at[[2L]]], signed = TRUE), other, own
      ))
    }
  }

  if (length(input$ordered)) {
    notes <- add(notes, "estimand", "review", nomo_scores_ordered_note(
      method, length(input$ordered)
    ))
  }

  if (length(input$cross_loaded)) {
    k <- length(input$cross_loaded)
    notes <- add(notes, "cross_loadings", "review", paste0(
      nomo_present_noun(k, "Item ", "Items "), nomo_present_or(input$cross_loaded, "and"),
      nomo_present_noun(k, " loads", " load"), " on more than one factor. A ",
      "unit-weighted score assigns each such item wholly to every factor it ",
      "loads on, which counts it more than once; a weighted score divides it ",
      "according to the model."
    ))
  }

  # A sum adds an item that loads against its factor as though it loaded with
  # it, which unit weighting's loading ratio, taken in absolute value, cannot
  # show (#145).
  reversed <- opposite[lengths(opposite) > 0L]
  if (unit && length(reversed)) {
    named <- unlist(lapply(names(reversed), function(f) {
      sprintf("%s on %s", reversed[[f]], f)
    }))
    several_items <- length(named) > 1L
    notes <- add(notes, "loading_signs", "concern", paste0(
      "Standardized loadings differ in sign within a factor: ",
      nomo_present_or(named, "and"), if (several_items) " load" else " loads",
      " against the strongest item of ", if (several_items) "their factors" else "its factor",
      ". A unit-weighted score gives every item the same positive weight, so ",
      "such an item counts against the factor it measures, while the model ",
      "weights it negatively. Reverse-score each such item before adding, or ",
      "use a model-weighted score."
    ))
  }

  heterogeneous <- unit_weighting$factor[
    is.finite(unit_weighting$loading_ratio) & unit_weighting$loading_ratio >= 2
  ]
  if (unit && length(heterogeneous)) {
    notes <- add(notes, "unit_weighting", "review", paste0(
      "The strongest standardized loading is at least twice the weakest for ",
      nomo_present_or(heterogeneous, "and"),
      ". Adding those items gives the weakest indicator the same say as the ",
      "strongest, so two people with the same total can differ on the construct ",
      "by having endorsed different items."
    ))
  }

  # A case the model used can still lack a score: a unit-weighted score needs
  # every item of its factor, which FIML does not, and lavaan returns no score
  # for a case it cannot estimate (#145).
  unscored <- vapply(scores, function(v) sum(!is.finite(v)), integer(1))
  unscored <- unscored[unscored > 0L]
  if (length(unscored)) {
    n <- nrow(scores)
    counts <- sprintf("%d of %d on %s", n - unscored, n, names(unscored))
    notes <- add(notes, "unscored_cases", "review", paste(
      "Not every case the model used has a score: the cases scored are",
      paste0(nomo_present_or(counts, "and"), ","),
      "and the others are `NA` in `scores`.",
      if (unit) {
        paste(
          "A unit-weighted score needs every item of its factor, so a case",
          "missing one has none, while regression and Bartlett scores use the",
          "items a case has."
        )
      } else {
        "lavaan returned no score for those cases."
      }
    ))
  }

  notes
}


# What lavaan computes for ordered indicators, and what the diagnostics then
# describe (#145). The diagnostics come from linear weights on the continuous
# latent responses underlying the items, so for every method they approximate
# the scores returned rather than describe them exactly.
nomo_scores_ordered_note <- function(method, k) {
  ordered <- sprintf("%s %s ordered", nomo_present_count(k, "indicator"),
                     nomo_present_noun(k, "is", "are"))
  approximate <- paste(
    "The validity, univocality, and correlational accuracy reported here are",
    "those of %s, so they approximate the properties of these scores rather",
    "than describe them, and the approximation worsens with fewer categories",
    "and more extreme thresholds."
  )
  switch(
    method,
    regression = paste(
      paste0(ordered, ", so lavaan computed these regression scores as empirical"),
      "Bayes modal (EBM) scores from the categorical model, not as weighted",
      "sums of the items.",
      sprintf(approximate, "linear regression scores of the continuous latent responses")
    ),
    bartlett = paste(
      paste0(ordered, ", so lavaan computed these Bartlett scores as"),
      "maximum-likelihood (ML) scores from the categorical model, not as",
      "weighted sums of the items. A case that answers every item of a factor",
      "in its highest or its lowest category has no finite ML score.",
      sprintf(approximate, "linear Bartlett scores of the continuous latent responses"),
      "ML scores can differ from them markedly."
    ),
    paste(
      paste0(ordered, ", and these scores add the observed category numbers,"),
      "while the validity, univocality, and correlational accuracy reported",
      "here are those of the same", method, "of the continuous latent responses",
      "underlying the items. They approximate the properties of these scores",
      "rather than describe them, and the approximation worsens with fewer",
      "categories and more extreme thresholds."
    )
  )
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
#' assumption their sum score makes is consistent with their data. The parallel
#' model is the supplied model with those two sets of constraints added and
#' nothing else changed, estimated as the supplied model was: the same cases,
#' estimator, and missing-data handling, with factor covariances and residual
#' covariances kept as specified. When the comparison cannot be made,
#' `parallel_test$note` says why.
#'
#' **A score is not the latent variable.** Grice (2001) evaluates factor scores
#' on three criteria, all reported here and all computed from the fitted model:
#'
#' * `validity`, the correlation between a score and the factor it estimates.
#'   For `method = "regression"` this equals the factor determinacy coefficient
#'   reported by [nomo_hierarchical()], because that method maximizes it.
#' * `univocality`, its largest correlation with a factor it does not
#'   represent.
#' * `correlational_accuracy`, how far correlations among scores sit from the
#'   correlations among the factors they stand in for: for each factor, the
#'   score correlation minus the factor correlation for the pair where the two
#'   differ most, so 0 is best.
#'
#' The third deserves attention before scores are used in later analyses. A
#' relationship estimated from scores carries that discrepancy as bias, and its
#' direction depends on the scoring method and the model rather than being a
#' constant that can be corrected for. Where a question can be asked of the
#' latent variables instead, asking it of scores replaces an unbiased answer
#' with a biased one.
#'
#' **Univocality is judged against the factor correlations.** When factors
#' correlate, a score correlates with the other factors through its own: by
#' the factor correlation times its validity. Bartlett scores, and sum scores
#' of items that each load on one factor, correlate with the other factors by
#' exactly that much. Only a departure from it is something a score takes from
#' another factor directly, so a note is raised when a score's correlation
#' with another factor departs from the factor correlation times its validity
#' by .05 or more, and not merely because factors correlate (Grice, 2001).
#'
#' **Ordered indicators.** With ordered indicators lavaan computes
#' `"regression"` scores as empirical Bayes modal scores, and `"bartlett"`
#' scores as maximum-likelihood scores, from the categorical model; `"sum"` and
#' `"mean"` add the observed category numbers. The
#' diagnostics are computed from linear weights on the continuous latent
#' responses underlying the items, so for ordered indicators they approximate
#' the properties of the scores returned, and a note says so.
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
#' a rule. These references, and the .05 for a discrepancy between
#' correlations, are fixed values from the literature; they are not read from
#' `guidance`.
#'
#' The supplied data is never modified, and scores are computed only for the
#' cases the model used. `rows` records which rows of the data those are, so
#' the scores can be matched to the data when some cases were dropped. A
#' unit-weighted score needs every item of its factor: under FIML a case the
#' model used can lack an item, and its sum or mean is then `NA`, which a note
#' counts. Regression and Bartlett scores use the items a case has.
#'
#' `print()` shows the score properties with a key to their columns, each
#' flagged note's first sentence, and the parallel-model test when it is not
#' flagged; `summary()` adds the loading spread and every note in full.
#' `plot()` draws each score's validity against Gorsuch's references.
#'
#' @param fit A `nomo_cfa` object or fitted `lavaan` measurement model.
#'   Single-group and single-level, with every factor measured by observed
#'   items and no structural paths: no regressions, covariates, or
#'   higher-order factors.
#' @param method Scoring method. `"sum"` and `"mean"` are unit weighted;
#'   `"regression"` and `"bartlett"` are model weighted.
#' @param guidance A `nomo_guidance` object from [nomo_defaults()]. It is
#'   checked and recorded with the result; the references the notes apply are
#'   fixed values from the literature (see Details), not settings read from it.
#'
#' @return An object of class `nomo_scores`. The fields to read are:
#'
#'   * `scores`: one column per factor, one row per case used.
#'   * `rows`: for each row of `scores`, the row of the data the model was
#'     fitted to. A case the model did not use, as listwise deletion drops an
#'     incomplete one, has no score, so `scores` can have fewer rows than the
#'     data. `data[x$rows, ]` holds the scored cases in the order of `scores`.
#'   * `method` and `weighting`: how the scores were computed.
#'   * `diagnostics`: Grice's validity, univocality, and correlational
#'     accuracy per factor.
#'   * `unit_weighting`: the loading evidence on whether unit weights suit
#'     the model, and `parallel_test`, the test of the parallel model that unit
#'     weighting assumes.
#'   * `score_correlations` and `factor_correlations`: for comparing the two.
#'   * `notes`: what the scores do and do not estimate.
#'
#'   Other fields record the call, the settings used, and intermediate engine
#'   results. They may change between releases and are not part of the stable
#'   interface (see `?nomologR`).
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
#' # Scores joined to the data, by the rows the model used
#' scored <- lavaan::HolzingerSwineford1939
#' scored[refined$rows, names(refined$scores)] <- refined$scores
#' head(scored[, c("id", names(refined$scores))])
#'
#' @export
nomo_scores <- function(fit,
                        method = c("sum", "mean", "regression", "bartlett"),
                        guidance = nomo_defaults()) {
  method <- nomo_match_arg(method)
  # Checked as nomo_cfa() checks it, so a value that is not guidance at all is
  # refused rather than recorded (#145).
  if (!is.list(guidance)) {
    stop("`guidance` must be a list returned by `nomo_defaults()`.", call. = FALSE)
  }
  nomo_defaults_check_safeguards(guidance)
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
    # Which row of the fitted data each score belongs to. Listwise deletion
    # leaves `scores` shorter than the data, and position alone cannot say
    # which cases are missing from it.
    rows = as.integer(lavaan::lavInspect(input$fit, "case.idx")),
    diagnostics = diagnostics,
    unit_weighting = unit_weighting,
    parallel_test = parallel,
    score_correlations = properties$cor_scores,
    factor_correlations = properties$cor_factors,
    notes = nomo_scores_notes(
      input, method, diagnostics, unit_weighting, parallel, properties,
      scores, nomo_scores_opposite_signs(input)
    ),
    fit = input$fit,
    guidance = guidance
  )
  class(out) <- c("nomo_scores", "list")
  out
}
