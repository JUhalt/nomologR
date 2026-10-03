# Convergent and discriminant validity evidence -------------------------------

#' Convergent and discriminant construct-validity evidence
#'
#' `nomo_validity()` summarizes several complementary forms of measurement
#' evidence from a fitted first-order CFA. Convergent evidence includes
#' standardized loadings and average variance extracted (AVE). Discriminant
#' evidence includes latent-factor correlations and, where appropriate, HTMT2
#' as the primary heterotrait-monotrait summary, with the original HTMT available
#' as a sensitivity/teaching comparison.
#'
#' The function deliberately avoids a binary declaration that a construct is
#' "valid" or "invalid". Numerical references trigger inspection and
#' explanation. HTMT-family results are considered together with theory,
#' indicator content, CFA fit, and latent-factor overlap. The Fornell-Larcker
#' comparison is available only by explicit request and is labeled legacy/
#' supporting evidence because simulation work has shown that it can miss
#' discriminant-validity problems that HTMT detects.
#'
#' @param fit A fitted [nomo_cfa()] object or fitted `lavaan` CFA model.
#' @param ave_obs_var Logical passed to [semTools::AVE()]. `TRUE` (default) uses
#'   observed variances in the denominator; `FALSE` uses model-implied
#'   variances. For ordinal indicators, `semTools` calculates AVE from the
#'   polychoric correlation structure in the model's unstandardized metric, so
#'   a fit with lavaan's theta parameterization gives a different AVE from the
#'   delta fit of the same model; the decision log then gives the mean squared
#'   standardized loading, which is the same under either.
#' @param htmt Which heterotrait-monotrait summaries to request: `"both"`
#'   (default), `"htmt2"`, `"htmt"`, or `"none"`. HTMT2 is prioritized
#'   because its geometric-mean formulation is designed for congeneric
#'   indicators; original HTMT assumes tau-equivalence.
#' @param htmt_missing Missing-data handling for the correlations behind HTMT
#'   and HTMT2, passed to [semTools::htmt()]. The default, `"default"`, follows
#'   the fitted model, so HTMT rests on the same cases as the CFA: `"fiml"`
#'   when the CFA used full-information maximum likelihood, `"pairwise"` when
#'   it used pairwise deletion, and `"listwise"` otherwise. Other supported
#'   values are `"listwise"`, `"pairwise"`, `"direct"`, `"ml"`, and `"fiml"`.
#'   The handling used and the number of cases appear in `htmt_status` and the
#'   decision log.
#' @param fornell_larcker Logical. If `TRUE`, also produce a legacy
#'   Fornell-Larcker matrix and pairwise comparison where available. It is not
#'   used as the primary discriminant-validity criterion.
#' @param guidance Guidance settings from [nomo_defaults()]. The AVE and HTMT
#'   references are review prompts rather than universal pass/fail cutoffs.
#'
#' @return A `nomo_validity` object. The fields to read are:
#'
#'   * `ave`: average variance extracted per construct, as convergent
#'     evidence. AVE is not defined for a factor with a cross-loaded
#'     indicator; its row has an `NA` estimate and the attention
#'     `"unavailable"`.
#'   * `latent_correlations`: construct correlations with intervals, and their
#'     `reference` and `attention`. A pair is flagged for review when its
#'     correlation could exceed the HTMT-family reference: the upper limit of
#'     its interval (Rönkkö & Cho, 2022), or the estimate when there is no
#'     interval, is above it. A correlation beyond 1 in absolute value is a
#'     concern.
#'   * `htmt2` and `htmt`: heterotrait-monotrait ratios per pair.
#'   * `discriminant`: each pair's separation evidence with its reference and
#'     interpretation.
#'   * `htmt_status`: which HTMT variants were computed, and why any was not.
#'     For a computed variant, `missing` is the missing-data handling used and
#'     `n` the number of cases: the complete cases under listwise deletion, the
#'     smallest pairwise count under pairwise deletion, and every case with an
#'     indicator observed otherwise. HTMT is not defined with one construct or
#'     for a construct with one indicator, which has no within-construct
#'     correlations; such pairs are left out, and the decision log records it
#'     as information rather than as missing evidence.
#'   * `fornell_larcker_pairs`: the historical comparison, when requested.
#'   * `standardized_loadings`, `references`, and `decision_log`.
#'
#'   `attention` in `standardized_loadings` is `"KEEP"`, `"REVIEW"`, or
#'   `"STRONG REVIEW"`, as in [nomo_cfa()]. `attention` in `ave` and
#'   `discriminant`, and `severity` in `decision_log`, are `"info"`,
#'   `"review"`, or `"concern"`. `attention` in `fornell_larcker_pairs` is
#'   `"info"` or `"review"`, or `"unavailable"` when the comparison could not
#'   be computed, as when an AVE is missing or negative and so has no square
#'   root. See **Conventions in returned tables** in `?nomologR`.
#'
#'   In the `"discriminant"` table of [nomo_table()], `signal` is the more
#'   severe of the HTMT-family flag (HTMT2, or HTMT when HTMT2 was not
#'   computed) and the latent correlation's `attention`, so a pair is still
#'   evaluated when no HTMT value exists.
#'
#'   Other fields record the call, the settings used, and intermediate engine
#'   results. They may change between releases and are not part of the stable
#'   interface (see `?nomologR`).
#'
#'   `print()` counts the convergent and construct-separation flags and names
#'   each flagged construct or pair; `summary()` adds the AVE and loading
#'   summary for each construct, the latent correlation and HTMT-family values
#'   for each pair, and the full reason for each flag.
#'
#' @references
#' Historical context:
#'
#' Campbell, D. T., & Fiske, D. W. (1959). Convergent and discriminant
#' validation by the multitrait-multimethod matrix. *Psychological Bulletin,
#' 56*(2), 81-105. \doi{10.1037/h0046016}
#'
#' Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models
#' with unobservable variables and measurement error. *Journal of Marketing
#' Research, 18*(1), 39-50. \doi{10.2307/3151312}
#'
#' Contemporary construct-separation evidence:
#'
#' Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for
#' assessing discriminant validity in variance-based structural equation
#' modeling. *Journal of the Academy of Marketing Science, 43*(1), 115-135.
#' \doi{10.1007/s11747-014-0403-8}
#'
#' Roemer, E., Schuberth, F., & Henseler, J. (2021). HTMT2--An improved
#' criterion for assessing discriminant validity in structural equation
#' modeling. *Industrial Management & Data Systems, 121*(12), 2637-2650.
#' \doi{10.1108/IMDS-02-2021-0082}
#'
#' Rönkkö, M., & Cho, E. (2022). An updated guideline for assessing
#' discriminant validity. *Organizational Research Methods, 25*(1), 6-47.
#' \doi{10.1177/1094428120968614}
#'
#' Voorhees, C. M., Brady, M. K., Calantone, R., & Ramirez, E. (2016).
#' Discriminant validity testing in marketing: An analysis, causes for concern,
#' and proposed remedies. *Journal of the Academy of Marketing Science, 44*(1),
#' 119-134. \doi{10.1007/s11747-015-0455-4}
#'
#' @export
#'
#' @examples
#' model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
#' val <- nomo_validity(cfa)
#' summary(val)
#' val$decision_log
nomo_validity <- function(fit,
                          ave_obs_var = TRUE,
                          htmt = c("both", "htmt2", "htmt", "none"),
                          htmt_missing = "default",
                          fornell_larcker = FALSE,
                          guidance = nomo_defaults()) {
  if (!is.logical(ave_obs_var) || length(ave_obs_var) != 1L || is.na(ave_obs_var)) {
    stop("`ave_obs_var` must be TRUE or FALSE.", call. = FALSE)
  }
  htmt <- nomo_match_arg(htmt)
  if (!is.character(htmt_missing) || length(htmt_missing) != 1L ||
      is.na(htmt_missing) || !nzchar(trimws(htmt_missing))) {
    stop("`htmt_missing` must be one non-empty character value.", call. = FALSE)
  }
  htmt_missing <- tolower(trimws(htmt_missing))
  allowed_missing <- c("default", "listwise", "pairwise", "direct", "ml", "fiml")
  if (!htmt_missing %in% allowed_missing) {
    stop(
      paste0("`htmt_missing` must be one of: ", paste(allowed_missing, collapse = ", "), "."),
      call. = FALSE
    )
  }
  if (!is.logical(fornell_larcker) || length(fornell_larcker) != 1L ||
      is.na(fornell_larcker)) {
    stop("`fornell_larcker` must be TRUE or FALSE.", call. = FALSE)
  }

  loading_reference <- nomo_guidance_value(guidance, "cfa_loading_reference")
  ave_reference <- nomo_guidance_value(guidance, "ave_reference")
  htmt_reference <- nomo_guidance_value(guidance, "htmt_reference")
  nomo_defaults_check_safeguards(guidance)
  # References print as the values they are compared with (#144).
  ave_reference_shown <- nomo_present_stat(ave_reference, "reliability")
  htmt_reference_shown <- nomo_present_stat(htmt_reference, "htmt")

  fit_info <- nomo_measurement_fit(
    fit,
    allow_cross_loadings = TRUE
  )

  loadings <- nomo_validity_standardized_loadings(fit_info$fit, guidance)

  ave_warnings <- character()
  ave_engine <- tryCatch(
    withCallingHandlers(
      semTools::AVE(
        fit_info$fit,
        obs.var = ave_obs_var,
        return.df = TRUE
      ),
      warning = function(w) {
        ave_warnings <<- unique(c(ave_warnings, conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      stop(paste0("AVE estimation failed: ", conditionMessage(e)), call. = FALSE)
    }
  )

  ave <- nomo_validity_ave_tidy(ave_engine, construct_names = fit_info$latent_names)
  if (nrow(ave)) {
    # semTools::AVE() is not defined for a factor with a cross-loaded
    # indicator. That NA is expected, so it is reported as not computed rather
    # than as an inadmissible value (#145).
    pe <- fit_info$parameter_estimates
    cross_factors <- unique(
      pe$lhs[pe$op == "=~" & pe$rhs %in% fit_info$cross_loaded_items]
    )
    cross_na <- !is.finite(ave$estimate) & ave$construct %in% cross_factors
    ave$reference <- ave_reference
    ave$attention <- ifelse(
      cross_na,
      "unavailable",
      ifelse(
        !is.finite(ave$estimate) | ave$estimate < 0 | ave$estimate > 1,
        "concern",
        ifelse(ave$estimate < ave_reference, "review", "info")
      )
    )
    ave$interpretation <- vapply(seq_len(nrow(ave)), function(i) {
      estimate <- ave$estimate[[i]]
      # The group or level, when there are several, so each row says which.
      label <- if (ave$block[[i]] == "overall") "AVE" else paste("AVE in", ave$block[[i]])
      if (cross_na[[i]]) {
        paste(
          "AVE is not computed for a factor with a cross-loaded indicator,",
          "because its share of that indicator's variance is not defined.",
          "The cross-loading entry in the decision log explains the model."
        )
      } else if (!is.finite(estimate) || estimate < 0 || estimate > 1) {
        paste(
          "AVE is unavailable or outside its conventional 0-1 range.",
          "Inspect model admissibility, cross-loadings, and indicator specification before interpretation."
        )
      } else if (estimate < ave_reference) {
        paste0(
          label, " (", nomo_present_stat(estimate, "reliability", reference = ave_reference),
          ") is below the configured convergent-evidence reference (",
          ave_reference_shown,
          "). Inspect standardized loadings, indicator-specific error, and content coverage; do not automatically delete items."
        )
      } else {
        paste0(
          label, " (", nomo_present_stat(estimate, "reliability", reference = ave_reference),
          ") is at or above the configured convergent-evidence reference (",
          ave_reference_shown,
          "). This contributes convergent evidence but does not establish construct validity by itself."
        )
      }
    }, character(1))
  }

  latent_correlations <- nomo_validity_latent_correlations(fit_info$fit)
  if (nrow(latent_correlations)) {
    latent_correlations <- nomo_validity_latent_attention(
      latent_correlations, htmt_reference
    )
  }

  htmt_inputs <- nomo_validity_htmt_inputs(fit_info)
  htmt2_matrix <- NULL
  htmt_matrix <- NULL
  htmt_status <- tibble::tibble(
    method = character(),
    requested = logical(),
    available = logical(),
    reason = character(),
    missing = character(),
    n = integer()
  )

  want_htmt2 <- htmt %in% c("both", "htmt2")
  want_htmt <- htmt %in% c("both", "htmt")

  add_status <- function(method, requested, available, reason = "",
                         missing = NA_character_, n = NA_integer_) {
    tibble::tibble(
      method = method,
      requested = requested,
      available = available,
      reason = reason,
      missing = missing,
      n = if (isTRUE(available)) as.integer(n) else NA_integer_
    )
  }

  if (htmt == "none") {
    htmt_status <- dplyr::bind_rows(
      add_status("HTMT2", FALSE, FALSE, "Not requested."),
      add_status("HTMT", FALSE, FALSE, "Not requested.")
    )
  } else if (!isTRUE(htmt_inputs$available)) {
    if (want_htmt2) {
      htmt_status <- dplyr::bind_rows(
        htmt_status,
        add_status("HTMT2", TRUE, FALSE, htmt_inputs$reason)
      )
    }
    if (want_htmt) {
      htmt_status <- dplyr::bind_rows(
        htmt_status,
        add_status("HTMT", TRUE, FALSE, htmt_inputs$reason)
      )
    }
  } else {
    # "default" follows the fitted model's own missing-data handling, so HTMT
    # and the CFA rest on the same cases; the cases used are recorded (#145).
    htmt_missing_used <- if (identical(htmt_missing, "default")) {
      nomo_validity_htmt_missing(fit_info$fit)
    } else {
      htmt_missing
    }
    htmt_n <- nomo_validity_htmt_n(htmt_inputs$data, htmt_missing_used)

    if (want_htmt2) {
      htmt2_error <- NULL
      htmt2_matrix <- tryCatch(
        semTools::htmt(
          model = htmt_inputs$model,
          data = htmt_inputs$data,
          missing = htmt_missing_used,
          ordered = if (length(htmt_inputs$ordered)) htmt_inputs$ordered else NULL,
          absolute = TRUE,
          htmt2 = TRUE
        ),
        error = function(e) {
          htmt2_error <<- conditionMessage(e)
          NULL
        }
      )
      htmt_status <- dplyr::bind_rows(
        htmt_status,
        add_status(
          "HTMT2", TRUE, !is.null(htmt2_matrix),
          if (is.null(htmt2_error)) "" else htmt2_error,
          missing = htmt_missing_used, n = htmt_n
        )
      )
    }

    if (want_htmt) {
      htmt_error <- NULL
      htmt_matrix <- tryCatch(
        semTools::htmt(
          model = htmt_inputs$model,
          data = htmt_inputs$data,
          missing = htmt_missing_used,
          ordered = if (length(htmt_inputs$ordered)) htmt_inputs$ordered else NULL,
          absolute = TRUE,
          htmt2 = FALSE
        ),
        error = function(e) {
          htmt_error <<- conditionMessage(e)
          NULL
        }
      )
      htmt_status <- dplyr::bind_rows(
        htmt_status,
        add_status(
          "HTMT", TRUE, !is.null(htmt_matrix),
          if (is.null(htmt_error)) "" else htmt_error,
          missing = htmt_missing_used, n = htmt_n
        )
      )
    }
  }

  htmt2 <- if (is.matrix(htmt2_matrix)) {
    nomo_matrix_pairs(htmt2_matrix, value_name = "estimate")
  } else {
    tibble::tibble()
  }
  if (nrow(htmt2)) htmt2$method <- "HTMT2"

  htmt_original <- if (is.matrix(htmt_matrix)) {
    nomo_matrix_pairs(htmt_matrix, value_name = "estimate")
  } else {
    tibble::tibble()
  }
  if (nrow(htmt_original)) htmt_original$method <- "HTMT"

  discriminant <- dplyr::bind_rows(htmt2, htmt_original)
  if (nrow(discriminant)) {
    discriminant$reference <- htmt_reference
    discriminant$attention <- ifelse(
      !is.finite(discriminant$estimate) | discriminant$estimate < 0,
      "concern",
      ifelse(discriminant$estimate > htmt_reference, "review", "info")
    )
    discriminant$interpretation <- vapply(seq_len(nrow(discriminant)), function(i) {
      method <- discriminant$method[[i]]
      estimate <- discriminant$estimate[[i]]
      if (!is.finite(estimate) || estimate < 0) {
        paste(method, "is unavailable or inadmissible; inspect the observed-correlation structure.")
      } else if (estimate > htmt_reference) {
        paste0(
          method, " (", nomo_present_stat(estimate, "htmt", reference = htmt_reference),
          ") exceeds the configured review reference (", htmt_reference_shown,
          "). This raises a construct-separation concern; inspect theoretical distinctiveness, item content, cross-construct overlap, and latent correlations rather than automatically merging or deleting constructs."
        )
      } else {
        paste0(
          method, " (", nomo_present_stat(estimate, "htmt", reference = htmt_reference),
          ") does not exceed the configured review reference (", htmt_reference_shown,
          "). This contributes evidence of empirical separation but is not a declaration that discriminant validity has been established."
        )
      }
    }, character(1))
  }

  fornell <- list(matrix = NULL, pairs = tibble::tibble(), reason = "Not requested.")
  if (fornell_larcker) {
    if (fit_info$ngroups == 1L && fit_info$nlevels == 1L) {
      fornell <- nomo_validity_fornell_larcker(fit_info$fit, ave)
    } else {
      fornell$reason <- paste(
        "Legacy Fornell-Larcker output is currently restricted to a single-group,",
        "single-level measurement model rather than silently combining blocks."
      )
    }
  }

  fit_context <- nomo_reliability_fit_context(fit_info$fit, guidance)

  references <- tibble::tibble(
    topic = c(
      "AVE",
      "HTMT",
      "HTMT2",
      "legacy_discriminant_evidence"
    ),
    citation = c(
      "Fornell & Larcker (1981)",
      "Henseler, Ringle, & Sarstedt (2015)",
      "Roemer, Schuberth, & Henseler (2021)",
      "Henseler et al. (2015); Voorhees et al. (2016)"
    ),
    purpose = c(
      "Treat AVE as convergent evidence about indicators, not as reliability or a binary validity verdict.",
      "Use heterotrait-monotrait evidence because legacy approaches can miss construct-overlap problems.",
      "Prioritize HTMT2 for congeneric indicators because it relaxes the original HTMT tau-equivalence assumption.",
      "Retain Fornell-Larcker only as optional historical/supporting information rather than the primary discriminant criterion."
    )
  )

  log <- nomo_log_new()
  log <- nomo_log_add(
    log,
    stage = "validity",
    object = "measurement_model",
    metric = "evidence_strategy",
    reference = "Fornell & Larcker (1981); Henseler et al. (2015); Roemer et al. (2021)",
    severity = "info",
    observation = paste(
      "Convergent and discriminant evidence are triangulated rather than reduced to one cutoff.",
      "HTMT2 is prioritized over original HTMT for congeneric indicators; Fornell-Larcker is optional legacy evidence."
    ),
    recommendation = "Interpret numerical evidence with theory, indicator content, CFA fit, and the intended construct distinctions.",
    rationale = "No single index is sufficient to declare a construct valid or invalid."
  )

  weak_loadings <- if (nrow(loadings)) {
    loadings[loadings$attention %in% c("REVIEW", "STRONG REVIEW"), , drop = FALSE]
  } else {
    loadings
  }
  if (nrow(weak_loadings)) {
    for (i in seq_len(nrow(weak_loadings))) {
      log <- nomo_log_add(
        log,
        stage = "validity",
        object = weak_loadings$item[[i]],
        metric = "standardized_loading",
        value = weak_loadings$loading[[i]],
        reference = paste0(
          "configured review reference = ", nomo_present_stat(loading_reference, "loading")
        ),
        severity = if (weak_loadings$attention[[i]] == "STRONG REVIEW") "concern" else "review",
        observation = weak_loadings$explanation[[i]],
        recommendation = "Inspect item content, precision, and model specification; do not automatically delete the indicator.",
        rationale = "Loading strength is one component of convergent evidence and must be interpreted in context."
      )
    }
  }

  if (nrow(ave)) {
    for (i in seq_len(nrow(ave))) {
      severity <- if (ave$attention[[i]] == "concern") {
        "concern"
      } else if (ave$attention[[i]] == "review") {
        "review"
      } else {
        "info"
      }
      log <- nomo_log_add(
        log,
        stage = "validity",
        object = ave$construct[[i]],
        metric = "AVE",
        value = ave$estimate[[i]],
        reference = paste0("configured review reference = ", ave_reference_shown),
        severity = severity,
        observation = ave$interpretation[[i]],
        recommendation = if (ave$attention[[i]] == "unavailable") {
          paste(
            "Use the standardized loadings as this factor's convergent",
            "evidence; do not change indicator membership only to obtain AVE."
          )
        } else if (severity == "info") {
          "Carry AVE forward as one piece of convergent evidence."
        } else {
          "Inspect standardized loadings, item content, error variance, and dimensionality before deciding whether any scale revision is warranted."
        },
        rationale = "AVE summarizes captured indicator variance; it is not a reliability coefficient or a pass/fail validity test."
      )
    }
  }

  # An ordered-indicator fit with the theta parameterization gives a different
  # AVE from the delta fit of the same model. The mean squared standardized
  # loading is the delta value, whatever the parameterization (#145).
  if (nrow(ave) && nomo_measurement_theta(fit_info) && nrow(loadings)) {
    ordered_factors <- unique(loadings$factor[loadings$item %in% fit_info$ordered])
    delta_ave <- vapply(ordered_factors, function(f) {
      mean(loadings$loading[loadings$factor == f]^2)
    }, numeric(1))
    log <- nomo_log_add(
      log,
      stage = "validity",
      object = paste(ordered_factors, collapse = ", "),
      metric = "parameterization",
      reference = "lavaan parameterization = \"theta\"",
      severity = "review",
      observation = paste0(
        "The model was fitted with lavaan's theta parameterization, and ",
        "semTools computes AVE in the model's unstandardized metric, so AVE ",
        "differs from that of a delta-parameterized fit of the same model. The ",
        "mean squared standardized loading, which is AVE under either, is ",
        paste(sprintf(
          "%s for %s",
          nomo_present_stat(delta_ave, "reliability", reference = ave_reference),
          ordered_factors
        ), collapse = ", "),
        "."
      ),
      recommendation = paste(
        "Report the mean squared standardized loading, or refit with",
        "`parameterization = \"delta\"`, before comparing AVE with its reference."
      ),
      rationale = "A coefficient should not depend on an identification choice that leaves the model unchanged."
    )
  }

  # A latent correlation is logged when it is flagged, or when its pair has no
  # HTMT-family value and so no other record of its separation (#145).
  if (nrow(latent_correlations)) {
    pair_key <- function(a, b, block) {
      paste(pmin(a, b), pmax(a, b), block, sep = "\r")
    }
    with_htmt <- if (nrow(discriminant)) {
      pair_key(discriminant$construct_1, discriminant$construct_2, discriminant$block)
    } else {
      character()
    }
    logged <- is.finite(latent_correlations$correlation) & (
      latent_correlations$attention != "info" |
        !pair_key(latent_correlations$construct_1, latent_correlations$construct_2,
                  latent_correlations$block) %in% with_htmt
    )
    for (i in which(logged)) {
      row <- latent_correlations[i, , drop = FALSE]
      severity <- row$attention
      log <- nomo_log_add(
        log,
        stage = "validity",
        object = nomo_validity_pair_label(row$construct_1, row$construct_2, row$block),
        metric = "latent_correlation",
        value = row$correlation,
        reference = paste0(
          "configured review reference = ", nomo_present_stat(htmt_reference, "r"),
          ", read from the interval's limit farthest from zero (R\u00f6nkk\u00f6 & Cho, 2022)"
        ),
        severity = severity,
        observation = row$interpretation,
        recommendation = if (severity == "info") {
          "Interpret this alongside HTMT-family evidence and theoretical distinctiveness."
        } else {
          paste(
            "Investigate construct overlap, item content, and theory; do not",
            "automatically merge constructs or delete indicators."
          )
        },
        rationale = paste(
          "Two constructs whose latent correlation could exceed the reference",
          "may not be empirically distinct, whether or not HTMT can be computed."
        )
      )
    }
  }

  if (nrow(discriminant)) {
    # The pair is named in model order, as the latent correlation's row is.
    named <- nomo_validity_orient_pairs(
      discriminant, unique(c(loadings[["factor"]], fit_info$latent_names))
    )
    for (i in seq_len(nrow(discriminant))) {
      severity <- if (discriminant$attention[[i]] == "concern") {
        "concern"
      } else if (discriminant$attention[[i]] == "review") {
        "review"
      } else {
        "info"
      }
      log <- nomo_log_add(
        log,
        stage = "validity",
        object = paste(named$construct_1[[i]], named$construct_2[[i]], sep = " vs "),
        metric = discriminant$method[[i]],
        value = discriminant$estimate[[i]],
        reference = paste0("configured review reference = ", htmt_reference_shown),
        severity = severity,
        observation = discriminant$interpretation[[i]],
        recommendation = if (severity == "info") {
          "Interpret this alongside latent correlations and theoretical distinctiveness."
        } else {
          "Investigate construct overlap, item wording/content, cross-loadings, and theory; do not automatically merge constructs or delete indicators."
        },
        rationale = "HTMT-family reference values are review heuristics rather than universal validity thresholds."
      )
    }
  }

  unavailable <- htmt_status[
    htmt_status$requested & !htmt_status$available,
    ,
    drop = FALSE
  ]
  # A statistic that cannot exist for this model, such as HTMT with one
  # construct, is information, not a gap in the evidence to review (#145).
  htmt_applicable <- !identical(htmt_inputs$applicable, FALSE)
  if (nrow(unavailable)) {
    for (i in seq_len(nrow(unavailable))) {
      log <- nomo_log_add(
        log,
        stage = "validity",
        object = "measurement_model",
        metric = unavailable$method[[i]],
        severity = if (htmt_applicable) "review" else "info",
        observation = paste(unavailable$method[[i]], "was not computed:", unavailable$reason[[i]]),
        recommendation = if (htmt_applicable) {
          "Use the available measurement evidence and report the stated limitation explicitly rather than substituting a different estimand silently."
        } else {
          "No action needed: HTMT is not defined for this model, and the latent correlations, if any, carry the construct-separation evidence."
        },
        rationale = "Unavailable evidence should be disclosed, not manufactured by changing the analysis."
      )
    }
  }
  single <- htmt_inputs$single
  if (length(single) && nrow(htmt_status) && any(htmt_status$available)) {
    log <- nomo_log_add(
      log,
      stage = "validity",
      object = paste(single, collapse = ", "),
      metric = "htmt_single_indicator",
      value = length(single),
      reference = "semTools::htmt()",
      severity = "info",
      observation = paste0(
        "HTMT-family values are not defined for pairs that include ",
        paste(single, collapse = ", "), ", ",
        nomo_present_noun(length(single), "a construct", "constructs"),
        " with one indicator and so no within-construct correlations. They ",
        "were computed for the other pairs."
      ),
      recommendation = "The latent correlation is the construct-separation evidence for those pairs.",
      rationale = "A statistic is reported only where it is defined."
    )
  }

  computed_htmt <- htmt_status[htmt_status$available, , drop = FALSE]
  if (nrow(computed_htmt)) {
    missing_used <- computed_htmt$missing[[1L]]
    n_used <- computed_htmt$n[[1L]]
    n_fit <- sum(lavaan::lavInspect(fit_info$fit, "nobs"))
    # Under the CFA's own handling, HTMT and the CFA rest on the same cases by
    # construction. Under pairwise deletion the smallest pairwise count is below
    # the CFA's total N, but the CFA's own correlations rest on the same counts,
    # so only a different handling is compared with the CFA's N (#145).
    same_handling <- identical(
      if (missing_used %in% c("ml", "direct")) "fiml" else missing_used,
      nomo_validity_htmt_missing(fit_info$fit)
    )
    fewer <- !same_handling && isTRUE(n_used < n_fit)
    log <- nomo_log_add(
      log,
      stage = "validity",
      object = "measurement_model",
      metric = "htmt_missing_data",
      value = n_used,
      reference = "semTools::htmt(); lavaan::lavCor()",
      severity = if (fewer) "review" else "info",
      observation = sprintf(
        "HTMT-family correlations used `missing = \"%s\"` (%s): n = %d, %s, of the %d cases the CFA analyzed.",
        missing_used,
        if (identical(htmt_missing, "default")) {
          "the fitted model's own missing-data handling"
        } else if (same_handling) {
          "as requested, the fitted model's own missing-data handling"
        } else {
          "as requested"
        },
        n_used,
        switch(
          missing_used,
          listwise = "the complete cases",
          pairwise = "the smallest number of cases for any pair of indicators",
          "the cases with any indicator observed"
        ),
        n_fit
      ),
      recommendation = if (fewer) {
        paste(
          "HTMT rests on fewer cases than the CFA. Report both sample sizes, or",
          "set `htmt_missing` to the CFA's missing-data handling."
        )
      } else if (same_handling) {
        paste(
          "No action needed; HTMT and the CFA use the same missing-data handling",
          "on the same cases."
        )
      } else {
        "No action needed; HTMT and the CFA rest on the same cases."
      },
      rationale = "Evidence computed on different cases should say so."
    )
  }

  if (length(fit_info$cross_loaded_items)) {
    log <- nomo_log_add(
      log,
      stage = "validity",
      object = paste(fit_info$cross_loaded_items, collapse = ", "),
      metric = "cross_loading",
      severity = "review",
      observation = paste(
        "Cross-loaded indicators were retained for inspection.",
        "semTools::AVE() returns NA for affected factors, and HTMT-family evidence is not computed because trait membership is ambiguous."
      ),
      recommendation = "Return to the theoretical measurement model and cross-loading evidence; do not force simple-structure validity coefficients.",
      rationale = "Changing indicator membership solely to obtain AVE or HTMT would alter the substantive model."
    )
  }

  if (fornell_larcker) {
    log <- nomo_log_add(
      log,
      stage = "validity",
      object = "measurement_model",
      metric = "Fornell-Larcker",
      severity = "info",
      observation = if (!is.null(fornell$matrix)) {
        "A legacy Fornell-Larcker comparison was produced as supporting information."
      } else {
        paste("Legacy Fornell-Larcker output was unavailable:", fornell$reason)
      },
      recommendation = "Do not prioritize the Fornell-Larcker rule over HTMT-family evidence, CFA results, and theory.",
      rationale = "Simulation research shows that legacy discriminant-validity criteria can fail to detect construct overlap in common conditions."
    )
  }

  if (identical(fit_info$post_check, FALSE)) {
    log <- nomo_log_add(
      log,
      stage = "validity",
      object = "measurement_model",
      metric = "lavaan_post_check",
      severity = "concern",
      observation = "lavaan's post-fitting admissibility check did not pass.",
      recommendation = "Investigate the improper solution before interpreting convergent or discriminant evidence.",
      rationale = "Validity summaries can be distorted by inadmissible measurement-model estimates."
    )
  }

  out <- list(
    call = match.call(),
    source = fit_info$source,
    ordered = fit_info$ordered,
    ngroups = fit_info$ngroups,
    nlevels = fit_info$nlevels,
    cross_loaded_items = fit_info$cross_loaded_items,
    ave_obs_var = ave_obs_var,
    loading_reference = loading_reference,
    ave_reference = ave_reference,
    htmt_reference = htmt_reference,
    standardized_loadings = loadings,
    ave_engine = ave_engine,
    ave = ave,
    ave_warnings = ave_warnings,
    latent_correlations = latent_correlations,
    htmt_requested = htmt,
    htmt_missing = htmt_missing,
    htmt_status = htmt_status,
    htmt_applicable = htmt_applicable,
    htmt2_matrix = htmt2_matrix,
    htmt_matrix = htmt_matrix,
    htmt2 = htmt2,
    htmt = htmt_original,
    discriminant = discriminant,
    fornell_larcker_requested = fornell_larcker,
    fornell_larcker = fornell$matrix,
    fornell_larcker_pairs = fornell$pairs,
    fornell_larcker_reason = fornell$reason,
    fit_context = fit_context,
    references = references,
    decision_log = log,
    guidance = guidance,
    fit = fit_info$fit
  )
  class(out) <- c("nomo_validity", "list")
  out
}


# A pair of constructs as the decision log names it, with its group or level
# when the model has more than one block.
nomo_validity_pair_label <- function(construct_1, construct_2, block = "overall") {
  pair <- paste(construct_1, construct_2, sep = " vs ")
  ifelse(block == "overall", pair, paste0(pair, " (", block, ")"))
}


# Latent correlations flagged against the HTMT-family reference (#145). Without
# this, a pair whose latent correlation is .98 went unflagged whenever HTMT
# could not be computed (several groups, a cross-loading, a covariance-matrix
# fit) or was not requested. The value read is the limit of the interval
# farthest from zero, as Ronkko and Cho (2022) recommend, or the estimate when
# there is no interval. A correlation beyond 1 is inadmissible.
nomo_validity_latent_attention <- function(latent, reference) {
  r <- latent$correlation
  limit <- pmax(abs(latent$ci_lower), abs(latent$ci_upper))
  read <- ifelse(is.finite(limit), limit, abs(r))
  latent$abs_correlation <- abs(r)
  latent$reference <- reference
  latent$attention <- ifelse(
    !is.finite(r), "unavailable",
    ifelse(abs(r) > 1, "concern", ifelse(read > reference, "review", "info"))
  )
  reference_shown <- nomo_present_stat(reference, "r")
  latent$interpretation <- vapply(seq_len(nrow(latent)), function(i) {
    if (!is.finite(r[[i]])) {
      return("The latent correlation could not be computed.")
    }
    value <- paste0("r = ", nomo_present_stat(r[[i]], "r"))
    interval <- nomo_present_ci(latent$ci_lower[[i]], latent$ci_upper[[i]], kind = "r")
    if (interval != nomo_present_missing) value <- paste0(value, ", 95% CI ", interval)
    beyond <- if (is.finite(limit[[i]]) && limit[[i]] > 1) {
      " The interval runs past 1, which a correlation cannot reach; it is lavaan's symmetric interval."
    } else {
      ""
    }
    switch(
      latent$attention[[i]],
      concern = paste0(
        "The latent correlation (", value, ") is beyond 1 in absolute value, ",
        "which is inadmissible. Inspect the measurement model before ",
        "interpreting construct separation.", beyond
      ),
      review = paste0(
        "The latent correlation (", value, ") could exceed the configured ",
        "review reference (", reference_shown, "): the ",
        if (is.finite(limit[[i]])) "limit of its interval farthest from zero" else "estimate",
        " is ", nomo_present_stat(read[[i]], "r", reference = reference),
        ". The two constructs may not be empirically distinct (R\u00f6nkk\u00f6 & Cho, 2022);",
        " inspect theory, item content, and HTMT-family evidence rather than",
        " automatically merging constructs.",
        beyond
      ),
      paste0(
        "The latent correlation (", value, ") stays within the configured ",
        "review reference (", reference_shown, "). Interpret its magnitude with ",
        "HTMT-family evidence, theory, and the intended distinction between ",
        "constructs.", beyond
      )
    )
  }, character(1))
  latent
}
