# Reliability evidence ---------------------------------------------------------

#' Model-based reliability evidence
#'
#' `nomo_reliability()` estimates score reliability from a fitted first-order
#' CFA measurement model. The primary estimate is a model-based omega-type
#' composite reliability from [semTools::compRelSEM()]. Coefficient alpha is
#' available as a familiar secondary statistic and is explicitly qualified by
#' its stronger assumptions.
#'
#' The function is deliberately measurement-first: it refuses nonconverged
#' models, structural SEMs, higher-order models, and cross-loaded first-order
#' indicators in the v0.1 workflow. When global CFA strain or an improper
#' solution is present, reliability is still inspectable but the decision log
#' warns that model-based reliability can be distorted by misspecification.
#'
#' Average variance extracted (AVE) is not a reliability coefficient and is
#' intentionally handled by [nomo_validity()] instead.
#'
#' @param fit A fitted [nomo_cfa()] object or a fitted `lavaan` CFA model.
#' @param obs.var Logical passed to [semTools::compRelSEM()]. `TRUE` (default)
#'   uses observed covariances in the denominator; `FALSE` uses model-implied
#'   covariances.
#' @param ordinal_scale Logical passed as `ord.scale` to
#'   [semTools::compRelSEM()]. For an all-ordered composite, `TRUE` (default)
#'   estimates reliability on the actual ordinal-score scale using the
#'   Green-Yang correction implemented by `semTools`; `FALSE` estimates the
#'   latent-response-scale coefficient.
#' @param include_alpha Logical. If `TRUE` (default), also return coefficient
#'   alpha as a secondary statistic where the requested estimand is defined.
#'   For ordered indicators with `ordinal_scale = TRUE`, observed-score alpha is
#'   deliberately reported as unavailable rather than silently switching to
#'   latent-response ("ordinal alpha") or numeric-score alpha. Alpha is not
#'   treated as the preferred reliability estimate for a general congeneric CFA.
#' @param ci Character. `"none"` (default) returns point estimates only.
#'   `"bootstrap"` adds nonparametric percentile confidence intervals by
#'   repeatedly refitting the same CFA with [lavaan::bootstrapLavaan()].
#' @param ci_level Confidence level for bootstrap intervals. Default is `0.95`.
#' @param ci_boot Number of ordinary bootstrap resamples when
#'   `ci = "bootstrap"`. Default is `1000`.
#' @param ci_seed Optional integer seed for reproducible bootstrap intervals.
#' @param guidance Guidance settings from [nomo_defaults()]. The configured
#'   reliability reference is a review prompt, not a pass/fail criterion.
#'
#' @return A `nomo_reliability` object containing direct `semTools` results,
#'   tidy reliability evidence, model-fit context, item-type context, literature
#'   references, and a structured decision log.
#'
#' @references
#' Dunn, T. J., Baguley, T., & Brunsden, V. (2014). From alpha to omega: a
#' practical solution to the pervasive problem of internal consistency
#' estimation. *British Journal of Psychology, 105*, 399-412.
#'
#' Flora, D. B. (2020). Your coefficient alpha is probably wrong, but which
#' coefficient omega is right? A tutorial on using R to obtain better reliability
#' estimates. *Advances in Methods and Practices in Psychological Science, 3*,
#' 484-501.
#'
#' Bell, S. M., Chalmers, R. P., & Flora, D. B. (2024). The impact of
#' measurement model misspecification on coefficient omega estimates of
#' composite reliability. *Educational and Psychological Measurement, 84*, 5-39.
#'
#' Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using
#' structural equation modeling: an alternative to coefficient alpha.
#' *Psychometrika, 74*, 155-167.
#'
#' Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for
#' population reliability coefficients: Evaluation of methods,
#' recommendations, and software for composite measures.
#' *Psychological Methods, 21*, 69-92.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' model <- '
#'   visual  =~ x1 + x2 + x3
#'   textual =~ x4 + x5 + x6
#'   speed   =~ x7 + x8 + x9
#' '
#' cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
#' rel <- nomo_reliability(cfa)
#' rel$evidence
#' rel$decision_log
#' }
nomo_reliability <- function(fit,
                             obs.var = TRUE,
                             ordinal_scale = TRUE,
                             include_alpha = TRUE,
                             ci = c("none", "bootstrap"),
                             ci_level = 0.95,
                             ci_boot = 1000L,
                             ci_seed = NULL,
                             guidance = nomo_defaults()) {
  if (!is.logical(obs.var) || length(obs.var) != 1L || is.na(obs.var)) {
    stop("`obs.var` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(ordinal_scale) || length(ordinal_scale) != 1L ||
      is.na(ordinal_scale)) {
    stop("`ordinal_scale` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(include_alpha) || length(include_alpha) != 1L ||
      is.na(include_alpha)) {
    stop("`include_alpha` must be TRUE or FALSE.", call. = FALSE)
  }

  ci <- match.arg(ci)
  ci_level <- suppressWarnings(as.numeric(ci_level)[1L])
  if (!is.finite(ci_level) || ci_level <= 0 || ci_level >= 1) {
    stop("`ci_level` must be one number strictly between 0 and 1.", call. = FALSE)
  }
  ci_boot <- suppressWarnings(as.integer(ci_boot)[1L])
  if (!is.finite(ci_boot) || ci_boot < 20L) {
    stop("`ci_boot` must be an integer of at least 20.", call. = FALSE)
  }
  if (!is.null(ci_seed)) {
    ci_seed <- suppressWarnings(as.integer(ci_seed)[1L])
    if (!is.finite(ci_seed)) {
      stop("`ci_seed` must be NULL or one finite integer.", call. = FALSE)
    }
  }

  reference <- nomo_guidance_value(guidance, "reliability_reference")
  fit_info <- nomo_measurement_fit(fit)
  type_context <- nomo_reliability_item_types(fit_info)

  mixed <- type_context[type_context$indicator_type == "mixed", , drop = FALSE]
  if (nrow(mixed)) {
    stop(
      paste0(
        "Reliability is not computed for a composite that mixes ordered and continuous indicators. Mixed construct(s): ",
        paste(mixed$construct, collapse = ", "),
        ". `semTools::compRelSEM()` does not define this coefficient for mixed indicator types."
      ),
      call. = FALSE
    )
  }

  omega_engine <- tryCatch(
    semTools::compRelSEM(
      fit_info$fit,
      obs.var = obs.var,
      tau.eq = FALSE,
      ord.scale = ordinal_scale,
      simplify = TRUE
    ),
    error = function(e) {
      stop(
        paste0("Omega/composite-reliability estimation failed: ", conditionMessage(e)),
        call. = FALSE
      )
    }
  )

  # Alpha is deliberately secondary, and its estimand must remain explicit.
  # Current semTools does not permit tau.eq = TRUE together with
  # ord.scale = TRUE for ordered composites.  That combination would conflate
  # alpha for observed integer-scored categories with the model-based
  # latent-response framework used by the ordered CFA.  We therefore compute
  # alpha only where the requested estimand is defined by the fitted model.
  alpha_engine <- NULL
  # Keep a stable zero-row schema so downstream availability checks are
  # warning-free when alpha is intentionally not estimated.
  alpha_tbl <- tibble::tibble(
    construct = character(),
    block = character(),
    metric = character(),
    estimate = numeric()
  )
  alpha_status <- tibble::tibble(
    construct = type_context$construct,
    indicator_type = type_context$indicator_type,
    requested = rep(include_alpha, nrow(type_context)),
    available = rep(FALSE, nrow(type_context)),
    score_scale = rep(NA_character_, nrow(type_context)),
    reason = rep(if (include_alpha) "" else "Coefficient alpha was not requested.", nrow(type_context))
  )

  if (include_alpha) {
    ordered_constructs <- type_context$construct[
      type_context$indicator_type == "ordered"
    ]
    continuous_constructs <- type_context$construct[
      type_context$indicator_type == "continuous"
    ]

    if (length(ordered_constructs) && ordinal_scale) {
      # For models containing continuous factors as well, semTools >= 0.5-9
      # allows tau.eq to name only the composites for which alpha is requested.
      # Ordered composites are intentionally skipped on the observed ordinal
      # scale rather than forcing a non-equivalent alternative.
      if (length(continuous_constructs)) {
        alpha_engine <- tryCatch(
          semTools::compRelSEM(
            fit_info$fit,
            obs.var = obs.var,
            tau.eq = continuous_constructs,
            ord.scale = TRUE,
            simplify = TRUE
          ),
          error = function(e) {
            stop(
              paste0("Coefficient-alpha estimation failed for continuous composites: ", conditionMessage(e)),
              call. = FALSE
            )
          }
        )
        alpha_tbl <- nomo_reliability_tidy(alpha_engine, metric = "alpha", construct_names = fit_info$latent_names)
        alpha_tbl <- alpha_tbl[
          alpha_tbl$construct %in% continuous_constructs,
          ,
          drop = FALSE
        ]
      }

      continuous_idx <- alpha_status$construct %in% continuous_constructs
      alpha_status$available[continuous_idx] <-
        alpha_status$construct[continuous_idx] %in% alpha_tbl$construct
      alpha_status$score_scale[continuous_idx] <- "observed_continuous"
      alpha_status$reason[continuous_idx] <- ifelse(
        alpha_status$available[continuous_idx],
        paste(
          "Alpha was computed for the continuous composite as a secondary statistic.",
          "Its reliability interpretation still depends on essential tau-equivalence."
        ),
        paste(
          "Alpha was requested for the continuous composite but was not returned by the reliability engine.",
          "Inspect the fitted model and engine output before interpretation."
        )
      )

      alpha_status$available[alpha_status$construct %in% ordered_constructs] <- FALSE
      alpha_status$score_scale[alpha_status$construct %in% ordered_constructs] <- "observed_ordinal"
      alpha_status$reason[alpha_status$construct %in% ordered_constructs] <- paste(
        "Observed-scale alpha is not computed from an ordered-indicator CFA.",
        "Current semTools intentionally disallows ord.scale = TRUE with tau-equivalent alpha for ordered composites because that is a different scoring analysis from model-based ordinal reliability.",
        "Use the observed-scale omega estimate as the primary reliability evidence; calculate observed-score alpha separately only when that estimand is substantively required."
      )
    } else {
      alpha_engine <- tryCatch(
        semTools::compRelSEM(
          fit_info$fit,
          obs.var = obs.var,
          tau.eq = TRUE,
          ord.scale = ordinal_scale,
          simplify = TRUE
        ),
        error = function(e) {
          stop(
            paste0("Coefficient-alpha estimation failed: ", conditionMessage(e)),
            call. = FALSE
          )
        }
      )
      alpha_tbl <- nomo_reliability_tidy(alpha_engine, metric = "alpha", construct_names = fit_info$latent_names)
      alpha_status$available <- alpha_status$construct %in% alpha_tbl$construct
      alpha_status$score_scale <- ifelse(
        alpha_status$indicator_type == "ordered",
        "latent_response",
        "observed_continuous"
      )
      alpha_status$reason <- ifelse(
        alpha_status$indicator_type == "ordered",
        paste(
          "Alpha is calculated on the polychoric/latent-response scale.",
          "This is not alpha for the observed summed ordinal score and should be labeled accordingly."
        ),
        paste(
          "Alpha was computed for the continuous composite as a secondary statistic.",
          "Its reliability interpretation depends on essential tau-equivalence."
        )
      )
    }
  }

  omega_tbl <- nomo_reliability_tidy(omega_engine, metric = "omega", construct_names = fit_info$latent_names)

  evidence <- dplyr::bind_rows(omega_tbl, alpha_tbl)
  if (!nrow(evidence)) {
    stop(
      "Reliability coefficients were returned by the engine but could not be converted to a tidy result.",
      call. = FALSE
    )
  }

  evidence$reference <- reference
  evidence$attention <- ifelse(
    !is.finite(evidence$estimate) |
      evidence$estimate < 0 |
      evidence$estimate > 1,
    "concern",
    ifelse(evidence$estimate < reference, "review", "info")
  )

  evidence$interpretation <- vapply(seq_len(nrow(evidence)), function(i) {
    metric <- evidence$metric[[i]]
    estimate <- evidence$estimate[[i]]

    if (!is.finite(estimate) || estimate < 0 || estimate > 1) {
      core <- paste(
        "The coefficient is outside the conventional 0-1 reliability range or unavailable.",
        "Inspect model admissibility, coding, and the measurement model before interpretation."
      )
    } else if (estimate < reference) {
      core <- paste0(
        "The coefficient is below the configured reliability reference (",
        format(reference, trim = TRUE),
        "). This is a prompt to investigate score precision for the intended use, not an automatic scale-revision rule."
      )
    } else {
      core <- paste0(
        "The coefficient is at or above the configured reliability reference (",
        format(reference, trim = TRUE),
        "). This contributes evidence of score consistency for this sample/model but does not establish construct validity."
      )
    }

    if (identical(metric, "alpha")) {
      paste(
        core,
        "Alpha is secondary here because its reliability interpretation relies on stronger assumptions, including essential tau-equivalence, than a general congeneric CFA."
      )
    } else {
      core
    }
  }, character(1))

  evidence$ci_lower <- NA_real_
  evidence$ci_upper <- NA_real_
  evidence$ci_n_success <- NA_integer_

  ci_status <- tibble::tibble(
    method = ci,
    level = ci_level,
    requested_draws = if (identical(ci, "bootstrap")) ci_boot else 0L,
    min_successful_draws = NA_integer_,
    available = FALSE,
    seed = if (is.null(ci_seed)) NA_integer_ else ci_seed,
    reason = if (identical(ci, "none")) {
      "Bootstrap confidence intervals were not requested."
    } else {
      ""
    }
  )

  if (identical(ci, "bootstrap")) {
    ci_result <- nomo_reliability_bootstrap_ci(
      fit_info = fit_info,
      evidence = evidence,
      type_context = type_context,
      obs.var = obs.var,
      ordinal_scale = ordinal_scale,
      include_alpha = include_alpha,
      level = ci_level,
      R = ci_boot,
      seed = ci_seed
    )

    if (nrow(ci_result$intervals)) {
      point_key <- paste(evidence$metric, evidence$construct, evidence$block, sep = "::")
      ci_key <- paste(
        ci_result$intervals$metric,
        ci_result$intervals$construct,
        ci_result$intervals$block,
        sep = "::"
      )
      idx <- match(point_key, ci_key)
      matched <- !is.na(idx)
      evidence$ci_lower[matched] <- ci_result$intervals$ci_lower[idx[matched]]
      evidence$ci_upper[matched] <- ci_result$intervals$ci_upper[idx[matched]]
      evidence$ci_n_success[matched] <- ci_result$intervals$n_success[idx[matched]]
    }
    ci_status <- ci_result$status
  }

  fit_context <- nomo_reliability_fit_context(fit_info$fit, guidance)
  model_strain <- FALSE
  if (nrow(fit_context) && "attention" %in% names(fit_context)) {
    model_strain <- any(fit_context$attention %in% c("review", "concern"), na.rm = TRUE)
  }
  improper <- identical(fit_info$post_check, FALSE)

  references <- tibble::tibble(
    topic = c(
      "omega_over_alpha",
      "omega_selection",
      "model_misspecification",
      "ordinal_reliability",
      "reliability_uncertainty"
    ),
    citation = c(
      "Dunn, Baguley, & Brunsden (2014)",
      "Flora (2020)",
      "Bell, Chalmers, & Flora (2024)",
      "Green & Yang (2009); semTools::compRelSEM()",
      "Kelley & Pornprasertmanit (2016)"
    ),
    purpose = c(
      "Use model-based omega rather than mechanically relying on alpha.",
      "Choose the omega coefficient to match the target score and measurement model.",
      "Interpret model-based reliability in light of measurement-model specification and fit.",
      "For ordered composites, distinguish the actual observed ordinal-score scale from the latent-response scale.",
      "Report interval estimates when sampling uncertainty matters; bootstrap intervals are explicit rather than silently expensive."
    )
  )

  log <- nomo_log_new()
  log <- nomo_log_add(
    log,
    stage = "reliability",
    object = "measurement_model",
    metric = "coefficient_choice",
    reference = "Dunn et al. (2014); Flora (2020)",
    severity = "info",
    observation = paste(
      "Model-based omega is the primary reliability coefficient.",
      "Alpha is secondary when requested."
    ),
    recommendation = "Interpret reliability for the intended score and fitted measurement model rather than selecting the largest coefficient.",
    rationale = "A congeneric CFA generally does not justify treating coefficient alpha as the default reliability estimator."
  )

  ordered_constructs <- type_context$construct[type_context$indicator_type == "ordered"]
  if (length(ordered_constructs)) {
    log <- nomo_log_add(
      log,
      stage = "reliability",
      object = paste(ordered_constructs, collapse = ", "),
      metric = "ordinal_scale",
      reference = "Green & Yang (2009); semTools::compRelSEM()",
      severity = "info",
      observation = if (ordinal_scale) {
        paste(
          "Reliability for all-ordered composites is estimated on the actual ordinal-score scale",
          "using the Green-Yang correction implemented by semTools."
        )
      } else {
        paste(
          "Reliability for all-ordered composites is estimated on the latent-response scale.",
          "That coefficient describes a hypothetical continuous response composite, not the observed summed ordinal score."
        )
      },
      recommendation = if (ordinal_scale) {
        "Use this coefficient when the practical score is formed from the observed ordinal responses."
      } else {
        "Label the latent-response-scale interpretation explicitly in reports."
      },
      rationale = "Observed ordinal scores and their underlying latent responses are different score scales."
    )
  }

  if (model_strain || improper) {
    log <- nomo_log_add(
      log,
      stage = "reliability",
      object = "measurement_model",
      metric = "model_dependence",
      reference = "Bell, Chalmers, & Flora (2024)",
      severity = if (improper) "concern" else "review",
      observation = if (improper) {
        paste(
          "lavaan's post-fitting admissibility check did not pass.",
          "Model-based reliability can be distorted by an improper or misspecified solution."
        )
      } else {
        paste(
          "At least one CFA fit reference is flagged for review.",
          "Model-based omega is conditional on the adequacy of the fitted measurement model."
        )
      },
      recommendation = "Investigate the CFA before treating the reliability coefficient as stable evidence.",
      rationale = "Simulation work shows omega can be biased when the measurement model is misspecified."
    )
  }

  if (include_alpha) {
    skipped_alpha <- alpha_status[!alpha_status$available, , drop = FALSE]
    computed_alpha <- alpha_status[alpha_status$available, , drop = FALSE]

    if (nrow(computed_alpha)) {
      log <- nomo_log_add(
        log,
        stage = "reliability",
        object = paste(computed_alpha$construct, collapse = ", "),
        metric = "alpha_assumptions",
        reference = "Dunn et al. (2014); Flora (2020)",
        severity = "info",
        observation = paste(
          "Coefficient alpha is shown for familiarity and comparison where its requested estimand is defined.",
          "nomologR does not infer essential tau-equivalence merely because alpha is large."
        ),
        recommendation = "Report alpha only with its assumptions and alongside the primary model-based reliability estimate.",
        rationale = "A high alpha can occur even when the measurement model or construct interpretation is problematic."
      )
    }

    if (nrow(skipped_alpha)) {
      for (j in seq_len(nrow(skipped_alpha))) {
        log <- nomo_log_add(
          log,
          stage = "reliability",
          object = skipped_alpha$construct[[j]],
          metric = "alpha_availability",
          reference = "semTools::compRelSEM(); Green & Yang (2009)",
          severity = "info",
          observation = skipped_alpha$reason[[j]],
          recommendation = paste(
            "Do not substitute a numerically different alpha definition without stating the score scale and estimand.",
            "Use the model-based omega result as the primary reliability evidence."
          ),
          rationale = "The package discloses an unavailable secondary coefficient rather than silently changing the reliability estimand."
        )
      }
    }
  }

  for (i in seq_len(nrow(evidence))) {
    sev <- switch(
      evidence$attention[[i]],
      concern = "concern",
      review = "review",
      "info"
    )
    log <- nomo_log_add(
      log,
      stage = "reliability",
      object = evidence$construct[[i]],
      metric = evidence$metric[[i]],
      value = evidence$estimate[[i]],
      reference = paste0("configured review reference = ", reference),
      severity = sev,
      observation = evidence$interpretation[[i]],
      recommendation = if (sev == "info") {
        "Carry this reliability evidence forward with dimensionality, fit, validity, and intended-score context."
      } else {
        "Inspect score purpose, indicator quality, dimensionality, and CFA evidence before changing the scale."
      },
      decision = "",
      rationale = "Reference values trigger review; they do not create automatic retain/delete or valid/invalid decisions."
    )
  }

  omega_tbl <- evidence[evidence$metric == "omega", c(
    "construct", "block", "metric", "estimate",
    "ci_lower", "ci_upper", "ci_n_success"
  ), drop = FALSE]
  alpha_tbl <- evidence[evidence$metric == "alpha", c(
    "construct", "block", "metric", "estimate",
    "ci_lower", "ci_upper", "ci_n_success"
  ), drop = FALSE]

  out <- list(
    call = match.call(),
    source = fit_info$source,
    ordered = fit_info$ordered,
    item_type_context = type_context,
    obs.var = obs.var,
    ordinal_scale = ordinal_scale,
    include_alpha = include_alpha,
    ci = ci,
    ci_level = ci_level,
    ci_boot = ci_boot,
    ci_seed = ci_seed,
    ci_status = ci_status,
    omega_engine = omega_engine,
    alpha_engine = alpha_engine,
    alpha_status = alpha_status,
    omega = omega_tbl,
    alpha = alpha_tbl,
    evidence = evidence,
    fit_context = fit_context,
    model_strain = model_strain,
    improper_solution = improper,
    references = references,
    decision_log = log,
    guidance = guidance,
    fit = fit_info$fit
  )
  class(out) <- c("nomo_reliability", "list")
  out
}
