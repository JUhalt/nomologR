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
#'   variances. For ordinal indicators, `semTools` calculates AVE using the
#'   polychoric correlation structure.
#' @param htmt Which heterotrait-monotrait summaries to request: `"both"`
#'   (default), `"htmt2"`, `"htmt"`, or `"none"`. HTMT2 is prioritized
#'   because its geometric-mean formulation is designed for congeneric
#'   indicators; original HTMT assumes tau-equivalence.
#' @param htmt_missing Missing-data option passed to [semTools::htmt()]. The
#'   default `"default"` delegates the unrestricted-correlation missing-data
#'   handling to `lavaan::lavCor()` rather than silently imposing listwise
#'   deletion. Other supported values are `"listwise"`, `"pairwise"`,
#'   `"direct"`, `"ml"`, and `"fiml"`.
#' @param fornell_larcker Logical. If `TRUE`, also produce a legacy
#'   Fornell-Larcker matrix and pairwise comparison where available. It is not
#'   used as the primary discriminant-validity criterion.
#' @param guidance Guidance settings from [nomo_defaults()]. The AVE and HTMT
#'   references are review prompts rather than universal pass/fail cutoffs.
#'
#' @return A `nomo_validity` object containing standardized loading evidence,
#'   direct `semTools::AVE()` results, latent correlations, HTMT2/HTMT evidence,
#'   optional legacy Fornell-Larcker information, model-fit context, research
#'   references, and a structured decision log.
#'
#' @references
#' Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models
#' with unobservable variables and measurement error. *Journal of Marketing
#' Research, 18*, 39-50. doi:10.2307/3151312
#'
#' Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for
#' assessing discriminant validity in variance-based structural equation
#' modeling. *Journal of the Academy of Marketing Science, 43*, 115-135.
#' doi:10.1007/s11747-014-0403-8
#'
#' Roemer, E., Schuberth, F., & Henseler, J. (2021). HTMT2--An improved
#' criterion for assessing discriminant validity in structural equation
#' modeling. *Industrial Management & Data Systems, 121*, 2637-2650.
#' doi:10.1108/IMDS-02-2021-0082
#'
#' Voorhees, C. M., Brady, M. K., Calantone, R., & Ramirez, E. (2016).
#' Discriminant validity testing in marketing: An analysis, causes for concern,
#' and proposed remedies. *Journal of the Academy of Marketing Science, 44*,
#' 119-134. doi:10.1007/s11747-015-0455-4
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
#' val <- nomo_validity(cfa)
#' val$ave
#' val$htmt2
#' val$decision_log
#' }
nomo_validity <- function(fit,
                          ave_obs_var = TRUE,
                          htmt = c("both", "htmt2", "htmt", "none"),
                          htmt_missing = "default",
                          fornell_larcker = FALSE,
                          guidance = nomo_defaults()) {
  if (!is.logical(ave_obs_var) || length(ave_obs_var) != 1L || is.na(ave_obs_var)) {
    stop("`ave_obs_var` must be TRUE or FALSE.", call. = FALSE)
  }
  htmt <- match.arg(htmt)
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
    ave$reference <- ave_reference
    ave$attention <- ifelse(
      !is.finite(ave$estimate) | ave$estimate < 0 | ave$estimate > 1,
      "concern",
      ifelse(ave$estimate < ave_reference, "review", "info")
    )
    ave$interpretation <- vapply(seq_len(nrow(ave)), function(i) {
      estimate <- ave$estimate[[i]]
      if (!is.finite(estimate) || estimate < 0 || estimate > 1) {
        paste(
          "AVE is unavailable or outside its conventional 0-1 range.",
          "Inspect model admissibility, cross-loadings, and indicator specification before interpretation."
        )
      } else if (estimate < ave_reference) {
        paste0(
          "AVE is below the configured convergent-evidence reference (",
          format(ave_reference, trim = TRUE),
          "). Inspect standardized loadings, indicator-specific error, and content coverage; do not automatically delete items."
        )
      } else {
        paste0(
          "AVE is at or above the configured convergent-evidence reference (",
          format(ave_reference, trim = TRUE),
          "). This contributes convergent evidence but does not establish construct validity by itself."
        )
      }
    }, character(1))
  }

  latent_correlations <- nomo_validity_latent_correlations(fit_info$fit)
  if (nrow(latent_correlations)) {
    latent_correlations$abs_correlation <- abs(latent_correlations$correlation)
    latent_correlations$interpretation <- paste(
      "Model-based latent-factor association; interpret its magnitude with HTMT-family evidence,",
      "theory, and the intended distinction between constructs."
    )
  }

  htmt_inputs <- nomo_validity_htmt_inputs(fit_info)
  htmt2_matrix <- NULL
  htmt_matrix <- NULL
  htmt_status <- tibble::tibble(
    method = character(),
    requested = logical(),
    available = logical(),
    reason = character()
  )

  want_htmt2 <- htmt %in% c("both", "htmt2")
  want_htmt <- htmt %in% c("both", "htmt")

  add_status <- function(method, requested, available, reason = "") {
    tibble::tibble(
      method = method,
      requested = requested,
      available = available,
      reason = reason
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
    if (want_htmt2) {
      htmt2_error <- NULL
      htmt2_matrix <- tryCatch(
        semTools::htmt(
          model = htmt_inputs$model,
          data = htmt_inputs$data,
          missing = htmt_missing,
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
          if (is.null(htmt2_error)) "" else htmt2_error
        )
      )
    }

    if (want_htmt) {
      htmt_error <- NULL
      htmt_matrix <- tryCatch(
        semTools::htmt(
          model = htmt_inputs$model,
          data = htmt_inputs$data,
          missing = htmt_missing,
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
          if (is.null(htmt_error)) "" else htmt_error
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
          method, " exceeds the configured review reference (",
          format(htmt_reference, trim = TRUE),
          "). This raises a construct-separation concern; inspect theoretical distinctiveness, item content, cross-construct overlap, and latent correlations rather than automatically merging or deleting constructs."
        )
      } else {
        paste0(
          method, " does not exceed the configured review reference (",
          format(htmt_reference, trim = TRUE),
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
        reference = paste0("configured review reference = ", loading_reference),
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
        reference = paste0("configured review reference = ", ave_reference),
        severity = severity,
        observation = ave$interpretation[[i]],
        recommendation = if (severity == "info") {
          "Carry AVE forward as one piece of convergent evidence."
        } else {
          "Inspect standardized loadings, item content, error variance, and dimensionality before deciding whether any scale revision is warranted."
        },
        rationale = "AVE summarizes captured indicator variance; it is not a reliability coefficient or a pass/fail validity test."
      )
    }
  }

  if (nrow(discriminant)) {
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
        object = paste(discriminant$construct_1[[i]], discriminant$construct_2[[i]], sep = " vs "),
        metric = discriminant$method[[i]],
        value = discriminant$estimate[[i]],
        reference = paste0("configured review reference = ", htmt_reference),
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
  if (nrow(unavailable)) {
    for (i in seq_len(nrow(unavailable))) {
      log <- nomo_log_add(
        log,
        stage = "validity",
        object = "measurement_model",
        metric = unavailable$method[[i]],
        severity = "review",
        observation = paste(unavailable$method[[i]], "was not computed:", unavailable$reason[[i]]),
        recommendation = "Use the available measurement evidence and report the stated limitation explicitly rather than substituting a different estimand silently.",
        rationale = "Unavailable evidence should be disclosed, not manufactured by changing the analysis."
      )
    }
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
