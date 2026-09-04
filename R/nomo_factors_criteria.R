# Additional factor-retention criteria and synthesis helpers -----------------

nomo_factors_criterion_plan <- function(criterion_set) {
  sets <- list(
    minimal = c(
      "parallel",
      "map_original"
    ),
    core = c(
      "parallel",
      "map_original",
      "map_revised",
      "ekc"
    ),
    extended = c(
      "parallel",
      "map_original",
      "map_revised",
      "ekc",
      "nest",
      "hull"
    ),
    all = c(
      "parallel",
      "map_original",
      "map_revised",
      "ekc",
      "nest",
      "hull",
      "comparison_data",
      "kaiser"
    )
  )

  sets[[criterion_set]]
}


nomo_factors_criterion_metadata <- function(id) {
  meta <- list(
    parallel = list(
      method = "Parallel analysis",
      family = "parallel",
      family_method = "Parallel analysis",
      role = "primary",
      reference = "Permutation-based common-factor parallel analysis"
    ),
    map_original = list(
      method = "MAP (original TR2)",
      family = "map",
      family_method = "MAP",
      role = "complementary",
      reference = "Velicer (1976) original MAP"
    ),
    map_revised = list(
      method = "MAP (revised TR4)",
      family = "map",
      family_method = "MAP",
      role = "complementary",
      reference = "Velicer, Eaton, and Fava (2000) revised MAP"
    ),
    ekc = list(
      method = "Empirical Kaiser criterion",
      family = "ekc",
      family_method = "Empirical Kaiser criterion",
      role = "complementary",
      reference = "Braeken and van Assen (2017)"
    ),
    nest = list(
      method = "NEST",
      family = "nest",
      family_method = "NEST",
      role = "extended",
      reference = "Next Eigenvalue Sufficiency Test"
    ),
    hull = list(
      method = "Hull (CAF)",
      family = "hull",
      family_method = "Hull",
      role = "extended",
      reference = "Hull method using PAF and CAF"
    ),
    comparison_data = list(
      method = "Comparison data",
      family = "comparison_data",
      family_method = "Comparison data",
      role = "extended",
      reference = "Ruscio and Roche (2012) comparison data"
    ),
    kaiser = list(
      method = "Kaiser-Guttman (> 1)",
      family = "kaiser",
      family_method = "Kaiser-Guttman",
      role = "legacy",
      reference = "Legacy eigenvalue-greater-than-one rule"
    )
  )

  meta[[id]]
}


nomo_factors_build_criteria <- function(criterion_set,
                                        pa,
                                        map,
                                        corr,
                                        analysis_data,
                                        item_types,
                                        correlation_method,
                                        common_n_available,
                                        max_factors,
                                        n_iter,
                                        quantile,
                                        seed,
                                        guidance,
                                        component_values) {
  plan <- nomo_factors_criterion_plan(criterion_set)
  evidence_rows <- list()
  status_rows <- list()
  details <- list()

  add_available <- function(id, n_factors, detail = NULL, qualification = "") {
    meta <- nomo_factors_criterion_metadata(id)
    evidence_rows[[length(evidence_rows) + 1L]] <<- tibble::tibble(
      criterion = id,
      method = meta$method,
      family = meta$family,
      family_method = meta$family_method,
      n_factors = as.integer(n_factors),
      role = meta$role,
      reference = meta$reference
    )
    status_rows[[length(status_rows) + 1L]] <<- tibble::tibble(
      criterion = id,
      method = meta$method,
      status = "available",
      reason = "",
      qualification = as.character(qualification)
    )
    if (!is.null(detail)) {
      details[[id]] <<- detail
    }
    invisible(NULL)
  }

  add_skipped <- function(id, reason) {
    meta <- nomo_factors_criterion_metadata(id)
    status_rows[[length(status_rows) + 1L]] <<- tibble::tibble(
      criterion = id,
      method = meta$method,
      status = "skipped",
      reason = as.character(reason),
      qualification = ""
    )
    invisible(NULL)
  }

  for (id in plan) {
    if (id == "parallel") {
      add_available(id, pa$n_factors, pa)
      next
    }

    if (id == "map_original") {
      add_available(id, map$n_factors_original, map)
      next
    }

    if (id == "map_revised") {
      add_available(id, map$n_factors_revised, map)
      next
    }

    if (id == "kaiser") {
      add_available(
        id,
        sum(component_values > 1),
        list(component_eigenvalues = component_values)
      )
      next
    }

    if (id == "ekc") {
      if (!common_n_available) {
        add_skipped(
          id,
          paste(
            "EKC needs one common sample size for the analyzed matrix;",
            "pairwise missing-data handling produced varying pairwise Ns."
          )
        )
        next
      }

      result <- nomo_factors_ekc(
        corr = corr,
        n_obs = nrow(analysis_data)
      )
      if (isTRUE(result$available)) {
        ekc_qualification <- if (!identical(correlation_method, "pearson")) {
          paste(
            "EKC is available here with qualification: its reference series is",
            "derived from product-moment sampling behavior, so use with the",
            "selected non-Pearson correlation model is approximate."
          )
        } else {
          ""
        }
        add_available(
          id,
          result$n_factors,
          result$detail,
          qualification = ekc_qualification
        )
      } else {
        add_skipped(id, result$reason)
      }
      next
    }

    continuous_reference_ok <-
      identical(correlation_method, "pearson") &&
      all(item_types$model_type == "continuous")

    if (!continuous_reference_ok) {
      add_skipped(
        id,
        paste(
          "This criterion uses continuous-reference machinery and is not",
          "run automatically for ordinal, binary, mixed, or non-Pearson analyses."
        )
      )
      next
    }

    if (!common_n_available || anyNA(analysis_data)) {
      add_skipped(
        id,
        paste(
          "This criterion is skipped because the current analysis does not",
          "have one complete common sample for all selected indicators."
        )
      )
      next
    }

    if (id == "nest") {
      result <- nomo_factors_nest(
        corr = corr,
        n_obs = nrow(analysis_data),
        n_iter = n_iter,
        seed = nomo_factors_seed_offset(seed, 101L)
      )
      if (isTRUE(result$available)) {
        add_available(id, result$n_factors, result$detail)
      } else {
        add_skipped(id, result$reason)
      }
      next
    }

    if (id == "hull") {
      if (ncol(analysis_data) < 6L) {
        add_skipped(
          id,
          "Hull requires at least six indicators and was not run."
        )
        next
      }

      result <- nomo_factors_hull(
        corr = corr,
        n_obs = nrow(analysis_data),
        n_iter = n_iter,
        quantile = quantile,
        seed = nomo_factors_seed_offset(seed, 202L)
      )
      if (isTRUE(result$available)) {
        add_available(id, result$n_factors, result$detail)
      } else {
        add_skipped(id, result$reason)
      }
      next
    }

    if (id == "comparison_data") {
      cd_population <- nomo_null_default(
        guidance$factor_cd_population,
        max(5000L, nrow(analysis_data))
      )
      cd_population <- max(as.integer(cd_population), nrow(analysis_data))
      cd_samples <- as.integer(
        nomo_null_default(guidance$factor_cd_samples, 100L)
      )
      cd_alpha <- as.numeric(
        nomo_null_default(guidance$factor_cd_alpha, 0.30)
      )

      result <- nomo_factors_cd(
        x = analysis_data,
        max_factors = max_factors,
        n_population = cd_population,
        n_samples = cd_samples,
        alpha = cd_alpha,
        seed = nomo_factors_seed_offset(seed, 303L)
      )
      if (isTRUE(result$available)) {
        add_available(id, result$n_factors, result$detail)
      } else {
        add_skipped(id, result$reason)
      }
      next
    }
  }

  evidence <- dplyr::bind_rows(evidence_rows)
  status <- dplyr::bind_rows(status_rows)

  if (nrow(evidence) > 0L) {
    evidence$criterion <- factor(evidence$criterion, levels = plan)
    evidence <- evidence[order(evidence$criterion), , drop = FALSE]
    evidence$criterion <- as.character(evidence$criterion)
    rownames(evidence) <- NULL
  }

  list(
    evidence = evidence,
    status = status,
    details = details,
    plan = plan
  )
}


nomo_factors_ekc <- function(corr, n_obs) {
  result <- tryCatch(
    suppressMessages(
      suppressWarnings(
        EFAtools::efa_ekc(corr, N = n_obs)
      )
    ),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    return(list(
      available = FALSE,
      n_factors = NA_integer_,
      detail = NULL,
      reason = sprintf("EKC could not be computed: %s", conditionMessage(result))
    ))
  }

  value <- nomo_factors_extract_n(
    result$n_factors,
    preferred = "BvA|Braeken|original"
  )
  if (is.na(value)) {
    return(list(
      available = FALSE,
      n_factors = NA_integer_,
      detail = result,
      reason = "EKC returned no usable factor-count suggestion."
    ))
  }

  list(
    available = TRUE,
    n_factors = value,
    detail = result,
    reason = ""
  )
}


nomo_factors_nest <- function(corr, n_obs, n_iter, seed) {
  result <- nomo_factors_with_seed(
    seed,
    tryCatch(
      suppressMessages(
        suppressWarnings(
          EFAtools::efa_nest(
            corr,
            N = n_obs,
            alpha = 0.05,
            n_datasets = n_iter
          )
        )
      ),
      error = function(e) e
    )
  )

  if (inherits(result, "error")) {
    return(list(
      available = FALSE,
      n_factors = NA_integer_,
      detail = NULL,
      reason = sprintf("NEST could not be computed: %s", conditionMessage(result))
    ))
  }

  value <- nomo_factors_extract_n(result$n_factors)
  list(
    available = !is.na(value),
    n_factors = value,
    detail = result,
    reason = if (is.na(value)) "NEST returned no usable factor-count suggestion." else ""
  )
}


nomo_factors_hull <- function(corr, n_obs, n_iter, quantile, seed) {
  result <- nomo_factors_with_seed(
    seed,
    tryCatch(
      suppressMessages(
        suppressWarnings(
          EFAtools::efa_hull(
            corr,
            N = n_obs,
            estimator = "PAF",
            gof = "CAF",
            eigen_type = "SMC",
            n_datasets = n_iter,
            percent = 100 * quantile,
            decision_rule = "percentile"
          )
        )
      ),
      error = function(e) e
    )
  )

  if (inherits(result, "error")) {
    return(list(
      available = FALSE,
      n_factors = NA_integer_,
      detail = NULL,
      reason = sprintf("Hull could not be computed: %s", conditionMessage(result))
    ))
  }

  value <- nomo_factors_extract_n(result$n_factors, preferred = "CAF")
  list(
    available = !is.na(value),
    n_factors = value,
    detail = result,
    reason = if (is.na(value)) "Hull returned no usable CAF suggestion." else ""
  )
}


nomo_factors_cd <- function(x,
                            max_factors,
                            n_population,
                            n_samples,
                            alpha,
                            seed) {
  result <- nomo_factors_with_seed(
    seed,
    tryCatch(
      suppressMessages(
        suppressWarnings(
          EFAtools::efa_cd(
            x,
            n_factors_max = max_factors,
            N_pop = n_population,
            N_samples = n_samples,
            alpha = alpha,
            cor_method = "pearson"
          )
        )
      ),
      error = function(e) e
    )
  )

  if (inherits(result, "error")) {
    return(list(
      available = FALSE,
      n_factors = NA_integer_,
      detail = NULL,
      reason = sprintf(
        "Comparison data could not be computed: %s",
        conditionMessage(result)
      )
    ))
  }

  value <- nomo_factors_extract_n(result$n_factors)
  list(
    available = !is.na(value),
    n_factors = value,
    detail = result,
    reason = if (is.na(value)) {
      "Comparison data returned no usable factor-count suggestion."
    } else {
      ""
    }
  )
}


nomo_factors_extract_n <- function(x, preferred = NULL) {
  values <- suppressWarnings(as.numeric(x))
  names(values) <- names(x)
  usable <- is.finite(values)
  values <- values[usable]
  if (length(values) == 0L) {
    return(NA_integer_)
  }

  if (!is.null(preferred) && !is.null(names(values))) {
    hit <- grep(preferred, names(values), ignore.case = TRUE)
    if (length(hit) > 0L) {
      return(as.integer(round(values[[hit[[1L]]]])))
    }
  }

  as.integer(round(values[[1L]]))
}


nomo_factors_with_seed <- function(seed, code) {
  old_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit(
    {
      if (old_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  set.seed(seed)
  force(code)
}


nomo_factors_seed_offset <- function(seed, offset) {
  modulus <- .Machine$integer.max - 1
  value <- (abs(as.double(seed)) + as.double(offset)) %% modulus
  as.integer(max(1, value))
}


nomo_factors_synthesis <- function(evidence, parallel_n, status = NULL) {
  skipped_n <- if (is.data.frame(status) && "status" %in% names(status)) {
    sum(status$status == "skipped")
  } else {
    0L
  }

  skipped_note <- if (skipped_n > 0L) {
    sprintf(
      " %d requested method%s %s not evaluated; see criterion status for the documented reason%s.",
      skipped_n,
      if (skipped_n == 1L) "" else "s",
      if (skipped_n == 1L) "was" else "were",
      if (skipped_n == 1L) "" else "s"
    )
  } else {
    ""
  }

  if (!is.data.frame(evidence) || nrow(evidence) == 0L) {
    concordance <- tibble::tibble(
      n_factors = as.integer(parallel_n),
      n_families = 1L,
      families = "Parallel analysis"
    )
    method_concordance <- tibble::tibble(
      n_factors = as.integer(parallel_n),
      n_criteria = 1L,
      methods = "Parallel analysis"
    )
    family_evidence <- tibble::tibble(
      family = "parallel",
      family_method = "Parallel analysis",
      n_factors = as.integer(parallel_n),
      candidates = as.character(parallel_n),
      internally_consistent = TRUE,
      methods = "Parallel analysis"
    )
    return(list(
      plausible_factors = as.integer(parallel_n),
      agreement = "primary_only",
      modal_factors = as.integer(parallel_n),
      support_for_primary = 1L,
      support_for_primary_methods = 1L,
      n_available = 1L,
      n_methods = 1L,
      n_families = 1L,
      min_factors = as.integer(parallel_n),
      max_factors = as.integer(parallel_n),
      concordance = concordance,
      family_concordance = concordance,
      method_concordance = method_concordance,
      family_evidence = family_evidence,
      text = paste0(
        sprintf(
          "Parallel analysis suggests investigating %d factor%s. ",
          parallel_n,
          if (parallel_n == 1L) "" else "s"
        ),
        "No additional recommended criterion families were available for triangulation, ",
        "so treat this as a primary candidate rather than a dimensionality verdict.",
        skipped_note
      )
    ))
  }

  # Legacy rules can be displayed for historical context but do not contribute
  # to the recommended synthesis.
  recommended <- evidence[evidence$role != "legacy", , drop = FALSE]
  if (nrow(recommended) == 0L) {
    recommended <- evidence[evidence$criterion == "parallel", , drop = FALSE]
  }

  if (!"family" %in% names(recommended)) {
    recommended$family <- vapply(
      recommended$criterion,
      function(id) nomo_factors_criterion_metadata(id)$family,
      character(1)
    )
  }
  if (!"family_method" %in% names(recommended)) {
    recommended$family_method <- vapply(
      recommended$criterion,
      function(id) nomo_factors_criterion_metadata(id)$family_method,
      character(1)
    )
  }

  method_concordance <- dplyr::bind_rows(
    lapply(sort(unique(recommended$n_factors)), function(k) {
      methods <- recommended$method[recommended$n_factors == k]
      tibble::tibble(
        n_factors = as.integer(k),
        n_criteria = as.integer(length(methods)),
        methods = paste(methods, collapse = "; ")
      )
    })
  )

  family_ids <- unique(recommended$family)
  family_evidence <- dplyr::bind_rows(
    lapply(family_ids, function(fid) {
      take <- recommended$family == fid
      vals <- sort(unique(recommended$n_factors[take]))
      methods <- unique(recommended$method[take])
      tibble::tibble(
        family = fid,
        family_method = recommended$family_method[which(take)[1L]],
        n_factors = if (length(vals) == 1L) as.integer(vals) else NA_integer_,
        candidates = paste(vals, collapse = ", "),
        internally_consistent = length(vals) == 1L,
        methods = paste(methods, collapse = "; ")
      )
    })
  )

  resolved_families <- family_evidence[
    family_evidence$internally_consistent & !is.na(family_evidence$n_factors),
    ,
    drop = FALSE
  ]

  family_concordance <- if (nrow(resolved_families) > 0L) {
    dplyr::bind_rows(
      lapply(sort(unique(resolved_families$n_factors)), function(k) {
        families <- resolved_families$family_method[resolved_families$n_factors == k]
        tibble::tibble(
          n_factors = as.integer(k),
          n_families = as.integer(length(families)),
          families = paste(families, collapse = "; ")
        )
      })
    )
  } else {
    tibble::tibble(
      n_factors = integer(),
      n_families = integer(),
      families = character()
    )
  }

  n_methods <- nrow(recommended)
  n_families <- nrow(family_evidence)
  all_counts <- sort(unique(recommended$n_factors))
  min_n <- min(all_counts)
  max_n <- max(all_counts)
  plausible <- as.integer(seq(min_n, max_n))

  support_primary_methods <- sum(recommended$n_factors == parallel_n)
  support_primary <- sum(
    resolved_families$internally_consistent &
      resolved_families$n_factors == parallel_n
  )

  family_counts <- if (nrow(resolved_families) > 0L) {
    table(resolved_families$n_factors)
  } else {
    table(integer())
  }
  modal_values <- if (length(family_counts) > 0L) {
    as.integer(names(family_counts)[family_counts == max(family_counts)])
  } else {
    as.integer(parallel_n)
  }

  split_families <- family_evidence[!family_evidence$internally_consistent, , drop = FALSE]
  other_resolved <- resolved_families[resolved_families$n_factors != parallel_n, , drop = FALSE]

  contrast_bits <- character()
  if (nrow(other_resolved) > 0L) {
    for (i in seq_len(nrow(other_resolved))) {
      contrast_bits <- c(
        contrast_bits,
        sprintf(
          "%s points to %d",
          other_resolved$family_method[[i]],
          other_resolved$n_factors[[i]]
        )
      )
    }
  }
  if (nrow(split_families) > 0L) {
    for (i in seq_len(nrow(split_families))) {
      contrast_bits <- c(
        contrast_bits,
        sprintf(
          "%s is internally split across %s",
          split_families$family_method[[i]],
          gsub(", ", " and ", split_families$candidates[[i]], fixed = TRUE)
        )
      )
    }
  }
  contrast_note <- if (length(contrast_bits) > 0L) {
    paste0(" At the criterion-family level, ", paste(contrast_bits, collapse = "; "), ".")
  } else {
    ""
  }

  fully_convergent <-
    nrow(split_families) == 0L &&
    nrow(resolved_families) == n_families &&
    length(unique(resolved_families$n_factors)) == 1L

  if (fully_convergent) {
    text <- paste0(
      sprintf(
        "All %d available criterion famil%s (%d method%s) point to %d factor%s. ",
        n_families,
        if (n_families == 1L) "y" else "ies",
        n_methods,
        if (n_methods == 1L) "" else "s",
        min_n,
        if (min_n == 1L) "" else "s"
      ),
      "Related methods within a family are grouped before concordance is summarized; ",
      "this is strong converging evidence for investigating that solution, not proof of dimensionality.",
      skipped_note
    )
    agreement <- "convergent"
  } else if (support_primary > n_families / 2) {
    text <- paste0(
      sprintf(
        "Parallel analysis suggests %d factor%s, and %d of %d available criterion families point to that same count; ",
        parallel_n,
        if (parallel_n == 1L) "" else "s",
        support_primary,
        n_families
      ),
      sprintf("family-level suggestions range from %d to %d.", min_n, max_n),
      contrast_note,
      " This is converging but not unanimous evidence. Criterion families are related rather than independent votes; compare plausible neighboring EFA solutions.",
      skipped_note
    )
    agreement <- "primary_majority"
  } else if ((max_n - min_n) <= 1L) {
    text <- paste0(
      sprintf(
        "Parallel analysis suggests %d factor%s, while available criterion families differ between %d and %d factors.",
        parallel_n,
        if (parallel_n == 1L) "" else "s",
        min_n,
        max_n
      ),
      contrast_note,
      " Carry both neighboring solutions into EFA and compare interpretability, simple structure, and stability. Criterion families are related rather than independent votes.",
      skipped_note
    )
    agreement <- "near"
  } else {
    text <- paste0(
      sprintf(
        "Parallel analysis suggests %d factor%s, but the %d available criterion families span %d to %d factors.",
        parallel_n,
        if (parallel_n == 1L) "" else "s",
        n_families,
        min_n,
        max_n
      ),
      contrast_note,
      " Retention evidence is materially divergent; inspect criterion-specific assumptions and compare multiple theoretically plausible EFA solutions. Criterion families are related rather than independent votes.",
      skipped_note
    )
    agreement <- "divergent"
  }

  list(
    plausible_factors = plausible,
    agreement = agreement,
    modal_factors = as.integer(modal_values),
    support_for_primary = as.integer(support_primary),
    support_for_primary_methods = as.integer(support_primary_methods),
    n_available = as.integer(n_families),
    n_methods = as.integer(n_methods),
    n_families = as.integer(n_families),
    min_factors = as.integer(min_n),
    max_factors = as.integer(max_n),
    concordance = family_concordance,
    family_concordance = family_concordance,
    method_concordance = method_concordance,
    family_evidence = family_evidence,
    text = text
  )
}
