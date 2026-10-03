# Exploratory structural equation modeling -------------------------------------------

#' Exploratory structural equation modeling beside its CFA
#'
#' `nomo_esem()` fits a measurement model as exploratory structural equation
#' modeling (ESEM), in which every item may load on every factor, and fits the
#' same factors as an independent-clusters CFA, in which each item loads on one
#' factor only. It compares their fit, their factor correlations, and the
#' cross-loadings the CFA fixes at zero.
#'
#' @details
#' **Why ESEM.** A CFA fixes every cross-loading at zero. When items in fact
#' have small cross-loadings, as items of related constructs usually do, the
#' constraint pushes that shared variance into the factor correlations, which
#' are then inflated, and into misfit (Asparouhov & Muthén, 2009). ESEM
#' estimates the cross-loadings instead, within a structural equation model, so
#' fit, standard errors, and further structure remain available (Marsh, Morin,
#' Parker, & Kaur, 2014).
#'
#' **Rotation.** With an a priori structure, Marsh et al. (2014) recommend
#' target rotation: each item's loading on its intended factor is free, and its
#' cross-loadings are targeted towards zero without being fixed there. This is
#' Browne's (2001) partially specified target rotation, `"target"` here, built
#' from `model`. Geomin rotation (`"geomin"`), which uses no target, is
#' available for a more exploratory reading. The solution depends on the
#' rotation, as any exploratory factor solution does.
#'
#' **Reading the comparison.** Marsh et al. (2014) suggest preferring ESEM when
#' it fits better and its factor correlations are lower, which shows that the
#' CFA's zero cross-loadings distort the structure; otherwise the CFA is the
#' more parsimonious account. Fit is compared on TLI and RMSEA, which penalize
#' the ESEM's extra parameters, so the CFA can fit better on them. The CFA is
#' nested in the ESEM, and their chi-square difference test is reported too,
#' but in large samples it rejects trivial misfit. The decision log also flags
#' cross-loadings at or above the guidance's cross-loading reference and main
#' loadings below its loading reference.
#'
#' **Admissibility.** Both models must converge; otherwise the function stops
#' and names the model that did not. The warnings `lavaan` raises are kept, and
#' a negative variance, a standardized loading above 1, or a latent correlation
#' above 1 in either model is recorded as a concern, as [nomo_cfa()] records
#' it. Cases `lavaan` did not use, such as incomplete cases without
#' `missing = "fiml"`, are reported.
#'
#' @param model A measurement model in which each indicator loads on one factor:
#'   a lavaan model string or a [nomo_model()] object. It defines the factors,
#'   the items, and the target.
#' @param data A data frame with the indicators.
#' @param rotation `"target"` (default) or `"geomin"`.
#' @param ordered Optional character vector of ordered indicators.
#' @param estimator,missing Optional lavaan `estimator` and `missing` options.
#' @param guidance Guidance settings from [nomo_defaults()]; its
#'   `efa_loading_reference` and `efa_crossloading_reference` are used, and
#'   they are checked before any model is fitted.
#'
#' @return A `nomo_esem` object. The fields to read are:
#'
#'   * `loadings`: each item's standardized loading on each factor in the
#'     ESEM, whether it is the item's main loading or a cross-loading, its
#'     standard error and p value, and the CFA's loading for main loadings.
#'   * `factor_correlations`: each pair of factors' correlation (`factor1`,
#'     `factor2`) in the ESEM and the CFA, and their difference.
#'   * `models`: both models' chi-square, degrees of freedom, p value, CFI,
#'     TLI, RMSEA, SRMR, AIC, and BIC. With a robust estimator, the chi-square
#'     is scaled and the indices are robust, as in [nomo_cfa()].
#'   * `comparisons`: the chi-square difference test of the CFA against the
#'     ESEM, with `chisq_diff`, `df_diff`, and `p_value`. It is the
#'     likelihood-ratio test under ML, the scaled difference test (Satorra &
#'     Bentler, 2001) under a robust ML estimator, and the scaled-and-shifted
#'     test (Satorra, 2000) under WLSMV, as [lavaan::lavTestLRT()] chooses.
#'   * `n` and `data_n`: the cases the models used and the rows of `data`.
#'   * `fits`: the fitted `lavaan` models, named `ESEM` and `CFA`.
#'   * `engine_warnings`: the warnings `lavaan` raised, as a list with elements
#'     `ESEM`, `CFA`, and `comparison` (the difference test).
#'   * `decision_log`.
#'
#'   Other fields record the call and the settings used. They may change
#'   between releases and are not part of the stable interface (see
#'   `?nomologR`).
#'
#'   `print()` shows both models' fit, the difference test, the factor
#'   correlations in each model, and the flags. `summary()` adds the ESEM
#'   loadings beside the CFA's and each flag's recommendation.
#'
#' @references
#' Asparouhov, T., & Muthén, B. (2009). Exploratory structural equation
#' modeling. *Structural Equation Modeling, 16*(3), 397-438.
#' \doi{10.1080/10705510903008204}
#'
#' Browne, M. W. (2001). An overview of analytic rotation in exploratory factor
#' analysis. *Multivariate Behavioral Research, 36*(1), 111-150.
#' \doi{10.1207/S15327906MBR3601_05}
#'
#' Marsh, H. W., Morin, A. J. S., Parker, P. D., & Kaur, G. (2014). Exploratory
#' structural equation modeling: An integration of the best features of
#' exploratory and confirmatory factor analysis. *Annual Review of Clinical
#' Psychology, 10*, 85-110. \doi{10.1146/annurev-clinpsy-032813-153700}
#'
#' Satorra, A. (2000). Scaled and adjusted restricted tests in multi-sample
#' analysis of moment structures. In R. D. H. Heijmans, D. S. G. Pollock, &
#' A. Satorra (Eds.), *Innovations in multivariate statistical analysis* (pp.
#' 233-247). Springer. \doi{10.1007/978-1-4615-4603-0_17}
#'
#' Satorra, A., & Bentler, P. M. (2001). A scaled difference chi-square test
#' statistic for moment structure analysis. *Psychometrika, 66*(4), 507-514.
#' \doi{10.1007/BF02296192}
#'
#' @examples
#' \donttest{
#' model <- nomo_model(list(
#'   Agency = paste0("ag", 1:4),
#'   Persistence = paste0("pe", 1:4)
#' ))
#' es <- nomo_esem(model, nomo_demo_network)
#' es
#' nomo_table(es, "factor_correlations")
#' }
#' @export
nomo_esem <- function(model,
                      data,
                      rotation = c("target", "geomin"),
                      ordered = NULL,
                      estimator = NULL,
                      missing = NULL,
                      guidance = nomo_defaults()) {
  rotation <- nomo_match_arg(rotation)
  if (inherits(model, "nomo_model")) model <- as.character(model)
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
        !nzchar(trimws(model))) {
    stop("`model` must be one non-empty lavaan model string.", call. = FALSE)
  }
  if (!is.data.frame(data) || !nrow(data)) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  partable <- nomo_network_model_table(model)
  user <- partable[partable$user == 1L, , drop = FALSE]
  if (any(user$op != "=~") || any(user$rhs %in% user$lhs)) {
    stop(
      "`model` must be a measurement model: factors and their indicators only.",
      call. = FALSE
    )
  }
  if (grepl("*", model, fixed = TRUE)) {
    stop(
      "`model` cannot fix or label loadings; the ESEM and the CFA estimate them all.",
      call. = FALSE
    )
  }
  if (anyDuplicated(user$rhs)) {
    stop(
      paste(
        "Each indicator must load on one factor in `model`: that structure is",
        "the CFA and the ESEM's rotation target."
      ),
      call. = FALSE
    )
  }
  structure <- nomo_method_variance_structure(model)
  if (length(structure$factors) < 2L) {
    stop("ESEM needs two or more factors; with one, it is the CFA itself.", call. = FALSE)
  }
  nomo_check_model_variables(model, data)
  # Checked before fitting, so a bad setting is named at once rather than
  # failing inside the decision log after both models are fitted (#145).
  references <- c(
    cross = nomo_guidance_value(guidance, "efa_crossloading_reference"),
    main = nomo_guidance_value(guidance, "efa_loading_reference")
  )

  items <- structure$items$item
  factors <- structure$factors
  model <- paste(
    vapply(factors, function(f) {
      paste(f, "=~", paste(items[structure$items$factor == f], collapse = " + "))
    }, character(1)),
    collapse = "\n"
  )
  esem_syntax <- paste0(
    paste0('efa("esem")*', factors, collapse = " + "),
    " =~ ", paste(items, collapse = " + ")
  )

  args <- list(data = data, std.lv = TRUE)
  if (length(ordered)) args$ordered <- ordered
  if (!is.null(estimator)) args$estimator <- estimator
  if (!is.null(missing)) args$missing <- missing

  esem_args <- c(list(model = esem_syntax), args, list(rotation = rotation))
  if (identical(rotation, "target")) {
    target <- matrix(0, length(items), length(factors), dimnames = list(items, factors))
    mask <- matrix(1, length(items), length(factors), dimnames = list(items, factors))
    mask[cbind(match(items, items), match(structure$items$factor, factors))] <- 0
    esem_args$rotation <- "pst"
    esem_args$rotation.args <- list(target = target, target.mask = mask)
  }
  # lavaan's warnings are kept with the model that raised them, as nomo_cfa()
  # keeps them, so an improper solution is reported rather than muffled (#145).
  engine_warnings <- list(ESEM = character(), CFA = character(), comparison = character())
  collect <- function(expr, label) {
    withCallingHandlers(expr, warning = function(w) {
      text <- trimws(gsub("[[:space:]]+", " ", conditionMessage(w)))
      engine_warnings[[label]] <<- unique(c(engine_warnings[[label]], text))
      invokeRestart("muffleWarning")
    })
  }
  fit_with <- function(fun_args, label) {
    fit <- tryCatch(
      collect(do.call(lavaan::sem, fun_args), label),
      error = function(e) {
        stop(sprintf("The %s could not be fitted: %s", label, conditionMessage(e)), call. = FALSE)
      }
    )
    # A model that did not converge has no fit measures to compare, so the
    # function stops here and names it, with what lavaan said (#145).
    if (!isTRUE(lavaan::lavInspect(fit, "converged"))) {
      said <- engine_warnings[[label]]
      stop(
        sprintf(
          paste(
            "The %s did not converge, so the ESEM and the CFA cannot be compared.",
            "Check that the sample is large enough for the number of indicators",
            "and that each factor is well defined.%s"
          ),
          label,
          if (length(said)) {
            paste0(" lavaan reported: ", sub("[.]?$", ".", paste(said, collapse = " | ")))
          } else {
            ""
          }
        ),
        call. = FALSE
      )
    }
    fit
  }
  esem_fit <- fit_with(esem_args, "ESEM")
  cfa_fit <- fit_with(c(list(model = model), args), "CFA")

  std_esem <- collect(lavaan::standardizedSolution(esem_fit), "ESEM")
  std_cfa <- collect(lavaan::standardizedSolution(cfa_fit), "CFA")
  grid <- expand.grid(item = items, factor = factors, stringsAsFactors = FALSE)
  main <- structure$items$factor[match(grid$item, items)] == grid$factor
  value <- function(std, lhs, op, rhs, column) {
    nomo_method_variance_value(std, lhs, op, rhs, column)
  }
  loadings <- tibble::tibble(
    item = grid$item,
    factor = grid$factor,
    role = ifelse(main, "main", "cross"),
    loading = mapply(value, list(std_esem), grid$factor, "=~", grid$item, "est.std"),
    se = mapply(value, list(std_esem), grid$factor, "=~", grid$item, "se"),
    p_value = mapply(value, list(std_esem), grid$factor, "=~", grid$item, "pvalue"),
    cfa_loading = ifelse(
      main, mapply(value, list(std_cfa), grid$factor, "=~", grid$item, "est.std"), NA_real_
    )
  )
  loadings <- loadings[order(match(loadings$item, items), loadings$role != "main"), , drop = FALSE]

  pairs <- nomo_method_variance_pairs(factors)
  esem_r <- vapply(seq_len(nrow(pairs)), function(i) {
    value(std_esem, pairs$factor1[[i]], "~~", pairs$factor2[[i]], "est.std")
  }, numeric(1))
  cfa_r <- vapply(seq_len(nrow(pairs)), function(i) {
    value(std_cfa, pairs$factor1[[i]], "~~", pairs$factor2[[i]], "est.std")
  }, numeric(1))
  factor_correlations <- tibble::tibble(
    factor1 = pairs$factor1,
    factor2 = pairs$factor2,
    esem = esem_r,
    cfa = cfa_r,
    difference = esem_r - cfa_r
  )

  # The variant of each value is kept, so print() and summary() can say which
  # chi-square and indices they show.
  variants <- list()
  fit_row <- function(fit, label) {
    m <- collect(lavaan::fitMeasures(fit), label)
    get <- function(...) {
      found <- nomo_cfa_first_measure(m, c(...))
      variants[[label]][[sub("[.].*$", "", c(...)[[1L]])]] <<- found$variant
      found$value
    }
    tibble::tibble(
      model = label,
      chisq = get("chisq.scaled", "chisq"),
      df = as.integer(get("df.scaled", "df")),
      pvalue = get("pvalue.scaled", "pvalue"),
      cfi = get("cfi.robust", "cfi.scaled", "cfi"),
      tli = get("tli.robust", "tli.scaled", "tli"),
      rmsea = get("rmsea.robust", "rmsea.scaled", "rmsea"),
      srmr = get("srmr"),
      aic = get("aic"),
      bic = get("bic")
    )
  }
  models <- dplyr::bind_rows(fit_row(esem_fit, "ESEM"), fit_row(cfa_fit, "CFA"))

  test <- collect(lavaan::lavTestLRT(esem_fit, cfa_fit), "comparison")
  comparisons <- tibble::tibble(
    chisq_diff = test[["Chisq diff"]][[2L]],
    df_diff = as.integer(test[["Df diff"]][[2L]]),
    p_value = test[["Pr(>Chisq)"]][[2L]]
  )

  # Improper solutions in either model, as nomo_cfa() finds them (#145).
  fits <- list(ESEM = esem_fit, CFA = cfa_fit)
  standardized <- list(ESEM = std_esem, CFA = std_cfa)
  improper <- dplyr::bind_rows(lapply(names(fits), function(label) {
    found <- nomo_cfa_heywood(
      parameter_estimates = collect(lavaan::parameterEstimates(fits[[label]]), label),
      standardized_solution = standardized[[label]],
      latent_names = factors,
      observed_names = items
    )
    found$model <- rep(label, nrow(found))
    found
  }))

  n <- as.integer(lavaan::lavInspect(esem_fit, "ntotal"))
  out <- list(
    call = match.call(),
    model = model,
    esem_syntax = esem_syntax,
    rotation = rotation,
    ordered = if (is.null(ordered)) character() else ordered,
    estimator = if (is.null(estimator)) NA_character_ else estimator,
    missing = if (is.null(missing)) NA_character_ else missing,
    n = n,
    data_n = nrow(data),
    loadings = loadings,
    factor_correlations = factor_correlations,
    models = models,
    comparisons = comparisons,
    fits = fits,
    engine_warnings = engine_warnings,
    decision_log = nomo_esem_log(loadings, factor_correlations, models, comparisons,
                                 rotation, references,
                                 cases = c(used = n, input = nrow(data)),
                                 engine_warnings = engine_warnings,
                                 improper = improper,
                                 method = nomo_compare_lrt_method(test)),
    fit_variants = variants,
    comparison_method = nomo_compare_lrt_method(test),
    guidance = guidance
  )
  class(out) <- c("nomo_esem", "list")
  out
}


nomo_esem_log <- function(loadings, factor_correlations, models, comparisons, rotation,
                          references, cases = c(used = NA_real_, input = NA_real_),
                          engine_warnings = list(), improper = NULL,
                          method = "standard") {
  log <- nomo_log_new()
  loading <- function(x, reference = NULL) nomo_present_stat(x, "loading", reference = reference)
  cross_ref <- references[["cross"]]
  main_ref <- references[["main"]]

  log <- nomo_log_add(
    log, stage = "esem", object = "rotation",
    metric = "esem_rotation",
    reference = "Marsh, Morin, Parker, & Kaur (2014); Browne (2001)",
    severity = "info",
    observation = if (identical(rotation, "target")) {
      paste(
        "Target rotation: each item's main loading is free and its",
        "cross-loadings are targeted towards zero."
      )
    } else {
      "Geomin rotation, without a target."
    },
    recommendation = "The ESEM solution depends on the rotation, as any exploratory solution does."
  )

  # Cases lavaan did not use are reported, as nomo_cfa() reports them (#145).
  if (is.finite(cases[["used"]])) {
    dropped <- max(0, cases[["input"]] - cases[["used"]])
    log <- nomo_log_add(
      log, stage = "esem", object = "sample", metric = "cases_used",
      value = cases[["used"]],
      reference = "Case retention should remain visible because missing-data handling can change the analyzed sample",
      severity = if (dropped > 0) "review" else "info",
      observation = if (dropped > 0) {
        sprintf(
          "%d of %d input cases were used; %s %s not used by the fitted models.",
          as.integer(cases[["used"]]), as.integer(cases[["input"]]),
          nomo_present_count(as.integer(dropped), "case"), nomo_present_noun(dropped, "was", "were")
        )
      } else {
        sprintf("All %d input cases were used by the fitted models.", as.integer(cases[["input"]]))
      },
      recommendation = if (dropped > 0) {
        paste(
          "Confirm that case loss follows the intended missing-data strategy;",
          "`missing = \"fiml\"` uses incomplete cases with continuous indicators."
        )
      } else {
        "Document the fitted models' sample and missing-data strategy."
      }
    )
  }

  for (label in names(engine_warnings)) {
    for (warning_text in engine_warnings[[label]]) {
      log <- nomo_log_add(
        log, stage = "esem",
        object = if (identical(label, "comparison")) "ESEM vs. CFA" else label,
        metric = "engine_warning", reference = "Engine warnings must remain visible",
        severity = "review", observation = warning_text,
        recommendation = "Inspect the warning in context before interpreting the comparison."
      )
    }
  }

  if (!is.null(improper) && nrow(improper)) {
    for (i in seq_len(nrow(improper))) {
      row <- improper[i, , drop = FALSE]
      log <- nomo_log_add(
        log, stage = "esem", object = paste(row$model, row$object), metric = row$issue,
        value = row$value,
        reference = "Proper solutions should not contain inadmissible variances, loadings, or latent correlations",
        severity = "concern",
        observation = paste0(
          row$explanation, " The value in the ", row$model, " is ",
          nomo_present_stat(row$value, "estimate"), "."
        ),
        recommendation = paste(
          "Do not read the comparison as evidence until the improper solution is",
          "understood; inspect identification, sampling instability, and the",
          "sample size."
        )
      )
    }
  }

  # TLI and RMSEA penalize the ESEM's extra parameters, so the CFA can fit
  # better on them; the chi-square test rejects trivial misfit in large samples.
  esem <- models[models$model == "ESEM", , drop = FALSE]
  cfa <- models[models$model == "CFA", , drop = FALSE]
  favoured <- isTRUE(esem$tli > cfa$tli && esem$rmsea < cfa$rmsea)
  largest <- factor_correlations$difference[which.max(abs(factor_correlations$difference))]
  log <- nomo_log_add(
    log, stage = "esem", object = "ESEM vs. CFA",
    metric = "esem_comparison",
    value = largest,
    reference = "Asparouhov & Muth\u00e9n (2009); Marsh et al. (2014)",
    severity = if (favoured) "review" else "info",
    observation = sprintf(
      paste(
        "ESEM: TLI %s, RMSEA %s. CFA: TLI %s, RMSEA %s. Fixing the",
        "cross-loadings at zero: %s%s. The largest change in a factor",
        "correlation, ESEM minus CFA, is %s."
      ),
      nomo_present_stat(esem$tli, "fit"), nomo_present_stat(esem$rmsea, "fit"),
      nomo_present_stat(cfa$tli, "fit"), nomo_present_stat(cfa$rmsea, "fit"),
      nomo_present_chisq(comparisons$chisq_diff, comparisons$df_diff,
                         comparisons$p_value, delta = TRUE),
      nomo_compare_method_note(method),
      nomo_present_stat(largest, "r", signed = TRUE)
    ),
    recommendation = if (favoured) {
      paste(
        "ESEM fits better even on indices that penalize its extra parameters.",
        "Where its factor correlations are lower, the CFA's zero cross-loadings",
        "are inflating them; Marsh et al. (2014) then prefer the ESEM, or a CFA",
        "with the cross-loadings the items' content supports."
      )
    } else {
      paste(
        "The ESEM does not fit better once its extra parameters are",
        "penalized, so the CFA is the more parsimonious account (Marsh et al.,",
        "2014)."
      )
    }
  )

  cross <- loadings[loadings$role == "cross" & abs(loadings$loading) >= cross_ref, , drop = FALSE]
  if (nrow(cross)) {
    log <- nomo_log_add(
      log, stage = "esem", object = paste(unique(cross$item), collapse = ", "),
      metric = "esem_cross_loading",
      value = max(abs(cross$loading)),
      reference = sprintf("Cross-loading reference %s", loading(cross_ref)),
      severity = "review",
      observation = sprintf(
        "Cross-loadings at or above %s: %s.", loading(cross_ref),
        paste0(cross$item, " on ", cross$factor, ", ",
               loading(cross$loading, reference = cross_ref), collapse = "; ")
      ),
      recommendation = paste(
        "Read the item's content for both factors. A cross-loading is evidence",
        "about the item, not an instruction to remove it."
      )
    )
  }

  weak <- loadings[loadings$role == "main" & abs(loadings$loading) < main_ref, , drop = FALSE]
  if (nrow(weak)) {
    log <- nomo_log_add(
      log, stage = "esem", object = paste(weak$item, collapse = ", "),
      metric = "esem_weak_main_loading",
      value = min(abs(weak$loading)),
      reference = sprintf("Loading reference %s", loading(main_ref)),
      severity = "review",
      observation = sprintf(
        "Main loadings below %s: %s.", loading(main_ref),
        paste0(weak$item, " on ", weak$factor, ", ",
               loading(weak$loading, reference = main_ref), collapse = "; ")
      ),
      recommendation = "Inspect the item's content and its cross-loadings together."
    )
  }
  log
}


# The header names the method's sources, and the facts say how many of the
# cases in `data` the models used (#145).
nomo_esem_present_header <- function(x, summary = FALSE) {
  nomo_present_header("nomo_esem", "ESEM beside its CFA", summary = summary,
                      source = "Asparouhov & Muth\u00e9n (2009); Marsh et al. (2014)")
  nomo_present_facts(c(
    sprintf("Rotation: %s", x$rotation),
    nomo_cfa_cases_fact(x$n, x$data_n),
    sprintf("Factors: %d", length(unique(x$loadings$factor)))
  ))
}


# A flagged row of the decision log is named by its object, except the sample.
nomo_esem_flagged <- function(log, recommendation = FALSE) {
  nomo_present_flagged(log, recommendation = recommendation,
                       unit = ifelse(log$metric == "cases_used", "Cases", log$object))
}


# Which chi-square and which versions of the indices the models table holds.
nomo_esem_version_note <- function(x) {
  v <- x$fit_variants$ESEM
  nomo_cfa_versions_note(
    c(chi_square = v[["chisq"]], CFI = v[["cfi"]], TLI = v[["tli"]], RMSEA = v[["rmsea"]]),
    nomo_cfa_test_label(x$fits$ESEM)
  )
}


nomo_esem_abbreviations <- c("ESEM", "CFA", "CFI", "TLI", "RMSEA", "SRMR", "df")


#' @export
print.nomo_esem <- function(x, ...) {
  nomo_esem_present_header(x)
  nomo_esem_present_fit(x$models, x$comparisons, nomo_esem_version_note(x),
                        x$comparison_method)
  nomo_esem_present_correlations(x$factor_correlations)
  nomo_esem_flagged(x$decision_log)
  nomo_cfa_present_key_note(nomo_esem_abbreviations)
  cat("\n")
  nomo_present_text(
    "Cross-loadings are estimated, not fixed at zero. The comparison is ",
    "evidence for choosing a model, not a verdict."
  )
  nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"models\")"),
    c("the loadings and each flag's recommendation", "every fit index")
  )
  invisible(x)
}


#' @export
summary.nomo_esem <- function(object, ...) {
  out <- object[c("rotation", "n", "data_n", "loadings", "factor_correlations", "models",
                  "comparisons", "comparison_method", "decision_log")]
  out$version_note <- nomo_esem_version_note(object)
  class(out) <- c("summary_nomo_esem", "list")
  out
}


#' @export
print.summary_nomo_esem <- function(x, ...) {
  nomo_esem_present_header(x, summary = TRUE)
  nomo_esem_present_fit(x$models, x$comparisons, x$version_note, x$comparison_method)

  factors <- unique(x$loadings$factor)
  items <- unique(x$loadings$item)
  wide <- data.frame(.item = items, stringsAsFactors = FALSE)
  for (f in factors) {
    rows <- x$loadings[x$loadings$factor == f, , drop = FALSE]
    wide[[f]] <- rows$loading[match(items, rows$item)]
  }
  main <- x$loadings[x$loadings$role == "main", , drop = FALSE]
  wide$.cfa <- main$cfa_loading[match(items, main$item)]
  loading <- function(v) nomo_present_stat(v, "loading")
  columns <- c("Item" = ".item", stats::setNames(factors, factors), "CFA" = ".cfa")
  nomo_present_section("ESEM loadings (standardized), with the CFA's main loading")
  nomo_present_table(
    wide, columns,
    formats = stats::setNames(rep(list(loading), length(columns) - 1L), columns[-1L]),
    more = "nomo_table(x, \"loadings\")"
  )
  nomo_esem_present_correlations(x$factor_correlations)
  nomo_esem_flagged(x$decision_log, recommendation = TRUE)
  nomo_present_key(nomo_cfa_key_entries(nomo_esem_abbreviations))
  cat("\n")
  nomo_present_text(
    "Cross-loadings are estimated, not fixed at zero. The comparison is ",
    "evidence for choosing a model, not a verdict."
  )
  nomo_present_pointer(
    c("nomo_table(x, \"loadings\")", "nomo_table(x, \"decision_log\")"),
    c("each loading's standard error and p value", "the decision log")
  )
  invisible(x)
}


# The fit of both models, with the variants they hold, and the difference test
# written as every chi-square difference in the package is (#144).
nomo_esem_present_fit <- function(models, comparisons, version_note, method) {
  nomo_present_section("Fit")
  nomo_present_table(
    models,
    c("Model" = "model", "Chi-square" = "chisq", "df" = "df", "p" = "pvalue",
      "CFI" = "cfi", "TLI" = "tli", "RMSEA" = "rmsea", "SRMR" = "srmr"),
    formats = nomo_cfa_fit_formats(),
    more = "nomo_table(x, \"models\")"
  )
  if (nzchar(version_note)) nomo_present_text(version_note, indent = 2L)
  test <- nomo_present_chisq(comparisons$chisq_diff, comparisons$df_diff,
                             comparisons$p_value, delta = TRUE)
  nomo_present_text(
    "CFA vs. ESEM: ",
    if (nzchar(test)) {
      paste0(test, nomo_compare_method_note(method))
    } else {
      "no difference test is available"
    },
    ".",
    indent = 2L
  )
}


nomo_esem_present_correlations <- function(correlations) {
  shown <- correlations
  shown$pair <- paste(shown$factor1, "with", shown$factor2)
  r <- function(v) nomo_present_stat(v, "r")
  nomo_present_section("Factor correlations")
  nomo_present_table(
    shown,
    c("Factors" = "pair", "ESEM" = "esem", "CFA" = "cfa", "Difference" = "difference"),
    formats = list(esem = r, cfa = r,
                   difference = function(v) nomo_present_stat(v, "r", signed = TRUE)),
    more = "nomo_table(x, \"factor_correlations\")"
  )
}
