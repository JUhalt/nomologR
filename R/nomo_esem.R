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
#' nested in the ESEM, and their likelihood-ratio test is reported too, but in
#' large samples it rejects trivial misfit. The decision log also flags
#' cross-loadings at or above the guidance's cross-loading reference and main
#' loadings below its loading reference.
#'
#' @param model A measurement model in which each indicator loads on one factor:
#'   a lavaan model string or a [nomo_model()] object. It defines the factors,
#'   the items, and the target.
#' @param data A data frame with the indicators.
#' @param rotation `"target"` (default) or `"geomin"`.
#' @param ordered Optional character vector of ordered indicators.
#' @param estimator,missing Optional lavaan `estimator` and `missing` options.
#' @param guidance Guidance settings from [nomo_defaults()]; its
#'   `efa_loading_reference` and `efa_crossloading_reference` are used.
#'
#' @return A `nomo_esem` object. The fields to read are:
#'
#'   * `loadings`: each item's standardized loading on each factor in the
#'     ESEM, whether it is the item's main loading or a cross-loading, its
#'     standard error and p-value, and the CFA's loading for main loadings.
#'   * `factor_correlations`: each pair of factors' correlation in the ESEM and
#'     the CFA, and their difference.
#'   * `fit`: both models' chi-square, degrees of freedom, CFI, TLI, RMSEA,
#'     SRMR, AIC, and BIC.
#'   * `comparison`: the likelihood-ratio test of the CFA against the ESEM.
#'   * `esem_fit`, `cfa_fit`, and `decision_log`.
#'
#'   Other fields record the call and the settings used. They may change
#'   between releases and are not part of the stable interface (see
#'   `?nomologR`).
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
  fit_with <- function(fun_args, label) {
    tryCatch(
      suppressWarnings(do.call(lavaan::sem, fun_args)),
      error = function(e) {
        stop(sprintf("The %s could not be fitted: %s", label, conditionMessage(e)), call. = FALSE)
      }
    )
  }
  esem_fit <- fit_with(esem_args, "ESEM")
  cfa_fit <- fit_with(c(list(model = model), args), "CFA")

  std_esem <- lavaan::standardizedSolution(esem_fit)
  std_cfa <- lavaan::standardizedSolution(cfa_fit)
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
    value(std_esem, pairs$factor_1[[i]], "~~", pairs$factor_2[[i]], "est.std")
  }, numeric(1))
  cfa_r <- vapply(seq_len(nrow(pairs)), function(i) {
    value(std_cfa, pairs$factor_1[[i]], "~~", pairs$factor_2[[i]], "est.std")
  }, numeric(1))
  factor_correlations <- tibble::tibble(
    factor_1 = pairs$factor_1,
    factor_2 = pairs$factor_2,
    esem = esem_r,
    cfa = cfa_r,
    difference = esem_r - cfa_r
  )

  fit_row <- function(fit, label) {
    m <- suppressWarnings(lavaan::fitMeasures(fit))
    get <- function(...) nomo_cfa_first_measure(m, c(...))$value
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
  fit <- dplyr::bind_rows(fit_row(esem_fit, "ESEM"), fit_row(cfa_fit, "CFA"))

  test <- suppressWarnings(lavaan::lavTestLRT(esem_fit, cfa_fit))
  comparison <- tibble::tibble(
    delta_chisq = test[["Chisq diff"]][[2L]],
    delta_df = as.integer(test[["Df diff"]][[2L]]),
    p_value = test[["Pr(>Chisq)"]][[2L]]
  )

  out <- list(
    call = match.call(),
    model = model,
    esem_syntax = esem_syntax,
    rotation = rotation,
    n = as.integer(lavaan::lavInspect(esem_fit, "ntotal")),
    loadings = loadings,
    factor_correlations = factor_correlations,
    fit = fit,
    comparison = comparison,
    esem_fit = esem_fit,
    cfa_fit = cfa_fit,
    decision_log = nomo_esem_log(loadings, factor_correlations, fit, comparison,
                                 rotation, guidance),
    guidance = guidance
  )
  class(out) <- c("nomo_esem", "list")
  out
}


nomo_esem_log <- function(loadings, factor_correlations, fit, comparison, rotation,
                          guidance) {
  log <- nomo_log_new()
  number <- function(x) nomo_present_number(x, 2L)
  cross_ref <- guidance$efa_crossloading_reference
  main_ref <- guidance$efa_loading_reference

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

  # TLI and RMSEA penalize the ESEM's extra parameters, so the CFA can fit
  # better on them; the chi-square test rejects trivial misfit in large samples.
  esem <- fit[fit$model == "ESEM", , drop = FALSE]
  cfa <- fit[fit$model == "CFA", , drop = FALSE]
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
        "cross-loadings at zero costs chi-square %s on %d df, p %s. The largest",
        "change in a factor correlation, ESEM minus CFA, is %s."
      ),
      nomo_present_number(esem$tli, 3L), nomo_present_number(esem$rmsea, 3L),
      nomo_present_number(cfa$tli, 3L), nomo_present_number(cfa$rmsea, 3L),
      number(comparison$delta_chisq), comparison$delta_df,
      nomo_present_p_text(comparison$p_value), nomo_present_signed(largest, 2L)
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
      reference = sprintf("Cross-loading reference %s", format(cross_ref)),
      severity = "review",
      observation = sprintf(
        "Cross-loadings at or above %s: %s.", format(cross_ref),
        paste0(cross$item, " on ", cross$factor, " ", number(cross$loading), collapse = "; ")
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
      reference = sprintf("Loading reference %s", format(main_ref)),
      severity = "review",
      observation = sprintf(
        "Main loadings below %s: %s.", format(main_ref),
        paste0(weak$item, " on ", weak$factor, " ", number(weak$loading), collapse = "; ")
      ),
      recommendation = "Inspect the item's content and its cross-loadings together."
    )
  }
  log
}


#' @export
print.nomo_esem <- function(x, ...) {
  nomo_present_header("nomo_esem", "ESEM beside its CFA")
  nomo_present_facts(c(
    sprintf("Rotation: %s", x$rotation),
    sprintf("N = %d", as.integer(x$n)),
    sprintf("Factors: %d", length(unique(x$loadings$factor)))
  ))
  nomo_esem_present_fit(x$fit, x$comparison)
  nomo_esem_present_correlations(x$factor_correlations)
  flagged <- x$decision_log[x$decision_log$severity %in% c("review", "concern"), , drop = FALSE]
  if (nrow(flagged)) {
    nomo_present_section("Flagged")
    nomo_present_bullets(paste0(flagged$severity, ": ", flagged$observation))
  }
  cat("\n")
  nomo_present_text(
    "Cross-loadings are estimated, not fixed at zero (Asparouhov & Muth\u00e9n, ",
    "2009). The comparison is evidence for choosing a model, not a verdict."
  )
  invisible(x)
}


#' @export
summary.nomo_esem <- function(object, ...) {
  out <- object[c("rotation", "n", "loadings", "factor_correlations", "fit",
                  "comparison", "decision_log")]
  class(out) <- c("summary_nomo_esem", "list")
  out
}


#' @export
print.summary_nomo_esem <- function(x, ...) {
  nomo_present_header("nomo_esem", "ESEM beside its CFA", summary = TRUE)
  nomo_present_facts(c(
    sprintf("Rotation: %s", x$rotation),
    sprintf("N = %d", as.integer(x$n))
  ))
  nomo_esem_present_fit(x$fit, x$comparison)

  factors <- unique(x$loadings$factor)
  items <- unique(x$loadings$item)
  wide <- data.frame(.item = items, stringsAsFactors = FALSE)
  for (f in factors) {
    rows <- x$loadings[x$loadings$factor == f, , drop = FALSE]
    wide[[f]] <- rows$loading[match(items, rows$item)]
  }
  main <- x$loadings[x$loadings$role == "main", , drop = FALSE]
  wide$.cfa <- main$cfa_loading[match(items, main$item)]
  nomo_present_section("ESEM loadings (standardized), with the CFA's main loading")
  nomo_present_table(
    wide,
    c("Item" = ".item", stats::setNames(factors, factors), "CFA" = ".cfa"),
    more = "nomo_table(x, \"loadings\")"
  )
  nomo_esem_present_correlations(x$factor_correlations)
  flagged <- x$decision_log[x$decision_log$severity %in% c("review", "concern"), , drop = FALSE]
  if (nrow(flagged)) {
    nomo_present_section("Flagged")
    nomo_present_bullets(paste0(
      flagged$severity, ": ", flagged$observation, " ", flagged$recommendation
    ))
  }
  invisible(x)
}


nomo_esem_present_fit <- function(fit, comparison) {
  nomo_present_section("Fit")
  nomo_present_table(
    fit,
    c("Model" = "model", "Chi-square" = "chisq", "df" = "df", "CFI" = "cfi",
      "TLI" = "tli", "RMSEA" = "rmsea", "SRMR" = "srmr"),
    formats = list(chisq = function(v) nomo_present_number(v, 2L),
                   df = function(v) format(v, trim = TRUE)),
    more = "nomo_table(x, \"fit\")"
  )
  nomo_present_text(
    sprintf(
      "CFA vs. ESEM: chi-square difference %s on %d df, p %s.",
      nomo_present_number(comparison$delta_chisq, 2L), comparison$delta_df,
      nomo_present_p_text(comparison$p_value)
    ),
    indent = 2L
  )
}


nomo_esem_present_correlations <- function(correlations) {
  shown <- correlations
  shown$pair <- paste(shown$factor_1, "with", shown$factor_2)
  nomo_present_section("Factor correlations")
  nomo_present_table(
    shown,
    c("Factors" = "pair", "ESEM" = "esem", "CFA" = "cfa", "Difference" = "difference"),
    formats = list(difference = function(v) nomo_present_signed(v, 3L)),
    more = "nomo_table(x, \"factor_correlations\")"
  )
}
