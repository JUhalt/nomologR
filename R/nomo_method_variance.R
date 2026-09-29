# Marker-based method variance -----------------------------------------------------

#' Method variance from a marker variable: the comprehensive CFA marker technique
#'
#' `nomo_method_variance()` tests whether a marker variable carries method
#' variance into a measurement model's indicators, whether that variance biases
#' the correlations among the substantive factors, and how much of each factor's
#' reliability it accounts for. It follows the comprehensive CFA marker
#' technique of Williams, Hartman, and Cavazotte (2010).
#'
#' @details
#' **The marker.** A marker variable is theoretically unrelated to the
#' substantive variables, so what it shares with them is taken to be method
#' variance (Lindell & Whitney, 2001). Williams et al. (2010) expand the
#' definition: a marker should also tap one or more of the biases present in the
#' measurement context, such as social desirability, transient mood, or common
#' scale anchors (Podsakoff et al., 2003). A marker chosen only because it is
#' unrelated, such as a demographic, may capture no method variance at all. The
#' marker is measured by two or more indicators, three or more by the authors'
#' recommendation.
#'
#' **Phase I: model comparisons.** Five models are fitted with
#' `lavaan::cfa(std.lv = TRUE)`:
#'
#' 1. *CFA*: the substantive factors and the marker factor, all correlated, with
#'    no method loadings. It supplies the marker indicators' loadings and error
#'    variances.
#' 2. *Baseline*: those loadings and error variances fixed at their CFA values,
#'    so the marker factor keeps its meaning, and the marker made orthogonal to
#'    the substantive factors.
#' 3. *Method-C*: Baseline plus a loading from the marker on every substantive
#'    indicator, constrained equal.
#' 4. *Method-U*: the same loadings, free to differ.
#' 5. *Method-R*: the retained method model with the substantive factor
#'    correlations fixed at their Baseline values.
#'
#' Baseline versus Method-C tests whether marker-based method variance is
#' present. Method-C versus Method-U tests whether its effects are equal, and
#' decides which method model is retained. The retained model versus Method-R
#' tests whether the method variance biases the substantive correlations. Each
#' comparison is a likelihood-ratio test at `alpha`.
#'
#' **Phase II: reliability decomposition.** From the completely standardized
#' estimates, each substantive factor's reliability in the Baseline model is
#' decomposed, using the retained method model, into a substantive part and a
#' method part (Williams et al., 2010, equations 1-3). `method_share` is the
#' method part as a proportion of the Baseline reliability.
#'
#' **Phase III: sensitivity.** Because the method loadings are estimates, the
#' retained model is refitted with them fixed at the upper ends of their 95%
#' and 99% confidence intervals (Method-S(.05) and Method-S(.01)), and the
#' substantive correlations are compared across the models.
#'
#' **What the technique cannot do.** It requires the marker to be orthogonal to
#' the substantive factors. In simulations, with an ideal marker it did not
#' find method variance that was absent. With a nonideal marker it sometimes
#' did, and with either kind it did not recover the substantive correlations
#' accurately (Richardson, Simmering, & Sturman, 2009). The results are evidence
#' about the method variance the chosen marker captures, not corrected
#' estimates, and other sources of method variance may remain (Podsakoff,
#' MacKenzie, & Podsakoff, 2012).
#'
#' @param model A lavaan measurement model for the substantive factors, as a
#'   string or a [nomo_model()] object. Indicators load on one factor each.
#' @param data A data frame holding the substantive and marker indicators.
#' @param marker The marker variable's indicators: two or more numeric column
#'   names that `model` does not use.
#' @param alpha Significance level for the model comparisons. Default `.05`.
#' @param estimator,missing Optional lavaan `estimator` and `missing` options.
#'   With a robust estimator, the comparisons use lavaan's scaled difference
#'   tests.
#' @param marker_name Name given to the marker factor. Default `"Marker"`.
#'
#' @return A `nomo_method_variance` object. The fields to read are:
#'
#'   * `models`: each model's chi-square, degrees of freedom, p-value
#'     (`pvalue`), CFI, TLI, RMSEA, and SRMR. With a robust estimator, the
#'     chi-square is scaled and the indices are robust, as in [nomo_cfa()].
#'   * `comparisons`: the three model comparisons, with their chi-square
#'     differences (`chisq_diff`), degrees of freedom (`df_diff`), and
#'     `p_value`.
#'   * `retained`: `"Method-C"` or `"Method-U"`.
#'   * `method_loadings`: each substantive indicator's standardized substantive
#'     and method loadings in the retained model, the share of its variance the
#'     marker accounts for, and the method loading's `p_value`.
#'   * `reliability`: each substantive factor's Baseline reliability, its
#'     substantive and method parts, and `method_share`.
#'   * `correlations`: each pair of substantive factors (`factor1`, `factor2`)
#'     and their correlation in the CFA, Baseline, retained, Method-S(.05), and
#'     Method-S(.01) models, with p-values in the retained and sensitivity
#'     models (`retained_p_value`, `method_s_05_p_value`,
#'     `method_s_01_p_value`).
#'   * `marker_correlations`: the marker's correlation with each substantive
#'     factor in the CFA model; `factor1` is the substantive factor and
#'     `factor2` the marker.
#'   * `fits` and `decision_log`.
#'
#'   A table with one p-value for a test or an estimate names it `p_value`, as
#'   the rest of the package does. `correlations` has one for each model, so
#'   each is named `<model>_p_value`. In `models`, `pvalue` is the p-value of
#'   each model's chi-square test, named as in the fit tables of [nomo_esem()]
#'   and [nomo_invariance()].
#'
#'   Other fields record the call, the settings used, and the syntax fitted.
#'   They may change between releases and are not part of the stable interface
#'   (see `?nomologR`).
#'
#' @references
#' Lindell, M. K., & Whitney, D. J. (2001). Accounting for common method
#' variance in cross-sectional research designs. *Journal of Applied
#' Psychology, 86*(1), 114-121. \doi{10.1037/0021-9010.86.1.114}
#'
#' Podsakoff, P. M., MacKenzie, S. B., Lee, J.-Y., & Podsakoff, N. P. (2003).
#' Common method biases in behavioral research: A critical review of the
#' literature and recommended remedies. *Journal of Applied Psychology, 88*(5),
#' 879-903. \doi{10.1037/0021-9010.88.5.879}
#'
#' Podsakoff, P. M., MacKenzie, S. B., & Podsakoff, N. P. (2012). Sources of
#' method bias in social science research and recommendations on how to
#' control it. *Annual Review of Psychology, 63*, 539-569.
#' \doi{10.1146/annurev-psych-120710-100452}
#'
#' Richardson, H. A., Simmering, M. J., & Sturman, M. C. (2009). A tale of
#' three perspectives: Examining post hoc statistical techniques for detection
#' and correction of common method variance. *Organizational Research Methods,
#' 12*(4), 762-800. \doi{10.1177/1094428109332834}
#'
#' Williams, L. J., Hartman, N., & Cavazotte, F. (2010). Method variance and
#' marker variables: A review and comprehensive CFA marker technique.
#' *Organizational Research Methods, 13*(3), 477-514.
#' \doi{10.1177/1094428110366036}
#'
#' @examples
#' \donttest{
#' # Two substantive factors and a marker, all sharing a method factor
#' # (simulated).
#' population <- "
#'   A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.6*a4
#'   B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4
#'   M =~ 0.7*m1 + 0.7*m2 + 0.6*m3
#'   CMV =~ 0.3*a1 + 0.3*a2 + 0.3*a3 + 0.3*a4 + 0.3*b1 + 0.3*b2 + 0.3*b3 +
#'          0.3*b4 + 0.3*m1 + 0.3*m2 + 0.3*m3
#'   A ~~ 0.4*B
#'   A ~~ 0*M
#'   B ~~ 0*M
#'   CMV ~~ 0*A + 0*B + 0*M
#' "
#' set.seed(2010)
#' dat <- lavaan::simulateData(population, sample.nobs = 600, standardized = TRUE)
#' mv <- nomo_method_variance(
#'   "A =~ a1 + a2 + a3 + a4\nB =~ b1 + b2 + b3 + b4",
#'   data = dat, marker = c("m1", "m2", "m3")
#' )
#' mv
#' nomo_table(mv, "reliability")
#' }
#' @export
nomo_method_variance <- function(model,
                                 data,
                                 marker,
                                 alpha = 0.05,
                                 estimator = NULL,
                                 missing = NULL,
                                 marker_name = "Marker") {
  if (inherits(model, "nomo_model")) model <- as.character(model)
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
        !nzchar(trimws(model))) {
    stop("`model` must be one non-empty lavaan model string.", call. = FALSE)
  }
  if (!is.data.frame(data) || !nrow(data)) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.numeric(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
        alpha <= 0 || alpha >= 0.5) {
    stop("`alpha` must be one number strictly between 0 and .5.", call. = FALSE)
  }
  if (!is.character(marker_name) || length(marker_name) != 1L ||
        is.na(marker_name) || !nzchar(marker_name)) {
    stop("`marker_name` must be one non-empty name.", call. = FALSE)
  }

  structure <- nomo_method_variance_structure(model)
  nomo_method_variance_check(structure, data, marker, marker_name)

  fit <- function(lines, label) {
    nomo_method_variance_fit(lines, data, estimator, missing, label)
  }

  # Phase I ----------------------------------------------------------------------
  marker_line <- paste0(marker_name, " =~ ", paste(marker, collapse = " + "))
  cfa <- fit(c(model, marker_line), "CFA")
  pe_cfa <- lavaan::parameterEstimates(cfa)
  loadings <- pe_cfa$est[pe_cfa$op == "=~" & pe_cfa$lhs == marker_name]
  errors <- vapply(marker, function(m) {
    pe_cfa$est[pe_cfa$op == "~~" & pe_cfa$lhs == m & pe_cfa$rhs == m]
  }, numeric(1))

  number <- function(x) sprintf("%.10g", x)
  baseline_lines <- c(
    model,
    paste0(marker_name, " =~ ", paste0(number(loadings), "*", marker, collapse = " + ")),
    paste0(marker, " ~~ ", number(errors), "*", marker),
    paste0(marker_name, " ~~ ", paste0("0*", structure$factors, collapse = " + "))
  )
  baseline <- fit(baseline_lines, "Baseline")

  items <- structure$items$item
  method_c_lines <- c(
    baseline_lines,
    paste0(marker_name, " =~ ", paste0("method*", items, collapse = " + "))
  )
  method_u_lines <- c(
    baseline_lines,
    paste0(marker_name, " =~ ", paste(items, collapse = " + "))
  )
  method_c <- fit(method_c_lines, "Method-C")
  method_u <- fit(method_u_lines, "Method-U")

  compare <- function(restricted, free, label, question) {
    test <- suppressWarnings(lavaan::lavTestLRT(free, restricted))
    tibble::tibble(
      comparison = label,
      question = question,
      chisq_diff = test[["Chisq diff"]][[2L]],
      df_diff = as.integer(test[["Df diff"]][[2L]]),
      p_value = test[["Pr(>Chisq)"]][[2L]]
    )
  }
  presence <- compare(baseline, method_c, "Baseline vs. Method-C",
                      "Is marker-based method variance present?")
  equality <- compare(method_c, method_u, "Method-C vs. Method-U",
                      "Are the method effects equal?")
  unequal <- equality$p_value < alpha
  retained_label <- if (unequal) "Method-U" else "Method-C"
  retained <- if (unequal) method_u else method_c
  retained_lines <- if (unequal) method_u_lines else method_c_lines

  pe_base <- lavaan::parameterEstimates(baseline, standardized = TRUE)
  pairs <- nomo_method_variance_pairs(structure$factors)
  fixed_r <- vapply(seq_len(nrow(pairs)), function(i) {
    nomo_method_variance_value(pe_base, pairs$factor1[[i]], "~~", pairs$factor2[[i]], "est")
  }, numeric(1))
  method_r_lines <- c(
    retained_lines,
    if (nrow(pairs)) {
      paste0(pairs$factor1, " ~~ ", number(fixed_r), "*", pairs$factor2)
    }
  )
  bias <- if (nrow(pairs)) {
    method_r <- fit(method_r_lines, "Method-R")
    compare(method_r, retained, paste(retained_label, "vs. Method-R"),
            "Does the method variance bias the substantive correlations?")
  } else {
    method_r <- NULL
    tibble::tibble(
      comparison = paste(retained_label, "vs. Method-R"),
      question = "Does the method variance bias the substantive correlations?",
      chisq_diff = NA_real_, df_diff = NA_integer_, p_value = NA_real_
    )
  }
  comparisons <- dplyr::bind_rows(presence, equality, bias)

  # Phase III ----------------------------------------------------------------------
  pe_ret <- lavaan::parameterEstimates(retained, standardized = TRUE)
  method_rows <- pe_ret[pe_ret$op == "=~" & pe_ret$lhs == marker_name &
                          pe_ret$rhs %in% items, , drop = FALSE]
  sensitivity <- lapply(c(`Method-S(.05)` = 0.05, `Method-S(.01)` = 0.01), function(level) {
    upper <- method_rows$est + stats::qnorm(1 - level / 2) * method_rows$se
    fit(c(
      baseline_lines,
      paste0(marker_name, " =~ ", paste0(number(upper), "*", method_rows$rhs, collapse = " + "))
    ), sprintf("Method-S(%s)", sub("^0", "", format(level))))
  })

  fits <- c(
    list(CFA = cfa, Baseline = baseline, `Method-C` = method_c, `Method-U` = method_u),
    if (!is.null(method_r)) list(`Method-R` = method_r),
    sensitivity
  )
  # Scaled or robust fit where the estimator provides it, as nomo_cfa() and
  # nomo_esem() report it.
  models <- dplyr::bind_rows(lapply(names(fits), function(label) {
    m <- suppressWarnings(lavaan::fitMeasures(fits[[label]]))
    get <- function(...) nomo_cfa_first_measure(m, c(...))$value
    tibble::tibble(
      model = label,
      chisq = get("chisq.scaled", "chisq"),
      df = as.integer(get("df.scaled", "df")),
      pvalue = get("pvalue.scaled", "pvalue"),
      cfi = get("cfi.robust", "cfi.scaled", "cfi"),
      tli = get("tli.robust", "tli.scaled", "tli"),
      rmsea = get("rmsea.robust", "rmsea.scaled", "rmsea"),
      srmr = get("srmr")
    )
  }))

  # Loadings and Phase II -----------------------------------------------------------
  method_loadings <- tibble::tibble(
    factor = structure$items$factor,
    item = items,
    substantive_loading = vapply(seq_along(items), function(i) {
      nomo_method_variance_value(pe_ret, structure$items$factor[[i]], "=~", items[[i]], "std.all")
    }, numeric(1)),
    method_loading = vapply(items, function(item) {
      nomo_method_variance_value(pe_ret, marker_name, "=~", item, "std.all")
    }, numeric(1), USE.NAMES = FALSE),
    p_value = vapply(items, function(item) {
      nomo_method_variance_value(pe_ret, marker_name, "=~", item, "pvalue")
    }, numeric(1), USE.NAMES = FALSE)
  )
  method_loadings$method_variance <- method_loadings$method_loading^2

  reliability <- nomo_method_variance_reliability(
    structure, pe_base, pe_ret, marker_name
  )

  correlations <- nomo_method_variance_correlations(
    pairs, fits, retained_label, sensitivity
  )
  pe_cfa_std <- lavaan::parameterEstimates(cfa, standardized = TRUE)
  marker_correlations <- tibble::tibble(
    factor1 = structure$factors,
    factor2 = marker_name,
    correlation = vapply(structure$factors, function(f) {
      nomo_method_variance_value(pe_cfa_std, f, "~~", marker_name, "std.all")
    }, numeric(1), USE.NAMES = FALSE)
  )

  out <- list(
    call = match.call(),
    model = model,
    marker = marker,
    marker_name = marker_name,
    alpha = alpha,
    estimator = if (is.null(estimator)) NA_character_ else estimator,
    missing = if (is.null(missing)) NA_character_ else missing,
    n = lavaan::lavInspect(cfa, "nobs"),
    models = models,
    comparisons = comparisons,
    retained = retained_label,
    method_loadings = method_loadings,
    reliability = reliability,
    correlations = correlations,
    marker_correlations = marker_correlations,
    fits = fits,
    decision_log = nomo_method_variance_log(
      marker, comparisons, retained_label, reliability, correlations, alpha
    )
  )
  class(out) <- c("nomo_method_variance", "list")
  out
}


# The substantive factors and their indicators, one factor per indicator.
nomo_method_variance_structure <- function(model) {
  partable <- nomo_network_model_table(model)
  measurement <- partable[partable$op == "=~", , drop = FALSE]
  if (!nrow(measurement)) {
    stop("`model` must define at least one factor with `=~`.", call. = FALSE)
  }
  if (anyDuplicated(measurement$rhs)) {
    stop(
      paste(
        "Each indicator must load on one factor. The reliability",
        "decomposition assumes simple structure."
      ),
      call. = FALSE
    )
  }
  list(
    factors = unique(measurement$lhs),
    items = tibble::tibble(factor = measurement$lhs, item = measurement$rhs)
  )
}


nomo_method_variance_check <- function(structure, data, marker, marker_name) {
  if (!is.character(marker) || length(marker) < 2L || anyNA(marker) ||
        anyDuplicated(marker)) {
    stop(
      "`marker` must name two or more distinct columns: the marker's indicators.",
      call. = FALSE
    )
  }
  overlap <- intersect(marker, structure$items$item)
  if (length(overlap)) {
    stop(
      sprintf(
        "Marker indicators cannot also be substantive indicators: %s.",
        paste(overlap, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  columns <- c(structure$items$item, marker)
  absent <- setdiff(columns, names(data))
  if (length(absent)) {
    stop(
      sprintf("Columns not in `data`: %s.", paste(absent, collapse = ", ")),
      call. = FALSE
    )
  }
  numeric <- vapply(data[columns], is.numeric, logical(1))
  if (!all(numeric)) {
    stop(
      sprintf(
        "Indicators must be numeric. Not numeric: %s.",
        paste(columns[!numeric], collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (marker_name %in% c(structure$factors, names(data))) {
    stop(
      sprintf(
        "`marker_name` \"%s\" is already a factor or column; choose another.",
        marker_name
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


nomo_method_variance_fit <- function(lines, data, estimator, missing, label) {
  args <- list(model = paste(lines, collapse = "\n"), data = data, std.lv = TRUE)
  if (!is.null(estimator)) args$estimator <- estimator
  if (!is.null(missing)) args$missing <- missing
  tryCatch(
    suppressWarnings(do.call(lavaan::cfa, args)),
    error = function(e) {
      stop(
        sprintf("The %s model could not be fitted: %s", label, conditionMessage(e)),
        call. = FALSE
      )
    }
  )
}


nomo_method_variance_value <- function(pe, lhs, op, rhs, column) {
  hit <- pe$op == op & ((pe$lhs == lhs & pe$rhs == rhs) | (pe$lhs == rhs & pe$rhs == lhs))
  if (any(hit)) pe[[column]][hit][[1L]] else NA_real_
}


nomo_method_variance_pairs <- function(factors) {
  if (length(factors) < 2L) {
    return(tibble::tibble(factor1 = character(), factor2 = character()))
  }
  combos <- utils::combn(factors, 2L)
  tibble::tibble(factor1 = combos[1L, ], factor2 = combos[2L, ])
}


# Williams et al. (2010), equations 1-3, from completely standardized estimates:
# the Baseline reliability, and its substantive and method parts in the
# retained method model.
nomo_method_variance_reliability <- function(structure, pe_base, pe_ret, marker_name) {
  dplyr::bind_rows(lapply(structure$factors, function(f) {
    items <- structure$items$item[structure$items$factor == f]
    std <- function(pe, lhs, op, rhs) {
      vapply(rhs, function(r) {
        nomo_method_variance_value(pe, if (op == "~~") r else lhs, op, r, "std.all")
      }, numeric(1))
    }
    base_loadings <- std(pe_base, f, "=~", items)
    base_errors <- std(pe_base, NA, "~~", items)
    total <- sum(base_loadings)^2 / (sum(base_loadings)^2 + sum(base_errors))

    substantive <- std(pe_ret, f, "=~", items)
    method <- std(pe_ret, marker_name, "=~", items)
    errors <- std(pe_ret, NA, "~~", items)
    denominator <- sum(substantive)^2 + sum(method)^2 + sum(errors)
    tibble::tibble(
      factor = f,
      reliability_total = total,
      reliability_substantive = sum(substantive)^2 / denominator,
      reliability_method = sum(method)^2 / denominator,
      method_share = (sum(method)^2 / denominator) / total
    )
  }))
}


# A table with one p-value calls it `p_value`, as the rest of the package does;
# this one has a p-value for each model, so each is `<model>_p_value`.
nomo_method_variance_correlations <- function(pairs, fits, retained_label, sensitivity) {
  if (!nrow(pairs)) {
    return(tibble::tibble(
      factor1 = character(), factor2 = character(), cfa = numeric(),
      baseline = numeric(), retained = numeric(), retained_p_value = numeric(),
      method_s_05 = numeric(), method_s_05_p_value = numeric(),
      method_s_01 = numeric(), method_s_01_p_value = numeric()
    ))
  }
  estimates <- lapply(fits, lavaan::parameterEstimates, standardized = TRUE)
  get <- function(label, column) {
    vapply(seq_len(nrow(pairs)), function(i) {
      nomo_method_variance_value(
        estimates[[label]], pairs$factor1[[i]], "~~", pairs$factor2[[i]], column
      )
    }, numeric(1))
  }
  tibble::tibble(
    factor1 = pairs$factor1,
    factor2 = pairs$factor2,
    cfa = get("CFA", "std.all"),
    baseline = get("Baseline", "std.all"),
    retained = get(retained_label, "std.all"),
    retained_p_value = get(retained_label, "pvalue"),
    method_s_05 = get("Method-S(.05)", "std.all"),
    method_s_05_p_value = get("Method-S(.05)", "pvalue"),
    method_s_01 = get("Method-S(.01)", "std.all"),
    method_s_01_p_value = get("Method-S(.01)", "pvalue")
  )
}


nomo_method_variance_log <- function(marker, comparisons, retained, reliability,
                                     correlations, alpha) {
  log <- nomo_log_new()
  number <- function(x) nomo_present_number(x, 2L)
  test_text <- function(row) {
    sprintf(
      "chi-square difference %s on %d df, p %s",
      number(row$chisq_diff), row$df_diff, nomo_present_p_text(row$p_value)
    )
  }

  log <- nomo_log_add(
    log, stage = "method_variance", object = "marker",
    metric = "marker_assumption",
    value = length(marker),
    reference = "Williams, Hartman, & Cavazotte (2010)",
    severity = "info",
    observation = sprintf(
      "The marker is measured by %s and is assumed orthogonal to the substantive factors.",
      paste(marker, collapse = ", ")
    ),
    recommendation = paste(
      "State why the marker is theoretically unrelated to the substantive",
      "variables and which biases in this measurement context it taps. With a",
      "nonideal marker the technique can find method variance that is not",
      "there, and it does not recover substantive correlations accurately",
      "(Richardson, Simmering, & Sturman, 2009).",
      if (length(marker) < 3L) {
        "Williams et al. recommend three or more marker indicators."
      } else {
        ""
      }
    )
  )

  presence <- comparisons[1L, , drop = FALSE]
  present <- presence$p_value < alpha
  log <- nomo_log_add(
    log, stage = "method_variance", object = "Baseline vs. Method-C",
    metric = "method_variance_presence",
    value = presence$p_value,
    reference = sprintf("Likelihood-ratio test at alpha = %s", format(alpha)),
    severity = if (present) "review" else "info",
    observation = sprintf(
      "Marker-based method variance is %s (%s).",
      if (present) "present" else "not detected", test_text(presence)
    ),
    recommendation = if (present) {
      paste(
        "Report the method loadings and the reliability decomposition, and",
        "whether the method variance biases the substantive correlations."
      )
    } else {
      "Absence of evidence for this marker is not evidence against other sources of method variance."
    }
  )

  equality <- comparisons[2L, , drop = FALSE]
  log <- nomo_log_add(
    log, stage = "method_variance", object = "Method-C vs. Method-U",
    metric = "method_variance_equality",
    value = equality$p_value,
    reference = "Method-C is the equal-effects model Lindell and Whitney (2001) assume",
    severity = "info",
    observation = sprintf(
      "The method effects are %s (%s), so %s is retained.",
      if (identical(retained, "Method-U")) "unequal" else "consistent with equality",
      test_text(equality), retained
    ),
    recommendation = if (identical(retained, "Method-U")) {
      "Unequal effects contradict the correlational marker technique's assumption of equal method effects."
    } else {
      "Equal effects are consistent with the correlational marker technique's assumption."
    }
  )

  bias <- comparisons[3L, , drop = FALSE]
  if (is.finite(bias$p_value)) {
    biased <- bias$p_value < alpha
    log <- nomo_log_add(
      log, stage = "method_variance", object = bias$comparison,
      metric = "method_variance_bias",
      value = bias$p_value,
      reference = "Substantive correlations fixed at their Baseline values",
      severity = if (biased) "review" else "info",
      observation = sprintf(
        "The method variance %s the substantive correlations (%s).",
        if (biased) "biases" else "does not detectably bias", test_text(bias)
      ),
      recommendation = if (biased) {
        paste(
          "Compare the Baseline and", retained, "correlations, and report",
          "both; neither is a corrected estimate of the true correlation."
        )
      } else {
        "The Baseline and method-model correlations can be read as equivalent here."
      }
    )

    significant <- correlations$retained_p_value < alpha
    changed <- (significant != (correlations$method_s_05_p_value < alpha) |
                  significant != (correlations$method_s_01_p_value < alpha)) %in% TRUE
    log <- nomo_log_add(
      log, stage = "method_variance", object = "sensitivity",
      metric = "method_variance_sensitivity",
      value = sum(changed),
      reference = "Method loadings at the upper ends of their 95% and 99% intervals",
      severity = if (any(changed)) "review" else "info",
      observation = if (any(changed)) {
        sprintf(
          "With larger method loadings, the significance of %s changes.",
          paste(correlations$factor1[changed], correlations$factor2[changed],
                sep = " with ", collapse = "; ")
        )
      } else {
        paste(
          "With the method loadings at the upper ends of their intervals, no",
          "substantive correlation changes its significance."
        )
      },
      recommendation = "Report the sensitivity models beside the retained model."
    )
  }

  log <- nomo_log_add(
    log, stage = "method_variance", object = "reliability",
    metric = "method_variance_reliability",
    value = max(reliability$method_share),
    reference = "Williams et al. (2010), reliability decomposition",
    severity = "info",
    observation = sprintf(
      "Share of each factor's reliability due to the marker: %s.",
      paste0(reliability$factor, " ", sprintf("%.1f%%", 100 * reliability$method_share),
             collapse = "; ")
    ),
    recommendation = "The rest is substantive; neither part corrects the other."
  )
  log
}


nomo_present_p_text <- function(p) {
  shown <- nomo_present_p(p)
  if (startsWith(shown, "<")) shown else paste("=", shown)
}


#' @export
print.nomo_method_variance <- function(x, ...) {
  nomo_present_header("nomo_method_variance", "Marker-based method variance")
  nomo_present_facts(c(
    sprintf("Marker: %s", paste(x$marker, collapse = ", ")),
    sprintf("N = %d", as.integer(x$n)),
    sprintf("Retained: %s", x$retained)
  ))
  nomo_method_variance_present_comparisons(x$comparisons)
  nomo_method_variance_present_reliability(x$reliability)
  nomo_method_variance_present_correlations(x$correlations)
  nomo_method_variance_present_note()
  invisible(x)
}


#' @export
summary.nomo_method_variance <- function(object, ...) {
  out <- object[c("marker", "n", "retained", "models", "comparisons",
                  "method_loadings", "reliability", "correlations",
                  "marker_correlations", "decision_log")]
  class(out) <- c("summary_nomo_method_variance", "list")
  out
}


#' @export
print.summary_nomo_method_variance <- function(x, ...) {
  nomo_present_header("nomo_method_variance", "Marker-based method variance", summary = TRUE)
  nomo_present_facts(c(
    sprintf("Marker: %s", paste(x$marker, collapse = ", ")),
    sprintf("N = %d", as.integer(x$n)),
    sprintf("Retained: %s", x$retained)
  ))
  nomo_present_section("Models")
  nomo_present_table(
    x$models,
    c("Model" = "model", "Chi-square" = "chisq", "df" = "df", "CFI" = "cfi",
      "TLI" = "tli", "RMSEA" = "rmsea", "SRMR" = "srmr"),
    formats = list(chisq = function(v) nomo_present_number(v, 2L),
                   df = function(v) format(v, trim = TRUE)),
    more = "nomo_table(x, \"models\")"
  )
  nomo_method_variance_present_comparisons(x$comparisons)

  loadings <- x$method_loadings
  loadings$share <- sprintf("%.1f%%", 100 * loadings$method_variance)
  nomo_present_section(sprintf("Loadings in %s (completely standardized)", x$retained))
  nomo_present_table(
    loadings,
    c("Factor" = "factor", "Item" = "item", "Substantive" = "substantive_loading",
      "Method" = "method_loading", "Method variance" = "share", "p" = "p_value"),
    formats = list(p_value = nomo_present_p),
    more = "nomo_table(x, \"loadings\")"
  )
  nomo_method_variance_present_reliability(x$reliability)
  nomo_method_variance_present_correlations(x$correlations)
  nomo_method_variance_present_note()
  invisible(x)
}


nomo_method_variance_present_comparisons <- function(comparisons) {
  nomo_present_section("Model comparisons")
  nomo_present_table(
    comparisons,
    c("Comparison" = "comparison", "Chi-square diff." = "chisq_diff",
      "df" = "df_diff", "p" = "p_value"),
    formats = list(chisq_diff = function(v) nomo_present_number(v, 2L),
                   df_diff = function(v) format(v, trim = TRUE),
                   p_value = nomo_present_p),
    more = "nomo_table(x, \"comparisons\")"
  )
}


nomo_method_variance_present_reliability <- function(reliability) {
  shown <- reliability
  shown$share <- sprintf("%.1f%%", 100 * shown$method_share)
  nomo_present_section("Reliability decomposition")
  nomo_present_table(
    shown,
    c("Factor" = "factor", "Total" = "reliability_total",
      "Substantive" = "reliability_substantive", "Method" = "reliability_method",
      "Method share" = "share"),
    more = "nomo_table(x, \"reliability\")"
  )
}


nomo_method_variance_present_correlations <- function(correlations) {
  if (!nrow(correlations)) return(invisible(NULL))
  shown <- correlations
  shown$pair <- paste(shown$factor1, "with", shown$factor2)
  nomo_present_section("Substantive correlations")
  nomo_present_table(
    shown,
    c("Factors" = "pair", "CFA" = "cfa", "Baseline" = "baseline",
      "Retained" = "retained", "S(.05)" = "method_s_05", "S(.01)" = "method_s_01"),
    more = "nomo_table(x, \"correlations\")"
  )
}


nomo_method_variance_present_note <- function() {
  cat("\n")
  nomo_present_text(
    "Comprehensive CFA marker technique (Williams, Hartman, & Cavazotte, 2010). ",
    "The results describe the method variance this marker captures; they are ",
    "not corrected estimates, and other sources of method variance may remain."
  )
}
