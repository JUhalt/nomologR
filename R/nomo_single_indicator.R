# Single-indicator latent variables ---------------------------------------------

#' The reliability of a composite modeled as a single indicator
#'
#' `nomo_single_indicator()` records the reliability of an observed composite,
#' such as a scale mean or sum, so that [nomo_network()] can model the
#' composite as a single-indicator latent variable. The composite's error
#' variance is fixed at \eqn{(1 - \rho)\sigma^2}, where \eqn{\rho} is its
#' reliability and \eqn{\sigma^2} its variance as the model analyzes it, so the
#' relationships it enters are corrected for its unreliability. The variance
#' is computed on the cases the model analyzes (the complete cases, under
#' listwise deletion) and with the estimator's denominator, so the fitted
#' model's reliability for the composite is the one supplied.
#'
#' @details
#' **Where the method comes from.** Spearman (1904) corrected a correlation for
#' the unreliability of its two measures. Structural equation modeling carried
#' the idea into the model itself: a composite becomes the one indicator of a
#' latent variable whose error variance is fixed from the composite's
#' reliability, as the SEM textbooks describe (Hayduk, 1987; Bollen, 1989).
#' Applied work in organizational research and marketing took it up where
#' modeling every item was impractical (Williams & Hazer, 1986), and Bagozzi and
#' Heatherton (1994) called it the "total aggregation" model.
#'
#' **Does it work?** Savalei (2019) compared single indicators with path
#' analysis, which ignores measurement error, and with full multiple-indicator
#' SEM, in samples of 30 to 200. Path analysis and single indicators whose
#' reliability was fixed a priori, at a value that slightly overestimated the
#' true one, gave the most accurate estimates and the most power. Single
#' indicators whose reliability was estimated from the same data (coefficient
#' alpha) and full SEM performed in between, and she recommended a
#' fixed-reliability single indicator in small samples. An omega estimated from
#' the same sample, as from [nomo_reliability()] on these data, corresponds to
#' the data-estimated variant. Where the items are available and the sample is
#' large enough, modeling them as indicators remains the fuller correction.
#'
#' **Which reliability.** A coefficient corrects only the error its design can
#' see: internal consistency, for example, leaves transient error in, and the
#' coefficient chosen decides what the correction means (DeShon, 1998). Omega
#' from the composite's own measurement model ([nomo_reliability()]) is the
#' default. Coefficient alpha understates reliability when loadings differ, so
#' it fixes too large an error variance and overcorrects; [nomo_network()] flags
#' it for review. The single indicator is the observed sum or mean, so with
#' ordered items the omega should be the observed-score one
#' (`ordinal_scale = TRUE`, the default); a latent-response omega
#' (`ordinal_scale = FALSE`) describes a hypothetical continuous composite, and
#' taking it brings a warning and is recorded in `source`.
#'
#' **Uncertainty.** A reliability is itself an estimate. Standard errors that
#' treat it as known are too small, and Oberski and Satorra (2013) derived the
#' term to add. Supply `se`, and [nomo_network()] adds it. The derivation
#' treats the reliability as estimated independently of the data; when it comes
#' from the same sample, the added term is an approximation.
#'
#' @param reliability The composite's reliability: one number strictly between
#'   0 and 1, or a [nomo_reliability()] result, whose omega for `construct` is
#'   used.
#' @param se Optional standard error of the reliability. With a
#'   [nomo_reliability()] result that has bootstrap intervals, the default is
#'   approximated from the omega interval, as its width divided by twice the
#'   normal quantile of its level.
#' @param coefficient Which coefficient `reliability` is: `"omega"` (default),
#'   `"alpha"`, or `"other"`.
#' @param source Optional text naming where the reliability comes from, such as
#'   a citation or "this sample". It is recorded in the decision log.
#' @param construct With a [nomo_reliability()] result, the construct whose
#'   omega is used.
#'
#' @return A `nomo_single_indicator` object. The fields to read are
#'   `reliability`, `se` (`NA` when not supplied), `coefficient`, `source`
#'   (`NA` when not supplied), and `construct` (`NA` unless taken from a
#'   [nomo_reliability()] result). `print()` shows the reliability, its
#'   standard error, and where it comes from.
#'
#' @references
#' Bagozzi, R. P., & Heatherton, T. F. (1994). A general approach to
#' representing multifaceted personality constructs: Application to state
#' self-esteem. *Structural Equation Modeling, 1*(1), 35-67.
#' \doi{10.1080/10705519409539961}
#'
#' Bollen, K. A. (1989). *Structural equations with latent variables*. Wiley.
#' \doi{10.1002/9781118619179}
#'
#' DeShon, R. P. (1998). A cautionary note on measurement error corrections in
#' structural equation models. *Psychological Methods, 3*(4), 412-423.
#' \doi{10.1037/1082-989X.3.4.412}
#'
#' Hayduk, L. A. (1987). *Structural equation modeling with LISREL: Essentials
#' and advances*. Johns Hopkins University Press.
#'
#' Oberski, D. L., & Satorra, A. (2013). Measurement error models with
#' uncertainty about the error variance. *Structural Equation Modeling, 20*(3),
#' 409-428. \doi{10.1080/10705511.2013.797820}
#'
#' Savalei, V. (2019). A comparison of several approaches for controlling
#' measurement error in small samples. *Psychological Methods, 24*(3), 352-370.
#' \doi{10.1037/met0000181}
#'
#' Spearman, C. (1904). The proof and measurement of association between two
#' things. *The American Journal of Psychology, 15*(1), 72-101.
#' \doi{10.2307/1412159}
#'
#' Williams, L. J., & Hazer, J. T. (1986). Antecedents and consequences of
#' satisfaction and commitment in turnover models: A reanalysis using latent
#' variable structural equation methods. *Journal of Applied Psychology,
#' 71*(2), 219-231. \doi{10.1037/0021-9010.71.2.219}
#'
#' @seealso [nomo_network()], whose `single_indicators` argument takes these
#'   records.
#'
#' @examples
#' # A published reliability, with its source.
#' nomo_single_indicator(.85, source = "Test manual, Table 4")
#'
#' # Omega from the composite's own measurement model in this sample.
#' cfa <- nomo_cfa("Persistence =~ pe1 + pe2 + pe3 + pe4", nomo_demo_network)
#' nomo_single_indicator(nomo_reliability(cfa), construct = "Persistence")
#' @export
nomo_single_indicator <- function(reliability,
                                  se = NULL,
                                  coefficient = c("omega", "alpha", "other"),
                                  source = NULL,
                                  construct = NULL) {
  coefficient <- nomo_match_arg(coefficient)

  if (!is.null(source) &&
        (!is.character(source) || length(source) != 1L || is.na(source) ||
           !nzchar(trimws(source)))) {
    stop("`source` must be NULL or one non-empty character string.", call. = FALSE)
  }

  if (inherits(reliability, "nomo_reliability")) {
    taken <- nomo_single_indicator_from_reliability(reliability, construct)
    if (!identical(coefficient, "omega")) {
      stop(
        "A `nomo_reliability()` result supplies omega; leave `coefficient` as \"omega\".",
        call. = FALSE
      )
    }
    reliability <- taken$reliability
    if (is.null(se)) se <- taken$se
    construct <- taken$construct
    if (is.null(source)) {
      source <- if (taken$latent_response) {
        "this sample (nomo_reliability(), latent-response scale)"
      } else {
        "this sample (nomo_reliability())"
      }
    }
  } else if (!is.null(construct)) {
    stop(
      "`construct` is used only when `reliability` is a `nomo_reliability()` result.",
      call. = FALSE
    )
  }

  nomo_single_indicator_check_value(reliability)

  if (!is.null(se) &&
        (!is.numeric(se) || length(se) != 1L || !is.finite(se) || se <= 0)) {
    stop("`se` must be NULL or one positive number.", call. = FALSE)
  }

  out <- list(
    reliability = as.numeric(reliability),
    se = if (is.null(se)) NA_real_ else as.numeric(se),
    coefficient = coefficient,
    source = if (is.null(source)) NA_character_ else trimws(source),
    construct = if (is.null(construct)) NA_character_ else construct
  )
  class(out) <- c("nomo_single_indicator", "list")
  out
}


nomo_single_indicator_check_value <- function(reliability) {
  if (!is.numeric(reliability) || length(reliability) != 1L ||
        !is.finite(reliability) || reliability <= 0 || reliability >= 1) {
    stop(
      "A reliability must be one number strictly between 0 and 1.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}


# Omega for one construct from a nomo_reliability() result, with a standard
# error approximated from its bootstrap interval when there is one.
nomo_single_indicator_from_reliability <- function(x, construct) {
  omega <- as.data.frame(x$omega)
  available <- unique(as.character(omega$construct))
  if (is.null(construct)) {
    if (length(available) != 1L) {
      stop(
        paste0(
          "Name the `construct` whose omega to use: one of ",
          paste(available, collapse = ", "), "."
        ),
        call. = FALSE
      )
    }
    construct <- available
  }
  if (!is.character(construct) || length(construct) != 1L ||
        !construct %in% available) {
    stop(
      paste0(
        "`construct` must be one of the constructs in the reliability result: ",
        paste(available, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  row <- omega[omega$construct == construct, , drop = FALSE]
  if (nrow(row) != 1L || !is.finite(row$estimate[[1L]])) {
    stop(
      sprintf(
        "Omega for `%s` is not a single estimate, so it cannot be used as a reliability.",
        construct
      ),
      call. = FALSE
    )
  }
  level <- if (is.numeric(x$ci_level) && length(x$ci_level) == 1L) x$ci_level else 0.95
  width <- row$ci_upper[[1L]] - row$ci_lower[[1L]]
  se <- if (is.finite(width) && width > 0) {
    width / (2 * stats::qnorm(1 - (1 - level) / 2))
  } else {
    NULL
  }

  # The single indicator is the observed sum or mean. With ordered items, a
  # latent-response omega describes a hypothetical continuous composite and
  # fixes too small an error variance for the observed one (#145).
  types <- x$item_type_context
  ordered <- is.data.frame(types) &&
    isTRUE(types$indicator_type[match(construct, types$construct)] == "ordered")
  latent_response <- ordered && isFALSE(x$ordinal_scale)
  if (latent_response) {
    warning(
      sprintf(
        paste(
          "Omega for `%s` is on the latent-response scale (`ordinal_scale =",
          "FALSE`), which describes a hypothetical continuous composite, not",
          "the observed sum or mean a single indicator models. Rerun",
          "nomo_reliability() with `ordinal_scale = TRUE` for the observed-score",
          "omega."
        ),
        construct
      ),
      call. = FALSE
    )
  }
  list(
    reliability = row$estimate[[1L]], se = se, construct = construct,
    latent_response = latent_response
  )
}


#' @export
print.nomo_single_indicator <- function(x, ...) {
  nomo_present_header("nomo_single_indicator", "Reliability for a single indicator")
  # Three decimals: the value is fixed in the model as given (#144).
  nomo_present_facts(c(
    sprintf("Reliability: %s (%s)", nomo_present_stat(x$reliability, "reliability", digits = 3L),
            x$coefficient),
    if (is.finite(x$se)) {
      sprintf("SE: %s", nomo_present_stat(x$se, "reliability", digits = 3L))
    } else {
      ""
    },
    if (!is.na(x$construct)) sprintf("Construct: %s", x$construct) else ""
  ))
  if (!is.na(x$source)) nomo_present_text("Source: ", x$source)
  if (is.finite(x$se)) nomo_present_text("SE = standard error of the reliability.")
  nomo_present_pointer(
    "?nomo_network",
    "how x in `single_indicators` fixes the composite's error variance"
  )
  invisible(x)
}


# In nomo_network() -------------------------------------------------------------

# The composites named in `single_indicators`, checked against the model and
# the data, one row each.
nomo_network_single_spec <- function(single_indicators, data, validation_data,
                                     ordered, prepared) {
  spec <- tibble::tibble(
    variable = character(),
    reliability = numeric(),
    se = numeric(),
    coefficient = character(),
    source = character()
  )
  if (is.null(single_indicators)) return(spec)

  if (is.numeric(single_indicators)) single_indicators <- as.list(single_indicators)
  labels <- names(single_indicators)
  if (!is.list(single_indicators) || !length(single_indicators) ||
        is.null(labels) || any(is.na(labels) | !nzchar(labels)) ||
        anyDuplicated(labels)) {
    stop(
      paste(
        "`single_indicators` must be a named list, or a named numeric vector,",
        "with one entry per composite."
      ),
      call. = FALSE
    )
  }

  # The fitted model includes the relations added from the hypotheses, which
  # may be the only place a composite appears.
  observed <- lavaan::lavNames(nomo_network_model_table(prepared$full_model), "ov")
  rows <- lapply(labels, function(v) {
    if (v %in% prepared$latent) {
      stop(
        sprintf(
          paste(
            "`single_indicators` names `%s`, a latent variable in `model`.",
            "Name the observed composite instead."
          ),
          v
        ),
        call. = FALSE
      )
    }
    if (!v %in% observed) {
      stop(
        sprintf("`single_indicators` names `%s`, which `model` does not use.", v),
        call. = FALSE
      )
    }
    if (v %in% ordered) {
      stop(
        sprintf(
          paste(
            "`%s` is declared ordered. A single indicator needs a continuous",
            "composite, such as a sum or a mean."
          ),
          v
        ),
        call. = FALSE
      )
    }
    for (d in list(data, validation_data)) {
      if (!is.null(d) && !is.numeric(d[[v]])) {
        stop(
          sprintf("`%s` must be numeric to be modeled as a single indicator.", v),
          call. = FALSE
        )
      }
    }

    entry <- single_indicators[[v]]
    if (inherits(entry, "nomo_single_indicator")) {
      record <- entry
    } else if (is.numeric(entry)) {
      nomo_single_indicator_check_value(entry)
      record <- list(
        reliability = entry, se = NA_real_, coefficient = "unspecified",
        source = NA_character_
      )
    } else {
      stop(
        paste(
          "Each entry of `single_indicators` must be a reliability or a",
          "`nomo_single_indicator()` record."
        ),
        call. = FALSE
      )
    }
    tibble::tibble(
      variable = v,
      reliability = as.numeric(record$reliability),
      se = as.numeric(record$se),
      coefficient = record$coefficient,
      source = as.character(record$source)
    )
  })
  dplyr::bind_rows(spec, rows)
}


# The model and data with each composite made the single indicator of a latent
# variable of the same name, its error variance fixed at (1 - reliability)
# times its variance as lavaan analyzes it, so the fitted model's reliability
# for the composite is the one supplied (#145): on the rows lavaan keeps, and
# with its denominator, N for the maximum-likelihood family and in models with
# ordered indicators, N - 1 for the other continuous-data estimators. Under
# full-information estimation a composite with missing values of its own still
# uses its observed values on those rows.
nomo_network_single_apply <- function(model, data, spec, missing = NULL,
                                      estimator = NULL, ordered = character()) {
  table <- tibble::tibble(
    variable = character(),
    indicator = character(),
    reliability = numeric(),
    se = numeric(),
    coefficient = character(),
    source = character(),
    n = integer(),
    variance = numeric(),
    error_variance = numeric()
  )
  if (!nrow(spec)) return(list(model = model, data = data, table = table))

  # lavaan's rows: under listwise deletion, the complete cases on the model's
  # observed variables; otherwise, unless missing = "ml.x", the cases complete
  # on its observed exogenous covariates, which fixed.x = TRUE requires. Each
  # composite becomes an indicator, so it is not one of those covariates.
  mode <- if (is.null(missing)) "listwise" else tolower(missing)
  partable <- nomo_network_model_table(model)
  required <- if (mode %in% c("listwise", "default")) {
    lavaan::lavNames(partable, "ov")
  } else if (mode %in% c("ml.x", "fiml.x")) {
    character()
  } else {
    setdiff(lavaan::lavNames(partable, "ov.x"), spec$variable)
  }
  required <- intersect(required, names(data))
  analyzed <- if (length(required)) {
    stats::complete.cases(data[required])
  } else {
    rep(TRUE, nrow(data))
  }
  n_denominator <- is.null(estimator) || grepl("^ML", toupper(estimator)) ||
    length(ordered) > 0L

  lines <- character()
  rows <- vector("list", nrow(spec))
  for (k in seq_len(nrow(spec))) {
    v <- spec$variable[[k]]
    indicator <- nomo_network_single_name(v, names(data))
    x <- data[[v]]
    used <- x[analyzed & !is.na(x)]
    n_used <- length(used)
    variance <- stats::var(used) * (if (n_denominator) (n_used - 1) / n_used else 1)
    if (!is.finite(variance) || variance <= 0) {
      stop(
        sprintf("`%s` does not vary, so its error variance cannot be fixed.", v),
        call. = FALSE
      )
    }
    data[[indicator]] <- x
    data[[v]] <- NULL
    error_variance <- (1 - spec$reliability[[k]]) * variance
    lines <- c(
      lines,
      sprintf("%s =~ %s", v, indicator),
      sprintf("%s ~~ %s*%s", indicator, sprintf("%.10g", error_variance), indicator)
    )
    rows[[k]] <- tibble::tibble(
      variable = v,
      indicator = indicator,
      reliability = spec$reliability[[k]],
      se = spec$se[[k]],
      coefficient = spec$coefficient[[k]],
      source = spec$source[[k]],
      n = as.integer(n_used),
      variance = variance,
      error_variance = error_variance
    )
  }

  model <- paste(
    c(
      model,
      "",
      "# Single indicators added by nomologR, each error variance fixed at",
      "# (1 - reliability) x the composite's variance",
      lines
    ),
    collapse = "\n"
  )
  list(model = model, data = data, table = dplyr::bind_rows(table, rows))
}


# The indicator takes the composite's name with a suffix no column already has.
nomo_network_single_name <- function(v, taken) {
  candidate <- paste0(v, "_si")
  i <- 1L
  while (candidate %in% taken) {
    i <- i + 1L
    candidate <- paste0(v, "_si", i)
  }
  candidate
}


# Each hypothesis refitted with each composite's reliability shifted, one
# composite at a time. Savalei (2019) found that misestimating a reliability by
# more than about .05 costs accuracy, so the evidence is shown across .10 either
# way. The refits within .05 also give each estimate's rate of change in the
# reliability, from which the variance a reliability's standard error adds is
# computed (Oberski & Satorra, 2013).
nomo_network_single_sensitivity <- function(spec, base, refit,
                                            shifts = c(-0.10, -0.05, 0.05, 0.10),
                                            perturb = seq_len(nrow(spec))) {
  ids <- base$hypothesis_evidence$id
  table <- tibble::tibble(
    variable = character(),
    shift = numeric(),
    reliability = numeric(),
    id = character(),
    estimate = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    concordance = character()
  )
  extra <- stats::setNames(numeric(length(ids)), ids)
  if (!nrow(spec)) return(list(table = table, extra = extra))

  keep <- c("id", "estimate", "ci_lower", "ci_upper", "concordance")
  rows <- list()
  for (k in perturb) {
    rho <- spec$reliability[[k]]
    evidence <- list(base$hypothesis_evidence[, keep, drop = FALSE])
    at <- 0
    for (shift in shifts) {
      shifted <- round(rho + shift, 10)
      if (shifted <= 0 || shifted >= 1) next
      changed <- spec
      changed$reliability[[k]] <- shifted
      refitted <- tryCatch(refit(changed), error = function(e) NULL)
      # A refit that fails still takes its place in the table, as evidence
      # that could not be evaluated at that reliability.
      evidence[[length(evidence) + 1L]] <- if (is.null(refitted)) {
        tibble::tibble(
          id = ids, estimate = NA_real_, ci_lower = NA_real_,
          ci_upper = NA_real_, concordance = "not_evaluable"
        )
      } else {
        refitted$hypothesis_evidence[, keep, drop = FALSE]
      }
      at <- c(at, shift)
    }

    for (j in seq_along(evidence)) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        variable = spec$variable[[k]],
        shift = at[[j]],
        reliability = round(rho + at[[j]], 10),
        evidence[[j]]
      )
    }

    se <- spec$se[[k]]
    if (!is.finite(se)) next
    # Each estimate's rate of change in the reliability: the least-squares
    # slope over the fits within .05, which is the central difference when
    # both sides are available.
    near <- abs(at) <= 0.05 + 1e-9
    for (id in ids) {
      estimate <- vapply(evidence[near], function(e) e$estimate[e$id == id][1L], numeric(1))
      points <- is.finite(estimate)
      if (sum(points) < 2L) next
      slope <- stats::coef(stats::lm(estimate[points] ~ at[near][points]))[[2L]]
      extra[[id]] <- extra[[id]] + slope^2 * se^2
    }
  }

  table <- dplyr::bind_rows(table, rows)
  table <- table[order(
    match(table$variable, spec$variable), match(table$id, ids), table$shift
  ), , drop = FALSE]
  list(table = table, extra = extra)
}


# The sample's hypothesis evidence re-evaluated with the variance the
# reliabilities' uncertainty adds.
nomo_network_single_reevaluate <- function(sample, hypotheses, data, extra,
                                           equivalence_alpha) {
  if (!any(extra > 0)) return(sample)
  sample$hypothesis_evidence <- nomo_network_hypothesis_evidence(
    hypotheses = hypotheses,
    parameter_estimates = sample$parameter_estimates,
    standardized_solution = sample$standardized_solution,
    converged = sample$converged,
    latent = as.character(lavaan::lavNames(sample$fit, type = "lv")),
    data = data,
    measurement_context = sample$measurement_context,
    equivalence_alpha = equivalence_alpha,
    extra_variance = extra
  )
  sample
}


# What the decision log records for each single indicator: the correction, the
# coefficient behind it, how the reliability's uncertainty is handled, and
# whether any hypothesis's evidence depends on the reliability assumed.
nomo_network_single_log <- function(table, sensitivity) {
  log <- nomo_log_new()
  # A reliability and its standard error print as reliabilities do, and a
  # variance as an estimate, each to three decimals (#144).
  number <- function(x) nomo_present_stat(x, "reliability", digits = 3L)
  variance <- function(x) nomo_present_stat(x, "estimate", digits = 3L)

  for (k in seq_len(nrow(table))) {
    row <- table[k, , drop = FALSE]
    v <- row$variable[[1L]]
    rel <- row$reliability[[1L]]

    log <- nomo_log_add(
      log, stage = "network", object = v,
      metric = "single_indicator",
      value = row$error_variance[[1L]],
      reference = paste(
        "Error variance fixed at (1 - reliability) x variance",
        "(Bollen, 1989; Savalei, 2019)"
      ),
      severity = "info",
      observation = sprintf(
        paste(
          "`%s` is modeled as a single-indicator latent variable. Its error",
          "variance is fixed at (1 - %s) x %s = %s, from a reliability of %s",
          "(%s; %s)."
        ),
        v, number(rel), variance(row$variance[[1L]]),
        variance(row$error_variance[[1L]]), number(rel),
        if (identical(row$coefficient[[1L]], "unspecified")) {
          "coefficient not stated"
        } else {
          row$coefficient[[1L]]
        },
        if (is.na(row$source[[1L]])) {
          "source not stated"
        } else {
          paste0("source: ", row$source[[1L]])
        }
      ),
      recommendation = paste(
        "The correction assumes the composite is unidimensional and that its",
        "reliability captures its measurement error. A coefficient corrects",
        "only the error its design can see; internal consistency, for example,",
        "leaves transient error in (DeShon, 1998)."
      )
    )

    if (identical(row$coefficient[[1L]], "alpha")) {
      log <- nomo_log_add(
        log, stage = "network", object = v,
        metric = "single_indicator_alpha",
        value = rel,
        reference = "Omega from the composite's own measurement model",
        severity = "review",
        observation = sprintf(
          paste(
            "The reliability of `%s` is coefficient alpha, which understates",
            "reliability when loadings differ. The fixed error variance is then",
            "too large, and the relationships `%s` enters are overcorrected."
          ),
          v, v
        ),
        recommendation = paste(
          "Use omega from the composite's own measurement model",
          "(nomo_reliability()). A correction made with alpha bounds the",
          "magnitude of the relationships from above."
        )
      )
    }

    se <- row$se[[1L]]
    log <- nomo_log_add(
      log, stage = "network", object = v,
      metric = "single_indicator_uncertainty",
      value = se,
      reference = "Oberski & Satorra (2013)",
      severity = "info",
      observation = if (is.finite(se)) {
        sprintf(
          paste(
            "Standard errors, intervals, and concordance add the uncertainty",
            "in the reliability of `%s` (standard error %s)."
          ),
          v, number(se)
        )
      } else {
        sprintf(
          paste(
            "Standard errors treat the reliability of `%s` as known, so they",
            "are too small to the extent that it is uncertain."
          ),
          v
        )
      },
      recommendation = if (is.finite(se)) {
        paste(
          "The added term treats the reliability as estimated independently",
          "of these data. When it comes from the same sample, the term is an",
          "approximation."
        )
      } else {
        "Supply its standard error as `se` in nomo_single_indicator() to add it."
      }
    )

    s <- sensitivity[sensitivity$variable == v, , drop = FALSE]
    changed <- unique(s$id[vapply(s$id, function(id) {
      length(unique(s$concordance[s$id == id])) > 1L
    }, logical(1))])
    if (length(changed)) {
      detail <- vapply(changed, function(id) {
        rows <- s[s$id == id, , drop = FALSE]
        rows <- rows[order(rows$reliability), , drop = FALSE]
        groups <- split(rows$reliability, rows$concordance)
        parts <- vapply(names(groups), function(label) {
          sprintf(
            "%s at %s", gsub("_", " ", label),
            paste(number(groups[[label]]), collapse = ", ")
          )
        }, character(1))
        paste0(id, " is ", paste(parts, collapse = " and "))
      }, character(1))
      log <- nomo_log_add(
        log, stage = "network", object = v,
        metric = "single_indicator_sensitivity",
        value = length(changed),
        reference = "Reliability shifted by .05 and .10 each way (Savalei, 2019)",
        severity = "review",
        observation = sprintf(
          "The evidence depends on the reliability assumed for `%s`: %s.",
          v, paste(detail, collapse = "; ")
        ),
        recommendation = paste(
          "Report the evidence across this range, and state where the",
          "reliability comes from and why it is trusted."
        )
      )
    } else {
      log <- nomo_log_add(
        log, stage = "network", object = v,
        metric = "single_indicator_sensitivity",
        value = 0L,
        reference = "Reliability shifted by .05 and .10 each way (Savalei, 2019)",
        severity = "info",
        observation = sprintf(
          "Across reliabilities from %s to %s for `%s`, no hypothesis's concordance changes.",
          number(min(s$reliability)), number(max(s$reliability)), v
        ),
        recommendation = paste(
          "The estimates still change with the reliability;",
          "nomo_table(x, \"sensitivity\") shows them."
        )
      )
    }
  }
  log
}
