# Missing-data sensitivity -----------------------------------------------------

# Schafer and Graham (2002, p. 157) treat a bias larger than about half the
# estimate's standard error as practically important, because beyond that it
# noticeably degrades the coverage of 95% confidence intervals.
nomo_missing_half_se <- 0.5


#' Missing-data sensitivity: does a result depend on how missing data were handled?
#'
#' `nomo_missing()` refits the same prespecified model under alternative
#' missing-data strategies. It reports whether the cases used, the estimates,
#' fit, reliability, or theory evidence change. It does not choose a strategy
#' and does not change the model.
#'
#' @details
#' **Which strategies.** For continuous indicators, listwise deletion is
#' compared with full-information maximum likelihood (FIML), and FIML is the
#' reference. lavaan does not offer FIML for ordered indicators estimated with
#' WLSMV. For those, listwise deletion is compared with pairwise deletion, and
#' pairwise deletion is the reference. The strategy the model was fitted with is
#' always included.
#'
#' Each strategy records what was requested and what lavaan actually used,
#' because the two can differ, and differently across lavaan versions. When
#' `missing = "ml"` is requested with ULS, lavaan 0.7 runs its two-stage method
#' instead, as it does with GLS, while lavaan 0.6-21 refuses; with MLM it is
#' refused. A refused strategy is reported as unavailable, with lavaan's
#' reason. If the reference itself cannot be
#' fitted, another strategy that was fitted becomes the reference, and the
#' decision log says so.
#'
#' **What each strategy assumes.** Listwise and pairwise deletion require data
#' missing completely at random (MCAR). FIML requires data missing at random
#' (MAR). Enders and Bandalos (2001) found all three unbiased under MCAR, with
#' FIML the most efficient. Under MAR, FIML remained unbiased, while listwise
#' and pairwise deletion were biased. FIML also assumes multivariate normality,
#' as complete-data maximum likelihood does.
#'
#' **What a comparison cannot show.** Whether data are MAR cannot in general be
#' tested from the data at hand (Schafer & Graham, 2002). Agreement between
#' strategies shows that a result does not depend on the choice between them.
#' It does not show that either strategy is unbiased; neither is guaranteed to
#' be when data are missing not at random.
#'
#' **When a difference is flagged.** Each estimate is compared with the
#' reference strategy's estimate and expressed in units of the reference
#' standard error. Schafer and Graham (2002) treat a bias larger than about half
#' a standard error as practically important. Beyond that size it noticeably
#' degrades the coverage of confidence intervals. A difference of that size is
#' flagged for review, with two qualifications:
#'
#' * The difference estimates listwise deletion's bias only if the data are MAR
#'   and the model is correct, since only then is FIML consistent. With ordered
#'   indicators, both strategies require MCAR, so a difference cannot be
#'   attributed to either one.
#' * The strategies analyse different cases, so part of any difference is
#'   sampling variability.
#'
#' A hypothesis whose concordance with its prediction differs between
#' strategies is also flagged for review.
#'
#' **Not implemented.** Mean substitution is not offered. It understates
#' variances and distorts covariances. Schafer and Graham (2002) show that even
#' under MCAR it narrows confidence intervals below their nominal coverage.
#' Multiple imputation and models for data missing not at random are outside
#' this function.
#'
#' @param x A `nomo_cfa` or `nomo_network` object.
#' @param data The data the model was fitted to, including any cases listwise
#'   deletion removed. For a network fitted to a `nomo_split`, supply the same
#'   `nomo_split`; the comparison uses its calibration sample. `data` is checked
#'   by refitting the original strategy, which must reproduce the fitted model.
#' @param strategies Optional character vector of lavaan `missing` options to
#'   compare. The default compares listwise deletion with FIML for continuous
#'   indicators, and with pairwise deletion for ordered indicators. lavaan's
#'   aliases `"fiml"` and `"direct"` are treated as `"ml"`.
#' @param reliability For a `nomo_cfa`, whether to compare reliability across
#'   strategies with [nomo_reliability()]. Default `TRUE`. Reliability for
#'   ordered indicators is slow to compute, so `FALSE` is useful when only the
#'   estimates are of interest.
#' @param ... Unused.
#'
#' @return A `nomo_missing` object containing:
#'
#' * `pattern`: missingness in the modelled variables.
#' * `variables`: missing values per variable.
#' * `strategies`: one row per strategy.
#' * `fit`: fit indices by strategy.
#' * `estimates`: standardized loadings and factor correlations for a
#'   `nomo_cfa`, or hypothesis estimates for a `nomo_network`, compared with
#'   the reference.
#' * `reliability`: for a `nomo_cfa`, reliability by strategy, unless
#'   `reliability = FALSE`.
#' * `decision_log`.
#' * `fits`: the refitted objects.
#'
#' @references
#' Enders, C. K., & Bandalos, D. L. (2001). The relative performance of full
#' information maximum likelihood estimation for missing data in structural
#' equation models. *Structural Equation Modeling, 8*(3), 430-457.
#' \doi{10.1207/S15328007SEM0803_5}
#'
#' Schafer, J. L., & Graham, J. W. (2002). Missing data: Our view of the state
#' of the art. *Psychological Methods, 7*(2), 147-177.
#' \doi{10.1037/1082-989X.7.2.147}
#'
#' @seealso [nomo_cfa()], [nomo_network()], and [nomo_screen()] for describing
#'   missingness before a model is fitted.
#'
#' @examples
#' model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
#' fit <- nomo_cfa(model, data = nomo_demo_continuous)
#'
#' sensitivity <- nomo_missing(fit, data = nomo_demo_continuous, reliability = FALSE)
#' sensitivity
#' nomo_table(sensitivity, "strategies")
#' @export
nomo_missing <- function(x, data, strategies = NULL, ...) {
  UseMethod("nomo_missing")
}


#' @export
nomo_missing.default <- function(x, data, strategies = NULL, ...) {
  stop(
    "`nomo_missing()` supports `nomo_cfa` and `nomo_network` objects.",
    call. = FALSE
  )
}


#' @rdname nomo_missing
#' @export
nomo_missing.nomo_cfa <- function(x, data, strategies = NULL, reliability = TRUE,
                                  ...) {
  if (!is.logical(reliability) || length(reliability) != 1L || is.na(reliability)) {
    stop("`reliability` must be TRUE or FALSE.", call. = FALSE)
  }
  data <- nomo_missing_check_data(data, x$data_n)
  guidance <- x$guidance
  ordered <- length(x$ordered) > 0L

  refit <- function(strategy) {
    tryCatch(
      nomo_cfa(
        x$model,
        data = data,
        ordered = if (ordered) x$ordered else NULL,
        estimator = if (identical(x$estimator_source, "researcher")) x$estimator else NULL,
        missing = strategy,
        std.lv = x$std.lv,
        control = x$control,
        modification_indices = FALSE,
        guidance = guidance
      ),
      error = function(e) nomo_missing_failure(e)
    )
  }

  nomo_missing_run(
    x = x,
    lavaan_fit = x$fit,
    data = data,
    ordered = ordered,
    strategies = strategies,
    refit = refit,
    fit_of = function(obj) obj$fit,
    estimates_of = nomo_missing_cfa_estimates,
    fit_evidence_of = function(obj) obj$fit_evidence,
    reliability_of = if (reliability) {
      function(obj) nomo_missing_reliability(obj, guidance)
    },
    kind = "nomo_cfa",
    call = match.call()
  )
}


#' @rdname nomo_missing
#' @export
nomo_missing.nomo_network <- function(x, data, strategies = NULL, ...) {
  if (inherits(data, "nomo_split")) data <- data$calibration
  data <- nomo_missing_check_data(data, x$data_n)
  ordered <- length(x$ordered) > 0L

  refit <- function(strategy) {
    tryCatch(
      nomo_network_fit_once(
        model_fitted = x$model_fitted,
        model_relations = x$model_relations,
        hypotheses = x$hypotheses,
        data = data,
        ordered = x$ordered,
        estimator_requested = if (is.na(x$estimator)) NULL else x$estimator,
        estimator_source = x$estimator_source,
        missing = strategy,
        std.lv = x$std.lv,
        control = x$control,
        guidance = x$guidance,
        equivalence_alpha = x$equivalence_alpha,
        sample_role = x$sample_role
      ),
      error = function(e) nomo_missing_failure(e)
    )
  }

  nomo_missing_run(
    x = x,
    lavaan_fit = x$fit,
    data = data,
    ordered = ordered,
    strategies = strategies,
    refit = refit,
    fit_of = function(obj) obj$fit,
    estimates_of = nomo_missing_network_estimates,
    fit_evidence_of = function(obj) nomo_missing_network_fit(obj$fit_evidence),
    reliability_of = NULL,
    kind = "nomo_network",
    call = match.call()
  )
}


# Engine -----------------------------------------------------------------------

nomo_missing_run <- function(x, lavaan_fit, data, ordered, strategies, refit,
                             fit_of, estimates_of, fit_evidence_of,
                             reliability_of, kind, call) {
  variables <- tryCatch(
    as.character(lavaan::lavNames(lavaan_fit, type = "ov")),
    error = function(e) character()
  )
  absent <- setdiff(variables, names(data))
  if (length(absent)) {
    stop(
      sprintf(
        "`data` does not contain the modelled variable(s): %s.",
        paste(absent, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  fitted_as <- nomo_missing_normalize(
    tryCatch(
      as.character(lavaan::lavInspect(lavaan_fit, "options")$missing)[1L],
      error = function(e) NA_character_
    )
  )
  plan <- nomo_missing_plan(strategies, fitted_as, ordered)
  pattern <- nomo_missing_pattern(data, variables)

  # The original strategy is refitted first. It must reproduce the fitted model
  # exactly, which confirms that `data` is the data the model was fitted to.
  original <- refit(fitted_as)
  nomo_missing_verify(original, fit_of, lavaan_fit)

  any_missing <- pattern$summary$n_incomplete > 0L
  fits <- stats::setNames(vector("list", length(plan$strategies)), plan$strategies)
  # With no missing values every strategy analyses the same cases, so only the
  # original is kept. `[<-` with list(NULL) keeps the slot; `[[<-` would drop it.
  for (s in plan$strategies) {
    fits[s] <- list(if (identical(s, fitted_as)) {
      original
    } else if (any_missing) {
      refit(s)
    } else {
      NULL
    })
  }

  strategy_table <- nomo_missing_strategy_table(
    plan, fits, fit_of, fitted_as, pattern, any_missing
  )
  usable <- strategy_table$strategy[strategy_table$available]

  # A reference that could not be fitted leaves nothing to compare against, so
  # the next preferred strategy that was fitted takes its place, and the log
  # says so.
  intended_reference <- plan$reference
  if (any_missing && !plan$reference %in% usable) {
    preferred <- c("ml", "ml.x", "pairwise", fitted_as)
    plan$reference <- preferred[preferred %in% usable][1L]
    strategy_table$role <- ifelse(
      strategy_table$strategy == plan$reference, "reference", "comparison"
    )
  }

  estimates <- nomo_missing_compare(
    dplyr::bind_rows(lapply(usable, function(s) {
      out <- estimates_of(fits[[s]])
      if (nrow(out)) out$strategy <- s
      out
    })),
    reference = plan$reference,
    strategies = plan$strategies
  )

  fit_table <- nomo_missing_fit_table(usable, fits, fit_evidence_of)

  reliability <- if (is.null(reliability_of)) {
    NULL
  } else {
    nomo_missing_compare_reliability(
      dplyr::bind_rows(lapply(usable, function(s) {
        out <- reliability_of(fits[[s]])
        if (nrow(out)) out$strategy <- s
        out
      })),
      reference = plan$reference,
      strategies = plan$strategies
    )
  }

  decision_log <- nomo_missing_log(
    pattern = pattern,
    strategies = strategy_table,
    estimates = estimates,
    reference = plan$reference,
    ordered = ordered,
    kind = kind
  )
  if (!identical(intended_reference, plan$reference)) {
    decision_log <- nomo_missing_reference_log(
      decision_log, intended_reference, plan$reference, kind
    )
  }

  out <- list(
    call = call,
    object = kind,
    reference = plan$reference,
    fitted_as = fitted_as,
    ordered = ordered,
    pattern = pattern$summary,
    variables = pattern$variables,
    strategies = strategy_table,
    fit = fit_table,
    estimates = estimates,
    reliability = reliability,
    decision_log = decision_log,
    fits = fits
  )
  class(out) <- c("nomo_missing", "list")
  out
}


# Records that the intended reference could not be fitted and which strategy
# took its place. Whether a difference still estimates bias depends on what the
# replacement assumes, so the recommendation says which case this is.
nomo_missing_reference_log <- function(log, intended, actual, kind) {
  nomo_log_add(
    log, stage = "missing_data",
    object = if (identical(kind, "nomo_network")) "network" else "cfa",
    metric = "reference_substituted", severity = "info",
    observation = sprintf(
      "%s could not be fitted, so %s is the reference instead.",
      nomo_missing_label(intended), nomo_missing_label_inline(actual)
    ),
    recommendation = if (identical(nomo_missing_requires(intended), "MAR") &&
                         identical(nomo_missing_requires(actual), "MCAR")) {
      paste(
        "The reference now requires data missing completely at random, a",
        "stronger assumption than FIML's, so a difference from it no longer",
        "estimates the bias of the other strategy."
      )
    } else {
      "Differences are measured from this strategy instead."
    }
  )
}


nomo_missing_check_data <- function(data, expected_n) {
  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop("`data` must be the non-empty data frame the model was fitted to.", call. = FALSE)
  }
  if (length(expected_n) == 1L && is.finite(expected_n) && nrow(data) != expected_n) {
    stop(
      sprintf(
        paste(
          "`data` has %d rows, but the model was fitted to %d. Supply the data",
          "the model was fitted to, including any cases listwise deletion removed."
        ),
        nrow(data), as.integer(expected_n)
      ),
      call. = FALSE
    )
  }
  data
}


nomo_missing_failure <- function(e) {
  structure(
    list(message = gsub("\\s+", " ", trimws(conditionMessage(e)))),
    class = "nomo_missing_failure"
  )
}


nomo_missing_verify <- function(original, fit_of, lavaan_fit) {
  if (inherits(original, "nomo_missing_failure")) {
    stop(
      paste0(
        "Refitting the model with its original missing-data strategy failed, ",
        "so `data` could not be confirmed: ", original$message
      ),
      call. = FALSE
    )
  }
  a <- tryCatch(lavaan::coef(fit_of(original)), error = function(e) NULL)
  b <- tryCatch(lavaan::coef(lavaan_fit), error = function(e) NULL)
  same <- !is.null(a) && !is.null(b) && identical(names(a), names(b)) &&
    isTRUE(all.equal(unname(as.numeric(a)), unname(as.numeric(b)), tolerance = 1e-6))
  if (!same) {
    stop(
      paste(
        "Refitting the model to `data` with its original missing-data strategy",
        "does not reproduce the fitted model. Supply the data the model was",
        "fitted to."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


# lavaan treats "fiml" and "direct" as "ml", and a missing option as listwise.
nomo_missing_normalize <- function(missing) {
  if (!length(missing) || is.na(missing[[1L]]) || !nzchar(trimws(missing[[1L]]))) {
    return("listwise")
  }
  m <- tolower(trimws(missing[[1L]]))
  if (m %in% c("fiml", "direct")) m <- "ml"
  if (identical(m, "fiml.x")) m <- "ml.x"
  m
}


nomo_missing_plan <- function(strategies, fitted_as, ordered) {
  if (is.null(strategies)) {
    reference <- if (ordered) "pairwise" else "ml"
    requested <- c("listwise", reference)
  } else {
    if (!is.character(strategies) || !length(strategies) || anyNA(strategies) ||
        any(!nzchar(trimws(strategies)))) {
      stop("`strategies` must be NULL or a character vector of lavaan `missing` options.", call. = FALSE)
    }
    requested <- vapply(strategies, nomo_missing_normalize, character(1), USE.NAMES = FALSE)
    reference <- NULL
  }
  all <- unique(c(requested, fitted_as))
  if (is.null(reference)) {
    preferred <- c("ml", "ml.x", "pairwise")
    reference <- if (any(preferred %in% all)) preferred[preferred %in% all][1L] else fitted_as
  }
  list(strategies = all, reference = reference)
}


nomo_missing_label <- function(strategy) {
  labels <- c(
    listwise = "Listwise deletion",
    ml = "FIML",
    ml.x = "FIML (ml.x)",
    pairwise = "Pairwise deletion",
    two.stage = "Two-stage ML",
    robust.two.stage = "Robust two-stage ML"
  )
  out <- unname(labels[strategy])
  ifelse(is.na(out), paste0("lavaan missing = \"", strategy, "\""), out)
}


# The same label for use inside a sentence: "listwise deletion", but "FIML".
nomo_missing_label_inline <- function(strategy) {
  label <- nomo_missing_label(strategy)
  ifelse(grepl("^[A-Z][a-z]", label), paste0(tolower(substr(label, 1L, 1L)), substring(label, 2L)), label)
}


# The mechanism each strategy requires, as Enders and Bandalos (2001) state it.
# Strategies they did not study are left unstated rather than guessed.
nomo_missing_requires <- function(strategy) {
  out <- rep(NA_character_, length(strategy))
  out[strategy %in% c("listwise", "pairwise")] <- "MCAR"
  out[strategy %in% c("ml", "ml.x")] <- "MAR"
  out
}


nomo_missing_pattern <- function(data, variables) {
  d <- data[, variables, drop = FALSE]
  observed <- !is.na(d)
  n <- nrow(d)
  n_observed <- rowSums(observed)
  complete <- n_observed == length(variables)

  coverage <- crossprod(observed * 1) / n
  low <- which(coverage == min(coverage), arr.ind = TRUE)[1L, ]
  low_pair <- if (low[[1L]] == low[[2L]]) {
    variables[low[[1L]]]
  } else {
    paste(variables[sort(low)], collapse = ", ")
  }

  summary <- tibble::tibble(
    n_cases = n,
    n_complete = sum(complete),
    n_incomplete = sum(!complete),
    pct_incomplete = mean(!complete),
    n_no_data = sum(n_observed == 0L),
    n_patterns = length(unique(apply(observed * 1L, 1L, paste, collapse = ""))),
    min_coverage = min(coverage),
    min_coverage_variables = low_pair
  )
  per_variable <- tibble::tibble(
    variable = variables,
    n_missing = as.integer(colSums(!observed)),
    pct_missing = colMeans(!observed)
  )
  list(summary = summary, variables = per_variable)
}


nomo_missing_strategy_table <- function(plan, fits, fit_of, fitted_as, pattern,
                                        any_missing) {
  rows <- lapply(plan$strategies, function(s) {
    obj <- fits[[s]]
    failed <- inherits(obj, "nomo_missing_failure")
    skipped <- is.null(obj)
    fit <- if (failed || skipped) NULL else fit_of(obj)

    engine <- if (is.null(fit)) {
      NA_character_
    } else {
      nomo_missing_normalize(tryCatch(
        as.character(lavaan::lavInspect(fit, "options")$missing)[1L],
        error = function(e) NA_character_
      ))
    }
    n_used <- if (is.null(fit)) {
      NA_real_
    } else {
      suppressWarnings(sum(as.numeric(unlist(tryCatch(
        lavaan::lavInspect(fit, "nobs"), error = function(e) NA_real_
      ))), na.rm = TRUE))
    }
    converged <- if (is.null(fit)) NA else isTRUE(tryCatch(
      lavaan::lavInspect(fit, "converged"), error = function(e) FALSE
    ))
    admissible <- if (is.null(fit)) NA else isTRUE(tryCatch(
      suppressWarnings(lavaan::lavInspect(fit, "post.check")),
      error = function(e) FALSE
    ))

    note <- if (failed) {
      obj$message
    } else if (skipped) {
      "Not fitted: no modelled variable has missing values."
    } else if (!identical(engine, s)) {
      sprintf("lavaan used missing = \"%s\" when \"%s\" was requested.", engine, s)
    } else {
      ""
    }

    tibble::tibble(
      strategy = s,
      label = nomo_missing_label(s),
      lavaan_missing = engine,
      requires = nomo_missing_requires(if (is.na(engine)) s else engine),
      role = if (identical(s, plan$reference)) "reference" else "comparison",
      as_fitted = identical(s, fitted_as),
      available = !failed && !skipped,
      converged = converged,
      admissible = admissible,
      n_used = n_used,
      n_discarded = if (is.finite(n_used)) pattern$summary$n_cases - n_used else NA_real_,
      note = note
    )
  })
  dplyr::bind_rows(rows)
}


nomo_missing_cfa_estimates <- function(obj) {
  loadings <- obj$standardized_loadings
  correlations <- obj$factor_correlations
  out <- list()
  if (is.data.frame(loadings) && nrow(loadings)) {
    out[[length(out) + 1L]] <- tibble::tibble(
      type = "loading",
      parameter = paste(loadings$factor, "=~", loadings$item),
      estimate = loadings$loading,
      se = loadings$se
    )
  }
  if (is.data.frame(correlations) && nrow(correlations)) {
    out[[length(out) + 1L]] <- tibble::tibble(
      type = "factor_correlation",
      parameter = paste(correlations$factor1, "~~", correlations$factor2),
      estimate = correlations$correlation,
      se = correlations$se
    )
  }
  dplyr::bind_rows(out)
}


# A network's fit evidence is one wide row (chisq, df, ..., srmr), unlike a
# nomo_cfa's long metric/value table. Reshaped here so the fit comparison reads
# both the same way.
nomo_missing_network_fit <- function(fe) {
  columns <- c(
    chi_square = "chisq", df = "df", p_value = "pvalue", CFI = "cfi",
    TLI = "tli", RMSEA = "rmsea", SRMR = "srmr"
  )
  tibble::tibble(
    metric = names(columns),
    value = vapply(columns, function(column) {
      if (column %in% names(fe)) as.numeric(fe[[column]][[1L]]) else NA_real_
    }, numeric(1), USE.NAMES = FALSE)
  )
}


# A network always carries its hypotheses' evidence, so there is always a row.
nomo_missing_network_estimates <- function(obj) {
  h <- obj$hypothesis_evidence
  tibble::tibble(
    type = "hypothesis",
    parameter = h$id,
    relation = h$relation,
    estimate = h$estimate,
    se = h$se,
    ci_lower = h$ci_lower,
    ci_upper = h$ci_upper,
    concordance = h$concordance
  )
}


nomo_missing_compare <- function(long, reference, strategies) {
  ref <- long[long$strategy == reference, c("type", "parameter", "estimate", "se"), drop = FALSE]
  names(ref) <- c("type", "parameter", "reference_estimate", "reference_se")
  if ("concordance" %in% names(long)) {
    ref$reference_concordance <- long$concordance[long$strategy == reference]
  }

  out <- dplyr::left_join(long, ref, by = c("type", "parameter"))
  is_ref <- out$strategy == reference
  out$role <- ifelse(is_ref, "reference", "comparison")
  out$difference <- ifelse(is_ref, NA_real_, out$estimate - out$reference_estimate)
  out$difference_in_se <- out$difference / out$reference_se
  out$beyond_half_se <- abs(out$difference_in_se) > nomo_missing_half_se
  if ("reference_concordance" %in% names(out)) {
    out$concordance_changed <- ifelse(
      is_ref, NA, out$concordance != out$reference_concordance
    )
  }

  # Parameters in model order, strategies in the order they were compared.
  out <- out[order(
    match(out$parameter, unique(long$parameter)),
    match(out$strategy, strategies)
  ), , drop = FALSE]
  front <- c("type", "parameter", "strategy", "role")
  tibble::as_tibble(out[, c(front, setdiff(names(out), front)), drop = FALSE])
}


nomo_missing_reliability <- function(obj, guidance) {
  rel <- tryCatch(nomo_reliability(obj, guidance = guidance), error = function(e) NULL)
  if (is.null(rel)) return(tibble::tibble())
  parts <- list(rel$omega, rel$alpha)
  parts <- parts[vapply(parts, function(p) is.data.frame(p) && nrow(p) > 0L, logical(1))]
  if (!length(parts)) return(tibble::tibble())
  out <- dplyr::bind_rows(parts)
  tibble::tibble(
    construct = out$construct,
    block = out$block,
    metric = out$metric,
    estimate = out$estimate
  )
}


nomo_missing_compare_reliability <- function(long, reference, strategies) {
  if (!nrow(long)) return(tibble::tibble())
  key <- c("construct", "block", "metric")
  ref <- long[long$strategy == reference, c(key, "estimate"), drop = FALSE]
  names(ref)[names(ref) == "estimate"] <- "reference_estimate"
  out <- dplyr::left_join(long, ref, by = key)
  is_ref <- out$strategy == reference
  out$role <- ifelse(is_ref, "reference", "comparison")
  out$difference <- ifelse(is_ref, NA_real_, out$estimate - out$reference_estimate)
  out <- out[order(
    match(paste(out$construct, out$block, out$metric), unique(paste(long$construct, long$block, long$metric))),
    match(out$strategy, strategies)
  ), , drop = FALSE]
  tibble::as_tibble(out[, c(key, "strategy", "role", "estimate", "reference_estimate", "difference")])
}


nomo_missing_fit_table <- function(usable, fits, fit_evidence_of) {
  keep <- c("chi_square", "df", "p_value", "CFI", "TLI", "RMSEA", "SRMR")
  rows <- lapply(usable, function(s) {
    ev <- fit_evidence_of(fits[[s]])
    values <- stats::setNames(rep(NA_real_, length(keep)), keep)
    if (is.data.frame(ev) && all(c("metric", "value") %in% names(ev))) {
      present <- intersect(keep, ev$metric)
      values[present] <- ev$value[match(present, ev$metric)]
    }
    tibble::as_tibble(c(list(strategy = s), as.list(values)))
  })
  dplyr::bind_rows(rows)
}


# Decision log -----------------------------------------------------------------

nomo_missing_log <- function(pattern, strategies, estimates, reference, ordered,
                             kind) {
  log <- nomo_log_new()
  s <- pattern$summary
  object <- if (identical(kind, "nomo_network")) "network" else "cfa"
  ref_label <- nomo_missing_label_inline(reference)

  if (s$n_incomplete == 0L) {
    return(nomo_log_add(
      log, stage = "missing_data", object = object, metric = "no_missing",
      value = 0, severity = "info",
      observation = sprintf(
        "None of the %d cases is missing a modelled variable.", s$n_cases
      ),
      recommendation = paste(
        "Every strategy analyses the same cases, so the choice of missing-data",
        "strategy cannot change these results and there is nothing to compare."
      )
    ))
  }

  log <- nomo_log_add(
    log, stage = "missing_data", object = object, metric = "missing_mechanism",
    value = s$pct_incomplete,
    reference = "Enders & Bandalos (2001); Schafer & Graham (2002)",
    severity = "info",
    observation = sprintf(
      "%d of %d cases (%.1f%%) are missing at least one modelled variable, in %d pattern(s).",
      s$n_incomplete, s$n_cases, 100 * s$pct_incomplete, s$n_patterns
    ),
    recommendation = paste(
      "Which strategy is appropriate depends on why the data are missing.",
      "Listwise and pairwise deletion require data missing completely at",
      "random (MCAR); FIML requires data missing at random (MAR) (Enders &",
      "Bandalos, 2001). Whether data are MAR cannot in general be tested from",
      "the data at hand (Schafer & Graham, 2002), so this comparison shows",
      "whether a result depends on the strategy, not which strategy is right."
    )
  )

  for (i in seq_len(nrow(strategies))) {
    row <- strategies[i, ]
    if (!row$available) {
      if (!startsWith(row$note, "Not fitted")) {
        log <- nomo_log_add(
          log, stage = "missing_data", object = object,
          metric = "strategy_unavailable", severity = "info",
          observation = sprintf("%s could not be fitted: %s", row$label, row$note),
          recommendation = "The comparison reports the strategies that could be fitted."
        )
      }
      next
    }
    if (!identical(row$lavaan_missing, row$strategy)) {
      log <- nomo_log_add(
        log, stage = "missing_data", object = object,
        metric = "strategy_substituted", severity = "info",
        observation = row$note,
        recommendation = paste(
          "Results under this strategy are what lavaan estimated, not what was",
          "requested; interpret them by the method lavaan used."
        )
      )
    }
    if (identical(row$strategy, "listwise") && isTRUE(row$n_discarded > 0)) {
      log <- nomo_log_add(
        log, stage = "missing_data", object = object,
        metric = "cases_discarded", value = row$n_discarded, severity = "info",
        observation = sprintf(
          "Listwise deletion analyses %d of %d cases, discarding %d (%.1f%%).",
          as.integer(row$n_used), s$n_cases, as.integer(row$n_discarded),
          100 * row$n_discarded / s$n_cases
        ),
        recommendation = paste(
          "Under MCAR, discarding cases costs efficiency rather than accuracy;",
          "under MAR, the complete cases can be unrepresentative and the",
          "estimates biased (Enders & Bandalos, 2001; Schafer & Graham, 2002)."
        )
      )
    }
    if (isFALSE(row$converged) || isFALSE(row$admissible)) {
      log <- nomo_log_add(
        log, stage = "missing_data", object = object,
        metric = if (isFALSE(row$converged)) "nonconvergence" else "inadmissible_solution",
        severity = "review",
        observation = sprintf(
          "Under %s the model %s.", row$label,
          if (isFALSE(row$converged)) "did not converge" else
            "has an inadmissible solution (a negative variance or a covariance matrix that is not positive definite)"
        ),
        recommendation = paste(
          "Estimates under this strategy are not a sound basis for comparison.",
          "Enders and Bandalos (2001) found nonconvergence under listwise",
          "deletion to rise with the proportion of missing data, as the",
          "analysed sample shrinks."
        )
      )
    }
  }

  if (any(strategies$available & strategies$lavaan_missing %in% c("ml", "ml.x"))) {
    log <- nomo_log_add(
      log, stage = "missing_data", object = object, metric = "fiml_normality",
      reference = "Enders & Bandalos (2001)", severity = "info",
      observation = "FIML was fitted.",
      recommendation = paste(
        "FIML assumes multivariate normality, as complete-data maximum",
        "likelihood does (Enders & Bandalos, 2001)."
      )
    )
  }

  log <- nomo_missing_difference_log(
    log, estimates, strategies, reference, ref_label, object
  )

  if (identical(kind, "nomo_network") && "concordance_changed" %in% names(estimates)) {
    changed <- estimates[nomo_missing_true(estimates$concordance_changed), , drop = FALSE]
    for (i in seq_len(nrow(changed))) {
      row <- changed[i, ]
      log <- nomo_log_add(
        log, stage = "missing_data", object = object,
        metric = "concordance_changed", severity = "review",
        observation = sprintf(
          "%s (%s) is %s under %s and %s under %s.",
          row$parameter, row$relation,
          row$reference_concordance, ref_label,
          row$concordance, nomo_missing_label_inline(row$strategy)
        ),
        recommendation = paste(
          "The theory evidence for this hypothesis depends on how missing data",
          "were handled. Report it under the strategy chosen in advance,",
          "together with this sensitivity."
        )
      )
    }
  }

  log
}


nomo_missing_true <- function(x) !is.na(x) & x


nomo_missing_difference_log <- function(log, estimates, strategies, reference,
                                        ref_label, object) {
  comparisons <- setdiff(unique(estimates$strategy), reference)
  n_cases <- max(strategies$n_used, na.rm = TRUE)

  for (s in comparisons) {
    rows <- estimates[estimates$strategy == s & is.finite(estimates$difference_in_se), , drop = FALSE]
    if (!nrow(rows)) next
    label <- nomo_missing_label(s)
    inline <- nomo_missing_label_inline(s)
    ref_inline <- nomo_missing_label_inline(reference)
    worst <- rows[which.max(abs(rows$difference_in_se)), ]
    flagged <- nomo_missing_true(rows$beyond_half_se)
    largest <- sprintf(
      "%s: %.3f against %.3f (%+.2f SE)",
      worst$parameter, worst$estimate, worst$reference_estimate, worst$difference_in_se
    )

    # How many fewer cases this strategy analysed than the fullest one. The
    # more cases a strategy discards, the larger the differences that sampling
    # variability alone produces, so the note says how many.
    n_here <- strategies$n_used[strategies$strategy == s]
    sampling <- if (length(n_here) == 1L && is.finite(n_here) && n_here < n_cases) {
      sprintf(
        paste(
          "%s analyses %d fewer cases than the fullest strategy (%.1f%%), and",
          "the more cases a strategy discards, the larger the differences that",
          "sampling variability alone produces."
        ),
        label, as.integer(n_cases - n_here), 100 * (n_cases - n_here) / n_cases
      )
    } else {
      "The strategies analyse different cases, so part of any difference is sampling variability."
    }

    if (any(flagged)) {
      attribution <- if (identical(reference, "ml") || identical(reference, "ml.x")) {
        paste(
          "If the data are MAR and the model is correct, FIML is consistent and",
          "this difference estimates the bias", inline, "introduces; Schafer and",
          "Graham (2002) treat a bias of this size as practically important."
        )
      } else if (identical(reference, "pairwise") && identical(s, "listwise")) {
        paste(
          "Listwise and pairwise deletion both require MCAR (Enders & Bandalos,",
          "2001), so this difference cannot be attributed to either; it shows",
          "that the result depends on the choice between them."
        )
      } else {
        "The difference shows that the result depends on the choice of strategy."
      }
      log <- nomo_log_add(
        log, stage = "missing_data", object = object,
        metric = "estimate_difference",
        value = abs(worst$difference_in_se),
        reference = "Schafer & Graham (2002): bias beyond about half a standard error is practically important",
        severity = "review",
        observation = sprintf(
          "%d of %d estimates under %s differ from the %s estimate by more than half its standard error. Largest: %s.",
          sum(flagged), nrow(rows), inline, ref_inline, largest
        ),
        recommendation = paste(
          attribution, sampling,
          "Report which strategy the results rest on and why."
        )
      )
    } else {
      log <- nomo_log_add(
        log, stage = "missing_data", object = object,
        metric = "estimates_agree",
        value = abs(worst$difference_in_se),
        reference = "Schafer & Graham (2002): bias beyond about half a standard error is practically important",
        severity = "info",
        observation = sprintf(
          "No estimate under %s differs from the %s estimate by more than half its standard error. Largest: %s.",
          inline, ref_inline, largest
        ),
        recommendation = paste(
          "These results do not depend on the choice between the two",
          "strategies. That does not show either is unbiased: neither is",
          "guaranteed to be when data are missing not at random."
        )
      )
    }
  }
  log
}
