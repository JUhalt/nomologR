# Power and sample size -------------------------------------------------------------

#' Power of the RMSEA tests of model fit
#'
#' `nomo_power_rmsea()` gives the power of MacCallum, Browne, and Sugawara's
#' (1996) RMSEA-based tests of fit at given sample sizes, or the smallest sample
#' size that reaches a target power.
#'
#' @details
#' MacCallum et al. (1996) framed the power of a covariance structure model's
#' overall test in terms of RMSEA, whose noncentral chi-square distribution has
#' noncentrality \eqn{(N - 1) \, df \, \varepsilon^2}{(N - 1) * df * RMSEA^2}.
#' Three tests are available:
#'
#' * `"close"`: the test of close fit, with null RMSEA .05 and alternative .08
#'   by default. Power is the chance of rejecting close fit when the fit is in
#'   fact mediocre.
#' * `"not_close"`: the test of not-close fit, with null .05 and alternative
#'   .01. Power is the chance of rejecting not-close fit when the fit is in fact
#'   close, which is what supports a claim of good fit.
#' * `"exact"`: the test of exact fit, with null 0 and alternative .05.
#'
#' Power depends on the degrees of freedom: a model with few of them needs a
#' large sample to test its fit. This is power for the model's overall fit test,
#' not for any one parameter; [nomo_power_simulate()] gives that.
#'
#' @param model The planned model: a lavaan model string, a [nomo_model()]
#'   object, a fitted [nomo_cfa()] result, or a `lavaan` fit. Its degrees of
#'   freedom are used. Give `model` or `df`, not both.
#' @param n Optional sample sizes at which to compute power.
#' @param df The model's degrees of freedom, instead of `model`.
#' @param power Target power for the smallest sample size. Default `.80`.
#' @param test `"close"` (default), `"not_close"`, or `"exact"`.
#' @param rmsea_null,rmsea_alt Optional null and alternative RMSEA values,
#'   replacing the test's defaults.
#' @param alpha Significance level. Default `.05`.
#'
#' @return A `nomo_power` object. The fields to read are `test`, `df`,
#'   `rmsea_null`, `rmsea_alt`, `alpha`, `target_power`, `power` (a table of
#'   sample sizes and their power), and `n_required` (the smallest sample size
#'   reaching `target_power`, or `NA` if none up to one million does).
#'
#'   Other fields record the kind of power analysis. They may change between
#'   releases and are not part of the stable interface (see `?nomologR`).
#'
#' @references
#' MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power analysis
#' and determination of sample size for covariance structure modeling.
#' *Psychological Methods, 1*(2), 130-149. \doi{10.1037/1082-989X.1.2.130}
#'
#' @seealso [nomo_power_simulate()] for the power to detect particular
#'   parameters.
#'
#' @examples
#' model <- nomo_model(list(A = paste0("a", 1:4), B = paste0("b", 1:4)))
#' nomo_power_rmsea(model)
#' nomo_power_rmsea(model, n = c(100, 200, 400), test = "not_close")
#' @export
nomo_power_rmsea <- function(model = NULL,
                             n = NULL,
                             df = NULL,
                             power = 0.80,
                             test = c("close", "not_close", "exact"),
                             rmsea_null = NULL,
                             rmsea_alt = NULL,
                             alpha = 0.05) {
  test <- nomo_match_arg(test)
  if (is.null(model) == is.null(df)) {
    stop("Give `model` or `df`, but not both.", call. = FALSE)
  }
  if (is.null(df)) df <- nomo_power_model_df(model)
  if (!is.numeric(df) || length(df) != 1L || !is.finite(df) || df < 1 ||
        df != round(df)) {
    stop(
      "`df` must be one positive whole number; a model with no degrees of freedom has no fit to test.",
      call. = FALSE
    )
  }
  nomo_power_check_probability(power, "power")
  nomo_power_check_probability(alpha, "alpha")

  defaults <- list(close = c(0.05, 0.08), not_close = c(0.05, 0.01), exact = c(0, 0.05))
  if (is.null(rmsea_null)) rmsea_null <- defaults[[test]][[1L]]
  if (is.null(rmsea_alt)) rmsea_alt <- defaults[[test]][[2L]]
  for (value in list(rmsea_null, rmsea_alt)) {
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < 0) {
      stop("RMSEA values must be single non-negative numbers.", call. = FALSE)
    }
  }
  if (identical(test, "not_close") != (rmsea_alt < rmsea_null) || rmsea_alt == rmsea_null) {
    stop(
      paste(
        "The alternative RMSEA must be above the null for tests of close or",
        "exact fit, and below it for the test of not-close fit."
      ),
      call. = FALSE
    )
  }

  # At very large noncentralities R's noncentral chi-square warns that its
  # series has not converged; that happens only near the million-case limit.
  power_at <- function(size) suppressWarnings({
    null_ncp <- (size - 1) * df * rmsea_null^2
    alt_ncp <- (size - 1) * df * rmsea_alt^2
    if (rmsea_alt > rmsea_null) {
      critical <- stats::qchisq(1 - alpha, df, ncp = null_ncp)
      stats::pchisq(critical, df, ncp = alt_ncp, lower.tail = FALSE)
    } else {
      critical <- stats::qchisq(alpha, df, ncp = null_ncp)
      stats::pchisq(critical, df, ncp = alt_ncp)
    }
  })

  # Power rises with N, so the smallest N reaching the target is bracketed by
  # doubling from 50 and then found by bisection. The search stops at a
  # million, where the noncentral chi-square is too extreme to compute well.
  lower <- 2
  upper <- 50
  while (power_at(upper) < power && upper < 1e6) {
    lower <- upper
    upper <- min(upper * 2, 1e6)
  }
  n_required <- if (power_at(upper) < power) {
    NA_integer_
  } else {
    while (upper - lower > 1) {
      middle <- floor((lower + upper) / 2)
      if (power_at(middle) >= power) upper <- middle else lower <- middle
    }
    as.integer(if (power_at(lower) >= power) lower else upper)
  }

  sizes <- if (is.null(n)) {
    if (is.na(n_required)) integer() else n_required
  } else {
    if (!is.numeric(n) || !length(n) || any(!is.finite(n)) || any(n < 2) ||
          any(n != round(n))) {
      stop("`n` must hold whole numbers of at least 2.", call. = FALSE)
    }
    as.integer(sort(unique(n)))
  }

  out <- list(
    type = "rmsea",
    test = test,
    df = as.integer(df),
    rmsea_null = rmsea_null,
    rmsea_alt = rmsea_alt,
    alpha = alpha,
    target_power = power,
    power = tibble::tibble(n = sizes, power = vapply(sizes, power_at, numeric(1))),
    n_required = n_required
  )
  class(out) <- c("nomo_power", "list")
  out
}


nomo_power_check_probability <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0 || x >= 1) {
    stop(sprintf("`%s` must be one number strictly between 0 and 1.", name), call. = FALSE)
  }
  invisible(TRUE)
}


# A planned model's degrees of freedom, from lavaan itself: the model fitted to
# an identity covariance matrix, which it reproduces easily.
nomo_power_model_df <- function(model) {
  if (inherits(model, "nomo_cfa")) {
    return(model$fit_evidence$value[model$fit_evidence$metric == "df"][[1L]])
  }
  if (inherits(model, "lavaan")) return(unname(lavaan::fitMeasures(model, "df")))
  if (inherits(model, "nomo_model")) model <- as.character(model)
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
        !nzchar(trimws(model))) {
    stop(
      "`model` must be a lavaan model string, a nomo_model(), a nomo_cfa(), or a lavaan fit.",
      call. = FALSE
    )
  }
  observed <- lavaan::lavNames(nomo_network_model_table(model), "ov")
  identity <- diag(length(observed))
  dimnames(identity) <- list(observed, observed)
  fit <- suppressWarnings(lavaan::sem(model, sample.cov = identity, sample.nobs = 500))
  unname(lavaan::fitMeasures(fit, "df"))
}


#' Monte Carlo power and sample size for a planned model
#'
#' `nomo_power_simulate()` estimates, for each sample size, how often a planned
#' model recovers its parameters: whether it converges, whether the solution is
#' proper, how biased the estimates and their standard errors are, how often the
#' intervals cover the population values, and how often each parameter is
#' detected. This is Muthén and Muthén's (2002) Monte Carlo approach.
#'
#' @details
#' **Experimental.** This function is experimental and remains so after
#' nomologR 1.0.0. The metric of its estimates and its summaries may be refined
#' during 1.x, without a deprecation period, as it is extended beyond complete
#' continuous data. Any change will be described in NEWS; see the package help
#' page, `?nomologR`, for the stability policy.
#'
#' Data are generated from `population`, a lavaan model whose parameters carry
#' their population values (for example `A =~ 0.7*a1`), with
#' `lavaan::simulateData()`. With `standardized = TRUE`, the default, the
#' residual variances of the observed variables are set so that each has unit
#' variance. The `analysis` model, which by default is `population` without its
#' values, is fitted to each data set with `lavaan::sem(std.lv = TRUE)`.
#'
#' **The metric.** Each estimate is the analysis model's unstandardized
#' estimate, and it is compared with the population value as written. Both are
#' in the metric that `std.lv = TRUE` sets: an exogenous factor has variance 1,
#' and an endogenous factor has residual variance 1. With the default
#' `standardized = TRUE`, loadings on exogenous factors are therefore
#' standardized, and covariances between exogenous factors are correlations. A
#' latent regression is not standardized. In `B ~ 0.4*A`, B's total variance
#' is 1.16, its residual variance of 1 plus 0.16 from A, so the completely
#' standardized coefficient is 0.4 divided by the square root of 1.16, or .37,
#' and B's indicators have standardized loadings 1.08 times their values. Bias,
#' coverage, and power are for the values as written.
#'
#' Muthén and Muthén (2002) suggest choosing the sample size at which three
#' conditions hold, and power for the parameter of interest is close to .80:
#'
#' * parameter and standard error biases are within 10% for every parameter;
#' * the standard error bias of the parameter whose power is assessed is within
#'   5%;
#' * coverage of the 95% interval lies between .91 and .98.
#'
#' `meets_references` records whether they hold at each sample size, for the
#' parameters in `focus`. Wolf, Harrington, Clark, and Miller (2013) showed
#' that the sample size a model needs varies widely with its structure, so
#' rules of thumb such as a fixed N or a ratio of cases to parameters are no
#' substitute. They also counted improper solutions, which are reported here.
#' Summaries use the replications that converged.
#'
#' @param population A lavaan model with population values for its parameters.
#' @param n Sample sizes to simulate.
#' @param reps Replications per sample size. Default 500; Muthén and Muthén used
#'   10,000, and more replications give steadier estimates.
#' @param analysis Optional analysis model. By default, `population` with its
#'   numeric values removed.
#' @param focus Optional parameters whose power is of interest, written as in
#'   lavaan (`"A~~B"`, `"B~A"`, `"A=~a1"`). By default, every parameter with a
#'   nonzero population value.
#' @param alpha Significance level for detection. Default `.05`.
#' @param seed Optional integer seed, for a reproducible simulation. The
#'   session's random-number state is restored afterwards.
#' @param standardized Passed to `lavaan::simulateData()`. Default `TRUE`,
#'   which gives the observed variables unit variance.
#'
#' @return A `nomo_power` object. The fields to read are `parameters` (for
#'   each sample size and parameter: population value, mean estimate, relative
#'   bias, standard deviation of the estimates, mean standard error, standard
#'   error bias, coverage, and power), `summary` (for each sample size: the
#'   proportions that converged and that were improper, the smallest power in
#'   `focus`, the largest absolute biases, the coverage range, and
#'   `meets_references`), `n_required` (the smallest simulated sample size that
#'   meets the references, or `NA`), `focus`, `reps`, `alpha`, and `seed` (the
#'   seed given, or `NA` without one).
#'
#'   Other fields record the call, the population and analysis models, and the
#'   kind of power analysis. They may change between releases and are not part
#'   of the stable interface (see `?nomologR`).
#'
#' @references
#' Muthén, L. K., & Muthén, B. O. (2002). How to use a Monte Carlo study to
#' decide on sample size and determine power. *Structural Equation Modeling,
#' 9*(4), 599-620. \doi{10.1207/S15328007SEM0904_8}
#'
#' Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013). Sample
#' size requirements for structural equation models: An evaluation of power,
#' bias, and solution propriety. *Educational and Psychological Measurement,
#' 73*(6), 913-934. \doi{10.1177/0013164413495237}
#'
#' @seealso [nomo_power_rmsea()] for the power of the overall fit tests.
#'
#' @examples
#' \donttest{
#' population <- "
#'   A =~ 0.7*a1 + 0.7*a2 + 0.6*a3 + 0.5*a4
#'   B =~ 0.7*b1 + 0.6*b2 + 0.6*b3 + 0.5*b4
#'   A ~~ 0.3*B
#' "
#' pw <- nomo_power_simulate(population, n = c(100, 200), reps = 100,
#'                           focus = "A~~B", seed = 2026)
#' pw
#' }
#' @export
nomo_power_simulate <- function(population,
                                n,
                                reps = 500,
                                analysis = NULL,
                                focus = NULL,
                                alpha = 0.05,
                                seed = NULL,
                                standardized = TRUE) {
  if (!is.character(population) || length(population) != 1L || is.na(population) ||
        !nzchar(trimws(population))) {
    stop("`population` must be one lavaan model string with population values.", call. = FALSE)
  }
  if (!is.numeric(n) || !length(n) || any(!is.finite(n)) || any(n < 10) ||
        any(n != round(n))) {
    stop("`n` must hold whole numbers of at least 10.", call. = FALSE)
  }
  if (!is.numeric(reps) || length(reps) != 1L || !is.finite(reps) || reps < 2 ||
        reps != round(reps)) {
    stop("`reps` must be one whole number of at least 2.", call. = FALSE)
  }
  nomo_power_check_probability(alpha, "alpha")
  if (!is.logical(standardized) || length(standardized) != 1L || is.na(standardized)) {
    stop("`standardized` must be TRUE or FALSE.", call. = FALSE)
  }
  if (is.null(analysis)) {
    analysis <- gsub(
      "(^|[^A-Za-z0-9_.])-?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?[[:space:]]*[*]",
      "\\1", population, perl = TRUE
    )
  }

  truth <- nomo_power_truth(population)
  focus <- nomo_power_focus(focus, truth)

  if (!is.null(seed)) {
    if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) ||
          seed != round(seed)) {
      stop("`seed` must be NULL or one whole number.", call. = FALSE)
    }
    rng_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (rng_exists) rng_before <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    on.exit({
      if (rng_exists) {
        assign(".Random.seed", rng_before, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    }, add = TRUE)
    set.seed(as.integer(seed))
  }

  sizes <- as.integer(sort(unique(n)))
  runs <- lapply(sizes, function(size) {
    nomo_power_replicate(population, analysis, truth, size, as.integer(reps),
                         standardized)
  })

  parameters <- dplyr::bind_rows(lapply(seq_along(sizes), function(i) {
    nomo_power_parameters(runs[[i]], truth, sizes[[i]], alpha)
  }))
  summary <- dplyr::bind_rows(lapply(seq_along(sizes), function(i) {
    nomo_power_summary(runs[[i]], parameters[parameters$n == sizes[[i]], , drop = FALSE],
                       focus, sizes[[i]])
  }))
  meeting <- summary$n[summary$meets_references %in% TRUE]

  out <- list(
    call = match.call(),
    type = "simulation",
    population = population,
    analysis = analysis,
    reps = as.integer(reps),
    alpha = alpha,
    seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
    focus = focus,
    parameters = parameters,
    summary = summary,
    n_required = if (length(meeting)) min(meeting) else NA_integer_
  )
  class(out) <- c("nomo_power", "list")
  out
}


# The parameters with population values: loadings, regressions, and
# covariances between different variables.
nomo_power_truth <- function(population) {
  # Parsed without lavaan's defaults, which would fix a first loading at 1 and
  # make it look like a population value.
  table <- tryCatch(
    as.data.frame(lavaan::lavaanify(population)),
    error = function(e) {
      stop(paste0("Could not parse `population`: ", conditionMessage(e)), call. = FALSE)
    }
  )
  keep <- table$op %in% c("=~", "~", "~~") & !is.na(table$ustart) &
    !(table$op == "~~" & table$lhs == table$rhs)
  table <- table[keep, , drop = FALSE]
  if (!nrow(table)) {
    stop("`population` gives no parameter a value, such as `A =~ 0.7*a1`.", call. = FALSE)
  }
  tibble::tibble(
    parameter = paste0(table$lhs, table$op, table$rhs),
    lhs = table$lhs,
    op = table$op,
    rhs = table$rhs,
    population = table$ustart
  )
}


nomo_power_focus <- function(focus, truth) {
  if (is.null(focus)) return(truth$parameter[truth$population != 0])
  if (!is.character(focus) || !length(focus) || anyNA(focus)) {
    stop("`focus` must be NULL or a character vector of parameters.", call. = FALSE)
  }
  focus <- gsub("[[:space:]]+", "", focus)
  # A covariance may be written either way round.
  reversed <- vapply(strsplit(focus, "~~", fixed = TRUE), function(p) {
    if (length(p) == 2L) paste0(p[[2L]], "~~", p[[1L]]) else ""
  }, character(1))
  focus <- ifelse(focus %in% truth$parameter | !reversed %in% truth$parameter,
                  focus, reversed)
  unknown <- setdiff(focus, truth$parameter)
  if (length(unknown)) {
    stop(
      sprintf(
        "`focus` names parameters without population values: %s.",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  focus
}


# One sample size: each replication's estimates, standard errors, intervals,
# and p-values for the parameters with population values.
nomo_power_replicate <- function(population, analysis, truth, size, reps, standardized) {
  k <- nrow(truth)
  blank <- matrix(NA_real_, reps, k)
  out <- list(estimate = blank, se = blank, lower = blank, upper = blank,
              p = blank, converged = logical(reps), proper = logical(reps))
  for (r in seq_len(reps)) {
    data <- lavaan::simulateData(population, sample.nobs = size,
                                 standardized = standardized)
    fit <- tryCatch(
      suppressWarnings(lavaan::sem(analysis, data = data, std.lv = TRUE)),
      error = function(e) NULL
    )
    if (is.null(fit) || !isTRUE(lavaan::lavInspect(fit, "converged"))) next
    out$converged[[r]] <- TRUE
    out$proper[[r]] <- isTRUE(suppressWarnings(lavaan::lavInspect(fit, "post.check")))
    pe <- lavaan::parameterEstimates(fit)
    for (j in seq_len(k)) {
      hit <- pe$op == truth$op[[j]] &
        ((pe$lhs == truth$lhs[[j]] & pe$rhs == truth$rhs[[j]]) |
           (truth$op[[j]] == "~~" & pe$lhs == truth$rhs[[j]] & pe$rhs == truth$lhs[[j]]))
      if (!any(hit)) next
      row <- pe[which(hit)[[1L]], , drop = FALSE]
      out$estimate[r, j] <- row$est
      out$se[r, j] <- row$se
      out$lower[r, j] <- row$ci.lower
      out$upper[r, j] <- row$ci.upper
      out$p[r, j] <- row$pvalue
    }
  }
  out
}


nomo_power_parameters <- function(run, truth, size, alpha) {
  used <- run$converged
  column <- function(m, f) {
    apply(m[used, , drop = FALSE], 2L, function(v) f(v[is.finite(v)]))
  }
  mean_estimate <- column(run$estimate, mean)
  sd_estimate <- column(run$estimate, stats::sd)
  mean_se <- column(run$se, mean)
  covered <- (run$lower <= rep(truth$population, each = nrow(run$lower))) &
    (run$upper >= rep(truth$population, each = nrow(run$upper)))
  tibble::tibble(
    n = size,
    parameter = truth$parameter,
    population = truth$population,
    mean_estimate = mean_estimate,
    relative_bias = ifelse(truth$population != 0,
                           (mean_estimate - truth$population) / truth$population,
                           NA_real_),
    sd_estimate = sd_estimate,
    mean_se = mean_se,
    se_bias = (mean_se - sd_estimate) / sd_estimate,
    coverage = column(covered * 1, mean),
    power = column((run$p < alpha) * 1, mean)
  )
}


nomo_power_summary <- function(run, parameters, focus, size) {
  in_focus <- parameters$parameter %in% focus
  bias_ok <- all(abs(parameters$relative_bias) <= 0.10, na.rm = TRUE)
  se_ok <- all(abs(parameters$se_bias) <= 0.10) &&
    all(abs(parameters$se_bias[in_focus]) <= 0.05)
  coverage_ok <- all(parameters$coverage >= 0.91 & parameters$coverage <= 0.98)
  power_ok <- all(parameters$power[in_focus] >= 0.80)
  tibble::tibble(
    n = size,
    converged = mean(run$converged),
    improper = if (any(run$converged)) mean(!run$proper[run$converged]) else NA_real_,
    min_power = min(parameters$power[in_focus]),
    max_abs_bias = suppressWarnings(max(abs(parameters$relative_bias), na.rm = TRUE)),
    max_abs_se_bias = max(abs(parameters$se_bias)),
    min_coverage = min(parameters$coverage),
    max_coverage = max(parameters$coverage),
    meets_references = isTRUE(bias_ok && se_ok && coverage_ok && power_ok)
  )
}


#' @export
print.nomo_power <- function(x, ...) {
  if (identical(x$type, "rmsea")) {
    labels <- c(close = "close fit", not_close = "not-close fit", exact = "exact fit")
    nomo_present_header("nomo_power", sprintf("Power of the test of %s", labels[[x$test]]))
    nomo_present_facts(c(
      sprintf("df: %d", x$df),
      sprintf("RMSEA: null %s, alternative %s", format(x$rmsea_null), format(x$rmsea_alt)),
      sprintf("alpha: %s", format(x$alpha))
    ))
    nomo_present_text(
      if (is.na(x$n_required)) {
        sprintf("No N up to one million reaches power %s.", format(x$target_power))
      } else {
        sprintf("Smallest N for power %s: %d.", format(x$target_power), x$n_required)
      },
      indent = 2L
    )
    if (nrow(x$power)) {
      nomo_present_section("Power by sample size")
      nomo_present_table(x$power, c("N" = "n", "Power" = "power"))
    }
    cat("\n")
    nomo_present_text(
      "MacCallum, Browne, & Sugawara (1996). This is power for the overall fit ",
      "test, not for any one parameter; nomo_power_simulate() gives that."
    )
    return(invisible(x))
  }

  nomo_present_header("nomo_power", "Monte Carlo power and sample size")
  nomo_present_facts(c(
    sprintf("Replications: %d per N", x$reps),
    sprintf("alpha: %s", format(x$alpha)),
    sprintf("Focus: %s", paste(x$focus, collapse = ", "))
  ))
  summary <- x$summary
  summary$converged_shown <- sprintf("%.0f%%", 100 * summary$converged)
  summary$improper_shown <- ifelse(is.finite(summary$improper),
                                   sprintf("%.0f%%", 100 * summary$improper), "-")
  summary$coverage <- paste(nomo_present_number(summary$min_coverage, 2L),
                            nomo_present_number(summary$max_coverage, 2L), sep = "-")
  summary$meets <- ifelse(summary$meets_references, "yes", "no")
  nomo_present_section("By sample size")
  nomo_present_table(
    summary,
    c("N" = "n", "Converged" = "converged_shown", "Improper" = "improper_shown",
      "Min power" = "min_power", "Max bias" = "max_abs_bias",
      "Max SE bias" = "max_abs_se_bias", "Coverage" = "coverage",
      "Meets" = "meets"),
    formats = list(min_power = function(v) nomo_present_number(v, 2L),
                   max_abs_bias = function(v) nomo_present_number(v, 2L),
                   max_abs_se_bias = function(v) nomo_present_number(v, 2L)),
    more = "x$parameters"
  )
  nomo_present_text(
    if (is.na(x$n_required)) {
      "No simulated N meets the references; try larger ones."
    } else {
      sprintf("Smallest simulated N meeting the references: %d.", x$n_required)
    },
    indent = 2L
  )
  cat("\n")
  nomo_present_text(
    "Biases are absolute and relative. References (Muth\u00e9n & ",
    "Muth\u00e9n, 2002): parameter and SE bias within 10%, SE bias within 5% ",
    "for the focus parameters, coverage .91-.98, and power .80 for the focus ",
    "parameters. They are guides for choosing N, not rules."
  )
  invisible(x)
}
