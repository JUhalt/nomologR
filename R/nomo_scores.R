# Input and structure ----------------------------------------------------------

nomo_scores_input <- function(x) {
  fit <- if (inherits(x, "nomo_cfa")) x$fit else x

  is_lavaan <- inherits(fit, "lavaan") ||
    isTRUE(tryCatch(methods::is(fit, "lavaan"), error = function(e) FALSE))
  if (!is_lavaan) {
    stop(
      "`fit` must be a `nomo_cfa` object or a fitted `lavaan` model.",
      call. = FALSE
    )
  }

  if (!isTRUE(tryCatch(lavaan::lavInspect(fit, "converged"), error = function(e) FALSE))) {
    stop(
      "`fit` did not converge. Scores are not computed from a nonconverged model.",
      call. = FALSE
    )
  }

  ngroups <- tryCatch(lavaan::lavInspect(fit, "ngroups"), error = function(e) 1L)
  nlevels <- tryCatch(lavaan::lavInspect(fit, "nlevels"), error = function(e) 1L)
  if (!identical(as.integer(ngroups), 1L) || !identical(as.integer(nlevels), 1L)) {
    stop(
      "`nomo_scores()` supports single-group, single-level models only.",
      call. = FALSE
    )
  }

  pe <- lavaan::parameterEstimates(fit)
  lv <- as.character(lavaan::lavNames(fit, type = "lv"))
  ov <- as.character(lavaan::lavNames(fit, type = "ov"))

  loadings <- pe[pe$op == "=~" & pe$rhs %in% ov, c("lhs", "rhs"), drop = FALSE]
  factors <- lapply(lv, function(f) as.character(loadings$rhs[loadings$lhs == f]))
  names(factors) <- lv

  # A factor measured only by other factors, as a second-order factor is, has
  # no items to weight, and the weights below would describe a model lavaan did
  # not fit (#145).
  unmeasured <- lv[!lengths(factors)]
  if (length(unmeasured)) {
    stop(
      sprintf(
        paste(
          "`fit` has %s without observed indicators (%s), such as a",
          "higher-order factor. `nomo_scores()` scores factors from their",
          "items; score the first-order measurement model, without the",
          "higher-order factor."
        ),
        nomo_present_noun(length(unmeasured), "a factor", "factors"),
        paste(unmeasured, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (any(pe$op == "~" & pe$lhs %in% lv & pe$rhs %in% lv)) {
    stop(
      paste(
        "`fit` contains regressions among latent variables. `nomo_scores()`",
        "scores a measurement model; score the measurement model and fit the",
        "structural model on the latent variables instead."
      ),
      call. = FALSE
    )
  }

  # Any other regression, such as a factor on an observed covariate, or a
  # factor loading on another factor, puts a structural part (lavaan's beta
  # matrix) into the model, and the scores' weights are written for a
  # measurement model alone (#145).
  structural <- pe[pe$op == "~" | (pe$op == "=~" & pe$rhs %in% lv), , drop = FALSE]
  if (nrow(structural)) {
    stop(
      sprintf(
        paste(
          "`fit` contains structural paths (%s). `nomo_scores()` scores a",
          "measurement model, in which each factor is measured by its items",
          "and nothing is regressed; score the measurement model and fit the",
          "structural paths on the latent variables instead."
        ),
        paste(unique(paste(structural$lhs, structural$op, structural$rhs)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  cross <- names(which(table(loadings$rhs) > 1L))

  ordered <- as.character(tryCatch(
    lavaan::lavNames(fit, type = "ov.ord"),
    error = function(e) character()
  ))

  list(
    fit = fit,
    pe = pe,
    lv = lv,
    ov = ov,
    factors = factors,
    cross_loaded = cross,
    ordered = ordered
  )
}


# Score computation ------------------------------------------------------------

# Unit-weighted scores are computed from the data lavaan actually used, so a
# case dropped from the fit does not acquire a score, and the supplied data is
# never modified.
nomo_scores_unit <- function(input, method) {
  data <- tryCatch(lavaan::lavInspect(input$fit, "data"), error = function(e) NULL)
  if (is.null(data)) {
    stop(
      paste(
        "The data used by `fit` could not be retrieved, so unit-weighted",
        "scores cannot be computed. Refit with the data available, or use",
        "`method = \"regression\"`."
      ),
      call. = FALSE
    )
  }
  data <- as.data.frame(data)

  out <- lapply(input$factors, function(items) {
    block <- as.matrix(data[, items, drop = FALSE])
    if (identical(method, "mean")) rowMeans(block) else rowSums(block)
  })

  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}


nomo_scores_refined <- function(input, method) {
  engine_method <- if (identical(method, "bartlett")) "Bartlett" else "regression"

  scores <- tryCatch(
    lavaan::lavPredict(input$fit, method = engine_method),
    error = function(e) e
  )
  if (inherits(scores, "error")) {
    stop(
      paste0(
        "lavaan could not compute ", method, " factor scores: ",
        conditionMessage(scores), "."
      ),
      call. = FALSE
    )
  }

  scores <- as.data.frame(as.matrix(scores), check.names = FALSE)
  scores[, input$lv, drop = FALSE]
}


# Model-implied score properties -----------------------------------------------
#
# Grice (2001) evaluates factor scores on three criteria, all of which are
# properties of the model rather than of a particular sample: validity, the
# correlation between a score and its own factor; univocality, its correlation
# with the other factors; and correlational accuracy, whether correlations
# among scores match correlations among the factors.
#
# All three follow from the weight matrix W that produces the scores, since
# cov(factors, scores) = Phi Lambda' W and cov(scores) = W' Sigma W. Writing W
# for each method puts unit-weighted and refined scores on the same footing,
# which is the point: a sum score is a weighted composite whose weights are all
# one, not a different kind of object.
nomo_scores_weights <- function(input, method) {
  est <- lavaan::lavInspect(input$fit, "est")
  lambda <- est$lambda
  psi <- est$psi
  theta <- est$theta
  items <- rownames(lambda)
  factors <- colnames(lambda)

  sigma <- lambda %*% psi %*% t(lambda) + theta

  weights <- switch(
    method,
    sum = ,
    mean = {
      w <- matrix(0, nrow = length(items), ncol = length(factors),
                  dimnames = list(items, factors))
      for (f in factors) {
        in_f <- items %in% input$factors[[f]]
        w[in_f, f] <- if (identical(method, "mean")) 1 / sum(in_f) else 1
      }
      w
    },
    regression = solve(sigma) %*% lambda %*% psi,
    bartlett = {
      inv_theta <- solve(theta)
      inv_theta %*% lambda %*% solve(t(lambda) %*% inv_theta %*% lambda)
    }
  )
  dimnames(weights) <- list(items, factors)

  list(
    weights = weights, lambda = lambda, psi = psi, theta = theta,
    sigma = sigma, items = items, factors = factors
  )
}


nomo_scores_properties <- function(parts) {
  w <- parts$weights
  psi <- parts$psi
  factors <- parts$factors

  # cov(factor, score) and cov(score, score) implied by the fitted model.
  cov_fs <- psi %*% t(parts$lambda) %*% w
  cov_ss <- t(w) %*% parts$sigma %*% w

  sd_f <- sqrt(diag(psi))
  sd_s <- sqrt(diag(cov_ss))
  cor_fs <- cov_fs / outer(sd_f, sd_s)
  cor_ss <- cov_ss / outer(sd_s, sd_s)
  cor_ff <- psi / outer(sd_f, sd_f)

  dimnames(cor_fs) <- list(factors, factors)
  dimnames(cor_ss) <- list(factors, factors)
  dimnames(cor_ff) <- list(factors, factors)

  validity <- diag(cor_fs)

  # Univocality: a score's correlation with the factors it does not represent.
  univocality <- vapply(seq_along(factors), function(k) {
    other <- cor_fs[-k, k, drop = TRUE]
    if (!length(other)) return(NA_real_)
    other[which.max(abs(other))]
  }, numeric(1))

  # Correlational accuracy: how far a score correlation sits from the factor
  # correlation it stands in for. Reported per factor as the largest
  # discrepancy involving that factor.
  accuracy <- vapply(seq_along(factors), function(k) {
    if (length(factors) < 2L) return(NA_real_)
    diff <- (cor_ss[k, -k, drop = TRUE] - cor_ff[k, -k, drop = TRUE])
    diff[which.max(abs(diff))]
  }, numeric(1))

  list(
    validity = stats::setNames(validity, factors),
    univocality = stats::setNames(univocality, factors),
    accuracy = stats::setNames(accuracy, factors),
    cor_scores = cor_ss,
    cor_factors = cor_ff,
    # Rows are factors and columns scores, so [j, k] is the correlation of
    # factor j with the score for factor k.
    cor_factor_scores = cor_fs
  )
}


# Univocality judged against the factor correlations (#145). A score reaches
# the other factors through its own: if it carried nothing of factor j beyond
# what factor k brings, its correlation with j would be the factor correlation
# times its validity, as it is for Bartlett scores, and for sum scores of a
# simple structure, whatever the factors' correlation. The departure from that
# is what a score takes from another factor directly, which is what Grice
# (2001) asks univocality to reveal. [j, k] is for the score of factor k.
nomo_scores_univocality_departure <- function(properties) {
  through <- properties$cor_factors *
    rep(unname(properties$validity), each = nrow(properties$cor_factors))
  departure <- properties$cor_factor_scores - through
  diag(departure) <- NA_real_
  list(departure = departure, through = through)
}


# Unit weighting as a model ----------------------------------------------------
#
# McNeish and Wolf (2020) show that sum scoring is not a model-free arithmetic
# calculation but a parallel factor model: unstandardized loadings and error
# variances assumed identical across items. So the question "are these items
# summable" is a question about whether that constrained model is tenable, and
# it is answered by comparing it with the congeneric model the researcher
# fitted, not by a rule of thumb about loading spread.
nomo_scores_unit_weighting <- function(input, parts) {
  std <- lavaan::standardizedSolution(input$fit)
  std <- std[std$op == "=~" & std$rhs %in% parts$items, , drop = FALSE]

  spread <- lapply(names(input$factors), function(f) {
    l <- std$est.std[std$lhs == f]
    l <- l[is.finite(l)]
    if (length(l) < 2L) {
      return(data.frame(
        factor = f, n_items = length(l), min_loading = NA_real_,
        max_loading = NA_real_, loading_ratio = NA_real_, loading_sd = NA_real_
      ))
    }
    data.frame(
      factor = f,
      n_items = length(l),
      min_loading = min(l),
      max_loading = max(l),
      loading_ratio = if (min(abs(l)) > 0) max(abs(l)) / min(abs(l)) else NA_real_,
      loading_sd = stats::sd(l)
    )
  })

  tibble::as_tibble(do.call(rbind, spread))
}


# Items whose standardized loading has the opposite sign to their factor's
# strongest loading. A sum adds them as they are, so each counts against the
# factor it measures; the model, by contrast, weights them negatively (#145).
# A named list: one character vector per factor, empty for most.
nomo_scores_opposite_signs <- function(input) {
  std <- lavaan::standardizedSolution(input$fit)
  std <- std[std$op == "=~" & is.finite(std$est.std), , drop = FALSE]
  out <- lapply(names(input$factors), function(f) {
    rows <- std[std$lhs == f & std$rhs %in% input$factors[[f]], , drop = FALSE]
    strongest <- sign(rows$est.std[which.max(abs(rows$est.std))])
    as.character(rows$rhs[sign(rows$est.std) == -strongest])
  })
  stats::setNames(out, names(input$factors))
}


nomo_scores_parallel_test <- function(input) {
  empty <- list(
    available = FALSE, chisq_diff = NA_real_, df_diff = NA_real_,
    p_value = NA_real_, note = ""
  )

  # The parallel model below is written for continuous indicators: equal
  # loadings and equal residual variances. With ordered indicators the
  # researcher's model uses a categorical estimator, in which an item's
  # residual variance is not a free parameter to hold equal, so the test is
  # not run rather than run against the wrong model.
  if (length(input$ordered)) {
    empty$note <- paste(
      "The parallel-model test is implemented here for continuous indicators.",
      "These indicators are ordered and were fitted with a categorical",
      "estimator, so the constraints that unit weighting assumes were not",
      "tested."
    )
    return(empty)
  }

  table <- nomo_scores_parallel_table(input)
  if (is.null(table)) {
    empty$note <- paste(
      "The parallel model could not be written for this fit, so the",
      "constraints that unit weighting assumes were not tested."
    )
    return(empty)
  }

  # The parallel model is estimated exactly as the fitted model was: the same
  # cases and sample statistics, estimator, missing-data handling, mean
  # structure, and test statistic. A refit with lavaan's defaults would set a
  # maximum-likelihood, listwise model against, say, a robust or
  # full-information one, and lavaan refuses that comparison.
  options <- input$fit@Options
  # Its standard errors are never read, so they are not resampled.
  if (identical(options$se, "bootstrap")) options$se <- "standard"
  parallel_fit <- tryCatch(
    suppressWarnings(lavaan::lavaan(
      model = table,
      slotOptions = options,
      slotData = input$fit@Data,
      slotSampleStats = input$fit@SampleStats
    )),
    error = function(e) e
  )
  if (inherits(parallel_fit, "error")) {
    empty$note <- paste0(
      "The parallel model could not be fitted, so the constraints that unit ",
      "weighting assumes were not tested. lavaan reported: ",
      nomo_scores_condition(parallel_fit), "."
    )
    return(empty)
  }
  if (!isTRUE(lavaan::lavInspect(parallel_fit, "converged"))) {
    empty$note <- paste(
      "The parallel model did not converge, so the constraints that unit",
      "weighting assumes could not be tested against this model."
    )
    return(empty)
  }

  # A model that already holds the loadings and the residual variances equal
  # gains no constraint, and a difference on no degrees of freedom is no test.
  # lavaan gives no degrees of freedom for a fit without a test statistic
  # (test = "none"), and then there is no comparison to make either: that is a
  # comparison not computed, not a model that is already parallel.
  added <- tryCatch(
    unname(
      lavaan::fitMeasures(parallel_fit, "df") - lavaan::fitMeasures(input$fit, "df")
    ),
    error = function(e) e
  )
  if (inherits(added, "error") || !isTRUE(is.finite(added))) {
    empty$note <- paste(
      "The comparison between the fitted model and the parallel model could",
      "not be computed, so unit weighting was not tested.",
      if (inherits(added, "error")) {
        sprintf("lavaan reported: %s.", nomo_scores_condition(added))
      } else {
        "lavaan did not report the degrees of freedom of the two models."
      }
    )
    return(empty)
  }
  if (added <= 0) {
    empty$note <- paste(
      "The model you fitted already holds the loadings and the residual",
      "variances equal within each factor, so the parallel model adds no",
      "constraint to it and there is nothing further to test: the fit of the",
      "model you fitted is itself the evidence for what unit weighting assumes."
    )
    return(empty)
  }

  warnings <- character()
  test <- tryCatch(
    withCallingHandlers(
      as.data.frame(lavaan::lavTestLRT(input$fit, parallel_fit)),
      warning = function(w) {
        warnings <<- unique(c(warnings, nomo_scores_condition(w)))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) e
  )
  if (inherits(test, "error")) {
    empty$note <- paste0(
      "The comparison between the fitted model and the parallel model could ",
      "not be computed, so unit weighting was not tested. lavaan reported: ",
      nomo_scores_condition(test), "."
    )
    return(empty)
  }

  row <- test[nrow(test), , drop = FALSE]
  chisq_diff <- nomo_compare_lrt_value(row, "chisq.*diff|diff.*chisq")
  df_diff <- nomo_compare_lrt_value(row, "^df.*diff|diff.*df")
  p_value <- nomo_compare_lrt_value(row, "pr\\(>chisq\\)|p.*value|pvalue")

  # Constraints that hold exactly leave a difference of zero, which the two
  # optimizations reach only to rounding error, on either side of it.
  if (is.finite(chisq_diff) && abs(chisq_diff) < 1e-6) chisq_diff <- 0

  # The parallel model is nested in the fitted one, so its chi-square cannot be
  # the smaller of the two. A negative difference comes from a scaled statistic
  # or from an optimization that stopped short, and it is not a test: its
  # p value of 1 would read as support for unit weighting.
  usable <- is.finite(chisq_diff) && chisq_diff >= 0 && is.finite(df_diff) &&
    df_diff > 0 && is.finite(p_value)
  if (!usable) {
    empty$note <- paste(c(
      sprintf(
        paste(
          "The comparison between the fitted model and the parallel model did",
          "not give a usable chi-square difference (lavaan returned %s on %s",
          "df), so unit weighting was not tested."
        ),
        if (is.finite(chisq_diff)) nomo_present_stat(chisq_diff, "stat") else "none",
        nomo_present_df(df_diff)
      ),
      sprintf("lavaan reported: %s.", warnings)
    ), collapse = " ")
    return(empty)
  }

  list(
    available = TRUE,
    chisq_diff = chisq_diff,
    df_diff = df_diff,
    p_value = p_value,
    note = ""
  )
}


# lavaan's message on one line, without a closing full stop, since the notes
# that quote it add their own.
nomo_scores_condition <- function(condition) {
  sub("[.]+$", "", trimws(gsub("[[:space:]]+", " ", conditionMessage(condition))))
}


# The parallel model is the fitted model with two sets of constraints added and
# nothing taken away: within each factor, one value for every loading and one
# for every residual variance, which is what unit weighting assumes about the
# items it adds together. It is written from the fitted model's own parameter
# table, so that everything else the researcher specified (factor covariances
# fixed or free, residual covariances, intercepts, constraints of their own) is
# carried over unchanged and the two models are nested.
nomo_scores_parallel_table <- function(input) {
  if (!length(input$factors) || length(input$cross_loaded)) return(NULL)
  if (any(lengths(input$factors) < 2L)) return(NULL)

  table <- as.data.frame(lavaan::parTable(input$fit), stringsAsFactors = FALSE)
  # Starting values and estimates belong to the fitted model. Defined
  # parameters do not change the fit, and one that names a loading fixed below
  # could no longer be evaluated.
  table <- table[
    table$op != ":=", setdiff(names(table), c("start", "est", "se")),
    drop = FALSE
  ]

  equal <- list()
  for (f in names(input$factors)) {
    items <- input$factors[[f]]
    sets <- list(
      which(table$op == "=~" & table$lhs == f & table$rhs %in% items),
      which(table$op == "~~" & table$lhs == table$rhs & table$lhs %in% items)
    )
    for (rows in sets) {
      free <- rows[table$free[rows] > 0L]
      value <- unique(table$ustart[setdiff(rows, free)])
      # Parameters the researcher fixed at different values cannot also be
      # equal, so no parallel model is nested in that fit; nor is one where the
      # fit holds fewer than two of them to set equal.
      if (length(rows) < 2L || length(value) > 1L || anyNA(value)) return(NULL)
      if (length(value)) {
        # One is already fixed, as a marker loading is at 1: the others take
        # its value, which leaves the factor's scale where the fit put it.
        table$free[free] <- 0L
        table$ustart[free] <- value
      } else {
        equal[[length(equal) + 1L]] <- data.frame(
          lhs = table$plabel[free[[1L]]], rhs = table$plabel[free[-1L]],
          stringsAsFactors = FALSE
        )
      }
    }
  }

  table$free[table$free > 0L] <- seq_len(sum(table$free > 0L))
  equal <- do.call(rbind, equal)
  if (!is.null(equal)) {
    constraints <- table[rep(1L, nrow(equal)), , drop = FALSE]
    constraints$lhs <- equal$lhs
    constraints$op <- "=="
    constraints$rhs <- equal$rhs
    constraints$user <- 2L
    constraints$block <- 0L
    constraints$group <- 0L
    constraints$free <- 0L
    constraints$ustart <- NA_real_
    constraints$exo <- 0L
    constraints$label <- ""
    constraints$plabel <- ""
    table <- rbind(table, constraints)
  }
  table$id <- seq_len(nrow(table))
  rownames(table) <- NULL
  table
}
