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

  loadings <- pe[pe$op == "=~" & pe$rhs %in% ov, c("lhs", "rhs"), drop = FALSE]
  factors <- lapply(lv, function(f) as.character(loadings$rhs[loadings$lhs == f]))
  names(factors) <- lv

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
    cor_factors = cor_ff
  )
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


nomo_scores_parallel_test <- function(input) {
  empty <- list(
    available = FALSE, chisq_diff = NA_real_, df_diff = NA_real_,
    p_value = NA_real_, note = ""
  )

  # The parallel model below is written for continuous indicators: equal
  # loadings and equal residual variances, fitted by maximum likelihood. With
  # ordered indicators the researcher's model uses a categorical estimator, and
  # a continuous parallel model is not a nested alternative to it, so the test
  # is not run rather than run against the wrong model.
  if (length(input$ordered)) {
    empty$note <- paste(
      "The parallel-model test is implemented here for continuous indicators.",
      "These indicators are ordered and were fitted with a categorical",
      "estimator, so the constraints that unit weighting assumes were not",
      "tested."
    )
    return(empty)
  }

  syntax <- nomo_scores_parallel_syntax(input)
  if (is.null(syntax)) {
    empty$note <- paste(
      "The parallel model could not be written for this fit, so the",
      "constraints that unit weighting assumes were not tested."
    )
    return(empty)
  }

  # Unit scoring has already retrieved these data, so they are available here.
  parallel_fit <- tryCatch(
    suppressWarnings(lavaan::cfa(
      syntax,
      data = as.data.frame(lavaan::lavInspect(input$fit, "data"))
    )),
    error = function(e) e
  )
  if (inherits(parallel_fit, "error") ||
        !isTRUE(tryCatch(lavaan::lavInspect(parallel_fit, "converged"),
                         error = function(e) FALSE))) {
    empty$note <- paste(
      "The parallel model did not converge, so the constraints that unit",
      "weighting assumes could not be tested against this model."
    )
    return(empty)
  }

  test <- tryCatch(
    suppressWarnings(as.data.frame(lavaan::lavTestLRT(input$fit, parallel_fit))),
    error = function(e) e
  )
  if (inherits(test, "error") || nrow(test) < 2L) {
    empty$note <- paste(
      "The comparison between the fitted model and the parallel model could",
      "not be computed, so unit weighting was not tested."
    )
    return(empty)
  }

  row <- test[nrow(test), , drop = FALSE]
  list(
    available = TRUE,
    chisq_diff = nomo_compare_lrt_value(row, "chisq.*diff|diff.*chisq"),
    df_diff = nomo_compare_lrt_value(row, "^df.*diff|diff.*df"),
    p_value = nomo_compare_lrt_value(row, "pr\\(>chisq\\)|p.*value|pvalue"),
    note = ""
  )
}


# The parallel model constrains every loading to a single value and every
# residual variance to a single value, within each factor, which is what unit
# weighting assumes about the items it adds together.
nomo_scores_parallel_syntax <- function(input) {
  if (!length(input$factors) || length(input$cross_loaded)) return(NULL)

  blocks <- vapply(names(input$factors), function(f) {
    items <- input$factors[[f]]
    if (length(items) < 2L) return(NA_character_)
    label <- paste0("l_", f)
    loadings <- paste(sprintf("%s*%s", label, items), collapse = " + ")
    residuals <- paste(
      sprintf("%s ~~ e_%s*%s", items, f, items),
      collapse = "\n"
    )
    paste0(f, " =~ ", loadings, "\n", residuals)
  }, character(1))

  if (anyNA(blocks)) return(NULL)
  paste(blocks, collapse = "\n")
}
