# Fixtures ---------------------------------------------------------------------

scores_items <- paste0("x", 1:8)

scores_population <- function(r = .50) {
  l1 <- c(.80, .75, .70, .65, 0, 0, 0, 0)
  l2 <- c(0, 0, 0, 0, .60, .55, .50, .45)
  L <- cbind(F1 = l1, F2 = l2)
  Phi <- matrix(c(1, r, r, 1), 2, dimnames = list(c("F1", "F2"), c("F1", "F2")))
  sigma <- L %*% Phi %*% t(L)
  diag(sigma) <- 1
  dimnames(sigma) <- list(scores_items, scores_items)
  list(sigma = sigma, lambda = L, phi = Phi)
}


scores_sample <- function(sigma, n = 600, seed = 3301) {
  set.seed(seed)
  z <- matrix(stats::rnorm(n * ncol(sigma)), n, ncol(sigma))
  dat <- as.data.frame(z %*% chol(sigma))
  names(dat) <- colnames(sigma)
  dat
}


scores_model <- "F1 =~ x1 + x2 + x3 + x4\nF2 =~ x5 + x6 + x7 + x8"


scores_fit <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    dat <- scores_sample(scores_population()$sigma)
    cache <<- lavaan::cfa(scores_model, data = dat, std.lv = TRUE)
    cache
  }
})


# Scores reproduce the engine --------------------------------------------------

test_that("refined scores reproduce lavaan::lavPredict exactly", {
  fit <- scores_fit()

  for (method in c("regression", "bartlett")) {
    engine <- as.matrix(lavaan::lavPredict(
      fit,
      method = if (identical(method, "bartlett")) "Bartlett" else "regression"
    ))
    ours <- as.matrix(nomo_scores(fit, method = method)$scores)
    expect_equal(unname(ours), unname(engine[, c("F1", "F2")]), tolerance = 1e-10)
  }
})


test_that("unit-weighted scores are the sum and the mean of the modeled items", {
  fit <- scores_fit()
  data <- as.data.frame(lavaan::lavInspect(fit, "data"))

  summed <- nomo_scores(fit, method = "sum")
  expect_equal(summed$scores$F1, rowSums(data[, paste0("x", 1:4)]))
  expect_equal(summed$scores$F2, rowSums(data[, paste0("x", 5:8)]))

  averaged <- nomo_scores(fit, method = "mean")
  expect_equal(averaged$scores$F1, rowMeans(data[, paste0("x", 1:4)]))

  # A mean is a linear transformation of a sum, so the model-implied properties
  # of the two are identical.
  expect_equal(summed$diagnostics$validity, averaged$diagnostics$validity)
  expect_equal(
    summed$diagnostics$correlational_accuracy,
    averaged$diagnostics$correlational_accuracy
  )
})


# Grice's criteria -------------------------------------------------------------

test_that("validity for regression scores is the factor determinacy coefficient", {
  fit <- scores_fit()
  est <- lavaan::lavInspect(fit, "est")
  sigma <- est$lambda %*% est$psi %*% t(est$lambda) + est$theta
  determinacy <- sqrt(diag(
    est$psi %*% t(est$lambda) %*% solve(sigma) %*% est$lambda %*% est$psi
  ))

  # Grice (2001): the regression method maximizes validity, so the validity
  # coefficient equals rho. This is the same quantity nomo_hierarchical()
  # reports as factor determinacy.
  expect_equal(
    nomo_scores(fit, method = "regression")$diagnostics$validity,
    unname(determinacy),
    tolerance = 1e-10
  )
})


test_that("model-implied score properties match the scores actually produced", {
  fit <- scores_fit()

  # The reported properties are model-implied: they describe the scores the
  # fitted model says these weights produce. The scores themselves come from
  # the data, so the two agree only to the extent the model reproduces the
  # sample covariances. Agreement close to the second decimal is the claim,
  # not identity.
  for (method in c("sum", "regression", "bartlett")) {
    out <- nomo_scores(fit, method = method)
    observed <- stats::cor(out$scores$F1, out$scores$F2)
    expect_lt(abs(out$score_correlations[1, 2] - observed), 0.02)
  }
})


test_that("correlational accuracy records the bias that scoring introduces", {
  fit <- scores_fit()
  factor_r <- nomo_scores(fit, method = "sum")$factor_correlations[1, 2]

  regression <- nomo_scores(fit, method = "regression")
  summed <- nomo_scores(fit, method = "sum")

  # The discrepancy is the score correlation minus the factor correlation it
  # stands in for, so it can be recovered from the two matrices.
  expect_equal(
    regression$diagnostics$correlational_accuracy[[1L]],
    regression$score_correlations[1, 2] - factor_r,
    tolerance = 1e-10
  )

  # The two weightings miss in opposite directions, which is why the package
  # reports the discrepancy rather than correcting for it.
  expect_gt(regression$diagnostics$correlational_accuracy[[1L]], 0)
  expect_lt(summed$diagnostics$correlational_accuracy[[1L]], 0)
})


test_that("a single-factor model has no univocality or accuracy to report", {
  dat <- scores_sample(scores_population()$sigma)
  fit <- lavaan::cfa(
    paste("F =~", paste(scores_items, collapse = " + ")),
    data = dat, std.lv = TRUE
  )

  out <- nomo_scores(fit, method = "regression")
  expect_identical(nrow(out$diagnostics), 1L)
  expect_true(is.na(out$diagnostics$univocality))
  expect_true(is.na(out$diagnostics$correlational_accuracy))
})


# Unit weighting is a model ----------------------------------------------------

test_that("unit weighting is tested as the parallel model it assumes", {
  fit <- scores_fit()
  out <- nomo_scores(fit, method = "sum")

  # Loadings were generated heterogeneous, so the parallel constraints should
  # be rejected.
  expect_true(out$parallel_test$available)
  expect_gt(out$parallel_test$chisq_diff, 0)
  expect_gt(out$parallel_test$df_diff, 0)
  expect_lt(out$parallel_test$p_value, .05)
  expect_match(
    paste(out$notes$note, collapse = " "),
    "parallel model that unit weighting assumes fits worse",
    fixed = TRUE
  )

  # A weighted score makes no such assumption, so no test is run for it.
  weighted <- nomo_scores(fit, method = "regression")
  expect_false(weighted$parallel_test$available)
})


test_that("parallel constraints consistent with the data are reported as such", {
  pop <- scores_population()
  equal <- c(rep(.65, 4), rep(.65, 4))
  L <- cbind(F1 = c(equal[1:4], 0, 0, 0, 0), F2 = c(0, 0, 0, 0, equal[5:8]))
  sigma <- L %*% pop$phi %*% t(L)
  diag(sigma) <- 1
  dimnames(sigma) <- list(scores_items, scores_items)

  fit <- lavaan::cfa(scores_model, data = scores_sample(sigma, seed = 77),
                     std.lv = TRUE)
  out <- nomo_scores(fit, method = "sum")

  expect_true(out$parallel_test$available)
  expect_gt(out$parallel_test$p_value, .05)
  expect_match(
    paste(out$notes$note, collapse = " "),
    "does not fit worse than the model you fitted",
    fixed = TRUE
  )
})


# Refusals and guards ----------------------------------------------------------

test_that("nomo_scores refuses what it cannot score honestly", {
  fit <- scores_fit()

  expect_error(nomo_scores(list()), "nomo_cfa")
  expect_error(nomo_scores(fit, method = "eap"), "should be one of")

  structural <- lavaan::sem(
    paste(scores_model, "\nF2 ~ F1"),
    data = scores_sample(scores_population()$sigma), std.lv = TRUE
  )
  expect_error(nomo_scores(structural), "regressions among latent variables")
})


test_that("supplied data is never modified and only scored cases are returned", {
  dat <- scores_sample(scores_population()$sigma)
  dat$x1[c(3L, 9L)] <- NA
  before <- dat

  fit <- lavaan::cfa(scores_model, data = dat, std.lv = TRUE)
  out <- nomo_scores(fit, method = "sum")

  expect_identical(dat, before)
  expect_identical(nrow(out$scores), nrow(dat) - 2L)
})


# Presentation -----------------------------------------------------------------

test_that("nomo_scores prints, summarizes, and tabulates its evidence", {
  out <- nomo_scores(scores_fit(), method = "sum")

  expect_output(print(out), "Score properties")
  expect_output(print(out), "Parallel model")
  expect_output(print(summary(out)), "Standardized loading spread")

  expect_identical(nomo_table(out, "scores"), out$scores)
  expect_identical(nomo_table(out, "diagnostics"), out$diagnostics)
  expect_identical(nomo_table(out, "unit_weighting"), out$unit_weighting)
  expect_identical(nomo_table(out, "notes"), out$notes)
})


# Crediting --------------------------------------------------------------------

test_that("nomo_scores credits only the methods a run actually used", {
  fit <- scores_fit()

  summed <- nomo_methods_used(nomo_scores(fit, method = "sum"))
  expect_true(all(c(
    "unit_weighted_score", "parallel_model_test", "factor_score_validity",
    "factor_score_univocality", "factor_score_correlational_accuracy"
  ) %in% summed))
  expect_false("factor_score_regression" %in% summed)

  weighted <- nomo_methods_used(nomo_scores(fit, method = "regression"))
  expect_true("factor_score_regression" %in% weighted)
  expect_false("unit_weighted_score" %in% weighted)
  # No unit weighting, so no constraints to test and no citation for testing them.
  expect_false("parallel_model_test" %in% weighted)

  expect_true("factor_score_bartlett" %in%
                nomo_methods_used(nomo_scores(fit, method = "bartlett")))

  # Univocality and correlational accuracy need a second factor to exist, so a
  # single-factor run must not be credited with them.
  dat <- scores_sample(scores_population()$sigma)
  single <- lavaan::cfa(
    paste("F =~", paste(scores_items, collapse = " + ")),
    data = dat, std.lv = TRUE
  )
  one_factor <- nomo_methods_used(nomo_scores(single, method = "regression"))
  expect_true("factor_score_validity" %in% one_factor)
  expect_false("factor_score_univocality" %in% one_factor)
  expect_false("factor_score_correlational_accuracy" %in% one_factor)

  # Every credited id exists in the registry, with its sources.
  credited <- nomo_methods(nomo_scores(fit, method = "sum"))
  expect_true(all(summed %in% credited$id))
  expect_true(all(nzchar(credited$references)))
})
