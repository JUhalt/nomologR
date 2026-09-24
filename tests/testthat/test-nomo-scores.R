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


test_that("the regression-then-Bartlett design recovers a latent regression (#62)", {
  pop <- scores_population(r = .50)

  # A sample whose covariance is exactly the population's, so every estimate is
  # the population value and the comparison is not clouded by sampling error.
  set.seed(62)
  z <- scale(matrix(stats::rnorm(400 * 8), 400, 8), scale = FALSE)
  z <- z %*% solve(chol(stats::cov(z))) %*% chol(pop$sigma)
  dat <- as.data.frame(z)
  names(dat) <- scores_items

  predictor_model <- lavaan::cfa("F1 =~ x1 + x2 + x3 + x4", data = dat, std.lv = TRUE)
  outcome_model <- lavaan::cfa("F2 =~ x5 + x6 + x7 + x8", data = dat, std.lv = TRUE)
  joint_model <- lavaan::cfa(scores_model, data = dat, std.lv = TRUE)

  slope <- function(outcome, predictor) {
    unname(stats::coef(stats::lm(outcome ~ predictor))[2])
  }
  score <- function(fit, method, factor) nomo_scores(fit, method = method)$scores[[factor]]

  # Skrondal and Laake (2001): regression scores for the predictor and Bartlett
  # scores for the outcome, each from a model of its own block, are consistent.
  expect_equal(
    slope(score(outcome_model, "bartlett", "F2"), score(predictor_model, "regression", "F1")),
    .50, tolerance = 1e-4
  )

  # Both conditions matter. The same method for both blocks falls short, and
  # scoring both factors from one joint model overshoots.
  expect_lt(
    slope(score(outcome_model, "regression", "F2"), score(predictor_model, "regression", "F1")),
    .45
  )
  expect_gt(
    slope(score(joint_model, "bartlett", "F2"), score(joint_model, "regression", "F1")),
    .55
  )

  # The note on a jointly scored model says it is not that design.
  note <- nomo_scores(joint_model, method = "regression")$notes
  accuracy <- note$note[note$topic == "correlational_accuracy"]
  expect_match(accuracy, "Skrondal and Laake (2001)", fixed = TRUE)
  expect_match(accuracy, "each from a measurement model of its own", fixed = TRUE)
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


# Refusals and notes on real fits (#72) ----------------------------------------

test_that("models that cannot be scored honestly are refused, each with its reason", {
  dat <- scores_sample(scores_population()$sigma)

  unconverged <- suppressWarnings(
    lavaan::cfa(scores_model, data = dat, control = list(iter.max = 2))
  )
  expect_false(lavaan::lavInspect(unconverged, "converged"))
  expect_error(nomo_scores(unconverged), "did not converge")

  grouped <- lavaan::cfa("Agency =~ ag1 + ag2 + ag3 + ag4",
                         data = nomo_demo_network, group = "group")
  expect_error(nomo_scores(grouped), "single-group, single-level")

  # Fitted from a covariance matrix, the model has no cases to score: a unit
  # weight needs the responses, and lavaan's own prediction needs them too.
  summary_only <- lavaan::cfa(scores_model, sample.cov = stats::cov(dat),
                              sample.nobs = nrow(dat))
  expect_error(nomo_scores(summary_only, method = "sum"), "could not be retrieved")
  expect_error(nomo_scores(summary_only, method = "regression"),
               "lavaan could not compute regression factor scores")
})


test_that("a single-indicator factor has no loading spread and no parallel test", {
  dat <- scores_sample(scores_population()$sigma)
  fit <- lavaan::cfa("F1 =~ x1 + x2 + x3 + x4\nF2 =~ x5\nx5 ~~ 0*x5", data = dat)
  out <- nomo_scores(fit, method = "sum")

  spread <- out$unit_weighting[out$unit_weighting$factor == "F2", ]
  expect_identical(spread$n_items, 1L)
  expect_true(is.na(spread$loading_ratio))
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "could not be written", fixed = TRUE)
})


test_that("cross-loaded items are named, and no parallel model is written for them", {
  fit <- nomo_cfa(
    "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5 + a5",
    data = nomo_demo_continuous
  )
  out <- nomo_scores(fit, method = "sum")

  expect_identical(out$parallel_test$available, FALSE)
  notes <- out$notes
  expect_match(notes$note[notes$topic == "cross_loadings"], "Item(s) a5", fixed = TRUE)
  expect_true(any(notes$topic == "unit_weighting" & notes$severity == "review" &
                    grepl("could not be written", notes$note, fixed = TRUE)))
})


test_that("ordered indicators are scored, with the estimand and the untested parallel model stated", {
  items <- c(paste0("a", 1:5), paste0("b", 1:5))
  fit <- nomo_cfa(
    "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
    data = nomo_demo_ordinal, ordered = items
  )
  out <- nomo_scores(fit, method = "sum")

  expect_identical(nrow(out$scores), nrow(nomo_demo_ordinal))
  expect_match(out$notes$note[out$notes$topic == "estimand" &
                                grepl("ordered", out$notes$note)],
               "10 indicator(s) are ordered", fixed = TRUE)

  # The parallel model is written for continuous indicators, so it is not run
  # against a categorical fit, and the reason says so.
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "implemented here for continuous indicators",
               fixed = TRUE)
})


test_that("loadings that differ twofold are flagged for a unit-weighted score", {
  # b5 is the weak item in the teaching data: its loading is under half b1's.
  fit <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
                  data = nomo_demo_continuous)
  out <- nomo_scores(fit, method = "sum")
  expect_true(any(grepl("at least twice the weakest for B", out$notes$note, fixed = TRUE)))
  expect_false(any(grepl("at least twice", nomo_scores(fit, "regression")$notes$note)))
})


test_that("a parallel model lavaan cannot fit or compare is reported, not hidden", {
  fit <- scores_fit()

  testthat::local_mocked_bindings(
    cfa = function(...) stop("simulated nonconvergence"),
    .package = "lavaan"
  )
  out <- nomo_scores(fit, method = "sum")
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "did not converge", fixed = TRUE)
})


test_that("a comparison lavaan cannot compute is reported, not hidden", {
  fit <- scores_fit()

  testthat::local_mocked_bindings(
    lavTestLRT = function(...) stop("simulated comparison failure"),
    .package = "lavaan"
  )
  out <- nomo_scores(fit, method = "sum")
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "could not be computed", fixed = TRUE)
})
