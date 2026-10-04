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

  # Nothing here is flagged, so the print has no Flagged section (#145).
  local_reproducible_output(width = 80)
  printed <- capture.output(print(out))
  expect_false("Flagged" %in% printed)
  expect_match(printed, "^  F +8 +[.]9[0-9]$", all = FALSE)
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
  expect_error(nomo_scores(fit, method = "eap"),
               '`method` must be one of "sum", "mean", "regression", or "bartlett", not "eap".',
               fixed = TRUE)

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
  # The rejected parallel model is a flag, so it is printed with the others.
  expect_output(print(out), "Unit weighting (Review): The parallel model", fixed = TRUE)
  expect_output(print(summary(out)), "Standardized loading spread")

  # The summary's class is named as every other summary class is; the name it
  # had before 1.0.0 stays as a second class for one release (#145).
  expect_identical(class(summary(out)),
                   c("summary_nomo_scores", "summary.nomo_scores", "list"))
  expect_true(is.function(getS3method("print", "summary_nomo_scores")))

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
  expect_match(notes$note[notes$topic == "cross_loadings"],
               "Item a5 loads on more than one factor.", fixed = TRUE)
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
               "10 indicators are ordered", fixed = TRUE)

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


test_that("a parallel model lavaan cannot fit is reported with lavaan's reason, not hidden", {
  fit <- scores_fit()

  testthat::local_mocked_bindings(
    lavaan = function(...) stop("simulated \n   estimation failure"),
    .package = "lavaan"
  )
  out <- nomo_scores(fit, method = "sum")
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "could not be fitted", fixed = TRUE)
  expect_match(out$parallel_test$note,
               "lavaan reported: simulated estimation failure.", fixed = TRUE)
  # The reason reaches the reader as a note for review.
  expect_true(any(out$notes$severity == "review" &
                    grepl("simulated estimation failure", out$notes$note, fixed = TRUE)))
})


test_that("a parallel model that does not converge is reported, not hidden", {
  fit <- scores_fit()
  unconverged <- suppressWarnings(lavaan::cfa(
    scores_model, data = scores_sample(scores_population()$sigma),
    control = list(iter.max = 2)
  ))
  expect_false(lavaan::lavInspect(unconverged, "converged"))

  testthat::local_mocked_bindings(
    lavaan = function(...) unconverged,
    .package = "lavaan"
  )
  out <- nomo_scores(fit, method = "sum")
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "did not converge", fixed = TRUE)
})


test_that("a comparison lavaan cannot compute is reported with lavaan's reason, not hidden", {
  fit <- scores_fit()

  testthat::local_mocked_bindings(
    lavTestLRT = function(...) stop("simulated comparison failure"),
    .package = "lavaan"
  )
  out <- nomo_scores(fit, method = "sum")
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "could not be computed", fixed = TRUE)
  expect_match(out$parallel_test$note,
               "lavaan reported: simulated comparison failure.", fixed = TRUE)
})


# The parallel model is the fitted model plus its constraints (#145) -----------

# The parallel model written out by hand, for lavaan to fit with whatever else
# a test specifies. `std.lv = TRUE` leaves the labelled loadings free.
scores_parallel_model <- paste(
  "F1 =~ l1*x1 + l1*x2 + l1*x3 + l1*x4",
  "F2 =~ l2*x5 + l2*x6 + l2*x7 + l2*x8",
  paste(sprintf("x%d ~~ e1*x%d", 1:4, 1:4), collapse = "\n"),
  paste(sprintf("x%d ~~ e2*x%d", 5:8, 5:8), collapse = "\n"),
  sep = "\n"
)

# nomo_scores()'s test against lavaan's own comparison of `fit` with `nested`.
expect_parallel_test <- function(fit, nested, df_diff = 12) {
  engine <- as.data.frame(lavaan::lavTestLRT(fit, nested))[2L, ]
  ours <- nomo_scores(fit, method = "sum")$parallel_test

  expect_true(ours$available)
  expect_identical(ours$note, "")
  expect_equal(ours$df_diff, df_diff)
  expect_equal(engine[["Df diff"]], df_diff)
  expect_equal(ours$chisq_diff, engine[["Chisq diff"]], tolerance = 1e-4)
  expect_equal(ours$p_value, engine[["Pr(>Chisq)"]], tolerance = 1e-4)
  expect_gt(ours$chisq_diff, 0)
  invisible(ours)
}


test_that("factors fixed as uncorrelated stay uncorrelated in the parallel model", {
  dat <- scores_sample(scores_population()$sigma)

  # Freeing the covariance in the parallel model, as a model written from the
  # loadings alone does, makes it fit better than the model it is meant to be
  # nested in: a negative difference that was read as support for a sum score.
  fit <- lavaan::cfa(scores_model, data = dat, std.lv = TRUE, orthogonal = TRUE)
  nested <- lavaan::cfa(scores_parallel_model, data = dat, std.lv = TRUE,
                        orthogonal = TRUE)
  ours <- expect_parallel_test(fit, nested)
  expect_lt(ours$p_value, .05)

  notes <- nomo_scores(fit, method = "sum")$notes$note
  expect_true(any(grepl("fits worse than the model you fitted", notes, fixed = TRUE)))
  expect_false(any(grepl("consistent with these data", notes, fixed = TRUE)))

  # The same through nomo_cfa(), with the covariance fixed in the syntax and
  # the first loadings fixed as markers.
  fixed <- nomo_cfa(paste0(scores_model, "\nF1 ~~ 0*F2"), data = dat)
  expect_parallel_test(fixed$fit, nested)
})


test_that("a residual covariance is kept, so only the parallel constraints are tested", {
  dat <- scores_sample(scores_population()$sigma)
  extra <- "\nx1 ~~ x2\nloading_gap := a - b"
  # The defined parameter names loadings the parallel model fixes; it does not
  # change the fit and is left out of the refit.
  model <- paste0("F1 =~ x1 + a*x2 + b*x3 + x4\nF2 =~ x5 + x6 + x7 + x8", extra)

  fit <- lavaan::cfa(model, data = dat)
  nested <- lavaan::cfa(paste0(scores_parallel_model, "\nx1 ~~ x2"), data = dat,
                        std.lv = TRUE)
  # Twelve constraints, not thirteen: the residual covariance is not one of them.
  expect_parallel_test(fit, nested, df_diff = 12)
})


test_that("the parallel model is refitted with the fit's own estimator and missing-data handling", {
  dat <- scores_sample(scores_population()$sigma)

  robust <- nomo_cfa(scores_model, data = dat, estimator = "MLR", std.lv = TRUE)
  expect_parallel_test(
    robust$fit,
    lavaan::cfa(scores_parallel_model, data = dat, estimator = "MLR", std.lv = TRUE)
  )

  holes <- dat
  set.seed(145)
  for (item in scores_items) holes[[item]][sample(nrow(holes), 20L)] <- NA
  fiml <- nomo_cfa(scores_model, data = holes, missing = "ml", std.lv = TRUE)
  ours <- expect_parallel_test(
    fiml$fit,
    lavaan::cfa(scores_parallel_model, data = holes, missing = "ml", std.lv = TRUE)
  )

  # Listwise deletion on the same data is a different test, on fewer cases.
  listwise <- nomo_cfa(scores_model, data = holes, std.lv = TRUE)
  dropped <- expect_parallel_test(
    listwise$fit,
    lavaan::cfa(scores_parallel_model, data = holes, std.lv = TRUE)
  )
  expect_false(isTRUE(all.equal(ours$chisq_diff, dropped$chisq_diff)))

  # Bootstrapped standard errors are not resampled for a model whose standard
  # errors are never read; the test is the one maximum likelihood gives. With
  # so few draws lavaan warns about the fitted model's own intervals.
  bootstrapped <- suppressWarnings(lavaan::cfa(
    scores_model, data = dat, std.lv = TRUE, se = "bootstrap", bootstrap = 5L
  ))
  expect_equal(
    suppressWarnings(nomo_scores(bootstrapped, method = "sum"))$parallel_test,
    nomo_scores(scores_fit(), method = "sum")$parallel_test,
    tolerance = 1e-6
  )
})


test_that("a fit that is already parallel, or cannot be, has no test and says why", {
  dat <- scores_sample(scores_population()$sigma)

  parallel <- lavaan::cfa(scores_parallel_model, data = dat, std.lv = TRUE)
  out <- nomo_scores(parallel, method = "sum")
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "adds no constraint to it", fixed = TRUE)

  # Loadings fixed at different values cannot also be held equal.
  unequal <- lavaan::cfa(
    "F1 =~ 1*x1 + 0.5*x2 + x3 + x4\nF2 =~ x5 + x6 + x7 + x8", data = dat
  )
  out <- nomo_scores(unequal, method = "sum")
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "could not be written", fixed = TRUE)
})


test_that("a fit without a test statistic is scored, with the comparison not computed", {
  dat <- scores_sample(scores_population()$sigma)

  # lavaan gives no fit measures for test = "none", the refit keeps the
  # option, and the two models cannot be compared. The scores are returned.
  fit <- lavaan::cfa(scores_model, data = dat, std.lv = TRUE, test = "none")
  out <- nomo_scores(fit, method = "sum")
  expect_equal(out$scores$F1, rowSums(dat[, paste0("x", 1:4)]))
  expect_false(out$parallel_test$available)
  expect_true(is.na(out$parallel_test$chisq_diff))
  expect_match(out$parallel_test$note, "could not be computed", fixed = TRUE)
  expect_match(out$parallel_test$note, "lavaan reported: ", fixed = TRUE)
  expect_false(grepl("already holds", out$parallel_test$note, fixed = TRUE))
  expect_false(grepl("..", out$parallel_test$note, fixed = TRUE))
  expect_false("parallel_model_test" %in% nomo_methods_used(out))
  expect_true(any(out$notes$severity == "review" &
                    grepl("could not be computed", out$notes$note, fixed = TRUE)))

  # Degrees of freedom left missing without an error are no comparison either,
  # and not a sign that the fitted model is already parallel.
  fit_measures <- lavaan::fitMeasures
  testthat::local_mocked_bindings(
    fitMeasures = function(object, fit.measures = "all", ...) {
      if (identical(fit.measures, "df")) return(c(df = NA_real_))
      fit_measures(object, fit.measures, ...)
    },
    .package = "lavaan"
  )
  out <- nomo_scores(scores_fit(), method = "sum")
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note,
               "did not report the degrees of freedom", fixed = TRUE)
  expect_false(grepl("already holds", out$parallel_test$note, fixed = TRUE))
})


test_that("a negative difference is reported as no test, not as support for a sum score", {
  fit <- scores_fit()
  difference <- function(value, p) {
    data.frame(
      Df = c(26, 38), `Chisq diff` = c(NA, value), `Df diff` = c(NA, 12),
      `Pr(>Chisq)` = c(NA, p), check.names = FALSE
    )
  }

  testthat::local_mocked_bindings(
    lavTestLRT = function(...) {
      warning("lavaan->lavTestLRT():  \n   some restricted models fit better")
      difference(-3.2, 1)
    },
    .package = "lavaan"
  )
  out <- nomo_scores(fit, method = "sum")
  expect_false(out$parallel_test$available)
  expect_true(is.na(out$parallel_test$p_value))
  expect_match(out$parallel_test$note,
               "usable chi-square difference (lavaan returned -3.20 on 12 df)",
               fixed = TRUE)
  expect_match(out$parallel_test$note,
               "lavaan reported: lavaan->lavTestLRT(): some restricted models fit better.",
               fixed = TRUE)
  expect_false(any(grepl("consistent with these data", out$notes$note, fixed = TRUE)))
  expect_false("parallel_model_test" %in% nomo_methods_used(out))

  # A difference lavaan could not compute is no test either.
  testthat::local_mocked_bindings(
    lavTestLRT = function(...) difference(NA_real_, NA_real_),
    .package = "lavaan"
  )
  out <- nomo_scores(fit, method = "sum")
  expect_false(out$parallel_test$available)
  expect_match(out$parallel_test$note, "lavaan returned none on 12 df", fixed = TRUE)
  expect_false(grepl("lavaan reported", out$parallel_test$note, fixed = TRUE))

  # Constraints that hold exactly differ from zero only by rounding error.
  testthat::local_mocked_bindings(
    lavTestLRT = function(...) difference(-3e-9, 1),
    .package = "lavaan"
  )
  out <- nomo_scores(fit, method = "sum")
  expect_true(out$parallel_test$available)
  expect_identical(out$parallel_test$chisq_diff, 0)
  expect_true(any(grepl("consistent with these data", out$notes$note, fixed = TRUE)))
})


# Matching scores to the data (#145) -------------------------------------------

test_that("`rows` says which row of the data each score belongs to", {
  dat <- scores_sample(scores_population()$sigma)
  dat$x1[c(3L, 9L)] <- NA
  dat$x7[40L] <- NA
  used <- setdiff(seq_len(nrow(dat)), c(3L, 9L, 40L))

  fit <- nomo_cfa(scores_model, data = dat)
  for (method in c("sum", "regression")) {
    out <- nomo_scores(fit, method = method)
    expect_identical(out$rows, used)
    expect_identical(length(out$rows), nrow(out$scores))
  }

  # The scores join the data by those rows, and by nothing else: by position
  # they would be assigned to the wrong cases from the first dropped row on.
  summed <- nomo_scores(fit, method = "sum")
  joined <- dat
  joined[summed$rows, names(summed$scores)] <- summed$scores
  expect_equal(joined$F1[used], rowSums(dat[used, paste0("x", 1:4)]),
               ignore_attr = TRUE)
  expect_equal(joined$F2[used], rowSums(dat[used, paste0("x", 5:8)]),
               ignore_attr = TRUE)
  expect_true(all(is.na(joined$F1[c(3L, 9L, 40L)])))
  expect_true(all(is.na(joined$F2[c(3L, 9L, 40L)])))
  # `scores` itself is unchanged: one column per factor and nothing else.
  expect_identical(names(summed$scores), c("F1", "F2"))

  # With every case used, the rows are simply all of them.
  complete <- nomo_scores(scores_fit(), method = "sum")
  expect_identical(complete$rows, seq_len(nrow(complete$scores)))
})


# Pre-RC fixes (#145) ----------------------------------------------------------

# Text as printed, with line breaks and runs of spaces read as one space.
flat_text <- function(x) gsub("[[:space:]]+", " ", paste(x, collapse = " "))


scores_hs_model <- "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9"

scores_hs_fit <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    cache <<- nomo_cfa(scores_hs_model, data = lavaan::HolzingerSwineford1939)
    cache
  }
})


test_that("ordered indicators: the notes say what lavaan computed for each method (#145)", {
  skip_on_cran()
  items <- c(paste0("a", 1:5), paste0("b", 1:5))
  fit <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
                  data = nomo_demo_ordinal, ordered = items)
  note_for <- function(method) {
    notes <- nomo_scores(fit, method = method)$notes
    notes$note[notes$topic == "estimand" & notes$severity == "review"]
  }

  # With ordered indicators lavaan's regression scores are its empirical Bayes
  # modal scores, and its Bartlett scores its maximum-likelihood scores.
  expect_equal(lavaan::lavPredict(fit$fit, method = "regression"),
               lavaan::lavPredict(fit$fit, method = "EBM"))
  expect_match(note_for("regression"),
               "lavaan computed these regression scores as empirical Bayes modal (EBM)",
               fixed = TRUE)
  bartlett <- note_for("bartlett")
  expect_match(bartlett, "as maximum-likelihood (ML) scores", fixed = TRUE)
  expect_match(bartlett, "no finite ML score", fixed = TRUE)
  # A sum adds categories; the diagnostics describe latent responses.
  expect_match(note_for("sum"), "add the observed category numbers", fixed = TRUE)
  for (method in c("regression", "bartlett", "sum")) {
    expect_match(note_for(method), "approximate the properties of these scores", fixed = TRUE)
    expect_false(grepl("treat the latent-response variables as continuous", note_for(method),
                       fixed = TRUE))
  }

  # The case with no ML score is counted, not left for the reader to find.
  bartlett_scores <- nomo_scores(fit, method = "bartlett")
  unscored <- bartlett_scores$notes$note[bartlett_scores$notes$topic == "unscored_cases"]
  expect_match(unscored, "499 of 500 on A and 499 of 500 on B", fixed = TRUE)
  expect_match(unscored, "lavaan returned no score for those cases.", fixed = TRUE)

  local_reproducible_output(width = 80)
  printed <- paste(capture.output(print(bartlett_scores)), collapse = " ")
  printed <- gsub("[[:space:]]+", " ", printed)
  expect_match(printed, "these values approximate the properties of the scores", fixed = TRUE)
  expect_match(printed, "Scored -- Cases with a score on the factor, of the 500", fixed = TRUE)
})


test_that("univocality is judged against the factor correlations, not a flat .30 (#145)", {
  skip_on_cran()
  fit <- scores_hs_fit()
  univocality_note <- function(method) {
    notes <- nomo_scores(fit, method = method)$notes
    notes$note[notes$topic == "univocality"]
  }

  # Bartlett scores, and sums of items that each load on one factor, reach the
  # other factors only through their own: the factor correlation times the
  # validity. Their univocality is above .30 here because the factors
  # correlate, and that is not a flaw.
  bartlett <- nomo_scores(fit, method = "bartlett")
  phi <- bartlett$factor_correlations
  expect_equal(bartlett$diagnostics$univocality[[2L]],
               phi["visual", "textual"] * bartlett$diagnostics$validity[[2L]],
               tolerance = 1e-6)
  expect_gt(max(abs(bartlett$diagnostics$univocality)), .30)
  expect_length(univocality_note("bartlett"), 0L)
  expect_length(univocality_note("sum"), 0L)

  # Regression scores take from the other factors directly, and the note says
  # by how much more than the factor correlation carries.
  note <- univocality_note("regression")
  expect_length(note, 1L)
  expect_match(note, "The score for visual correlates .52 with textual", fixed = TRUE)
  expect_match(note, "through visual alone it would correlate .39", fixed = TRUE)
  expect_match(note, "the factor correlation (.46) times the score's validity (.85)",
               fixed = TRUE)
  expect_match(note, "The difference, +.13, is what the score takes from textual directly",
               fixed = TRUE)

  # A cross-loaded item puts one factor's variance into another's sum.
  cross <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5 + a5",
                    data = nomo_demo_continuous)
  expect_true("univocality" %in% nomo_scores(cross, method = "sum")$notes$topic)
})


test_that("higher-order factors and structural paths are refused, each with its reason (#145)", {
  skip_on_cran()
  hs <- lavaan::HolzingerSwineford1939

  second_order <- nomo_cfa(paste(scores_hs_model, "g =~ visual + textual + speed",
                                 sep = "\n"), data = hs)
  for (method in c("sum", "regression", "bartlett")) {
    expect_error(nomo_scores(second_order, method = method),
                 "`fit` has a factor without observed indicators (g)", fixed = TRUE)
  }

  covariate <- lavaan::sem(paste(scores_hs_model, "visual ~ ageyr", sep = "\n"), data = hs)
  for (method in c("sum", "regression")) {
    expect_error(nomo_scores(covariate, method = method),
                 "`fit` contains structural paths or covariates (visual ~ ageyr)", fixed = TRUE)
  }
  observed <- lavaan::sem(paste(scores_hs_model, "x1 ~ ageyr", sep = "\n"), data = hs)
  expect_error(nomo_scores(observed), "structural paths or covariates (x1 ~ ageyr)", fixed = TRUE)
  nested <- lavaan::cfa("visual =~ x1 + x2 + x3 + textual\ntextual =~ x4 + x5 + x6",
                        data = hs)
  expect_error(nomo_scores(nested), "structural paths or covariates (visual =~ textual)",
               fixed = TRUE)

  # A covariance between a factor and an observed variable makes lavaan carry
  # the variable as a latent variable of its own, which had stopped with an
  # internal vapply() error (sum), a singular matrix (Bartlett), or a "factor"
  # row for it (regression).
  linked <- lavaan::cfa(paste(scores_hs_model, "visual ~~ ageyr", sep = "\n"), data = hs)
  expect_true("ageyr" %in% colnames(lavaan::lavInspect(linked, "est")$lambda))
  for (method in c("sum", "regression", "bartlett")) {
    expect_error(nomo_scores(linked, method = method),
                 "`fit` contains structural paths or covariates (visual ~~ ageyr)",
                 fixed = TRUE)
  }
  expect_error(nomo_scores(linked), "fit the paths and covariates on the latent variables",
               fixed = TRUE)
  two <- lavaan::cfa(paste(scores_hs_model, "ageyr ~~ visual + textual", sep = "\n"),
                     data = hs)
  expect_error(nomo_scores(two), "(visual ~~ ageyr, textual ~~ ageyr)", fixed = TRUE)
  # An item of another factor is linked the same way.
  item <- lavaan::cfa(paste(scores_hs_model, "visual ~~ x4", sep = "\n"), data = hs)
  expect_error(nomo_scores(item), "(visual ~~ x4)", fixed = TRUE)

  # A residual covariance between an item and an observed variable outside the
  # factors adds no latent variable, and the scores are lavaan's own.
  residual <- lavaan::cfa(paste(scores_hs_model, "x1 ~~ ageyr", sep = "\n"), data = hs)
  kept <- nomo_scores(residual, method = "regression")
  expect_equal(unname(as.matrix(kept$scores)),
               unname(as.matrix(lavaan::lavPredict(residual))[, kept$diagnostics$factor]),
               tolerance = 1e-10)
  expect_identical(kept$diagnostics$factor, c("visual", "textual", "speed"))
})


test_that("cases with no unit-weighted score under FIML are counted and shown (#145)", {
  skip_on_cran()
  holes <- lavaan::HolzingerSwineford1939
  set.seed(1)
  holes$x1[sample(nrow(holes), 40)] <- NA
  holes$x5[sample(nrow(holes), 30)] <- NA
  fit <- nomo_cfa(scores_hs_model, data = holes, missing = "ml")

  summed <- nomo_scores(fit, method = "sum")
  expect_identical(nrow(summed$scores), 301L)
  expect_identical(unname(colSums(is.na(summed$scores))), c(40, 30, 0))
  note <- summed$notes[summed$notes$topic == "unscored_cases", ]
  expect_identical(note$severity, "review")
  expect_match(note$note, "the cases scored are 261 of 301 on visual and 271 of 301 on textual",
               fixed = TRUE)
  expect_match(note$note, "A unit-weighted score needs every item of its factor", fixed = TRUE)

  # Regression scores use the items each case has, so all are scored.
  regression <- nomo_scores(fit, method = "regression")
  expect_identical(sum(is.na(regression$scores)), 0L)
  expect_false("unscored_cases" %in% regression$notes$topic)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(summed))
  expect_match(printed, "^  Factor +Items +Scored +Validity", all = FALSE)
  expect_match(printed, "^  visual +3 +261 ", all = FALSE)
  expect_true(any(grepl("Unscored cases (Review):", printed, fixed = TRUE)))
  # With every case scored, the column is not shown.
  expect_false(any(grepl("Scored", capture.output(print(regression)), fixed = TRUE)))
  detail <- capture.output(print(summary(summed)))
  expect_match(detail, "^  visual +3 +261 ", all = FALSE)
})


test_that("`guidance` is checked, and its safeguards hold (#145)", {
  fit <- scores_fit()
  expect_error(nomo_scores(fit, guidance = "banana"),
               "`guidance` must be a list returned by `nomo_defaults()`.", fixed = TRUE)
  unsafe <- nomo_defaults()
  unsafe$auto_delete <- TRUE
  expect_error(nomo_scores(fit, guidance = unsafe),
               "`guidance$auto_delete` cannot be `TRUE`", fixed = TRUE)
  expect_identical(nomo_scores(fit, method = "regression")$guidance, nomo_defaults())
})


test_that("an item loading against its factor is named for a unit-weighted score (#145)", {
  skip_on_cran()
  hs <- lavaan::HolzingerSwineford1939
  hs$x3 <- -hs$x3
  summed <- nomo_scores(nomo_cfa(scores_hs_model, data = hs), method = "sum")
  note <- summed$notes[summed$notes$topic == "loading_signs", ]
  expect_identical(note$severity, "concern")
  expect_match(note$note, "x3 on visual loads against the strongest item of its factor",
               fixed = TRUE)
  expect_match(note$note, "Reverse-score each such item", fixed = TRUE)
  # The stored ratio is unchanged; the summary does not show it as though the
  # loadings agreed.
  expect_equal(summed$unit_weighting$loading_ratio[[1L]], 1.82, tolerance = 0.01)
  local_reproducible_output(width = 80)
  detail <- capture.output(print(summary(summed)))
  expect_match(detail, "^  visual +-0[.]58 +0[.]77 +--$", all = FALSE)

  # A model-weighted score weights the item negatively, so there is no note.
  expect_false("loading_signs" %in%
                 nomo_scores(nomo_cfa(scores_hs_model, data = hs), "regression")$notes$topic)

  # Two such items, on two factors.
  hs$x6 <- -hs$x6
  two <- nomo_scores(nomo_cfa(scores_hs_model, data = hs), method = "sum")
  expect_match(two$notes$note[two$notes$topic == "loading_signs"],
               "x3 on visual and x6 on textual load against the strongest item of their factors",
               fixed = TRUE)
})


test_that("the reliability note points a sum score to omega, not alpha (#145)", {
  note <- nomo_scores(scores_fit(), method = "sum")$notes
  note <- note$note[note$topic == "reliability"]
  expect_match(note, "The reliability of a unit-weighted score is omega", fixed = TRUE)
  expect_match(note, "essential tau-equivalence", fixed = TRUE)
  expect_false(grepl("alpha is the reliability coefficient", note, fixed = TRUE))
})


test_that("the print explains its columns and states the parallel test once (#145)", {
  skip_on_cran()
  summed <- nomo_scores(scores_fit(), method = "sum")
  local_reproducible_output(width = 80)
  printed <- capture.output(print(summed))
  detail <- capture.output(print(summary(summed)))

  for (out in list(printed, detail)) {
    expect_identical(sum(grepl("What these columns mean", out, fixed = TRUE)), 1L)
    expect_true(any(grepl("Validity -- Correlation of the score with its own factor", out,
                          fixed = TRUE)))
    expect_true(any(grepl("Univocality -- Largest correlation of the score with another",
                          out, fixed = TRUE)))
    expect_true(any(grepl("Correlational accuracy -- Score correlation minus factor", out,
                          fixed = TRUE)))
    # The parallel-model test is printed once.
    expect_identical(sum(grepl("Delta chi-square(", out, fixed = TRUE)), 1L)
    expect_false(any(nchar(out) > 80L))
  }
  # One Flagged section, concern before review, under the unit flagged.
  flagged <- printed[seq(which(printed == "Flagged") + 1L, length(printed))]
  bullets <- grep("^  - ", flagged, value = TRUE)
  expect_match(bullets[[1L]], "^  - Correlational accuracy \\(Concern\\): ")
  expect_true(all(grepl("\\(Review\\)", bullets[-1L])))
  # print() gives each note's first sentence, summary() all of it.
  expect_false(any(grepl("Skrondal and Laake", printed, fixed = TRUE)))
  expect_true(any(grepl("Skrondal and Laake", detail, fixed = TRUE)))
  expect_match(printed[[length(printed)]], "for the score properties as a table.", fixed = TRUE)

  # A parallel model that fits is shown on its own, as the evidence it is.
  pop <- scores_population()
  L <- cbind(F1 = c(rep(.65, 4), 0, 0, 0, 0), F2 = c(0, 0, 0, 0, rep(.65, 4)))
  sigma <- L %*% pop$phi %*% t(L)
  diag(sigma) <- 1
  dimnames(sigma) <- list(scores_items, scores_items)
  parallel <- lavaan::cfa(scores_model, data = scores_sample(sigma, seed = 77), std.lv = TRUE)
  fits <- capture.output(print(nomo_scores(parallel, method = "sum")))
  expect_true(any(grepl("Parallel model (what unit weighting assumes)", fits, fixed = TRUE)))
  expect_identical(sum(grepl("Delta chi-square(", fits, fixed = TRUE)), 1L)
})


test_that("a unit flagged by two notes has one bullet in the Flagged section (#145)", {
  # The teaching data rejects the parallel model, and B's loadings differ
  # twofold: two review notes on unit weighting.
  summed <- nomo_scores(
    nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
             data = nomo_demo_continuous),
    method = "sum"
  )
  notes <- summed$notes
  expect_identical(sum(notes$topic == "unit_weighting" & notes$severity == "review"), 2L)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(summed))
  detail <- capture.output(print(summary(summed)))
  for (out in list(printed, detail)) {
    expect_identical(sum(grepl("^  - Unit weighting \\(", out)), 1L)
  }
  # Joined in the order raised: the parallel-model test, then the loading ratio.
  bullet <- function(out) {
    from <- grep("^  - Unit weighting \\(Review\\): ", out)
    ends <- c(grep("^  - ", out), which(out == ""))
    flat_text(out[from:(min(ends[ends > from]) - 1L)])
  }
  expect_true(endsWith(bullet(printed), paste(
    "fits worse than the model you fitted (Delta chi-square(16) = 171.84, p < .001).",
    "The strongest standardized loading is at least twice the weakest for B."
  )))
  expect_match(bullet(detail), "describe what it costs. The strongest standardized loading",
               fixed = TRUE)
  expect_true(endsWith(bullet(detail), "by having endorsed different items."))

  # Two statuses on one unit print once, under the more severe.
  mixed <- tibble::tibble(
    topic = c("unit_weighting", "validity", "unit_weighting"),
    severity = c("review", "review", "concern"),
    note = c("First finding. More.", "Low validity.", "Second finding. More.")
  )
  shown <- capture.output(nomologR:::nomo_scores_present_flagged(mixed))
  expect_identical(shown[3:4], c("  - Unit weighting (Concern): First finding. Second finding.",
                                 "  - Validity (Review): Low validity."))
})


test_that("a one-factor score cites only the properties it has (#145)", {
  dat <- scores_sample(scores_population()$sigma)
  fit <- lavaan::cfa(paste("F =~", paste(scores_items, collapse = " + ")), data = dat,
                     std.lv = TRUE)
  out <- nomo_scores(fit, method = "sum")
  rejection <- out$notes$note[grepl("fits worse", out$notes$note, fixed = TRUE)]
  expect_match(rejection, "that `validity` describes what it costs", fixed = TRUE)
  expect_false(grepl("correlational_accuracy", rejection, fixed = TRUE))
  local_reproducible_output(width = 80)
  printed <- capture.output(print(out))
  expect_false(any(grepl("Univocality", printed, fixed = TRUE)))
  expect_false(any(grepl("Correlational accuracy", printed, fixed = TRUE)))
})


test_that("a sentence's end is found without breaking a citation's page (#145)", {
  first <- nomologR:::nomo_scores_first_sentence
  expect_identical(first("Validity is below .90 for B. Gorsuch (1983, p. 260) recommended."),
                   "Validity is below .90 for B.")
  expect_identical(first("One sentence only."), "One sentence only.")
  bound <- nomologR:::nomo_scores_bind("Delta chi-square(2) = 3.00 and (1983, p. 260)")
  expect_false(grepl("Delta chi-square", bound, fixed = TRUE))
  expect_false(grepl("p. 2", bound, fixed = TRUE))
})


test_that("plot() draws each score's validity against Gorsuch's references (#145)", {
  out <- nomo_scores(scores_fit(), method = "sum")
  p <- plot(out)
  expect_s3_class(p, "ggplot")
  # Both validities are below .90 here, so both are marked for review.
  expect_identical(as.character(p$data$status), c("review", "review"))
  built <- ggplot2::ggplot_build(p)
  expect_equal(sort(built$data[[1L]]$xintercept), c(.80, .90))
  expect_match(flat_text(p$labels$caption), "Gorsuch's (1983) .80, and .90", fixed = TRUE)
  expect_identical(p$labels$title, "Validity of the scores (Grice, 2001)")
  expect_error(plot(out, type = "scores"), "`type` must be one of", fixed = TRUE)
  skip_on_cran()
  bartlett <- plot(nomo_scores(scores_hs_fit(), method = "bartlett"))
  expect_identical(as.character(bartlett$data$status), c("review", "none", "review"))
})
