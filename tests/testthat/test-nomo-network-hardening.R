make_network_truth_fixture <- function(n = 900L,
                                       beta = .50,
                                       weak_loading = FALSE,
                                       seed = 7101L) {
  set.seed(seed)

  A <- rnorm(n)
  B <- beta * A + rnorm(n, sd = 1)

  a3_loading <- if (isTRUE(weak_loading)) .15 else .75

  data.frame(
    a1 = .84 * A + rnorm(n, sd = .55),
    a2 = .80 * A + rnorm(n, sd = .60),
    a3 = a3_loading * A + rnorm(n, sd = .98),
    b1 = .84 * B + rnorm(n, sd = .55),
    b2 = .80 * B + rnorm(n, sd = .60),
    b3 = .75 * B + rnorm(n, sd = .65)
  )
}


network_truth_model <- function() {
  "
    A =~ a1 + a2 + a3
    B =~ b1 + b2 + b3
  "
}


test_that("known opposite-direction population is classified as inconsistent", {
  dat <- make_network_truth_fixture(
    n = 1000L,
    beta = -.55,
    seed = 7102L
  )

  out <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  row <- out$hypothesis_evidence

  expect_true(out$converged)
  expect_lt(row$estimate, 0)
  expect_lt(row$ci_upper, 0)
  expect_equal(row$concordance, "inconsistent")
})


test_that("correct direction below a prespecified magnitude remains distinct from concordance", {
  dat <- make_network_truth_fixture(
    n = 1400L,
    beta = .08,
    seed = 7103L
  )

  out <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(min = .30)
    )
  )

  row <- out$hypothesis_evidence

  expect_gt(row$estimate, 0)
  expect_lt(row$estimate, .30)
  expect_equal(
    row$concordance,
    "direction_concordant_below_magnitude"
  )
})


test_that("weak measurement propagates a review flag without rewriting theory evidence", {
  dat <- make_network_truth_fixture(
    n = 1100L,
    beta = .45,
    weak_loading = TRUE,
    seed = 7104L
  )

  out <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  expect_true(out$converged)
  expect_true(
    out$measurement_context$summary$attention %in% c("review", "concern")
  )
  expect_gte(
    out$measurement_context$summary$loading_review_flags,
    1L
  )
  expect_true(
    out$hypothesis_evidence$measurement_attention %in% c("review", "concern")
  )
  expect_match(
    out$hypothesis_evidence$interpretation,
    "Measurement context also requires review"
  )
})


test_that("primary-validation sign reversal is surfaced as a replication discrepancy", {
  primary <- make_network_truth_fixture(
    n = 900L,
    beta = .55,
    seed = 7105L
  )
  validation <- make_network_truth_fixture(
    n = 900L,
    beta = -.55,
    seed = 7106L
  )

  out <- nomo_network(
    network_truth_model(),
    data = primary,
    validation_data = validation,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  rep <- out$replication_evidence

  expect_gt(rep$primary_estimate, 0)
  expect_lt(rep$validation_estimate, 0)
  expect_equal(rep$replication_status, "sign_reversal")
  expect_match(rep$interpretation, "changes sign")
})


test_that("validation fit preserves the exact prespecified fitted model", {
  primary <- make_network_truth_fixture(
    n = 700L,
    beta = .45,
    seed = 7107L
  )
  validation <- make_network_truth_fixture(
    n = 700L,
    beta = .45,
    seed = 7108L
  )

  out <- nomo_network(
    network_truth_model(),
    data = primary,
    validation_data = validation,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  expect_identical(
    out$model_fitted,
    out$validation$model_fitted
  )
  expect_true(out$model_relations$added_from_hypothesis)
})


test_that("hardening keeps relation evidence report-ready and non-binary", {
  dat <- make_network_truth_fixture(
    n = 800L,
    beta = .45,
    seed = 7109L
  )

  out <- nomo_network(
    network_truth_model(),
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(min = .20)
    )
  )

  tab <- nomo_table(out, "hypotheses")

  expect_true(all(c(
    "estimate",
    "se",
    "ci_lower",
    "ci_upper",
    "p_value",
    "concordance",
    "measurement_attention",
    "confirmatory_status"
  ) %in% names(tab)))

  expect_false(any(c(
    "valid",
    "invalid",
    "pass",
    "fail"
  ) %in% names(tab)))
})
