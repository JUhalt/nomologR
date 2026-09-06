make_continuous_noninvariance_fixture <- function(n = 450L,
                                                  loading_b = .30,
                                                  intercept_b = 0,
                                                  seed = 7201L) {
  set.seed(seed)

  one_group <- function(n, loading2, intercept3) {
    f <- rnorm(n)

    data.frame(
      x1 = .85 * f + rnorm(n, sd = .55),
      x2 = loading2 * f + rnorm(n, sd = .60),
      x3 = intercept3 + .80 * f + rnorm(n, sd = .60),
      x4 = .78 * f + rnorm(n, sd = .62)
    )
  }

  rbind(
    transform(
      one_group(
        n,
        loading2 = .85,
        intercept3 = 0
      ),
      group = "A"
    ),
    transform(
      one_group(
        n,
        loading2 = loading_b,
        intercept3 = intercept_b
      ),
      group = "B"
    )
  )
}


test_that("known loading noninvariance produces worse metric fit and localized strain", {
  dat <- make_continuous_noninvariance_fixture(
    n = 500L,
    loading_b = .25,
    seed = 7202L
  )

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    localize = TRUE
  )

  metric <- out$fit_evidence[
    out$fit_evidence$level == "metric",
    ,
    drop = FALSE
  ]

  expect_true(all(out$fit_evidence$converged))
  expect_true(is.finite(metric$delta_cfi))
  expect_lt(metric$delta_cfi, 0)

  if (is.finite(metric$lrt_p)) {
    expect_lt(metric$lrt_p, .05)
  }

  expect_s3_class(out$local_strain, "tbl_df")
  expect_gt(nrow(out$local_strain), 0L)
  expect_true(all(out$local_strain$diagnostic_only))
})


test_that("known intercept noninvariance is exposed when scalar constraints are added", {
  dat <- make_continuous_noninvariance_fixture(
    n = 500L,
    loading_b = .85,
    intercept_b = 1.00,
    seed = 7203L
  )

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric", "scalar"),
    localize = FALSE
  )

  scalar <- out$fit_evidence[
    out$fit_evidence$level == "scalar",
    ,
    drop = FALSE
  ]

  expect_true(all(out$fit_evidence$converged))
  expect_true(is.finite(scalar$delta_cfi))
  expect_lt(scalar$delta_cfi, 0)

  if (is.finite(scalar$lrt_p)) {
    expect_lt(scalar$lrt_p, .05)
  }
})


test_that("researcher-specified partial release improves a known loading-mismatch model", {
  dat <- make_continuous_noninvariance_fixture(
    n = 500L,
    loading_b = .25,
    seed = 7204L
  )

  full <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    localize = FALSE
  )

  partial_spec <- nomo_partial(
    level = "metric",
    syntax = "F =~ x2",
    rationale = paste(
      "Hardening simulation deliberately generated loading noninvariance",
      "for x2."
    )
  )

  partial <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    partial = partial_spec,
    localize = FALSE
  )

  full_metric <- full$fit_evidence[
    full$fit_evidence$level == "metric",
    ,
    drop = FALSE
  ]
  partial_metric <- partial$fit_evidence[
    partial$fit_evidence$level == "metric",
    ,
    drop = FALSE
  ]

  expect_true(is.finite(full_metric$cfi))
  expect_true(is.finite(partial_metric$cfi))
  expect_gt(partial_metric$cfi, full_metric$cfi)

  expect_match(
    partial_metric$partial_requested,
    "F =~ x2",
    fixed = TRUE
  )
  expect_true(any(
    partial$decision_log$metric == "researcher_requested_release"
  ))
})


test_that("localized diagnostics never create partial invariance by themselves", {
  dat <- make_continuous_noninvariance_fixture(
    n = 450L,
    loading_b = .30,
    seed = 7205L
  )

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    localize = TRUE
  )

  expect_null(out$partial)
  expect_true(
    all(out$fit_evidence$partial_requested == "")
  )
  expect_true(any(
    out$decision_log$metric == "score_diagnostics"
  ))
})


test_that("hardening retains exact category-aware identification notes", {
  set.seed(7206L)
  n <- 260L

  make_binary <- function(n) {
    f <- rnorm(n)
    latent <- data.frame(
      u1 = .85 * f + rnorm(n, sd = .65),
      u2 = .82 * f + rnorm(n, sd = .68),
      u3 = .80 * f + rnorm(n, sd = .70),
      u4 = .78 * f + rnorm(n, sd = .72)
    )

    as.data.frame(lapply(
      latent,
      function(x) as.integer(x > 0)
    ))
  }

  dat <- rbind(
    transform(make_binary(n), group = "A"),
    transform(make_binary(n), group = "B")
  )

  out <- nomo_invariance(
    "F =~ u1 + u2 + u3 + u4",
    data = dat,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "strong"),
    localize = FALSE
  )

  expect_equal(out$indicator_type, "ordered_binary")
  expect_match(
    out$identification_note,
    "threshold, loading, and intercept restrictions"
  )
  expect_equal(
    out$fit_evidence$constraints[[2L]],
    "thresholds, loadings, intercepts"
  )
})
