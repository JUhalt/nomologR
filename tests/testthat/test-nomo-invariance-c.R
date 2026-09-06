make_invariance_fixture_c <- function(n = 150L, seed = 6901L) {
  set.seed(seed)

  one_group <- function(n, loading2 = .78, intercept2 = 0) {
    f <- rnorm(n)
    data.frame(
      x1 = .82 * f + rnorm(n, sd = .60),
      x2 = intercept2 + loading2 * f + rnorm(n, sd = .62),
      x3 = .74 * f + rnorm(n, sd = .66),
      x4 = .76 * f + rnorm(n, sd = .64)
    )
  }

  rbind(
    transform(one_group(n), group = "A"),
    transform(one_group(n), group = "B")
  )
}


make_ordered_fixture_c <- function(n = 220L, categories = 5L, seed = 6902L) {
  set.seed(seed)

  cuts_for <- function(k) {
    if (k == 2L) return(c(-Inf, 0, Inf))
    if (k == 3L) return(c(-Inf, -.45, .45, Inf))
    if (k == 5L) return(c(-Inf, -1, -.35, .35, 1, Inf))
    stop("unsupported test category count")
  }

  one_group <- function(n) {
    f <- rnorm(n)
    latent <- data.frame(
      u1 = .84 * f + rnorm(n, sd = .65),
      u2 = .80 * f + rnorm(n, sd = .70),
      u3 = .78 * f + rnorm(n, sd = .72),
      u4 = .76 * f + rnorm(n, sd = .74)
    )

    as.data.frame(lapply(
      latent,
      function(x) {
        cut(
          x,
          breaks = cuts_for(categories),
          labels = FALSE,
          include.lowest = TRUE
        )
      }
    ))
  }

  rbind(
    transform(one_group(n), group = "A"),
    transform(one_group(n), group = "B")
  )
}


test_that("binary indicators use simultaneous strong restrictions", {
  dat <- make_ordered_fixture_c(categories = 2L, seed = 6903L)

  out <- nomo_invariance(
    "F =~ u1 + u2 + u3 + u4",
    data = dat,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "strong"),
    localize = FALSE
  )

  expect_equal(out$indicator_type, "ordered_binary")
  expect_equal(out$requested_levels, c("configural", "strong"))
  expect_equal(
    out$fit_evidence$constraints[[2L]],
    "thresholds, loadings, intercepts"
  )
  expect_match(out$identification_note, "binary")
})


test_that("three-category indicators fold threshold equality into metric step", {
  dat <- make_ordered_fixture_c(categories = 3L, seed = 6904L)

  out <- nomo_invariance(
    "F =~ u1 + u2 + u3 + u4",
    data = dat,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "metric"),
    localize = FALSE
  )

  expect_equal(out$indicator_type, "ordered_three_category")
  expect_equal(out$requested_levels, c("configural", "metric"))
  expect_equal(
    out$fit_evidence$constraints[[2L]],
    "thresholds, loadings"
  )
  expect_match(out$identification_note, "three-category")
})


test_that("four-plus category indicators retain separate threshold step", {
  dat <- make_ordered_fixture_c(categories = 5L, seed = 6905L)

  out <- nomo_invariance(
    "F =~ u1 + u2 + u3 + u4",
    data = dat,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "thresholds", "metric"),
    localize = FALSE
  )

  expect_equal(out$indicator_type, "ordered_polytomous")
  expect_equal(
    out$requested_levels,
    c("configural", "thresholds", "metric")
  )
})


test_that("partial invariance is researcher-specified and cumulative", {
  dat <- make_invariance_fixture_c(seed = 6906L)

  partial <- nomo_partial(
    level = c("metric", "scalar"),
    syntax = c("F =~ x2", "x3 ~ 1"),
    rationale = c(
      "Loading release was prespecified.",
      "Intercept release was prespecified."
    )
  )

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric", "scalar"),
    partial = partial,
    localize = FALSE
  )

  expect_equal(
    out$partial_requested$requested_release,
    c("", "F =~ x2", "F =~ x2; x3 ~ 1")
  )
  expect_match(out$fit_evidence$partial_requested[[2L]], "F =~ x2")
  expect_match(
    out$fit_evidence$partial_requested[[3L]],
    "x3 ~ 1"
  )
  expect_true(any(out$decision_log$metric == "researcher_requested_release"))
})


test_that("partial release levels must belong to the applicable sequence", {
  dat <- make_ordered_fixture_c(categories = 2L, seed = 6907L)

  partial <- nomo_partial(
    level = "metric",
    syntax = "F =~ u2",
    rationale = "Not a valid level for binary sequence."
  )

  expect_error(
    nomo_invariance(
      "F =~ u1 + u2 + u3 + u4",
      data = dat,
      group = "group",
      ordered = c("u1", "u2", "u3", "u4"),
      levels = c("configural", "strong"),
      partial = partial
    ),
    "not part of this model's sequence"
  )
})


test_that("localized score diagnostics are retained but never auto-applied", {
  dat <- make_invariance_fixture_c(
    n = 180L,
    seed = 6908L
  )

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    localize = TRUE
  )

  expect_true(is.list(out$score_diagnostics))
  expect_true("metric" %in% names(out$score_diagnostics))
  expect_s3_class(out$local_strain, "tbl_df")
  expect_null(out$partial)
  expect_false(any(grepl(
    "automatic.*release",
    out$fit_evidence$partial_requested,
    ignore.case = TRUE
  )))
})


test_that("invariance report tables and plots are available", {
  dat <- make_invariance_fixture_c(seed = 6909L)

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    localize = TRUE
  )

  ftab <- nomo_table(out, "fit")
  expect_s3_class(ftab, "tbl_df")
  expect_true(all(c(
    "level", "constraints", "delta_cfi", "delta_rmsea"
  ) %in% names(ftab)))

  expect_s3_class(plot(out, type = "fit"), "ggplot")
  expect_s3_class(plot(out, type = "change"), "ggplot")

  if (nrow(out$local_strain)) {
    expect_s3_class(plot(out, type = "local_strain"), "ggplot")
  }
})


test_that("invariance print output exposes identification and researcher control", {
  dat <- make_ordered_fixture_c(categories = 3L, seed = 6910L)

  out <- nomo_invariance(
    "F =~ u1 + u2 + u3 + u4",
    data = dat,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "metric"),
    localize = FALSE
  )

  expect_output(print(out), "Indicator treatment")
  expect_output(print(out), "not automatic pass/fail")
  expect_output(print(summary(out)), "Identification/sequence note")
})
