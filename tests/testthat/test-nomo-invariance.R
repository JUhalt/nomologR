make_invariance_fixture <- function(n = 120L, seed = 6101L) {
  set.seed(seed)

  one_group <- function(n) {
    f <- rnorm(n)
    data.frame(
      x1 = .80 * f + rnorm(n, sd = .60),
      x2 = .78 * f + rnorm(n, sd = .62),
      x3 = .74 * f + rnorm(n, sd = .66),
      x4 = .76 * f + rnorm(n, sd = .64)
    )
  }

  rbind(
    transform(one_group(n), group = "A"),
    transform(one_group(n), group = "B")
  )
}


make_ordinal_invariance_fixture <- function(n = 180L, seed = 6701L) {
  set.seed(seed)

  one_group <- function(n) {
    f <- rnorm(n)
    latent_items <- data.frame(
      u1 = .85 * f + rnorm(n, sd = .65),
      u2 = .80 * f + rnorm(n, sd = .70),
      u3 = .78 * f + rnorm(n, sd = .72),
      u4 = .76 * f + rnorm(n, sd = .74)
    )

    as.data.frame(lapply(
      latent_items,
      function(x) {
        cut(
          x,
          breaks = c(-Inf, -1.0, -0.35, 0.35, 1.0, Inf),
          labels = FALSE
        )
      }
    ))
  }

  rbind(
    transform(one_group(n), group = "A"),
    transform(one_group(n), group = "B")
  )
}


test_that("continuous invariance retains separate configural and metric models", {
  dat <- make_invariance_fixture()
  model <- "F =~ x1 + x2 + x3 + x4"

  out <- nomo_invariance(
    model,
    data = dat,
    group = "group",
    levels = c("configural", "metric")
  )

  expect_s3_class(out, "nomo_invariance")
  expect_equal(out$indicator_type, "continuous")
  expect_equal(out$requested_levels, c("configural", "metric"))
  expect_equal(out$completed_levels, c("configural", "metric"))
  expect_true(inherits(out$syntax$configural, "measEq.syntax"))
  expect_true(inherits(out$syntax$metric, "measEq.syntax"))
  expect_true(inherits(out$fits$configural, "lavaan"))
  expect_true(inherits(out$fits$metric, "lavaan"))
  expect_true(all(out$fit_evidence$converged))

  expect_true(is.na(out$fit_evidence$delta_cfi[[1L]]))
  expect_true(is.finite(out$fit_evidence$delta_cfi[[2L]]))
})


test_that("ordered-polytomous invariance inserts threshold step before metric", {
  dat <- make_ordinal_invariance_fixture()
  model <- "F =~ u1 + u2 + u3 + u4"

  out <- nomo_invariance(
    model,
    data = dat,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "thresholds", "metric")
  )

  expect_s3_class(out, "nomo_invariance")
  expect_equal(out$indicator_type, "ordered_polytomous")
  expect_equal(
    out$requested_levels,
    c("configural", "thresholds", "metric")
  )
  expect_equal(out$estimator, "WLSMV")
  expect_equal(out$estimator_source, "ordered_default")
  expect_equal(out$ID.cat, "Wu.Estabrook.2016")
  expect_equal(out$parameterization, "theta")

  expect_true(all(out$ordered_categories$categories >= 4L))
  expect_equal(
    out$fit_evidence$constraints,
    c("none", "thresholds", "thresholds, loadings")
  )

  expect_true(inherits(out$syntax$thresholds, "measEq.syntax"))
  expect_true(inherits(out$fits$thresholds, "lavaan"))
})


test_that("ordered default sequence exposes threshold-aware model progression", {
  dat <- make_ordinal_invariance_fixture(seed = 6702L)
  model <- "F =~ u1 + u2 + u3 + u4"

  out <- nomo_invariance(
    model,
    data = dat,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "thresholds")
  )

  expect_equal(
    names(out$constraints),
    c("configural", "thresholds")
  )
  expect_match(
    paste(out$decision_log$observation, collapse = " "),
    "Threshold invariance is evaluated before loading invariance"
  )
})


test_that("ordered invariance refuses a continuous-style sequence that skips thresholds", {
  dat <- make_ordinal_invariance_fixture(seed = 6703L)
  model <- "F =~ u1 + u2 + u3 + u4"

  expect_error(
    nomo_invariance(
      model,
      data = dat,
      group = "group",
      ordered = c("u1", "u2", "u3", "u4"),
      levels = c("configural", "metric")
    ),
    "ordered prefix"
  )
})


test_that("binary and three-category structures receive identification-aware sequences", {
  binary <- make_ordinal_invariance_fixture(seed = 6704L)
  for (item in c("u1", "u2", "u3", "u4")) {
    binary[[item]] <- ifelse(binary[[item]] <= 3, 0, 1)
  }

  model <- "F =~ u1 + u2 + u3 + u4"

  out_binary <- nomo_invariance(
    model,
    data = binary,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "strong"),
    localize = FALSE
  )

  expect_equal(out_binary$indicator_type, "ordered_binary")
  expect_equal(
    out_binary$fit_evidence$constraints[[2L]],
    "thresholds, loadings, intercepts"
  )

  three <- make_ordinal_invariance_fixture(seed = 6705L)
  for (item in c("u1", "u2", "u3", "u4")) {
    three[[item]] <- ifelse(
      three[[item]] <= 2,
      1,
      ifelse(three[[item]] <= 4, 2, 3)
    )
  }

  out_three <- nomo_invariance(
    model,
    data = three,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "metric"),
    localize = FALSE
  )

  expect_equal(out_three$indicator_type, "ordered_three_category")
  expect_equal(
    out_three$fit_evidence$constraints[[2L]],
    "thresholds, loadings"
  )
})


test_that("invariance output preserves syntax and avoids pass-fail declarations", {
  dat <- make_invariance_fixture(seed = 6102L)
  model <- "F =~ x1 + x2 + x3 + x4"

  out <- nomo_invariance(
    model,
    data = dat,
    group = "group",
    levels = c("configural", "metric")
  )

  expect_true(is.character(out$syntax_text$configural))
  expect_true(nzchar(out$syntax_text$configural))
  expect_true(is.character(out$syntax_text$metric))
  expect_true(nzchar(out$syntax_text$metric))

  expect_false(any(c(
    "pass", "fail", "invariant", "verdict"
  ) %in% names(out$fit_evidence)))

  expect_output(print(out), "not universal pass/fail")
  expect_output(print(summary(out)), "No single")
})


test_that("continuous invariance validates sequential levels and grouping data", {
  dat <- make_invariance_fixture(seed = 6103L)
  model <- "F =~ x1 + x2 + x3 + x4"

  expect_error(
    nomo_invariance(
      model,
      data = dat,
      group = "group",
      levels = "metric"
    ),
    "ordered prefix"
  )

  expect_error(
    nomo_invariance(
      model,
      data = dat,
      group = "missing_group",
      levels = "configural"
    ),
    "not found"
  )

  dat_one <- dat
  dat_one$group <- "A"
  expect_error(
    nomo_invariance(
      model,
      data = dat_one,
      group = "group",
      levels = "configural"
    ),
    "at least two"
  )
})


test_that("ordered models reject ML and FIML shortcuts", {
  dat <- make_ordinal_invariance_fixture(seed = 6705L)
  model <- "F =~ u1 + u2 + u3 + u4"
  ordered <- c("u1", "u2", "u3", "u4")

  expect_error(
    nomo_invariance(
      model,
      data = dat,
      group = "group",
      ordered = ordered,
      levels = "configural",
      estimator = "MLR"
    ),
    "ML-family"
  )

  expect_error(
    nomo_invariance(
      model,
      data = dat,
      group = "group",
      ordered = ordered,
      levels = "configural",
      missing = "fiml"
    ),
    "FIML"
  )
})
