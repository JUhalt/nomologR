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

# ---- recovered from hygiene consolidation: test-nomo-invariance.R ----
# ---- consolidated from test-checkpoint-c-invariance-edges.R ----
make_checkpoint_inv_data <- function(n = 130L, seed = 7401L) {
  set.seed(seed)

  one_group <- function(n) {
    f <- rnorm(n)
    data.frame(
      x1 = .82 * f + rnorm(n, sd = .60),
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


test_that("invariance sequence helper covers continuous and ordered structures", {
  continuous <- nomologR:::nomo_invariance_sequences()
  expect_equal(
    continuous$sequence,
    c("configural", "metric", "scalar", "strict")
  )

  category2 <- tibble::tibble(
    item = c("u1", "u2"),
    categories = c(2L, 2L)
  )
  binary <- nomologR:::nomo_invariance_sequences(
    ordered = category2$item,
    category_table = category2
  )
  expect_equal(
    binary$sequence,
    c("configural", "strong", "strict")
  )

  category3 <- tibble::tibble(
    item = c("u1", "u2"),
    categories = c(3L, 3L)
  )
  three <- nomologR:::nomo_invariance_sequences(
    ordered = category3$item,
    category_table = category3
  )
  expect_equal(
    three$sequence,
    c("configural", "metric", "scalar", "strict")
  )

  category5 <- tibble::tibble(
    item = c("u1", "u2"),
    categories = c(5L, 5L)
  )
  poly <- nomologR:::nomo_invariance_sequences(
    ordered = category5$item,
    category_table = category5
  )
  expect_equal(
    poly$sequence,
    c("configural", "thresholds", "metric", "scalar", "strict")
  )

  mixed <- tibble::tibble(
    item = c("u1", "u2"),
    categories = c(2L, 5L)
  )
  mixed_out <- nomologR:::nomo_invariance_sequences(
    ordered = mixed$item,
    category_table = mixed
  )
  expect_equal(mixed_out$type, "ordered_with_binary")
})


test_that("invariance level validator exercises valid and invalid prefixes", {
  sequence <- c("configural", "metric", "scalar", "strict")

  expect_equal(
    nomologR:::nomo_invariance_validate_levels(NULL, sequence),
    sequence
  )
  expect_equal(
    nomologR:::nomo_invariance_validate_levels(
      c("configural", "metric"),
      sequence
    ),
    c("configural", "metric")
  )

  expect_error(
    nomologR:::nomo_invariance_validate_levels(
      "metric",
      sequence
    ),
    "ordered prefix"
  )
  expect_error(
    nomologR:::nomo_invariance_validate_levels(
      c("configural", "banana"),
      sequence
    ),
    "may contain only"
  )
})


test_that("ordered-category helper catches unusable items", {
  dat <- data.frame(
    u1 = c(1, 1, 1, 1),
    u2 = c(1, 2, 1, 2)
  )

  tab <- nomologR:::nomo_invariance_ordered_categories(
    dat,
    c("u1", "u2")
  )

  expect_equal(tab$categories, c(1L, 2L))

  expect_error(
    nomologR:::nomo_invariance_validate_ordered_categories(tab),
    "fewer than two"
  )
})


test_that("null fit row and configural score test are represented explicitly", {
  row <- nomologR:::nomo_invariance_fit_row(
    level = "metric",
    constraints = "loadings",
    fit = NULL,
    error = "synthetic failure",
    partial_requested = "F =~ x2"
  )

  expect_equal(row$status, "fit_error")
  expect_false(row$converged)
  expect_equal(row$error, "synthetic failure")
  expect_match(row$partial_requested, "F =~ x2", fixed = TRUE)

  score <- nomologR:::nomo_invariance_score_test(
    fit = NULL,
    level = "configural"
  )

  expect_equal(nrow(score$table), 0L)
  expect_null(score$raw)
})


test_that("partial helper carries releases forward and validates sequences", {
  p <- nomo_partial(
    level = c("metric", "scalar"),
    syntax = c("F =~ x2", "x3 ~ 1"),
    rationale = c("metric reason", "scalar reason")
  )

  sequence <- c("configural", "metric", "scalar", "strict")

  expect_equal(
    nomologR:::nomo_invariance_partial_for_level(
      p,
      "configural",
      sequence
    ),
    character()
  )
  expect_equal(
    nomologR:::nomo_invariance_partial_for_level(
      p,
      "metric",
      sequence
    ),
    "F =~ x2"
  )
  expect_equal(
    nomologR:::nomo_invariance_partial_for_level(
      p,
      "scalar",
      sequence
    ),
    c("F =~ x2", "x3 ~ 1")
  )

  cum <- nomologR:::nomo_invariance_partial_cumulative(
    p,
    sequence[1:3],
    sequence
  )

  expect_equal(
    cum$requested_release,
    c("", "F =~ x2", "F =~ x2; x3 ~ 1")
  )

  bad <- nomo_partial(
    level = "strong",
    syntax = "F =~ x2",
    rationale = "binary-only release"
  )

  expect_error(
    nomologR:::nomo_invariance_validate_partial(
      bad,
      sequence
    ),
    "not part"
  )
})


test_that("invariance main argument validation covers edge paths", {
  dat <- make_checkpoint_inv_data()
  model <- "F =~ x1 + x2 + x3 + x4"

  expect_error(
    nomo_invariance("", dat, "group"),
    "non-empty"
  )
  expect_error(
    nomo_invariance(model, list(), "group"),
    "non-empty data frame"
  )
  expect_error(
    nomo_invariance(model, dat, ""),
    "non-empty"
  )
  expect_error(
    nomo_invariance(model, dat, "missing_group"),
    "not found"
  )

  dat_na <- dat
  dat_na$group[[1L]] <- NA_character_
  expect_error(
    nomo_invariance(model, dat_na, "group"),
    "missing values"
  )

  one <- dat
  one$group <- "A"
  expect_error(
    nomo_invariance(model, one, "group"),
    "at least two"
  )

  expect_error(
    nomo_invariance(
      model,
      dat,
      "group",
      ordered = "missing_item"
    ),
    "not found"
  )
  expect_error(
    nomo_invariance(
      model,
      dat,
      "group",
      localize = NA
    ),
    "TRUE or FALSE"
  )
  expect_error(
    nomo_invariance(
      model,
      dat,
      "group",
      estimator = ""
    ),
    "non-empty"
  )
  expect_error(
    nomo_invariance(
      model,
      dat,
      "group",
      missing = ""
    ),
    "non-empty"
  )
  expect_error(
    nomo_invariance(
      model,
      dat,
      "group",
      ID.fac = ""
    ),
    "non-empty"
  )
})


test_that("invariance table method covers all table branches", {
  dat <- make_checkpoint_inv_data(seed = 7402L)

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    localize = TRUE
  )

  for (type in c(
    "fit",
    "categories",
    "partial",
    "local_strain",
    "decision_log"
  )) {
    expect_s3_class(nomo_table(out, type), "data.frame")
  }

  expect_equal(nrow(nomo_table(out, "partial")), 0L)
})


test_that("partial table method returns researcher release provenance", {
  dat <- make_checkpoint_inv_data(seed = 7403L)

  p <- nomo_partial(
    level = "metric",
    syntax = "F =~ x2",
    rationale = "coverage hardening release"
  )

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    partial = p,
    localize = FALSE
  )

  tab <- nomo_table(out, "partial")
  expect_equal(nrow(tab), 1L)
  expect_equal(tab$syntax, "F =~ x2")
})


test_that("invariance presentation covers available plot and summary branches", {
  dat <- make_checkpoint_inv_data(seed = 7404L)

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    localize = TRUE
  )

  expect_s3_class(plot(out, type = "fit"), "ggplot")
  expect_s3_class(plot(out, type = "change"), "ggplot")
  expect_output(print(summary(out)), "Fit and change evidence")

  if (nrow(out$local_strain)) {
    expect_s3_class(plot(out, type = "local_strain"), "ggplot")
  }
})

# ---- consolidated from test-nomo-invariance-c.R ----
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

# ---- consolidated from test-nomo-invariance-hardening.R ----
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

# ---- consolidated from test-coverage-sprint-invariance.R ----
# Pre-v0.1 coverage sprint: invariance presentation branches ------------------

test_that("invariance label helpers cover metric and parameter types", {
  expect_equal(
    nomologR:::nomo_invariance_metric_label(
      c("delta_cfi", "delta_rmsea", "delta_srmr", "other")
    ),
    c("\u0394CFI", "\u0394RMSEA", "\u0394SRMR", "other")
  )

  pt <- data.frame(
    lhs = c("F", "x1", "x2", "x3", "x4", "x5"),
    op = c("=~", "~1", "|", "~~", "~~", "~"),
    rhs = c("x1", "", "t1", "x3", "x6", "1"),
    group = c(1, 2, 3, 1, 1, 0)
  )

  labs <- lapply(seq_len(nrow(pt)), function(i) {
    nomologR:::nomo_invariance_parameter_label(pt, i, c("A", "B"))
  })

  expect_match(labs[[1L]]$base, "Loading:", fixed = TRUE)
  expect_match(labs[[2L]]$base, "Intercept:", fixed = TRUE)
  expect_match(labs[[3L]]$base, "Threshold:", fixed = TRUE)
  expect_equal(labs[[3L]]$group, "Group 3")
  expect_match(labs[[4L]]$base, "Residual variance:", fixed = TRUE)
  expect_match(labs[[5L]]$base, "Covariance:", fixed = TRUE)
  expect_equal(labs[[6L]]$group, "")
})


test_that("invariance pretty-constraint helper preserves invalid inputs", {
  expect_null(nomologR:::nomo_invariance_pretty_constraint(NULL, NULL))
  expect_equal(nomologR:::nomo_invariance_pretty_constraint("", NULL), "")
  expect_equal(
    nomologR:::nomo_invariance_pretty_constraint("a == b == c", NULL),
    "a == b == c"
  )
})


test_that("invariance print and summary cover ordered and partial presentation", {
  run <- make_m9_full_report_run()
  inv <- run$results$invariance

  shown <- inv
  shown$ordered <- c("x1")
  shown$ID.cat <- "Wu.Estabrook.2016"
  shown$parameterization <- "theta"
  shown$partial <- list(
    n = 1L,
    releases = tibble::tibble(
      level = "metric",
      syntax = "WellBeing =~ w2",
      rationale = "Synthetic researcher-specified release."
    )
  )

  txt <- paste(capture.output(print(shown)), collapse = "\n")
  expect_match(txt, "Ordered identification", fixed = TRUE)
  expect_match(txt, "Researcher-specified partial releases", fixed = TRUE)

  s <- summary(shown)
  s$ordered_categories <- tibble::tibble(
    item = "x1",
    categories = 4L
  )
  s$partial <- shown$partial

  txt2 <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(txt2, "Observed ordered categories", fixed = TRUE)
  expect_match(txt2, "Researcher-specified partial invariance", fixed = TRUE)
})


test_that("invariance plots cover fit, change, local-strain, and empty errors", {
  run <- make_m9_full_report_run()
  inv <- run$results$invariance

  p1 <- plot(inv, type = "fit")
  expect_s3_class(p1, "ggplot")

  p2 <- plot(inv, type = "change")
  expect_s3_class(p2, "ggplot")

  no_fit <- inv
  no_fit$fit_evidence$cfi[] <- NA_real_
  no_fit$fit_evidence$rmsea[] <- NA_real_
  no_fit$fit_evidence$srmr[] <- NA_real_
  expect_error(plot(no_fit, type = "fit"), "No finite invariance fit evidence")

  no_change <- inv
  no_change$fit_evidence$delta_cfi[] <- NA_real_
  no_change$fit_evidence$delta_rmsea[] <- NA_real_
  no_change$fit_evidence$delta_srmr[] <- NA_real_
  expect_error(
    plot(no_change, type = "change"),
    "No finite change-in-fit evidence"
  )

  if (nrow(inv$local_strain)) {
    p3 <- plot(inv, type = "local_strain")
    expect_s3_class(p3, "ggplot")
  }

  no_local <- inv
  no_local$local_strain <- no_local$local_strain[0, , drop = FALSE]
  expect_error(
    plot(no_local, type = "local_strain"),
    "No equality-constraint score diagnostics"
  )
})


test_that("invariance local-strain display handles empty diagnostics", {
  run <- make_m9_full_report_run()
  inv <- run$results$invariance

  empty <- inv
  empty$local_strain <- empty$local_strain[0, , drop = FALSE]
  out <- nomologR:::nomo_invariance_local_strain_display(empty)
  expect_equal(nrow(out), 0L)
})
