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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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

  expect_output(print(out), "not pass/fail")
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

  structure <- tibble::tibble(factor = "F", item = c("x1", "x2", "x3", "x4"))
  expect_error(
    nomologR:::nomo_invariance_validate_partial(
      bad,
      nomologR:::nomo_invariance_sequences(),
      structure
    ),
    "not part"
  )

  checked <- nomologR:::nomo_invariance_validate_partial(
    p, nomologR:::nomo_invariance_sequences(), structure
  )
  expect_identical(checked$partial$releases$level, c("metric", "scalar"))
  expect_identical(checked$declared, c("metric", "scalar"))
  expect_identical(
    nomologR:::nomo_invariance_validate_partial(NULL, list(), structure),
    list(partial = NULL, declared = NULL)
  )
})


test_that("each release must name a parameter the levels hold equal (#145)", {
  continuous <- nomologR:::nomo_invariance_sequences()
  structure <- tibble::tibble(
    factor = c("F", "F", "F", "G", "G"),
    item = c("x1", "x2", "x3", "F", "x4")
  )
  check <- function(level, syntax, sequence = continuous) {
    nomologR:::nomo_invariance_validate_partial(
      nomo_partial(level, syntax, "Prespecified."), sequence, structure,
      hint = " A hint."
    )
  }

  # A loading, intercept, or residual variance of an indicator in the model.
  ok <- check(c("metric", "scalar", "strict", "metric"),
              c("F =~ x2", "x3 ~ 1", "x1 ~~ x1", "G =~ F"))
  expect_identical(ok$partial$releases$level, c("metric", "scalar", "strict", "metric"))

  # semTools would ignore each of these silently.
  for (syntax in c("x9 ~ 1", "f =~ x2", "F =~ x4", "F ~ 1", "x1 ~~ x2", "F ~ x1",
                   "F =~ x2\nx3 ~ 1")) {
    expect_error(check("metric", syntax), "does not name a loading", fixed = TRUE)
  }
  expect_error(check("metric", "x9 ~ 1"), "levels hold equal. A hint.", fixed = TRUE)
  expect_error(check("metric", "hello"), "is not lavaan parameter syntax", fixed = TRUE)
  expect_error(check("metric", "x1 ~ "), "is not lavaan parameter syntax", fixed = TRUE)
  expect_error(check("metric", "x1 | t1"),
               "frees a threshold, and no level of this sequence holds thresholds equal",
               fixed = TRUE)

  # Declared after the level that first holds it equal, the two models would
  # not be nested.
  expect_error(
    check("scalar", "F =~ x2"),
    "frees a loading, and loadings are first held equal at the metric level, so declare it at metric rather than scalar",
    fixed = TRUE
  )
  polytomous <- nomologR:::nomo_invariance_sequences(
    "x1", tibble::tibble(item = "x1", categories = 5L)
  )
  expect_error(check("metric", "x1 | t1", polytomous), "at the thresholds level",
               fixed = TRUE)

  # Declared before it, the release changes nothing there, so it is applied
  # from that level and the declared level is kept for the log.
  moved <- check(c("metric", "metric"), c("x3 ~ 1", "x1 ~~ x1"))
  expect_identical(moved$partial$releases$level, c("scalar", "strict"))
  expect_identical(moved$declared, c("metric", "metric"))
  binary <- nomologR:::nomo_invariance_sequences(
    "x1", tibble::tibble(item = "x1", categories = 2L)
  )
  expect_identical(check("strong", "x1 | t1", binary)$partial$releases$level, "strong")
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
  expect_output(print(summary(out)), "Fit by level")

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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  dat <- make_ordered_fixture_c(categories = 3L, seed = 6910L)

  out <- nomo_invariance(
    "F =~ u1 + u2 + u3 + u4",
    data = dat,
    group = "group",
    ordered = c("u1", "u2", "u3", "u4"),
    levels = c("configural", "metric"),
    localize = FALSE
  )

  expect_output(print(out), "Indicators:")
  expect_output(print(out), "never frees a parameter")
  expect_output(print(summary(out)), "Identification and sequence")
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
  skip_on_cran()
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
    c("Delta CFI", "Delta RMSEA", "Delta SRMR", "other")
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
  skip_on_cran()
  run <- make_m9_full_report_run()
  inv <- run$results$invariance

  shown <- inv
  shown$ordered <- c("x1")
  shown$ID.cat <- "Wu.Estabrook.2016"
  shown$parameterization <- "theta"
  shown$partial <- list(
    n = 1L,
    # Shaped as nomo_partial() returns it, release_id included.
    releases = tibble::tibble(
      release_id = "R1",
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


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("closeout: invariance sequence, level, partial, fit-row, and LRT error branches are covered", {
  expect_error(
    nomologR:::nomo_invariance_sequences("x1", NULL),
    "observed category information"
  )

  seq3 <- nomologR:::nomo_invariance_sequences(
    c("x1", "x2"),
    tibble::tibble(item = c("x1", "x2"), categories = c(3L, 4L))
  )
  expect_identical(seq3$type, "ordered_with_three_category")

  expect_error(
    nomologR:::nomo_invariance_validate_levels(
      1,
      c("configural", "metric")
    ),
    "`levels`"
  )

  row <- nomologR:::nomo_invariance_fit_row(
    level = "configural",
    constraints = character(),
    fit = NULL
  )
  expect_identical(row$constraints[[1L]], "none")

  lrt <- nomologR:::nomo_invariance_lrt(NULL, NULL)
  expect_true(is.na(lrt$chisq))

  expect_error(
    nomologR:::nomo_invariance_validate_partial(
      list(),
      c("configural", "metric")
    ),
    "object created by"
  )
})


test_that("closeout: invariance score-test handles engine errors, warnings, and missing columns", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- make_m9_full_report_run()$results$invariance$fits$metric

  testthat::local_mocked_bindings(
    lavTestScore = function(...) stop("synthetic score failure"),
    .package = "lavaan"
  )
  bad <- nomologR:::nomo_invariance_score_test(fit, "metric")
  expect_equal(nrow(bad$table), 0L)
  expect_match(bad$error, "synthetic score failure", fixed = TRUE)

  testthat::local_mocked_bindings(
    lavTestScore = function(...) list(uni = data.frame()),
    .package = "lavaan"
  )
  empty <- nomologR:::nomo_invariance_score_test(fit, "metric")
  expect_equal(nrow(empty$table), 0L)

  testthat::local_mocked_bindings(
    lavTestScore = function(...) {
      warning("synthetic score warning")
      list(uni = data.frame(foo = 1:2))
    },
    .package = "lavaan"
  )
  sparse <- nomologR:::nomo_invariance_score_test(fit, "metric")
  expect_equal(nrow(sparse$table), 2L)
  expect_true(all(is.na(sparse$table$score_x2)))
  expect_true(length(sparse$warning) >= 1L)
})


test_that("closeout: invariance decision log includes explicit missing-data configuration", {
  fit_evidence <- tibble::tibble(
    level = "configural",
    status = "estimated",
    constraints = "none",
    partial_requested = "",
    cfi = .95,
    rmsea = .05,
    srmr = .04,
    delta_cfi = NA_real_,
    delta_rmsea = NA_real_,
    delta_srmr = NA_real_,
    lrt_p = NA_real_
  )
  log <- nomologR:::nomo_invariance_decision_log(
    group = "g",
    groups = c("A", "B"),
    requested_levels = "configural",
    fit_evidence = fit_evidence,
    estimator = "MLR",
    missing = "fiml",
    ID.fac = "std.lv",
    ID.cat = "Wu.Estabrook.2016",
    parameterization = "theta",
    ordered = character(),
    category_table = tibble::tibble(),
    sequence_note = "Synthetic sequence.",
    partial = NULL,
    localize = FALSE,
    score_diagnostics = NULL
  )
  expect_true(any(log$metric == "missing"))
})


test_that("closeout: invariance public validation covers model, ordered, identification, and guidance guards", {
  dat <- lavaan::HolzingerSwineford1939
  model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "

  expect_error(
    nomo_invariance(model, dat, group = "school", ordered = 1),
    "`ordered`"
  )
  expect_error(
    nomo_invariance(model, dat, group = "school", ID.cat = ""),
    "`ID.cat`"
  )
  expect_error(
    nomo_invariance(model, dat, group = "school", parameterization = ""),
    "`parameterization`"
  )
  expect_error(
    nomo_invariance(model, dat, group = "school", guidance = 1),
    "`guidance`"
  )
  expect_error(
    nomo_invariance(
      model,
      dat,
      group = "school",
      ordered = c("x1", "x2", "x3"),
      ID.fac = "marker"
    ),
    "Wu-Estabrook"
  )

  nm <- nomo_model(list(visual = c("x1", "x2", "x3")))
  out <- nomo_invariance(
    nm,
    dat,
    group = "school",
    levels = "configural"
  )
  expect_s3_class(out, "nomo_invariance")
})


test_that("closeout: invariance pretty-constraint covers label fallbacks and unmatched labels", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- make_m9_full_report_run()$results$invariance$fits$metric

  expect_identical(
    nomologR:::nomo_invariance_pretty_constraint("a == b == c", fit),
    "a == b == c"
  )

  testthat::local_mocked_bindings(
    parTable = function(...) data.frame(),
    .package = "lavaan"
  )
  expect_identical(
    nomologR:::nomo_invariance_pretty_constraint(".p1. == .p2.", fit),
    ".p1. == .p2."
  )

  pt <- data.frame(
    lhs = c("F", "F"),
    op = c("=~", "=~"),
    rhs = c("x1", "x1"),
    group = c(1L, 2L),
    label = c(".p1.", ".p2.")
  )
  testthat::local_mocked_bindings(
    parTable = function(...) pt,
    lavInspect = function(...) c("A", "B"),
    .package = "lavaan"
  )
  same <- nomologR:::nomo_invariance_pretty_constraint(".p1. == .p2.", fit)
  expect_match(same, "Loading:", fixed = TRUE)

  expect_identical(
    nomologR:::nomo_invariance_pretty_constraint(".missing. == .p2.", fit),
    ".missing. == .p2."
  )

  pt2 <- data.frame(
    lhs = c("F", "x2"),
    op = c("=~", "~1"),
    rhs = c("x1", ""),
    group = c(1L, 2L),
    label = c(".p1.", ".p2.")
  )
  testthat::local_mocked_bindings(
    parTable = function(...) pt2,
    lavInspect = function(...) c("A", "B"),
    .package = "lavaan"
  )
  different <- nomologR:::nomo_invariance_pretty_constraint(".p1. == .p2.", fit)
  expect_match(different, "=", fixed = TRUE)
})


test_that("closeout B: invariance LRT captures warnings and missing finite comparison values", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    lavTestLRT = function(...) {
      warning("synthetic LRT warning")
      data.frame(
        "Chisq diff" = c(NA_real_, NA_real_),
        "Df diff" = c(NA_real_, NA_real_),
        "Pr(>Chisq)" = c(NA_real_, NA_real_),
        check.names = FALSE
      )
    },
    .package = "lavaan"
  )

  out <- nomologR:::nomo_invariance_lrt(list(), list())
  expect_true(is.na(out$chisq))
  expect_true(is.na(out$df))
  expect_true(is.na(out$p))
  expect_true(any(grepl(
    "synthetic LRT warning",
    out$warnings,
    fixed = TRUE
  )))
})


test_that("closeout B: invariance syntax-generation failures are wrapped by level", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    measEq.syntax = function(...) stop("synthetic syntax failure"),
    .package = "semTools"
  )

  expect_error(
    nomo_invariance(
      "
        visual =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
      ",
      lavaan::HolzingerSwineford1939,
      group = "school",
      levels = "configural",
      localize = FALSE
    ),
    "Could not generate configural invariance syntax"
  )
})


test_that("closeout B: invariance captures CFA warnings and unavailable fit-measure detail", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  original_cfa <- lavaan::cfa

  testthat::local_mocked_bindings(
    cfa = function(...) {
      warning("synthetic invariance CFA warning")
      original_cfa(...)
    },
    fitMeasures = function(...) stop("synthetic fit-measure failure"),
    .package = "lavaan"
  )

  out <- suppressWarnings(
    nomo_invariance(
      "
      visual =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ",
      lavaan::HolzingerSwineford1939,
      group = "school",
      levels = "configural",
      localize = FALSE
    )
  )

  expect_true(any(grepl(
    "synthetic invariance CFA warning",
    out$engine_warnings$configural,
    fixed = TRUE
  )))
  expect_length(out$fit_measures$configural, 0L)
})


test_that("closeout B: ordered invariance uses WLSMV and theta parameterization by default", {
  set.seed(6204)
  n <- 360
  latent <- rnorm(n)
  group <- rep(c("A", "B"), each = n / 2L)

  make_ordered <- function(lambda) {
    z <- lambda * latent + rnorm(n, sd = .75)
    ordered(
      cut(
        z,
        breaks = c(-Inf, -.60, 0, .60, Inf),
        labels = FALSE
      )
    )
  }

  dat <- data.frame(
    i1 = make_ordered(.80),
    i2 = make_ordered(.75),
    i3 = make_ordered(.70),
    group = group
  )

  out <- nomo_invariance(
    "F =~ i1 + i2 + i3",
    dat,
    group = "group",
    ordered = c("i1", "i2", "i3"),
    levels = "configural",
    localize = FALSE
  )

  expect_identical(out$estimator, "WLSMV")
  expect_identical(out$estimator_source, "ordered_default")
  expect_identical(out$parameterization, "theta")
})


test_that("closeout B: invariance pretty constraints cover label-only tables and group-free fallbacks", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- structure(list(), class = "synthetic_fit")

  same <- data.frame(
    lhs = c("F", "F"),
    op = c("=~", "=~"),
    rhs = c("x1", "x1"),
    group = c(0L, 0L),
    label = c(".p1.", ".p2."),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    parTable = function(...) same,
    lavInspect = function(...) character(),
    .package = "lavaan"
  )
  pretty_same <- nomologR:::nomo_invariance_pretty_constraint(
    ".p1. == .p2.",
    fit
  )
  expect_match(pretty_same, "Loading:", fixed = TRUE)
  expect_false(grepl("[", pretty_same, fixed = TRUE))

  no_labels <- same[, c("lhs", "op", "rhs", "group"), drop = FALSE]
  testthat::local_mocked_bindings(
    parTable = function(...) no_labels,
    .package = "lavaan"
  )
  expect_identical(
    nomologR:::nomo_invariance_pretty_constraint(
      ".p1. == .p2.",
      fit
    ),
    ".p1. == .p2."
  )

  different <- data.frame(
    lhs = c("F", "x2"),
    op = c("=~", "~1"),
    rhs = c("x1", ""),
    group = c(0L, 0L),
    label = c(".p1.", ".p2."),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    parTable = function(...) different,
    lavInspect = function(...) character(),
    .package = "lavaan"
  )
  pretty_different <- nomologR:::nomo_invariance_pretty_constraint(
    ".p1. == .p2.",
    fit
  )
  expect_match(pretty_different, "Loading:", fixed = TRUE)
  expect_match(pretty_different, "Intercept:", fixed = TRUE)
})


test_that("closeout C: invariance LRT returns NA when expected comparison columns are absent", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    lavTestLRT = function(...) {
      data.frame(
        arbitrary = c(1, 2),
        another = c(3, 4)
      )
    },
    .package = "lavaan"
  )

  out <- nomologR:::nomo_invariance_lrt(list(), list())

  expect_true(is.na(out$chisq))
  expect_true(is.na(out$df))
  expect_true(is.na(out$p))
})


test_that("closeout C: invariance records researcher estimator and missing-data choices", {
  out <- nomo_invariance(
    "
      visual =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ",
    lavaan::HolzingerSwineford1939,
    group = "school",
    levels = "configural",
    estimator = "MLR",
    missing = "listwise",
    localize = FALSE
  )

  expect_identical(out$estimator_source, "researcher")
  expect_identical(out$estimator, "MLR")
  expect_identical(out$missing, "listwise")
})


test_that("closeout C: invariance fit failures are retained as evidence instead of escaping the workflow", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "

  testthat::local_mocked_bindings(
    measEq.syntax = function(...) model,
    .package = "semTools"
  )

  testthat::local_mocked_bindings(
    cfa = function(...) stop("synthetic invariance fit failure"),
    .package = "lavaan"
  )

  out <- nomo_invariance(
    model,
    lavaan::HolzingerSwineford1939,
    group = "school",
    levels = "configural",
    localize = FALSE
  )

  expect_false(out$fit_evidence$converged[[1L]])
  expect_match(
    out$fit_evidence$error[[1L]],
    "synthetic invariance fit failure",
    fixed = TRUE
  )
  expect_length(out$fit_measures$configural, 0L)

  # The failed level stays in fit_evidence but was not completed (#145).
  expect_identical(out$completed_levels, character())
  expect_output(print(out), "Completed: none", fixed = TRUE)
  expect_output(print(summary(out)), "Levels completed: none", fixed = TRUE)
  # nomo_run()'s key evidence had ended in a dangling "completed ".
  expect_identical(
    nomologR:::nomo_run_key_evidence(list(results = list(invariance = out))),
    "Invariance: completed none"
  )
})


test_that("latent means compare the groups once intercepts are invariant (#129)", {
  skip_on_cran()
  model <- "Agency =~ ag1 + ag2 + ag3 + ag4"
  full <- nomo_invariance(model, data = nomo_demo_network, group = "group",
                          levels = c("configural", "metric", "scalar"))
  release <- nomo_partial(level = "scalar", syntax = "ag3 ~ 1",
                          rationale = "The ag3 intercept differs by mode.")
  partial <- nomo_invariance(model, data = nomo_demo_network, group = "group",
                             levels = c("configural", "metric", "scalar"),
                             partial = release)

  means <- nomo_table(partial, "latent_means")
  expect_identical(means$level, "scalar")
  expect_identical(means$group, "paper")
  expect_identical(means$reference_group, "online")
  expect_identical(means$factor, "Agency")
  # In the population, Agency is .25 SD higher on paper, and the ag3 intercept
  # .50 higher. Held equal, that intercept inflates the latent difference;
  # released, the interval covers the population value.
  expect_gt(full$latent_means$estimate, means$estimate)
  expect_lt(means$ci_lower, .25)
  expect_gt(means$ci_upper, .25)
  expect_equal(means$ci_upper - means$ci_lower, 2 * stats::qnorm(.975) * means$se,
               tolerance = 1e-6)

  log <- partial$decision_log[partial$decision_log$metric == "latent_means", ]
  expect_identical(log$severity, "info")
  expect_match(log$observation, "Agency in paper 0.", fixed = TRUE)
  expect_match(log$recommendation, "known-groups evidence", fixed = TRUE)
  expect_true("latent_mean_comparison" %in% nomo_methods(partial)$id)

  local_reproducible_output(width = 80)
  printed <- capture.output(print(summary(partial)))
  expect_match(printed, "Latent means relative to online (its latent SD)",
               fixed = TRUE, all = FALSE)
})


test_that("latent means need intercepts held equal and std.lv identification", {
  skip_on_cran()
  model <- "Agency =~ ag1 + ag2 + ag3 + ag4"
  metric <- nomo_invariance(model, data = nomo_demo_network, group = "group",
                            levels = c("configural", "metric"))
  expect_identical(nrow(metric$latent_means), 0L)
  expect_false("latent_means" %in% metric$decision_log$metric)
  expect_false("latent_mean_comparison" %in% nomo_methods(metric)$id)

  marker <- nomo_invariance(model, data = nomo_demo_network, group = "group",
                            levels = c("configural", "metric", "scalar"),
                            ID.fac = "UL")
  expect_identical(nrow(marker$latent_means), 0L)

  # Results saved before latent means still summarize and tabulate.
  old <- metric
  old$latent_means <- NULL
  local_reproducible_output(width = 80)
  expect_no_warning(capture.output(print(summary(old))))
  expect_null(nomo_table(old, "latent_means"))
})


# Audit fixes before the 1.0 freeze (#145) ------------------------------------------

test_that("indicators stored as ordered factors are modeled as ordered without `ordered` (#145)", {
  skip_on_cran()
  # Five five-category items stored as ordered factors, with a population
  # latent difference of .50 SD. lavaan fits them as categorical; recorded as
  # continuous, the scalar model had been unidentified and the difference 0.
  set.seed(11)
  n <- 400
  make_group <- function(shift) {
    f <- rnorm(n, shift)
    as.data.frame(lapply(1:5, function(i) {
      ordered(cut(.8 * f + rnorm(n, sd = .6), c(-Inf, -1, -.3, .3, 1, Inf), labels = FALSE))
    }), col.names = paste0("u", 1:5))
  }
  dat <- rbind(transform(make_group(0), grp = "A"), transform(make_group(.5), grp = "B"))

  out <- nomo_invariance("F =~ u1 + u2 + u3 + u4 + u5", dat, group = "grp", ordered = "u1",
                         levels = c("configural", "thresholds", "metric", "scalar"),
                         localize = FALSE)
  expect_identical(out$ordered, paste0("u", 1:5))
  expect_identical(out$ordered_detected, paste0("u", 2:5))
  expect_identical(out$indicator_type, "ordered_polytomous")
  expect_identical(out$estimator, "WLSMV")
  expect_identical(out$estimator_source, "ordered_default")
  expect_identical(lavaan::lavInspect(out$fits$scalar, "options")$parameterization, "theta")
  expect_identical(out$fit_evidence$df, c(10, 20, 24, 28))
  means <- out$latent_means
  expect_identical(means$level, "scalar")
  expect_true(means$ci_lower < .50 && .50 < means$ci_upper)

  row <- out$decision_log[out$decision_log$metric == "ordered_detected", ]
  expect_identical(row$severity, "review")
  expect_identical(row$stage, "invariance")
  expect_identical(row$object, "u2, u3, u4, u5")
  expect_true("categorical_invariance" %in% nomo_methods_used(out))
})


test_that("nomo_demo_ordinal is modeled as ordered without `ordered` (#145)", {
  skip_on_cran()
  dat <- nomo_demo_ordinal
  dat$half <- rep(c("first", "second"), length.out = nrow(dat))
  out <- nomo_invariance("A =~ a1 + a2 + a3 + a4", dat, group = "half",
                         levels = "configural", localize = FALSE)
  expect_identical(out$ordered_detected, paste0("a", 1:4))
  expect_identical(out$indicator_type, "ordered_polytomous")
  expect_identical(lavaan::lavInspect(out$fits$configural, "options")$parameterization, "theta")
  expect_true("ordered_detected" %in% out$decision_log$metric)

  # Declared, nothing is detected and nothing is logged.
  declared <- nomo_invariance("A =~ a1 + a2 + a3 + a4", dat, group = "half",
                              ordered = paste0("a", 1:4), levels = "configural",
                              localize = FALSE)
  expect_identical(declared$ordered_detected, character())
  expect_false("ordered_detected" %in% declared$decision_log$metric)
})


test_that("an error that detected ordered factors cause says why (#145)", {
  # These calls had run on the continuous sequence; with the indicators
  # detected as ordered they stop, and each message names them.
  set.seed(5)
  dat <- as.data.frame(lapply(1:5, function(i) ordered(sample(1:5, 120, replace = TRUE))),
                       col.names = paste0("u", 1:5))
  dat$grp <- rep(c("A", "B"), each = 60)
  model <- "F =~ u1 + u2 + u3 + u4 + u5"
  note <- paste0("u2, u3, u4, and u5 are stored as ordered factors and modeled as ordered; ",
                 "convert them to numeric to model them as continuous.")
  run <- function(...) nomo_invariance(model, dat, group = "grp", ordered = "u1", ...)

  expect_error(run(levels = c("configural", "metric", "scalar")),
               paste0("ordered prefix of configural -> thresholds -> metric -> scalar -> strict. ",
                      note), fixed = TRUE)
  expect_error(run(levels = c("configural", "strong")),
               paste0("may contain only: configural, thresholds, metric, scalar, strict. ", note),
               fixed = TRUE)
  expect_error(run(ID.fac = "UL"),
               paste0("should use `ID.fac = \"std.lv\"`. ", note), fixed = TRUE)
  expect_error(run(estimator = "MLR"),
               paste0("categorical-data estimator supported by lavaan. ", note), fixed = TRUE)
  expect_error(run(missing = "fiml"),
               paste0("for declared ordered indicators. ", note), fixed = TRUE)

  # One detected indicator reads in the singular.
  one <- dat
  for (item in c("u1", "u3", "u4", "u5")) one[[item]] <- as.integer(one[[item]])
  expect_error(
    nomo_invariance(model, one, group = "grp", ID.fac = "UL"),
    paste("u2 is stored as an ordered factor and modeled as ordered;",
          "convert it to numeric to model it as continuous."),
    fixed = TRUE
  )

  # Declared in `ordered`, nothing was detected and the message is unchanged.
  expect_error(
    nomo_invariance(model, dat, group = "grp", ordered = paste0("u", 1:5), ID.fac = "UL"),
    "should use `ID\\.fac = \"std\\.lv\"`\\.$"
  )
})


test_that("a release that frees no parameter is an error, not a silent no-op (#145)", {
  model <- "Agency =~ ag1 + ag2 + ag3 + ag4"
  release <- function(level, syntax) nomo_partial(level, syntax, "Prespecified.")
  fit <- function(...) {
    nomo_invariance(model, nomo_demo_network, group = "group",
                    levels = c("configural", "metric", "scalar"), localize = FALSE, ...)
  }

  # A missing item and a factor-name case typo had left the model fully
  # constrained while the output reported the release.
  expect_error(fit(partial = release("scalar", "ag9 ~ 1")), "Release `ag9 ~ 1` does not name",
               fixed = TRUE)
  expect_error(fit(partial = release("metric", "agency =~ ag3")),
               "Release `agency =~ ag3` does not name", fixed = TRUE)
  expect_error(fit(partial = release("metric", "ag3")), "is not lavaan parameter syntax",
               fixed = TRUE)
  expect_error(fit(partial = release("metric", "ag1 | t1")),
               "no level of this sequence holds thresholds equal", fixed = TRUE)
  expect_error(fit(partial = list(releases = data.frame())), "object created by `nomo_partial()`",
               fixed = TRUE)

  # Named correctly but fixed in the generated model: under unit loadings the
  # marker loading is 1 in every group.
  expect_error(
    fit(partial = release("metric", "Agency =~ ag1"), ID.fac = "UL"),
    "Release `Agency =~ ag1` frees no parameter at the metric level",
    fixed = TRUE
  )

  # A residual-variance release frees one parameter at the strict level.
  full <- nomo_invariance(model, nomo_demo_network, group = "group", localize = FALSE)
  freed <- nomo_invariance(model, nomo_demo_network, group = "group", localize = FALSE,
                           partial = release("strict", "ag3 ~~ ag3"))
  strict <- function(x) x$fit_evidence$df[x$fit_evidence$level == "strict"]
  expect_identical(strict(full) - strict(freed), 1)
})


test_that("a release declared at the wrong level is moved or refused (#145)", {
  model <- "Agency =~ ag1 + ag2 + ag3 + ag4"
  levels <- c("configural", "metric", "scalar")

  # A loading released only at scalar would leave scalar not nested in metric.
  expect_error(
    nomo_invariance(model, nomo_demo_network, group = "group", levels = levels,
                    partial = nomo_partial("scalar", "Agency =~ ag3", "Late.")),
    "declare it at metric rather than scalar", fixed = TRUE
  )

  # An intercept declared at metric changes nothing there; it applies from
  # scalar, and the summary and log say so.
  early <- nomo_invariance(model, nomo_demo_network, group = "group", levels = levels,
                           localize = FALSE,
                           partial = nomo_partial("metric", "ag3 ~ 1", "Early."))
  expect_identical(early$partial$releases$level, "scalar")
  expect_identical(early$fit_evidence$partial_requested, c("", "", "ag3 ~ 1"))
  expect_identical(early$fit_evidence$df, c(4, 7, 9))
  log <- early$decision_log[early$decision_log$metric == "researcher_requested_release", ]
  expect_identical(log$reference, "scalar")
  expect_match(log$observation, "at the metric level. No level holds its parameters equal before scalar",
               fixed = TRUE)
  local_reproducible_output(width = 80)
  printed <- capture.output(print(summary(early)))
  expect_match(printed, "intercepts from scalar, except ag3 ~ 1.", fixed = TRUE, all = FALSE)
  expect_match(printed, "P1 (scalar): ag3 ~ 1.", fixed = TRUE, all = FALSE)
})


test_that("latent means come from the fitted parameter table (#145)", {
  skip_on_cran()
  # A second-order model: semTools identifies it by unit loadings, so no
  # factor's reference mean is 0 with variance 1, and the group's own G mean
  # had been reported as a standardized difference.
  set.seed(3)
  n <- 300
  make_group <- function(shift) {
    g <- rnorm(n, shift)
    out <- list()
    for (k in 1:3) {
      f <- .8 * g + rnorm(n, sd = .6)
      for (j in 1:3) out[[paste0("y", k, j)]] <- .8 * f + rnorm(n, sd = .6)
    }
    as.data.frame(out)
  }
  dat <- rbind(transform(make_group(0), grp = "a"), transform(make_group(.5), grp = "b"))
  model <- paste(
    "F1 =~ y11 + y12 + y13", "F2 =~ y21 + y22 + y23", "F3 =~ y31 + y32 + y33",
    "G =~ F1 + F2 + F3", sep = "\n"
  )
  messages <- character()
  higher <- withCallingHandlers(
    nomo_invariance(model, dat, group = "grp", localize = FALSE,
                    levels = c("configural", "metric", "scalar")),
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("ID.fac set to \"ul\"", messages, fixed = TRUE)))
  expect_identical(higher$ID.fac, "ul")
  expect_identical(nrow(higher$latent_means), 0L)
  id <- higher$decision_log[higher$decision_log$metric == "factor_identification", ]
  expect_identical(id$severity, "review")
  expect_match(id$observation, "uses `ul`, not the requested `std.lv`", fixed = TRUE)
  expect_match(id$recommendation, "none are reported here", fixed = TRUE)
  expect_false(any(c("latent_means", "latent_means_fixed") %in% higher$decision_log$metric))
})


test_that("a latent mean fixed by releasing every intercept is not reported (#145)", {
  model <- "Agency =~ ag1 + ag2 + ag3 + ag4"
  all_free <- nomo_invariance(
    model, nomo_demo_network, group = "group", localize = FALSE,
    levels = c("configural", "metric", "scalar"),
    partial = nomo_partial("scalar", paste0("ag", 1:4, " ~ 1"), "Every intercept differs.")
  )
  # It had been reported as a difference of 0.00 [0.00, 0.00].
  expect_identical(nrow(all_free$latent_means), 0L)
  log <- all_free$decision_log
  expect_false("latent_means" %in% log$metric)
  fixed <- log[log$metric == "latent_means_fixed", ]
  expect_identical(fixed$severity, "review")
  expect_identical(fixed$object, "Agency")
  expect_match(fixed$observation, "No latent mean difference is estimated for Agency at the scalar level",
               fixed = TRUE)
  expect_match(fixed$observation, "fixed at 0 in every group.", fixed = TRUE)
  expect_false("latent_mean_comparison" %in% nomo_methods_used(all_free))

  # The note names each factor and level once.
  log2 <- nomologR:::nomo_invariance_fixed_means_log(tibble::tibble(
    level = c("scalar", "scalar", "strict", "strict"), factor = c("A", "B", "A", "B"),
    design = "groups"
  ))
  expect_identical(log2$object, "A, B")
  expect_match(log2$observation, "for A, B at the scalar and strict levels", fixed = TRUE)
  expect_match(log2$observation, "of their indicators released, each latent mean", fixed = TRUE)
})


test_that("ID.cat is restricted to the Wu-Estabrook identification the sequences use (#145)", {
  model <- "Agency =~ ag1 + ag2 + ag3 + ag4"
  for (id_cat in c("millsap", "Millsap.Tein.2004", "mplus", "lisrel")) {
    expect_error(
      nomo_invariance(model, nomo_demo_network, group = "group", ID.cat = id_cat),
      "`ID.cat` must be \"Wu.Estabrook.2016\" or a semTools alias", fixed = TRUE
    )
  }
  wu <- nomo_invariance(model, nomo_demo_network, group = "group", levels = "configural",
                        ID.cat = "Wu", localize = FALSE)
  expect_identical(wu$completed_levels, "configural")
})


test_that("completed_levels leaves out a level that did not converge (#145)", {
  skip_on_cran()
  # Binary items with a loading released at strong: the strong model does not
  # converge, and completed_levels had listed it anyway.
  set.seed(1)
  n <- 150
  make_group <- function() {
    f <- rnorm(n)
    as.data.frame(lapply(1:4, function(i) as.integer(.8 * f + rnorm(n, sd = .6) > 0)),
                  col.names = paste0("u", 1:4))
  }
  dat <- rbind(transform(make_group(), grp = "A"), transform(make_group(), grp = "B"))
  out <- suppressWarnings(nomo_invariance(
    "F =~ u1 + u2 + u3 + u4", dat, group = "grp", ordered = paste0("u", 1:4),
    localize = FALSE, partial = nomo_partial("strong", "F =~ u2", "Prespecified.")
  ))
  expect_identical(out$fit_evidence$level, c("configural", "strong"))
  expect_identical(out$fit_evidence$status, c("estimated", "not_converged"))
  expect_identical(out$completed_levels, "configural")
  expect_output(print(out), "Completed: configural\n", fixed = TRUE)
})
