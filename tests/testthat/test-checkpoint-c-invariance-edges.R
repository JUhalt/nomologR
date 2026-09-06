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
