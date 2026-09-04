test_that("nomo_efa recovers a simple correlated two-factor structure", {
  set.seed(3101)
  n <- 700
  f1 <- rnorm(n)
  f2 <- 0.35 * f1 + sqrt(1 - 0.35^2) * rnorm(n)

  dat <- data.frame(
    a1 = .82 * f1 + rnorm(n, sd = .55),
    a2 = .78 * f1 + rnorm(n, sd = .60),
    a3 = .75 * f1 + rnorm(n, sd = .62),
    a4 = .80 * f1 + rnorm(n, sd = .58),
    b1 = .82 * f2 + rnorm(n, sd = .55),
    b2 = .78 * f2 + rnorm(n, sd = .60),
    b3 = .75 * f2 + rnorm(n, sd = .62),
    b4 = .80 * f2 + rnorm(n, sd = .58)
  )

  out <- nomo_efa(dat, factors = 2)

  expect_s3_class(out, "nomo_efa")
  expect_equal(nrow(out$item_summary), 8)
  expect_equal(dim(out$pattern_matrix), c(8, 2))
  expect_equal(dim(out$factor_correlations), c(2, 2))

  primary <- out$item_summary$primary_factor
  expect_true(length(unique(primary[1:4])) == 1L)
  expect_true(length(unique(primary[5:8])) == 1L)
  expect_false(primary[[1L]] == primary[[5L]])

  expect_true(all(abs(out$item_summary$primary_loading) > .55))
  expect_true(all(out$item_summary$communality > .35))
})


test_that("cross-loading and weak items trigger review without deletion", {
  set.seed(3102)
  n <- 900
  f1 <- rnorm(n)
  f2 <- 0.30 * f1 + sqrt(1 - 0.30^2) * rnorm(n)

  dat <- data.frame(
    a1 = .82 * f1 + rnorm(n, sd = .55),
    a2 = .78 * f1 + rnorm(n, sd = .60),
    a3 = .75 * f1 + rnorm(n, sd = .62),
    cross = .58 * f1 + .52 * f2 + rnorm(n, sd = .55),
    b1 = .82 * f2 + rnorm(n, sd = .55),
    b2 = .78 * f2 + rnorm(n, sd = .60),
    b3 = .75 * f2 + rnorm(n, sd = .62),
    weak = .12 * f1 + .10 * f2 + rnorm(n, sd = 1.00)
  )

  out <- nomo_efa(dat, factors = 2)
  cross <- out$item_summary[out$item_summary$item == "cross", ]
  weak <- out$item_summary[out$item_summary$item == "weak", ]

  expect_true(cross$cross_loading)
  expect_true(cross$attention %in% c("REVIEW", "STRONG REVIEW"))
  expect_true(weak$weak_primary || weak$low_communality)
  expect_true(weak$attention %in% c("REVIEW", "STRONG REVIEW"))
  expect_equal(out$items, names(dat))
  expect_equal(nrow(out$item_summary), ncol(dat))
})


test_that("ordinal items use polychoric EFA when declared ordinal", {
  set.seed(3103)
  n <- 550
  f1 <- rnorm(n)
  f2 <- 0.40 * f1 + sqrt(1 - 0.40^2) * rnorm(n)
  z <- data.frame(
    a1 = .80 * f1 + rnorm(n, sd = .65),
    a2 = .75 * f1 + rnorm(n, sd = .68),
    a3 = .78 * f1 + rnorm(n, sd = .66),
    a4 = .72 * f1 + rnorm(n, sd = .70),
    b1 = .80 * f2 + rnorm(n, sd = .65),
    b2 = .75 * f2 + rnorm(n, sd = .68),
    b3 = .78 * f2 + rnorm(n, sd = .66),
    b4 = .72 * f2 + rnorm(n, sd = .70)
  )
  dat <- as.data.frame(lapply(
    z,
    function(x) as.integer(cut(
      x,
      breaks = c(-Inf, -1, -.3, .3, 1, Inf),
      labels = FALSE
    ))
  ))
  types <- stats::setNames(rep("ordinal", ncol(dat)), names(dat))

  out <- nomo_efa(dat, factors = 2, types = types)

  expect_equal(out$correlation, "polychoric")
  expect_true(all(out$modeling_types$model_type == "ordinal"))
  expect_equal(nrow(out$item_summary), 8)
})


test_that("nomo_factors objects hand factor decisions into EFA explicitly", {
  set.seed(3104)
  dat <- as.data.frame(matrix(rnorm(400), ncol = 4))
  names(dat) <- paste0("i", 1:4)

  fake <- list(
    parallel = list(n_factors = 1L),
    items = names(dat),
    correlation = "pearson",
    missing = "pairwise",
    modeling_types = tibble::tibble(
      item = names(dat),
      screen_type = "numeric_continuous",
      model_type = "continuous",
      source = "inferred_from_storage"
    )
  )
  class(fake) <- c("nomo_factors", "list")

  out <- nomo_efa(dat, factors = fake)

  expect_equal(out$n_factors, 1L)
  expect_equal(out$factor_source, "nomo_factors")
  expect_equal(out$correlation, "pearson")
})


test_that("EFA rejects invalid factor counts and nonconstant items", {
  dat <- data.frame(
    a = rnorm(100),
    b = rnorm(100),
    c = rnorm(100),
    d = rnorm(100)
  )

  expect_error(nomo_efa(dat, factors = 0), "positive integer")
  expect_error(nomo_efa(dat, factors = 4), "smaller than")
  dat$c <- 1
  expect_error(nomo_efa(dat, factors = 1), "nonconstant")
})


test_that("presentation methods return stable user-facing objects", {
  set.seed(3105)
  f <- rnorm(300)
  dat <- data.frame(
    i1 = .8 * f + rnorm(300, sd = .6),
    i2 = .8 * f + rnorm(300, sd = .6),
    i3 = .7 * f + rnorm(300, sd = .7),
    i4 = .7 * f + rnorm(300, sd = .7)
  )
  out <- nomo_efa(dat, factors = 1)

  expect_s3_class(summary(out), "summary_nomo_efa")
  expect_s3_class(plot(out, type = "pattern"), "ggplot")
  expect_s3_class(plot(out, type = "items"), "ggplot")
  expect_s3_class(plot(out, type = "residuals"), "ggplot")
  expect_s3_class(plot(out, type = "factor_correlations"), "ggplot")

  expect_output(print(out), "No items were automatically deleted")
  expect_output(print(summary(out)), "Item-level structural review")
})
