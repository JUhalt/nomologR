test_that("reliability presentation is compact and returns a ggplot", {
  model <- '
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  '
  cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
  rel <- nomo_reliability(cfa)

  printed <- capture.output(print(rel))
  expect_true(any(grepl("Primary coefficient: model-based omega", printed, fixed = TRUE)))
  expect_true(any(grepl("not pass/fail reliability rules", printed, fixed = TRUE)))

  s <- summary(rel)
  expect_s3_class(s, "summary_nomo_reliability")
  expect_true(all(c(
    "construct", "omega", "alpha", "omega_scale", "signal"
  ) %in% names(s$table)))
  expect_setequal(s$table$construct, c("F1", "F2"))

  p <- plot(rel)
  expect_s3_class(p, "ggplot")
})


test_that("ordinal reliability summary preserves unavailable observed-scale alpha", {
  set.seed(5301)
  n <- 700
  f <- rnorm(n)
  z <- data.frame(
    i1 = .82 * f + rnorm(n, sd = .60),
    i2 = .78 * f + rnorm(n, sd = .65),
    i3 = .74 * f + rnorm(n, sd = .68),
    i4 = .80 * f + rnorm(n, sd = .62)
  )
  dat <- as.data.frame(lapply(z, function(x) {
    ordered(cut(x, breaks = c(-Inf, -.7, 0, .7, Inf), labels = FALSE))
  }))
  cfa <- nomo_cfa('F =~ i1 + i2 + i3 + i4', data = dat, ordered = names(dat))
  rel <- nomo_reliability(cfa, ordinal_scale = TRUE, include_alpha = TRUE)
  s <- summary(rel)

  expect_equal(s$table$construct, "F")
  expect_equal(s$table$omega_scale, "observed_ordinal")
  expect_false(s$table$alpha_available)
  expect_true(is.na(s$table$alpha))
  expect_s3_class(plot(rel), "ggplot")
})


test_that("validity presentation separates convergent and discriminant questions", {
  set.seed(5302)
  n <- 900
  f1 <- rnorm(n)
  f2 <- .35 * f1 + sqrt(1 - .35^2) * rnorm(n)
  dat <- data.frame(
    A1 = .82 * f1 + rnorm(n, sd = .55),
    A2 = .79 * f1 + rnorm(n, sd = .58),
    A3 = .76 * f1 + rnorm(n, sd = .62),
    A4 = .80 * f1 + rnorm(n, sd = .57),
    B1 = .83 * f2 + rnorm(n, sd = .54),
    B2 = .78 * f2 + rnorm(n, sd = .60),
    B3 = .75 * f2 + rnorm(n, sd = .63),
    B4 = .81 * f2 + rnorm(n, sd = .56)
  )
  model <- '
    F1 =~ A1 + A2 + A3 + A4
    F2 =~ B1 + B2 + B3 + B4
  '
  cfa <- nomo_cfa(model, data = dat)
  val <- nomo_validity(cfa, htmt = "both")

  printed <- capture.output(print(val))
  expect_true(any(grepl("No single index", printed, fixed = TRUE)))

  s <- summary(val)
  expect_s3_class(s, "summary_nomo_validity")
  expect_true(all(c(
    "construct", "AVE", "min_abs_loading", "n_loading_review", "signal"
  ) %in% names(s$convergent)))
  expect_true(all(c(
    "construct_1", "construct_2", "latent_r",
    "latent_r_ci_lower", "latent_r_ci_upper",
    "HTMT2", "HTMT", "signal"
  ) %in% names(s$discriminant)))

  expect_s3_class(plot(val, type = "ave"), "ggplot")
  expect_s3_class(plot(val, type = "discriminant"), "ggplot")
  expect_error(plot(val, type = "loadings"), "arg")
})


test_that("weak measurement remains a review signal despite good global fit", {
  set.seed(5303)
  n <- 1000
  f <- rnorm(n)
  dat <- data.frame(
    W1 = .30 * f + rnorm(n, sd = .95),
    W2 = .34 * f + rnorm(n, sd = .94),
    W3 = .28 * f + rnorm(n, sd = .96),
    W4 = .32 * f + rnorm(n, sd = .95),
    W5 = .31 * f + rnorm(n, sd = .95)
  )
  cfa <- nomo_cfa('Weak =~ W1 + W2 + W3 + W4 + W5', data = dat)
  rel <- nomo_reliability(cfa)
  val <- nomo_validity(cfa, htmt = "none")

  rs <- summary(rel)
  vs <- summary(val)
  expect_equal(rs$table$construct, "Weak")
  expect_equal(rs$table$block, "overall")
  expect_true(is.finite(rs$table$omega))
  expect_true(is.finite(rs$table$alpha))
  expect_equal(rs$table$signal, "review")
  expect_true(rs$table$alpha_available)
  expect_equal(vs$convergent$construct, "Weak")
  expect_equal(vs$convergent$signal, "review")
})


test_that("redundant constructs are flagged in the discriminant summary without auto-merging", {
  set.seed(5304)
  n <- 1400
  f1 <- rnorm(n)
  f2 <- .94 * f1 + sqrt(1 - .94^2) * rnorm(n)
  dat <- data.frame(
    A1 = .86 * f1 + rnorm(n, sd = .48),
    A2 = .83 * f1 + rnorm(n, sd = .51),
    A3 = .84 * f1 + rnorm(n, sd = .50),
    B1 = .86 * f2 + rnorm(n, sd = .48),
    B2 = .83 * f2 + rnorm(n, sd = .51),
    B3 = .84 * f2 + rnorm(n, sd = .50)
  )
  model <- '
    F1 =~ A1 + A2 + A3
    F2 =~ B1 + B2 + B3
  '
  cfa <- nomo_cfa(model, data = dat)
  val <- nomo_validity(cfa, htmt = "both")
  s <- summary(val)

  expect_true(any(s$discriminant$signal == "review"))

  review_recommendations <- val$decision_log$recommendation[
    val$decision_log$severity %in% c("review", "concern")
  ]

  expect_true(any(grepl(
    "do not automatically merge constructs",
    review_recommendations,
    fixed = TRUE
  )))
  expect_false(any(grepl(
    "^\\s*merge\\b|\\bmust\\s+merge\\b|\\bshould\\s+merge\\b",
    review_recommendations,
    ignore.case = TRUE,
    perl = TRUE
  )))
  expect_true(all(is.na(val$decision_log$decision) | val$decision_log$decision == ""))
})

test_that("reliability plot gives omega greater visual weight without extra y-grid clutter", {
  model <- '
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  '
  cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)
  rel <- nomo_reliability(cfa)
  p <- plot(rel)

  expect_s3_class(p, "ggplot")
  expect_equal(p$coordinates$limits$x, c(0, 1))
  expect_true(length(p$layers) >= 5L)

  # Omega and alpha points are drawn in separate layers so omega can be
  # intentionally emphasized while alpha remains a secondary comparison.
  point_sizes <- vapply(
    p$layers,
    function(layer) {
      val <- layer$aes_params$size
      if (is.null(val)) NA_real_ else as.numeric(val)
    },
    numeric(1)
  )
  expect_true(any(point_sizes == 3.4, na.rm = TRUE))
  expect_true(any(point_sizes == 2.5, na.rm = TRUE))

  expect_s3_class(
    p$theme$panel.grid.minor.y,
    "element_blank"
  )
})

