test_that("nomo_factors validates core inputs", {
  dat <- data.frame(
    a = 1:10,
    b = 2:11,
    c = 3:12
  )

  expect_error(nomo_factors(matrix(1:9, 3, 3)), "data frame")
  expect_error(nomo_factors(dat, items = c("a", "nope", "c")), "Unknown")
  expect_error(nomo_factors(dat, items = c("a", "b")), "at least three")
  expect_error(nomo_factors(dat, n_iter = 5), "at least 10")
  expect_error(nomo_factors(dat, quantile = 1), "strictly between")
  expect_error(
    nomo_factors(dat, types = c(nope = "ordinal")),
    "not selected"
  )
})


test_that("numeric-discrete items remain conservative unless overridden", {
  set.seed(1)
  f <- rnorm(250)
  latent <- replicate(5, 0.8 * f + rnorm(250, sd = 0.6))
  likert <- as.data.frame(
    apply(
      latent,
      2,
      function(x) as.integer(cut(
        x,
        breaks = c(-Inf, -0.8, -0.2, 0.2, 0.8, Inf),
        labels = FALSE
      ))
    )
  )
  names(likert) <- paste0("i", 1:5)

  pearson <- nomo_factors(likert, n_iter = 10, seed = 10)
  expect_identical(pearson$correlation_method, "pearson")
  expect_true(
    any(pearson$decision_log$metric == "numeric_discrete_assumption")
  )

  ordinal_types <- stats::setNames(
    rep("ordinal", ncol(likert)),
    names(likert)
  )
  poly <- nomo_factors(
    likert,
    types = ordinal_types,
    n_iter = 10,
    seed = 10
  )
  expect_identical(poly$correlation_method, "polychoric")
  expect_true(all(poly$item_types$model_type == "ordinal"))
})


test_that("auto correlation selection respects declared indicator type", {
  set.seed(2)
  n <- 250
  f <- rnorm(n)

  binary <- data.frame(
    a = 0.8 * f + rnorm(n) > 0,
    b = 0.8 * f + rnorm(n) > 0,
    c = 0.8 * f + rnorm(n) > 0,
    d = 0.8 * f + rnorm(n) > 0
  )
  b <- nomo_factors(binary, n_iter = 10, seed = 20)
  expect_identical(b$correlation_method, "tetrachoric")

  ordinal <- data.frame(
    a = ordered(cut(0.8 * f + rnorm(n), 5, labels = FALSE)),
    b = ordered(cut(0.8 * f + rnorm(n), 5, labels = FALSE)),
    c = ordered(cut(0.8 * f + rnorm(n), 5, labels = FALSE)),
    d = ordered(cut(0.8 * f + rnorm(n), 5, labels = FALSE))
  )
  o <- nomo_factors(ordinal, n_iter = 10, seed = 20)
  expect_identical(o$correlation_method, "polychoric")

  mixed <- ordinal
  mixed$a <- as.numeric(scale(f + rnorm(n)))
  m <- nomo_factors(mixed, n_iter = 10, seed = 20)
  expect_identical(m$correlation_method, "mixed")
})


test_that("parallel analysis recovers a strong one-factor population", {
  set.seed(101)
  n <- 500
  f <- rnorm(n)
  dat <- as.data.frame(
    replicate(6, 0.85 * f + rnorm(n, sd = 0.45))
  )
  names(dat) <- paste0("i", 1:6)

  out <- nomo_factors(dat, n_iter = 20, seed = 1001)

  expect_s3_class(out, "nomo_factors")
  expect_equal(out$parallel$n_factors, 1L)
  expect_true(out$parallel$table$retained[[1L]])
  expect_false(any(out$parallel$table$retained[-1L]))
  expect_true(out$map$n_factors %in% 1:2)
  expect_equal(nrow(out$parallel$table), 6)
  expect_true(out$kmo$available)
  expect_true(out$bartlett$available)
  expect_true(out$kmo$overall > 0.5)
})


test_that("parallel analysis recovers a strong correlated two-factor population", {
  set.seed(102)
  n <- 650
  z1 <- rnorm(n)
  z2 <- rnorm(n)
  f1 <- z1
  f2 <- 0.35 * z1 + sqrt(1 - 0.35^2) * z2

  dat <- data.frame(
    a1 = 0.85 * f1 + rnorm(n, sd = 0.45),
    a2 = 0.85 * f1 + rnorm(n, sd = 0.45),
    a3 = 0.85 * f1 + rnorm(n, sd = 0.45),
    b1 = 0.85 * f2 + rnorm(n, sd = 0.45),
    b2 = 0.85 * f2 + rnorm(n, sd = 0.45),
    b3 = 0.85 * f2 + rnorm(n, sd = 0.45)
  )

  out <- nomo_factors(dat, n_iter = 20, seed = 1002)

  expect_equal(out$parallel$n_factors, 2L)
  expect_true(out$map$n_factors %in% 1:3)
  expect_true(2L %in% out$plausible_factors)
})


test_that("seed makes null reference reproducible without changing caller RNG", {
  set.seed(103)
  f <- rnorm(250)
  dat <- as.data.frame(
    replicate(5, 0.8 * f + rnorm(250, sd = 0.6))
  )

  set.seed(777)
  before <- .Random.seed
  a <- nomo_factors(dat, n_iter = 10, seed = 44)
  after <- .Random.seed

  b <- nomo_factors(dat, n_iter = 10, seed = 44)

  expect_identical(before, after)
  expect_equal(
    a$parallel$random_eigenvalues,
    b$parallel$random_eigenvalues
  )
})


test_that("pairwise missingness skips Bartlett rather than inventing one N", {
  set.seed(104)
  f <- rnorm(220)
  dat <- as.data.frame(
    replicate(5, 0.8 * f + rnorm(220, sd = 0.6))
  )
  dat[1:20, 1] <- NA_real_
  dat[21:35, 2] <- NA_real_

  pairwise <- nomo_factors(dat, n_iter = 10, seed = 51)
  expect_false(pairwise$bartlett$available)
  expect_true(any(pairwise$decision_log$metric == "bartlett"))

  complete <- nomo_factors(
    dat,
    missing = "complete",
    n_iter = 10,
    seed = 51
  )
  expect_true(complete$bartlett$available)
  expect_equal(complete$n_cases, sum(stats::complete.cases(dat)))
})


test_that("non-positive-definite matrices are never silently smoothed", {
  set.seed(105)
  x <- rnorm(180)
  dat <- data.frame(
    a = x,
    b = x,
    c = x + rnorm(180, sd = 0.01),
    d = rnorm(180)
  )

  expect_error(
    nomo_factors(dat, n_iter = 10, seed = 60),
    "not positive definite"
  )

  expect_silent(
    out <- nomo_factors(
      dat,
      n_iter = 10,
      seed = 60,
      smooth = TRUE
    )
  )
  expect_true(out$smoothed)
  expect_true(any(out$decision_log$metric == "correlation_smoothing"))
})


test_that("summary and plot methods expose retention evidence", {
  set.seed(106)
  f <- rnorm(220)
  dat <- as.data.frame(
    replicate(5, 0.8 * f + rnorm(220, sd = 0.6))
  )
  names(dat) <- paste0("i", 1:5)

  out <- nomo_factors(dat, n_iter = 10, seed = 70)
  s <- summary(out)

  expect_s3_class(s, "summary_nomo_factors")
  expect_equal(nrow(s$evidence), 2)
  expect_match(s$recommendation, "factor")

  expect_s3_class(plot(out), "ggplot")
  expect_s3_class(plot(out, type = "scree"), "ggplot")
  expect_s3_class(plot(out, type = "evidence"), "ggplot")
  expect_s3_class(plot(out, type = "kmo"), "ggplot")
})


test_that("nomo_factors does not modify supplied data", {
  set.seed(107)
  f <- rnorm(160)
  dat <- as.data.frame(
    replicate(4, 0.8 * f + rnorm(160, sd = 0.6))
  )
  original <- dat

  invisible(nomo_factors(dat, n_iter = 10, seed = 80))
  expect_identical(dat, original)
})


test_that("original MAP includes the zero-component candidate", {
  r <- matrix(
    c(
      1.00, 0.02, 0.01, 0.00,
      0.02, 1.00, 0.01, 0.00,
      0.01, 0.01, 1.00, 0.02,
      0.00, 0.00, 0.02, 1.00
    ),
    nrow = 4,
    byrow = TRUE
  )

  out <- nomo_factors_map(r, max_factors = 3)

  expect_equal(out$table$n_factors[[1L]], 0L)
  expect_equal(
    out$table$map[[1L]],
    mean(r[row(r) != col(r)]^2),
    tolerance = 1e-14
  )
  expect_true(out$n_factors %in% out$table$n_factors)
})


test_that("original MAP matches psych VSS for overlapping positive component counts", {
  set.seed(108)
  dat <- as.data.frame(matrix(rnorm(800), ncol = 4))
  r <- stats::cor(dat)

  ours <- nomo_factors_map(r, max_factors = 3)$table
  theirs <- suppressWarnings(
    suppressMessages(
      psych::VSS(
        r,
        n = 3,
        rotate = "none",
        fm = "pc",
        n.obs = nrow(dat),
        plot = FALSE
      )$map
    )
  )

  ours_positive <- ours$map[ours$n_factors >= 1L]
  compared <- seq_len(min(length(ours_positive), length(theirs)))
  compared <- compared[
    is.finite(ours_positive[compared]) & is.finite(theirs[compared])
  ]

  expect_gt(length(compared), 0L)
  expect_equal(
    ours_positive[compared],
    as.numeric(theirs[compared]),
    tolerance = 1e-8
  )
})


test_that("summary formats very small Bartlett p-values for people", {
  set.seed(109)
  f <- rnorm(300)
  dat <- as.data.frame(
    replicate(5, 0.8 * f + rnorm(300, sd = 0.6))
  )

  out <- nomo_factors(dat, n_iter = 10, seed = 90)
  s <- summary(out)

  expect_true("display" %in% names(s$adequacy))
  expect_match(
    s$adequacy$display[s$adequacy$metric == "Bartlett"],
    "p < \\.001"
  )
  expect_identical(out$evidence$method, c("Parallel analysis", "MAP (original)"))
})
