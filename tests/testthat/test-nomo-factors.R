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
  expect_equal(nrow(s$evidence), 4)
  expect_identical(
    s$evidence$criterion,
    c("parallel", "map_original", "map_revised", "ekc")
  )
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
  expect_equal(out$table$map, out$table$map_original)
  expect_identical(out$table$minimum, out$table$minimum_original)
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
  expect_identical(
    out$evidence$method,
    c(
      "Parallel analysis",
      "MAP (original TR2)",
      "MAP (revised TR4)",
      "Empirical Kaiser criterion"
    )
  )
})

# ---- recovered from hygiene consolidation: test-nomo-factors.R ----
# ---- consolidated from test-nomo-factors-coverage-final.R ----
make_cov_final_data <- function(n = 180L, seed = 9301L) {
  set.seed(seed)
  f <- stats::rnorm(n)
  out <- as.data.frame(
    replicate(5, 0.80 * f + stats::rnorm(n, sd = 0.60))
  )
  names(out) <- paste0("i", seq_len(ncol(out)))
  out
}

make_cov_final_ordinal <- function(n = 180L, seed = 9302L) {
  set.seed(seed)
  f <- stats::rnorm(n)
  latent <- replicate(6, 0.78 * f + stats::rnorm(n, sd = 0.65))
  out <- as.data.frame(lapply(seq_len(ncol(latent)), function(j) {
    ordered(cut(
      latent[, j],
      breaks = c(-Inf, -0.9, -0.25, 0.25, 0.9, Inf),
      labels = 1:5
    ))
  }))
  names(out) <- paste0("o", seq_len(ncol(out)))
  out
}


test_that("remaining public input-validation branches are explicit", {
  dat <- make_cov_final_data(n = 60L)

  expect_error(nomo_factors(data.frame()), "at least one row")

  zero_cols <- data.frame(row.names = seq_len(4L))
  expect_error(nomo_factors(zero_cols), "at least one column")

  expect_error(nomo_factors(dat, guidance = 1), "guidance")
  expect_error(nomo_factors(dat, items = character()), "non-empty character")
  expect_error(nomo_factors(dat, items = c("i1", NA_character_, "i3")), "missing or empty")
  expect_error(nomo_factors(dat, items = c("i1", "i1", "i3")), "duplicate")
  expect_error(nomo_factors(dat, n_iter = 10.5), "single integer")
  expect_error(nomo_factors(dat, quantile = NA_real_), "strictly between")
  expect_error(nomo_factors(dat, seed = 2.5), "single integer")
  expect_error(nomo_factors(dat, fm = NA_character_), "non-empty character")
  expect_error(nomo_factors(dat, smooth = NA), "TRUE.*FALSE")
  expect_error(nomo_factors(dat, max_factors = 0), "positive integer")
})


test_that("remaining modeling-type defensive branches are protected", {
  dat <- make_cov_final_data(n = 60L)

  expect_error(
    nomo_factors(dat, types = c(i1 = "ordinal", i1 = "binary")),
    "unique item names"
  )
  expect_error(
    nomo_factors(dat, types = c(i1 = "unsupported")),
    "Unsupported modeling type"
  )

  character_dat <- data.frame(
    a = letters[1:6],
    b = stats::rnorm(6),
    c = stats::rnorm(6)
  )
  expect_error(
    nomo_factors(
      character_dat,
      types = c(a = "continuous"),
      n_iter = 10,
      seed = 9303
    ),
    "marked continuous but is not stored numerically"
  )

  selected <- data.frame(a = 1:4)
  bad_types <- tibble::tibble(item = "a", model_type = "unsupported")
  expect_error(
    nomo_factors_numeric_data(selected, bad_types),
    "Unsupported modeling type"
  )

  expect_identical(
    nomo_factors_choose_correlation("pearson", c("continuous", "ordinal")),
    "pearson"
  )
  expect_error(
    nomo_factors_choose_correlation("tetrachoric", c("binary", "ordinal")),
    "requires all selected items to be binary"
  )
  expect_error(
    nomo_factors_choose_correlation("polychoric", c("ordinal", "continuous")),
    "requires ordinal/binary"
  )

  x <- data.frame(a = 1:6, b = 2:7, c = 3:8)
  expect_error(
    nomo_factors_correlation(
      x,
      model_types = rep("continuous", 3),
      method = "not-a-method",
      use = "complete"
    ),
    "Could not estimate"
  )
})


test_that("presentation covers unavailable and smoothed states", {
  out <- nomo_factors(
    make_cov_final_data(),
    criterion_set = "core",
    n_iter = 10,
    seed = 9304
  )

  smoothed <- out
  smoothed$smoothed <- TRUE
  expect_match(
    paste(capture.output(print(smoothed)), collapse = "\n"),
    "explicitly smoothed"
  )

  unavailable <- out
  unavailable$kmo$available <- FALSE
  unavailable$kmo$overall <- NA_real_
  unavailable$bartlett <- list(
    available = FALSE,
    chisq = NA_real_,
    df = NA_real_,
    p_value = NA_real_,
    reason = "No common sample size."
  )

  printed <- paste(capture.output(print(unavailable)), collapse = "\n")
  expect_match(printed, "KMO: unavailable")

  s <- summary(unavailable)
  expect_identical(s$adequacy$display[s$adequacy$metric == "KMO"], "unavailable")
  expect_identical(
    s$adequacy$display[s$adequacy$metric == "Bartlett"],
    "No common sample size."
  )
  expect_error(
    plot(unavailable, type = "kmo"),
    "KMO/MSA values are unavailable"
  )
})


test_that("presentation covers alternate PA labels and legacy context", {
  out <- nomo_factors(
    make_cov_final_data(seed = 9305L),
    criterion_set = "core",
    n_iter = 10,
    seed = 9305
  )

  for (rule in c("mean", "crawford")) {
    alt <- out
    alt$parallel$rule <- rule
    alt$parallel$table$random_reference <- if (rule == "mean") {
      alt$parallel$table$random_mean
    } else {
      alt$parallel$table$random_crawford
    }
    expect_s3_class(plot(alt, type = "retention"), "ggplot")
  }

  legacy <- out
  row <- legacy$evidence[1L, , drop = FALSE]
  row$criterion <- "kaiser"
  row$method <- "Kaiser-Guttman (> 1)"
  row$family <- "kaiser"
  row$family_method <- "Kaiser-Guttman"
  row$n_factors <- 3L
  row$role <- "legacy"
  row$reference <- "Legacy eigenvalue-greater-than-one rule"
  legacy$evidence <- dplyr::bind_rows(legacy$evidence, row)

  p <- plot(legacy, type = "evidence")
  expect_match(p$labels$caption, "Legacy criteria are context only")

  expect_error(
    plot(out, type = "parallel_rules", show_values = NA),
    "show_values"
  )
})


test_that("concordance presentation falls back and fails informatively", {
  out <- nomo_factors(
    make_cov_final_data(seed = 9306L),
    criterion_set = "core",
    n_iter = 10,
    seed = 9306
  )

  fallback <- out
  fallback$family_concordance <- NULL
  expect_s3_class(plot(fallback, type = "concordance"), "ggplot")

  empty <- out
  empty$family_concordance <- data.frame()
  empty$concordance <- data.frame()
  expect_error(
    plot(empty, type = "concordance"),
    "No internally consistent criterion-family recommendations"
  )
})


test_that("summary prints skipped criteria and qualification together", {
  out <- nomo_factors(
    make_cov_final_ordinal(),
    criterion_set = "extended",
    n_iter = 10,
    seed = 9307
  )

  printed <- paste(capture.output(print(summary(out))), collapse = "\n")
  expect_match(printed, "Criteria available with qualification")
  expect_match(printed, "approximate")
  expect_match(printed, "Criteria requested but not run")
  expect_match(printed, "NEST")
  expect_match(printed, "Hull")
})


test_that("p-value formatter covers unavailable, tiny, and ordinary values", {
  expect_identical(nomo_format_p(NA_real_), "unavailable")
  expect_identical(nomo_format_p(c(0.1, 0.2)), "unavailable")
  expect_identical(nomo_format_p(0.0005), "< .001")
  expect_identical(nomo_format_p(0.05), "= 0.050")
})

# ---- consolidated from test-nomo-factors-coverage.R ----
make_factor_coverage_data <- function(n = 180L, p = 8L, seed = 9001L) {
  set.seed(seed)
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  f1 <- z1
  f2 <- 0.35 * z1 + sqrt(1 - 0.35^2) * z2
  x <- lapply(seq_len(p), function(j) {
    f <- if (j <= ceiling(p / 2)) f1 else f2
    0.78 * f + stats::rnorm(n, sd = 0.62)
  })
  out <- as.data.frame(x)
  names(out) <- paste0("i", seq_len(p))
  out
}


test_that("nomo_factors hardens public input validation branches", {
  good <- make_factor_coverage_data(n = 30L, p = 3L)

  expect_error(
    nomo_factors(data.frame(a = numeric(), b = numeric(), c = numeric())),
    "at least one row"
  )
  expect_error(
    nomo_factors(data.frame(row.names = seq_len(3))),
    "at least one column"
  )
  expect_error(nomo_factors(good, guidance = 1), "guidance.*list")

  expect_error(nomo_factors(good, items = 1:3), "non-empty character")
  expect_error(nomo_factors(good, items = character()), "non-empty character")
  expect_error(nomo_factors(good, items = c("i1", NA_character_, "i3")), "missing or empty")
  expect_error(nomo_factors(good, items = c("i1", "", "i3")), "missing or empty")
  expect_error(nomo_factors(good, items = c("i1", "i1", "i3")), "duplicate")

  expect_error(nomo_factors(good, n_iter = NA_real_), "n_iter")
  expect_error(nomo_factors(good, n_iter = "10"), "n_iter")
  expect_error(nomo_factors(good, n_iter = 10.5), "n_iter")
  expect_error(nomo_factors(good, quantile = NA_real_), "quantile")
  expect_error(nomo_factors(good, quantile = "0.95"), "quantile")
  expect_error(nomo_factors(good, quantile = 0), "quantile")
  expect_error(nomo_factors(good, seed = NA_real_), "seed")
  expect_error(nomo_factors(good, seed = 1.5), "seed")
  expect_error(nomo_factors(good, fm = NA_character_), "fm")
  expect_error(nomo_factors(good, fm = ""), "fm")
  expect_error(nomo_factors(good, fm = 1), "fm")
  expect_error(nomo_factors(good, smooth = NA), "smooth")
  expect_error(nomo_factors(good, smooth = 1), "smooth")
})


test_that("nomo_factors rejects unusable observed-data configurations explicitly", {
  constant <- data.frame(
    a = rep(1, 8),
    b = 1:8,
    c = 8:1
  )
  expect_error(
    nomo_factors(constant),
    "nonconstant observed data"
  )

  all_missing <- data.frame(
    a = rep(NA_real_, 8),
    b = 1:8,
    c = 8:1
  )
  expect_error(
    nomo_factors(all_missing),
    "nonconstant observed data"
  )

  sparse <- data.frame(
    a = c(1, 2, 3, 4),
    b = c(2, 3, NA, NA),
    c = c(3, 4, NA, NA)
  )
  expect_error(
    nomo_factors(sparse, missing = "complete"),
    "Fewer than three complete cases"
  )
  expect_error(
    nomo_factors(sparse, missing = "pairwise"),
    "fewer than three jointly observed"
  )

  good <- make_factor_coverage_data(n = 40L, p = 4L, seed = 9002L)
  expect_error(nomo_factors(good, max_factors = 0), "positive integer")
  expect_error(nomo_factors(good, max_factors = 1.5), "positive integer")
})


test_that("model-type validation covers invalid defaults and overrides", {
  numeric_dat <- data.frame(a = 1:5, b = 2:6, c = 3:7)
  numeric_screen <- stats::setNames(
    rep("numeric_continuous", 3L),
    names(numeric_dat)
  )

  expect_error(
    nomo_factors_model_types(
      selected = data.frame(a = factor(letters[1:5]), b = 2:6, c = 3:7),
      items = c("a", "b", "c"),
      screen_types = c(a = "nominal", b = "numeric_continuous", c = "numeric_continuous")
    ),
    "defensible default"
  )

  expect_error(
    nomo_factors_model_types(numeric_dat, names(numeric_dat), numeric_screen, "ordinal"),
    "named character vector"
  )
  expect_error(
    nomo_factors_model_types(
      numeric_dat,
      names(numeric_dat),
      numeric_screen,
      c(a = "ordinal", a = "ordinal")
    ),
    "unique item names"
  )
  expect_error(
    nomo_factors_model_types(
      numeric_dat,
      names(numeric_dat),
      numeric_screen,
      c(z = "ordinal")
    ),
    "not selected"
  )
  expect_error(
    nomo_factors_model_types(
      numeric_dat,
      names(numeric_dat),
      numeric_screen,
      c(a = "nominal")
    ),
    "Unsupported modeling type"
  )

  ordered_dat <- numeric_dat
  ordered_dat$a <- ordered(letters[1:5])
  ordered_screen <- numeric_screen
  ordered_screen[["a"]] <- "ordered"
  expect_error(
    nomo_factors_model_types(
      ordered_dat,
      names(ordered_dat),
      ordered_screen,
      c(a = "continuous")
    ),
    "marked continuous"
  )

  character_dat <- numeric_dat
  character_dat$a <- letters[1:5]
  expect_error(
    nomo_factors_model_types(
      character_dat,
      names(character_dat),
      numeric_screen,
      c(a = "ordinal")
    ),
    "marked ordinal"
  )

  three_level <- numeric_dat
  three_level$a <- c(1, 2, 3, 1, 2)
  discrete_screen <- numeric_screen
  discrete_screen[["a"]] <- "numeric_discrete"
  expect_error(
    nomo_factors_model_types(
      three_level,
      names(three_level),
      discrete_screen,
      c(a = "binary")
    ),
    "exactly two observed values"
  )
})


test_that("numeric conversion and explicit correlation guards are covered", {
  selected <- data.frame(
    continuous = c(1.2, 2.3, 3.4, 4.5),
    ordinal = ordered(c("low", "mid", "high", "mid"), levels = c("low", "mid", "high")),
    binary_factor = factor(c("no", "yes", "no", "yes")),
    binary_logical = c(TRUE, FALSE, TRUE, FALSE),
    binary_numeric = c(10, 20, 10, 20)
  )
  types <- tibble::tibble(
    item = names(selected),
    screen_type = c("numeric_continuous", "ordered", "binary", "binary", "binary"),
    model_type = c("continuous", "ordinal", "binary", "binary", "binary"),
    source = "test"
  )

  converted <- nomo_factors_numeric_data(selected, types)
  expect_true(all(vapply(converted, is.numeric, logical(1))))
  expect_identical(sort(unique(converted$binary_numeric)), c(0, 1))

  expect_identical(
    nomo_factors_choose_correlation("pearson", c("ordinal", "ordinal")),
    "pearson"
  )
  expect_identical(
    nomo_factors_choose_correlation("mixed", c("continuous", "ordinal")),
    "mixed"
  )
  expect_error(
    nomo_factors_choose_correlation("tetrachoric", c("binary", "ordinal")),
    "requires all selected items to be binary"
  )
  expect_error(
    nomo_factors_choose_correlation("polychoric", c("continuous", "ordinal")),
    "requires ordinal/binary"
  )

  x <- data.frame(a = 1:6, b = 2:7, c = 3:8)
  expect_error(
    nomo_factors_correlation(
      x,
      model_types = rep("continuous", 3L),
      method = "not-a-method",
      use = "pairwise"
    ),
    "Could not estimate the not-a-method correlation matrix"
  )

  expect_error(
    nomo_factors_factor_eigenvalues("not-a-correlation-matrix", fm = "minres"),
    "Could not obtain common-factor eigenvalues"
  )
})


test_that("pairwise-N, MAP truncation, KMO singularity, and Bartlett unavailability are explicit", {
  x <- data.frame(
    a = c(1, 2, NA, 4),
    b = c(1, NA, 3, 4),
    c = c(NA, 2, 3, 4)
  )
  nmat <- nomo_factors_pairwise_n(x)
  expect_equal(unname(diag(nmat)), c(3L, 3L, 3L))
  expect_equal(nmat[1, 2], 2L)

  perfect <- matrix(1, nrow = 4, ncol = 4)
  mapped <- nomo_factors_map(perfect, max_factors = 3L)
  expect_true(mapped$truncated)
  expect_equal(mapped$m_last, 0L)

  kmo <- nomo_factors_kmo(perfect, paste0("i", 1:4))
  expect_false(kmo$available)
  expect_true(all(is.na(kmo$item$msa)))

  bart <- nomo_factors_bartlett(diag(3), n = 30L, available = FALSE)
  expect_false(bart$available)
  expect_match(bart$reason, "pairwise missing-data")
})


test_that("small samples and unavailable supporting diagnostics are logged", {
  item_types <- tibble::tibble(
    item = c("a", "b", "c"),
    screen_type = rep("numeric_continuous", 3L),
    model_type = rep("continuous", 3L),
    source = rep("inferred_from_storage", 3L)
  )
  pa <- list(n_factors = 1L, rule = "percentile", quantile = 0.95,
             sensitivity = tibble::tibble(rule = c("percentile", "mean", "crawford"), n_factors = 1L, selected = c(TRUE, FALSE, FALSE)),
             n_smoothed_null = 0L)
  map <- list(n_factors_original = 1L, n_factors_revised = 1L)
  criteria <- list(
    evidence = tibble::tibble(
      criterion = c("parallel", "map_original"),
      method = c("Parallel analysis", "MAP (original TR2)"),
      family = c("parallel", "map"),
      family_method = c("Parallel analysis", "MAP"),
      n_factors = c(1L, 1L),
      role = c("primary", "complementary"),
      reference = c("", "")
    ),
    status = tibble::tibble(
      criterion = c("parallel", "map_original"),
      method = c("Parallel analysis", "MAP (original TR2)"),
      status = c("available", "available"),
      reason = c("", ""),
      qualification = c("", "")
    )
  )
  synthesis <- nomo_factors_synthesis(criteria$evidence, parallel_n = 1L, status = criteria$status)

  log <- nomo_factors_log(
    item_types = item_types,
    requested_correlation = "pearson",
    correlation_method = "pearson",
    missing = "complete",
    min_pairwise_n = 40L,
    smoothed = FALSE,
    original_min_eigen = 0.2,
    kmo = list(available = FALSE, overall = NA_real_),
    bartlett = list(available = FALSE, p_value = NA_real_, reason = "Unavailable for test."),
    pa = pa,
    map = map,
    criteria = criteria,
    synthesis = synthesis,
    guidance = nomo_defaults()
  )

  expect_true(any(log$metric == "small_sample_prompt"))
  expect_true(any(log$metric == "kmo" & log$severity == "review"))
  expect_true(any(log$metric == "bartlett"))
})


test_that("criterion plans, metadata, extraction, and seed helpers cover all variants", {
  expect_identical(nomo_factors_criterion_plan("minimal"), c("parallel", "map_original"))
  expect_equal(length(nomo_factors_criterion_plan("core")), 4L)
  expect_equal(length(nomo_factors_criterion_plan("extended")), 6L)
  expect_equal(length(nomo_factors_criterion_plan("all")), 8L)

  ids <- c("parallel", "map_original", "map_revised", "ekc", "nest", "hull", "comparison_data", "kaiser")
  meta <- lapply(ids, nomo_factors_criterion_metadata)
  expect_true(all(vapply(meta, is.list, logical(1))))
  expect_true(all(vapply(meta, function(x) nzchar(x$method), logical(1))))

  expect_true(is.na(nomo_factors_extract_n(numeric())))
  expect_true(is.na(nomo_factors_extract_n(c(a = NA_real_, b = Inf))))
  expect_equal(nomo_factors_extract_n(c(other = 2, BvA = 3), preferred = "BvA"), 3L)
  expect_equal(nomo_factors_extract_n(c(2.4)), 2L)

  expect_true(nomo_factors_seed_offset(-10L, 5L) >= 1L)
  expect_type(nomo_factors_seed_offset(.Machine$integer.max, 100L), "integer")
})


test_that("criterion wrappers convert engine failures into documented unavailability", {
  ekc <- nomo_factors_ekc("not-a-correlation-matrix", n_obs = 20L)
  expect_false(isTRUE(ekc$available))
  expect_match(ekc$reason, "EKC could not be computed")

  nest <- nomo_factors_nest(
    "not-a-correlation-matrix",
    n_obs = 20L,
    n_iter = 10L,
    seed = 9201L
  )
  expect_false(isTRUE(nest$available))
  expect_match(nest$reason, "NEST could not be computed")

  hull <- nomo_factors_hull(
    "not-a-correlation-matrix",
    n_obs = 20L,
    n_iter = 10L,
    quantile = 0.95,
    seed = 9202L
  )
  expect_false(isTRUE(hull$available))
  expect_match(hull$reason, "Hull could not be computed")

  cd <- nomo_factors_cd(
    x = "not-data",
    max_factors = 2L,
    n_population = 100L,
    n_samples = 10L,
    alpha = 0.30,
    seed = 9203L
  )
  expect_false(isTRUE(cd$available))
  expect_match(cd$reason, "Comparison data could not be computed")
})


test_that("extended criteria document missing-data and short-scale skips", {
  dat <- make_factor_coverage_data(n = 140L, p = 8L, seed = 9301L)
  dat[1:10, 1] <- NA_real_
  dat[11:20, 2] <- NA_real_

  out <- nomo_factors(
    dat,
    criterion_set = "extended",
    missing = "pairwise",
    n_iter = 10,
    seed = 9302L
  )

  status <- out$criterion_status
  expect_identical(status$status[status$criterion == "ekc"], "skipped")
  expect_match(status$reason[status$criterion == "ekc"], "common sample size")
  expect_true(all(status$status[status$criterion %in% c("nest", "hull")] == "skipped"))
  expect_true(all(grepl(
    "complete common sample",
    status$reason[status$criterion %in% c("nest", "hull")],
    fixed = TRUE
  )))

  short <- make_factor_coverage_data(n = 150L, p = 5L, seed = 9303L)
  short_out <- nomo_factors(
    short,
    criterion_set = "extended",
    n_iter = 10,
    seed = 9304L
  )
  hull_status <- short_out$criterion_status[short_out$criterion_status$criterion == "hull", , drop = FALSE]
  expect_identical(hull_status$status, "skipped")
  expect_match(hull_status$reason, "at least six indicators")
})


test_that("all criterion set exposes comparison data and legacy context without legacy voting", {
  dat <- make_factor_coverage_data(n = 160L, p = 8L, seed = 9401L)
  guidance <- nomo_defaults()
  guidance$factor_cd_population <- 600L
  guidance$factor_cd_samples <- 10L
  guidance$factor_cd_alpha <- 0.30

  out <- nomo_factors(
    dat,
    criterion_set = "all",
    max_factors = 3L,
    n_iter = 10,
    seed = 9402L,
    guidance = guidance
  )

  expect_identical(
    out$criterion_status$criterion,
    c("parallel", "map_original", "map_revised", "ekc", "nest", "hull", "comparison_data", "kaiser")
  )
  expect_true(any(out$evidence$criterion == "kaiser"))
  expect_identical(out$evidence$role[out$evidence$criterion == "kaiser"], "legacy")
  expect_false(any(out$family_evidence$family == "kaiser"))
  expect_true(out$criterion_status$status[out$criterion_status$criterion == "comparison_data"] %in% c("available", "skipped"))
})


test_that("synthesis covers primary-only, neighboring, divergent, and skipped-method narratives", {
  primary_only <- nomo_factors_synthesis(
    tibble::tibble(),
    parallel_n = 1L,
    status = tibble::tibble(status = "skipped")
  )
  expect_identical(primary_only$agreement, "primary_only")
  expect_match(primary_only$text, "No additional recommended criterion families")
  expect_match(primary_only$text, "1 requested method was not evaluated")

  make_evidence <- function(counts) {
    tibble::tibble(
      criterion = c("parallel", "map_original", "ekc"),
      method = c("Parallel analysis", "MAP (original TR2)", "Empirical Kaiser criterion"),
      n_factors = as.integer(counts),
      role = c("primary", "complementary", "complementary"),
      reference = c("", "", "")
    )
  }

  near <- nomo_factors_synthesis(
    make_evidence(c(1, 2, 2)),
    parallel_n = 1L,
    status = tibble::tibble(status = c("available", "available", "skipped"))
  )
  expect_identical(near$agreement, "near")
  expect_match(near$text, "Carry both neighboring solutions")
  expect_match(near$text, "1 requested method was not evaluated")

  divergent <- nomo_factors_synthesis(
    make_evidence(c(1, 3, 4)),
    parallel_n = 1L,
    status = tibble::tibble(status = c("available", "skipped", "skipped"))
  )
  expect_identical(divergent$agreement, "divergent")
  expect_match(divergent$text, "materially divergent")
  expect_match(divergent$text, "2 requested methods were not evaluated")
})

# ---- consolidated from test-nomo-factors-m2b.R ----
make_m2b_two_factor <- function(n = 450L, seed = 20260903L) {
  set.seed(seed)
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  f1 <- z1
  f2 <- 0.35 * z1 + sqrt(1 - 0.35^2) * z2

  data.frame(
    a1 = 0.85 * f1 + stats::rnorm(n, sd = 0.50),
    a2 = 0.82 * f1 + stats::rnorm(n, sd = 0.52),
    a3 = 0.78 * f1 + stats::rnorm(n, sd = 0.56),
    a4 = 0.80 * f1 + stats::rnorm(n, sd = 0.54),
    b1 = 0.85 * f2 + stats::rnorm(n, sd = 0.50),
    b2 = 0.82 * f2 + stats::rnorm(n, sd = 0.52),
    b3 = 0.78 * f2 + stats::rnorm(n, sd = 0.56),
    b4 = 0.80 * f2 + stats::rnorm(n, sd = 0.54)
  )
}


make_m2b_ordinal <- function(n = 450L, seed = 20260904L) {
  set.seed(seed)
  f <- stats::rnorm(n)
  latent <- replicate(6, 0.82 * f + stats::rnorm(n, sd = 0.58))

  out <- as.data.frame(
    lapply(seq_len(ncol(latent)), function(j) {
      x <- latent[, j]
      ordered(
        cut(
          x,
          breaks = stats::quantile(
            x,
            probs = seq(0, 1, length.out = 6),
            na.rm = TRUE
          ),
          include.lowest = TRUE,
          labels = FALSE
        ),
        levels = 1:5
      )
    })
  )
  names(out) <- paste0("i", seq_len(ncol(out)))
  out
}


test_that("core criterion set exposes multi-criterion retention evidence", {
  dat <- make_m2b_two_factor()

  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 401
  )

  expect_identical(out$criterion_set, "core")
  expect_identical(
    out$evidence$criterion,
    c("parallel", "map_original", "map_revised", "ekc")
  )
  expect_true(all(out$criterion_status$status == "available"))
  expect_equal(out$parallel$n_factors, 2L)
  expect_true(2L %in% out$plausible_factors)
  expect_true(all(c("map_original", "map_revised") %in% names(out$map$table)))
})


test_that("minimal criterion set stays deliberately compact", {
  dat <- make_m2b_two_factor(n = 320L, seed = 2)

  out <- nomo_factors(
    dat,
    criterion_set = "minimal",
    n_iter = 10,
    seed = 402
  )

  expect_identical(
    out$evidence$criterion,
    c("parallel", "map_original")
  )
  expect_identical(
    out$criterion_status$criterion,
    c("parallel", "map_original")
  )
})


test_that("parallel-analysis rule sensitivity is always retained", {
  dat <- make_m2b_two_factor(n = 350L, seed = 3)

  for (rule in c("percentile", "mean", "crawford")) {
    out <- nomo_factors(
      dat,
      criterion_set = "minimal",
      parallel_rule = rule,
      n_iter = 10,
      seed = 403
    )

    expect_identical(out$parallel$rule, rule)
    expect_identical(
      out$parallel$sensitivity$rule,
      c("percentile", "mean", "crawford")
    )
    expect_equal(sum(out$parallel$sensitivity$selected), 1L)
    expect_true(out$parallel$sensitivity$selected[out$parallel$sensitivity$rule == rule])
    expect_equal(
      out$parallel$n_factors,
      out$parallel$sensitivity$n_factors[out$parallel$sensitivity$rule == rule]
    )
  }
})


test_that("revised and original MAP are both explicit", {
  dat <- make_m2b_two_factor(n = 360L, seed = 4)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 404
  )

  expect_true(all(c(
    "map_original",
    "map_revised",
    "minimum_original",
    "minimum_revised"
  ) %in% names(out$map$table)))
  expect_true(out$map$n_factors_original %in% out$map$table$n_factors)
  expect_true(out$map$n_factors_revised %in% out$map$table$n_factors)
  expect_true(any(out$decision_log$metric == "map_revised"))
})


test_that("EKC is available in the core set for ordinary Pearson data", {
  dat <- make_m2b_two_factor(n = 380L, seed = 5)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 405
  )

  ekc_status <- out$criterion_status[
    out$criterion_status$criterion == "ekc",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(ekc_status), 1L)
  expect_identical(ekc_status$status, "available")
  expect_true(any(out$evidence$criterion == "ekc"))
})


test_that("ordinal analyses skip incompatible extended criteria explicitly", {
  dat <- make_m2b_ordinal()

  out <- nomo_factors(
    dat,
    criterion_set = "extended",
    n_iter = 10,
    seed = 406
  )

  expect_identical(out$correlation_method, "polychoric")
  expect_identical(out$correlation, "polychoric")
  expect_identical(out$modeling_types, out$item_types)
  skipped <- out$criterion_status[
    out$criterion_status$criterion %in% c("nest", "hull"),
    ,
    drop = FALSE
  ]
  expect_equal(nrow(skipped), 2L)
  expect_true(all(skipped$status == "skipped"))
  expect_true(all(grepl("continuous-reference", skipped$reason, fixed = TRUE)))
  expect_false(any(out$evidence$criterion %in% c("nest", "hull")))

  ekc_status <- out$criterion_status[
    out$criterion_status$criterion == "ekc",
    ,
    drop = FALSE
  ]
  expect_equal(nrow(ekc_status), 1L)
  expect_identical(ekc_status$status, "available")
  expect_match(ekc_status$qualification, "approximate")

  ekc_log <- out$decision_log[out$decision_log$metric == "ekc", , drop = FALSE]
  expect_equal(nrow(ekc_log), 1L)
  expect_identical(ekc_log$severity, "review")
  expect_match(ekc_log$observation, "approximate")
})


test_that("NEST and Hull wrappers run on supported continuous data", {
  dat <- make_m2b_two_factor(n = 300L, seed = 6)
  r <- stats::cor(dat)

  nest <- nomo_factors_nest(
    corr = r,
    n_obs = nrow(dat),
    n_iter = 10,
    seed = 407
  )
  expect_true(nest$available)
  expect_true(is.finite(nest$n_factors))

  hull <- nomo_factors_hull(
    corr = r,
    n_obs = nrow(dat),
    n_iter = 10,
    quantile = 0.95,
    seed = 408
  )
  expect_true(hull$available)
  expect_true(is.finite(hull$n_factors))
})


test_that("comparison-data wrapper runs reproducibly on supported raw data", {
  dat <- make_m2b_two_factor(n = 180L, seed = 7)

  a <- nomo_factors_cd(
    x = dat,
    max_factors = 3L,
    n_population = 1000L,
    n_samples = 10L,
    alpha = 0.30,
    seed = 409
  )
  b <- nomo_factors_cd(
    x = dat,
    max_factors = 3L,
    n_population = 1000L,
    n_samples = 10L,
    alpha = 0.30,
    seed = 409
  )

  expect_true(a$available)
  expect_true(is.finite(a$n_factors))
  expect_equal(a$n_factors, b$n_factors)
})


test_that("legacy Kaiser evidence never votes in the synthesis", {
  evidence <- tibble::tibble(
    criterion = c("parallel", "map_original", "kaiser"),
    method = c(
      "Parallel analysis",
      "MAP (original TR2)",
      "Kaiser-Guttman (> 1)"
    ),
    n_factors = c(2L, 2L, 5L),
    role = c("primary", "complementary", "legacy"),
    reference = c("", "", "")
  )

  syn <- nomo_factors_synthesis(evidence, parallel_n = 2L)

  expect_equal(syn$n_available, 2L)
  expect_equal(syn$min_factors, 2L)
  expect_equal(syn$max_factors, 2L)
  expect_equal(syn$plausible_factors, 2L)
})


test_that("M2B plot suite exposes sensitivity and concordance", {
  dat <- make_m2b_two_factor(n = 320L, seed = 8)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 410
  )

  for (type in c(
    "retention",
    "parallel_rules",
    "scree",
    "map",
    "evidence",
    "concordance",
    "kmo"
  )) {
    expect_s3_class(plot(out, type = type), "ggplot")
  }
})


test_that("summary exposes criteria status, PA sensitivity, and concordance", {
  dat <- make_m2b_two_factor(n = 320L, seed = 9)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 411
  )
  s <- summary(out)

  expect_s3_class(s, "summary_nomo_factors")
  expect_identical(s$criterion_set, "core")
  expect_equal(nrow(s$parallel_sensitivity), 3L)
  expect_true(is.data.frame(s$criterion_status))
  expect_true(is.data.frame(s$concordance))
  expect_true(is.data.frame(s$family_evidence))
  expect_identical(s$correlation, out$correlation_method)
  expect_identical(s$modeling_types, out$item_types)
})


test_that("M2B presentation distinguishes convergence from voting", {
  dat <- make_m2b_two_factor(n = 320L, seed = 10)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 412
  )

  expect_match(out$recommendation, "Related methods within a family")

  p_rules <- plot(out, type = "parallel_rules")
  expect_true(any(grepl("selected", as.character(p_rules$data$rule_display), fixed = TRUE)))

  p_map <- plot(out, type = "map")
  expect_true(isTRUE(p_map$facet$params$free$y))

  p_evidence <- plot(out, type = "evidence")
  expect_identical(p_evidence$labels$x, "Suggested factor count")
  expect_false(any(grepl("(primary)", as.character(p_evidence$data$method_display), fixed = TRUE)))
  expect_match(p_evidence$labels$caption, "not independent votes")

  p_concordance <- plot(out, type = "concordance")
  expect_identical(p_concordance$labels$y, "Number of criterion families")
  expect_match(p_concordance$labels$caption, "not independent votes")
})



test_that("criterion-family synthesis groups MAP variants before concordance", {
  evidence <- tibble::tibble(
    criterion = c("parallel", "map_original", "map_revised", "ekc", "nest", "hull"),
    method = c(
      "Parallel analysis",
      "MAP (original TR2)",
      "MAP (revised TR4)",
      "Empirical Kaiser criterion",
      "NEST",
      "Hull (CAF)"
    ),
    n_factors = c(2L, 1L, 1L, 2L, 2L, 2L),
    role = c("primary", "complementary", "complementary", "complementary", "extended", "extended"),
    reference = rep("", 6L)
  )

  syn <- nomo_factors_synthesis(evidence, parallel_n = 2L)

  expect_equal(syn$n_methods, 6L)
  expect_equal(syn$n_families, 5L)
  expect_equal(syn$support_for_primary, 4L)
  expect_equal(
    syn$family_concordance$n_families[syn$family_concordance$n_factors == 1L],
    1L
  )
  expect_equal(
    syn$family_concordance$n_families[syn$family_concordance$n_factors == 2L],
    4L
  )
  expect_match(syn$text, "4 of 5 available criterion families")
  expect_match(syn$text, "MAP points to 1")
})


test_that("internally split criterion families are not forced into one concordance bar", {
  evidence <- tibble::tibble(
    criterion = c("parallel", "map_original", "map_revised", "ekc"),
    method = c(
      "Parallel analysis",
      "MAP (original TR2)",
      "MAP (revised TR4)",
      "Empirical Kaiser criterion"
    ),
    n_factors = c(2L, 1L, 2L, 2L),
    role = c("primary", "complementary", "complementary", "complementary"),
    reference = rep("", 4L)
  )

  syn <- nomo_factors_synthesis(evidence, parallel_n = 2L)
  map_family <- syn$family_evidence[syn$family_evidence$family == "map", , drop = FALSE]

  expect_false(map_family$internally_consistent)
  expect_true(is.na(map_family$n_factors))
  expect_match(map_family$candidates, "1, 2", fixed = TRUE)
  expect_equal(sum(syn$family_concordance$n_families), 2L)
  expect_match(syn$text, "MAP is internally split across 1 and 2")
})


test_that("summary surfaces qualified criteria for non-Pearson workflows", {
  dat <- make_m2b_ordinal(n = 320L, seed = 18)
  out <- nomo_factors(
    dat,
    criterion_set = "core",
    n_iter = 10,
    seed = 413
  )

  printed <- capture.output(print(summary(out)))
  expect_true(any(grepl("Criteria available with qualification", printed, fixed = TRUE)))
  expect_true(any(grepl("approximate", printed, fixed = TRUE)))
})

# ---- consolidated from test-nomo-factors-overrides-closeout.R ----
make_closeout_ordinal_one <- function(n = 450L, seed = 9401L) {
  set.seed(seed)
  f <- stats::rnorm(n)
  latent <- replicate(6, 0.85 * f + stats::rnorm(n, sd = 0.50))
  out <- as.data.frame(lapply(seq_len(ncol(latent)), function(j) {
    ordered(cut(
      latent[, j],
      breaks = c(-Inf, -0.9, -0.25, 0.25, 0.9, Inf),
      labels = 1:5
    ))
  }))
  names(out) <- paste0("i", seq_len(ncol(out)))
  out
}

make_closeout_ordinal_two <- function(n = 520L, seed = 9402L) {
  set.seed(seed)
  z1 <- stats::rnorm(n)
  z2 <- stats::rnorm(n)
  f1 <- z1
  f2 <- 0.30 * z1 + sqrt(1 - 0.30^2) * z2
  latent <- cbind(
    0.86 * f1 + stats::rnorm(n, sd = 0.48),
    0.84 * f1 + stats::rnorm(n, sd = 0.50),
    0.82 * f1 + stats::rnorm(n, sd = 0.52),
    0.80 * f1 + stats::rnorm(n, sd = 0.54),
    0.86 * f2 + stats::rnorm(n, sd = 0.48),
    0.84 * f2 + stats::rnorm(n, sd = 0.50),
    0.82 * f2 + stats::rnorm(n, sd = 0.52),
    0.80 * f2 + stats::rnorm(n, sd = 0.54)
  )
  out <- as.data.frame(lapply(seq_len(ncol(latent)), function(j) {
    ordered(cut(
      latent[, j],
      breaks = c(-Inf, -0.9, -0.25, 0.25, 0.9, Inf),
      labels = 1:5
    ))
  }))
  names(out) <- c(paste0("a", 1:4), paste0("b", 1:4))
  out
}


test_that("explicit modeling overrides can rescue defensibly coded factors", {
  levels_order <- c("Strongly disagree", "Disagree", "Agree", "Strongly agree")
  set.seed(9403)
  f <- stats::rnorm(260)
  latent <- replicate(4, 0.82 * f + stats::rnorm(260, sd = 0.58))
  dat <- as.data.frame(lapply(seq_len(ncol(latent)), function(j) {
    factor(
      cut(
        latent[, j],
        breaks = c(-Inf, -0.6, 0, 0.6, Inf),
        labels = levels_order
      ),
      levels = levels_order,
      ordered = FALSE
    )
  }))
  names(dat) <- paste0("q", 1:4)

  expect_error(
    nomo_factors(dat, n_iter = 10, seed = 9404),
    "defensible default factor-modeling type"
  )

  declared <- stats::setNames(rep("ordinal", ncol(dat)), names(dat))
  out <- nomo_factors(dat, types = declared, n_iter = 10, seed = 9404)

  expect_identical(out$correlation_method, "polychoric")
  expect_true(all(out$item_types$model_type == "ordinal"))
  expect_true(all(out$item_types$source == "user_override"))
  expect_true(any(out$decision_log$metric == "modeling_type_override"))
  expect_match(
    out$decision_log$recommendation[out$decision_log$metric == "modeling_type_override"],
    "factor level order"
  )
})


test_that("researcher control retains storage-safety boundaries", {
  dat <- data.frame(
    a = letters[1:8],
    b = stats::rnorm(8),
    c = stats::rnorm(8)
  )
  expect_error(
    nomo_factors(dat, types = c(a = "ordinal"), n_iter = 10, seed = 9405),
    "cannot be converted to ordered scores safely"
  )

  factor_dat <- data.frame(
    a = factor(rep(c("low", "high"), 4)),
    b = stats::rnorm(8),
    c = stats::rnorm(8)
  )
  expect_error(
    nomo_factors(factor_dat, types = c(a = "continuous"), n_iter = 10, seed = 9406),
    "marked continuous but is not stored numerically"
  )
})


test_that("hard data failures precede modeling-type inference", {
  constant <- data.frame(
    a = rep("same", 12),
    b = stats::rnorm(12),
    c = stats::rnorm(12)
  )
  expect_error(nomo_factors(constant), "nonconstant observed data")

  all_missing <- data.frame(
    a = rep(NA_character_, 12),
    b = stats::rnorm(12),
    c = stats::rnorm(12)
  )
  expect_error(nomo_factors(all_missing), "nonconstant observed data")
})


test_that("ordinal one- and two-factor simulations recover ordinary structure", {
  one <- nomo_factors(
    make_closeout_ordinal_one(),
    criterion_set = "core",
    n_iter = 20,
    seed = 9407
  )
  expect_identical(one$correlation_method, "polychoric")
  expect_equal(one$parallel$n_factors, 1L)

  two <- nomo_factors(
    make_closeout_ordinal_two(),
    criterion_set = "core",
    n_iter = 20,
    seed = 9408
  )
  expect_identical(two$correlation_method, "polychoric")
  expect_equal(two$parallel$n_factors, 2L)
  expect_true(2L %in% two$plausible_factors)
})
