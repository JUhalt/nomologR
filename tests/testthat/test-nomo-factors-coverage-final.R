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
