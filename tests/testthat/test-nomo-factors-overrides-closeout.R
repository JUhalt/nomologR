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
