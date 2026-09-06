make_efa_factor_count_data <- function(n = 260L, seed = 8201L) {
  set.seed(seed)

  f1 <- rnorm(n)
  f2 <- .30 * f1 + sqrt(1 - .30^2) * rnorm(n)

  data.frame(
    a1 = .82 * f1 + rnorm(n, sd = .55),
    a2 = .78 * f1 + rnorm(n, sd = .60),
    a3 = .74 * f1 + rnorm(n, sd = .65),
    b1 = .82 * f2 + rnorm(n, sd = .55),
    b2 = .78 * f2 + rnorm(n, sd = .60),
    b3 = .74 * f2 + rnorm(n, sd = .65)
  )
}


test_that("nomo_efa can inherit M2 context while researcher overrides factor count", {
  dat <- make_efa_factor_count_data()

  fac <- nomo_factors(
    dat,
    criterion_set = "minimal",
    n_iter = 10L,
    seed = 2026
  )

  primary <- as.integer(fac$parallel$n_factors)
  chosen <- if (identical(primary, 1L)) 2L else 1L

  efa <- nomo_efa(
    dat,
    factors = fac,
    factor_count = chosen
  )

  expect_equal(efa$n_factors, chosen)
  expect_equal(
    efa$factor_source,
    "researcher_with_nomo_factors_context"
  )
  expect_equal(
    efa$factor_context$primary_parallel,
    primary
  )
  expect_equal(
    efa$factor_context$selected_factor_count,
    chosen
  )
})


test_that("factor-count override preserves inherited modeling-type provenance", {
  set.seed(8202L)
  n <- 260L
  f <- rnorm(n)

  make_ord <- function(x) {
    ordered(
      cut(
        x,
        breaks = c(-Inf, -.75, 0, .75, Inf),
        labels = FALSE
      )
    )
  }

  dat <- data.frame(
    q1 = make_ord(.82 * f + rnorm(n, sd = .60)),
    q2 = make_ord(.78 * f + rnorm(n, sd = .62)),
    q3 = make_ord(.76 * f + rnorm(n, sd = .64)),
    q4 = make_ord(.72 * f + rnorm(n, sd = .68))
  )

  fac <- nomo_factors(
    dat,
    types = stats::setNames(rep("ordinal", 4L), names(dat)),
    criterion_set = "minimal",
    n_iter = 10L,
    seed = 2026
  )

  efa <- nomo_efa(
    dat,
    factors = fac,
    factor_count = 1L
  )

  expect_true(all(
    efa$modeling_types$source == "inherited_from_nomo_factors"
  ))
  expect_true(any(
    efa$decision_log$metric == "modeling_types_inherited"
  ))
})


test_that("factor_count is reserved for a nomo_factors handoff", {
  dat <- make_efa_factor_count_data(seed = 8203L)

  expect_error(
    nomo_efa(
      dat,
      factors = 1L,
      factor_count = 1L
    ),
    "only when `factors` is a `nomo_factors` object"
  )

  fac <- nomo_factors(
    dat,
    criterion_set = "minimal",
    n_iter = 10L,
    seed = 2026
  )

  expect_error(
    nomo_efa(
      dat,
      factors = fac,
      factor_count = 1.5
    ),
    "positive integer"
  )
})
