make_m8_full_data <- function(n = 300L, seed = 8401L) {
  set.seed(seed)

  f <- rnorm(n)
  group <- rep(c("A", "B"), length.out = n)

  data.frame(
    i1 = .82 * f + rnorm(n, sd = .55),
    i2 = .78 * f + rnorm(n, sd = .60),
    i3 = .75 * f + rnorm(n, sd = .64),
    i4 = .72 * f + rnorm(n, sd = .68),
    criterion = .55 * f + rnorm(n, sd = .75),
    group = group
  )
}


m8_full_settings <- function() {
  list(
    factors = list(
      criterion_set = "minimal",
      n_iter = 10L,
      seed = 2026L
    )
  )
}


m8_model <- function() {
  "WellBeing =~ i1 + i2 + i3 + i4"
}
