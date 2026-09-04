test_that("nomo_efa is implemented beginning in Milestone 3", {
  set.seed(3001)
  f <- rnorm(120)
  dat <- data.frame(
    x1 = .8 * f + rnorm(120, sd = .6),
    x2 = .8 * f + rnorm(120, sd = .6),
    x3 = .7 * f + rnorm(120, sd = .7),
    x4 = .7 * f + rnorm(120, sd = .7)
  )

  expect_s3_class(nomo_efa(dat, factors = 1), "nomo_efa")
})
