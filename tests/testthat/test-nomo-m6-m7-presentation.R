test_that("M6/M7 presentation tables preserve full precision internally", {
  set.seed(6911)
  f <- rnorm(260)
  dat <- data.frame(
    x1 = .8 * f + rnorm(260, sd = .6),
    x2 = .8 * f + rnorm(260, sd = .6),
    x3 = .7 * f + rnorm(260, sd = .7),
    x4 = .7 * f + rnorm(260, sd = .7),
    group = rep(c("A", "B"), each = 130)
  )

  inv <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric"),
    localize = FALSE
  )

  tab <- nomo_table(inv, "fit")
  expect_type(tab$cfi, "double")
  expect_false(is.character(tab$cfi))
})


test_that("M6/M7 plots carry explanatory titles and captions", {
  set.seed(6912)
  n <- 320
  A <- rnorm(n)
  B <- .4 * A + rnorm(n, sd = .9)

  dat <- data.frame(
    a1 = .8 * A + rnorm(n, sd = .6),
    a2 = .8 * A + rnorm(n, sd = .6),
    a3 = .8 * A + rnorm(n, sd = .6),
    b1 = .8 * B + rnorm(n, sd = .6),
    b2 = .8 * B + rnorm(n, sd = .6),
    b3 = .8 * B + rnorm(n, sd = .6)
  )

  net <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
    ",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  p <- plot(net, type = "effects")
  expect_true(nzchar(p$labels$title))
  expect_true(nzchar(p$labels$subtitle))
  expect_true(nzchar(p$labels$caption))
})
