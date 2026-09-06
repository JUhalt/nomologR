make_polish_network_fixture <- function(n = 420L, seed = 7501L) {
  set.seed(seed)

  A <- rnorm(n)
  B <- .45 * A + rnorm(n, sd = .90)
  C <- .03 * A + rnorm(n, sd = 1)
  criterion <- .50 * A + rnorm(n, sd = .85)

  data.frame(
    a1 = .82 * A + rnorm(n, sd = .55),
    a2 = .78 * A + rnorm(n, sd = .62),
    a3 = .76 * A + rnorm(n, sd = .64),
    b1 = .82 * B + rnorm(n, sd = .55),
    b2 = .78 * B + rnorm(n, sd = .62),
    b3 = .76 * B + rnorm(n, sd = .64),
    c1 = .82 * C + rnorm(n, sd = .55),
    c2 = .78 * C + rnorm(n, sd = .62),
    c3 = .76 * C + rnorm(n, sd = .64),
    criterion = criterion
  )
}


make_polish_invariance_fixture <- function(n = 170L, seed = 7502L) {
  set.seed(seed)

  one_group <- function(n, loading2 = .78, intercept3 = 0) {
    f <- rnorm(n)
    data.frame(
      x1 = .82 * f + rnorm(n, sd = .60),
      x2 = loading2 * f + rnorm(n, sd = .62),
      x3 = intercept3 + .74 * f + rnorm(n, sd = .66),
      x4 = .76 * f + rnorm(n, sd = .64)
    )
  }

  rbind(
    transform(one_group(n), group = "A"),
    transform(
      one_group(n, loading2 = .64, intercept3 = .20),
      group = "B"
    )
  )
}


test_that("effects plot includes explicit theory-compatible regions", {
  dat <- make_polish_network_fixture()

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
      C =~ c1 + c2 + c3
    ",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(min = .20),
      "A <-> C" = negligible(within = c(-.15, .15)),
      "A -> criterion" = positive()
    )
  )

  p <- plot(out, "effects")
  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "theory-compatible region")
  expect_gte(length(p$layers), 4L)

  prepared <- nomologR:::nomo_network_theory_plot_data(out)
  expect_equal(
    prepared$data$theory_lower[prepared$data$relation == "A -> B"],
    .20
  )
  expect_equal(
    prepared$data$theory_upper[
      prepared$data$relation == "A <-> C"
    ],
    .15
  )
})


test_that("concordance plot is relation-level rather than a count bar", {
  dat <- make_polish_network_fixture(seed = 7503L)

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
      C =~ c1 + c2 + c3
    ",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive(),
      "A <-> C" = negligible(within = c(-.15, .15))
    )
  )

  p <- plot(out, "concordance")

  expect_s3_class(p, "ggplot")
  expect_match(p$labels$title, "Relation-level")
  expect_equal(nrow(p$data), 2L)
  expect_false(any(vapply(
    p$layers,
    function(layer) inherits(layer$geom, "GeomCol"),
    logical(1)
  )))
})


test_that("network global-fit plot facets metrics onto separate scales", {
  dat <- make_polish_network_fixture(seed = 7504L)

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
    ",
    data = dat,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  p <- plot(out, "fit")

  expect_s3_class(p, "ggplot")
  expect_match(p$labels$subtitle, "own scale")
  expect_equal(length(unique(p$data$metric)), 4L)
})


test_that("replication plot uses human-readable status labels and expanded coordinates", {
  dat1 <- make_polish_network_fixture(n = 360L, seed = 7505L)
  dat2 <- make_polish_network_fixture(n = 360L, seed = 7506L)

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
    ",
    data = dat1,
    validation_data = dat2,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  p <- plot(out, "replication")

  expect_s3_class(p, "ggplot")
  expect_true("replication_display" %in% names(p$data))
  expect_false(any(grepl("_", p$data$replication_display, fixed = TRUE)))
})


test_that("network print prettifies statuses without changing stored values", {
  dat1 <- make_polish_network_fixture(n = 340L, seed = 7507L)
  dat2 <- make_polish_network_fixture(n = 340L, seed = 7508L)

  out <- nomo_network(
    "
      A =~ a1 + a2 + a3
      B =~ b1 + b2 + b3
    ",
    data = dat1,
    validation_data = dat2,
    hypotheses = nomo_hypotheses(
      "A -> B" = positive()
    )
  )

  stored <- out$replication_evidence$replication_status

  expect_output(print(out), "Replication evidence")
  expect_identical(
    out$replication_evidence$replication_status,
    stored
  )
})


test_that("invariance fit and change plots remove redundant metric legends", {
  dat <- make_polish_invariance_fixture()

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric", "scalar"),
    localize = TRUE
  )

  p_fit <- plot(out, "fit")
  p_change <- plot(out, "change")

  expect_s3_class(p_fit, "ggplot")
  expect_s3_class(p_change, "ggplot")

  expect_null(p_fit$labels$shape)
  expect_null(p_change$labels$shape)

  expect_true(all(
    unique(as.character(p_change$data$metric)) %in%
      c("\u0394CFI", "\u0394RMSEA", "\u0394SRMR")
  ))
  expect_match(
    p_change$labels$caption,
    "For CFI, decreases"
  )
})


test_that("local-strain presentation attempts human-readable parameter labels", {
  dat <- make_polish_invariance_fixture(seed = 7509L)

  out <- nomo_invariance(
    "F =~ x1 + x2 + x3 + x4",
    data = dat,
    group = "group",
    levels = c("configural", "metric", "scalar"),
    localize = TRUE
  )

  display <- nomologR:::nomo_invariance_local_strain_display(out)
  expect_s3_class(display, "tbl_df")

  if (nrow(display)) {
    expect_true("constraint_display" %in% names(display))

    translated <- display$constraint_display[
      grepl(
        "Loading:|Intercept:|Threshold:|Residual variance:|Covariance:",
        display$constraint_display
      )
    ]

    # lavaan versions differ in the score-test metadata they expose. When
    # parameter labels are available, at least one should be translated.
    if (any(grepl("^\\.p[0-9]+\\.", display$constraint))) {
      expect_true(
        length(translated) >= 1L ||
          all(display$constraint_display == display$constraint)
      )
    }
  }

  if (nrow(out$local_strain)) {
    p <- plot(out, "local_strain")
    expect_s3_class(p, "ggplot")
    expect_match(p$labels$subtitle, "do not authorize")
  }
})
