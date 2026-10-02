test_that("expectation helpers encode direction and SESOI information", {
  p <- positive()
  expect_s3_class(p, "nomo_expectation")
  expect_equal(p$prediction, "positive")
  expect_equal(p$lower, 0)
  expect_true(is.infinite(p$upper))
  expect_false(p$lower_inclusive)
  expect_true(p$confirmable)

  p20 <- positive(min = .20)
  expect_equal(p20$lower, .20)
  expect_true(p20$lower_inclusive)
  expect_true(p20$magnitude_specified)

  n20 <- negative(max = -.20)
  expect_equal(n20$upper, -.20)
  expect_true(n20$upper_inclusive)

  null_unquantified <- negligible()
  expect_false(null_unquantified$confirmable)
  expect_false(null_unquantified$magnitude_specified)

  null_quantified <- negligible(within = c(-.10, .10))
  expect_true(null_quantified$confirmable)
  expect_equal(c(null_quantified$lower, null_quantified$upper), c(-.10, .10))
})


test_that("expectation helpers reject incoherent regions", {
  expect_error(positive(min = 0), "greater than zero")
  expect_error(positive(min = .30, max = .20), "greater than")
  expect_error(negative(max = 0), "less than zero")
  expect_error(negative(min = -.10, max = -.20), "greater than")
  expect_error(negligible(within = c(.10, .20)), "include zero")
  expect_error(negligible(within = c(.10, -.10)), "ordered")
})


test_that("nomo_hypotheses creates a machine-readable theory table", {
  h <- nomo_hypotheses(
    "A -> B" = positive(min = .20),
    "A -> C" = negligible(within = c(-.10, .10)),
    "B <-> C" = negative()
  )

  expect_s3_class(h, "nomo_hypotheses")
  expect_equal(h$n, 3L)
  expect_equal(h$hypotheses$id, c("H1", "H2", "H3"))
  expect_equal(
    h$hypotheses$relation_type,
    c("directed", "directed", "association")
  )
  expect_equal(
    h$hypotheses$prediction,
    c("positive", "negligible", "negative")
  )
  expect_true(all(h$hypotheses$scale == "standardized"))
  expect_true(all(h$hypotheses$origin == "a_priori"))
})


test_that("post-hoc origin and unstandardized predictions remain explicit", {
  h <- nomo_hypotheses(
    "X -> Y" = positive(
      min = 2,
      scale = "unstandardized",
      origin = "post_hoc"
    )
  )

  expect_equal(h$hypotheses$scale, "unstandardized")
  expect_equal(h$hypotheses$origin, "post_hoc")
})


test_that("nomo_hypotheses refuses malformed or duplicate logical relations", {
  expect_error(nomo_hypotheses(), "at least one")
  expect_error(nomo_hypotheses(positive()), "must be named")
  expect_error(
    nomo_hypotheses("A B" = positive()),
    "must use"
  )
  expect_error(
    nomo_hypotheses("A -> A" = positive()),
    "different nodes"
  )
  expect_error(
    nomo_hypotheses(
      "A <-> B" = positive(),
      "B <-> A" = negative()
    ),
    "same logical relation"
  )
})


test_that("nomo_hypotheses refuses a path beside an association for one pair (#145)", {
  expect_error(
    nomo_hypotheses(
      "A -> B" = positive(),
      "C -> D" = positive(),
      "B <-> A" = positive()
    ),
    "Hypotheses `A -> B` and `B <-> A` give the same two variables both a directed path and an association",
    fixed = TRUE
  )
  # The pair is named in the order it was written, whichever relation is first.
  expect_error(
    nomo_hypotheses(
      "C <-> D" = positive(),
      "A <-> B" = positive(),
      "B -> A" = positive()
    ),
    "Hypotheses `A <-> B` and `B -> A` give the same two variables both",
    fixed = TRUE
  )

  # Directed paths in both directions are a reciprocal pair, which a model can
  # identify. They are accepted here and left to nomo_network(), which sees
  # the model.
  reciprocal <- nomo_hypotheses(
    "A -> B" = positive(),
    "B -> A" = positive()
  )
  expect_equal(reciprocal$n, 2L)
  expect_identical(reciprocal$hypotheses$relation_type, c("directed", "directed"))

  # An association beside them is still refused.
  expect_error(
    nomo_hypotheses(
      "A -> B" = positive(),
      "B -> A" = positive(),
      "A <-> B" = positive()
    ),
    "Hypotheses `A -> B` and `A <-> B` give the same two variables both",
    fixed = TRUE
  )

  # Relations that share one variable are separate pairs.
  shared <- nomo_hypotheses(
    "A -> B" = positive(),
    "A <-> C" = positive(),
    "C -> B" = positive()
  )
  expect_equal(shared$n, 3L)
})


test_that("hypothesis print and summary methods are stable", {
  h <- nomo_hypotheses(
    "A -> B" = positive(),
    "A -> C" = negligible()
  )

  expect_output(print(h), "theory-specified")
  expect_output(print(h), "cannot be confirmed")

  # The abbreviation is spelled out where a reader first meets it (#145).
  printed <- gsub("\\s+", " ", paste(capture.output(print(h)), collapse = " "))
  expect_match(printed, "smallest effect size of interest (SESOI) region", fixed = TRUE)

  s <- summary(h)
  expect_s3_class(s, "summary_nomo_hypotheses")
  expect_equal(s$n, 2L)
  expect_equal(s$quantitatively_confirmable, 1L)
  expect_output(print(s), "A priori")
})


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("M6 and M7 exported entry points are implemented", {
  expect_true(is.function(nomo_hypotheses))
  expect_true(is.function(nomo_network))
  expect_true(is.function(nomo_invariance))
  expect_true(is.function(nomo_partial))
  expect_true(is.function(nomo_table))
})


test_that("hypothesis language preserves researcher provenance", {
  h <- nomo_hypotheses(
    "A -> B" = positive(origin = "a_priori"),
    "A <-> C" = negligible(
      within = c(-.10, .10),
      origin = "post_hoc"
    )
  )

  tab <- nomo_table(h)

  expect_equal(tab$origin, c("a_priori", "post_hoc"))
  expect_equal(tab$confirmable, c(TRUE, TRUE))
})


test_that("closeout: small public validators cover remaining malformed inputs", {
  expect_error(
    nomologR:::nomo_expectation_scalar(Inf, "demo"),
    "finite numeric"
  )
  expect_error(positive(max = 0), "greater than zero")
  expect_error(negative(min = 0), "less than zero")
  expect_error(negligible(within = c(0, Inf)), "two finite numeric bounds")

  expect_error(nomologR:::nomo_parse_relation(NULL), "non-empty relation")
  expect_error(nomologR:::nomo_parse_relation("A -> B -> C"), "exactly two")
  expect_error(
    nomo_hypotheses(
      "A -> B" = positive(),
      "A -> B" = negative()
    ),
    "unique"
  )
  expect_error(
    nomo_hypotheses("A -> B" = 1),
    "positive"
  )

  expect_error(nomo_partial(1, "F =~ x2", "why"), "`level`")
  expect_error(nomo_partial("metric", 1, "why"), "`syntax`")
  expect_error(nomo_partial("metric", "F =~ x2", 1), "`rationale`")
  expect_error(
    nomo_partial(
      level = c("metric", "scalar"),
      syntax = c("F =~ x2", "x3 ~ 1", "x4 ~ 1"),
      rationale = "why"
    ),
    "length 1 or a common length"
  )
  expect_error(
    nomo_partial(
      level = "metric",
      syntax = c("F =~ x2", "F =~ x3", "F =~ x4"),
      rationale = c("a", "b")
    ),
    "match the number"
  )

  dup <- list(c("x1", "x2"), c("x3", "x4"))
  names(dup) <- c("F", "F")
  expect_error(nomo_model(dup), "unique, non-empty factor names")
})
