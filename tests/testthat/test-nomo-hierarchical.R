# Fixtures ---------------------------------------------------------------------

hier_items <- paste0("x", 1:12)
hier_groups <- list(
  A = paste0("x", 1:4),
  B = paste0("x", 5:8),
  C = paste0("x", 9:12)
)
hier_grp <- rep(1:3, each = 4)

hier_bifactor_population <- function() {
  lam_g <- c(.70, .60, .65, .55, .60, .50, .70, .60, .50, .60, .55, .65)
  lam_s <- c(.40, .50, .30, .45, .50, .35, .40, .30, .45, .40, .50, .35)
  L <- cbind(lam_g, sapply(1:3, function(k) ifelse(hier_grp == k, lam_s, 0)))
  sigma <- L %*% t(L)
  diag(sigma) <- 1
  dimnames(sigma) <- list(hier_items, hier_items)
  list(sigma = sigma, lam_g = lam_g, lam_s = lam_s)
}

hier_higher_population <- function() {
  lam1 <- c(.70, .65, .60, .75, .70, .60, .65, .70, .55, .70, .65, .60)
  gam <- c(.80, .70, .60)
  F <- sapply(1:3, function(k) ifelse(hier_grp == k, lam1, 0))
  phi <- gam %*% t(gam)
  diag(phi) <- 1
  sigma <- F %*% phi %*% t(F)
  diag(sigma) <- 1
  dimnames(sigma) <- list(hier_items, hier_items)
  list(sigma = sigma, lam1 = lam1, gam = gam)
}

hier_population_fit <- function(model, sigma) {
  lavaan::cfa(
    as.character(model),
    sample.cov = sigma,
    sample.nobs = 100000,
    std.lv = TRUE,
    sample.cov.rescale = FALSE
  )
}

hier_sample <- function(sigma, n, seed) {
  set.seed(seed)
  z <- matrix(stats::rnorm(n * ncol(sigma)), n, ncol(sigma))
  dat <- as.data.frame(z %*% chol(sigma))
  names(dat) <- colnames(sigma)
  dat
}

hier_index <- function(h, name) h$indices$estimate[h$indices$index == name]

hier_semtools_value <- function(x) as.numeric(unlist(x))[1L]


# nomo_model() -----------------------------------------------------------------

test_that("the correlated structure is unchanged", {
  m <- nomo_model(hier_groups)
  expect_equal(
    as.character(m),
    paste(
      "A =~ x1 + x2 + x3 + x4",
      "B =~ x5 + x6 + x7 + x8",
      "C =~ x9 + x10 + x11 + x12",
      sep = "\n"
    )
  )
  expect_equal(attr(m, "structure"), "correlated")
  expect_null(attr(m, "general"))
  expect_length(attr(m, "notes"), 0L)
  expect_equal(nomo_model(hier_groups, structure = "correlated"), m)
})


test_that("higher-order syntax states identification and flags three factors", {
  m <- nomo_model(hier_groups, structure = "higher_order", general = "G")
  syntax <- strsplit(as.character(m), "\n", fixed = TRUE)[[1L]]

  expect_true("G =~ NA*A + B + C" %in% syntax)
  expect_true("G ~~ 1*G" %in% syntax)
  expect_equal(attr(m, "structure"), "higher_order")
  expect_equal(attr(m, "general"), "G")
  expect_match(attr(m, "notes"), "just\\s+identified")
  expect_match(attr(m, "notes"), "correlated-factors")

  four <- c(hier_groups, list(D = c("y1", "y2", "y3")))
  expect_length(attr(nomo_model(four, structure = "higher_order"), "notes"), 0L)
})


test_that("bifactor syntax writes orthogonality and identification explicitly", {
  m <- nomo_model(hier_groups, structure = "bifactor", general = "G")
  syntax <- strsplit(as.character(m), "\n", fixed = TRUE)[[1L]]

  expect_true(paste0("G =~ NA*", paste(hier_items, collapse = " + ")) %in% syntax)
  expect_true("A =~ NA*x1 + x2 + x3 + x4" %in% syntax)
  expect_true(all(c("G ~~ 1*G", "A ~~ 1*A", "B ~~ 1*B", "C ~~ 1*C") %in% syntax))
  expect_true("G ~~ 0*A + 0*B + 0*C" %in% syntax)
  expect_true("A ~~ 0*B + 0*C" %in% syntax)
  expect_true("B ~~ 0*C" %in% syntax)
  expect_length(attr(m, "notes"), 0L)

  # Identified the same way whatever std.lv is.
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 101)
  a <- lavaan::cfa(as.character(m), data = dat, std.lv = FALSE)
  b <- lavaan::cfa(as.character(m), data = dat, std.lv = TRUE)
  expect_equal(
    lavaan::fitMeasures(a, "chisq"),
    lavaan::fitMeasures(b, "chisq"),
    tolerance = 1e-6
  )
})


test_that("fragile bifactor configurations are noted, impossible ones refused", {
  two <- nomo_model(hier_groups[1:2], structure = "bifactor")
  expect_match(attr(two, "notes"), "two group factors")

  two_item <- nomo_model(
    list(A = c("a1", "a2"), B = c("b1", "b2", "b3"), C = c("c1", "c2", "c3")),
    structure = "bifactor"
  )
  expect_match(attr(two_item, "notes"), "two indicators \\(A\\)")

  expect_error(
    nomo_model(hier_groups["A"], structure = "bifactor"),
    "at least two group factors"
  )
  expect_error(
    nomo_model(list(A = "a1", B = c("b1", "b2")), structure = "bifactor"),
    "at least two indicators; A has one"
  )
  expect_error(
    nomo_model(list(A = c("a1", "a2"), B = c("a2", "b2")), structure = "bifactor"),
    "only one group factor"
  )
})


test_that("higher-order models need three first-order factors", {
  expect_error(
    nomo_model(hier_groups[1:2], structure = "higher_order"),
    "at least three first-order factors"
  )
  expect_error(
    nomo_model(
      list(A = c("a1", "a2"), B = c("a2", "b2"), C = c("c1", "c2")),
      structure = "higher_order"
    ),
    "only one first-order factor"
  )
})


test_that("the general factor name must not collide", {
  expect_error(nomo_model(hier_groups, "bifactor", general = "A"), "already a factor")
  expect_error(nomo_model(hier_groups, "bifactor", general = "x1"), "is an indicator")
  expect_error(nomo_model(hier_groups, "bifactor", general = ""), "single, non-empty")
})


test_that("printed syntax includes identification notes", {
  m <- nomo_model(hier_groups, structure = "higher_order")
  out <- paste(capture.output(print(m)), collapse = "\n")
  expect_match(out, "G =~ NA*A + B + C", fixed = TRUE)
  expect_match(out, "Identification notes:")
})


# Indices against exact population values --------------------------------------

test_that("bifactor indices reproduce the population values exactly", {
  pop <- hier_bifactor_population()
  fit <- hier_population_fit(nomo_model(hier_groups, "bifactor"), pop$sigma)
  h <- nomo_hierarchical(fit, obs.var = FALSE)

  sigma <- pop$sigma
  lam_g <- pop$lam_g
  lam_s <- pop$lam_s
  total <- sum(sigma)

  expect_equal(hier_index(h, "omega_hierarchical"), sum(lam_g)^2 / total, tolerance = 1e-6)
  expect_equal(
    hier_index(h, "omega_total"),
    (sum(lam_g)^2 + sum(sapply(1:3, function(k) sum(lam_s[hier_grp == k])^2))) / total,
    tolerance = 1e-6
  )
  expect_equal(hier_index(h, "ecv"), sum(lam_g^2) / sum(lam_g^2 + lam_s^2), tolerance = 1e-6)
  expect_equal(hier_index(h, "puc"), 1 - 3 * choose(4, 2) / choose(12, 2))
  expect_equal(
    hier_index(h, "omega_hierarchical_relative"),
    hier_index(h, "omega_hierarchical") / hier_index(h, "omega_total")
  )

  expected_hs <- sapply(1:3, function(k) {
    sum(lam_s[hier_grp == k])^2 / sum(sigma[hier_grp == k, hier_grp == k])
  })
  expect_equal(h$subscales$omega_hierarchical_subscale, expected_hs, tolerance = 1e-6)

  expect_equal(h$loadings$general_loading, lam_g, tolerance = 1e-6)
  expect_equal(h$loadings$group_loading, lam_s, tolerance = 1e-6)
  expect_equal(h$loadings$item_ecv, lam_g^2 / (lam_g^2 + lam_s^2), tolerance = 1e-6)

  # Table columns carry values, not stray names.
  for (col in names(h$subscales)) expect_null(names(h$subscales[[col]]), info = col)
  for (col in names(h$loadings)) expect_null(names(h$loadings[[col]]), info = col)
})


test_that("higher-order indices reproduce the Schmid-Leiman decomposition", {
  pop <- hier_higher_population()
  fit <- hier_population_fit(nomo_model(hier_groups, "higher_order"), pop$sigma)
  h <- nomo_hierarchical(fit, obs.var = FALSE)

  general <- pop$lam1 * pop$gam[hier_grp]
  group <- pop$lam1 * sqrt(1 - pop$gam[hier_grp]^2)

  expect_equal(h$structure, "higher_order")
  expect_equal(h$loadings$general_loading, general, tolerance = 1e-6)
  expect_equal(h$loadings$group_loading, group, tolerance = 1e-6)
  expect_equal(hier_index(h, "omega_hierarchical"), sum(general)^2 / sum(pop$sigma), tolerance = 1e-6)
  expect_equal(hier_index(h, "ecv"), sum(general^2) / sum(general^2 + group^2), tolerance = 1e-6)
})


# Indices against semTools -----------------------------------------------------

test_that("bifactor omegas match semTools::compRelSEM() under both denominators", {
  skip_if_not_installed("semTools")
  dat <- hier_sample(hier_bifactor_population()$sigma, 700, 202)
  fit <- lavaan::cfa(as.character(nomo_model(hier_groups, "bifactor")), data = dat)

  for (ov in c(TRUE, FALSE)) {
    h <- nomo_hierarchical(fit, obs.var = ov)

    reference_h <- semTools::compRelSEM(fit, true = "omegaH", obs.var = ov)
    expect_equal(
      hier_index(h, "omega_hierarchical"),
      hier_semtools_value(reference_h$G),
      tolerance = 1e-6,
      info = paste("obs.var =", ov)
    )

    w <- "Acomp <~ x1 + x2 + x3 + x4"
    ref_hs <- semTools::compRelSEM(fit, W = w, true = list(Acomp = "A"), obs.var = ov)
    ref_s <- semTools::compRelSEM(fit, W = w, true = list(Acomp = c("G", "A")), obs.var = ov)
    a <- h$subscales[h$subscales$subscale == "A", ]
    expect_equal(a$omega_hierarchical_subscale, hier_semtools_value(ref_hs), tolerance = 1e-6)
    expect_equal(a$omega_subscale, hier_semtools_value(ref_s), tolerance = 1e-6)

    ref_total <- semTools::compRelSEM(fit, obs.var = ov)
    expect_equal(hier_index(h, "omega_total"), hier_semtools_value(ref_total$G), tolerance = 1e-6)
  }
})


test_that("higher-order omega matches semTools::compRelSEM()", {
  skip_if_not_installed("semTools")
  dat <- hier_sample(hier_higher_population()$sigma, 700, 303)
  fit <- lavaan::cfa(as.character(nomo_model(hier_groups, "higher_order")), data = dat)

  for (ov in c(TRUE, FALSE)) {
    h <- nomo_hierarchical(fit, obs.var = ov)
    w <- paste("TOT <~", paste(hier_items, collapse = " + "))
    reference <- semTools::compRelSEM(fit, W = w, true = list(TOT = "G"), obs.var = ov)
    expect_equal(
      hier_index(h, "omega_hierarchical"),
      hier_semtools_value(reference),
      tolerance = 1e-6,
      info = paste("obs.var =", ov)
    )
  }
})


test_that("ordered indicators report the latent-response estimand", {
  skip_if_not_installed("semTools")
  dat <- hier_sample(hier_bifactor_population()$sigma, 800, 404)
  ord <- as.data.frame(lapply(dat, function(x) ordered(cut(x, c(-Inf, -1, 0, 1, Inf)))))
  fit <- nomo_cfa(nomo_model(hier_groups, "bifactor"), data = ord, ordered = hier_items)

  h <- nomo_hierarchical(fit, obs.var = FALSE)
  reference <- semTools::compRelSEM(fit$fit, true = "omegaH", obs.var = FALSE, ord.scale = FALSE)

  expect_equal(h$estimand, "latent_response")
  expect_equal(hier_index(h, "omega_hierarchical"), hier_semtools_value(reference$G), tolerance = 1e-6)
  expect_true(any(grepl("latent-response", h$notes$note)))
  expect_true(any(grepl("upper bound", h$notes$note)))
})


# Structure detection and refusals ---------------------------------------------

test_that("structure is read from hand-written syntax too", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 505)
  hand <- paste(
    "Gen =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12",
    "A =~ x1 + x2 + x3 + x4",
    "B =~ x5 + x6 + x7 + x8",
    "C =~ x9 + x10 + x11 + x12",
    sep = "\n"
  )
  fit <- lavaan::cfa(hand, data = dat, orthogonal = TRUE, std.lv = TRUE)
  h <- nomo_hierarchical(fit)

  expect_equal(h$structure, "bifactor")
  expect_equal(h$general, "Gen")
  expect_equal(names(h$groups), c("A", "B", "C"))
  expect_equal(h$source, "lavaan")
})


test_that("items loading only on the general factor are allowed and noted", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 606)
  syntax <- paste(
    "G =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12",
    "A =~ x1 + x2 + x3 + x4",
    "B =~ x5 + x6 + x7 + x8",
    sep = "\n"
  )
  fit <- lavaan::cfa(syntax, data = dat, orthogonal = TRUE, std.lv = TRUE)
  h <- nomo_hierarchical(fit)

  expect_true(all(is.na(h$loadings$subscale[9:12])))
  expect_equal(h$loadings$group_loading[9:12], rep(0, 4))
  expect_true(any(grepl("general factor only", h$notes$note)))
  expect_equal(hier_index(h, "puc"), 1 - 2 * choose(4, 2) / choose(12, 2))
})


test_that("models that are not hierarchical are refused with a pointer", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 707)

  correlated <- nomo_cfa(nomo_model(hier_groups), data = dat)
  expect_error(nomo_hierarchical(correlated), "neither a higher-order model")
  expect_error(nomo_hierarchical(correlated), "nomo_reliability")

  single <- nomo_cfa(
    paste("F =~", paste(hier_items, collapse = " + ")),
    data = dat
  )
  expect_error(nomo_hierarchical(single), "no group factors")

  expect_error(nomo_hierarchical(list()), "nomo_cfa")
  expect_error(nomo_hierarchical(correlated, obs.var = NA), "TRUE or FALSE")
  expect_error(nomo_hierarchical(correlated, general = c("a", "b")), "single factor name")
})


test_that("correlated general and group factors are refused", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 808)
  syntax <- paste(
    "G =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12",
    "A =~ x1 + x2 + x3 + x4",
    "B =~ x5 + x6 + x7 + x8",
    "C =~ x9 + x10 + x11 + x12",
    "A ~~ B",
    "G ~~ 0*A + 0*B + 0*C",
    "A ~~ 0*C",
    "B ~~ 0*C",
    sep = "\n"
  )
  fit <- suppressWarnings(lavaan::cfa(syntax, data = dat, std.lv = TRUE))
  skip_if_not(isTRUE(lavaan::lavInspect(fit, "converged")))
  expect_error(nomo_hierarchical(fit), "not orthogonal")
})


test_that("the general argument is checked against the model", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 909)
  bf <- nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat)
  expect_equal(nomo_hierarchical(bf, general = "G")$general, "G")
  expect_error(nomo_hierarchical(bf, general = "Nope"), "not a factor")
  expect_error(nomo_hierarchical(bf, general = "A"), "does not load on every item")

  ho <- nomo_cfa(
    nomo_model(hier_groups, "higher_order"),
    data = hier_sample(hier_higher_population()$sigma, 600, 910)
  )
  expect_error(nomo_hierarchical(ho, general = "A"), "not the second-order factor")
})


test_that("multi-group models are refused", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 800, 911)
  dat$grp <- rep(c("a", "b"), 400)
  fit <- lavaan::cfa(
    as.character(nomo_model(hier_groups, "bifactor")),
    data = dat,
    group = "grp"
  )
  expect_error(nomo_hierarchical(fit), "single-group, single-level")
})


# Notes ------------------------------------------------------------------------

test_that("identification and model-choice notes are recorded", {
  dat <- hier_sample(hier_higher_population()$sigma, 600, 1001)
  h <- nomo_hierarchical(nomo_cfa(nomo_model(hier_groups, "higher_order"), data = dat))

  expect_true("identification" %in% h$notes$topic)
  expect_match(h$notes$note[h$notes$topic == "identification"], "just\\s+identified")
  expect_match(h$notes$note[h$notes$topic == "model_choice"], "Reise, 2012")
  expect_match(h$notes$note[h$notes$topic == "structure"], "Schmid-Leiman")
  expect_true(all(c("identification", "model_choice") %in%
                    h$decision_log$metric[h$decision_log$severity == "review"]))
})


test_that("the reliability refusal points to nomo_hierarchical()", {
  dat <- hier_sample(hier_higher_population()$sigma, 600, 1002)
  ho <- nomo_cfa(nomo_model(hier_groups, "higher_order"), data = dat)
  expect_error(nomo_reliability(ho), "nomo_hierarchical")

  bf <- nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat)
  expect_error(nomo_reliability(bf), "nomo_hierarchical")
})


# The caution the documentation makes ------------------------------------------

test_that("a bifactor model fits higher-order data at least as well", {
  dat <- hier_sample(hier_higher_population()$sigma, 600, 1101)
  correlated <- nomo_cfa(nomo_model(hier_groups), data = dat)
  higher <- nomo_cfa(nomo_model(hier_groups, "higher_order"), data = dat)
  bifactor <- nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat)

  chisq <- function(x) unname(lavaan::fitMeasures(x$fit, "chisq"))
  df <- function(x) unname(lavaan::fitMeasures(x$fit, "df"))

  # The data came from a higher-order model, yet the bifactor model fits at
  # least as well, because the higher-order model is nested within it.
  expect_lte(chisq(bifactor), chisq(higher))

  # With three first-order factors the two non-bifactor models are equivalent.
  expect_equal(chisq(higher), chisq(correlated), tolerance = 1e-6)
  expect_equal(df(higher), df(correlated))

  cmp <- nomo_compare(
    correlated, higher, bifactor,
    rationale = "Compare competing structures for the same twelve items.",
    origin = "a_priori"
  )
  expect_s3_class(cmp, "nomo_compare")
  # Against the correlated-factors reference: the higher-order model is
  # equivalent, and the bifactor model is a less constrained nesting model.
  expect_setequal(cmp$comparisons$relation, c("equivalent", "less_constrained"))
})


# Presentation -----------------------------------------------------------------

test_that("print, summary, plot, and nomo_table present the evidence", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 1201)
  h <- nomo_hierarchical(nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat))

  printed <- paste(capture.output(print(h)), collapse = "\n")
  expect_match(printed, "Bifactor model | general factor: G", fixed = TRUE)
  expect_match(printed, "omega_hierarchical")
  expect_match(printed, "No index is treated as a pass/fail threshold")

  s <- summary(h)
  expect_s3_class(s, "summary_nomo_hierarchical")
  summary_text <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(summary_text, "no benchmark value establishes")

  expect_s3_class(plot(h), "ggplot")
  expect_s3_class(plot(h, type = "loadings"), "ggplot")

  expect_equal(nomo_table(h), h$indices)
  expect_equal(nomo_table(h, "subscales"), h$subscales)
  expect_equal(nomo_table(h, "loadings"), h$loadings)
  expect_equal(nomo_table(h, "notes"), h$notes)
  expect_equal(nomo_table(h, "decision_log"), h$decision_log)

  # Plot labels stay ASCII so every graphics device can render them.
  labels <- unlist(plot(h)$labels)
  expect_false(any(grepl("[^\x01-\x7F]", labels)))
})


test_that("the variance decomposition shares sum to one", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 1202)
  h <- nomo_hierarchical(nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat))
  shares <- plot(h)$data
  totals <- as.numeric(tapply(shares$share, shares$composite, sum))
  expect_equal(totals, rep(1, length(totals)), tolerance = 1e-10)
})


# Methods registry -------------------------------------------------------------

test_that("nomo_methods() credits hierarchical methods only where used", {
  dat <- hier_sample(hier_higher_population()$sigma, 600, 1301)
  correlated <- nomo_cfa(nomo_model(hier_groups), data = dat)
  higher <- nomo_cfa(nomo_model(hier_groups, "higher_order"), data = dat)
  bifactor <- nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat)

  expect_false(any(c("bifactor_model", "higher_order_model") %in% nomo_methods(correlated)$id))
  expect_true("higher_order_model" %in% nomo_methods(higher)$id)
  expect_true("bifactor_model" %in% nomo_methods(bifactor)$id)

  h_bf <- nomo_methods(nomo_hierarchical(bifactor))$id
  expect_true(all(c("omega_hierarchical", "omega_hierarchical_subscale", "ecv", "puc") %in% h_bf))
  expect_false("schmid_leiman" %in% h_bf)

  h_ho <- nomo_methods(nomo_hierarchical(higher))$id
  expect_true(all(c("higher_order_model", "schmid_leiman") %in% h_ho))
})
