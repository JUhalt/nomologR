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
  # Two group factors of four indicators each are identified but fragile.
  two <- nomo_model(hier_groups[1:2], structure = "bifactor")
  expect_match(attr(two, "notes"), "two group factors")
  expect_identical(names(attr(two, "notes")), "review")

  # Accepted but not identified without a constraint: noted as concerns (#145).
  two_item <- nomo_model(
    list(A = c("a1", "a2"), B = c("b1", "b2", "b3"), C = c("c1", "c2", "c3")),
    structure = "bifactor"
  )
  expect_identical(names(attr(two_item, "notes")), "concern")
  expect_match(attr(two_item, "notes"), "Group factor A has two indicators, so the model is not identified",
               fixed = TRUE)
  expect_match(attr(two_item, "notes"), "only through their product", fixed = TRUE)
  expect_no_match(attr(two_item, "notes"), "weakly identified", fixed = TRUE)

  three_each <- nomo_model(
    list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3")),
    structure = "bifactor"
  )
  expect_identical(names(attr(three_each, "notes")), "concern")
  expect_match(attr(three_each, "notes"), "not identified without an added constraint", fixed = TRUE)
  expect_no_match(attr(three_each, "notes"), "empirically under-identified", fixed = TRUE)

  # With one group factor of four indicators the model is identified again.
  three_four <- nomo_model(
    list(A = c("a1", "a2", "a3"), B = c("b1", "b2", "b3", "b4")),
    structure = "bifactor"
  )
  expect_identical(names(attr(three_four, "notes")), "review")

  both <- nomo_model(list(A = c("a1", "a2"), B = c("b1", "b2")), structure = "bifactor")
  expect_identical(names(attr(both, "notes")), c("concern", "concern"))
  expect_match(attr(both, "notes")[[2L]], "Group factors A, B have two indicators each", fixed = TRUE)
  expect_output(print(both), "- Concern: Group factors A, B", fixed = TRUE)

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
  expect_match(out, "\nIdentification notes\n", fixed = TRUE)
  expect_match(out, "  - Review: With three first-order factors", fixed = TRUE)

  # Notes without a severity are printed as they are.
  attr(m, "notes") <- unname(attr(m, "notes"))
  expect_output(print(m), "\n  - With three first-order factors", fixed = TRUE)
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
  expect_output(print(h), "latent-response composite (ordered indicators)", fixed = TRUE)
  expect_equal(hier_index(h, "omega_hierarchical"), hier_semtools_value(reference$G), tolerance = 1e-6)
  expect_true(any(grepl("latent-response", h$notes$note)))
  expect_true(any(grepl("upper bound", h$notes$note)))
})


test_that("the delta and theta parameterizations give the same indices (#145)", {
  skip_on_cran()
  dat <- hier_sample(hier_bifactor_population()$sigma, 800, 404)
  ord <- as.data.frame(lapply(dat, function(x) ordered(cut(x, c(-Inf, -1, 0, 1, Inf)))))
  model <- as.character(nomo_model(hier_groups, "bifactor"))
  # Ordered-factor columns are fitted as ordered without being named, and the
  # estimand follows what lavaan fitted.
  delta <- lavaan::cfa(model, data = ord)
  theta <- lavaan::cfa(model, data = ord, parameterization = "theta")

  for (obs_var in c(TRUE, FALSE)) {
    hd <- nomo_hierarchical(delta, obs.var = obs_var)
    ht <- nomo_hierarchical(theta, obs.var = obs_var)
    expect_identical(ht$estimand, "latent_response")
    expect_equal(ht$indices$estimate, hd$indices$estimate, tolerance = 1e-5)
    expect_equal(ht$subscales, hd$subscales, tolerance = 1e-5)
    expect_equal(ht$factors, hd$factors, tolerance = 1e-5)
    expect_equal(ht$loadings, hd$loadings, tolerance = 1e-5)
  }
  expect_lt(hier_index(ht, "omega_total"), 1)

  # The loadings are lavaan's standardized loadings under either one.
  std <- lavaan::standardizedSolution(theta)
  general <- std[std$op == "=~" & std$lhs == "G", ]
  expect_equal(ht$loadings$general_loading, general$est.std[match(ht$loadings$item, general$rhs)],
               tolerance = 1e-5)
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


test_that("nonconverged fits and latent regressions are refused", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 400, 1401)

  nonconverged <- suppressWarnings(lavaan::cfa(
    as.character(nomo_model(hier_groups, "bifactor")),
    data = dat,
    control = list(iter.max = 1L)
  ))
  skip_if(isTRUE(lavaan::lavInspect(nonconverged, "converged")))
  expect_error(nomo_hierarchical(nonconverged), "did not converge")

  regression <- lavaan::sem(
    "A =~ x1 + x2 + x3 + x4\nB =~ x5 + x6 + x7 + x8\nB ~ A",
    data = dat
  )
  expect_error(nomo_hierarchical(regression), "latent regressions")
})


# Structure detection from constructed loading tables. Several of these
# structures would not be identified if fitted, so detection is tested directly.
hier_loadings <- function(...) {
  pairs <- list(...)
  data.frame(
    lhs = unlist(lapply(pairs, `[[`, 1L)),
    rhs = unlist(lapply(pairs, `[[`, 2L)),
    stringsAsFactors = FALSE
  )
}
hier_pairs <- function(lhs, rhs) lapply(rhs, function(r) c(lhs, r))


test_that("higher-order detection refuses unsupported structures", {
  ov <- paste0("x", 1:9)
  first <- c(
    hier_pairs("F1", ov[1:3]), hier_pairs("F2", ov[4:6]), hier_pairs("F3", ov[7:9])
  )
  lv <- c("F1", "F2", "F3", "G")

  two_second <- do.call(hier_loadings, c(first, hier_pairs("G", "F1"), hier_pairs("H", c("F2", "F3"))))
  expect_error(
    nomologR:::nomo_hierarchical_structure_higher(two_second, c(lv, "H"), ov, c("G", "H"), NULL),
    "more than one second-order factor"
  )

  mixed <- do.call(hier_loadings, c(first, hier_pairs("G", c("F1", "F2", "F3", "x1"))))
  expect_error(
    nomologR:::nomo_hierarchical_structure_higher(mixed, lv, ov, "G", NULL),
    "also loads directly on items"
  )

  stray <- do.call(hier_loadings, c(
    first, hier_pairs("F4", c("y1", "y2", "y3")), hier_pairs("G", c("F1", "F2", "F3"))
  ))
  expect_error(
    nomologR:::nomo_hierarchical_structure_higher(
      stray, c(lv, "F4"), c(ov, "y1", "y2", "y3"), "G", NULL
    ),
    "F4 does not"
  )

  cross <- do.call(hier_loadings, c(first, hier_pairs("F2", "x1"), hier_pairs("G", c("F1", "F2", "F3"))))
  expect_error(
    nomologR:::nomo_hierarchical_structure_higher(cross, lv, ov, "G", NULL),
    "more than one first-order factor \\(x1\\)"
  )
})


test_that("bifactor detection refuses ambiguous or overlapping structures", {
  ov <- paste0("x", 1:6)

  two_general <- do.call(hier_loadings, c(
    hier_pairs("G1", ov), hier_pairs("G2", ov),
    hier_pairs("A", ov[1:3]), hier_pairs("B", ov[4:6])
  ))
  expect_error(
    nomologR:::nomo_hierarchical_structure_bifactor(
      two_general, c("G1", "G2", "A", "B"), ov, NULL
    ),
    "More than one factor loads on every item \\(G1, G2\\)"
  )
  # Naming the general factor resolves the ambiguity only if the rest is valid.
  expect_error(
    nomologR:::nomo_hierarchical_structure_bifactor(
      two_general, c("G1", "G2", "A", "B"), ov, "G1"
    ),
    "more than one group factor"
  )

  overlap <- do.call(hier_loadings, c(
    hier_pairs("G", ov), hier_pairs("A", ov[1:4]), hier_pairs("B", ov[4:6])
  ))
  expect_error(
    nomologR:::nomo_hierarchical_structure_bifactor(overlap, c("G", "A", "B"), ov, NULL),
    "more than one group factor \\(x4\\)"
  )
})


test_that("mixed-sign group loadings and improper solutions are noted", {
  structure <- list(
    type = "bifactor",
    general = "G",
    groups = list(A = c("x1", "x2"), B = c("x3", "x4")),
    items = paste0("x", 1:4)
  )
  computed <- list(loadings = tibble::tibble(
    item = paste0("x", 1:4),
    subscale = c("A", "A", "B", "B"),
    general_loading = c(.6, .6, .6, .6),
    group_loading = c(.4, -.3, .4, .3)
  ))
  theta <- diag(c(.2, -.05, .3, .3))
  dimnames(theta) <- list(paste0("x", 1:4), paste0("x", 1:4))
  matrices <- list(theta = theta)
  input <- list(ordered = character())

  notes <- nomologR:::nomo_hierarchical_notes(input, structure, matrices, computed, TRUE)

  mixed <- notes[notes$topic == "group_loadings", ]
  expect_equal(nrow(mixed), 1L)
  expect_equal(mixed$severity, "review")
  expect_match(mixed$note, "Group factor A has loadings of mixed sign", fixed = TRUE)
  expect_match(mixed$brief, "so what it represents beyond the general factor", fixed = TRUE)

  improper <- notes[notes$topic == "improper_solution", ]
  expect_equal(improper$severity, "concern")
  expect_match(improper$note, "Negative residual variance for x2")
})


# Notes ------------------------------------------------------------------------

test_that("identification and model-choice notes are recorded", {
  dat <- hier_sample(hier_higher_population()$sigma, 600, 1001)
  h <- nomo_hierarchical(nomo_cfa(nomo_model(hier_groups, "higher_order"), data = dat))

  expect_output(print(h), "Higher-order model | General factor: G", fixed = TRUE)
  expect_true("identification" %in% h$notes$topic)
  expect_match(h$notes$note[h$notes$topic == "identification"], "just\\s+identified")
  expect_match(h$notes$note[h$notes$topic == "model_choice"], "Reise, 2012")
  expect_match(h$notes$note[h$notes$topic == "structure"], "Schmid-Leiman")
  expect_true("identification" %in% h$decision_log$metric[h$decision_log$severity == "review"])
  # The model-choice caution holds for every model, whatever the data, so it is
  # information, not a flag (#145).
  expect_identical(h$decision_log$severity[h$decision_log$metric == "model_choice"], "info")
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
  skip_on_cran()
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
  expect_match(printed, "Bifactor model | General factor: G", fixed = TRUE)
  expect_match(printed, "Omega hierarchical")
  expect_match(printed, "no value is treated as a pass/fail threshold")

  s <- summary(h)
  expect_s3_class(s, "summary_nomo_hierarchical")
  summary_text <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(summary_text, "no benchmark value establishes")

  expect_s3_class(plot(h), "ggplot")
  expect_s3_class(plot(h, type = "loadings"), "ggplot")

  expect_equal(nomo_table(h), h$indices)
  expect_equal(nomo_table(h, "factors"), h$factors)
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
  # A proper solution draws every composite and loading, with no caption.
  expect_null(plot(h)$labels$caption)
  expect_null(plot(h, type = "loadings")$labels$caption)
})


# Methods registry -------------------------------------------------------------

test_that("nomo_methods() credits hierarchical methods only where used", {
  skip_on_cran()
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


# Factor determinacy and construct replicability (#56) -------------------------

# Standardized loadings from Table 2 of Rodriguez, Reise and Haviland (2016),
# taken from Osman et al. (2009): the MASC, 39 items, one general factor and
# four group factors. The paper publishes the determinacy and replicability
# values these loadings produce, so this table is a reference computation
# rather than a fixture of our own making.
masc_table2 <- function() {
  general <- c(
    .38, .82, .82, .77, .74, .98, .77, .79, .66, .73, .82, .98,
    .24, .56, .06, .51, .46, .68, .59, .71, .60,
    .55, .83, .97, .90, .81, .90, .60, .78, .65,
    .60, .84, .66, .73, .70, .76, .74, .86, .76
  )
  ps <- c(-.13, .10, .38, .35, .28, .02, .28, .32, .39, .19, .42, .11)
  ha <- c(.59, .43, .78, .71, .45, .13, .43, .17, .50)
  sa <- c(.75, .47, .06, .31, .51, .36, .27, .28, .17)
  sp <- c(.24, .36, .26, .09, .10, .28, .16, .11, .41)

  items <- sprintf("i%02d", seq_along(general))
  groups <- list(
    PS = items[1:12], HA = items[13:21], SA = items[22:30], SP = items[31:39]
  )
  group_std <- c(ps, ha, sa, sp)
  group_of <- rep(names(groups), times = lengths(groups))

  names(general) <- items
  names(group_std) <- items
  names(group_of) <- items

  lambda <- cbind(
    GEN = general,
    PS = ifelse(group_of == "PS", group_std, 0),
    HA = ifelse(group_of == "HA", group_std, 0),
    SA = ifelse(group_of == "SA", group_std, 0),
    SP = ifelse(group_of == "SP", group_std, 0)
  )
  implied <- lambda %*% t(lambda)
  diag(implied) <- 1
  dimnames(implied) <- list(items, items)

  list(
    matrices = list(implied = implied),
    structure = list(type = "bifactor", general = "GEN", groups = groups,
                     items = items),
    items = items,
    general_std = general,
    group_std = group_std,
    group_of = group_of
  )
}


test_that("factor determinacy and construct replicability reproduce the published MASC values", {
  masc <- masc_table2()

  out <- nomologR:::nomo_hierarchical_factor_scores(
    matrices = masc$matrices,
    structure = masc$structure,
    items = masc$items,
    general_std = masc$general_std,
    group_std = masc$group_std,
    group_of = masc$group_of
  )

  expect_identical(out$factor, c("GEN", "PS", "HA", "SA", "SP"))
  expect_identical(out$role, c("general", rep("group", 4L)))
  expect_identical(out$n_items, c(39L, 12L, 9L, 9L, 9L))

  # Rodriguez et al. (2016), p. 142: factor determinacy for the general factor
  # and PS, HA, SA, SP.
  expect_equal(round(out$factor_determinacy, 2), c(.99, .86, .92, .95, .80))

  # Same page: the minimum possible correlation between two sets of equally
  # valid factor scores, 2 * rho^2 - 1. The paper squares loadings already
  # rounded to two decimals, so PS differs in the second decimal.
  expect_equal(round(out$min_competing_r, 2), c(.98, .47, .71, .80, .28))

  # Rodriguez et al. (2016), p. 143: H for the general factor and each group.
  expect_equal(round(out$construct_replicability, 2), c(.99, .52, .81, .70, .39))
})


test_that("determinacy and replicability are the same quantity only without group factors", {
  masc <- masc_table2()

  # Hancock and Mueller derived H for a unidimensional construct, where it is
  # the squared correlation between the factor and an optimally weighted
  # composite: exactly what determinacy squares to. Removing the group factors
  # must therefore make the two agree.
  flat <- masc
  flat$structure$groups <- list()
  flat$group_std[] <- 0
  flat$group_of[] <- NA_character_
  lambda <- matrix(flat$general_std, ncol = 1L)
  implied <- lambda %*% t(lambda)
  diag(implied) <- 1
  dimnames(implied) <- list(flat$items, flat$items)
  flat$matrices$implied <- implied

  out <- nomologR:::nomo_hierarchical_factor_scores(
    matrices = flat$matrices,
    structure = flat$structure,
    items = flat$items,
    general_std = flat$general_std,
    group_std = flat$group_std,
    group_of = flat$group_of
  )

  expect_equal(
    out$determinacy_r2[[1L]],
    out$construct_replicability[[1L]],
    tolerance = 1e-10
  )

  # With the group factors present they are not the same quantity, and the
  # difference is not rounding: H cannot see the group factors at all.
  bifactor <- nomologR:::nomo_hierarchical_factor_scores(
    matrices = masc$matrices,
    structure = masc$structure,
    items = masc$items,
    general_std = masc$general_std,
    group_std = masc$group_std,
    group_of = masc$group_of
  )

  expect_equal(bifactor$construct_replicability[[1L]], out$construct_replicability[[1L]])
  expect_lt(bifactor$determinacy_r2[[1L]], out$determinacy_r2[[1L]])
})


test_that("determinacy and replicability are credited only when they produced values", {
  pop <- hier_bifactor_population()
  fit <- hier_population_fit(
    nomo_model(hier_groups, structure = "bifactor", general = "G"),
    pop$sigma
  )

  h <- nomo_hierarchical(fit)
  credited <- nomo_methods_used(h)
  expect_true("factor_determinacy" %in% credited)
  expect_true("construct_replicability" %in% credited)

  # Determinacy does not depend on the omega denominator, because it is defined
  # on the model-reproduced matrix.
  expect_equal(
    nomo_hierarchical(fit, obs.var = FALSE)$factors$factor_determinacy,
    h$factors$factor_determinacy
  )

  # A run that could not produce either value must not be cited for it.
  none <- h
  none$factors$factor_determinacy <- NA_real_
  none$factors$construct_replicability <- NA_real_
  expect_false("factor_determinacy" %in% nomo_methods_used(none))
  expect_false("construct_replicability" %in% nomo_methods_used(none))

  # The registry entries exist and carry their sources.
  reg <- nomo_methods(h)
  expect_true(all(c("factor_determinacy", "construct_replicability") %in% reg$id))
})


test_that("undefined determinacy and replicability are NA with a note, not guesses", {
  masc <- masc_table2()

  # A standardized loading at one leaves no residual for H to divide by.
  heywood <- masc
  heywood$general_std[[1L]] <- 1
  out <- nomologR:::nomo_hierarchical_factor_scores(
    matrices = heywood$matrices,
    structure = heywood$structure,
    items = heywood$items,
    general_std = heywood$general_std,
    group_std = heywood$group_std,
    group_of = heywood$group_of
  )
  expect_true(is.na(out$construct_replicability[[1L]]))

  notes <- nomologR:::nomo_hierarchical_factor_score_notes(
    tibble::tibble(topic = character(), severity = character(), note = character()),
    function(notes, topic, severity, note, brief = note) {
      tibble::add_row(notes, topic = topic, severity = severity, note = note)
    },
    out
  )
  expect_true(any(notes$severity == "concern"))
  expect_match(
    paste(notes$note, collapse = " "),
    "left missing (shown as --) rather than guessed",
    fixed = TRUE
  )
})


# Pre-RC findings (#145) and the shared output style (#144) ---------------------

hier_negative_disturbance_fit <- function() {
  # A's correlations with B and C ask for a loading on the second-order factor
  # above 1: its square is .8 * .8 / .6. A's disturbance variance is therefore
  # negative in the population, and the data reproduce the population
  # covariance matrix exactly, so it is estimated below zero.
  pop <- "
    A =~ .7*x1 + .7*x2 + .7*x3
    B =~ .7*x4 + .7*x5 + .7*x6
    C =~ .7*x7 + .7*x8 + .7*x9
    A ~~ .8*B
    A ~~ .8*C
    B ~~ .6*C
  "
  dat <- nomo_test_simulate(pop, n = 600, seed = 1, exact = TRUE)
  suppressWarnings(lavaan::cfa(
    "A =~ x1 + x2 + x3\nB =~ x4 + x5 + x6\nC =~ x7 + x8 + x9\nG =~ A + B + C",
    data = dat
  ))
}


test_that("a negative disturbance variance gives NA indices and a concern, not a crash (#145)", {
  fit <- hier_negative_disturbance_fit()
  expect_true(lavaan::lavInspect(fit, "converged"))
  expect_lt(lavaan::lavInspect(fit, "est")$psi["A", "A"], 0)

  expect_no_warning(h <- nomo_hierarchical(fit))
  index <- function(name) h$indices$estimate[h$indices$index == name]
  # The total score includes A's negative contribution; the general factor's
  # share does not.
  expect_true(is.na(index("omega_total")))
  expect_true(is.na(index("ecv")))
  expect_true(is.finite(index("omega_hierarchical")))
  expect_true(is.finite(index("puc")))
  expect_true(is.na(h$subscales$omega_subscale[h$subscales$subscale == "A"]))
  expect_true(is.finite(h$subscales$omega_subscale[h$subscales$subscale == "B"]))
  expect_true(all(is.na(h$loadings$group_loading[h$loadings$subscale == "A"])))
  expect_true(is.na(h$factors$construct_replicability[h$factors$factor == "A"]))
  expect_true(is.finite(h$factors$factor_determinacy[h$factors$factor == "G"]))

  improper <- h$notes[h$notes$topic == "improper_solution", ]
  expect_identical(improper$severity, "concern")
  expect_match(improper$note, "Negative disturbance variance for A.", fixed = TRUE)
  expect_match(h$indices$interpretation[h$indices$index == "omega_total"],
               "Not computed: a variance estimate this index depends on is negative", fixed = TRUE)
  expect_match(
    h$decision_log$observation[h$decision_log$metric == "omega_hierarchical_subscale" &
                                 h$decision_log$object == "A"],
    "is not computed", fixed = TRUE
  )

  local_reproducible_output(width = 80)
  printed <- capture.output(print(h))
  expect_true("  Omega total                         --" %in% printed)
  expect_match(printed, "^  - Concern: Negative disturbance variance for A", all = FALSE)
  summarized <- capture.output(print(summary(h)))
  # A page citation is never broken after "p." (#144).
  expect_true(any(grepl("Gorsuch (1983, p. 260)", summarized, fixed = TRUE)))
  expect_false(any(grepl("\\bp\\.$", summarized)))
  expect_true(all(nchar(summarized) <= 80L))
  expect_identical(nomologR:::nomo_hierarchical_bind_pages("pp. 6-47 and p. 3"),
                   "pp.\u00a06-47 and p.\u00a03")
  # The variance plot leaves out a composite it cannot divide, and the
  # loadings plot a loading it cannot standardize; each caption says which.
  p <- plot(h)
  expect_setequal(as.character(unique(p$data$composite)), c("B", "C"))
  expect_identical(
    gsub("\\s+", " ", p$labels$caption),
    "Not drawn: Total score and A, whose shares rest on a negative variance estimate (see x$notes)."
  )
  p <- plot(h, type = "loadings")
  expect_false(anyNA(p$data$loading))
  expect_identical(
    gsub("\\s+", " ", p$labels$caption),
    "Not drawn: group loadings of x1, x2, x3, which rest on a negative variance estimate (see x$notes)."
  )
  # Both sources, and nothing at all to draw.
  both <- h
  both$loadings$general_loading[1:2] <- NA_real_
  expect_match(gsub("\\s+", " ", plot(both, type = "loadings")$labels$caption),
               "Not drawn: general loadings of x1, x2 and group loadings of x1, x2, x3,",
               fixed = TRUE)
  both$loadings$general_loading <- NA_real_
  both$loadings$group_loading <- NA_real_
  expect_error(plot(both, type = "loadings"), "No finite standardized loadings", fixed = TRUE)
  both$indices$estimate[both$indices$index == "omega_hierarchical"] <- NA_real_
  both$subscales$omega_subscale <- NA_real_
  expect_error(plot(both), "No composite's variance shares can be drawn", fixed = TRUE)

  # In a bifactor model a negative factor variance is named as one.
  notes <- nomologR:::nomo_hierarchical_notes(
    list(ordered = character()),
    list(type = "bifactor", general = "G", groups = list(A = c("x1", "x2"), B = c("x3", "x4")),
         items = paste0("x", 1:4)),
    list(theta = diag(4), psi = structure(diag(c(1, -.1, .2)), dimnames = list(c("G", "A", "B"), c("G", "A", "B"))),
         sources = c("G", "A", "B")),
    list(loadings = tibble::tibble(subscale = c("A", "A", "B", "B"),
                                   group_loading = c(NA, NA, .4, -.3)),
         factors = NULL),
    TRUE
  )
  expect_match(notes$note[notes$topic == "improper_solution"], "Negative factor variance for A.",
               fixed = TRUE)
  expect_match(notes$brief[notes$topic == "group_loadings"],
               "Group factor B has loadings of mixed sign", fixed = TRUE)
})


test_that("print gives one sentence per flag and summary the full notes (#145)", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 1201)
  h <- nomo_hierarchical(nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat))
  local_reproducible_output(width = 80)
  printed <- capture.output(print(h))
  expect_identical(printed[[1L]], "<nomo_hierarchical> Hierarchical model evaluation")
  expect_identical(printed[[2L]], "Rodriguez, Reise, and Haviland (2016).")
  # The model-choice paragraph is a caution for every model, not a flag.
  expect_false(any(grepl("Bonifay", printed, fixed = TRUE)))
  expect_false(any(grepl("^Notes$", printed)))
  expect_true("Item set" %in% printed)
  expect_match(printed, "^  ECV +\\.[0-9]{2}$", all = FALSE)
  expect_match(printed, "Min competing r -- The lowest correlation", fixed = TRUE, all = FALSE)
  expect_match(printed, "do not choose between a bifactor and a higher-order", fixed = TRUE,
               all = FALSE)
  expect_true(all(nchar(printed) <= 80L))

  summarized <- capture.output(print(summary(h)))
  expect_true("Factor scores" %in% summarized)
  expect_true("Notes" %in% summarized)
  expect_match(summarized, "Bonifay, Lane, and Reise (2017)", fixed = TRUE, all = FALSE)
  expect_match(summarized, "^  - Omega total = \\.[0-9]{2}: Common sources explain \\.[0-9]{2}",
               all = FALSE)
  # Each note's brief form is one sentence; its full form is longer.
  expect_true(all(nchar(h$notes$brief) <= nchar(h$notes$note)))

  # A digits argument still sets the precision.
  expect_match(capture.output(print(h, digits = 3L)), "^  Omega total +\\.[0-9]{3}$", all = FALSE)

  # A saved object from before the brief notes and factor scores still prints.
  old <- h
  old$notes$brief <- NULL
  old$factors <- NULL
  out <- capture.output(print(old))
  expect_false(any(grepl("Min competing r", out, fixed = TRUE)))
  expect_match(out, "^  - Review:", all = FALSE)
})


test_that("H is the squared determinacy for unidimensional data, as the notes say (#145)", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 1201)
  h <- nomo_hierarchical(nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat))
  note <- h$notes$note[h$notes$topic == "factor_scores" & h$notes$severity == "info"]
  expect_match(note, "H equals the squared determinacy (`determinacy_r2`)", fixed = TRUE)
  expect_false(grepl("equivalent when the data are unidimensional", note, fixed = TRUE))
})


test_that("plurals agree with their counts, and the plot legend says group factors (#145)", {
  structure <- list(
    type = "bifactor", general = "G",
    groups = list(A = c("x1", "x2"), B = c("x3", "x4")), items = paste0("x", 1:5)
  )
  computed <- list(loadings = tibble::tibble(
    subscale = c("A", "A", "B", "B", NA), group_loading = c(.4, -.3, .4, -.3, 0)
  ), factors = NULL)
  notes <- nomologR:::nomo_hierarchical_notes(
    list(ordered = character()), structure,
    list(theta = structure(diag(5), dimnames = list(paste0("x", 1:5), paste0("x", 1:5))),
         psi = structure(diag(3), dimnames = list(c("G", "A", "B"), c("G", "A", "B"))),
         sources = c("G", "A", "B")),
    computed, TRUE
  )
  expect_match(notes$note[notes$topic == "structure"], "Item x5 loads on the general factor only.",
               fixed = TRUE)
  expect_match(notes$note[notes$topic == "group_loadings"], "Group factors A, B have loadings",
               fixed = TRUE)
  expect_false(any(grepl("(s)", notes$note, fixed = TRUE)))

  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 1201)
  h <- nomo_hierarchical(nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat))
  p <- plot(h)
  expect_true("Group factors" %in% levels(p$data$source))
  expect_identical(p$scales$get_scales("x")$labels(c(0, .5, 1)), c("0", ".50", "1.00"))
})


test_that("guidance must be a list, and automatic changes are refused (#145)", {
  dat <- hier_sample(hier_bifactor_population()$sigma, 600, 1201)
  bf <- nomo_cfa(nomo_model(hier_groups, "bifactor"), data = dat)
  expect_error(nomo_hierarchical(bf, guidance = "teaching"), "`guidance` must be a list")
  guidance <- nomo_defaults()
  guidance$auto_respecify <- TRUE
  expect_error(nomo_hierarchical(bf, guidance = guidance), "guidance$auto_respecify",
               fixed = TRUE)
})
