test_that("measurement helper validation and tidy conversion branches are exercised", {
  expect_error(
    nomo_guidance_value(list(), "reliability_reference"),
    "missing required setting"
  )
  expect_error(
    nomo_guidance_value(list(reliability_reference = Inf), "reliability_reference"),
    "finite numeric"
  )

  num_named <- c(F1 = .80, F2 = .75)
  tidy_named <- nomo_reliability_tidy(num_named, metric = "omega")
  expect_equal(tidy_named$construct, c("F1", "F2"))
  expect_equal(tidy_named$block, c("overall", "overall"))

  num_unnamed <- unname(num_named)
  tidy_constructs <- nomo_reliability_tidy(
    num_unnamed,
    metric = "omega",
    construct_names = c("A", "B")
  )
  expect_equal(tidy_constructs$construct, c("A", "B"))

  tidy_fallback <- nomo_reliability_tidy(
    unname(c(.80, .75)),
    metric = "omega"
  )
  expect_equal(tidy_fallback$construct, c("construct_1", "construct_2"))

  df <- data.frame(
    group = c(1, 2),
    F1 = c(.80, .82),
    F2 = c(.74, .76)
  )
  tidy_df <- nomo_reliability_tidy(df, metric = "omega")
  expect_setequal(unique(tidy_df$construct), c("F1", "F2"))
  expect_setequal(unique(tidy_df$block), c("1", "2"))

  expect_equal(
    nrow(nomo_reliability_tidy(
      data.frame(group = c("a", "b")),
      metric = "omega"
    )),
    0L
  )

  list_input <- list(
    F1 = c(group1 = .80, group2 = .81),
    F2 = numeric()
  )
  tidy_list <- nomo_reliability_tidy(list_input, metric = "omega")
  expect_equal(unique(tidy_list$construct), "F1")
  expect_equal(tidy_list$block, c("group1", "group2"))

  expect_equal(nrow(nomo_reliability_tidy("not supported", "omega")), 0L)
})


test_that("item-type and matrix helpers cover meaningful edge cases", {
  expect_equal(
    nrow(nomo_reliability_item_types(list(parameter_estimates = NULL))),
    0L
  )

  no_loads <- data.frame(lhs = "F1", op = "~~", rhs = "F1")
  expect_equal(
    nrow(nomo_reliability_item_types(list(
      parameter_estimates = no_loads,
      ordered = character()
    ))),
    0L
  )

  pe <- data.frame(
    lhs = c("F1", "F1", "F2", "F2"),
    op = rep("=~", 4),
    rhs = c("a1", "a2", "b1", "b2")
  )
  types <- nomo_reliability_item_types(list(
    parameter_estimates = pe,
    ordered = c("a1", "a2", "b1")
  ))
  expect_equal(
    unname(types$indicator_type[match(c("F1", "F2"), types$construct)]),
    c("ordered", "mixed")
  )

  expect_equal(nrow(nomo_matrix_pairs(1:4)), 0L)
  expect_equal(nrow(nomo_matrix_pairs(matrix(1, 1, 1))), 0L)

  unnamed <- matrix(c(1, .3, .3, 1), 2, 2)
  pairs <- nomo_matrix_pairs(unnamed, value_name = "r")
  expect_equal(pairs$construct_1, "V2")
  expect_equal(pairs$construct_2, "V1")
  expect_equal(pairs$r, .3)
})


test_that("measurement-model guards cover nonconvergence and higher-order CFA", {
  model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
  '

  not_fit <- lavaan::cfa(
    model,
    data = lavaan::HolzingerSwineford1939,
    do.fit = FALSE
  )
  expect_error(
    nomo_measurement_fit(not_fit),
    "did not converge"
  )

  higher_model <- '
    visual  =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
    speed   =~ x7 + x8 + x9
    general =~ visual + textual + speed
  '
  higher <- lavaan::cfa(
    higher_model,
    data = lavaan::HolzingerSwineford1939
  )
  expect_true(lavaan::lavInspect(higher, "converged"))
  expect_error(
    nomo_measurement_fit(higher),
    "higher-order"
  )
})


test_that("reliability refuses a composite that mixes ordered and continuous indicators", {
  set.seed(5601)
  n <- 600
  f <- rnorm(n)

  latent <- data.frame(
    i1 = .80 * f + rnorm(n, sd = .60),
    i2 = .78 * f + rnorm(n, sd = .62),
    i3 = .76 * f + rnorm(n, sd = .64),
    i4 = .74 * f + rnorm(n, sd = .66)
  )

  dat <- latent
  dat$i1 <- ordered(cut(
    latent$i1,
    breaks = c(-Inf, -.5, .5, Inf),
    labels = FALSE
  ))
  dat$i2 <- ordered(cut(
    latent$i2,
    breaks = c(-Inf, -.5, .5, Inf),
    labels = FALSE
  ))

  fit <- lavaan::cfa(
    'F =~ i1 + i2 + i3 + i4',
    data = dat,
    ordered = c("i1", "i2"),
    estimator = "WLSMV"
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  expect_error(
    nomo_reliability(fit),
    "mixes ordered and continuous"
  )
})


test_that("bootstrap statistic handles ordered and continuous constructs without changing alpha estimands", {
  set.seed(5602)
  n <- 700
  f1 <- rnorm(n)
  f2 <- .30 * f1 + sqrt(1 - .30^2) * rnorm(n)

  a1 <- .82 * f1 + rnorm(n, sd = .58)
  a2 <- .78 * f1 + rnorm(n, sd = .62)
  a3 <- .75 * f1 + rnorm(n, sd = .65)

  dat <- data.frame(
    A1 = ordered(cut(a1, c(-Inf, -.6, 0, .6, Inf), labels = FALSE)),
    A2 = ordered(cut(a2, c(-Inf, -.6, 0, .6, Inf), labels = FALSE)),
    A3 = ordered(cut(a3, c(-Inf, -.6, 0, .6, Inf), labels = FALSE)),
    B1 = .82 * f2 + rnorm(n, sd = .58),
    B2 = .78 * f2 + rnorm(n, sd = .62),
    B3 = .75 * f2 + rnorm(n, sd = .65)
  )

  fit <- lavaan::cfa(
    '
      F1 =~ A1 + A2 + A3
      F2 =~ B1 + B2 + B3
    ',
    data = dat,
    ordered = c("A1", "A2", "A3"),
    estimator = "WLSMV"
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  fit_info <- nomo_measurement_fit(fit)
  type_context <- nomo_reliability_item_types(fit_info)
  expect_equal(
    unname(type_context$indicator_type[match(c("F1", "F2"), type_context$construct)]),
    c("ordered", "continuous")
  )

  expected <- c(
    "omega::F1::overall",
    "omega::F2::overall",
    "alpha::F2::overall"
  )

  stat <- nomo_reliability_boot_stat(
    fit = fit,
    expected_keys = expected,
    construct_names = fit_info$latent_names,
    type_context = type_context,
    obs.var = TRUE,
    ordinal_scale = TRUE,
    include_alpha = TRUE
  )

  expect_equal(names(stat), expected)
  expect_true(all(is.finite(stat)))
  expect_false("alpha::F1::overall" %in% names(stat))

  no_alpha <- nomo_reliability_boot_stat(
    fit = fit,
    expected_keys = expected[1:2],
    construct_names = fit_info$latent_names,
    type_context = type_context,
    obs.var = TRUE,
    ordinal_scale = TRUE,
    include_alpha = FALSE
  )
  expect_true(all(is.finite(no_alpha)))
})


test_that("bootstrap CI helper reports engine failure instead of manufacturing intervals", {
  evidence <- tibble::tibble(
    construct = "F",
    block = "overall",
    metric = "omega",
    estimate = .80
  )

  bad_fit_info <- list(
    fit = structure(list(), class = "definitely_not_lavaan"),
    latent_names = "F"
  )

  result <- nomo_reliability_bootstrap_ci(
    fit_info = bad_fit_info,
    evidence = evidence,
    type_context = tibble::tibble(
      construct = "F",
      indicator_type = "continuous"
    ),
    obs.var = TRUE,
    ordinal_scale = TRUE,
    include_alpha = TRUE,
    level = .95,
    R = 20L,
    seed = 5603L
  )

  expect_false(result$status$available)
  expect_match(result$status$reason, "Bootstrap failed")
  expect_true(all(is.na(result$intervals$ci_lower)))
  expect_true(all(is.na(result$intervals$ci_upper)))
})


test_that("HTMT and Fornell-Larcker helpers disclose unavailable cases", {
  unavailable_group <- nomo_validity_htmt_inputs(list(
    ngroups = 2L,
    nlevels = 1L
  ))
  expect_false(unavailable_group$available)
  expect_match(unavailable_group$reason, "not silently pooled")

  unavailable_cross <- nomo_validity_htmt_inputs(list(
    ngroups = 1L,
    nlevels = 1L,
    cross_loaded_items = "x1"
  ))
  expect_false(unavailable_cross$available)
  expect_match(unavailable_cross$reason, "cross-loaded")

  unavailable_pe <- nomo_validity_htmt_inputs(list(
    ngroups = 1L,
    nlevels = 1L,
    cross_loaded_items = character(),
    parameter_estimates = NULL
  ))
  expect_false(unavailable_pe$available)
  expect_match(unavailable_pe$reason, "could not be recovered")

  one_factor_pe <- data.frame(
    lhs = rep("F", 3),
    op = rep("=~", 3),
    rhs = c("x1", "x2", "x3")
  )
  unavailable_one <- nomo_validity_htmt_inputs(list(
    ngroups = 1L,
    nlevels = 1L,
    cross_loaded_items = character(),
    parameter_estimates = one_factor_pe
  ))
  expect_false(unavailable_one$available)
  expect_match(unavailable_one$reason, "at least two latent constructs")

  fl_bad_fit <- nomo_validity_fornell_larcker(
    structure(list(), class = "not_lavaan"),
    tibble::tibble(
      construct = c("F1", "F2"),
      block = "overall",
      estimate = c(.60, .62)
    )
  )
  expect_null(fl_bad_fit$matrix)
  expect_match(fl_bad_fit$reason, "single latent-correlation matrix")

  model <- '
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  '
  fit <- lavaan::cfa(model, data = lavaan::HolzingerSwineford1939)

  fl_no_overall <- nomo_validity_fornell_larcker(
    fit,
    tibble::tibble(
      construct = c("F1", "F2"),
      block = c("group1", "group1"),
      estimate = c(.60, .62)
    )
  )
  expect_null(fl_no_overall$matrix)
  expect_match(fl_no_overall$reason, "Single-block AVE")

  fl_mismatch <- nomo_validity_fornell_larcker(
    fit,
    tibble::tibble(
      construct = c("X", "Y"),
      block = "overall",
      estimate = c(.60, .62)
    )
  )
  expect_null(fl_mismatch$matrix)
  expect_match(fl_mismatch$reason, "could not be aligned")
})


test_that("presentation branches communicate point-only and interval workflows", {
  model <- '
    F1 =~ x1 + x2 + x3
    F2 =~ x4 + x5 + x6
  '
  cfa <- nomo_cfa(model, data = lavaan::HolzingerSwineford1939)

  rel <- nomo_reliability(cfa, ci = "none")
  printed <- capture.output(print(rel))
  expect_true(any(grepl("point estimates only", printed, fixed = TRUE)))

  summary_printed <- capture.output(print(summary(rel)))
  expect_true(any(grepl("Sampling uncertainty was not bootstrapped", summary_printed, fixed = TRUE)))

  p <- plot(rel)
  expect_s3_class(p, "ggplot")
  expect_true(any(grepl(
    "bootstrap CIs are optional",
    p$labels$subtitle,
    fixed = TRUE
  )))

  one_factor_cfa <- nomo_cfa(
    'F =~ x1 + x2 + x3',
    data = lavaan::HolzingerSwineford1939
  )
  val <- nomo_validity(one_factor_cfa, htmt = "none")
  val_summary <- capture.output(print(summary(val)))
  expect_true(any(grepl(
    "No pairwise construct-separation summary is available",
    val_summary,
    fixed = TRUE
  )))
})
