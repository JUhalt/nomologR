# ---- consolidated from test-nomo-m5-coverage-hardening.R ----
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
    plot_text(p$labels$subtitle),
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


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

test_that("closeout: measurement helper fallback schemas cover data-frame, list, and HTMT alignment branches", {
  # Two rows without a group column are two blocks, never one block twice
  # (#145); one row is the single overall block.
  tidy_df <- nomologR:::nomo_reliability_tidy(
    data.frame(F1 = c(.8, .9)),
    metric = "omega"
  )
  expect_identical(tidy_df$block, c("block_1", "block_2"))
  one_row <- nomologR:::nomo_reliability_tidy(data.frame(F1 = .8), metric = "omega")
  expect_identical(one_row$block, "overall")

  tidy_list <- nomologR:::nomo_reliability_tidy(
    list(F1 = c(.8, .9)),
    metric = "omega"
  )
  expect_identical(tidy_list$block, c("block_1", "block_2"))

  fit <- make_m9_report_run()$results$cfa$fit
  fi <- nomologR:::nomo_measurement_fit(fit)
  pe <- fi$parameter_estimates
  pe2 <- pe[pe$op == "=~", , drop = FALSE]
  pe2$lhs <- rep(c("A", "B"), length.out = nrow(pe2))
  pe2$rhs <- paste0("not_an_observed_item_", seq_len(nrow(pe2)))
  fi$parameter_estimates <- pe2
  fi$latent_names <- c("A", "B")
  fi$cross_loaded_items <- character()
  fi$ngroups <- 1L
  fi$nlevels <- 1L

  h <- nomologR:::nomo_validity_htmt_inputs(fi)
  expect_false(h$available)
  expect_match(h$reason, "could not be aligned", fixed = TRUE)
})


test_that("closeout: latent-correlation helper covers missing-CI and cor.lv list fallbacks", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- make_m9_report_run()$results$cfa$fit
  original_lav_names <- lavaan::lavNames

  testthat::local_mocked_bindings(
    standardizedSolution = function(...) data.frame(
      lhs = "A",
      op = "~~",
      rhs = "B",
      est.std = .40
    ),
    lavNames = function(object, type, ...) {
      if (identical(type, "lv")) c("A", "B") else original_lav_names(object, type, ...)
    },
    .package = "lavaan"
  )
  direct <- nomologR:::nomo_validity_latent_correlations(fit)
  expect_true(all(is.na(direct$ci_lower)))
  expect_true(all(is.na(direct$ci_upper)))

  cor_list <- list(
    g1 = matrix(
      c(1, .3, .3, 1), 2, 2,
      dimnames = list(c("A", "B"), c("A", "B"))
    ),
    g2 = matrix(
      c(1, .4, .4, 1), 2, 2,
      dimnames = list(c("A", "B"), c("A", "B"))
    )
  )
  testthat::local_mocked_bindings(
    standardizedSolution = function(...) data.frame(),
    lavNames = function(...) character(),
    lavInspect = function(object, what, ...) {
      if (identical(what, "cor.lv")) cor_list else NULL
    },
    .package = "lavaan"
  )
  fallback <- nomologR:::nomo_validity_latent_correlations(fit)
  expect_setequal(unique(fallback$block), c("g1", "g2"))
})


test_that("closeout B: measurement-fit fallbacks normalize absent parameter metadata and group-level counts", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- make_m9_report_run()$results$cfa$fit
  original_inspect <- lavaan::lavInspect

  testthat::local_mocked_bindings(
    parameterEstimates = function(...) NULL,
    lavNames = function(...) character(),
    lavInspect = function(object, what, ...) {
      if (identical(what, "converged")) return(TRUE)
      if (identical(what, "ordered")) return(character())
      if (identical(what, "ngroups")) return(NA_real_)
      if (identical(what, "nlevels")) return(NA_real_)
      if (identical(what, "post.check")) return(TRUE)
      original_inspect(object, what, ...)
    },
    .package = "lavaan"
  )

  info <- nomologR:::nomo_measurement_fit(fit)
  expect_length(info$cross_loaded_items, 0L)
  expect_identical(info$ngroups, 1L)
  expect_identical(info$nlevels, 1L)
})


test_that("closeout B: measurement fit-context and latent-correlation helpers preserve empty and unnamed fallbacks", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- make_m9_report_run()$results$cfa$fit

  testthat::local_mocked_bindings(
    fitMeasures = function(...) numeric(),
    .package = "lavaan"
  )
  empty_fit <- nomologR:::nomo_reliability_fit_context(
    fit,
    nomo_defaults()
  )
  expect_equal(nrow(empty_fit), 0L)

  mat <- matrix(
    c(1, .30, .30, 1),
    2, 2,
    dimnames = list(c("A", "B"), c("A", "B"))
  )

  testthat::local_mocked_bindings(
    standardizedSolution = function(...) data.frame(),
    lavNames = function(...) character(),
    lavInspect = function(object, what, ...) {
      if (identical(what, "cor.lv")) {
        return(unname(list(mat, mat)))
      }
      NULL
    },
    .package = "lavaan"
  )
  unnamed <- nomologR:::nomo_validity_latent_correlations(fit)
  expect_setequal(unique(unnamed$block), c("block_1", "block_2"))

  testthat::local_mocked_bindings(
    standardizedSolution = function(...) data.frame(),
    lavNames = function(...) character(),
    lavInspect = function(...) NULL,
    .package = "lavaan"
  )
  none <- nomologR:::nomo_validity_latent_correlations(fit)
  expect_equal(nrow(none), 0L)
})


test_that("closeout B: HTMT input recovery handles one-element data lists and unavailable raw data", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- lavaan::cfa(
    "
      visual =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ",
    data = lavaan::HolzingerSwineford1939
  )
  fi <- nomologR:::nomo_measurement_fit(fit)

  raw <- as.matrix(
    lavaan::HolzingerSwineford1939[
      ,
      c("x1", "x2", "x3", "x4", "x5", "x6")
    ]
  )

  testthat::local_mocked_bindings(
    lavInspect = function(object, what, ...) {
      if (identical(what, "data")) return(list(raw))
      NULL
    },
    lavNames = function(object, type, ...) {
      if (identical(type, "ov")) return(colnames(raw))
      character()
    },
    .package = "lavaan"
  )
  available <- nomologR:::nomo_validity_htmt_inputs(fi)
  expect_true(available$available)

  testthat::local_mocked_bindings(
    lavInspect = function(...) NULL,
    .package = "lavaan"
  )
  unavailable <- nomologR:::nomo_validity_htmt_inputs(fi)
  expect_false(unavailable$available)
  expect_match(unavailable$reason, "Raw observed data", fixed = TRUE)
})


# semTools return shapes (#145) ------------------------------------------------

test_that("each semTools return shape keeps one row per construct and block", {
  # A multi-group result with several composites names its groups in the row
  # names, with no group column.
  by_rows <- data.frame(Agency = c(.84, .86), Persistence = c(.82, .81),
                        row.names = c("online", "paper"))
  tidy <- nomo_reliability_tidy(by_rows, "omega", c("Agency", "Persistence"))
  expect_identical(tidy$construct, rep(c("Agency", "Persistence"), each = 2L))
  expect_identical(tidy$block, rep(c("online", "paper"), 2L))

  # One composite across groups comes back as a vector named by group.
  one <- nomo_reliability_tidy(c(online = .84, paper = .86), "omega", "Agency")
  expect_identical(one$construct, c("Agency", "Agency"))
  expect_identical(one$block, c("online", "paper"))
  # A vector named by its constructs is read as before.
  named <- nomo_reliability_tidy(c(Agency = .84), "omega", "Agency")
  expect_identical(c(named$construct, named$block), c("Agency", "overall"))

  # A lone composite is unnamed; with a single-indicator factor dropped, the
  # remaining composite is named from the factors that have one.
  lone <- nomo_reliability_tidy(.835, "omega", "A")
  expect_identical(lone$construct, "A")

  # AVE for factors with a cross-loaded indicator is a logical NA vector.
  na_ave <- nomo_reliability_tidy(c(A = NA, B = NA), "AVE")
  expect_identical(na_ave$construct, c("A", "B"))
  expect_true(all(is.na(na_ave$estimate)))
  expect_type(na_ave$estimate, "double")
  na_df <- nomo_reliability_tidy(
    data.frame(group = c("g1", "g2"), A = c(NA, NA)), "AVE"
  )
  expect_identical(na_df$block, c("g1", "g2"))

  # A multilevel AVE lists each level, with that level's factors inside it.
  levels <- list(within = c(FW = .64), cluster = c(FB = .96))
  multilevel <- nomo_reliability_tidy(levels, "AVE", c("FW", "FB"))
  expect_identical(multilevel$construct, c("FW", "FB"))
  expect_identical(multilevel$block, c("within", "cluster"))

  # The AVE table drops the metric column, and stays empty for a shape it
  # cannot read.
  ave <- nomo_validity_ave_tidy(c(A = NA, B = .5))
  expect_identical(names(ave), c("construct", "block", "estimate"))
  expect_identical(nrow(nomo_validity_ave_tidy("not supported")), 0L)
})


test_that("measurement-fit names come from factors with more than one indicator", {
  fit <- lavaan::cfa("S =~ b1\nA =~ a1 + a2 + a3 + a4 + a5", data = nomo_demo_continuous)
  info <- nomo_measurement_fit(fit)
  expect_identical(info$latent_names, "A")

  # A multi-group model counts each indicator once.
  mg <- lavaan::cfa("Agency =~ ag1 + ag2 + ag3 + ag4\nS =~ pe1",
                    data = nomo_demo_network, group = "group")
  types <- nomo_reliability_item_types(nomo_measurement_fit(mg))
  expect_identical(unname(types$n_items), c(4L, 1L))
})


test_that("ordered indicators are read from what lavaan fitted", {
  # Ordered-factor columns are fitted as categorical even when `ordered` does
  # not name them, and lavInspect(fit, "ordered") is then empty (#145).
  fit <- lavaan::cfa("A =~ a1 + a2 + a3 + a4 + a5", data = nomo_demo_ordinal)
  expect_length(lavaan::lavInspect(fit, "ordered"), 0L)
  info <- nomo_measurement_fit(fit)
  expect_identical(info$ordered, paste0("a", 1:5))
  expect_identical(unname(nomo_reliability_item_types(info)$indicator_type), "ordered")
})


test_that("HTMT's missing-data handling follows the fitted model, and its cases are counted", {
  model <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  listwise <- lavaan::cfa(model, data = nomo_demo_continuous)
  fiml <- lavaan::cfa(model, data = nomo_demo_continuous, missing = "fiml")
  expect_identical(nomo_validity_htmt_missing(listwise), "listwise")
  expect_identical(nomo_validity_htmt_missing(fiml), "fiml")
  ord <- nomo_demo_ordinal
  ord$a1[1:20] <- NA
  pairwise <- lavaan::cfa(model, data = ord, missing = "pairwise")
  expect_identical(nomo_validity_htmt_missing(pairwise), "pairwise")
  expect_identical(nomo_validity_htmt_missing(NULL), "listwise")

  dat <- data.frame(a = c(1, NA, 3, NA), b = c(1, 2, NA, NA), c = c(1, 2, 3, NA))
  expect_identical(nomo_validity_htmt_n(dat, "listwise"), 1L)
  expect_identical(nomo_validity_htmt_n(dat, "pairwise"), 1L)
  expect_identical(nomo_validity_htmt_n(dat, "fiml"), 3L)
})
