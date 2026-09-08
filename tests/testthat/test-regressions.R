# ---- consolidated from test-checkpoint-c-regressions.R ----
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


test_that("partial invariance always requires rationale", {
  expect_error(
    nomo_partial(
      level = "metric",
      syntax = "F =~ x2",
      rationale = ""
    ),
    "blank"
  )
})


test_that("report-ready table generic refuses unsupported classes normally", {
  expect_error(
    nomo_table(list(a = 1)),
    "no applicable method",
    ignore.case = TRUE
  )
})

# ---- consolidated from test-checkpoint-c-table-edges.R ----
test_that("nomo_table generic covers hypothesis and unsupported-object behavior", {
  h <- nomo_hypotheses(
    "A -> B" = positive(),
    "A <-> C" = negligible(within = c(-.10, .10))
  )

  expect_identical(
    nomo_table(h),
    h$hypotheses
  )

  expect_error(
    nomo_table(list(a = 1)),
    "no applicable method",
    ignore.case = TRUE
  )
})


# ---- pre-v0.1 exact-zero-map closeout A -------------------------------------

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


test_that("closeout: screen and split cover empty, binary, other, and RNG cleanup paths", {
  empty_cols <- data.frame(row.names = 1:3)
  expect_error(nomo_screen(empty_cols), "at least one column")

  expect_equal(
    nomologR:::nomo_screen_item_type(c(TRUE, FALSE, TRUE)),
    "binary"
  )
  expect_equal(
    nomologR:::nomo_screen_item_type(as.Date("2026-01-01") + 0:2),
    "other"
  )

  scr <- nomo_screen(data.frame(x = c(1, 2, 3, 4, 5)), items = "x")
  scr$decision_log <- nomologR:::nomo_log_new()
  txt <- paste(capture.output(print(scr)), collapse = "\n")
  expect_match(txt, "Decision log: no entries", fixed = TRUE)

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  invisible(nomo_split(data.frame(x = 1:10), seed = 2026L))
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})


test_that("closeout: screening relationship diagnostics cover non-finite scores and no numeric reference", {
  selected <- data.frame(
    a = c(1, Inf, 3, 4, 5),
    b = c(1, 2, 3, 4, 5)
  )
  item_summary <- dplyr::bind_rows(
    nomologR:::nomo_screen_item_summary(selected$a, "a"),
    nomologR:::nomo_screen_item_summary(selected$b, "b")
  )
  guidance <- nomo_defaults()
  rel <- nomologR:::nomo_screen_relationships(
    selected = selected,
    item_summary = item_summary,
    guidance = guidance
  )
  expect_true(any(rel$decision_log$metric == "non_finite_scores"))

  selected2 <- data.frame(
    a = 1:8,
    b = 1:8,
    c = 8:1
  )
  item_summary2 <- dplyr::bind_rows(
    nomologR:::nomo_screen_item_summary(selected2$a, "a"),
    nomologR:::nomo_screen_item_summary(selected2$b, "b"),
    nomologR:::nomo_screen_item_summary(selected2$c, "c")
  )
  guidance2 <- nomo_defaults()
  guidance2$item_total_reference <- NA_real_
  rel2 <- nomologR:::nomo_screen_relationships(
    selected = selected2,
    item_summary = item_summary2,
    guidance = guidance2
  )
  neg <- rel2$decision_log[
    rel2$decision_log$metric == "corrected_item_rest" &
      rel2$decision_log$value < 0,
    ,
    drop = FALSE
  ]
  expect_gt(nrow(neg), 0L)
  expect_true(any(grepl(
    "Negative sign requires coding/structure review",
    neg$reference,
    fixed = TRUE
  )))
})


test_that("closeout: screening descriptives cover guidance fallback and ceiling concentration", {
  x <- c(1, 2, 3, 4, rep(5, 16))
  selected <- data.frame(item = x)
  item_summary <- nomologR:::nomo_screen_item_summary(x, "item")

  guidance <- nomo_defaults()
  guidance$response_concentration_reference <- "bad"
  guidance$nzv_frequency_ratio_reference <- NULL
  guidance$nzv_percent_unique_reference <- Inf

  out <- nomologR:::nomo_screen_descriptives(
    selected = selected,
    item_summary = item_summary,
    guidance = guidance
  )
  expect_true(any(
    out$decision_log$metric %in%
      c("response_concentration", "ceiling_concentration")
  ))

  guidance$response_concentration_reference <- .70
  out2 <- nomologR:::nomo_screen_descriptives(
    selected = selected,
    item_summary = item_summary,
    guidance = guidance
  )
  expect_true(any(out2$decision_log$metric == "ceiling_concentration"))
})


test_that("closeout: screen presentation guards and rare evidence states are exercised", {
  expect_error(
    nomologR:::summary.nomo_screen(list()),
    "inherit from `nomo_screen`"
  )
  expect_error(
    nomologR:::plot.nomo_screen(list()),
    "inherit from `nomo_screen`"
  )

  ord <- ordered(
    c("low", "low", "high", "high"),
    levels = c("low", "mid", "high")
  )
  scr <- nomo_screen(data.frame(ord = ord, x = 1:4))
  ev <- nomologR:::nomo_screen_evidence_data(scr, scr$items)
  expect_true(any(
    ev$metric == "unused_response_categories" &
      as.character(ev$severity) == "review"
  ))

  extra <- nomologR:::nomo_log_add(
    scr$decision_log,
    stage = "screen",
    object = "x",
    metric = "does_not_map_to_evidence_grid",
    severity = "review"
  )
  scr$decision_log <- extra
  expect_s3_class(
    nomologR:::nomo_screen_evidence_data(scr, scr$items),
    "data.frame"
  )

  one <- nomo_screen(data.frame(x = 1:6), items = "x")
  expect_error(
    plot(one, type = "interitem"),
    "At least two relationship-eligible"
  )

  no_resp <- scr
  no_resp$response_distribution <- no_resp$response_distribution[0, , drop = FALSE]
  expect_error(
    plot(no_resp, type = "responses"),
    "No observed/declared response categories"
  )

  concern <- nomo_screen(data.frame(a = 1:8, b = 1:8, c = 8:1))
  concern$decision_log <- nomologR:::nomo_log_add(
    concern$decision_log,
    stage = "screen",
    object = "c",
    metric = "corrected_item_rest",
    severity = "concern"
  )
  p <- plot(concern, type = "item_rest")
  expect_s3_class(p, "ggplot")
})


test_that("closeout: run helper validation covers the remaining argument branches", {
  x <- make_m9_minimal_run()

  expect_error(
    nomologR:::nomo_run_set_stage(x, "not_a_stage", "completed"),
    "Unknown workflow stage"
  )

  broken_split <- structure(
    list(
      calibration = data.frame(),
      validation = data.frame(x = 1)
    ),
    class = c("nomo_split", "list")
  )
  expect_error(
    nomologR:::nomo_run_data_roles(broken_split),
    "non-empty"
  )

  scales <- list(S = c("i1", "i2"))
  expect_error(
    nomologR:::nomo_run_validate_settings(1, scales),
    "named list"
  )
  expect_identical(
    nomologR:::nomo_run_validate_settings(list(), scales),
    list()
  )

  dup_stage <- list(list(), list())
  names(dup_stage) <- c("efa", "efa")
  expect_error(
    nomologR:::nomo_run_validate_settings(dup_stage, scales),
    "unique stage names"
  )

  dup_args <- list(1, 2)
  names(dup_args) <- c("rotation", "rotation")
  expect_error(
    nomologR:::nomo_run_validate_settings(
      list(efa = dup_args),
      scales
    ),
    "uniquely named arguments"
  )

  expect_error(
    nomologR:::nomo_run_validate_settings(
      list(efa = list(types = c("continuous"))),
      scales
    ),
    "named character vector"
  )

  expect_error(
    nomologR:::nomo_run_scope_settings(
      settings = list(
        efa = list(types = c(i1 = "continuous", bad = "continuous"))
      ),
      stage = "efa",
      items = scales$S,
      scales = scales
    ),
    "unknown pipeline item"
  )

  scoped <- nomologR:::nomo_run_scope_settings(
    settings = list(
      efa = list(types = c(i1 = "continuous", i2 = "continuous"))
    ),
    stage = "efa",
    items = "i1",
    scales = scales
  )
  expect_identical(names(scoped$types), "i1")

  none <- nomologR:::nomo_run_scope_settings(
    settings = list(
      efa = list(types = c(i1 = "continuous", i2 = "continuous"))
    ),
    stage = "efa",
    items = "not_in_scope",
    scales = scales
  )
  expect_null(none$types)

  expect_error(
    nomologR:::nomo_run_component_call(
      fun = function(...) NULL,
      fixed = list(a = 1),
      extra = list(a = 2),
      stage = "efa"
    ),
    "cannot override"
  )

  expect_error(
    nomologR:::nomo_run_normalize_factor_counts("one", scales),
    "positive integer"
  )
  expect_error(
    nomologR:::nomo_run_normalize_factor_counts(
      c(S = 1, Extra = 1),
      scales
    ),
    "names must match"
  )

  expect_identical(
    nomologR:::nomo_run_normalize_rationale(NULL, c("A", "B")),
    c(A = "", B = "")
  )
  expect_error(
    nomologR:::nomo_run_normalize_rationale(1, "A"),
    "character text"
  )
  expect_identical(
    nomologR:::nomo_run_normalize_rationale(
      c(B = "b", A = "a"),
      c("A", "B")
    ),
    c(A = "a", B = "b")
  )

  nm <- nomo_model(list(F = c("i1", "i2")))
  expect_match(
    nomologR:::nomo_run_normalize_cfa_model(nm),
    "F =~"
  )

  expect_error(
    nomologR:::nomo_run_normalize_measurement_decision(1),
    "proceed"
  )

  expect_identical(
    nomologR:::nomo_run_merge_future_settings(x, list()),
    x
  )

  expect_error(
    nomo_run(
      data = data.frame(i1 = 1:6, i2 = 6:1),
      scales = scales,
      guidance = 1
    ),
    "`guidance` must be a list"
  )
})


test_that("closeout: measurement request handles components without decision logs", {
  x <- make_m9_minimal_run()
  x$results$cfa <- list(heywood_detected = FALSE, decision_log = NULL)
  x$results$reliability <- list(decision_log = NULL)
  x$results$validity <- list(decision_log = NULL)

  req <- nomologR:::nomo_run_measurement_request(x)
  expect_equal(nrow(req), 1L)
  expect_match(req$observation, "0 concern and 0 review", fixed = TRUE)
})


test_that("closeout: run decision handlers block cleanly when component calls fail", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- make_m9_minimal_run()

  testthat::local_mocked_bindings(
    nomo_efa = function(...) stop("synthetic EFA failure"),
    .package = "nomologR"
  )
  blocked_efa <- nomologR:::nomo_run_apply_factor_decision(x, 1L)
  expect_identical(blocked_efa$status, "blocked")
})


test_that("closeout: CFA workflow decision paths retain failed stages", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  base <- make_m9_minimal_run()
  base$next_stage <- "cfa"
  base$decision_requests <- tibble::tibble(
    id = "cfa_model",
    stage = "cfa",
    scope = "measurement_model",
    observation = "",
    reason = "",
    options = "",
    consequence = "",
    example = ""
  )

  expect_error(
    nomologR:::nomo_run_apply_cfa_model(
      base,
      list(value = "S =~ i1 + i2", rationale = 1)
    ),
    "rationale"
  )

  testthat::local_mocked_bindings(
    nomo_cfa = function(...) structure(
      list(converged = FALSE),
      class = c("nomo_cfa", "list")
    ),
    .package = "nomologR"
  )
  nc <- nomologR:::nomo_run_apply_cfa_model(
    base,
    list(value = "S =~ i1 + i2", rationale = "fixture")
  )
  expect_identical(nc$status, "blocked")
  expect_identical(nc$blocked$stage, "cfa")
})


test_that("closeout: reliability and validity failures block their own workflow stages", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  base <- make_m9_minimal_run()
  base$next_stage <- "cfa"
  base$decision_requests <- tibble::tibble(
    id = "cfa_model",
    stage = "cfa",
    scope = "measurement_model",
    observation = "",
    reason = "",
    options = "",
    consequence = "",
    example = ""
  )

  fake_cfa <- structure(
    list(converged = TRUE),
    class = c("nomo_cfa", "list")
  )

  testthat::local_mocked_bindings(
    nomo_cfa = function(...) fake_cfa,
    nomo_reliability = function(...) stop("synthetic reliability failure"),
    .package = "nomologR"
  )
  br <- nomologR:::nomo_run_apply_cfa_model(
    base,
    list(value = "S =~ i1 + i2", rationale = "fixture")
  )
  expect_identical(br$blocked$stage, "reliability")

  testthat::local_mocked_bindings(
    nomo_cfa = function(...) fake_cfa,
    nomo_reliability = function(...) structure(
      list(decision_log = nomologR:::nomo_log_new()),
      class = c("nomo_reliability", "list")
    ),
    nomo_validity = function(...) stop("synthetic validity failure"),
    .package = "nomologR"
  )
  bv <- nomologR:::nomo_run_apply_cfa_model(
    base,
    list(value = "S =~ i1 + i2", rationale = "fixture")
  )
  expect_identical(bv$blocked$stage, "validity")
})


test_that("closeout: downstream run helpers return blocked invariance and network states", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- make_m9_minimal_run()
  x$decisions$cfa_model <- list(value = "S =~ i1 + i2")
  x$settings$invariance <- list(group = "group")

  testthat::local_mocked_bindings(
    nomo_invariance = function(...) stop("synthetic invariance failure"),
    .package = "nomologR"
  )
  bi <- nomologR:::nomo_run_run_invariance(x)
  expect_identical(bi$status, "blocked")

  x2 <- make_m9_minimal_run()
  x2$decisions$cfa_model <- list(value = "S =~ i1 + i2")
  x2$settings$network <- list(
    hypotheses = nomo_hypotheses("S -> criterion" = positive())
  )

  testthat::local_mocked_bindings(
    nomo_network = function(...) stop("synthetic network failure"),
    .package = "nomologR"
  )
  bn <- nomologR:::nomo_run_run_network(x2)
  expect_identical(bn$status, "blocked")
})


test_that("closeout: measurement-model decision rationale is scalar text", {
  x <- make_m9_minimal_run()
  x$decision_requests <- tibble::tibble(
    id = "measurement_model",
    stage = "measurement_review",
    scope = "measurement_model",
    observation = "",
    reason = "",
    options = "",
    consequence = "",
    example = ""
  )
  expect_error(
    nomologR:::nomo_run_apply_measurement_decision(
      x,
      list(value = "proceed", rationale = 1)
    ),
    "rationale"
  )
})


test_that("closeout: network low-level classifiers cover non-evaluable and inconclusive states", {
  expect_error(
    nomologR:::nomo_network_model_table("F =~"),
    "Could not parse"
  )
  expect_false(
    nomologR:::nomo_network_relation_present(
      data.frame(),
      list(relation_type = "directed", source = "A", target = "B")
    )
  )
  expect_true(is.na(
    nomologR:::nomo_network_fit_measure(numeric(), "cfi")
  ))
  expect_length(
    nomologR:::nomo_network_match_index(
      data.frame(),
      list(relation_type = "directed", source = "A", target = "B")
    ),
    0L
  )

  expect_true(is.na(
    nomologR:::nomo_network_interval_within_region(
      NA_real_, .2, 0, Inf, FALSE, FALSE
    )
  ))
  expect_true(
    nomologR:::nomo_network_interval_within_region(
      -.2, .2, -Inf, .5, FALSE, TRUE
    )
  )
  expect_true(is.na(
    nomologR:::nomo_network_interval_overlaps_region(
      NA_real_, .2, 0, 1
    )
  ))
  expect_true(is.na(
    nomologR:::nomo_network_direction_correct(NA_real_, "positive")
  ))

  h <- as.list(
    nomo_hypotheses("A -> B" = positive(min = .20))$hypotheses[1, , drop = FALSE]
  )
  ne <- nomologR:::nomo_network_classify(
    h, estimate = .3, ci_lower = .2, ci_upper = .4, converged = FALSE
  )
  expect_identical(ne$concordance, "not_evaluable")

  h_inconclusive <- as.list(
    nomo_hypotheses("A -> B" = positive())$hypotheses[1, , drop = FALSE]
  )
  inc <- nomologR:::nomo_network_classify(
    h_inconclusive,
    estimate = -.10,
    ci_lower = -.20,
    ci_upper = .20,
    converged = TRUE
  )
  expect_identical(inc$concordance, "inconclusive")
})


test_that("closeout: network measurement context covers empty and strained measurement evidence", {
  fit <- make_m9_report_run()$results$cfa$fit
  g <- nomo_defaults()

  empty <- nomologR:::nomo_network_measurement_context(
    fit = fit,
    standardized_solution = data.frame(
      lhs = character(), op = character(), rhs = character(),
      est.std = numeric()
    ),
    parameter_estimates = data.frame(
      lhs = character(), op = character(), rhs = character(), est = numeric()
    ),
    fit_evidence = tibble::tibble(
      cfi = NA_real_, tli = NA_real_, rmsea = NA_real_, srmr = NA_real_
    ),
    converged = TRUE,
    warnings = character(),
    guidance = g
  )
  expect_equal(nrow(empty$loadings), 0L)
  expect_equal(nrow(empty$variances), 0L)

  g$cfa_loading_reference <- NA_real_
  strained <- nomologR:::nomo_network_measurement_context(
    fit = fit,
    standardized_solution = data.frame(
      lhs = "WellBeing", op = "=~", rhs = "i1", est.std = .30
    ),
    parameter_estimates = data.frame(
      lhs = "i1", op = "~~", rhs = "i1", est = -.10
    ),
    fit_evidence = tibble::tibble(
      cfi = .50, tli = .50, rmsea = .20, srmr = .20
    ),
    converged = FALSE,
    warnings = "synthetic engine warning",
    guidance = g
  )
  expect_identical(strained$summary$attention[[1L]], "concern")
  expect_match(strained$summary$observation[[1L]], "negative variance", fixed = TRUE)
})


test_that("closeout: network replication and log handle non-evaluable evidence and warnings", {
  a <- tibble::tibble(
    id = "H1",
    relation = "A -> B",
    prediction = "positive",
    estimate = NA_real_,
    concordance = "not_evaluable"
  )
  b <- tibble::tibble(
    id = "H1",
    relation = "A -> B",
    prediction = "positive",
    estimate = .2,
    concordance = "concordant"
  )
  rep <- nomologR:::nomo_network_replication_evidence(
    list(hypothesis_evidence = a),
    list(hypothesis_evidence = b)
  )
  expect_identical(rep$replication_status[[1L]], "not_evaluable")

  measurement <- list(
    summary = tibble::tibble(
      loading_review_flags = 0L,
      attention = "info",
      observation = "Synthetic measurement context."
    )
  )
  additions <- tibble::tibble(
    relation = "A -> B",
    syntax = "B ~ A",
    added_from_hypothesis = FALSE
  )
  evidence <- tibble::tibble(
    relation = "A -> B",
    concordance = "not_evaluable",
    estimate = NA_real_,
    theoretical_region = "(0, +Inf)",
    interpretation = "Not evaluable."
  )
  log <- nomologR:::nomo_network_decision_log(
    model_additions = additions,
    hypotheses_evidence = evidence,
    converged = FALSE,
    warnings = "synthetic warning",
    estimator = "ML",
    ordered = character(),
    measurement_context = measurement,
    sample_role = "primary"
  )
  expect_true(any(log$metric == "engine_warnings"))
  expect_true(any(log$severity == "concern"))
})


test_that("closeout: network public validation covers ordered validation and ML/FIML guards", {
  dat <- data.frame(
    i1 = rnorm(30),
    i2 = rnorm(30),
    criterion = rnorm(30)
  )
  val <- dat
  val$i1 <- NULL
  h <- nomo_hypotheses("F -> criterion" = positive())
  model <- "F =~ i1 + i2"

  expect_error(
    nomo_network(model, dat, h, guidance = 1),
    "`guidance` must be a list"
  )
  expect_error(
    nomo_network(model, dat, h, ordered = 1),
    "`ordered`"
  )
  expect_error(
    nomo_network(
      model, dat, h,
      validation_data = val,
      ordered = "i1"
    ),
    "not found in validation data"
  )
  expect_error(
    nomo_network(
      model, dat, h,
      ordered = "i1",
      estimator = "ml"
    ),
    "ML-family"
  )
  expect_error(
    nomo_network(
      model, dat, h,
      ordered = "i1",
      missing = "fiml"
    ),
    "FIML"
  )

  nm <- nomo_model(list(F = c("i1", "i2")))
  expect_error(
    nomo_network(
      nm,
      dat,
      nomo_hypotheses("F -> missing_node" = positive())
    ),
    "neither latent variables"
  )
})


test_that("closeout: network fit wrapper exposes missing/control arguments before engine failure", {
  h <- nomo_hypotheses("F -> criterion" = positive())
  rels <- tibble::tibble(
    id = "H1",
    relation = "F -> criterion",
    syntax = "criterion ~ F",
    already_in_model = FALSE,
    added_from_hypothesis = TRUE,
    origin = "a_priori"
  )
  expect_error(
    nomologR:::nomo_network_fit_once(
      model_fitted = "F =~ missing_item",
      model_relations = rels,
      hypotheses = h,
      data = data.frame(x = 1:10),
      ordered = character(),
      estimator_requested = NULL,
      estimator_source = "lavaan_default",
      missing = "fiml",
      std.lv = TRUE,
      control = list(iter.max = 1L),
      guidance = nomo_defaults(),
      equivalence_alpha = .05,
      sample_role = "primary"
    ),
    "Nomological-network estimation failed"
  )
})


test_that("closeout: network presentation covers empty evidence and replication branches", {
  net <- make_m9_full_report_run()$results$network

  if (nrow(net$replication_evidence)) {
    s <- summary(net)
    expect_gt(nrow(s$replication_counts), 0L)
    txt <- paste(capture.output(print(s)), collapse = "\n")
    expect_match(txt, "Replication evidence", fixed = TRUE)
  }

  empty_effects <- net
  empty_effects$hypothesis_evidence$estimate[] <- NA_real_
  empty_effects$hypothesis_evidence$ci_lower[] <- NA_real_
  empty_effects$hypothesis_evidence$ci_upper[] <- NA_real_
  expect_error(
    plot(empty_effects, type = "effects"),
    "No finite hypothesis estimates"
  )

  empty_con <- net
  empty_con$hypothesis_evidence <- empty_con$hypothesis_evidence[0, , drop = FALSE]
  expect_error(
    plot(empty_con, type = "concordance"),
    "No hypothesis evidence"
  )

  empty_fit <- net
  for (nm in c("cfi", "tli", "rmsea", "srmr")) {
    empty_fit$fit_evidence[[nm]] <- NA_real_
  }
  expect_error(
    plot(empty_fit, type = "fit"),
    "No finite global fit evidence"
  )

  no_rep <- net
  no_rep$replication_evidence <- no_rep$replication_evidence[0, , drop = FALSE]
  expect_error(
    plot(no_rep, type = "replication"),
    "No validation sample"
  )

  if (nrow(net$replication_evidence)) {
    bad_rep <- net
    bad_rep$replication_evidence$primary_estimate[] <- NA_real_
    bad_rep$replication_evidence$validation_estimate[] <- NA_real_
    expect_error(
      plot(bad_rep, type = "replication"),
      "No finite replication estimates"
    )
  }
})


test_that("closeout: reliability bootstrap helper covers alignment and finite-draw qualifications", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  evidence <- tibble::tibble(
    construct = c("F1", "F2"),
    block = c("overall", "overall"),
    metric = c("omega", "omega"),
    estimate = c(.8, .8)
  )
  type_context <- tibble::tibble(
    construct = c("F1", "F2"),
    indicator_type = c("continuous", "continuous")
  )
  fit_info <- list(fit = structure(list(), class = "lavaan"), latent_names = c("F1", "F2"))

  testthat::local_mocked_bindings(
    bootstrapLavaan = function(...) matrix(numeric(), 0L, 0L),
    .package = "lavaan"
  )
  bad_align <- nomologR:::nomo_reliability_bootstrap_ci(
    fit_info, evidence, type_context,
    obs.var = TRUE, ordinal_scale = TRUE, include_alpha = FALSE,
    level = .95, R = 20L, seed = 1L
  )
  expect_false(bad_align$status$available[[1L]])
  expect_match(bad_align$status$reason[[1L]], "could not be aligned", fixed = TRUE)

  draws <- matrix(.80, nrow = 20L, ncol = 2L)
  testthat::local_mocked_bindings(
    bootstrapLavaan = function(...) draws,
    .package = "lavaan"
  )
  ok <- nomologR:::nomo_reliability_bootstrap_ci(
    fit_info, evidence, type_context,
    obs.var = TRUE, ordinal_scale = TRUE, include_alpha = FALSE,
    level = .95, R = 20L, seed = 1L
  )
  expect_true(ok$status$available[[1L]])
  expect_identical(colnames(draws), NULL)

  few <- matrix(NA_real_, nrow = 20L, ncol = 2L)
  few[1:9, ] <- .80
  colnames(few) <- c("omega::F1::overall", "omega::F2::overall")
  testthat::local_mocked_bindings(
    bootstrapLavaan = function(...) few,
    .package = "lavaan"
  )
  too_few <- nomologR:::nomo_reliability_bootstrap_ci(
    fit_info, evidence, type_context,
    obs.var = TRUE, ordinal_scale = TRUE, include_alpha = FALSE,
    level = .95, R = 20L, seed = 1L
  )
  expect_false(too_few$status$available[[1L]])
  expect_match(too_few$status$reason[[1L]], "too few finite", fixed = TRUE)

  qualified <- matrix(NA_real_, nrow = 20L, ncol = 2L)
  qualified[1:15, ] <- seq(.70, .84, length.out = 15L)
  colnames(qualified) <- c("omega::F1::overall", "omega::F2::overall")
  testthat::local_mocked_bindings(
    bootstrapLavaan = function(...) qualified,
    .package = "lavaan"
  )
  q <- nomologR:::nomo_reliability_bootstrap_ci(
    fit_info, evidence, type_context,
    obs.var = TRUE, ordinal_scale = TRUE, include_alpha = FALSE,
    level = .95, R = 20L, seed = 1L
  )
  expect_true(q$status$available[[1L]])
  expect_match(q$status$reason[[1L]], "only 15 of 20", fixed = TRUE)
})


test_that("closeout: reliability bootstrap statistic returns all-NA when engines fail", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    compRelSEM = function(...) stop("synthetic engine failure"),
    .package = "semTools"
  )

  out <- nomologR:::nomo_reliability_boot_stat(
    fit = structure(list(), class = "lavaan"),
    expected_keys = "omega::F::overall",
    construct_names = "F",
    type_context = tibble::tibble(
      construct = "F",
      indicator_type = "continuous"
    ),
    obs.var = TRUE,
    ordinal_scale = TRUE,
    include_alpha = FALSE
  )
  expect_true(is.na(out[[1L]]))
})


test_that("closeout: reliability engine errors and inadmissible coefficient interpretation are explicit", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa

  testthat::local_mocked_bindings(
    compRelSEM = function(...) stop("synthetic omega failure"),
    .package = "semTools"
  )
  expect_error(
    nomo_reliability(cfa),
    "Omega/composite-reliability estimation failed"
  )

  testthat::local_mocked_bindings(
    compRelSEM = function(...) c(WellBeing = 1.20),
    .package = "semTools"
  )
  bad <- nomo_reliability(cfa, include_alpha = FALSE)
  expect_identical(bad$evidence$attention[[1L]], "concern")
  expect_match(
    bad$evidence$interpretation[[1L]],
    "outside the conventional 0-1",
    fixed = TRUE
  )

  testthat::local_mocked_bindings(
    nomo_reliability_tidy = function(...) tibble::tibble(),
    .package = "nomologR"
  )
  expect_error(
    nomo_reliability(cfa, include_alpha = FALSE),
    "could not be converted"
  )
})


test_that("closeout: mixed ordered/continuous constructs compute alpha only for continuous composites", {
  set.seed(5608)
  n <- 500
  f1 <- rnorm(n)
  f2 <- .25 * f1 + sqrt(1 - .25^2) * rnorm(n)

  a1 <- .8 * f1 + rnorm(n, sd = .6)
  a2 <- .75 * f1 + rnorm(n, sd = .65)
  a3 <- .7 * f1 + rnorm(n, sd = .7)

  dat <- data.frame(
    A1 = ordered(cut(a1, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    A2 = ordered(cut(a2, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    A3 = ordered(cut(a3, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    B1 = .8 * f2 + rnorm(n, sd = .6),
    B2 = .75 * f2 + rnorm(n, sd = .65),
    B3 = .7 * f2 + rnorm(n, sd = .7)
  )

  fit <- lavaan::cfa(
    "
      F1 =~ A1 + A2 + A3
      F2 =~ B1 + B2 + B3
    ",
    data = dat,
    ordered = c("A1", "A2", "A3"),
    estimator = "WLSMV"
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  rel <- nomo_reliability(
    fit,
    ordinal_scale = TRUE,
    include_alpha = TRUE
  )
  expect_true(any(rel$alpha_status$construct == "F2"))
  expect_false(rel$alpha_status$available[rel$alpha_status$construct == "F1"])
})


test_that("closeout: validity AVE and HTMT engine failure paths are retained as evidence", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa

  testthat::local_mocked_bindings(
    AVE = function(...) stop("synthetic AVE failure"),
    .package = "semTools"
  )
  expect_error(
    nomo_validity(cfa, htmt = "none"),
    "AVE estimation failed"
  )

  testthat::local_mocked_bindings(
    AVE = function(...) {
      warning("synthetic AVE warning")
      c(WellBeing = 1.20)
    },
    .package = "semTools"
  )
  out <- nomo_validity(cfa, htmt = "none")
  expect_identical(out$ave$attention[[1L]], "concern")
  expect_true(length(out$ave_warnings) >= 1L)
})


test_that("closeout: validity HTMT failures and inadmissible values are explicit", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa(
    "
      visual =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ",
    data = dat
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  testthat::local_mocked_bindings(
    htmt = function(...) stop("synthetic HTMT failure"),
    .package = "semTools"
  )
  failed <- nomo_validity(fit, htmt = "both")
  expect_true(all(failed$htmt_status$requested))
  expect_true(all(!failed$htmt_status$available))

  bad_mat <- matrix(
    c(1, -.20, -.20, 1),
    2, 2,
    dimnames = list(c("visual", "textual"), c("visual", "textual"))
  )
  testthat::local_mocked_bindings(
    htmt = function(...) bad_mat,
    .package = "semTools"
  )
  bad <- nomo_validity(fit, htmt = "both")
  expect_true(any(bad$discriminant$attention == "concern"))
  expect_true(any(grepl(
    "unavailable or inadmissible",
    bad$discriminant$interpretation,
    fixed = TRUE
  )))
})


test_that("closeout: validity grouped Fornell-Larcker and post-check concern paths are explicit", {
  dat <- lavaan::HolzingerSwineford1939
  fit <- lavaan::cfa(
    "
      visual =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ",
    data = dat,
    group = "school"
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  grouped <- nomo_validity(
    fit,
    htmt = "none",
    fornell_larcker = TRUE
  )
  expect_match(
    grouped$fornell_larcker_reason,
    "restricted to a single-group",
    fixed = TRUE
  )

  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )
  base_cfa <- make_m9_report_run()$results$cfa
  original_measurement_fit <- nomologR:::nomo_measurement_fit
  testthat::local_mocked_bindings(
    nomo_measurement_fit = function(...) {
      z <- original_measurement_fit(...)
      z$post_check <- FALSE
      z
    },
    .package = "nomologR"
  )
  strained <- nomo_validity(base_cfa, htmt = "none")
  expect_true(any(
    strained$decision_log$metric == "lavaan_post_check" &
      strained$decision_log$severity == "concern"
  ))
})


test_that("closeout: measurement helper fallback schemas cover data-frame, list, and HTMT alignment branches", {
  tidy_df <- nomologR:::nomo_reliability_tidy(
    data.frame(F1 = c(.8, .9)),
    metric = "omega"
  )
  expect_true(all(tidy_df$block == "overall"))

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


test_that("closeout: invariance sequence, level, partial, fit-row, and LRT error branches are covered", {
  expect_error(
    nomologR:::nomo_invariance_sequences("x1", NULL),
    "observed category information"
  )

  seq3 <- nomologR:::nomo_invariance_sequences(
    c("x1", "x2"),
    tibble::tibble(item = c("x1", "x2"), categories = c(3L, 4L))
  )
  expect_identical(seq3$type, "ordered_with_three_category")

  expect_error(
    nomologR:::nomo_invariance_validate_levels(
      1,
      c("configural", "metric")
    ),
    "`levels`"
  )

  row <- nomologR:::nomo_invariance_fit_row(
    level = "configural",
    constraints = character(),
    fit = NULL
  )
  expect_identical(row$constraints[[1L]], "none")

  lrt <- nomologR:::nomo_invariance_lrt(NULL, NULL)
  expect_true(is.na(lrt$chisq))

  expect_error(
    nomologR:::nomo_invariance_validate_partial(
      list(),
      c("configural", "metric")
    ),
    "object created by"
  )
})


test_that("closeout: invariance score-test handles engine errors, warnings, and missing columns", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- make_m9_full_report_run()$results$invariance$fits$metric

  testthat::local_mocked_bindings(
    lavTestScore = function(...) stop("synthetic score failure"),
    .package = "lavaan"
  )
  bad <- nomologR:::nomo_invariance_score_test(fit, "metric")
  expect_equal(nrow(bad$table), 0L)
  expect_match(bad$error, "synthetic score failure", fixed = TRUE)

  testthat::local_mocked_bindings(
    lavTestScore = function(...) list(uni = data.frame()),
    .package = "lavaan"
  )
  empty <- nomologR:::nomo_invariance_score_test(fit, "metric")
  expect_equal(nrow(empty$table), 0L)

  testthat::local_mocked_bindings(
    lavTestScore = function(...) {
      warning("synthetic score warning")
      list(uni = data.frame(foo = 1:2))
    },
    .package = "lavaan"
  )
  sparse <- nomologR:::nomo_invariance_score_test(fit, "metric")
  expect_equal(nrow(sparse$table), 2L)
  expect_true(all(is.na(sparse$table$score_x2)))
  expect_true(length(sparse$warning) >= 1L)
})


test_that("closeout: invariance decision log includes explicit missing-data configuration", {
  fit_evidence <- tibble::tibble(
    level = "configural",
    status = "estimated",
    constraints = "none",
    partial_requested = "",
    cfi = .95,
    rmsea = .05,
    srmr = .04,
    delta_cfi = NA_real_,
    delta_rmsea = NA_real_,
    delta_srmr = NA_real_,
    lrt_p = NA_real_
  )
  log <- nomologR:::nomo_invariance_decision_log(
    group = "g",
    groups = c("A", "B"),
    requested_levels = "configural",
    fit_evidence = fit_evidence,
    estimator = "MLR",
    missing = "fiml",
    ID.fac = "std.lv",
    ID.cat = "Wu.Estabrook.2016",
    parameterization = "theta",
    ordered = character(),
    category_table = tibble::tibble(),
    sequence_note = "Synthetic sequence.",
    partial = NULL,
    localize = FALSE,
    score_diagnostics = NULL
  )
  expect_true(any(log$metric == "missing"))
})


test_that("closeout: invariance public validation covers model, ordered, identification, and guidance guards", {
  dat <- lavaan::HolzingerSwineford1939
  model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "

  expect_error(
    nomo_invariance(model, dat, group = "school", ordered = 1),
    "`ordered`"
  )
  expect_error(
    nomo_invariance(model, dat, group = "school", ID.cat = ""),
    "`ID.cat`"
  )
  expect_error(
    nomo_invariance(model, dat, group = "school", parameterization = ""),
    "`parameterization`"
  )
  expect_error(
    nomo_invariance(model, dat, group = "school", guidance = 1),
    "`guidance`"
  )
  expect_error(
    nomo_invariance(
      model,
      dat,
      group = "school",
      ordered = c("x1", "x2", "x3"),
      ID.fac = "marker"
    ),
    "Wu-Estabrook"
  )

  nm <- nomo_model(list(visual = c("x1", "x2", "x3")))
  out <- nomo_invariance(
    nm,
    dat,
    group = "school",
    levels = "configural"
  )
  expect_s3_class(out, "nomo_invariance")
})


test_that("closeout: invariance pretty-constraint covers label fallbacks and unmatched labels", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- make_m9_full_report_run()$results$invariance$fits$metric

  expect_identical(
    nomologR:::nomo_invariance_pretty_constraint("a == b == c", fit),
    "a == b == c"
  )

  testthat::local_mocked_bindings(
    parTable = function(...) data.frame(),
    .package = "lavaan"
  )
  expect_identical(
    nomologR:::nomo_invariance_pretty_constraint(".p1. == .p2.", fit),
    ".p1. == .p2."
  )

  pt <- data.frame(
    lhs = c("F", "F"),
    op = c("=~", "=~"),
    rhs = c("x1", "x1"),
    group = c(1L, 2L),
    label = c(".p1.", ".p2.")
  )
  testthat::local_mocked_bindings(
    parTable = function(...) pt,
    lavInspect = function(...) c("A", "B"),
    .package = "lavaan"
  )
  same <- nomologR:::nomo_invariance_pretty_constraint(".p1. == .p2.", fit)
  expect_match(same, "Loading:", fixed = TRUE)

  expect_identical(
    nomologR:::nomo_invariance_pretty_constraint(".missing. == .p2.", fit),
    ".missing. == .p2."
  )

  pt2 <- data.frame(
    lhs = c("F", "x2"),
    op = c("=~", "~1"),
    rhs = c("x1", ""),
    group = c(1L, 2L),
    label = c(".p1.", ".p2.")
  )
  testthat::local_mocked_bindings(
    parTable = function(...) pt2,
    lavInspect = function(...) c("A", "B"),
    .package = "lavaan"
  )
  different <- nomologR:::nomo_invariance_pretty_constraint(".p1. == .p2.", fit)
  expect_match(different, "=", fixed = TRUE)
})


test_that("closeout: factor helpers cover singular adequacy and RNG cleanup", {
  singular <- matrix(
    1,
    3, 3,
    dimnames = list(c("a", "b", "c"), c("a", "b", "c"))
  )
  kmo <- nomologR:::nomo_factors_kmo(singular, c("a", "b", "c"))
  expect_false(kmo$available)

  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".Random.seed", envir = .GlobalEnv)
  }
  expect_identical(
    nomologR:::nomo_factors_with_seed(2026L, 42L),
    42L
  )
  expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
})


test_that("closeout: parallel analysis fails transparently when no null iterations are usable", {
  dat <- data.frame(
    a = rep(1, 20),
    b = rep(1, 20),
    c = rep(1, 20)
  )
  types <- tibble::tibble(
    item = names(dat),
    model_type = "continuous"
  )
  expect_error(
    suppressWarnings(
      nomologR:::nomo_factors_parallel(
        x = dat,
        model_types = types,
        method = "pearson",
        use = "pairwise.complete.obs",
        observed = c(2, 1, .5),
        n_iter = 10L,
        quantile = .95,
        parallel_rule = "percentile",
        seed = 2026L,
        fm = "minres"
      )
    ),
    "usable null iterations"
  )
})


test_that("closeout: factor criterion synthesis covers legacy fallback and unresolved families", {
  legacy_parallel <- tibble::tibble(
    criterion = "parallel",
    method = "Parallel analysis",
    family = "parallel",
    family_method = "Parallel analysis",
    n_factors = 2L,
    role = "legacy",
    reference = "synthetic"
  )
  syn <- nomologR:::nomo_factors_synthesis(
    evidence = legacy_parallel,
    parallel_n = 2L
  )
  expect_true(2L %in% syn$plausible_factors)

  conflicted <- tibble::tibble(
    criterion = c("map_original", "map_revised"),
    method = c("MAP original", "MAP revised"),
    family = c("map", "map"),
    family_method = c("MAP", "MAP"),
    n_factors = c(1L, 2L),
    role = c("complementary", "complementary"),
    reference = c("a", "b")
  )
  syn2 <- nomologR:::nomo_factors_synthesis(
    evidence = conflicted,
    parallel_n = 1L
  )
  expect_equal(nrow(syn2$family_concordance), 0L)
  expect_identical(syn2$modal_factors, 1L)
})


test_that("closeout: unavailable extended factor criteria are recorded as skipped", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- as.data.frame(matrix(rnorm(180), ncol = 6))
  names(dat) <- paste0("x", 1:6)
  corr <- stats::cor(dat)
  types <- tibble::tibble(
    item = names(dat),
    model_type = "continuous"
  )
  pa <- list(n_factors = 1L)
  map <- list(n_factors_original = 1L, n_factors_revised = 1L)

  unavailable <- function(...) list(
    available = FALSE,
    n_factors = NA_integer_,
    detail = NULL,
    reason = "synthetic unavailable"
  )

  testthat::local_mocked_bindings(
    nomo_factors_nest = unavailable,
    nomo_factors_hull = unavailable,
    nomo_factors_cd = unavailable,
    .package = "nomologR"
  )

  out <- nomologR:::nomo_factors_build_criteria(
    criterion_set = "all",
    pa = pa,
    map = map,
    corr = corr,
    analysis_data = dat,
    item_types = types,
    correlation_method = "pearson",
    common_n_available = TRUE,
    max_factors = 3L,
    n_iter = 10L,
    quantile = .95,
    seed = 2026L,
    guidance = nomo_defaults(),
    component_values = eigen(corr, symmetric = TRUE)$values
  )
  expect_true(all(
    c("nest", "hull", "comparison_data") %in%
      out$status$criterion[out$status$status == "skipped"]
  ))
})


test_that("closeout: EFA item summary uses documented fallback references", {
  pattern <- matrix(
    c(.6, .2, .3, .5),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(c("i1", "i2"), c("F1", "F2"))
  )
  out <- nomologR:::nomo_efa_item_summary(
    pattern = pattern,
    communality = c(.40, .34),
    uniqueness = c(.60, .66),
    complexity = c(1.2, 1.4),
    guidance = list()
  )
  expect_equal(nrow(out), 2L)
})


test_that("closeout: CFA accepts nomo_model and rejects non-list guidance", {
  dat <- lavaan::HolzingerSwineford1939
  nm <- nomo_model(list(visual = c("x1", "x2", "x3")))
  out <- nomo_cfa(nm, dat, modification_indices = FALSE)
  expect_s3_class(out, "nomo_cfa")

  expect_error(
    nomo_cfa(
      "visual =~ x1 + x2 + x3",
      dat,
      guidance = 1
    ),
    "`guidance`"
  )
})


test_that("closeout: CFA captures residual and modification-index warnings and MI errors", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- lavaan::HolzingerSwineford1939
  model <- "visual =~ x1 + x2 + x3"
  original_residuals <- lavaan::lavResiduals
  original_mi <- lavaan::modificationIndices

  testthat::local_mocked_bindings(
    lavResiduals = function(...) {
      warning("synthetic residual warning")
      original_residuals(...)
    },
    modificationIndices = function(...) {
      warning("synthetic MI warning")
      original_mi(...)
    },
    .package = "lavaan"
  )
  warned <- nomo_cfa(model, dat)
  expect_true(any(grepl("synthetic", warned$engine_warnings, fixed = TRUE)))

  testthat::local_mocked_bindings(
    modificationIndices = function(...) stop("synthetic MI error"),
    .package = "lavaan"
  )
  failed_mi <- nomo_cfa(model, dat)
  expect_equal(nrow(failed_mi$modification_indices), 0L)
  mi_log <- failed_mi$decision_log[
    failed_mi$decision_log$metric == "modification_indices",
    ,
    drop = FALSE
  ]
  expect_gt(nrow(mi_log), 0L)
  expect_true(any(grepl(
    "synthetic MI error",
    mi_log$observation,
    fixed = TRUE
  )))
})


test_that("closeout: report table helpers return schema-correct empty fallbacks", {
  m <- matrix(1:4, 2, 2)
  flat <- nomologR:::nomo_report_flatten_table(m)
  expect_s3_class(flat, "data.frame")

  run <- make_m9_full_report_run()
  stages <- c(
    "screen", "factors", "efa", "cfa",
    "reliability", "validity", "invariance", "network"
  )

  for (stage in stages) {
    comps <- nomologR:::nomo_report_component_list(run, stage)
    if (length(comps)) {
      empty <- nomologR:::nomo_report_component_table(
        comps[[1L]],
        stage = stage,
        type = "not_a_real_table"
      )
      expect_equal(nrow(empty), 0L)
    }
  }
})


test_that("closeout: report deviations cover sparse post-hoc and workflow-decision schemas", {
  run <- make_m9_minimal_run()
  run$results$network <- list(
    hypothesis_evidence = tibble::tibble(
      confirmatory_status = "post_hoc_exploratory"
    )
  )
  run$decision_log <- tibble::tibble(
    id = "post_decision",
    decision = "revise"
  )

  dev <- nomologR:::nomo_report_deviations(run)
  expect_true(any(dev$type == "post-hoc nomological relation"))
  expect_true(any(dev$type == "workflow deviation/revision decision"))
})


test_that("closeout: report package-version and citation helpers expose package metadata", {
  versions <- nomologR:::nomo_report_package_versions()
  expect_s3_class(versions, "data.frame")
  expect_true(all(c("package", "version") %in% names(versions)))
  expect_true("nomologR" %in% versions$package)

  cites <- nomologR:::nomo_report_citations()
  expect_s3_class(cites, "data.frame")
  expect_true(all(c("package", "installed_version", "citation") %in% names(cites)))
  expect_true("nomologR" %in% cites$package)
})


test_that("closeout: report development-template fallback and missing-template guard are explicit", {

  dev_root <- tempfile("nomo-report-dev-")

  dev_template <- file.path(
    dev_root,
    "inst",
    "rmarkdown",
    "nomo-report.Rmd"
  )

  dir.create(
    dirname(dev_template),
    recursive = TRUE,
    showWarnings = FALSE
  )

  writeLines(
    'title: "__NOMO_REPORT_TITLE__"',
    dev_template
  )

  path <- nomologR:::nomo_report_template_path(
    installed = "",
    development = dev_template
  )

  expect_identical(path, dev_template)
  expect_true(file.exists(path))

  missing_template <- file.path(
    tempfile("nomo-report-missing-"),
    "inst",
    "rmarkdown",
    "nomo-report.Rmd"
  )

  expect_error(
    nomologR:::nomo_report_template_path(
      installed = "",
      development = missing_template
    ),
    "Could not locate"
  )
})


test_that("closeout: report template preparation fails closed when copy is impossible", {
  src <- tempfile(fileext = ".Rmd")
  writeLines('title: "__NOMO_REPORT_TITLE__"', src)
  impossible <- file.path(tempfile(), "child", "report.Rmd")
  expect_error(
    suppressWarnings(
      nomologR:::nomo_report_prepare_template(src, impossible, "Title")
    ),
    "Could not prepare"
  )
})


test_that("closeout: report rendering dependency and output-directory guards are explicit", {
  run <- make_m9_report_run()

  bad_parent <- tempfile()
  writeLines("not a directory", bad_parent)
  out <- file.path(bad_parent, "report.html")
  expect_error(
    nomo_report(run, file = out, overwrite = TRUE, quiet = TRUE),
    "Could not create report output directory"
  )
})


# ---- pre-v0.1 exact-zero-map closeout B -------------------------------------

test_that("closeout B: factor retention rejects non-finite correlation matrices and keeps explicit defaults", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- data.frame(
    a = rnorm(40),
    b = rnorm(40),
    c = rnorm(40)
  )

  testthat::local_mocked_bindings(
    nomo_factors_correlation = function(...) {
      matrix(
        c(
          1, NA, 0,
          NA, 1, 0,
          0, 0, 1
        ),
        3, 3
      )
    },
    .package = "nomologR"
  )

  expect_error(
    nomo_factors(
      dat,
      n_iter = 10L,
      criterion_set = "minimal",
      correlation = "pearson"
    ),
    "non-finite values"
  )

  expect_identical(nomologR:::nomo_null_default(5L, 1L), 5L)
})


test_that("closeout B: parallel analysis preserves an existing RNG state and handles all-retained solutions", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- data.frame(a = 1:20, b = 21:40, c = 41:60)

  testthat::local_mocked_bindings(
    nomo_factors_correlation = function(...) diag(3),
    nomo_factors_factor_eigenvalues = function(...) c(.5, .4, .3),
    .package = "nomologR"
  )

  set.seed(711)
  before <- .Random.seed

  out <- nomologR:::nomo_factors_parallel(
    x = x,
    model_types = rep("continuous", 3L),
    method = "pearson",
    use = "pairwise.complete.obs",
    observed = c(10, 10, 10),
    n_iter = 10L,
    quantile = .95,
    parallel_rule = "percentile",
    seed = 2026L,
    fm = "minres"
  )

  expect_identical(.Random.seed, before)
  expect_identical(out$n_factors, 3L)
})


test_that("closeout B: parallel analysis records smoothed null matrices", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- data.frame(a = 1:20, b = 21:40, c = 41:60)
  singular <- matrix(1, 3, 3)

  testthat::local_mocked_bindings(
    nomo_factors_correlation = function(...) singular,
    nomo_factors_factor_eigenvalues = function(...) c(.5, .4, .3),
    .package = "nomologR"
  )

  out <- suppressWarnings(
    nomologR:::nomo_factors_parallel(
      x = x,
      model_types = rep("continuous", 3L),
      method = "pearson",
      use = "pairwise.complete.obs",
      observed = c(1, .6, .2),
      n_iter = 10L,
      quantile = .95,
      parallel_rule = "percentile",
      seed = 2026L,
      fm = "minres"
    )
  )

  expect_equal(out$n_smoothed_null, 10L)
})


test_that("closeout B: parallel analysis skips null matrices that cannot be smoothed", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- data.frame(a = 1:20, b = 21:40, c = 41:60)
  singular <- matrix(1, 3, 3)

  testthat::local_mocked_bindings(
    nomo_factors_correlation = function(...) singular,
    .package = "nomologR"
  )
  testthat::local_mocked_bindings(
    cor.smooth = function(...) stop("synthetic smoothing failure"),
    .package = "psych"
  )

  expect_error(
    nomologR:::nomo_factors_parallel(
      x = x,
      model_types = rep("continuous", 3L),
      method = "pearson",
      use = "pairwise.complete.obs",
      observed = c(1, .6, .2),
      n_iter = 10L,
      quantile = .95,
      parallel_rule = "percentile",
      seed = 2026L,
      fm = "minres"
    ),
    "usable null iterations"
  )
})


test_that("closeout B: parallel analysis rejects unusable factor eigenvalues", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- data.frame(a = 1:20, b = 21:40, c = 41:60)

  testthat::local_mocked_bindings(
    nomo_factors_correlation = function(...) diag(3),
    nomo_factors_factor_eigenvalues = function(...) c(1, NA_real_, .5),
    .package = "nomologR"
  )

  expect_error(
    nomologR:::nomo_factors_parallel(
      x = x,
      model_types = rep("continuous", 3L),
      method = "pearson",
      use = "pairwise.complete.obs",
      observed = c(1, .6, .2),
      n_iter = 10L,
      quantile = .95,
      parallel_rule = "percentile",
      seed = 2026L,
      fm = "minres"
    ),
    "usable null iterations"
  )
})


test_that("closeout B: parallel analysis retains a qualified partial set of usable null iterations", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- data.frame(a = 1:30, b = 31:60, c = 61:90)
  calls <- 0L

  testthat::local_mocked_bindings(
    nomo_factors_correlation = function(...) diag(3),
    nomo_factors_factor_eigenvalues = function(...) {
      calls <<- calls + 1L
      if (calls <= 16L) c(.5, .4, .3) else NULL
    },
    .package = "nomologR"
  )

  out <- nomologR:::nomo_factors_parallel(
    x = x,
    model_types = rep("continuous", 3L),
    method = "pearson",
    use = "pairwise.complete.obs",
    observed = c(1, .6, .2),
    n_iter = 20L,
    quantile = .95,
    parallel_rule = "percentile",
    seed = 2026L,
    fm = "minres"
  )

  expect_equal(out$n_valid, 16L)
  expect_equal(nrow(out$random_eigenvalues), 16L)
})


test_that("closeout B: MAP truncation and unavailable adequacy engines are explicit", {
  map <- nomologR:::nomo_factors_map(
    corr = matrix(1, 3, 3),
    max_factors = 2L
  )
  expect_true(map$truncated)

  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    KMO = function(...) stop("synthetic KMO failure"),
    .package = "psych"
  )
  kmo <- nomologR:::nomo_factors_kmo(diag(3), c("a", "b", "c"))
  expect_false(kmo$available)

  testthat::local_mocked_bindings(
    cortest.bartlett = function(...) stop("synthetic Bartlett failure"),
    .package = "psych"
  )
  bart <- nomologR:::nomo_factors_bartlett(diag(3), n = 100L, available = TRUE)
  expect_false(bart$available)
  expect_match(bart$reason, "could not be computed", fixed = TRUE)
})


test_that("closeout B: KMO decision-log severity distinguishes concern and review", {
  make_log <- function(kmo_value) {
    nomologR:::nomo_factors_log(
      item_types = tibble::tibble(
        item = "x",
        model_type = "continuous",
        source = "inferred"
      ),
      requested_correlation = "pearson",
      correlation_method = "pearson",
      missing = "complete",
      min_pairwise_n = 100L,
      smoothed = FALSE,
      original_min_eigen = .50,
      kmo = list(
        available = TRUE,
        overall = kmo_value,
        item = tibble::tibble(item = "x", msa = kmo_value)
      ),
      bartlett = list(
        available = FALSE,
        reason = "Synthetic unavailable."
      ),
      pa = list(
        n_factors = 1L,
        rule = "percentile",
        quantile = .95,
        sensitivity = tibble::tibble(
          rule = c("percentile", "mean", "crawford"),
          n_factors = c(1L, 1L, 1L)
        )
      ),
      map = list(
        n_factors_original = 1L,
        n_factors_revised = 1L,
        truncated = FALSE,
        m_last = 1L
      ),
      criteria = list(
        status = tibble::tibble(),
        evidence = tibble::tibble()
      ),
      synthesis = list(
        support_for_primary = 1L,
        agreement = "convergent",
        text = "Synthetic convergence."
      ),
      guidance = nomo_defaults()
    )
  }

  concern <- make_log(.40)
  review <- make_log(.55)

  expect_identical(
    concern$severity[concern$metric == "kmo"][[1L]],
    "concern"
  )
  expect_identical(
    review$severity[review$metric == "kmo"][[1L]],
    "review"
  )
})


test_that("closeout B: EKC and comparison-data helpers preserve no-suggestion outcomes", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    efa_ekc = function(...) list(n_factors = NA_real_),
    .package = "EFAtools"
  )
  ekc <- nomologR:::nomo_factors_ekc(diag(3), n_obs = 100L)
  expect_false(ekc$available)
  expect_match(ekc$reason, "no usable", fixed = TRUE)

  testthat::local_mocked_bindings(
    efa_cd = function(...) list(n_factors = NA_real_),
    .package = "EFAtools"
  )
  cd <- nomologR:::nomo_factors_cd(
    x = data.frame(
      a = rnorm(40),
      b = rnorm(40),
      c = rnorm(40)
    ),
    max_factors = 2L,
    n_population = 100L,
    n_samples = 10L,
    alpha = .30,
    seed = 2026L
  )
  expect_false(cd$available)
  expect_match(cd$reason, "no usable", fixed = TRUE)
})


test_that("closeout B: EFA estimation errors are wrapped with component context", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- data.frame(
    a = rnorm(80),
    b = rnorm(80),
    c = rnorm(80),
    d = rnorm(80)
  )

  testthat::local_mocked_bindings(
    fa = function(...) stop("synthetic EFA engine failure"),
    .package = "psych"
  )

  expect_error(
    nomo_efa(
      dat,
      factors = 1L,
      rotation = "varimax",
      correlation = "pearson",
      missing = "complete"
    ),
    "EFA estimation failed"
  )
})


test_that("closeout B: EFA derives structure, communalities, uniqueness, and complexity when engines omit them", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  set.seed(6201)
  n <- 90
  f1 <- rnorm(n)
  f2 <- .30 * f1 + sqrt(1 - .30^2) * rnorm(n)
  dat <- data.frame(
    a1 = .80 * f1 + rnorm(n, sd = .60),
    a2 = .75 * f1 + rnorm(n, sd = .65),
    a3 = .70 * f1 + rnorm(n, sd = .70),
    b1 = .80 * f2 + rnorm(n, sd = .60),
    b2 = .75 * f2 + rnorm(n, sd = .65),
    b3 = .70 * f2 + rnorm(n, sd = .70)
  )

  original_fa <- psych::fa
  testthat::local_mocked_bindings(
    fa = function(...) {
      z <- original_fa(...)
      z$Structure <- NULL
      z$communality <- NULL
      z$communalities <- NULL
      z$uniquenesses <- NULL
      z$complexity <- NULL
      z
    },
    .package = "psych"
  )

  guidance <- nomo_defaults()
  guidance$factor_small_n_reference <- NULL

  out <- nomo_efa(
    dat,
    factors = 2L,
    rotation = "oblimin",
    correlation = "pearson",
    missing = "complete",
    guidance = guidance
  )

  expect_equal(dim(out$structure_matrix), dim(out$pattern_matrix))
  expect_true(all(is.finite(out$item_summary$communality)))
  expect_true(any(out$decision_log$metric == "sample_size"))
})


test_that("closeout B: EFA uses an engine communalities alias when the primary field is absent", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  set.seed(6202)
  dat <- as.data.frame(matrix(rnorm(480), ncol = 6))
  names(dat) <- paste0("x", 1:6)

  original_fa <- psych::fa
  testthat::local_mocked_bindings(
    fa = function(...) {
      z <- original_fa(...)
      fallback <- if (!is.null(z$communality)) {
        z$communality
      } else {
        rep(.50, nrow(unclass(z$loadings)))
      }
      z$communality <- NULL
      z$communalities <- fallback
      z
    },
    .package = "psych"
  )

  out <- nomo_efa(
    dat,
    factors = 1L,
    rotation = "varimax",
    correlation = "pearson",
    missing = "complete"
  )
  expect_true(all(is.finite(out$item_summary$communality)))
})


test_that("closeout B: EFA presentation covers researcher, review, unavailable adequacy, and orthogonal branches", {
  item_summary <- tibble::tibble(
    item = c("i1", "i2"),
    primary_factor = c("F1", "F2"),
    primary_loading = c(.70, .60),
    secondary_factor = c("F2", "F1"),
    secondary_loading = c(.10, .20),
    communality = c(.50, .45),
    attention = c("KEEP", "REVIEW"),
    explanation = c("No review.", "Synthetic review.")
  )

  efa <- structure(
    list(
      factor_source = "researcher",
      n_cases = 80L,
      n_items = 2L,
      n_factors = 2L,
      correlation = "pearson",
      fm = "minres",
      rotation = "varimax",
      rmsr = .05,
      item_summary = item_summary,
      pattern_matrix = matrix(
        0,
        2, 2,
        dimnames = list(c("i1", "i2"), c("F1", "F2"))
      ),
      items = c("i1", "i2"),
      oblique = FALSE,
      factor_correlations = matrix(
        c(1, 0, 0, 1),
        2, 2,
        dimnames = list(c("F1", "F2"), c("F1", "F2"))
      ),
      residual_matrix = matrix(
        0,
        2, 2,
        dimnames = list(c("i1", "i2"), c("i1", "i2"))
      ),
      residual_pairs = tibble::tibble(),
      kmo = list(available = FALSE, overall = NA_real_),
      bartlett = list(
        available = TRUE,
        df = 1,
        chisq = 2,
        p_value = .123
      ),
      factor_context = NULL,
      extraction_note = "",
      structure_matrix = matrix(
        0,
        2, 2,
        dimnames = list(c("i1", "i2"), c("F1", "F2"))
      ),
      sample_adequacy = tibble::tibble(),
      decision_log = tibble::tibble(),
      guidance = nomo_defaults()
    ),
    class = c("nomo_efa", "list")
  )

  print_text <- paste(capture.output(print(efa)), collapse = "\n")
  expect_match(print_text, "researcher specified", fixed = TRUE)

  s <- summary(efa)
  summary_text <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(summary_text, "researcher specified", fixed = TRUE)
  expect_match(summary_text, "KMO unavailable", fixed = TRUE)
  expect_match(summary_text, "p = 0.123", fixed = TRUE)
  expect_match(summary_text, "Items requiring review", fixed = TRUE)
  expect_match(summary_text, "Factor correlations", fixed = TRUE)

  expect_s3_class(plot(efa, type = "pattern"), "ggplot")
  expect_s3_class(plot(efa, type = "residuals"), "ggplot")
  expect_s3_class(plot(efa, type = "factor_correlations"), "ggplot")
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


test_that("closeout B: model and screening type helpers cover missing factor names and ordered binary items", {
  bad <- list(c("x1", "x2"), c("x3", "x4"))
  names(bad) <- c("F1", NA_character_)
  expect_error(
    nomo_model(bad),
    "unique, non-empty factor names"
  )

  binary_ordered <- ordered(
    c("no", "yes", "no", "yes"),
    levels = c("no", "yes")
  )
  expect_identical(
    nomologR:::nomo_screen_item_type(binary_ordered),
    "binary"
  )
})


test_that("closeout B: screen evidence aliases floor and ceiling concentration to one presentation metric", {
  scr <- nomo_screen(
    data.frame(x = c(1, 1, 1, 1, 2, 3)),
    items = "x"
  )

  scr$decision_log <- nomologR:::nomo_log_add(
    scr$decision_log,
    stage = "screen",
    object = "x",
    metric = "floor_concentration",
    severity = "review"
  )

  dat <- nomologR:::nomo_screen_evidence_data(scr, "x")
  row <- dat[
    dat$metric == "response_concentration",
    ,
    drop = FALSE
  ]
  expect_identical(as.character(row$severity[[1L]]), "review")
})


test_that("closeout B: reliability alpha engine errors remain specific to the requested estimand", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa
  original_comp <- semTools::compRelSEM
  calls <- 0L

  testthat::local_mocked_bindings(
    compRelSEM = function(...) {
      calls <<- calls + 1L
      if (calls == 1L) {
        return(original_comp(...))
      }
      stop("synthetic alpha failure")
    },
    .package = "semTools"
  )

  expect_error(
    nomo_reliability(cfa, include_alpha = TRUE),
    "Coefficient-alpha estimation failed"
  )
})


test_that("closeout B: continuous-alpha failure is explicit in a model containing separate ordered and continuous composites", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  set.seed(6203)
  n <- 320
  f1 <- rnorm(n)
  f2 <- .25 * f1 + sqrt(1 - .25^2) * rnorm(n)

  a1 <- .80 * f1 + rnorm(n, sd = .60)
  a2 <- .75 * f1 + rnorm(n, sd = .65)
  a3 <- .70 * f1 + rnorm(n, sd = .70)

  dat <- data.frame(
    A1 = ordered(cut(a1, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    A2 = ordered(cut(a2, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    A3 = ordered(cut(a3, c(-Inf, -.5, .5, Inf), labels = FALSE)),
    B1 = .80 * f2 + rnorm(n, sd = .60),
    B2 = .75 * f2 + rnorm(n, sd = .65),
    B3 = .70 * f2 + rnorm(n, sd = .70)
  )

  fit <- lavaan::cfa(
    "
      F1 =~ A1 + A2 + A3
      F2 =~ B1 + B2 + B3
    ",
    data = dat,
    ordered = c("A1", "A2", "A3"),
    estimator = "WLSMV"
  )
  expect_true(lavaan::lavInspect(fit, "converged"))

  original_comp <- semTools::compRelSEM
  testthat::local_mocked_bindings(
    compRelSEM = function(..., tau.eq = FALSE) {
      if (is.character(tau.eq)) {
        stop("synthetic continuous-alpha failure")
      }
      original_comp(..., tau.eq = tau.eq)
    },
    .package = "semTools"
  )

  expect_error(
    nomo_reliability(
      fit,
      ordinal_scale = TRUE,
      include_alpha = TRUE
    ),
    "Coefficient-alpha estimation failed for continuous composites"
  )
})


test_that("closeout B: reliability marks an improper measurement model as a model-dependence concern", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa
  original_measurement_fit <- nomologR:::nomo_measurement_fit

  testthat::local_mocked_bindings(
    nomo_measurement_fit = function(...) {
      z <- original_measurement_fit(...)
      z$post_check <- FALSE
      z
    },
    .package = "nomologR"
  )

  rel <- nomo_reliability(cfa, include_alpha = FALSE)
  dep <- rel$decision_log[
    rel$decision_log$metric == "model_dependence",
    ,
    drop = FALSE
  ]
  expect_gt(nrow(dep), 0L)
  expect_identical(dep$severity[[1L]], "concern")
  expect_match(dep$observation[[1L]], "admissibility", fixed = TRUE)
})


test_that("closeout B: CFA fallbacks recover names when standardized output is unavailable and nobs is unusable", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- lavaan::HolzingerSwineford1939
  original_inspect <- lavaan::lavInspect

  testthat::local_mocked_bindings(
    lavInspect = function(object, what, ...) {
      if (identical(what, "options")) return(list())
      if (identical(what, "nobs")) return(0)
      original_inspect(object, what, ...)
    },
    standardizedSolution = function(...) data.frame(),
    .package = "lavaan"
  )

  out <- nomo_cfa(
    "visual =~ x1 + x2 + x3",
    dat,
    estimator = "MLR",
    modification_indices = FALSE
  )

  expect_true(is.na(out$n_used))
  expect_true(is.na(out$n_dropped))
  expect_true(length(lavaan::lavNames(out$fit, type = "lv")) >= 1L)
})


test_that("closeout B: CFA loading and factor-correlation helpers fill optional statistics with NA", {
  loads <- nomologR:::nomo_cfa_loadings(
    data.frame(
      lhs = "F",
      op = "=~",
      rhs = "x1",
      est.std = .70
    ),
    guidance = nomo_defaults()
  )
  expect_true(is.na(loads$se[[1L]]))
  expect_true(is.na(loads$z[[1L]]))
  expect_true(is.na(loads$p_value[[1L]]))
  expect_true(is.na(loads$ci_lower[[1L]]))
  expect_true(is.na(loads$ci_upper[[1L]]))

  cors <- nomologR:::nomo_cfa_factor_correlations(
    data.frame(
      lhs = "F1",
      op = "~~",
      rhs = "F2",
      est.std = .30
    ),
    latent_names = c("F1", "F2")
  )
  expect_true(is.na(cors$se[[1L]]))
})


test_that("closeout B: CFA summary displays engine/requested estimator differences", {
  cfa <- make_m9_report_run()$results$cfa
  s <- summary(cfa)
  s$estimator <- "MLR"
  s$estimator_engine <- "ML"

  txt <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(txt, "engine: ML", fixed = TRUE)
})


test_that("closeout B: run decision validation covers empty and unnamed decision sets", {
  expect_identical(
    nomologR:::nomo_run_validate_decisions(list()),
    list()
  )
  expect_error(
    nomologR:::nomo_run_validate_decisions(list(1L)),
    "unique non-empty names"
  )
})


test_that("closeout B: factor decision requests describe fallback and unavailable plausible sets", {
  x <- make_m9_minimal_run()
  x$scales <- list(A = c("a1", "a2"), B = c("b1", "b2"))
  x$results$factors <- list(
    A = list(
      parallel = list(n_factors = 2L),
      plausible_factors = integer()
    ),
    B = list(
      parallel = list(n_factors = NA_integer_),
      plausible_factors = integer()
    )
  )

  req <- nomologR:::nomo_run_factor_requests(x)
  expect_equal(nrow(req), 2L)
  expect_true(any(grepl("2", req$observation, fixed = TRUE)))
  expect_true(any(grepl(
    "no compact plausible set was available",
    req$observation,
    fixed = TRUE
  )))
})


test_that("closeout B: downstream workflow stops immediately at blocked invariance or network branches", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- make_m9_minimal_run()

  testthat::local_mocked_bindings(
    nomo_run_run_invariance = function(z) {
      z$status <- "blocked"
      z
    },
    .package = "nomologR"
  )
  inv_block <- nomologR:::nomo_run_finish_downstream(x)
  expect_identical(inv_block$status, "blocked")

  testthat::local_mocked_bindings(
    nomo_run_run_invariance = function(z) {
      z$status <- "paused"
      z
    },
    nomo_run_run_network = function(z) {
      z$status <- "blocked"
      z
    },
    .package = "nomologR"
  )
  net_block <- nomologR:::nomo_run_finish_downstream(x)
  expect_identical(net_block$status, "blocked")
})


test_that("closeout B: run recipe identifies split-sample network roles", {
  x <- make_m9_minimal_run()
  x$sample_design <- "calibration_validation"

  recipe <- nomologR:::nomo_run_recipe_table(x)
  network <- recipe[recipe$stage == "network", , drop = FALSE]
  expect_identical(network$data_role[[1L]], "calibration + validation")
})


test_that("closeout B: fresh workflows block transparently when initial screening fails", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    nomo_screen = function(...) stop("synthetic screening failure"),
    .package = "nomologR"
  )

  out <- nomo_run(
    data = data.frame(i1 = 1:8, i2 = 8:1),
    scales = list(S = c("i1", "i2"))
  )
  expect_identical(out$status, "blocked")
  expect_identical(out$blocked$stage, "screen")
})


test_that("closeout B: network fitting captures warnings and explicit researcher estimator selection", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  dat <- lavaan::HolzingerSwineford1939
  model <- "visual =~ x1 + x2 + x3"
  h <- nomo_hypotheses("visual -> x4" = positive())
  original_sem <- lavaan::sem

  testthat::local_mocked_bindings(
    sem = function(...) {
      warning("synthetic SEM warning")
      original_sem(...)
    },
    .package = "lavaan"
  )

  out <- nomo_network(
    model,
    dat,
    h,
    estimator = "MLR"
  )

  expect_identical(out$estimator_source, "researcher")
  expect_true(any(grepl(
    "synthetic SEM warning",
    out$engine_warnings,
    fixed = TRUE
  )))
})


test_that("closeout B: network decision log classifies supportive replication as informational", {
  measurement <- list(
    summary = tibble::tibble(
      loading_review_flags = 0L,
      attention = "info",
      observation = "Synthetic measurement context."
    )
  )
  additions <- tibble::tibble(
    relation = "A -> B",
    syntax = "B ~ A",
    added_from_hypothesis = FALSE
  )
  evidence <- tibble::tibble(
    relation = "A -> B",
    concordance = "concordant",
    estimate = .30,
    theoretical_region = "(0, +Inf)",
    interpretation = "Synthetic concordance."
  )
  replication <- tibble::tibble(
    relation = "A -> B",
    replication_status = "replicated_concordance",
    estimate_shift = .01,
    interpretation = "Synthetic replication."
  )

  log <- nomologR:::nomo_network_decision_log(
    model_additions = additions,
    hypotheses_evidence = evidence,
    converged = TRUE,
    warnings = character(),
    estimator = NULL,
    ordered = character(),
    measurement_context = measurement,
    replication_evidence = replication,
    sample_role = "primary"
  )

  row <- log[
    log$stage == "network_replication",
    ,
    drop = FALSE
  ]
  expect_gt(nrow(row), 0L)
  expect_identical(row$severity[[1L]], "info")
})


test_that("closeout B: network presentation covers zero-span theory regions and populated replication summaries", {
  net <- make_m9_full_report_run()$results$network

  zero_net <- net
  zero_net$hypothesis_evidence$estimate[] <- 0
  zero_net$hypothesis_evidence$ci_lower[] <- 0
  zero_net$hypothesis_evidence$ci_upper[] <- 0
  zero_net$hypotheses$hypotheses$lower[] <- -Inf
  zero_net$hypotheses$hypotheses$upper[] <- Inf

  prepared <- nomologR:::nomo_network_theory_plot_data(zero_net)
  expect_equal(prepared$limits, c(-.1, .1))

  net$validation_n <- 100L
  net$replication_evidence <- tibble::tibble(
    id = "H1",
    relation = net$hypothesis_evidence$relation[[1L]],
    prediction = net$hypothesis_evidence$prediction[[1L]],
    primary_estimate = .30,
    validation_estimate = .28,
    estimate_shift = -.02,
    primary_concordance = "concordant",
    validation_concordance = "concordant",
    replication_status = "replicated_concordance",
    interpretation = "Synthetic replicated concordance."
  )

  s <- summary(net)
  expect_gt(nrow(s$replication_counts), 0L)

  txt <- paste(capture.output(print(s)), collapse = "\n")
  expect_match(txt, "Validation N: 100", fixed = TRUE)
  expect_match(txt, "Replication evidence", fixed = TRUE)

  expect_s3_class(plot(net, type = "replication"), "ggplot")
})


test_that("closeout B: invariance LRT captures warnings and missing finite comparison values", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    lavTestLRT = function(...) {
      warning("synthetic LRT warning")
      data.frame(
        "Chisq diff" = c(NA_real_, NA_real_),
        "Df diff" = c(NA_real_, NA_real_),
        "Pr(>Chisq)" = c(NA_real_, NA_real_),
        check.names = FALSE
      )
    },
    .package = "lavaan"
  )

  out <- nomologR:::nomo_invariance_lrt(list(), list())
  expect_true(is.na(out$chisq))
  expect_true(is.na(out$df))
  expect_true(is.na(out$p))
  expect_true(any(grepl(
    "synthetic LRT warning",
    out$warnings,
    fixed = TRUE
  )))
})


test_that("closeout B: invariance syntax-generation failures are wrapped by level", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    measEq.syntax = function(...) stop("synthetic syntax failure"),
    .package = "semTools"
  )

  expect_error(
    nomo_invariance(
      "
        visual =~ x1 + x2 + x3
        textual =~ x4 + x5 + x6
      ",
      lavaan::HolzingerSwineford1939,
      group = "school",
      levels = "configural",
      localize = FALSE
    ),
    "Could not generate configural invariance syntax"
  )
})


test_that("closeout B: invariance captures CFA warnings and unavailable fit-measure detail", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  original_cfa <- lavaan::cfa

  testthat::local_mocked_bindings(
    cfa = function(...) {
      warning("synthetic invariance CFA warning")
      original_cfa(...)
    },
    fitMeasures = function(...) stop("synthetic fit-measure failure"),
    .package = "lavaan"
  )

  out <- suppressWarnings(
    nomo_invariance(
      "
      visual =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ",
      lavaan::HolzingerSwineford1939,
      group = "school",
      levels = "configural",
      localize = FALSE
    )
  )

  expect_true(any(grepl(
    "synthetic invariance CFA warning",
    out$engine_warnings$configural,
    fixed = TRUE
  )))
  expect_length(out$fit_measures$configural, 0L)
})


test_that("closeout B: ordered invariance uses WLSMV and theta parameterization by default", {
  set.seed(6204)
  n <- 360
  latent <- rnorm(n)
  group <- rep(c("A", "B"), each = n / 2L)

  make_ordered <- function(lambda) {
    z <- lambda * latent + rnorm(n, sd = .75)
    ordered(
      cut(
        z,
        breaks = c(-Inf, -.60, 0, .60, Inf),
        labels = FALSE
      )
    )
  }

  dat <- data.frame(
    i1 = make_ordered(.80),
    i2 = make_ordered(.75),
    i3 = make_ordered(.70),
    group = group
  )

  out <- nomo_invariance(
    "F =~ i1 + i2 + i3",
    dat,
    group = "group",
    ordered = c("i1", "i2", "i3"),
    levels = "configural",
    localize = FALSE
  )

  expect_identical(out$estimator, "WLSMV")
  expect_identical(out$estimator_source, "ordered_default")
  expect_identical(out$parameterization, "theta")
})


test_that("closeout B: invariance pretty constraints cover label-only tables and group-free fallbacks", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  fit <- structure(list(), class = "synthetic_fit")

  same <- data.frame(
    lhs = c("F", "F"),
    op = c("=~", "=~"),
    rhs = c("x1", "x1"),
    group = c(0L, 0L),
    label = c(".p1.", ".p2."),
    stringsAsFactors = FALSE
  )

  testthat::local_mocked_bindings(
    parTable = function(...) same,
    lavInspect = function(...) character(),
    .package = "lavaan"
  )
  pretty_same <- nomologR:::nomo_invariance_pretty_constraint(
    ".p1. == .p2.",
    fit
  )
  expect_match(pretty_same, "Loading:", fixed = TRUE)
  expect_false(grepl("[", pretty_same, fixed = TRUE))

  no_labels <- same[, c("lhs", "op", "rhs", "group"), drop = FALSE]
  testthat::local_mocked_bindings(
    parTable = function(...) no_labels,
    .package = "lavaan"
  )
  expect_identical(
    nomologR:::nomo_invariance_pretty_constraint(
      ".p1. == .p2.",
      fit
    ),
    ".p1. == .p2."
  )

  different <- data.frame(
    lhs = c("F", "x2"),
    op = c("=~", "~1"),
    rhs = c("x1", ""),
    group = c(0L, 0L),
    label = c(".p1.", ".p2."),
    stringsAsFactors = FALSE
  )
  testthat::local_mocked_bindings(
    parTable = function(...) different,
    lavInspect = function(...) character(),
    .package = "lavaan"
  )
  pretty_different <- nomologR:::nomo_invariance_pretty_constraint(
    ".p1. == .p2.",
    fit
  )
  expect_match(pretty_different, "Loading:", fixed = TRUE)
  expect_match(pretty_different, "Intercept:", fixed = TRUE)
})


test_that("closeout B: validity handles an empty standardized-loading evidence table", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  cfa <- make_m9_report_run()$results$cfa

  testthat::local_mocked_bindings(
    nomo_validity_standardized_loadings = function(...) tibble::tibble(),
    .package = "nomologR"
  )

  out <- nomo_validity(cfa, htmt = "none")
  expect_equal(nrow(out$standardized_loadings), 0L)
  expect_false(any(out$decision_log$metric == "standardized_loading"))
})


test_that("closeout B: report deviation extraction handles sparse partial and workflow-decision schemas", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- make_m9_minimal_run()
  x$results$invariance <- structure(list(), class = "synthetic_invariance")
  x$results$network <- NULL
  x$decision_log <- tibble::tibble()

  testthat::local_mocked_bindings(
    nomo_table = function(object, type, ...) {
      if (identical(type, "partial")) {
        return(tibble::tibble(syntax = "x1 ~ 1"))
      }
      tibble::tibble()
    },
    .package = "nomologR"
  )

  partial <- nomologR:::nomo_report_deviations(x)
  expect_identical(partial$scope[[1L]], "")
  expect_identical(partial$rationale[[1L]], "")

  x2 <- make_m9_minimal_run()
  x2$results$invariance <- NULL
  x2$results$network <- NULL
  x2$decision_log <- tibble::tibble(id = "posthoc_deviation")

  workflow <- nomologR:::nomo_report_deviations(x2)
  expect_equal(nrow(workflow), 1L)
  expect_identical(workflow$detail[[1L]], "")
  expect_identical(workflow$rationale[[1L]], "")
})


test_that("closeout B: report package metadata helpers can represent unavailable packages and citation failures", {
  missing <- nomologR:::nomo_report_package_versions(
    packages = "definitely_not_a_real_nomologr_package",
    namespace_available = function(pkg) FALSE
  )
  expect_identical(missing$version[[1L]], "not installed")

  missing_citation <- nomologR:::nomo_report_citations(
    packages = "definitely_not_a_real_nomologr_package",
    namespace_available = function(pkg) FALSE
  )
  expect_identical(
    missing_citation$installed_version[[1L]],
    "not installed"
  )

  failed_citation <- nomologR:::nomo_report_citations(
    packages = "nomologR",
    namespace_available = function(pkg) TRUE,
    citation_fun = function(pkg) stop("synthetic citation failure"),
    version_fun = function(pkg) "0.0.0"
  )
  expect_match(
    failed_citation$citation[[1L]],
    "Citation unavailable: synthetic citation failure",
    fixed = TRUE
  )

  expect_true(nomologR:::nomo_report_namespace_available("base"))
  expect_type(nomologR:::nomo_report_pandoc_available(), "logical")
})


test_that("closeout B: report dependency guards distinguish rmarkdown, knitr, and Pandoc", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  run <- make_m9_report_run()

  testthat::local_mocked_bindings(
    nomo_report_namespace_available = function(pkg) FALSE,
    .package = "nomologR"
  )
  expect_error(
    nomo_report(
      run,
      file = tempfile(fileext = ".html"),
      overwrite = TRUE
    ),
    "requires the suggested package `rmarkdown`"
  )

  testthat::local_mocked_bindings(
    nomo_report_namespace_available = function(pkg) {
      !identical(pkg, "knitr")
    },
    .package = "nomologR"
  )
  expect_error(
    nomo_report(
      run,
      file = tempfile(fileext = ".html"),
      overwrite = TRUE
    ),
    "requires the suggested package `knitr`"
  )

  testthat::local_mocked_bindings(
    nomo_report_namespace_available = function(pkg) TRUE,
    nomo_report_pandoc_available = function() FALSE,
    .package = "nomologR"
  )
  expect_error(
    nomo_report(
      run,
      file = tempfile(fileext = ".html"),
      overwrite = TRUE
    ),
    "Pandoc is required"
  )
})

# ---- pre-v0.1 exact-zero-map closeout C -------------------------------------

test_that("closeout C: CFA loading helper returns its stable empty schema when no loading rows exist", {
  standardized <- data.frame(
    lhs = "F",
    op = "~~",
    rhs = "F",
    est.std = 1
  )

  out <- nomologR:::nomo_cfa_loadings(
    standardized,
    guidance = nomo_defaults()
  )

  expect_equal(nrow(out), 0L)
  expect_identical(
    names(out),
    c(
      "factor", "item", "loading", "se", "z", "p_value",
      "ci_lower", "ci_upper", "attention", "explanation"
    )
  )
})


test_that("closeout C: EFA presentation covers nomo_factors handoff and oblique pattern text", {
  item_summary <- tibble::tibble(
    item = c("i1", "i2"),
    primary_factor = c("F1", "F2"),
    primary_loading = c(.70, .65),
    secondary_factor = c("F2", "F1"),
    secondary_loading = c(.15, .10),
    communality = c(.50, .45),
    attention = c("KEEP", "KEEP"),
    explanation = c("No review.", "No review.")
  )

  efa <- structure(
    list(
      factor_source = "nomo_factors",
      n_cases = 100L,
      n_items = 2L,
      n_factors = 2L,
      correlation = "pearson",
      fm = "minres",
      rotation = "oblimin",
      rmsr = .04,
      item_summary = item_summary,
      pattern_matrix = matrix(
        c(.70, .15, .10, .65),
        2, 2,
        byrow = TRUE,
        dimnames = list(c("i1", "i2"), c("F1", "F2"))
      ),
      items = c("i1", "i2"),
      oblique = TRUE,
      factor_correlations = matrix(
        c(1, .30, .30, 1),
        2, 2,
        dimnames = list(c("F1", "F2"), c("F1", "F2"))
      ),
      residual_matrix = matrix(
        c(0, .02, .02, 0),
        2, 2,
        dimnames = list(c("i1", "i2"), c("i1", "i2"))
      ),
      residual_pairs = tibble::tibble(),
      kmo = list(available = FALSE, overall = NA_real_),
      bartlett = list(
        available = FALSE,
        df = NA_real_,
        chisq = NA_real_,
        p_value = NA_real_
      ),
      factor_context = list(),
      extraction_note = "",
      structure_matrix = matrix(
        c(.70, .36, .30, .65),
        2, 2,
        byrow = TRUE,
        dimnames = list(c("i1", "i2"), c("F1", "F2"))
      ),
      sample_adequacy = tibble::tibble(),
      decision_log = tibble::tibble(),
      guidance = nomo_defaults()
    ),
    class = c("nomo_efa", "list")
  )

  printed <- paste(capture.output(print(efa)), collapse = "\n")
  expect_match(printed, "nomo_factors() handoff", fixed = TRUE)

  summary_printed <- paste(
    capture.output(print(summary(efa))),
    collapse = "\n"
  )
  expect_match(summary_printed, "from nomo_factors()", fixed = TRUE)

  p <- plot(efa, type = "pattern")
  expect_s3_class(p, "ggplot")
  expect_match(
    p$labels$subtitle,
    "Oblique solution",
    fixed = TRUE
  )
})


test_that("closeout C: parallel analysis restores an initially absent RNG state", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- data.frame(a = 1:20, b = 21:40, c = 41:60)

  testthat::local_mocked_bindings(
    nomo_factors_correlation = function(...) diag(3),
    nomo_factors_factor_eigenvalues = function(...) c(.5, .4, .3),
    .package = "nomologR"
  )

  probe <- function() {
    old_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    if (old_exists) {
      old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }

    on.exit(
      {
        if (old_exists) {
          assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      },
      add = TRUE
    )

    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }

    out <- nomologR:::nomo_factors_parallel(
      x = x,
      model_types = rep("continuous", 3L),
      method = "pearson",
      use = "pairwise.complete.obs",
      observed = c(1, .6, .2),
      n_iter = 10L,
      quantile = .95,
      parallel_rule = "percentile",
      seed = 2026L,
      fm = "minres"
    )

    list(
      out = out,
      seed_exists_after = exists(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )
    )
  }

  result <- probe()
  expect_false(result$seed_exists_after)
  expect_equal(result$out$n_valid, 10L)
})


test_that("closeout C: parallel analysis skips null matrices whose eigen decomposition fails", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  x <- data.frame(a = 1:20, b = 21:40, c = 41:60)

  testthat::local_mocked_bindings(
    nomo_factors_correlation = function(...) {
      matrix(seq_len(6), nrow = 2L, ncol = 3L)
    },
    .package = "nomologR"
  )

  expect_error(
    nomologR:::nomo_factors_parallel(
      x = x,
      model_types = rep("continuous", 3L),
      method = "pearson",
      use = "pairwise.complete.obs",
      observed = c(1, .6, .2),
      n_iter = 10L,
      quantile = .95,
      parallel_rule = "percentile",
      seed = 2026L,
      fm = "minres"
    ),
    "usable null iterations"
  )
})


test_that("closeout C: null-default helper covers both NULL and zero-length fallback inputs", {
  expect_identical(
    nomologR:::nomo_null_default(NULL, 7L),
    7L
  )
  expect_identical(
    nomologR:::nomo_null_default(integer(), 7L),
    7L
  )
})


test_that("closeout C: invariance LRT returns NA when expected comparison columns are absent", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  testthat::local_mocked_bindings(
    lavTestLRT = function(...) {
      data.frame(
        arbitrary = c(1, 2),
        another = c(3, 4)
      )
    },
    .package = "lavaan"
  )

  out <- nomologR:::nomo_invariance_lrt(list(), list())

  expect_true(is.na(out$chisq))
  expect_true(is.na(out$df))
  expect_true(is.na(out$p))
})


test_that("closeout C: invariance records researcher estimator and missing-data choices", {
  out <- nomo_invariance(
    "
      visual =~ x1 + x2 + x3
      textual =~ x4 + x5 + x6
    ",
    lavaan::HolzingerSwineford1939,
    group = "school",
    levels = "configural",
    estimator = "MLR",
    missing = "listwise",
    localize = FALSE
  )

  expect_identical(out$estimator_source, "researcher")
  expect_identical(out$estimator, "MLR")
  expect_identical(out$missing, "listwise")
})


test_that("closeout C: invariance fit failures are retained as evidence instead of escaping the workflow", {
  skip_if_not(
    exists("local_mocked_bindings", envir = asNamespace("testthat"), inherits = FALSE)
  )

  model <- "
    visual =~ x1 + x2 + x3
    textual =~ x4 + x5 + x6
  "

  testthat::local_mocked_bindings(
    measEq.syntax = function(...) model,
    .package = "semTools"
  )

  testthat::local_mocked_bindings(
    cfa = function(...) stop("synthetic invariance fit failure"),
    .package = "lavaan"
  )

  out <- nomo_invariance(
    model,
    lavaan::HolzingerSwineford1939,
    group = "school",
    levels = "configural",
    localize = FALSE
  )

  expect_false(out$fit_evidence$converged[[1L]])
  expect_match(
    out$fit_evidence$error[[1L]],
    "synthetic invariance fit failure",
    fixed = TRUE
  )
  expect_length(out$fit_measures$configural, 0L)
})


test_that("closeout C: network replication decision log marks inconclusive replication for review", {
  measurement <- list(
    summary = tibble::tibble(
      loading_review_flags = 0L,
      attention = "info",
      observation = "Synthetic measurement context."
    )
  )

  additions <- tibble::tibble(
    relation = "A -> B",
    syntax = "B ~ A",
    added_from_hypothesis = FALSE
  )

  evidence <- tibble::tibble(
    relation = "A -> B",
    concordance = "concordant",
    estimate = .30,
    theoretical_region = "(0, +Inf)",
    interpretation = "Synthetic concordance."
  )

  replication <- tibble::tibble(
    relation = "A -> B",
    replication_status = "mixed_or_inconclusive",
    estimate_shift = .01,
    interpretation = "Synthetic mixed replication evidence."
  )

  log <- nomologR:::nomo_network_decision_log(
    model_additions = additions,
    hypotheses_evidence = evidence,
    converged = TRUE,
    warnings = character(),
    estimator = NULL,
    ordered = character(),
    measurement_context = measurement,
    replication_evidence = replication,
    sample_role = "primary"
  )

  row <- log[
    log$stage == "network_replication",
    ,
    drop = FALSE
  ]

  expect_gt(nrow(row), 0L)
  expect_identical(row$severity[[1L]], "review")
})


test_that("closeout C: replication plots reject rows with no finite paired estimates", {
  net <- make_m9_full_report_run()$results$network

  net$replication_evidence <- tibble::tibble(
    primary_estimate = NA_real_,
    validation_estimate = NA_real_
  )

  expect_error(
    plot(net, type = "replication"),
    "No finite replication estimates are available to plot"
  )
})


test_that("closeout C: workflow decision validation rejects non-list inputs", {
  expect_error(
    nomologR:::nomo_run_validate_decisions(1L),
    "`decisions` must be a named list",
    fixed = TRUE
  )
})
