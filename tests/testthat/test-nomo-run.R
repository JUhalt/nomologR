# ---- consolidated from test-nomo-run-coverage.R ----
test_that("nomo_run internal validators cover consequential failure branches", {
  dat <- make_m8_full_data(n = 120L, seed = 8601L)
  roles <- nomologR:::nomo_run_data_roles(dat)
  scales <- list(WellBeing = c("i1", "i2", "i3", "i4"))

  expect_error(
    nomologR:::nomo_run_data_roles(data.frame()),
    "non-empty"
  )

  expect_error(
    nomologR:::nomo_run_validate_scales(list(), roles),
    "non-empty"
  )
  expect_error(
    nomologR:::nomo_run_validate_scales(
      list(WellBeing = c("i1", "i1")),
      roles
    ),
    "unique"
  )

  expect_error(
    nomologR:::nomo_run_validate_settings(
      list(unknown = list()),
      scales
    ),
    "Unknown"
  )
  expect_error(
    nomologR:::nomo_run_validate_settings(
      list(factors = 1),
      scales
    ),
    "must be a list"
  )
  expect_error(
    nomologR:::nomo_run_validate_settings(
      list(factors = list(types = c(nope = "ordinal"))),
      scales
    ),
    "outside"
  )

  expect_error(
    nomologR:::nomo_run_validate_decisions(list(bad = 1)),
    "Unsupported"
  )

  expect_error(
    nomologR:::nomo_run_normalize_factor_counts(0, scales),
    "positive integer"
  )
  expect_error(
    nomologR:::nomo_run_normalize_factor_counts(4, scales),
    "smaller"
  )

  expect_error(
    nomologR:::nomo_run_normalize_cfa_model(1),
    "measurement-model"
  )
  expect_error(
    nomologR:::nomo_run_normalize_measurement_decision("maybe"),
    "proceed"
  )
})


test_that("structured decision and rationale validation branches are covered", {
  expect_error(
    nomologR:::nomo_run_unpack_decision(
      list(value = 1L, rationale = "x", extra = TRUE)
    ),
    "only"
  )

  expect_error(
    nomologR:::nomo_run_normalize_rationale(
      c(A = "x"),
      c("A", "B")
    ),
    "matching"
  )

  expect_equal(
    nomologR:::nomo_run_normalize_measurement_decision(" PROCEED "),
    "proceed"
  )
})


test_that("overlapping supplied scale membership is retained and logged", {
  set.seed(8602L)
  f <- rnorm(180)
  dat <- data.frame(
    i1 = f + rnorm(180),
    i2 = f + rnorm(180),
    i3 = f + rnorm(180),
    i4 = f + rnorm(180),
    i5 = f + rnorm(180)
  )

  run <- nomo_run(
    data = dat,
    scales = list(
      A = c("i1", "i2", "i3"),
      B = c("i3", "i4", "i5")
    ),
    settings = list(
      factors = list(
        criterion_set = "minimal",
        n_iter = 10L,
        seed = 2026L
      )
    )
  )

  expect_true(any(run$decision_log$id == "overlapping_items"))
  expect_equal(run$scales$A, c("i1", "i2", "i3"))
  expect_equal(run$scales$B, c("i3", "i4", "i5"))
})


test_that("resume protects source data scales and guidance", {
  dat <- make_m8_full_data(n = 180L, seed = 8603L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings()
  )

  expect_error(
    nomo_run(
      resume = run,
      data = dat
    ),
    "Do not supply"
  )

  g <- nomo_defaults()
  g$factor_small_n_reference <- g$factor_small_n_reference + 1

  expect_error(
    nomo_run(
      resume = run,
      guidance = g
    ),
    "Guidance cannot"
  )

  expect_error(
    nomo_run(
      resume = list()
    ),
    "nomo_run"
  )
})


test_that("a run saved before nomo_defaults() gained an element resumes with the defaults", {
  dat <- make_m8_full_data(n = 180L, seed = 8603L)
  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings()
  )

  # A run saved by an earlier release lacks guidance elements added since.
  saved <- run
  saved$guidance$factor_small_n_reference <- NULL
  resumed <- nomo_run(
    resume = saved,
    guidance = nomo_defaults(),
    decisions = list(factor_count = 1L)
  )
  expect_identical(resumed$next_stage, "cfa")
  expect_identical(resumed$guidance, saved$guidance)

  # A changed value still stops the resume.
  changed <- nomo_defaults()
  changed$item_total_reference <- 0.40
  expect_error(nomo_run(resume = saved, guidance = changed), "Guidance cannot", fixed = TRUE)
  expect_error(nomo_run(resume = saved, guidance = "teaching"), "Guidance cannot", fixed = TRUE)
})


test_that("blocked result presentation and table branches are retained", {
  dat <- make_m8_full_data(n = 180L, seed = 8604L)
  dat$i4 <- 1

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings()
  )

  expect_equal(run$status, "blocked")
  expect_output(print(run), "blocked", ignore.case = TRUE)
  expect_s3_class(nomo_table(run, "requests"), "data.frame")
  expect_s3_class(nomo_table(run, "scales"), "data.frame")
})

# ---- consolidated from test-nomo-run-m8-full.R ----
test_that("one-call prespecification consumes decisions in workflow order", {
  dat <- make_m8_full_data(seed = 8501L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(
      factor_count = list(
        value = 1L,
        rationale = "One-factor EFA selected."
      ),
      cfa_model = list(
        value = m8_model(),
        rationale = "Prespecified measurement model."
      ),
      measurement_model = list(
        value = "proceed",
        rationale = "Measurement evidence reviewed."
      )
    )
  )

  expect_equal(run$status, "complete")
  expect_true(all(c(
    "factor_count",
    "cfa_model",
    "measurement_model"
  ) %in% names(run$decisions)))

  expect_equal(
    run$stage_status$status[
      run$stage_status$stage %in% c(
        "screen", "factors", "efa", "cfa", "reliability", "validity"
      )
    ],
    rep("completed", 6L)
  )
})


test_that("pipeline-controlled stage settings cannot be overridden", {
  skip_on_cran()
  dat <- make_m8_full_data(seed = 8502L)
  items <- c("i1", "i2", "i3", "i4")

  expect_error(
    nomo_run(
      data = dat,
      scales = list(WellBeing = items),
      settings = list(cfa = list(data = dat))
    ),
    "pipeline-controlled"
  )

  expect_error(
    nomo_run(
      data = dat,
      scales = list(WellBeing = items),
      settings = list(efa = list(factor_count = 2L))
    ),
    "pipeline-controlled"
  )

  expect_error(
    nomo_run(
      data = dat,
      scales = list(WellBeing = items),
      settings = list(invariance = list())
    ),
    NA
  )
})


test_that("requested branches require explicit identifying inputs", {
  dat <- make_m8_full_data(seed = 8503L)
  items <- c("i1", "i2", "i3", "i4")

  expect_error(
    nomo_run(
      data = dat,
      scales = list(WellBeing = items),
      settings = list(invariance = list(levels = "configural"))
    ),
    "group"
  )

  expect_error(
    nomo_run(
      data = dat,
      scales = list(WellBeing = items),
      settings = list(network = list(add_missing = TRUE))
    ),
    "hypotheses"
  )
})


test_that("decision ordering is explicit rather than silently skipped", {
  dat <- make_m8_full_data(seed = 8504L)

  expect_error(
    nomo_run(
      data = dat,
      scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
      settings = m8_full_settings(),
      decisions = list(cfa_model = m8_model())
    ),
    "could not be consumed"
  )

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings()
  )

  expect_error(
    nomo_run(
      resume = run,
      decisions = list(measurement_model = "proceed")
    ),
    "could not be consumed"
  )
})


test_that("invalid CFA and measurement decisions fail clearly", {
  dat <- make_m8_full_data(seed = 8505L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(factor_count = 1L)
  )

  expect_error(
    nomo_run(
      resume = run,
      decisions = list(cfa_model = "")
    ),
    "non-empty"
  )

  run <- nomo_run(
    resume = run,
    decisions = list(cfa_model = m8_model())
  )

  expect_error(
    nomo_run(
      resume = run,
      decisions = list(measurement_model = "pass")
    ),
    "proceed"
  )
})


test_that("CFA estimation errors block downstream stages", {
  dat <- make_m8_full_data(seed = 8506L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(
      factor_count = 1L,
      cfa_model = "WellBeing =~ i1 + i2 + missing_item"
    )
  )

  expect_equal(run$status, "blocked")
  expect_equal(run$blocked$stage, "cfa")
  expect_null(run$results$reliability)
  expect_null(run$results$validity)
  expect_match(run$decision_requests$consequence, "No later")

  expect_error(
    nomo_run(resume = run),
    "blocked"
  )
})


test_that("multiple scales require named factor-count decisions", {
  set.seed(8507L)
  n <- 320L
  a <- rnorm(n)
  b <- rnorm(n)

  dat <- data.frame(
    a1 = .8 * a + rnorm(n, sd = .6),
    a2 = .8 * a + rnorm(n, sd = .6),
    a3 = .7 * a + rnorm(n, sd = .7),
    a4 = .7 * a + rnorm(n, sd = .7),
    b1 = .8 * b + rnorm(n, sd = .6),
    b2 = .8 * b + rnorm(n, sd = .6),
    b3 = .7 * b + rnorm(n, sd = .7),
    b4 = .7 * b + rnorm(n, sd = .7)
  )

  scales <- list(
    A = c("a1", "a2", "a3", "a4"),
    B = c("b1", "b2", "b3", "b4")
  )

  run <- nomo_run(
    data = dat,
    scales = scales,
    settings = m8_full_settings()
  )

  expect_error(
    nomo_run(
      resume = run,
      decisions = list(factor_count = c(1L, 1L))
    ),
    "named"
  )

  run <- nomo_run(
    resume = run,
    decisions = list(
      factor_count = c(A = 1L, B = 1L)
    )
  )

  expect_s3_class(run$results$efa$A, "nomo_efa")
  expect_s3_class(run$results$efa$B, "nomo_efa")
})


test_that("recipe settings and component log expose reproducibility provenance", {
  skip_on_cran()
  dat <- make_m8_full_data(seed = 8508L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(
      factor_count = 1L,
      cfa_model = m8_model()
    )
  )

  recipe <- nomo_table(run, "recipe")
  settings <- nomo_table(run, "settings")
  component_log <- nomo_table(run, "component_log")

  expect_true(all(c(
    "function_name",
    "data_role",
    "researcher_control"
  ) %in% names(recipe)))
  # One row per stage, then scores and missing-data sensitivity (#145).
  expect_identical(settings$stage, c(nomologR:::nomo_run_stage_order(), "scores", "missing"))
  expect_identical(settings$values[settings$stage == "factors"],
                   "criterion_set = \"minimal\", n_iter = 10L, seed = 2026L")
  expect_false(any(settings$configured[settings$stage %in% c("scores", "missing")]))
  expect_gt(nrow(component_log), 0L)

  s <- summary(run)
  expect_s3_class(s, "summary_nomo_run")
  expect_output(print(s), "Component recipe")
})


test_that("completed workflow rejects decision changes", {
  dat <- make_m8_full_data(seed = 8509L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(
      factor_count = 1L,
      cfa_model = m8_model(),
      measurement_model = "proceed"
    )
  )

  expect_error(
    nomo_run(
      resume = run,
      decisions = list(measurement_model = "revise")
    ),
    "already complete"
  )
})

# ---- consolidated from test-nomo-run-m8a.R ----
make_m8a_data <- function(n = 220L, seed = 8101L) {
  set.seed(seed)
  f <- rnorm(n)

  data.frame(
    i1 = .82 * f + rnorm(n, sd = .55),
    i2 = .78 * f + rnorm(n, sd = .60),
    i3 = .75 * f + rnorm(n, sd = .64),
    i4 = .72 * f + rnorm(n, sd = .68)
  )
}


m8a_settings <- function() {
  list(
    factors = list(
      criterion_set = "minimal",
      n_iter = 10L,
      seed = 2026L
    )
  )
}


test_that("nomo_run computes nonconsequential evidence then pauses before EFA", {
  dat <- make_m8a_data()

  out <- nomo_run(
    data = dat,
    scales = list(WellBeing = names(dat)),
    settings = m8a_settings()
  )

  expect_s3_class(out, "nomo_run")
  expect_equal(out$status, "paused")
  expect_equal(out$next_stage, "efa")

  expect_s3_class(out$results$screen$WellBeing, "nomo_screen")
  expect_s3_class(out$results$factors$WellBeing, "nomo_factors")
  expect_length(out$results$efa, 0L)

  expect_equal(
    out$stage_status$status[out$stage_status$stage == "screen"],
    "completed"
  )
  expect_equal(
    out$stage_status$status[out$stage_status$stage == "factors"],
    "completed"
  )
  expect_equal(
    out$stage_status$status[out$stage_status$stage == "efa"],
    "awaiting_decision"
  )

  expect_true(all(c(
    "observation",
    "reason",
    "options",
    "consequence"
  ) %in% names(out$decision_requests)))

  expect_match(
    out$decision_requests$reason[[1L]],
    "does not authorize"
  )
})


test_that("factor-count decision resumes without recomputing completed stages", {
  dat <- make_m8a_data(seed = 8102L)

  first <- nomo_run(
    data = dat,
    scales = list(WellBeing = names(dat)),
    settings = m8a_settings()
  )

  screen_before <- first$results$screen$WellBeing
  factors_before <- first$results$factors$WellBeing

  second <- nomo_run(
    resume = first,
    decisions = list(
      factor_count = list(
        value = 1L,
        rationale = "Substantive interpretation and retention evidence support one factor."
      )
    )
  )

  expect_identical(second$results$screen$WellBeing, screen_before)
  expect_identical(second$results$factors$WellBeing, factors_before)

  expect_s3_class(second$results$efa$WellBeing, "nomo_efa")
  expect_equal(second$results$efa$WellBeing$n_factors, 1L)
  expect_equal(
    second$results$efa$WellBeing$factor_source,
    "researcher_with_nomo_factors_context"
  )

  expect_equal(second$status, "paused")
  expect_equal(second$next_stage, "cfa")

  decision_row <- second$decision_log[
    second$decision_log$id == "factor_count:WellBeing",
    ,
    drop = FALSE
  ]
  expect_equal(decision_row$decision, "1")
  expect_match(decision_row$rationale, "Substantive interpretation")
})


test_that("nomo_run honors researcher factor counts rather than PA suggestions", {
  dat <- make_m8a_data(n = 300L, seed = 8103L)

  first <- nomo_run(
    data = dat,
    scales = list(WellBeing = names(dat)),
    settings = m8a_settings()
  )

  pa <- first$results$factors$WellBeing$parallel$n_factors
  chosen <- if (identical(as.integer(pa), 1L)) 2L else 1L

  second <- nomo_run(
    resume = first,
    decisions = list(factor_count = chosen)
  )

  expect_equal(second$results$efa$WellBeing$n_factors, chosen)
  expect_equal(
    second$decisions$factor_count$value[["WellBeing"]],
    chosen
  )
})


test_that("nomo_run respects nomo_split sample roles", {
  dat <- make_m8a_data(n = 260L, seed = 8104L)
  split <- nomo_split(
    dat,
    validation_prop = .40,
    seed = 2026
  )

  out <- nomo_run(
    data = split,
    scales = list(WellBeing = names(dat)),
    settings = m8a_settings()
  )

  expect_equal(out$sample_design, "calibration_validation")
  expect_equal(
    out$results$factors$WellBeing$n_cases,
    split$n_calibration
  )
  expect_equal(
    out$sample_n$n[out$sample_n$role == "confirmatory"],
    split$n_validation
  )
})


test_that("teaching and research modes do not change analysis", {
  dat <- make_m8a_data(seed = 8105L)

  teaching <- nomo_run(
    data = dat,
    scales = list(WellBeing = names(dat)),
    mode = "teaching",
    settings = m8a_settings()
  )

  research <- nomo_run(
    resume = teaching,
    mode = "research"
  )

  expect_identical(research$results, teaching$results)
  expect_identical(research$decision_log, teaching$decision_log)
  expect_equal(research$mode, "research")

  expect_output(print(teaching), "Reason:")
  expect_output(print(research), "Researcher decision required")
  # Research mode is compact: the request, without the teaching explanation.
  expect_false(any(grepl("Reason:", utils::capture.output(print(research)), fixed = TRUE)))
})


test_that("nomo_run validates named scale definitions", {
  dat <- make_m8a_data(seed = 8106L)

  expect_error(
    nomo_run(
      data = dat,
      scales = list(names(dat)),
      settings = m8a_settings()
    ),
    "unique, non-empty names"
  )

  expect_error(
    nomo_run(
      data = dat,
      scales = list(WellBeing = c("i1", "missing")),
      settings = m8a_settings()
    ),
    "unavailable"
  )
})


test_that("component failures block rather than silently skip a stage", {
  dat <- make_m8a_data(seed = 8107L)
  dat$i4 <- 1

  out <- nomo_run(
    data = dat,
    scales = list(WellBeing = names(dat)),
    settings = m8a_settings()
  )

  expect_equal(out$status, "blocked")
  expect_equal(out$next_stage, "factors")
  expect_equal(
    out$stage_status$status[out$stage_status$stage == "factors"],
    "blocked"
  )
  expect_length(out$results$efa, 0L)
  expect_match(out$decision_requests$consequence[[1L]], "No later")
})


test_that("nomo_table exposes pipeline state without mutating it", {
  dat <- make_m8a_data(seed = 8108L)

  out <- nomo_run(
    data = dat,
    scales = list(WellBeing = names(dat)),
    settings = m8a_settings()
  )

  expect_s3_class(nomo_table(out, "stages"), "data.frame")
  expect_s3_class(nomo_table(out, "requests"), "data.frame")
  expect_s3_class(nomo_table(out, "decisions"), "data.frame")
  expect_s3_class(nomo_table(out, "component_log"), "data.frame")
  expect_s3_class(nomo_table(out, "scales"), "data.frame")

  expect_output(print(summary(out)), "Stages")
})

# ---- consolidated from test-nomo-run-m8b.R ----
test_that("CFA model handoff runs CFA reliability and validity then pauses", {
  dat <- make_m8_full_data()

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(factor_count = 1L)
  )

  expect_equal(run$next_stage, "cfa")

  run <- nomo_run(
    resume = run,
    decisions = list(
      cfa_model = list(
        value = m8_model(),
        rationale = "Prespecified one-factor measurement model."
      )
    )
  )

  expect_s3_class(run$results$cfa, "nomo_cfa")
  expect_s3_class(run$results$reliability, "nomo_reliability")
  expect_s3_class(run$results$validity, "nomo_validity")

  expect_equal(run$status, "paused")
  expect_equal(run$next_stage, "measurement_review")
  expect_equal(run$decision_requests$id, "measurement_model")

  expect_match(run$decision_requests$reason, "inherit the measurement model")
  expect_match(run$decision_requests$consequence, "does not declare")
})


test_that("measurement proceed completes when optional branches are not requested", {
  dat <- make_m8_full_data(seed = 8402L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(
      factor_count = 1L,
      cfa_model = list(
        value = m8_model(),
        rationale = "Prespecified model."
      ),
      measurement_model = list(
        value = "proceed",
        rationale = "Measurement evidence reviewed."
      )
    )
  )

  expect_equal(run$status, "complete")
  expect_null(run$next_stage)
  expect_equal(
    run$stage_status$status[run$stage_status$stage == "invariance"],
    "not_requested"
  )
  expect_equal(
    run$stage_status$status[run$stage_status$stage == "network"],
    "not_requested"
  )

  expect_true(any(run$decision_log$id == "workflow_complete"))
})


test_that("measurement revise stops without hidden downstream refitting", {
  dat <- make_m8_full_data(seed = 8403L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(
      factor_count = 1L,
      cfa_model = m8_model(),
      measurement_model = list(
        value = "revise",
        rationale = "I want to reconsider the measurement model."
      )
    )
  )

  expect_equal(run$status, "paused")
  expect_equal(run$next_stage, "restart")
  expect_null(run$results$invariance)
  expect_null(run$results$network)
  expect_equal(run$decision_requests$id, "restart_with_revised_model")
  expect_match(run$decision_requests$consequence, "No invariance")
})


test_that("future-stage settings can be added without recomputing completed stages", {
  dat <- make_m8_full_data(seed = 8404L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(
      factor_count = 1L,
      cfa_model = m8_model()
    )
  )

  cfa_before <- run$results$cfa

  run2 <- nomo_run(
    resume = run,
    settings = list(
      invariance = list(
        group = "group",
        levels = "configural",
        localize = FALSE
      )
    )
  )

  expect_identical(run2$results$cfa, cfa_before)
  expect_equal(run2$settings$invariance$group, "group")

  expect_error(
    nomo_run(
      resume = run2,
      settings = list(
        cfa = list(std.lv = TRUE)
      )
    ),
    "cannot be changed"
  )
})


test_that("invariance branch is explicit and researcher controlled", {
  skip_on_cran()
  dat <- make_m8_full_data(n = 360L, seed = 8405L)

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = c(
      m8_full_settings(),
      list(
        invariance = list(
          group = "group",
          levels = "configural",
          localize = FALSE
        )
      )
    ),
    decisions = list(
      factor_count = 1L,
      cfa_model = m8_model(),
      measurement_model = "proceed"
    )
  )

  expect_equal(run$status, "complete")
  expect_s3_class(run$results$invariance, "nomo_invariance")
  expect_null(run$results$invariance$partial)
  expect_equal(
    run$stage_status$status[run$stage_status$stage == "invariance"],
    "completed"
  )
})


test_that("network branch uses explicit hypotheses", {
  dat <- make_m8_full_data(n = 360L, seed = 8406L)

  h <- nomo_hypotheses(
    "WellBeing -> criterion" = positive(min = .10)
  )

  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = c(
      m8_full_settings(),
      list(network = list(hypotheses = h))
    ),
    decisions = list(
      factor_count = 1L,
      cfa_model = m8_model(),
      measurement_model = "proceed"
    )
  )

  expect_equal(run$status, "complete")
  expect_s3_class(run$results$network, "nomo_network")
  expect_equal(nrow(run$results$network$hypothesis_evidence), 1L)
  expect_true(
    run$results$network$model_relations$added_from_hypothesis[
      run$results$network$model_relations$relation == "WellBeing -> criterion"
    ]
  )
})


test_that("nomo_split roles carry into CFA and network replication", {
  skip_on_cran()
  dat <- make_m8_full_data(n = 400L, seed = 8407L)
  split <- nomo_split(dat, validation_prop = .40, seed = 2026)

  h <- nomo_hypotheses(
    "WellBeing -> criterion" = positive()
  )

  run <- nomo_run(
    data = split,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = c(
      m8_full_settings(),
      list(network = list(hypotheses = h))
    ),
    decisions = list(
      factor_count = 1L,
      cfa_model = m8_model(),
      measurement_model = "proceed"
    )
  )

  expect_equal(run$sample_design, "calibration_validation")
  expect_equal(run$results$cfa$data_n, split$n_validation)
  expect_s3_class(run$results$network, "nomo_network")
  expect_false(is.null(run$results$network$validation))
  # Nothing set for the CFA, so nothing is inherited or recorded.
  expect_false(any(startsWith(run$decision_log$id, "estimation_settings:")))
})


run_downstream <- function(dat, cfa, invariance = list(), network = list()) {
  nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = c(
      m8_full_settings(),
      list(
        cfa = cfa,
        invariance = c(list(group = "group", levels = "configural", localize = FALSE), invariance),
        network = c(list(hypotheses = nomo_hypotheses("WellBeing -> criterion" = positive())),
                    network)
      )
    ),
    decisions = list(factor_count = 1L, cfa_model = m8_model(), measurement_model = "proceed")
  )
}


test_that("invariance and the network are estimated with the CFA stage's settings", {
  skip_on_cran()
  dat <- make_m8_full_data(n = 360L, seed = 8406L)
  dat$i1[1:40] <- NA
  options_of <- function(fit) lavaan::lavInspect(fit, "options")
  nobs_of <- function(fit) sum(unlist(lavaan::lavInspect(fit, "nobs")))

  # The CFA was estimated with FIML, so invariance and the network are too,
  # on the same 360 cases rather than the 320 complete ones (#145).
  run <- run_downstream(dat, cfa = list(missing = "fiml"))
  expect_identical(run$status, "complete")
  expect_identical(options_of(run$results$invariance$fits$configural)$missing, "ml")
  expect_identical(options_of(run$results$network$fit)$missing, "ml")
  expect_equal(nobs_of(run$results$invariance$fits$configural), 360)
  expect_equal(nobs_of(run$results$network$fit), 360)

  row <- run$decision_log[run$decision_log$id == "estimation_settings:invariance", ]
  expect_identical(row$stage, "invariance")
  expect_identical(row$source, "pipeline")
  expect_identical(
    row$observation,
    paste("Invariance testing re-estimates the measurement model with the CFA stage's",
          "missing = \"fiml\", from `settings$cfa`.")
  )
  expect_identical(row$decision, "missing = \"fiml\"")
  expect_identical(row$consequence, "This stage uses the reviewed CFA's estimation settings.")
  network_row <- run$decision_log[run$decision_log$id == "estimation_settings:network", ]
  expect_match(network_row$observation, "The nomological network re-estimates", fixed = TRUE)

  # A stage's own setting is used and the difference recorded; NULL asks for
  # the default.
  own <- run_downstream(dat, cfa = list(missing = "fiml", estimator = "MLR"),
                        invariance = list(missing = "listwise"),
                        network = list(missing = NULL))
  expect_identical(own$status, "complete")
  configural <- options_of(own$results$invariance$fits$configural)
  expect_identical(configural$missing, "listwise")
  expect_true("yuan.bentler.mplus" %in% configural$test)
  expect_identical(options_of(own$results$network$fit)$missing, "listwise")

  row <- own$decision_log[own$decision_log$id == "estimation_settings:invariance", ]
  expect_identical(row$source, "researcher_input")
  expect_match(row$observation, "with the CFA stage's estimator = \"MLR\", from `settings$cfa`.",
               fixed = TRUE)
  expect_match(row$observation,
               "`settings$invariance` sets missing = \"listwise\", where the CFA stage used missing = \"fiml\".",
               fixed = TRUE)
  expect_identical(row$decision, "estimator = \"MLR\", missing = \"listwise\"")
  expect_match(row$consequence, "differ from the reviewed CFA's", fixed = TRUE)
  network_row <- own$decision_log[own$decision_log$id == "estimation_settings:network", ]
  expect_match(network_row$observation,
               "sets missing = NULL (the default), where the CFA stage used missing = \"fiml\".",
               fixed = TRUE)
})


test_that("an ordinal CFA's indicators stay ordinal in invariance and the network", {
  skip_on_cran()
  dat <- make_m8_full_data(n = 400L, seed = 8409L)
  items <- c("i1", "i2", "i3", "i4")
  for (i in items) dat[[i]] <- as.integer(cut(dat[[i]], c(-Inf, -1, 0, 1, Inf)))

  run <- run_downstream(dat, cfa = list(ordered = items))
  expect_identical(run$status, "complete")
  expect_identical(run$results$invariance$ordered, items)
  expect_setequal(lavaan::lavNames(run$results$network$fit, "ov.ord"), items)
  row <- run$decision_log[run$decision_log$id == "estimation_settings:network", ]
  expect_match(row$observation, "ordered = c(\"i1\", \"i2\", \"i3\", \"i4\")", fixed = TRUE)
})


test_that("research and teaching modes retain identical statistical objects", {
  dat <- make_m8_full_data(seed = 8408L)

  teaching <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(
      factor_count = 1L,
      cfa_model = m8_model()
    )
  )

  research <- nomo_run(
    resume = teaching,
    mode = "research"
  )

  expect_identical(research$results, teaching$results)
  expect_identical(research$decisions, teaching$decisions)
  expect_equal(research$mode, "research")

  expect_output(print(teaching), "Reason:")
  expect_output(print(research), "Researcher decision required")
  # Research mode is compact: the request, without the teaching explanation.
  expect_false(any(grepl("Reason:", utils::capture.output(print(research)), fixed = TRUE)))
})


# Edge cases and failure paths ------------------------------------------------
#
# Moved from test-regressions.R (#36). These tests were consolidated during
# pre-v0.1 hardening and exercise defensive branches, sometimes through
# internal helpers directly.

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
  expect_match(req$observation, "Their logs hold 0 concern entries and 0 review entries.",
               fixed = TRUE)

  # Counts agree in number (#145).
  x$results$cfa$decision_log <- tibble::tibble(severity = c("concern", "review", "review"))
  req <- nomologR:::nomo_run_measurement_request(x)
  expect_match(req$observation, "1 concern entry and 2 review entries.", fixed = TRUE)
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
    data = data.frame(i1 = 1:8, i2 = 8:1, i3 = c(2:8, 1)),
    scales = list(S = c("i1", "i2", "i3"))
  )
  expect_identical(out$status, "blocked")
  expect_identical(out$blocked$stage, "screen")
})


test_that("closeout C: workflow decision validation rejects non-list inputs", {
  expect_error(
    nomologR:::nomo_run_validate_decisions(1L),
    "`decisions` must be a named list",
    fixed = TRUE
  )
})


# Pre-RC fixes (#145) ----------------------------------------------------------

test_that("settings given when resuming merge argument by argument and are logged (#145)", {
  dat <- make_m8_full_data(seed = 8510L)
  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = c(m8_full_settings(), list(cfa = list(estimator = "MLR", std.lv = TRUE)))
  )
  resumed <- nomo_run(resume = run, settings = list(cfa = list(missing = "fiml")))
  # The estimator and std.lv given at the start are kept.
  expect_identical(resumed$settings$cfa, list(estimator = "MLR", std.lv = TRUE, missing = "fiml"))
  row <- resumed$decision_log[resumed$decision_log$id == "settings:cfa", ]
  expect_identical(nrow(row), 1L)
  expect_identical(row$source, "researcher_input")
  expect_identical(row$observation,
                   "The workflow was resumed with `settings$cfa`: missing = \"fiml\".")
  expect_identical(row$decision, "estimator = \"MLR\", std.lv = TRUE, missing = \"fiml\"")

  # Giving the same settings again changes nothing and adds no row.
  again <- nomo_run(resume = resumed, settings = list(cfa = list(missing = "fiml")))
  expect_identical(again$decision_log, resumed$decision_log)

  # NULL asks a component for its default, so it is kept rather than dropped.
  reset <- nomo_run(resume = resumed, settings = list(cfa = list(std.lv = NULL)))
  expect_true("std.lv" %in% names(reset$settings$cfa))
  expect_null(reset$settings$cfa$std.lv)

  # Attached evidence is logged at the CFA, and an object by its class.
  attached <- nomo_run(resume = run, settings = list(missing = list()))
  row <- attached$decision_log[attached$decision_log$id == "settings:missing", ]
  expect_identical(row$stage, "cfa")
  expect_identical(row$decision, "list()")
  h <- nomo_hypotheses("WellBeing -> criterion" = positive())
  network <- nomo_run(resume = run, settings = list(network = list(hypotheses = h)))
  expect_identical(network$decision_log$decision[network$decision_log$id == "settings:network"],
                   "hypotheses = <nomo_hypotheses>")

  # The merged settings are validated as a whole.
  expect_error(nomo_run(resume = run, settings = list(cfa = list(data = dat))),
               "cannot override pipeline-controlled argument: data.", fixed = TRUE)
  expect_error(nomo_run(resume = run, settings = list(bogus = list())),
               "Unknown workflow setting stage: bogus.", fixed = TRUE)
})


test_that("settings whose stage has passed are refused rather than stored unused (#145)", {
  skip_on_cran()
  dat <- make_m8_full_data(seed = 8511L)
  scales <- list(WellBeing = c("i1", "i2", "i3", "i4"))
  review <- nomo_run(
    data = dat, scales = scales, settings = m8_full_settings(),
    decisions = list(factor_count = 1L, cfa_model = m8_model())
  )
  # Scores and missing-data sensitivity run with the CFA, which has been fitted.
  expect_error(
    nomo_run(resume = review, settings = list(scores = list(method = "sum"))),
    "Settings for `scores` cannot be changed while resuming: they run with the CFA",
    fixed = TRUE
  )
  expect_error(
    nomo_run(resume = review, settings = list(missing = list())),
    "Settings for `missing` cannot be changed while resuming", fixed = TRUE
  )

  # A branch the completed run marked not requested would never run.
  done <- nomo_run(resume = review, decisions = list(measurement_model = "proceed"))
  expect_error(
    nomo_run(resume = done, settings = list(invariance = list(group = "group"))),
    "the completed workflow marked the stage not requested, so they would never run",
    fixed = TRUE
  )
  expect_error(
    nomo_run(resume = done, settings = list(efa = list(rotation = "varimax"))),
    "Settings for `efa` cannot be changed while resuming: the stage has already completed.",
    fixed = TRUE
  )

  lock <- nomologR:::nomo_run_settings_lock
  blocked <- list(stage_status = tibble::tibble(stage = "cfa", status = "blocked"))
  expect_identical(lock(blocked, "cfa"), "the stage is blocked")
  expect_match(lock(blocked, "scores"), "they run with the CFA", fixed = TRUE)
  expect_match(lock(list(results = list(scores = list())), "scores"), "they run with the CFA",
               fixed = TRUE)
  expect_null(lock(list(), "scores"))
  expect_null(lock(list(), "invariance"))

  # At the pause after "revise", the run goes no further, but nomo_revise()
  # carries its settings into the revision. Evidence this run has not computed
  # may still be requested, scores and missing-data sensitivity as much as the
  # downstream branches; evidence it computed keeps its settings (#145).
  fitted <- list(next_stage = "restart",
                 stage_status = tibble::tibble(stage = "cfa", status = "completed"))
  expect_null(lock(fitted, "scores"))
  expect_null(lock(fitted, "missing"))
  expect_identical(lock(fitted, "cfa"), "the stage has already completed")
  fitted$results <- list(scores = list(), missing = list(cfa = list()))
  expect_identical(
    lock(fitted, "scores"),
    "this run's scores were computed with them, and `nomo_revise()` carries them into a revision as they are"
  )
  expect_match(lock(fitted, "missing"), "this run's missing-data comparison was computed with them",
               fixed = TRUE)

  restart <- nomo_run(resume = review, decisions = list(measurement_model = "revise"))
  added <- nomo_run(resume = restart, settings = list(scores = list(method = "sum"),
                                                      invariance = list(group = "group")))
  expect_identical(added$next_stage, "restart")
  expect_true(all(c("settings:scores", "settings:invariance") %in% added$decision_log$id))
  revised <- nomo_revise(added, cfa_model = paste(m8_model(), "\ni1 ~~ i2"),
                         rationale = "i1 and i2 share wording.", compare = FALSE)
  expect_identical(revised$settings$scores, list(method = "sum"))
  expect_s3_class(revised$results$scores, "nomo_scores")
})


test_that("inputs that cannot work are refused before any stage runs (#145)", {
  dat <- make_m8_full_data(seed = 8512L)
  expect_error(
    nomo_run(data = dat, scales = list(Pair = c("i1", "i2"), WellBeing = c("i1", "i2", "i3"))),
    "Scale `Pair` has 2 items; a guided run needs at least three items per scale",
    fixed = TRUE
  )
  expect_error(
    nomo_run(data = dat, scales = list(WellBeing = c("i1", "nope", "gone"))),
    "contains item columns unavailable in the workflow data: nope, gone.", fixed = TRUE
  )
  expect_error(
    nomo_run(data = dat, scales = list(WellBeing = c("i1", "i2", "nope"))),
    "contains an item column unavailable in the workflow data: nope.", fixed = TRUE
  )
  expect_error(
    nomo_run(data = dat, scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
             settings = list(invariance = list(group = "grp"))),
    "`settings$invariance$group` is \"grp\", which is not a column of the data",
    fixed = TRUE
  )
  expect_error(
    nomo_run(data = dat, scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
             settings = list(missing = list(strategies = c("listwise", "bogus")))),
    "must name lavaan `missing` options, \"listwise\",", fixed = TRUE
  )
  expect_error(
    nomo_run(data = dat, scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
             settings = list(missing = list(strategies = "bogus"))),
    "or \"default\", not \"bogus\".", fixed = TRUE
  )
  expect_error(
    nomo_run(data = dat, scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
             settings = list(cfa = list(model = "x", data = dat))),
    "cannot override pipeline-controlled arguments: model, data.", fixed = TRUE
  )
  expect_error(
    nomologR:::nomo_run_validate_settings(list(a = list(), b = list()), list()),
    "Unknown workflow setting stages: a, b.", fixed = TRUE
  )
  expect_error(nomologR:::nomo_run_validate_decisions(list(a = 1, b = 2)),
               "Unsupported workflow decision names: a, b.", fixed = TRUE)
  expect_error(nomologR:::nomo_run_validate_decisions(list(a = 1)),
               "Unsupported workflow decision name: a.", fixed = TRUE)
  # Without the data roles, the group is checked only for its form.
  expect_identical(
    nomologR:::nomo_run_validate_settings(list(invariance = list(group = "grp")), list())$invariance,
    list(group = "grp")
  )
  # The stage-level guard names its arguments in the same way.
  expect_error(
    nomologR:::nomo_run_component_call(function(...) NULL, list(a = 1, b = 2),
                                       list(a = 1, b = 2), "efa"),
    "cannot override pipeline-controlled arguments: a, b.", fixed = TRUE
  )
})


test_that("a guidance list that asks for automatic deletion is refused (#145)", {
  dat <- make_m8_full_data(seed = 8513L)
  g <- nomo_defaults()
  g$auto_delete <- TRUE
  expect_error(
    nomo_run(data = dat, scales = list(WellBeing = c("i1", "i2", "i3", "i4")), guidance = g),
    "`guidance$auto_delete` cannot be `TRUE`: nomologR never deletes an item", fixed = TRUE
  )
})


test_that("decisions left over at a pause are named in number (#145)", {
  dat <- make_m8_full_data(seed = 8514L)
  run <- nomo_run(data = dat, scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
                  settings = m8_full_settings())
  expect_error(nomo_run(resume = run, decisions = list(cfa_model = m8_model())),
               "A decision could not be consumed", fixed = TRUE)
  expect_error(
    nomo_run(resume = run, decisions = list(cfa_model = m8_model(), measurement_model = "proceed")),
    "Decisions could not be consumed", fixed = TRUE
  )
})


test_that("a measurement model whose items differ from the scales is recorded (#145)", {
  dat <- make_m8_full_data(seed = 8515L)
  run <- nomo_run(
    data = dat,
    scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
    settings = m8_full_settings(),
    decisions = list(
      factor_count = 1L,
      cfa_model = list(value = "WellBeing =~ i1 + i2 + i3 + criterion",
                       rationale = "i4 is a pilot item; criterion is a marker.")
    )
  )
  # The run is not blocked: the difference is the researcher's decision.
  expect_identical(run$next_stage, "measurement_review")
  row <- run$decision_log[run$decision_log$id == "cfa_item_set", ]
  expect_identical(
    row$observation,
    paste("The measurement model leaves out i4, which the earlier stages screened and",
          "explored, and includes criterion, which no supplied scale contains, so no item",
          "audit, retention, or EFA evidence covers it.")
  )
  expect_identical(row$decision, "left out: i4; added: criterion")
  expect_identical(row$rationale, "i4 is a pilot item; criterion is a marker.")
  expect_identical(row$source, "researcher_decision")
  expect_match(run$decision_requests$observation, "decision-log row cfa_item_set", fixed = TRUE)

  set <- nomologR:::nomo_run_model_item_set
  expect_null(set("this is not lavaan syntax ~~~ =~", list(A = "a1")))
  expect_identical(set("A =~ a1 + a2 + a3", list(A = c("a1", "a2", "a3"))),
                   list(omitted = character(), added = character()))
  added_only <- nomologR:::nomo_run_cfa_model_log(
    list(scales = list(A = c("a1", "a2", "a3")), decision_log = NULL, results = list(),
         settings = list()),
    "A =~ a1 + a2 + a3 + b1 + b2", ""
  )
  expect_identical(
    added_only$decision_log$observation,
    paste("The measurement model includes b1, b2, which no supplied scale contains, so no",
          "item audit, retention, or EFA evidence covers them.")
  )
})


test_that("items the exploratory stages treated as categorical are flagged before a continuous CFA (#145)", {
  skip_on_cran()
  dat <- make_m8_full_data(seed = 8516L)
  items <- c("i1", "i2", "i3", "i4")
  for (i in items) dat[[i]] <- as.integer(dat[[i]] > stats::median(dat[[i]]))

  # The tetrachoric parallel analysis prints nothing to the console (#145).
  printed <- utils::capture.output(
    run <- nomo_run(data = dat, scales = list(WellBeing = items), settings = m8_full_settings(),
                    decisions = list(factor_count = 1L))
  )
  expect_identical(printed, character())
  expect_match(run$decision_requests$observation,
               "The exploratory stages treated i1, i2, i3, i4 as categorical (tetrachoric correlations)",
               fixed = TRUE)
  expect_match(run$decision_requests$options, "`settings = list(cfa = list(ordered = ...))`",
               fixed = TRUE)

  fitted <- nomo_run(resume = run, decisions = list(cfa_model = m8_model()))
  row <- fitted$decision_log[fitted$decision_log$id == "item_types", ]
  expect_identical(row$source, "pipeline")
  expect_identical(row$decision, "")
  expect_match(row$observation, "while the CFA treats every indicator as continuous", fixed = TRUE)
  expect_match(fitted$decision_requests$observation, "decision-log row item_types", fixed = TRUE)

  # Declaring them ordered keeps the two halves consistent, and nothing is flagged.
  ordered <- nomo_run(resume = run, settings = list(cfa = list(ordered = items)),
                      decisions = list(cfa_model = m8_model()))
  expect_false("item_types" %in% ordered$decision_log$id)
  expect_false(grepl("item_types", ordered$decision_requests$observation, fixed = TRUE))
})


test_that("the item-type note names every categorical item and method (#145)", {
  x <- make_m9_minimal_run()
  types <- function(item, type) tibble::tibble(item = item, model_type = type)
  x$results$factors <- list(
    A = list(correlation = "mixed", modeling_types = types(c("a1", "a2"), c("binary", "continuous"))),
    B = list(correlation = "pearson", modeling_types = types("b1", "continuous"))
  )
  x$results$efa <- list(C = list(correlation = "polychoric", modeling_types = types("c1", "ordinal")))
  expect_identical(nomologR:::nomo_run_categorical_items(x),
                   list(items = c("a1", "c1"), methods = c("mixed", "polychoric")))
  expect_match(nomologR:::nomo_run_item_type_note(x),
               "treated a1, c1 as categorical (mixed and polychoric correlations)", fixed = TRUE)
  x$settings$cfa$ordered <- "a1"
  expect_identical(nomologR:::nomo_run_item_type_note(x), "")
})


test_that("a parallel-analysis count of 0 gets its own request and a usable example (#145)", {
  x <- make_m9_minimal_run()
  x$scales <- list(Noise = c("n1", "n2", "n3"))
  x$results$factors <- list(Noise = list(parallel = list(n_factors = 0L), plausible_factors = integer()))
  req <- nomologR:::nomo_run_factor_requests(x)
  expect_identical(
    req$observation,
    paste("Parallel analysis suggests 0 factors: it found no factor above the null reference,",
          "so there is no count to adopt. To fit an EFA anyway, give the count in",
          "`factor_count`, with the substantive reason as its rationale. The pipeline has not",
          "adopted a factor count.")
  )
  expect_identical(req$example, "decisions = list(factor_count = 1L)")

  x$results$factors$Noise$plausible_factors <- 2L
  req <- nomologR:::nomo_run_factor_requests(x)
  expect_match(req$observation, "so there is no count to adopt; the retained plausible set is 2.",
               fixed = TRUE)
})


test_that("a run on noise items can fit the count the request suggests (#145)", {
  skip_on_cran()
  set.seed(1)
  noise <- as.data.frame(matrix(stats::rnorm(300 * 5), 300, 5))
  names(noise) <- paste0("n", 1:5)
  run <- nomo_run(noise, list(Noise = names(noise)),
                  settings = list(factors = list(n_iter = 20L, seed = 1L)))
  expect_identical(run$results$factors$Noise$parallel$n_factors, 0L)
  expect_match(run$decision_requests$observation, "Parallel analysis suggests 0 factors", fixed = TRUE)
  expect_false(grepl("plausible set is 0", run$decision_requests$observation, fixed = TRUE))
  resumed <- nomo_run(resume = run, decisions = list(factor_count = 1L))
  expect_identical(resumed$next_stage, "cfa")
})


test_that("a held-back item's missing status or recommendation is not quoted as NA (#145)", {
  h <- readRDS(test_path("fixtures", "contentvalidR", "handoff-walkthrough-sort-v0.10.1.rds"))
  ev <- h$item_evidence
  ev$recommendation[ev$item == "EF5"] <- NA_character_
  ev$status[ev$item == "TF5"] <- NA_character_
  ev$recommendation[ev$item == "TF5"] <- NA_character_
  h$item_evidence <- ev
  log <- nomologR:::nomo_run_handoff_log(nomologR:::nomo_run_workflow_log_new(),
                                         nomologR:::nomo_handoff_read(h))
  expect_identical(log$observation[log$id == "held_back:EF5"],
                   "EF5 was held back by content review: status \"Review\".")
  expect_identical(log$observation[log$id == "held_back:TF5"],
                   "TF5 was held back by content review.")
})


test_that("the recipe names attached evidence with its own status (#145)", {
  x <- make_m9_minimal_run()
  x$settings <- list(screen = list(effort = TRUE), scores = list(method = "sum"),
                     missing = list(), network = list(hypotheses = list()))
  recipe <- nomologR:::nomo_run_recipe_table(x)
  attached <- recipe[recipe$scope %in% c("careless_responding", "theory_network") |
                       recipe$stage %in% c("scores", "missing"), ]
  expect_identical(attached$function_name, c("nomo_screen(effort = TRUE)", "nomo_scores()",
                                             "nomo_missing()", "nomo_missing()"))
  expect_identical(attached$status, c("completed", "not_started", "not_started", "not_started"))

  x$results$effort <- list()
  x$results$scores <- list()
  x$decision_log <- nomologR:::nomo_run_workflow_log_add(
    x$decision_log, id = "missing_data_cfa", stage = "cfa", scope = "measurement_model",
    decision = "not computed"
  )
  recipe <- nomologR:::nomo_run_recipe_table(x)
  expect_identical(recipe$status[recipe$stage == "scores"], "completed")
  expect_identical(recipe$status[recipe$stage == "missing" & recipe$scope == "measurement_model"],
                   "not_computed")
})


test_that("the missing-data log names only the strategies that were fitted (#145)", {
  observe <- nomologR:::nomo_run_missing_observation
  strategies <- tibble::tibble(
    strategy = c("listwise", "ml", "pairwise"),
    available = c(TRUE, FALSE, FALSE),
    note = c("", "Not fitted: no modeled variable has missing values.", "lavaan refused it.")
  )
  out <- observe(strategies, "measurement model")
  expect_false(out$compared)
  expect_identical(
    out$text,
    paste("The measurement model could be fitted under listwise deletion only, so no",
          "missing-data strategies were compared. Not fitted: FIML (no modeled variable has",
          "missing values); pairwise deletion (lavaan refused it).")
  )
  strategies$available <- TRUE
  out <- observe(strategies, "network")
  expect_true(out$compared)
  expect_identical(
    out$text,
    paste("The network was fitted under listwise deletion, FIML, and pairwise deletion to",
          "show how much its results depend on missing-data handling.")
  )
  strategies$available <- FALSE
  expect_match(observe(strategies, "network")$text, "under no strategy only", fixed = TRUE)

  skip_on_cran()
  # Complete data leave nothing to compare, and the log no longer claims a refit.
  dat <- make_m8_full_data(seed = 8517L)
  run <- nomo_run(data = dat, scales = list(WellBeing = c("i1", "i2", "i3", "i4")),
                  settings = c(m8_full_settings(), list(missing = list(reliability = FALSE))),
                  decisions = list(factor_count = 1L, cfa_model = m8_model()))
  row <- run$decision_log[run$decision_log$id == "missing_data_cfa", ]
  expect_match(row$observation, "could be fitted under listwise deletion only", fixed = TRUE)
  expect_identical(row$consequence,
                   "The fitted model and its results are unchanged, and there is no refit to compare.")
  expect_true(paste("Missing-data sensitivity: only one strategy could be fitted for the",
                    "measurement model") %in% nomologR:::nomo_run_key_evidence(run))
})


# The shared output style (#144) ------------------------------------------------

run_lines <- function(x, width = 80L) {
  old <- options(width = width)
  on.exit(options(old))
  utils::capture.output(print(x))
}


test_that("a blocked run prints one block with what to do, not a decision request (#145)", {
  run <- make_m9_minimal_run()
  run$status <- "blocked"
  run$next_stage <- "factors"
  run$stage_status$status[2L] <- "blocked"
  run$blocked <- list(stage = "factors", scope = "S",
                      message = "`nomo_factors()` requires at least three candidate items")
  run$decision_requests$id <- "blocked:factors:S"
  out <- run_lines(run)
  text <- paste(out, collapse = "\n")
  expect_false(grepl("Researcher decision required", text, fixed = TRUE))
  expect_false(grepl("consequential decision is unresolved", text, fixed = TRUE))
  expect_identical(sum(grepl("requires at least three candidate items", out, fixed = TRUE)), 1L)
  expect_true("Blocked at the factors stage (S)" %in% out)
  expect_true("  `nomo_factors()` requires at least three candidate items." %in% out)
  expect_match(text, "workflow cannot be resumed or revised.", fixed = TRUE)
  expect_true(any(grepl("| Blocked: factors", out, fixed = TRUE)))
  expect_identical(utils::tail(out, 2L), c(
    "See summary(x) for the stages and recorded decisions and",
    "nomo_table(x, \"component_log\") for the component logs."
  ))

  run$mode <- "research"
  expect_true("  No later stage was run. Correct the input and start a new nomo_run()." %in%
                run_lines(run))

  # The summary shows the same block in place of the request.
  s <- paste(utils::capture.output(print(summary(run))), collapse = "\n")
  expect_match(s, "Blocked at the factors stage (S)", fixed = TRUE)
  expect_false(grepl("Researcher decision required", s, fixed = TRUE))
})


test_that("the guided run's facts follow the shared style (#144, #145)", {
  run <- make_m9_minimal_run()
  out <- run_lines(run)
  expect_identical(out[1:4], c(
    "<nomo_run> Guided workflow",
    "Status: Paused | Mode: teaching | Sample design: same sample",
    "Exploratory cases: 5 | Confirmatory cases: 5 | Scales: 1",
    "Completed: screen -> factors | Next: EFA"
  ))
  # No all-caps status, and stage codes are read as words.
  expect_false(any(grepl("PAUSED|measurement_review", out)))
  expect_true("Researcher decision required on the factor count" %in% out)
  expect_true("EFA = exploratory factor analysis." %in% out)

  next_fact <- nomologR:::nomo_run_next_fact
  expect_identical(next_fact(list(status = "paused", next_stage = "measurement_review")),
                   "Next: measurement review")
  expect_identical(next_fact(list(status = "paused", next_stage = "restart")),
                   "Next: revision with nomo_revise() or a new run")
  expect_identical(next_fact(list(status = "complete", next_stage = NULL)), "")
  expect_identical(next_fact(list(status = "blocked", next_stage = "cfa")), "Blocked: CFA")

  words <- nomologR:::nomo_run_stage_words
  expect_identical(words(c("efa", "measurement_review", "theory_network")),
                   c("EFA", "measurement review", "theory network"))
  expect_identical(words(c("screen", "missing"), cell = TRUE), c("Screen", "Missing data"))
  expect_identical(nomologR:::nomo_run_status_words(c("not_started", "awaiting_decision")),
                   c("Not started", "Awaiting decision"))
})


test_that("each decision request has a title in words, and shared items are counted once (#145)", {
  title <- nomologR:::nomo_run_request_title
  expect_identical(title("factor_count:A", 2L), "Researcher decision required on the factor counts")
  expect_identical(title("factor_count:A", 1L), "Researcher decision required on the factor count")
  expect_identical(title("cfa_model", 1L), "Researcher decision required on the measurement model")
  expect_identical(title("measurement_model", 1L),
                   "Researcher decision required after the measurement review")
  expect_identical(title("restart_with_revised_model", 1L),
                   "Researcher decision required on a revision")

  # A request for the whole model carries no scope label.
  run <- make_m9_minimal_run()
  run$decision_requests$id <- "cfa_model"
  run$decision_requests$scope <- "measurement_model"
  out <- run_lines(run)
  expect_true("  - Retention evidence is available." %in% out)
  expect_false(any(grepl("measurement_model:", out, fixed = TRUE)))

  skip_on_cran()
  dat <- make_m8_full_data(seed = 8520L)
  shared <- nomo_run(data = dat, scales = list(A = c("i1", "i2", "i3"), B = c("i3", "i4", "i2")),
                     settings = m8_full_settings())
  expect_match(nomologR:::nomo_run_key_evidence(shared)[[1L]],
               "^Item audit: 4 items \\(6 audits, since an item in two scales is audited in each\\); flags: ")
})


test_that("key evidence follows the number rules and names the fit version (#144, #145)", {
  skip_on_cran()
  cfa <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5",
                  data = nomo_demo_continuous, estimator = "MLR")
  evidence <- nomologR:::nomo_run_key_evidence(list(results = list(cfa = cfa)))
  expect_match(evidence, "^CFA: CFI \\.[0-9]{3} \\(robust\\), RMSEA 0\\.[0-9]{3} \\(robust\\), SRMR 0\\.[0-9]{3};")
  # A model its fit cannot test says so instead of quoting perfect fit.
  just <- nomo_cfa("A =~ a1 + a2 + a3", data = nomo_demo_continuous)
  expect_match(nomologR:::nomo_run_key_evidence(list(results = list(cfa = just))),
               "^CFA: fit not testable \\(just identified\\); loading flags: ")
  # Resuming a run saved without settings still merges.
  merged <- nomologR:::nomo_run_merge_future_settings(
    list(scales = list(A = c("a1", "a2", "a3"))), list(cfa = list(std.lv = TRUE))
  )
  expect_identical(merged$settings, list(cfa = list(std.lv = TRUE)))

  complete <- make_m9_report_run()
  evidence <- nomologR:::nomo_run_key_evidence(complete)
  # Omega is a reliability: two decimals, no leading zero.
  expect_true(any(grepl("^Reliability: omega \\.[0-9]{2}$", evidence)))
  expect_match(evidence[startsWith(evidence, "CFA:")], "CFI (1\\.000|\\.[0-9]{3}), RMSEA 0\\.")

  # Each value stays on one line with what it measures.
  bound <- nomologR:::nomo_run_bind_evidence(c(
    "CFA: CFI .930 (robust), RMSEA 0.051, SRMR 0.052; loading flags: none",
    "Parallel analysis suggests: Agency 1, Persistence --"
  ))
  nb <- nomologR:::nomo_present_nbsp
  expect_identical(bound, c(
    paste0("CFA: CFI", nb, ".930", nb, "(robust), RMSEA", nb, "0.051, SRMR", nb,
           "0.052; loading flags: none"),
    paste0("Parallel analysis suggests: Agency", nb, "1, Persistence", nb, "--")
  ))
})


test_that("print and summary of a complete run define each abbreviation and end with a pointer (#144)", {
  complete <- make_m9_report_run()
  out <- run_lines(complete)
  text <- paste(out, collapse = " ")
  expect_match(text, "Status: Complete", fixed = TRUE)
  expect_match(text, "EFA = exploratory factor analysis; CFA = confirmatory factor analysis;",
               fixed = TRUE)
  expect_match(text, "SRMR = standardized root mean square residual.", fixed = TRUE)
  expect_identical(utils::tail(out, 2L), c(
    "See summary(x) for the stages and recorded decisions and",
    "nomo_report(x, file = \"report.html\") for an archived report."
  ))

  s <- utils::capture.output(print(summary(complete)))
  expect_true(all(c("Abbreviations", "Methods", "Primary methods", "Component recipe") %in% s))
  expect_true("  EFA -- Exploratory factor analysis." %in% s)
  expect_true(any(grepl("^  Methods used: [0-9]+ \\([0-9]+ primary, [0-9]+ historical\\)$", s)))
  expect_true(any(grepl("^  Screen +Completed$", s)))
  expect_false(any(grepl("^  [A-Z][A-Za-z]+ +(completed|not started|not requested)$", s)))
  expect_match(paste(s, collapse = " "),
               "nomo_table\\(x, \"component_log\"\\) for the [0-9]+ rows of the component logs\\.$")

  # Every line fits a narrow console, prose to 39 columns.
  for (lines in list(run_lines(complete, 40L), run_lines(summary(complete), 40L))) {
    expect_true(all(nchar(lines) <= 40L))
  }
})


test_that("the summary lists each component flag where it was raised (#144)", {
  place <- nomologR:::nomo_run_flag_place
  expect_identical(
    place(c("screen", "screen", "factors", "efa", "cfa", "reliability", "missing", "missing", "scores"),
          c("Agency", "careless_responding", "Agency", "Agency", "measurement_model",
            "measurement_model", "measurement_model", "theory_network", "measurement_model")),
    c("the Agency item audit", "the careless-responding screen", "the Agency factor retention",
      "the Agency EFA", "the CFA", "reliability", "the missing-data comparison",
      "the network's missing-data comparison", "scores")
  )
  expect_identical(place("workflow", "workflow"), "workflow")

  log <- tibble::tibble(
    pipeline_component = c("cfa", "validity", "factors", "screen"),
    pipeline_scope = c("measurement_model", "measurement_model", "Agency", "Agency"),
    object = c("b5", "b5", "retention", "ag1"),
    severity = c("review", "review", "review", "info"),
    observation = c("Low loading.", "Low loading.", "The rules disagree.", "Fine.")
  )
  scales <- tibble::tibble(scale = "Agency", n_items = 1L, items = "b5")
  out <- utils::capture.output(nomologR:::nomo_run_present_flagged(log, scales))
  expect_identical(out, c(
    "", "Flagged",
    "  - b5 in the CFA, b5 in validity (Review): Low loading.",
    "  - The Agency factor retention (Review): The rules disagree."
  ))
  expect_identical(utils::capture.output(nomologR:::nomo_run_present_flagged(log[0, ], scales)),
                   character())
})


test_that("an output with no abbreviation prints no note (#144)", {
  expect_identical(utils::capture.output(nomologR:::nomo_run_present_abbreviations("Status: Blocked")),
                   character())
  expect_identical(nomologR:::nomo_run_abbreviations(c("nomo_cfa() and HTMT2", "MLR")),
                   c("HTMT2", "MLR"))
})
