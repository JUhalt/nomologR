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
  expect_equal(nrow(settings), 8L)
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

  expect_output(print(teaching), "Observation:")
  expect_output(print(research), "Decision requests")
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

  expect_output(print(summary(out)), "Stage status")
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

  expect_output(print(teaching), "Observation:")
  expect_output(print(research), "Decision requests")
})
