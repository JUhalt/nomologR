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
