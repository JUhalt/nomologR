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
