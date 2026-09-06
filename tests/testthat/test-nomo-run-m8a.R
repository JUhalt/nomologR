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
