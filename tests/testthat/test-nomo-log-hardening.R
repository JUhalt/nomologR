test_that("internal decision-log schema and row binding are stable", {
  log <- nomologR:::nomo_log_new()

  expect_s3_class(log, "data.frame")
  expect_equal(
    names(log),
    c(
      "stage", "object", "metric", "value", "reference", "severity",
      "observation", "recommendation", "decision", "rationale"
    )
  )
  expect_equal(nrow(log), 0L)

  log2 <- nomologR:::nomo_log_add(
    log,
    stage = "test",
    object = "item",
    metric = "demo",
    value = 1,
    reference = "reference",
    severity = "review",
    observation = "observation",
    recommendation = "recommendation",
    decision = "retain",
    rationale = "researcher rationale"
  )

  expect_equal(nrow(log2), 1L)
  expect_equal(log2$severity, "review")
  expect_equal(log2$value, 1)
  expect_equal(log2$decision, "retain")
  expect_equal(log2$rationale, "researcher rationale")
})


test_that("internal decision-log validation rejects malformed inputs", {
  expect_error(
    nomologR:::nomo_log_add(
      list(),
      stage = "test",
      object = "item",
      metric = "metric"
    ),
    "data frame"
  )

  expect_error(
    nomologR:::nomo_log_add(
      nomologR:::nomo_log_new(),
      stage = "test",
      object = "item",
      metric = "metric",
      severity = "unknown"
    ),
    "arg"
  )
})


test_that("legacy not-implemented guard remains explicit until release cleanup", {
  expect_error(
    nomologR:::nomo_not_implemented("example", "Milestone X"),
    "example\\(\\) is part of Milestone X"
  )
  expect_error(
    nomologR:::nomo_not_implemented("example", "Milestone X"),
    "public API is being stabilized"
  )
})
