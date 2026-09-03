test_that("internal decision log has the expected schema", {
  log <- nomologR:::nomo_log_new()

  expect_s3_class(log, "data.frame")
  expect_named(
    log,
    c(
      "stage", "object", "metric", "value", "reference", "severity",
      "observation", "recommendation", "decision", "rationale"
    )
  )
})

test_that("decision log appends a transparent review record", {
  log <- nomologR:::nomo_log_new()
  log <- nomologR:::nomo_log_add(
    log,
    stage = "screen",
    object = "item_4",
    metric = "corrected item-total correlation",
    value = 0.27,
    reference = "teaching reference ~= .30",
    severity = "review",
    observation = "The item is somewhat weakly associated with the remaining items.",
    recommendation = "Inspect wording, construct coverage, and redundancy before deciding."
  )

  expect_equal(nrow(log), 1L)
  expect_identical(log$severity[[1]], "review")
  expect_equal(log$value[[1]], 0.27)
  expect_match(log$recommendation[[1]], "before deciding")
})

test_that("decision log rejects unsupported severity labels", {
  expect_error(
    nomologR:::nomo_log_add(
      nomologR:::nomo_log_new(),
      stage = "screen",
      object = "item_1",
      metric = "example",
      severity = "delete"
    )
  )
})
