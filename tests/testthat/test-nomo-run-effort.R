# Careless responding in the guided workflow (#73) -----------------------------

run_effort_data <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) return(cache)
    # Three scales of four items on a 1-5 scale, with ten straight-liners.
    set.seed(73)
    n <- 300
    make <- function(f) as.numeric(pmin(5, pmax(1, round(3 + f + stats::rnorm(n, sd = .8)))))
    cols <- list()
    for (s in c("A", "B", "C")) {
      f <- stats::rnorm(n)
      for (j in 1:4) cols[[paste0(tolower(s), j)]] <- make(f)
    }
    dat <- as.data.frame(cols)
    dat[1:10, ] <- 3
    cache <<- list(
      data = dat,
      scales = list(A = paste0("a", 1:4), B = paste0("b", 1:4), C = paste0("c", 1:4))
    )
    cache
  }
})


run_effort <- function(screen = list(effort = TRUE), scales = run_effort_data()$scales) {
  nomo_run(run_effort_data()$data, scales = scales,
           settings = list(screen = screen, factors = list(seed = 73)))
}


test_that("careless responding is computed once across the instrument, not per scale", {
  run <- run_effort()
  fx <- run_effort_data()

  e <- run$results$effort
  expect_s3_class(e, "nomo_screen")
  expect_identical(e$items, unlist(fx$scales, use.names = FALSE))
  expect_identical(e$effort_settings$scales, fx$scales)
  # Across three scales even-odd consistency exists; within one scale it cannot.
  expect_true(any(is.finite(e$effort$even_odd)))

  # The per-scale audits stay as they were, without the indices.
  for (scope in names(fx$scales)) {
    expect_null(run$results$screen[[scope]]$effort)
  }

  # It matches the standalone call exactly.
  standalone <- nomo_screen(fx$data, items = unlist(fx$scales, use.names = FALSE),
                            effort = TRUE, scales = fx$scales)
  expect_identical(e$effort, standalone$effort)

  entry <- run$decision_log[run$decision_log$id == "careless_responding", ]
  expect_identical(entry$scope, "instrument")
  expect_match(entry$observation, "once across all 12 items, using 3 scale(s)", fixed = TRUE)
  expect_match(entry$observation, "No reverse keying was declared", fixed = TRUE)
})


test_that("a run that does not ask for careless responding is unchanged", {
  run <- nomo_run(run_effort_data()$data, scales = run_effort_data()$scales,
                  settings = list(factors = list(seed = 73)))
  expect_null(run$results$effort)
  expect_false("careless_responding" %in% run$decision_log$id)
})


test_that("the careless-responding log reaches the run's component log", {
  run <- run_effort()
  component <- nomo_table(run, "component_log")
  effort_rows <- component[component$pipeline_scope == "careless_responding", ]
  expect_gt(nrow(effort_rows), 0L)
  expect_true("long_string" %in% effort_rows$metric)
  expect_true(all(effort_rows$pipeline_component == "screen"))
})


test_that("careless-responding methods are credited to the run", {
  used <- nomo_methods(run_effort())$id
  expect_true(any(grepl("long_string|inter_item_sd|even_odd", used)))
})


test_that("keying set in settings is used and recorded", {
  run <- run_effort(list(effort = TRUE, reverse = "a2", scale_range = c(1, 5)))
  expect_identical(run$results$effort$effort_settings$reverse, "a2")
  entry <- run$decision_log[run$decision_log$id == "careless_responding", ]
  expect_match(entry$observation, "set in `settings$screen`", fixed = TRUE)
  expect_identical(entry$source, "researcher_input")
})


test_that("an unreadable effort setting is refused before any stage runs", {
  expect_error(run_effort(list(effort = "yes")),
               "`settings$screen$effort` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(run_effort(list(effort = NA)), "TRUE or FALSE", fixed = TRUE)
})


test_that("a handoff's declared keying reaches the run, and settings override it", {
  h <- readRDS(test_path("fixtures", "contentvalidR", "handoff-walkthrough-sort-v0.7.0.rds"))
  set.seed(73)
  n <- 300
  f <- stats::rnorm(n)
  items <- c(paste0("EF", 1:6), paste0("TF", 1:6))
  data <- as.data.frame(stats::setNames(lapply(items, function(i) {
    as.numeric(pmin(5, pmax(1, round(3 + f + stats::rnorm(n, sd = .9)))))
  }), items))

  from_handoff <- nomo_run(data, scales = h,
                           settings = list(screen = list(effort = TRUE),
                                           factors = list(seed = 73)))
  settings <- from_handoff$results$effort$effort_settings
  expect_identical(settings$reverse, c("EF2", "TF2"))
  expect_identical(settings$scale_range, c(1, 5))
  entry <- from_handoff$decision_log[from_handoff$decision_log$id == "careless_responding", ]
  expect_identical(entry$source, "content_review")
  expect_match(entry$observation, "came from the content-review handoff", fixed = TRUE)

  overridden <- nomo_run(data, scales = h,
                         settings = list(screen = list(effort = TRUE, reverse = "EF2",
                                                       scale_range = c(1, 5)),
                                         factors = list(seed = 73)))
  expect_identical(overridden$results$effort$effort_settings$reverse, "EF2")
  entry <- overridden$decision_log[overridden$decision_log$id == "careless_responding", ]
  expect_match(entry$observation, "in place of the keying the content-review handoff declared",
               fixed = TRUE)
})


test_that("a careless-responding screen that fails blocks the run with its reason", {
  # A reverse-keyed item outside the run is refused by nomo_screen().
  run <- run_effort(list(effort = TRUE, reverse = "zz9", scale_range = c(1, 5)))
  expect_identical(run$status, "blocked")
  expect_identical(run$blocked$scope, "careless_responding")
})


test_that("the report summarizes careless responding after the item audits", {
  report <- nomologR:::nomo_report_effort(run_effort()$results$effort)
  expect_match(report$summary, "Computed once across all 12 items for 300 cases, using 3 scale(s).",
               fixed = TRUE)
  expect_identical(report$indices$index[[1L]], "Long-string")
  expect_gte(report$indices$cases_flagged[[1L]], 10L)
  expect_true(is.na(report$indices$cases_flagged[[3L]]))
  expect_match(report$indices$rule[[1L]], "a run of 6 or more, half the items", fixed = TRUE)
  expect_true(all(report$log$metric %in% c("long_string", "psychometric_antonym",
                                           "psychometric_synonym", "even_odd",
                                           "index_disagreement")))
})


test_that("the rendered report carries the careless-responding section", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  file <- nomo_report(run_effort(), file = tempfile(fileext = ".html"),
                      include_plots = FALSE, include_session = FALSE, quiet = TRUE)
  html <- gsub("[[:space:]]+", " ", paste(readLines(file, warn = FALSE), collapse = " "))
  expect_match(html, 'id="careless-responding"', fixed = TRUE)
  expect_match(html, "Computed once across all 12 items", fixed = TRUE)
})
