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
  skip_on_cran()
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
  expect_match(entry$observation, "once across all 12 items, using 3 scales", fixed = TRUE)
  expect_match(entry$observation, "No reverse keying was declared", fixed = TRUE)
})


test_that("a run that does not ask for careless responding is unchanged", {
  skip_on_cran()
  run <- nomo_run(run_effort_data()$data, scales = run_effort_data()$scales,
                  settings = list(factors = list(seed = 73)))
  expect_null(run$results$effort)
  expect_false("careless_responding" %in% run$decision_log$id)
})


test_that("the careless-responding log reaches the run's component log", {
  skip_on_cran()
  run <- run_effort()
  component <- nomo_table(run, "component_log")
  effort_rows <- component[component$pipeline_scope == "careless_responding", ]
  expect_gt(nrow(effort_rows), 0L)
  expect_true("long_string" %in% effort_rows$metric)
  expect_true(all(effort_rows$pipeline_component == "screen"))
})


test_that("careless-responding methods are credited to the run", {
  skip_on_cran()
  used <- nomo_methods(run_effort())$id
  expect_true(any(grepl("long_string|inter_item_sd|even_odd", used)))
})


test_that("keying set in settings is used and recorded", {
  skip_on_cran()
  run <- run_effort(list(effort = TRUE, reverse = "a2", scale_range = c(1, 5)))
  expect_identical(run$results$effort$effort_settings$reverse, "a2")
  entry <- run$decision_log[run$decision_log$id == "careless_responding", ]
  expect_match(entry$observation, "set in `settings$screen`", fixed = TRUE)
  expect_identical(entry$source, "researcher_input")
})


test_that("the pair threshold set in settings reaches the screen", {
  skip_on_cran()
  run <- run_effort(list(effort = TRUE, pair_magnitude = 0.5))
  expect_identical(run$results$effort$effort_settings$pair_magnitude, 0.5)
})


test_that("an unreadable effort setting is refused before any stage runs", {
  expect_error(run_effort(list(effort = "yes")),
               "`settings$screen$effort` must be TRUE or FALSE.", fixed = TRUE)
  expect_error(run_effort(list(effort = NA)), "TRUE or FALSE", fixed = TRUE)
})


run_effort_handoff <- function(version = "0.7.0", fit = "walkthrough-sort") {
  readRDS(test_path("fixtures", "contentvalidR", sprintf("handoff-%s-v%s.rds", fit, version)))
}


run_effort_handoff_data <- function() {
  set.seed(73)
  n <- 300
  f <- stats::rnorm(n)
  items <- c(paste0("EF", 1:6), paste0("TF", 1:6))
  as.data.frame(stats::setNames(lapply(items, function(i) {
    as.numeric(pmin(5, pmax(1, round(3 + f + stats::rnorm(n, sd = .9)))))
  }), items))
}


run_effort_from_handoff <- function(h, screen) {
  nomo_run(run_effort_handoff_data(), scales = h,
           settings = list(screen = screen, factors = list(seed = 73, n_iter = 10L)))
}


run_effort_entry <- function(run, id) run$decision_log[run$decision_log$id == id, ]


test_that("a handoff's declared keying reaches the run, and settings override it", {
  skip_on_cran()
  h <- run_effort_handoff()

  from_handoff <- run_effort_from_handoff(h, list(effort = TRUE))
  settings <- from_handoff$results$effort$effort_settings
  expect_identical(settings$reverse, c("EF2", "TF2"))
  expect_identical(settings$scale_range, c(1, 5))
  entry <- run_effort_entry(from_handoff, "careless_responding")
  expect_identical(entry$source, "content_review")
  expect_match(entry$observation, "Reverse keying and the response scale came from the content-review handoff.",
               fixed = TRUE)
  expect_false("keying" %in% from_handoff$decision_log$id)

  # Both replaced. c(0, 6) recodes a 1-5 response exactly as c(1, 5) does.
  overridden <- run_effort_from_handoff(h, list(effort = TRUE, reverse = "EF2",
                                                scale_range = c(0, 6)))
  expect_identical(overridden$results$effort$effort_settings$reverse, "EF2")
  expect_identical(overridden$results$effort$effort_settings$scale_range, c(0, 6))
  entry <- run_effort_entry(overridden, "careless_responding")
  expect_identical(entry$source, "researcher_input")
  expect_match(entry$observation, "in place of the keying the content-review handoff declared",
               fixed = TRUE)
  keying <- run_effort_entry(overridden, "keying")
  expect_match(keying$observation,
               "`settings$screen$reverse` (EF2) was used in place of the reverse-keyed item(s) the content-review handoff declared (EF2, TF2).",
               fixed = TRUE)
  expect_match(keying$observation,
               "`settings$screen$scale_range` (0 to 6) was used in place of the response scale the content-review handoff declared (1 to 5).",
               fixed = TRUE)
  expect_identical(keying$decision, "reverse: EF2; scale_range: 0 to 6")
  expect_identical(keying$source, "researcher_input")
})


test_that("reverse and scale_range in settings each replace only their own half of a handoff's keying", {
  skip_on_cran()
  # Only `reverse` given: the handoff's response scale still applies, so the
  # screen runs rather than asking for a range the handoff recorded (#145).
  reverse_only <- run_effort_from_handoff(run_effort_handoff(), list(effort = TRUE, reverse = "EF2"))
  expect_identical(reverse_only$status, "paused")
  expect_identical(reverse_only$results$effort$effort_settings$reverse, "EF2")
  expect_identical(reverse_only$results$effort$effort_settings$scale_range, c(1, 5))
  expect_match(run_effort_entry(reverse_only, "careless_responding")$observation,
               paste("Reverse keying was set in `settings$screen`, in place of the content-review",
                     "handoff's; the response scale came from the content-review handoff."),
               fixed = TRUE)

  # A handoff that declares reverse-keyed items without the response scale:
  # the range given in settings completes the keying and keeps EF2 and TF2.
  no_range <- run_effort_handoff("0.10.1")
  no_range$item_evidence$response_min <- NA
  no_range$item_evidence$response_max <- NA
  expect_identical(run_effort_from_handoff(no_range, list(effort = TRUE))$status, "blocked")
  completed <- run_effort_from_handoff(no_range, list(effort = TRUE, scale_range = c(1, 5)))
  expect_identical(completed$status, "paused")
  expect_identical(completed$results$effort$effort_settings$reverse, c("EF2", "TF2"))
  expect_identical(completed$results$effort$effort_settings$scale_range, c(1, 5))
  entry <- run_effort_entry(completed, "careless_responding")
  expect_match(entry$observation,
               paste("Reverse keying came from the content-review handoff; the response scale was",
                     "set in `settings$screen`, where the content-review handoff recorded none."),
               fixed = TRUE)
  expect_identical(entry$source, "researcher_input")
  expect_match(run_effort_entry(completed, "keying")$observation,
               "`settings$screen$scale_range` (1 to 5) supplied the response scale the content-review handoff did not record.",
               fixed = TRUE)

  # Declared keying with nothing reversed needs no response scale.
  none_reversed <- run_effort_handoff("0.10.1", "walkthrough-sort-none-reversed")
  none_reversed$item_evidence$response_min <- NA
  none_reversed$item_evidence$response_max <- NA
  unrecorded <- run_effort_from_handoff(none_reversed, list(effort = TRUE))
  expect_identical(unrecorded$results$effort$effort_settings$reverse, character(0))
  expect_match(run_effort_entry(unrecorded, "careless_responding")$observation,
               "Reverse keying came from the content-review handoff; the response scale was not recorded.",
               fixed = TRUE)
})


test_that("keying replaced in settings reaches the item audits and the log without effort", {
  skip_on_cran()
  # EF2 and TF2 answered reversed, so their item-rest correlations are negative.
  data <- run_effort_handoff_data()
  data[c("EF2", "TF2")] <- 6 - data[c("EF2", "TF2")]
  run_keyed <- function(screen) {
    nomo_run(data, scales = run_effort_handoff("0.10.1"),
             settings = list(screen = screen, factors = list(seed = 73, n_iter = 10L)))
  }
  keyed_note <- function(run, scope) {
    log <- run$results$screen[[scope]]$decision_log
    any(grepl("declared reverse-keyed", log$observation, fixed = TRUE))
  }

  # A response scale given alone keeps the handoff's reverse-keyed items.
  range_only <- run_keyed(list(scale_range = c(1, 5)))
  expect_true(keyed_note(range_only, "EF"))
  expect_true(keyed_note(range_only, "TF"))
  expect_false("keying" %in% range_only$decision_log$id)

  # Reverse keying replaced without effort is still recorded in the run log.
  replaced <- run_keyed(list(reverse = "EF2"))
  expect_true(keyed_note(replaced, "EF"))
  expect_false(keyed_note(replaced, "TF"))
  expect_false("careless_responding" %in% replaced$decision_log$id)
  keying <- run_effort_entry(replaced, "keying")
  expect_identical(keying$stage, "screen")
  expect_match(keying$observation, "in place of the reverse-keyed item(s)", fixed = TRUE)
})


test_that("keying is resolved one argument at a time", {
  h <- nomologR:::nomo_handoff_read(run_effort_handoff("0.10.1"))
  scales <- h$scales
  keying <- function(screen, handoff = h) {
    nomologR:::nomo_run_screen_keying(list(screen = screen), scales, handoff)
  }

  # A value equal to the handoff's is the handoff's, not a replacement.
  same <- keying(list(reverse = c("TF2", "EF2"), scale_range = c(1L, 5L)))
  expect_identical(unname(same$origin), c("handoff", "handoff"))
  expect_identical(nomologR:::nomo_run_keying_log(nomologR:::nomo_run_workflow_log_new(), same),
                   nomologR:::nomo_run_workflow_log_new())

  # The content-review article's case: responses already recoded.
  replaced <- keying(list(reverse = character(0)))
  expect_identical(unname(replaced$origin), c("override", "handoff"))
  expect_identical(replaced$reverse, character(0))
  row <- nomologR:::nomo_run_keying_log(nomologR:::nomo_run_workflow_log_new(), replaced)
  expect_identical(
    row$observation,
    paste("`settings$screen$reverse` (none) was used in place of the reverse-keyed item(s)",
          "the content-review handoff declared (EF2, TF2).")
  )
  expect_identical(row$decision, "reverse: none; scale_range: 1 to 5")

  # A handoff's reverse-keyed items are those among the run's items.
  fewer <- nomologR:::nomo_run_screen_keying(list(), list(EF = c("EF1", "EF3")), h)
  expect_identical(fewer$reverse, character(0))

  # Without a handoff, settings are the only source.
  plain <- keying(list(reverse = "EF2", scale_range = c(1, 5)), handoff = NULL)
  expect_identical(unname(plain$origin), c("settings", "settings"))
  expect_identical(unname(keying(list(), handoff = NULL)$origin), c("none", "none"))
})


test_that("a response scale set without reverse keying is recorded as such", {
  skip_on_cran()
  run <- run_effort(list(effort = TRUE, scale_range = c(1, 5)))
  expect_identical(run$results$effort$effort_settings$scale_range, c(1, 5))
  expect_match(run_effort_entry(run, "careless_responding")$observation,
               paste("No reverse keying was declared, so no item was recoded; the response scale",
                     "was set in `settings$screen`."),
               fixed = TRUE)
})


test_that("a misspelled reverse-keyed item is refused in both effort modes", {
  # The per-scale audits keep only their own scale's items, so without this
  # check a typo was dropped silently when effort was not requested (#145).
  for (effort in c(FALSE, TRUE)) {
    expect_error(
      run_effort(list(effort = effort, reverse = c("a2", "zz9"), scale_range = c(1, 5))),
      "`settings$screen$reverse` names item(s) outside `scales`: zz9.", fixed = TRUE
    )
  }
  expect_error(run_effort(list(reverse = 2)),
               "`settings$screen$reverse` must be a character vector of item names.", fixed = TRUE)
  expect_error(run_effort(list(reverse = NA_character_)), "character vector", fixed = TRUE)
})


test_that("a careless-responding screen that fails blocks the run with its reason", {
  # An unreadable pair threshold is refused by nomo_screen().
  run <- run_effort(list(effort = TRUE, pair_magnitude = 2))
  expect_identical(run$status, "blocked")
  expect_identical(run$blocked$scope, "careless_responding")
  expect_match(run$blocked$message, "`pair_magnitude`", fixed = TRUE)
})


test_that("the report summarizes careless responding after the item audits", {
  skip_on_cran()
  report <- nomologR:::nomo_report_effort(run_effort()$results$effort)
  expect_match(report$summary, "Computed once across all 12 items for 300 cases, using 3 scales.",
               fixed = TRUE)
  expect_identical(report$indices$index[[1L]], "Long-string")
  # Twelve items are fewer than the long-string rule is applied to (#145), so
  # the report says so instead of counting flags.
  expect_true(is.na(report$indices$cases_flagged[[1L]]))
  expect_identical(report$indices$rule[[1L]], "not applied: fewer than 20 items")
  expect_true(is.na(report$indices$cases_flagged[[3L]]))
  expect_true(all(report$log$metric %in% c("long_string", "psychometric_antonym",
                                           "psychometric_synonym", "even_odd",
                                           "index_disagreement")))

  # The minimum is a guidance setting, and the run's guidance reaches the
  # instrument-wide screen: lowered to twelve items, the rule and its count of
  # the ten straight-liners return.
  lower <- nomo_defaults()
  lower$long_string_min_items <- 12L
  moved <- nomo_run(run_effort_data()$data, scales = run_effort_data()$scales,
                    guidance = lower,
                    settings = list(screen = list(effort = TRUE), factors = list(seed = 73)))
  report <- nomologR:::nomo_report_effort(moved$results$effort)
  expect_gte(report$indices$cases_flagged[[1L]], 10L)
  expect_match(report$indices$rule[[1L]], "a run of 6 or more, half the items", fixed = TRUE)
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
