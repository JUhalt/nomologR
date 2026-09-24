# Fixtures ---------------------------------------------------------------------
#
# Genuine contentvalidR output from the v0.6.0 and v0.7.0 tags, stored so the
# reader is tested against the real interface without contentvalidR installed
# (#46, #53). See fixtures/contentvalidR/README.md and MANIFEST.csv.

handoff_fixture <- function(fit, version) {
  readRDS(test_path(
    "fixtures", "contentvalidR", sprintf("handoff-%s-v%s.rds", fit, version)
  ))
}


# Response data with the reviewed items' names: two correlated factors, five
# response options.
handoff_responses <- function(items, n = 300, seed = 46) {
  set.seed(seed)
  f <- stats::rnorm(n)
  out <- lapply(items, function(i) {
    as.numeric(pmin(5, pmax(1, round(3 + f + stats::rnorm(n, sd = .9)))))
  })
  names(out) <- items
  as.data.frame(out)
}

walkthrough_items <- c(paste0("EF", 1:6), paste0("TF", 1:6))


# Reading every fixture ---------------------------------------------------------

test_that("every stored handoff is read without contentvalidR", {
  for (fit in c("walkthrough-sort", "expert-krippendorff", "delphi")) {
    for (version in c("0.6.0", "0.7.0")) {
      raw <- handoff_fixture(fit, version)
      h <- nomologR:::nomo_handoff_read(raw)

      expect_s3_class(h, "nomo_handoff")
      expect_identical(h$provenance$schema_version, 1L)
      expect_identical(h$provenance$package_version, version)
      expect_identical(h$items, raw$item_evidence$item[raw$item_evidence$carried])
      # Keying fields first appear in 0.7.0; before that they are absent, not
      # an error.
      expect_identical(h$keying$recorded, identical(version, "0.7.0"))
    }
  }
})


test_that("the two producer versions agree on every carry decision", {
  for (fit in c("walkthrough-sort", "expert-krippendorff", "delphi")) {
    old <- nomologR:::nomo_handoff_read(handoff_fixture(fit, "0.6.0"))
    new <- nomologR:::nomo_handoff_read(handoff_fixture(fit, "0.7.0"))
    expect_identical(old$items, new$items)
    expect_identical(old$scales, new$scales)
    shared <- c("item", "scale", "carried", "status", "recommendation", "rule")
    expect_identical(old$evidence[, shared], new$evidence[, shared])
  }
})


test_that("declared keying maps onto reverse and scale_range as agreed", {
  # 0.7.0 walkthrough: EF2 and TF2 reverse-worded on a 1-5 response scale.
  keyed <- nomologR:::nomo_handoff_read(handoff_fixture("walkthrough-sort", "0.7.0"))$keying
  expect_true(keyed$declared)
  expect_identical(keyed$reverse, c("EF2", "TF2"))
  expect_identical(keyed$scale_range, c(1, 5))

  # 0.7.0 expert panel: keying NA throughout means nobody said.
  unknown <- nomologR:::nomo_handoff_read(handoff_fixture("expert-krippendorff", "0.7.0"))$keying
  expect_true(unknown$recorded)
  expect_false(unknown$declared)
  expect_null(unknown$reverse)
  expect_null(unknown$scale_range)

  # "Checked, none reversed" is a different fact from "nobody said".
  raw <- handoff_fixture("walkthrough-sort", "0.7.0")
  raw$item_evidence$keying <- 1L
  read <- nomologR:::nomo_handoff_read(raw)
  none <- read$keying
  expect_true(none$declared)
  expect_identical(none$reverse, character(0))
  log <- nomologR:::nomo_handoff_log(read)
  expect_match(log$observation[log$metric == "keying"],
               "declared keying with no reverse-keyed item", fixed = TRUE)
})


test_that("a handoff without panel statistics is read, since not every review has them", {
  raw <- handoff_fixture("walkthrough-sort", "0.7.0")
  raw$panel_statistics <- NULL
  expect_null(nomologR:::nomo_handoff_read(raw)$panel)
  expect_s3_class(
    nomologR:::nomo_handoff_read(handoff_fixture("expert-krippendorff", "0.7.0"))$panel,
    "tbl_df"
  )
})


# Screening ---------------------------------------------------------------------

test_that("nomo_screen screens only carried items and quotes held-back ones verbatim", {
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  data <- handoff_responses(walkthrough_items)
  out <- nomo_screen(data, items = h)

  expect_identical(out$items, h$items)
  expect_false(any(c("EF5", "TF5") %in% out$item_summary$item))
  expect_s3_class(out$handoff, "nomo_handoff")

  log <- out$decision_log
  provenance <- log[log$metric == "content_review_provenance", ]
  expect_match(provenance$observation, "contentvalidR 0.7.0", fixed = TRUE)
  expect_match(provenance$observation, "not from these data", fixed = TRUE)
  expect_match(provenance$reference, "Howard & Melloy (2016)", fixed = TRUE)

  held <- log[log$metric == "held_back_item", ]
  expect_identical(held$object, c("EF5", "TF5"))
  expect_match(held$observation[[1L]],
               "status \"Review\", recommendation \"Review\"", fixed = TRUE)

  counts <- log[log$metric == "carry_decisions", ]
  expect_match(counts$observation, "10 of 12 reviewed item(s) were carried and 2 held back",
               fixed = TRUE)
})


test_that("the handoff's scales and keying reach the careless-responding indices", {
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  out <- nomo_screen(handoff_responses(walkthrough_items), items = h, effort = TRUE)

  expect_identical(out$effort_settings$reverse, c("EF2", "TF2"))
  expect_identical(out$effort_settings$scale_range, c(1, 5))
  expect_identical(names(out$effort_settings$scales), c("EF", "TF"))
  keying <- out$decision_log[out$decision_log$metric == "keying", ]
  expect_match(keying$observation, "EF2, TF2, on a 1 to 5 response scale", fixed = TRUE)
})


test_that("a handoff without keying leaves keying undeclared, never forward", {
  h <- handoff_fixture("walkthrough-sort", "0.6.0")
  out <- nomo_screen(handoff_responses(walkthrough_items), items = h, effort = TRUE)

  expect_null(out$effort_settings$reverse)
  keying <- out$decision_log[out$decision_log$metric == "keying", ]
  expect_match(keying$observation, "predates keying fields", fixed = TRUE)
})


test_that("keying supplied in the call is the researcher's and is recorded as such", {
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  out <- nomo_screen(handoff_responses(walkthrough_items), items = h, effort = TRUE,
                     reverse = "EF2", scale_range = c(1, 5))

  expect_identical(out$effort_settings$reverse, "EF2")
  expect_true("keying_override" %in% out$decision_log$metric)
})


test_that("a carried item missing from the data is refused, not dropped", {
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  data <- handoff_responses(setdiff(walkthrough_items, c("EF1", "TF6")))
  expect_error(nomo_screen(data, items = h), "not columns of `data`: EF1, TF6", fixed = TRUE)
})


test_that("a review with no construct mapping can be screened", {
  h <- handoff_fixture("delphi", "0.7.0")
  out <- nomo_screen(handoff_responses(paste0("S", 1:5)), items = h)

  expect_identical(out$items, c("S1", "S2", "S3", "S5"))
  held <- out$decision_log[out$decision_log$metric == "held_back_item", ]
  expect_identical(held$object, "S4")
  expect_match(held$observation,
               "status \"Review\", recommendation \"No consensus\"", fixed = TRUE)
})


# The guided workflow -----------------------------------------------------------

test_that("nomo_run takes its scales and their provenance from the handoff", {
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  run <- nomo_run(handoff_responses(walkthrough_items), scales = h,
                  settings = list(factors = list(seed = 46)))

  expect_identical(run$scales, h$scales)
  expect_s3_class(run$handoff, "nomo_handoff")

  log <- run$decision_log
  review <- log[log$id == "content_review", ]
  expect_identical(review$source, "content_review")
  expect_match(review$observation, "contentvalidR 0.7.0", fixed = TRUE)

  held <- log[startsWith(log$id, "held_back:"), ]
  expect_identical(held$scope, c("EF", "TF"))
  expect_true(all(held$source == "content_review"))

  definitions <- log[startsWith(log$id, "scale_definition:"), ]
  expect_true(all(definitions$source == "content_review"))
  expect_match(definitions$observation[[1L]], "carried from content review", fixed = TRUE)
})


test_that("nomo_run refuses a review with no construct mapping and says what to do", {
  h <- handoff_fixture("expert-krippendorff", "0.7.0")
  expect_error(
    nomo_run(handoff_responses(paste0("Item", 1:5)), scales = h),
    "does not invent construct membership", fixed = TRUE
  )
  expect_error(
    nomo_run(handoff_responses(paste0("Item", 1:5)), scales = h),
    "Carried items: Item1, Item2, Item3.", fixed = TRUE
  )
})


test_that("a run without a handoff is unchanged", {
  scales <- list(A = c("EF1", "EF2", "EF3"), B = c("TF1", "TF2", "TF3"))
  run <- nomo_run(handoff_responses(walkthrough_items), scales = scales,
                  settings = list(factors = list(seed = 46)))
  expect_null(run$handoff)
  expect_false("content_review" %in% run$decision_log$source)
})


# Refusals ----------------------------------------------------------------------

test_that("an unknown schema version is refused, naming both package versions", {
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  h$provenance$schema_version <- 2L
  expect_error(
    nomologR:::nomo_handoff_read(h),
    "schema version 2, written by contentvalidR 0.7.0", fixed = TRUE
  )
  expect_error(nomologR:::nomo_handoff_read(h), "reads schema version 1", fixed = TRUE)
})


test_that("fields this release does not know are ignored", {
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  h$future_field <- "added in a later minor release"
  h$item_evidence$future_column <- "x"
  expect_identical(nomologR:::nomo_handoff_read(h)$items,
                   nomologR:::nomo_handoff_read(handoff_fixture("walkthrough-sort", "0.7.0"))$items)
})


test_that("objects content_handoff() could not have produced are refused as malformed", {
  base <- handoff_fixture("walkthrough-sort", "0.7.0")
  malformed <- function(h) {
    expect_error(nomologR:::nomo_handoff_read(h), "This handoff is malformed", fixed = TRUE)
  }

  h <- base; h$item_evidence$keying[1L] <- NA
  malformed(h)

  h <- base; h$item_evidence$response_min[1L] <- NA
  malformed(h)

  h <- base; h$item_evidence$response_max <- h$item_evidence$response_max + (seq_len(nrow(h$item_evidence)) == 1L)
  malformed(h)

  h <- base; h$items <- c(h$items, "EF5")
  malformed(h)

  h <- base; h$scales <- NULL
  malformed(h)

  h <- base; h$item_evidence$carried[1L] <- NA
  malformed(h)

  h <- base; h$item_evidence$rule <- NULL
  malformed(h)

  h <- base; h$item_statistics <- "not a table"
  malformed(h)

  h <- base; h$item_evidence <- "not a table"
  malformed(h)

  delphi <- handoff_fixture("delphi", "0.7.0")
  delphi$scales <- list(A = delphi$items)
  malformed(delphi)
})


test_that("an item placed in two scales is noted before it becomes a cross-loading", {
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  h$scales$EF <- c(h$scales$EF, "TF1")
  out <- nomo_screen(handoff_responses(walkthrough_items), items = h)
  shared <- out$decision_log[out$decision_log$metric == "shared_items", ]
  expect_identical(shared$severity, "review")
  expect_match(shared$observation, "TF1 belong to more than one scale", fixed = TRUE)
})


# The report ---------------------------------------------------------------------

test_that("a run from a handoff reports its content review; other runs do not", {
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  run <- nomo_run(handoff_responses(walkthrough_items), scales = h,
                  settings = list(factors = list(seed = 46)))

  review <- nomologR:::nomo_report_content_review(run)
  expect_match(review$summary, "content review in contentvalidR 0.7.0", fixed = TRUE)
  expect_match(review$summary, "10 of 12 reviewed item(s) were carried", fixed = TRUE)
  expect_match(review$summary, "EF2, TF2, on a 1 to 5 response scale", fixed = TRUE)
  expect_match(review$summary, "Howard & Melloy (2016)", fixed = TRUE)
  expect_identical(nrow(review$items), 12L)
  expect_identical(review$items$recommendation[review$items$item == "EF5"], "Review")

  scales <- list(A = c("EF1", "EF2", "EF3"), B = c("TF1", "TF2", "TF3"))
  plain <- nomo_run(handoff_responses(walkthrough_items), scales = scales,
                    settings = list(factors = list(seed = 46)))
  expect_null(nomologR:::nomo_report_content_review(plain))

  # Each keying state has its own sentence.
  undeclared <- nomo_run(handoff_responses(walkthrough_items),
                         scales = handoff_fixture("walkthrough-sort", "0.6.0"),
                         settings = list(factors = list(seed = 46)))
  expect_match(nomologR:::nomo_report_content_review(undeclared)$summary,
               "predates keying fields", fixed = TRUE)
  run$handoff$keying$declared <- FALSE
  expect_match(nomologR:::nomo_report_content_review(run)$summary,
               "Reverse keying was not declared.", fixed = TRUE)
  run$handoff$keying$declared <- TRUE
  run$handoff$keying$reverse <- character(0)
  expect_match(nomologR:::nomo_report_content_review(run)$summary,
               "no reverse-keyed item", fixed = TRUE)
  run$handoff$keying$reverse <- "EF2"
  run$handoff$keying$scale_range <- NULL
  expect_match(nomologR:::nomo_report_content_review(run)$summary,
               "with no response scale recorded", fixed = TRUE)
})


test_that("the rendered report opens with the content review", {
  skip_on_cran()
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if_not(rmarkdown::pandoc_available())

  run <- nomo_run(handoff_responses(walkthrough_items),
                  scales = handoff_fixture("walkthrough-sort", "0.7.0"),
                  settings = list(factors = list(seed = 46)))
  file <- nomo_report(run, file = tempfile(fileext = ".html"),
                      include_plots = FALSE, include_session = FALSE, quiet = TRUE)
  # Pandoc wraps long lines, so whitespace is collapsed before matching.
  html <- gsub("[[:space:]]+", " ", paste(readLines(file, warn = FALSE), collapse = " "))

  expect_match(html, 'id="content-review"', fixed = TRUE)
  expect_match(html, "content review in contentvalidR 0.7.0", fixed = TRUE)
  expect_match(html, "Howard &amp; Melloy", fixed = TRUE)
})


test_that("the handoff and the keying the screen reports are the same, whatever the version", {
  # Keying declared with no range: nothing to recode against, and the screen's
  # own refusal is the backstop when reverse-keyed items would need recoding.
  h <- handoff_fixture("walkthrough-sort", "0.7.0")
  h$item_evidence$response_min <- NA_integer_
  h$item_evidence$response_max <- NA_integer_
  read <- nomologR:::nomo_handoff_read(h)
  expect_identical(read$keying$reverse, c("EF2", "TF2"))
  expect_null(read$keying$scale_range)
  expect_match(nomologR:::nomo_handoff_log(read)$observation[
    nomologR:::nomo_handoff_log(read)$metric == "keying"],
    "without the response scale", fixed = TRUE)
  expect_error(
    nomo_screen(handoff_responses(walkthrough_items), items = h, effort = TRUE),
    "scale_range"
  )
})
