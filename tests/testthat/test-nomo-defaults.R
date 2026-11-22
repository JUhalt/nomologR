test_that("nomo_defaults returns stable teaching guidance", {
  x <- nomo_defaults()

  expect_identical(x$profile, "teaching")
  expect_equal(x$item_total_reference, 0.30)
  # The fewest screened items at which long-string becomes a flag (#145).
  expect_identical(x$long_string_min_items, 20L)
  expect_equal(x$efa_loading_reference, 0.40)
  expect_equal(x$fit_reference$cfi, 0.95)
  expect_false(x$auto_delete)
  expect_false(x$auto_respecify)
  expect_match(x$interpretation, "do not automatically")
})

test_that("unknown guidance profiles are rejected", {
  expect_error(nomo_defaults("automatic-delete"))
})

test_that("auto_delete and auto_respecify cannot be switched on (#145)", {
  # They record a design rule; setting either to TRUE was silently ignored.
  check <- nomologR:::nomo_defaults_check_safeguards
  expect_identical(check(nomo_defaults()), nomo_defaults())
  expect_identical(check(list()), list())

  guidance <- nomo_defaults()
  guidance$auto_delete <- TRUE
  expect_error(check(guidance), "`guidance$auto_delete` cannot be `TRUE`", fixed = TRUE)
  guidance$auto_respecify <- TRUE
  expect_error(
    check(guidance),
    "`guidance$auto_delete` or `guidance$auto_respecify` cannot be `TRUE`: nomologR never",
    fixed = TRUE
  )

  dat <- nomo_demo_continuous[1:150, paste0("a", 1:5)]
  expect_error(nomo_efa(dat, factors = 1, guidance = guidance), "never deletes an item")
  expect_error(nomo_factors(dat, n_iter = 10, guidance = guidance), "never deletes an item")
})


test_that("?nomo_defaults names every function that refuses the safeguards set to TRUE", {
  # The functions that take `guidance`, read from the namespace.
  ns <- asNamespace("nomologR")
  takes_guidance <- Filter(function(name) {
    f <- get(name, envir = ns)
    is.function(f) && "guidance" %in% names(formals(f))
  }, getNamespaceExports("nomologR"))
  expect_gte(length(takes_guidance), 15L)

  details <- nomo_test_rd_text("nomo_defaults", "\\details")
  paragraph <- sub("^.*\\\\code\\{auto_delete\\} and \\\\code\\{auto_respecify\\} are always", "",
                   details)
  expect_true(grepl("Every function that takes \\code{guidance} stops", paragraph, fixed = TRUE))
  named <- regmatches(paragraph, gregexpr("nomo_[a-z_]+(?=\\(\\))", paragraph, perl = TRUE))[[1L]]
  expect_setequal(unique(named), takes_guidance)

  # Each of them stops, before anything is fitted.
  skip_on_cran()
  guidance <- nomo_defaults()
  guidance$auto_delete <- TRUE
  dat <- nomo_demo_continuous
  items <- c(paste0("a", 1:5), paste0("b", 1:5))
  two <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  fit <- nomo_cfa(two, dat, modification_indices = FALSE)
  agency <- "Agency =~ ag1 + ag2 + ag3 + ag4"
  network <- paste(agency, "Persistence =~ pe1 + pe2 + pe3 + pe4", sep = "\n")
  calls <- list(
    nomo_screen = function() nomo_screen(dat[, items], guidance = guidance),
    nomo_factors = function() nomo_factors(dat[, items], n_iter = 10, guidance = guidance),
    nomo_efa = function() nomo_efa(dat[, items], factors = 2, guidance = guidance),
    nomo_cfa = function() nomo_cfa(two, dat, guidance = guidance),
    nomo_esem = function() nomo_esem(network, nomo_demo_network, guidance = guidance),
    nomo_compare = function() {
      nomo_compare(a = fit, b = fit, rationale = "Same model.", guidance = guidance)
    },
    nomo_split = function() nomo_split(dat, guidance = guidance),
    nomo_reliability = function() nomo_reliability(fit, guidance = guidance),
    nomo_validity = function() nomo_validity(fit, guidance = guidance),
    nomo_hierarchical = function() nomo_hierarchical(fit, guidance = guidance),
    nomo_scores = function() nomo_scores(fit, guidance = guidance),
    nomo_invariance = function() {
      nomo_invariance(agency, data = nomo_demo_network, group = "group", guidance = guidance)
    },
    nomo_invariance_longitudinal = function() {
      nomo_invariance_longitudinal("Wellbeing =~ w1 + w2 + w3 + w4",
                                   data = nomo_demo_longitudinal,
                                   occasions = c("t1", "t2", "t3"), guidance = guidance)
    },
    nomo_network = function() {
      nomo_network(network, data = nomo_demo_network,
                   hypotheses = nomo_hypotheses("Agency -> Persistence" = positive()),
                   guidance = guidance)
    },
    nomo_run = function() {
      nomo_run(dat[, items], scales = list(A = paste0("a", 1:5), B = paste0("b", 1:5)),
               guidance = guidance)
    }
  )
  expect_setequal(names(calls), takes_guidance)
  for (name in names(calls)) {
    expect_error(calls[[name]](), "`guidance$auto_delete` cannot be `TRUE`", fixed = TRUE,
                 label = name)
  }
})


test_that("?nomo_defaults says to pass a complete guidance list", {
  # The rule is written once, on this page; ?nomo_efa says what nomo_efa() adds.
  details <- nomo_test_rd_text("nomo_defaults", "\\details")
  expect_true(grepl("Pass a complete list as \\code{guidance}", details, fixed = TRUE))
  expect_true(grepl(
    "is completed from the defaults by \\code{\\link[=nomo_efa]{nomo_efa()}} only",
    details, fixed = TRUE
  ))
  expect_true(grepl("is completed from", nomo_test_rd_text("nomo_efa", "\\arguments"),
                    fixed = TRUE))

  # What the paragraph describes. A list with one setting is completed by
  # nomo_efa(), and gives what the complete list with that change gives.
  skip_on_cran()
  dat <- nomo_demo_continuous[, c(paste0("a", 1:5), paste0("b", 1:5))]
  one <- list(efa_loading_reference = 0.60)
  whole <- nomo_defaults()
  whole$efa_loading_reference <- 0.60
  completed <- nomo_efa(dat, factors = 2, guidance = one)
  expect_identical(completed$guidance, whole)
  expect_identical(completed$item_summary$attention,
                   nomo_efa(dat, factors = 2, guidance = whole)$item_summary$attention)

  # Elsewhere a missing setting can stop the call, which names it.
  expect_error(nomo_split(dat, guidance = one),
               "`guidance` must include `factor_small_n_reference`", fixed = TRUE)
  expect_error(nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5", dat, guidance = one),
               "`guidance` is missing required setting(s): cfa_loading_reference, fit_reference.",
               fixed = TRUE)
  # The complete list is accepted.
  expect_s3_class(nomo_split(dat, guidance = whole), "nomo_split")
})
