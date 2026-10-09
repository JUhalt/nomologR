# Number rules -----------------------------------------------------------------

test_that("the leading zero follows whether the statistic can exceed 1", {
  # APA 7: drop the leading zero only for statistics that cannot exceed 1.
  expect_identical(nomologR:::nomo_apa_number(0.456, 2, bounded = TRUE), ".46")
  expect_identical(nomologR:::nomo_apa_number(0.456, 2, bounded = FALSE), "0.46")
  expect_identical(nomologR:::nomo_apa_number(-0.456, 2, bounded = TRUE), "-.46")

  # A bounded statistic at exactly 1 has no leading zero to drop.
  expect_identical(nomologR:::nomo_apa_number(1, 3, bounded = TRUE), "1.000")

  # Unbounded statistics above 1 are untouched.
  expect_identical(nomologR:::nomo_apa_number(18.519, 2), "18.52")
})


test_that("a value that rounds to zero is never written with a sign", {
  expect_identical(nomologR:::nomo_apa_number(-0.0002, 3, bounded = TRUE), ".000")
  expect_identical(nomologR:::nomo_apa_number(-0.0002, 3, bounded = FALSE), "0.000")
})


test_that("missing values become an em dash, as APA marks empty cells", {
  expect_identical(nomologR:::nomo_apa_number(NA_real_), "\u2014")
  expect_identical(nomologR:::nomo_apa_number(Inf), "\u2014")
})


test_that("p values lose the leading zero and small ones become a bound", {
  expect_identical(nomologR:::nomo_apa_p(0.0421), ".042")
  expect_identical(nomologR:::nomo_apa_p(0.00042), "< .001")
  expect_identical(nomologR:::nomo_apa_p(0.001), ".001")
  expect_identical(nomologR:::nomo_apa_p(NA_real_), "\u2014")
})


test_that("intervals follow their estimate's rule and are omitted when absent", {
  expect_identical(
    nomologR:::nomo_apa_interval(0.46, 0.39, 0.53, bounded = TRUE),
    ".46 [.39, .53]"
  )
  expect_identical(
    nomologR:::nomo_apa_interval(0.46, 0.39, 0.53, bounded = FALSE),
    "0.46 [0.39, 0.53]"
  )
  expect_identical(nomologR:::nomo_apa_interval(0.46, NA, NA, bounded = TRUE), ".46")
})


# Tables from real objects -----------------------------------------------------

apa_model <- "Agency =~ ag1 + ag2 + ag3 + ag4\nPersistence =~ pe1 + pe2 + pe3 + pe4"

apa_cfa <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- nomo_cfa(apa_model, nomo_demo_network)
    cache
  }
})


test_that("the loadings table lays out one column per factor", {
  skip_on_cran()
  tab <- nomo_apa_table(apa_cfa(), "loadings", number = 1)

  expect_s3_class(tab, "nomo_apa_table")
  expect_identical(tab$number, 1L)
  expect_identical(names(tab$body), c("Item", "Agency", "Persistence"))
  expect_identical(tab$stub, "Item")

  # Standardized loadings keep the leading zero: in an improper solution they
  # can exceed 1, so under the rule as written they are not bounded.
  sl <- apa_cfa()$standardized_loadings
  ag1 <- sl$loading[sl$item == "ag1"]
  expect_identical(tab$body$Agency[[1L]], formatC(round(ag1, 2), format = "f", digits = 2))
  expect_match(tab$body$Agency[[1L]], "^0\\.")

  # Items that do not load on a factor are blank, and the note says why.
  expect_identical(tab$body$Persistence[tab$body$Item == "ag1"], "")
  expect_match(tab$notes$general, "fixed to zero", fixed = TRUE)
})


test_that("the fit table applies each index's bound", {
  tab <- nomo_apa_table(apa_cfa(), "fit")
  fe <- apa_cfa()$fit_evidence
  value <- function(m) fe$value[fe$metric == m][[1L]]

  expect_identical(tab$stub, "Model")
  expect_identical(tab$body$CFI, nomologR:::nomo_apa_number(value("CFI"), 3, bounded = TRUE))
  expect_identical(tab$body$TLI, nomologR:::nomo_apa_number(value("TLI"), 3, bounded = FALSE))
  expect_match(tab$body$SRMR, "^0\\.")

  # Reference-value language: no verdicts.
  text <- paste(unlist(tab$body), tab$notes$general, collapse = " ")
  expect_no_match(text, "PASS|FAIL|good fit|poor fit|acceptable", ignore.case = TRUE)
  expect_match(tab$notes$general, "not against fixed cutoffs", fixed = TRUE)
})


test_that("correlation and reliability tables drop the leading zero", {
  skip_on_cran()
  cors <- nomo_apa_table(apa_cfa(), "factor_correlations")
  expect_match(cors$body[[2L]][[1L]], "^\\.")

  rel <- nomo_apa_table(nomo_reliability(apa_cfa()))
  expect_identical(rel$body$Construct, c("Agency", "Persistence"))
  expect_true(all(grepl("^\\.", rel$body[[2L]])))
})


test_that("the invariance table reports changes without a signed zero", {
  skip_on_cran()
  inv <- nomo_invariance(apa_model, data = nomo_demo_network, group = "group")
  tab <- nomo_apa_table(inv)

  expect_identical(tab$body$Model[[1L]], "Configural")
  expect_false(any(grepl("^-\\.0+$|^-0\\.0+$", unlist(tab$body))))
  expect_match(tab$notes$general, "Grouping variable: group, with", fixed = TRUE)
})


test_that("the hypotheses table reports evidence, not whether it was prespecified", {
  skip_on_cran()
  # A path and a correlation on different pairs, so both are estimable.
  h <- nomo_hypotheses(
    "Agency -> Persistence" = positive(min = .20),
    "Agency <-> SD" = negligible(within = c(-.10, .10))
  )
  net <- nomo_network(
    paste(apa_model, "SD =~ sd1 + sd2 + sd3", sep = "\n"),
    data = nomo_demo_network, hypotheses = h
  )
  tab <- nomo_apa_table(net)
  he <- net$hypothesis_evidence

  # Evidence comes from concordance; confirmatory_status only records whether
  # the hypothesis was specified in advance.
  expect_identical(tab$body$Evidence, nomologR:::nomo_apa_concordance(he$concordance))
  expect_false(any(tab$body$Evidence %in% c("A priori", "a priori")))

  # A directed path can exceed 1 and keeps its zero; a correlation cannot.
  directed <- he$relation_type == "directed"
  expect_match(tab$body[[3L]][directed], "^0\\.")
  expect_match(tab$body[[3L]][!directed], "^\\.")
})


test_that("a relation the model cannot estimate is shown as an empty cell", {
  skip_on_cran()
  # A hypothesized path the model lacks, with additions turned off, has no
  # estimate; APA marks that with a dash rather than a zero or a blank that
  # could be misread as a value. (A path and a correlation between the same
  # two factors, the earlier way to get one, is now refused, #145.)
  h <- nomo_hypotheses(
    "Agency -> Performance" = positive(),
    "Agency <-> Persistence" = positive()
  )
  net <- nomo_network(
    apa_model, data = nomo_demo_network, hypotheses = h, add_missing = FALSE
  )
  tab <- nomo_apa_table(net)

  missing <- !is.finite(net$hypothesis_evidence$estimate)
  expect_true(any(missing))
  expect_true(all(tab$body[[3L]][missing] == "\u2014"))
})


test_that("a post hoc hypothesis is marked with a specific note", {
  skip_on_cran()
  h <- nomo_hypotheses("Agency -> Persistence" = positive(min = .20))
  net <- nomo_network(apa_model, data = nomo_demo_network, hypotheses = h)
  net$hypothesis_evidence$confirmatory_status <- "post_hoc"

  tab <- nomo_apa_table(net)
  expect_match(tab$body$Hypothesis, "^a^", fixed = TRUE)
  expect_match(tab$notes$specific, "exploratory", fixed = TRUE)
  expect_match(nomologR:::nomo_apa_notes_text(tab$notes)[[2L]], "^a^", fixed = TRUE)
})


# Rendering --------------------------------------------------------------------

test_that("knitted tables carry the APA number, title, alignment and notes", {
  md <- nomologR:::nomo_apa_markdown(nomo_apa_table(apa_cfa(), "loadings", number = 3))

  expect_identical(md[[1L]], "**Table 3**")
  expect_identical(md[[3L]], "*Standardized Factor Loadings*")
  # Stub column left-aligned, every other column centered.
  expect_match(md[[6L]], "^\\| :-{3,} \\| :-{3,}: \\| :-{3,}: \\|$")
  expect_match(md[[length(md)]], "^\\*Note\\.\\*")
})


test_that("the loadings table is stable", {
  # ASCII only, so the snapshot does not depend on the platform's encoding.
  expect_snapshot(print(nomo_apa_table(apa_cfa(), "loadings", number = 1)))
})


test_that("unsupported objects and arguments are refused with an explanation", {
  expect_error(nomo_apa_table(list()), "No APA table is available")
  expect_error(nomo_apa_table(list()), "nomo_validity", fixed = TRUE)
  expect_error(nomo_apa_table(apa_cfa(), "loadings", number = 0), "whole number")
  expect_error(nomo_apa_table(apa_cfa(), "nonsense"), "`type` must be one of", fixed = TRUE)
})


test_that("a one-table result refuses a `type`, and `title` must be one string (#145)", {
  # The check comes first, so a bare object of each class is enough.
  for (cls in c("nomo_reliability", "nomo_invariance", "nomo_retest")) {
    expect_error(
      nomo_apa_table(structure(list(), class = cls), "nonsense"),
      sprintf('`type` must be NULL for a `%s` result, which has one APA table, not "nonsense".', cls),
      fixed = TRUE
    )
  }

  for (bad in list(c("A", "B"), NA_character_, 1, character())) {
    expect_error(nomo_apa_table(apa_cfa(), "fit", title = bad),
                 "`title` must be NULL or one string.", fixed = TRUE)
  }
  expect_identical(nomo_apa_table(apa_cfa(), "fit", title = "Fit")$title, "Fit")
})


test_that("?nomo_apa_table lists the fields of the table it returns (#145)", {
  # The structure is covered by the stability policy, so each field is named.
  value <- nomo_test_rd_text("nomo_apa_table", "\\value")
  tab <- nomo_apa_table(apa_cfa(), "fit", number = 2)

  for (field in c(names(tab), names(tab$notes))) {
    expect_true(grepl(sprintf("\\code{%s}", field), value, fixed = TRUE), label = field)
  }
  expect_true(grepl("Markdown", value, fixed = TRUE))
})


# Remaining paths (#72) --------------------------------------------------------

test_that("a one-factor model has no factor-correlation table, and says why", {
  skip_on_cran()
  one <- nomo_cfa("Agency =~ ag1 + ag2 + ag3 + ag4", nomo_demo_network)
  expect_error(nomo_apa_table(one, "factor_correlations"), "one factor")
})


test_that("a reliability coefficient that was not computed is an empty cell", {
  skip_on_cran()
  rel <- nomo_reliability(apa_cfa())
  # Alpha for one construct only: its other cell is a dash, which the note
  # explains (#145).
  rel$alpha <- rel$alpha[rel$alpha$construct == "Agency", , drop = FALSE]
  tab <- nomo_apa_table(rel)
  # Columns: construct, omega, alpha. Omega was computed; alpha only once.
  expect_false(any(tab$body[[2L]] == nomologR:::nomo_apa_dash))
  expect_identical(tab$body[[3L]][[2L]], nomologR:::nomo_apa_dash)
  expect_match(tab$notes$general, "\u2014 = not computed.", fixed = TRUE)
})


test_that("alpha that was never computed is left out, with no note about it (#145)", {
  skip_on_cran()
  tab <- nomo_apa_table(nomo_reliability(apa_cfa(), include_alpha = FALSE))
  expect_identical(names(tab$body), c("Construct", "\u03c9"))
  expect_false(grepl("alpha", tab$notes$general, ignore.case = TRUE))
  expect_false(grepl("\u2014", tab$notes$general, fixed = TRUE))
})


test_that("the network fit table reports the RMSEA with its interval", {
  skip_on_cran()
  net <- nomo_network(
    paste(apa_model, "Persistence ~ Agency", sep = "\n"),
    data = nomo_demo_network,
    hypotheses = nomo_hypotheses("Agency -> Persistence" = positive())
  )
  tab <- nomo_apa_table(net, "fit", number = 2)
  body <- tab$body

  expect_identical(body$Model, "Nomological network")
  measures <- lavaan::fitMeasures(net$fit)
  expect_identical(body[["RMSEA [90% CI]"]], nomologR:::nomo_apa_interval(
    net$fit_evidence$rmsea, measures[["rmsea.ci.lower"]], measures[["rmsea.ci.upper"]],
    digits = 3L, bounded = FALSE
  ))
  expect_match(body[["RMSEA [90% CI]"]], "[", fixed = TRUE)
  expect_match(paste(tab$notes$general, collapse = " "), "N* = 800", fixed = TRUE)
})


test_that("probability notes follow the general and specific notes", {
  tab <- nomologR:::nomo_apa_new(
    body = data.frame(Stub = "a", Value = "1", stringsAsFactors = FALSE),
    title = "A Table", stub = "Stub",
    general = "General note.", specific = "Specific note.",
    probability = "*p* < .05."
  )
  notes <- nomologR:::nomo_apa_notes_text(tab$notes)
  expect_identical(length(notes), 3L)
  expect_identical(notes[[3L]], "*p* < .05.")
})


test_that("knitting a table emits its markdown", {
  skip_if_not_installed("knitr")
  tab <- nomo_apa_table(apa_cfa(), "loadings", number = 1)
  out <- knitr::knit_print(tab)
  expect_s3_class(out, "knit_asis")
  expect_identical(
    as.character(out),
    paste(nomologR:::nomo_apa_markdown(tab), collapse = "\n")
  )
})


# Convergent and discriminant evidence ------------------------------------------

apa_validity <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      scales <- list(Agency = paste0("ag", 1:4), Persistence = paste0("pe", 1:4),
                     SocialDesirability = paste0("sd", 1:3))
      cache <<- nomo_validity(nomo_cfa(nomo_model(scales), nomo_demo_network))
    }
    cache
  }
})


test_that("the discriminant table gives each pair its correlation and ratios", {
  skip_on_cran()
  v <- apa_validity()
  tab <- nomo_apa_table(v)
  expect_identical(tab$source, "nomo_validity")
  expect_identical(tab$title, "Construct Correlations and Heterotrait-Monotrait Ratios")
  expect_identical(names(tab$body), c("Constructs", "*r* [95% CI]", "HTMT2", "HTMT"))
  expect_identical(tab$body$Constructs[[1L]], "Agency with Persistence")

  lc <- v$latent_correlations[v$latent_correlations$construct_1 == "Agency" &
                                v$latent_correlations$construct_2 == "Persistence", ]
  # A correlation cannot exceed 1 and loses its leading zero; a ratio can, and
  # keeps it.
  expect_identical(
    tab$body[["*r* [95% CI]"]][[1L]],
    nomologR:::nomo_apa_interval(lc$correlation, lc$ci_lower, lc$ci_upper, bounded = TRUE)
  )
  expect_true(startsWith(tab$body[["*r* [95% CI]"]][[1L]], "."))
  expect_true(startsWith(tab$body$HTMT2[[1L]], "0."))

  notes <- paste(tab$notes$general, collapse = " ")
  expect_match(notes, "Cho, 2022", fixed = TRUE)
  expect_match(notes, "Roemer et al., 2021", fixed = TRUE)
  expect_match(notes, "The review reference for the ratios is 0.85.", fixed = TRUE)
  expect_false(grepl("Fornell", notes, fixed = TRUE))
})


test_that("the discriminant table leaves out ratios that were not computed", {
  skip_on_cran()
  none <- nomo_validity(apa_validity()$fit, htmt = "none")
  tab <- nomo_apa_table(none, "discriminant")
  expect_identical(names(tab$body), c("Constructs", "*r* [95% CI]"))
  expect_match(paste(tab$notes$general, collapse = " "),
               "Heterotrait-monotrait ratios were not computed", fixed = TRUE)

  only2 <- nomo_validity(apa_validity()$fit, htmt = "htmt2")
  expect_identical(names(nomo_apa_table(only2)$body),
                   c("Constructs", "*r* [95% CI]", "HTMT2"))
})


test_that("the convergent table counts indicators and writes AVE without a zero", {
  skip_on_cran()
  tab <- nomo_apa_table(apa_validity(), "convergent", number = 5)
  expect_identical(tab$number, 5L)
  expect_identical(tab$title, "Average Variance Extracted")
  expect_identical(names(tab$body), c("Construct", "*k*", "AVE"))
  expect_identical(tab$body$Construct, c("Agency", "Persistence", "SocialDesirability"))
  expect_identical(tab$body[["*k*"]], c("4", "4", "3"))
  expect_true(all(startsWith(tab$body$AVE, ".")))
  expect_match(paste(tab$notes$general, collapse = " "),
               "The review reference is .50;", fixed = TRUE)

  no_ave <- apa_validity()
  no_ave$ave$estimate <- NA_real_
  expect_error(nomo_apa_table(no_ave, "convergent"), "No average variance extracted")
})


test_that("one construct has a convergent table and no pair table", {
  skip_on_cran()
  one <- nomo_validity(nomo_cfa("Agency =~ ag1 + ag2 + ag3 + ag4", nomo_demo_network))
  expect_error(nomo_apa_table(one), "one construct, so there are no construct pairs")
  expect_identical(nomo_apa_table(one, "convergent")$body$Construct, "Agency")
})


test_that("a multi-group model's tables name the groups the same way", {
  skip_on_cran()
  fit <- lavaan::cfa("visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6",
                     data = lavaan::HolzingerSwineford1939, group = "school")
  v <- nomo_validity(fit)
  pairs <- nomo_apa_table(v)
  convergent <- nomo_apa_table(v, "convergent")
  expect_identical(names(pairs$body), c("Constructs", "Group", "*r* [95% CI]"))
  expect_setequal(pairs$body$Group, c("Pasteur", "Grant-White"))
  expect_setequal(convergent$body$Group, pairs$body$Group)
  # Each loading is listed once per group; the indicators are counted once.
  expect_identical(unique(convergent$body[["*k*"]]), "3")
})


test_that("the validity tables are stable", {
  skip_on_cran()
  # The pair table's note cites Ronkko with its diacritics, so only its body is
  # snapshotted; the convergent table is ASCII throughout.
  expect_snapshot(print(nomo_apa_table(apa_validity(), number = 4)$body))
  expect_snapshot(print(nomo_apa_table(apa_validity(), "convergent", number = 5)))
})


# Pre-RC audit (#145) -------------------------------------------------------------

test_that("a reliability interval's heading carries the level it was computed at (#145)", {
  skip_on_cran()
  # A bootstrap at 90%, set on the object, so the test needs no resampling.
  rel <- nomo_reliability(apa_cfa())
  for (tbl in c("omega", "alpha")) {
    rel[[tbl]]$ci_lower <- rel[[tbl]]$estimate - .05
    rel[[tbl]]$ci_upper <- rel[[tbl]]$estimate + .05
  }
  rel$ci_status <- tibble::tibble(method = "bootstrap", level = .9, requested_draws = 40L,
                                  min_successful_draws = 40L, available = TRUE,
                                  seed = 1L, workers = 1L, reason = "")
  tab <- nomo_apa_table(rel)
  expect_identical(names(tab$body), c("Construct", "\u03c9 [90% CI]", "\u03b1 [90% CI]"))
  expect_match(tab$notes$general, "CI = confidence interval", fixed = TRUE)
  expect_match(tab$notes$general,
               "90% CI = percentile bootstrap confidence interval, from 40 draws.", fixed = TRUE)
  # Without a recorded status, the default level and no count of draws.
  rel$ci_status <- NULL
  tab <- nomo_apa_table(rel)
  expect_identical(names(tab$body)[[2L]], "\u03c9 [95% CI]")
  expect_match(tab$notes$general, "95% CI = percentile bootstrap confidence interval.",
               fixed = TRUE)
})


test_that("omega for ordered indicators is named for its scale, and alpha's absence explained (#145)", {
  skip_on_cran()
  two <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  ordinal <- nomo_reliability(nomo_cfa(two, nomo_demo_ordinal, ordered = names(nomo_demo_ordinal)))
  tab <- nomo_apa_table(ordinal)
  expect_identical(names(tab$body), c("Construct", "\u03c9"))
  expect_match(tab$notes$general,
               "categorical omega, for the sum of the observed ordinal item scores (Green & Yang, 2009)",
               fixed = TRUE)
  expect_match(tab$notes$general, "observed-score alpha is not computed for ordered indicators",
               fixed = TRUE)
  expect_false(grepl("reported alongside omega", tab$notes$general, fixed = TRUE))

  # On the latent-response scale it is named so, and a construct of another
  # kind beside it is marked with a specific note instead.
  latent <- ordinal
  latent$ordinal_scale <- FALSE
  expect_match(nomo_apa_table(latent)$notes$general, "continuous latent responses", fixed = TRUE)
  mixed <- ordinal
  mixed$alpha_status$indicator_type[[2L]] <- "continuous"
  tab <- nomo_apa_table(mixed)
  expect_identical(
    tab$body[[2L]][[1L]],
    paste0(nomologR:::nomo_apa_number(ordinal$omega$estimate[[1L]], 2, TRUE), "^a^")
  )
  expect_match(tab$notes$specific, "^Categorical omega")
  expect_match(tab$notes$general, "\u03c9 = coefficient omega.", fixed = TRUE)
  # An omega that was not computed is a dash without a marker, and explained.
  gone <- mixed
  gone$omega$estimate[[1L]] <- NA_real_
  tab <- nomo_apa_table(gone)
  expect_identical(tab$body[[2L]][[1L]], nomologR:::nomo_apa_dash)
  expect_identical(tab$notes$specific, character())
  expect_match(tab$notes$general, paste(nomologR:::nomo_apa_dash, "= not computed."),
               fixed = TRUE)
  # Alpha requested for continuous indicators but not available is said so.
  mixed$alpha_status$indicator_type <- "continuous"
  expect_match(nomo_apa_table(mixed)$notes$general,
               "Coefficient alpha could not be computed for these constructs.", fixed = TRUE)
})


test_that("a partial invariance model is labeled and its releases named (#145)", {
  skip_on_cran()
  inv <- nomo_invariance(
    "Agency =~ ag1 + ag2 + ag3 + ag4", data = nomo_demo_network, group = "group",
    levels = c("configural", "metric", "scalar"), estimator = "MLR",
    partial = nomo_partial(level = "scalar", syntax = "ag3 ~ 1", rationale = "Prespecified.")
  )
  tab <- nomo_apa_table(inv)
  expect_identical(tab$body$Model, c("Configural", "Metric", "Partial scalar^a^"))
  expect_identical(tab$notes$specific,
                   "Partial invariance: the intercept of ag3 was freed across groups.")
  # The first model has nothing above it: its change cells are blank, not dashes.
  expect_identical(unname(unlist(tab$body[1L, 7:10])), rep("", 4L))
  expect_false(grepl("\u2014", tab$notes$general, fixed = TRUE))
  # The sample, the estimator, and the versions of the fit statistics.
  note <- tab$notes$general
  expect_match(note, "Grouping variable: group, with *n* = 400 (online) and *n* = 400 (paper).",
               fixed = TRUE)
  expect_match(note, "scaled test statistic (MLR); *N* = 800.", fixed = TRUE)
  expect_match(note, "the \u0394\u03c7\u00b2 values are scaled difference tests.", fixed = TRUE)
  expect_match(note, "CFI and RMSEA are robust values.", fixed = TRUE)
  expect_match(note, "\u0394 = change from the model above.", fixed = TRUE)

  # A dash marks a value that was not computed, and the note says so.
  failed <- inv
  failed$fit_evidence$lrt_chisq[[3L]] <- NA_real_
  failed$group_n <- NULL
  tab <- nomo_apa_table(failed)
  expect_identical(tab$body[[9L]][[3L]], "\u2014")
  expect_match(tab$notes$general, "\u2014 = not computed.", fixed = TRUE)
  expect_match(tab$notes$general, "Grouping variable: group.", fixed = TRUE)

  # A result saved before the releases, versions, and cases were recorded.
  old <- inv
  old$fit_evidence$partial_requested <- NULL
  old$fit_variants <- NULL
  old$n_used <- NULL
  tab <- nomo_apa_table(old)
  expect_identical(tab$body$Model[[3L]], "Scalar")
  expect_false(grepl("robust values", tab$notes$general, fixed = TRUE))
  expect_false(grepl("difference tests", tab$notes$general, fixed = TRUE))
  expect_false(grepl("*N* =", tab$notes$general, fixed = TRUE))

  # Across occasions, the title and the note name the occasions.
  occasions <- inv
  occasions$design <- "occasions"
  occasions$occasions <- c("t1", "t2")
  tab <- nomo_apa_table(occasions)
  expect_identical(tab$title, "Measurement Invariance Across Occasions")
  expect_match(tab$notes$general, "Occasions: t1, t2.", fixed = TRUE)
  expect_match(tab$notes$specific, "freed across occasions.", fixed = TRUE)
})


test_that("releases read as parameters in words", {
  words <- nomologR:::nomo_apa_release_words(
    c("ag3 ~ 1", "F =~ x2", "x3 ~~ x3", "F ~~ F", "x1 ~~ x2", "G ~~ H", "u1 | t2", "a == b"),
    observed = c("x1", "x2", "x3", "u1")
  )
  expect_identical(words, c(
    "the intercept of ag3", "the loading of x2 on F", "the residual variance of x3",
    "the variance of F", "the residual covariance of x1 and x2", "the covariance of G and H",
    "threshold t2 of u1", "a == b"
  ))
})


test_that("fit tables name the estimator, the cases analyzed, and scaled or robust values (#145)", {
  skip_on_cran()
  two <- "A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + b5"
  mlr <- nomo_apa_table(nomo_cfa(two, nomo_demo_continuous, estimator = "MLR"), "fit")
  expect_match(mlr$notes$general, paste(
    "Estimated with maximum likelihood with robust standard errors and a scaled",
    "test statistic (MLR); *N* = 473."
  ), fixed = TRUE)
  expect_match(mlr$notes$general,
               "The \u03c7\u00b2 value is the Yuan-Bentler scaled test statistic.",
               fixed = TRUE)
  expect_match(mlr$notes$general, "CFI, TLI, and RMSEA are robust values.", fixed = TRUE)
  expect_match(mlr$notes$general, "CI = confidence interval", fixed = TRUE)
  # A result saved before the versions were recorded has no sentence about them.
  old <- nomo_cfa(two, nomo_demo_continuous, estimator = "MLR")
  old$fit_evidence$variant <- NULL
  expect_false(grepl("robust values", nomo_apa_table(old, "fit")$notes$general, fixed = TRUE))

  # The network reports the cases its fit analyzed after listwise deletion,
  # and its estimator, not "the network model".
  net <- nomo_network(two, nomo_demo_continuous, nomo_hypotheses("A -> B" = positive()))
  fit <- nomo_apa_table(net, "fit")
  expect_match(fit$notes$general, "Estimated with maximum likelihood (ML); *N* = 473.",
               fixed = TRUE)
  expect_false(grepl("network model", fit$notes$general, fixed = TRUE))
  expect_match(nomo_apa_table(net)$notes$general, "*N* = 473.", fixed = TRUE)
  # Versions and the validation sample, as a robust network with validation
  # data records them; an object without the cases used reports the rows.
  net$fit_evidence$chisq_version <- "scaled"
  net$fit_evidence$index_version <- "mixed"
  net$validation_n_used <- 320
  net$n_used <- NA_real_
  net$estimator <- NA_character_
  note <- nomo_apa_table(net, "fit")$notes$general
  expect_match(note, "The \u03c7\u00b2 value is the scaled test statistic.", fixed = TRUE)
  expect_match(note, "CFI, TLI, and RMSEA are robust or scaled values.", fixed = TRUE)
  expect_match(note, "the validation sample (*N* = 320) is reported separately.", fixed = TRUE)
  expect_match(note, "(ML); *N* = 500.", fixed = TRUE)

  # Without an estimator or a sample size the sentence is left out, and an
  # estimator without a name in the glossary is given as it is.
  expect_identical(nomologR:::nomo_apa_sample_note(NA, NA), "")
  expect_identical(nomologR:::nomo_apa_sample_note("PML", 10), "Estimated with PML; *N* = 10.")
  expect_identical(nomologR:::nomo_apa_sample_note(character(), numeric()), "")
  expect_identical(nomologR:::nomo_apa_fit_estimator(NULL), NA_character_)
  expect_identical(nomologR:::nomo_apa_versions_note("scaled", c(CFI = "robust")),
                   "The \u03c7\u00b2 value is the scaled test statistic. CFI is a robust value.")
})


test_that("correlations a model fixes are not tabled as estimates (#145)", {
  skip_on_cran()
  hs <- lavaan::HolzingerSwineford1939
  factors <- list(visual = c("x1", "x2", "x3"), textual = c("x4", "x5", "x6"),
                  speed = c("x7", "x8", "x9"))
  bifactor <- nomo_cfa(nomo_model(factors, structure = "bifactor"), hs,
                       modification_indices = FALSE)
  expect_error(nomo_apa_table(bifactor, "factor_correlations"),
               "No factor correlation is estimated in this model", fixed = TRUE)
  higher <- nomo_cfa(nomo_model(factors, structure = "higher_order"), hs,
                     modification_indices = FALSE)
  expect_error(nomo_apa_table(higher, "factor_correlations"),
               "relates its factors through a higher-order factor", fixed = TRUE)

  orth <- nomo_cfa(
    "visual =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed =~ x7 + x8 + x9\nvisual ~~ 0*textual",
    hs, modification_indices = FALSE
  )
  tab <- nomo_apa_table(orth, "factor_correlations")
  expect_identical(tab$body$Factors, c("visual with speed", "textual with speed"))
  expect_match(tab$notes$general,
               "Correlations the model fixes are not shown: visual with textual.", fixed = TRUE)
  # Every abbreviation in the headings is defined, r among them.
  expect_match(tab$notes$general,
               "*r* = latent correlation from the confirmatory factor analysis; CI = confidence interval.",
               fixed = TRUE)
})


apa_net <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      three <- paste(apa_model, "SocialDesirability =~ sd1 + sd2 + sd3", sep = "\n")
      cache <<- nomo_network(three, data = nomo_demo_network, hypotheses = nomo_hypotheses(
        "Agency -> Persistence" = positive(min = .20),
        "Agency <-> SocialDesirability" = negligible(within = c(-.15, .088)),
        "Agency -> Performance" = positive(scale = "unstandardized")
      ))
    }
    cache
  }
})


test_that("the hypotheses table shows the interval each concordance was judged on (#145)", {
  skip_on_cran()
  net <- apa_net()
  he <- net$hypothesis_evidence
  tab <- nomo_apa_table(net)
  # The region beside the prediction; a direction alone needs none.
  expect_identical(tab$body$Prediction,
                   c("Positive, \u2265 0.20", "Negligible, [-.15, .088]", "Positive"))
  # The negligible prediction shows its 90% equivalence interval, marked, and
  # the unstandardized estimate is marked too.
  h2 <- he[he$id == "H2", ]
  expect_identical(
    tab$body[["Estimate [95% CI]"]][[2L]],
    sprintf("%s [%s, %s]^a^", nomologR:::nomo_apa_number(h2$estimate, 2, TRUE),
            nomologR:::nomo_apa_number(h2$equivalence_ci_lower, 2, TRUE),
            nomologR:::nomo_apa_number(h2$equivalence_ci_upper, 2, TRUE))
  )
  expect_match(tab$body[["Estimate [95% CI]"]][[3L]], "^b^", fixed = TRUE)
  expect_identical(tab$notes$specific, c(
    paste("The interval is the 90% equivalence interval (two one-sided tests at \u03b1 = .05),",
          "on which concordance with a negligible prediction is judged."),
    "Unstandardized estimate."
  ))
  expect_match(tab$notes$general, "Estimates are standardized except where marked.", fixed = TRUE)
  expect_match(tab$notes$general, "CI = confidence interval.", fixed = TRUE)

  # A cell is marked for what it shows: an estimate without its equivalence
  # interval, or no estimate at all, carries no marker, and the dash is
  # explained.
  bare <- net
  bare$hypothesis_evidence$equivalence_ci_lower[[2L]] <- NA_real_
  bare$hypothesis_evidence$estimate[[3L]] <- NA_real_
  tab <- nomo_apa_table(bare)
  expect_identical(tab$body[[3L]][[2L]], nomologR:::nomo_apa_number(h2$estimate, 2, TRUE))
  expect_identical(tab$body[[3L]][[3L]], nomologR:::nomo_apa_dash)
  expect_identical(tab$notes$specific, character())
  expect_match(tab$notes$general, paste(nomologR:::nomo_apa_dash, "= not estimated."),
               fixed = TRUE)

  # Every prediction negligible: the heading itself names the interval.
  only <- net
  only$hypothesis_evidence <- he[he$id == "H2", ]
  tab <- nomo_apa_table(only)
  expect_identical(names(tab$body)[[3L]], "Estimate [90% CI]")
  expect_identical(tab$notes$specific, character())
  expect_match(tab$notes$general, "CI = confidence interval: the equivalence interval", fixed = TRUE)
  expect_match(tab$notes$general, "Estimates are standardized.", fixed = TRUE)
})


test_that("evidence labels are the console's, and a bare negligible prediction is explained (#145)", {
  expect_identical(
    nomologR:::nomo_apa_concordance(c("concordant", "directionally_concordant_imprecise",
                                      "not_confirmable_without_sesoi", "something_new", NA)),
    c("Concordant", "In region, imprecise", "Not confirmable", "Something new", "\u2014")
  )
  # Every stored value has its words, so no label is left undefined.
  expect_true(all(c(
    "concordant", "directionally_concordant_imprecise", "direction_concordant_below_magnitude",
    "direction_concordant_above_magnitude", "inconclusive", "inconsistent", "not_evaluable",
    "not_confirmable_without_sesoi"
  ) %in% names(nomologR:::nomo_apa_evidence_words)))
  skip_on_cran()
  net <- apa_net()
  # The note defines the labels the table shows, in the order shown, and no other.
  note <- nomo_apa_table(net)$notes$general
  expect_match(note, "not a verdict on the measure. Concordant = the interval lies inside the predicted region.",
               fixed = TRUE)
  expect_false(grepl("Not confirmable =", note, fixed = TRUE))
  net$hypothesis_evidence$concordance[2:3] <- c("not_confirmable_without_sesoi", "something_new")
  note <- nomo_apa_table(net)$notes$general
  expect_match(note, paste(
    "Concordant = the interval lies inside the predicted region; Not confirmable = a",
    "negligible prediction without a smallest effect size of interest, which a",
    "nonsignificant estimate cannot confirm."
  ), fixed = TRUE)
  expect_false(grepl("Something new =", note, fixed = TRUE))
  # A table whose labels are all unknown has no definitions to give.
  net$hypothesis_evidence$concordance <- "something_new"
  expect_false(grepl(" = the ", nomo_apa_table(net)$notes$general, fixed = TRUE))
})


test_that("the console fits an APA table to its width and names what it leaves out (#145)", {
  skip_on_cran()
  net <- apa_net()
  net$hypothesis_evidence$confirmatory_status[[1L]] <- "post_hoc"
  net$hypothesis_evidence$concordance[[2L]] <- "not_confirmable_without_sesoi"
  rt <- nomo_retest(data.frame(a = c(1, 2, 3, 4, 5, 6), b = c(1.2, 2.1, 2.8, 4.3, 5.1, 5.7)),
                    scores = list(Agency = c("a", "b")))
  tables <- list(nomo_apa_table(net, number = 1), nomo_apa_table(rt),
                 nomo_apa_table(apa_cfa(), "fit"))
  for (width in c(80L, 40L)) {
    local_reproducible_output(width = width)
    for (tab in tables) {
      out <- capture.output(print(tab))
      expect_true(all(nchar(out, type = "width") <= width), label = paste(tab$title, width))
      # No pandoc markup reaches the console.
      expect_false(any(grepl("^", out, fixed = TRUE) | grepl("*", out, fixed = TRUE)))
    }
  }

  local_reproducible_output(width = 80)
  out <- capture.output(print(tables[[1L]]))
  expect_identical(out[[1L]], "<nomo_apa_table> Manuscript table in APA style")
  expect_identical(out[[3L]], "Table 1")
  # The marker reads "(a)" in the cell, kept with the word before it, and
  # before its note.
  expect_true(any(grepl("Persistence (a)", out, fixed = TRUE)))
  expect_true(any(startsWith(out, "(a) Specified after the data were seen")))
  expect_match(paste(utils::tail(out, 2L), collapse = " "),
               "knitr::knit_print(x) for the Markdown a knitted document renders.",
               fixed = TRUE)
  # The retest table's long headings wrap, so it fits 80 columns.
  retest <- capture.output(print(tables[[2L]]))
  expect_true(any(grepl("^Composite +n +\\[95% CI\\] +\\[95% CI\\] +\\[95% CI\\] +SEM +SDC$",
                        retest)))

  # A number keeps its column whether or not a note marker follows it: the
  # closing bracket of every interval sits in one column.
  rows <- out[grepl("^H[0-9]:", out)]
  expect_identical(length(rows), 3L)
  expect_identical(length(unique(vapply(gregexpr("]", rows, fixed = TRUE), max, numeric(1)))), 1L)
  expect_true(any(grepl("] (b)", rows, fixed = TRUE)))
  expect_true(any(grepl("] (c)", rows, fixed = TRUE)))
  # Each specific note starts its own line, so no marker is left at a line's end.
  expect_true(any(startsWith(out, "(b) The interval is the 90% equivalence interval")))
  expect_true(any(startsWith(out, "(c) Unstandardized estimate.")))

  # At 40 columns the fit table keeps its stub and the columns that fit, and
  # names the rest.
  local_reproducible_output(width = 40)
  fit <- capture.output(print(tables[[3L]]))
  expect_true(any(grepl("Not shown for width:", fit, fixed = TRUE)))
  expect_true(any(grepl("See x$body.", fit, fixed = TRUE)))
  expect_true(any(grepl("^Model ", fit) & grepl(" p ", fit, fixed = TRUE)))
  expect_false(any(grepl("SRMR", fit[grepl("^Model ", fit)], fixed = TRUE)))
  # The hypotheses table keeps its Evidence, the status, whatever it drops.
  narrow <- capture.output(print(tables[[1L]]))
  expect_true(any(grepl("^Hypothesis +Evidence$", narrow)))
  expect_true(any(grepl("Not shown for width: Prediction,", narrow, fixed = TRUE)))
})


test_that("a console too narrow drops columns from the right, a p value among them (#145)", {
  # A p value belongs to the estimate or test beside it, so it is not kept
  # when that column goes: the correlation stays, and its p value is named.
  tab <- nomologR:::nomo_apa_new(
    body = data.frame(
      Factors = c("Agency with Persistence", "Persistence with SocialDesirability"),
      r = c(".46 [.39, .53]", ".00 [-.09, .09]^a^"), p = c("< .001", ".963"),
      stringsAsFactors = FALSE
    ),
    title = "Factor Correlations", stub = "Factors",
    specific = c("First.", "Second.")
  )
  names(tab$body) <- c("Factors", "*r* [95% CI]", "*p*")
  local_reproducible_output(width = 40)
  out <- capture.output(print(tab))
  expect_true(all(nchar(out, type = "width") <= 40L))
  expect_true(any(grepl("^Factors +r \\[95% CI\\]$", out)))
  expect_true(any(grepl("Not shown for width: p. See x$body.", out, fixed = TRUE)))
  expect_true(any(grepl(".00 [-.09, .09] (a)", out, fixed = TRUE)))
  # In the console each specific note has its line; knitted, they run on in
  # one paragraph, as APA sets them.
  expect_true(all(c("(a) First.", "(b) Second.") %in% out))
  md <- nomologR:::nomo_apa_markdown(tab, latex = FALSE)
  expect_identical(md[[length(md)]], "^a^ First. ^b^ Second.")
})


test_that("a multi-group reliability table has a row for each construct and group (#145)", {
  skip_on_cran()
  # Two groups, set on the object, so the test needs no multi-group fit.
  rel <- nomo_reliability(apa_cfa())
  by_group <- function(tbl) {
    paper <- tbl
    paper$estimate <- tbl$estimate - .02
    tbl$block <- "online"
    paper$block <- "paper"
    rbind(tbl, paper)
  }
  rel$omega <- by_group(rel$omega)
  rel$alpha <- by_group(rel$alpha)
  tab <- nomo_apa_table(rel)
  expect_identical(tab$body$Construct, c("Agency (online)", "Persistence (online)",
                                         "Agency (paper)", "Persistence (paper)"))
  # No group's coefficient is dropped: each row has its own.
  expect_identical(
    tab$body[[2L]],
    nomologR:::nomo_apa_number(rel$omega$estimate, 2L, bounded = TRUE)
  )
})


test_that("the hypotheses note names composites modeled as single indicators", {
  skip_on_cran()
  net <- apa_net()
  net$single_indicators <- data.frame(
    variable = c("persistence", "sd_score"), reliability = c(.8, .62),
    coefficient = c("omega", "unspecified"), stringsAsFactors = FALSE
  )
  note <- nomo_apa_table(net)$notes$general
  expect_match(note, "persistence and sd_score were modeled as single-indicator latent variables",
               fixed = TRUE)
  expect_match(note, "(reliability: persistence = .80, omega; sd_score = .62).", fixed = TRUE)
  net$single_indicators <- net$single_indicators[1L, ]
  expect_match(nomo_apa_table(net)$notes$general,
               "persistence was modeled as a single-indicator latent variable, with its error variance",
               fixed = TRUE)
})


test_that("fractional degrees of freedom are not rounded to a whole number (#145)", {
  expect_identical(nomologR:::nomo_apa_df(c(34, 33.468, NA)),
                   c("34", "33.47", nomologR:::nomo_apa_dash))
  # An adjusted test, such as the mean- and variance-adjusted one, has them.
  cfa <- apa_cfa()
  cfa$fit_evidence$value[cfa$fit_evidence$metric == "df"] <- 18.42
  expect_identical(nomo_apa_table(cfa, "fit")$body[["*df*"]], "18.42")
})


test_that("knitted notes are separate paragraphs, and an unnumbered table has no number line (#145)", {
  tab <- nomologR:::nomo_apa_new(
    body = data.frame(Stub = c("first", "a much longer stub entry"), Value = c("1", "2"),
                      stringsAsFactors = FALSE),
    title = "A Table", stub = "Stub",
    general = "General note.", specific = "Specific note.", probability = "*p* < .05."
  )
  md <- nomologR:::nomo_apa_markdown(tab, latex = FALSE)
  expect_identical(md[[1L]], "*A Table*")
  expect_false(any(grepl("**Table", md, fixed = TRUE)))
  notes <- md[(length(md) - 5L):length(md)]
  expect_identical(notes, c("", "*Note.* General note.", "", "^a^ Specific note.", "",
                            "*p* < .05."))
  # The separator's dashes follow each column's widest entry, with room for
  # the cell padding; a superscript counts as one character.
  expect_identical(md[[4L]], "| :-------------------------- | :-------: |")
  tab$body$Value[[2L]] <- "1234567^a^"
  expect_identical(nomologR:::nomo_apa_markdown(tab)[[4L]],
                   "| :-------------------------- | :----------: |")
  expect_false(any(grepl("^Table", capture.output(print(tab)))))
})


test_that("a PDF knitted with pdflatex gets TeX math, not Greek letters (#145)", {
  skip_on_cran()
  rel <- nomo_reliability(apa_cfa())
  inv <- nomo_invariance(apa_model, data = nomo_demo_network, group = "group",
                         levels = c("configural", "metric"))
  rt <- nomo_retest(data.frame(a = c(1, 2, 3, 4, 5, 6), b = c(1.2, 2.1, 2.8, 4.3, 5.1, 5.7)),
                    scores = c("a", "b"))
  tables <- list(nomo_apa_table(apa_cfa(), "fit"), nomo_apa_table(rel), nomo_apa_table(inv),
                 nomo_apa_table(rt), nomo_apa_table(apa_net()), nomo_apa_table(apa_validity()))
  for (tab in tables) {
    tex <- paste(nomologR:::nomo_apa_markdown(tab, latex = TRUE), collapse = "\n")
    codes <- utf8ToInt(tex)
    # Above U+00FF only the em dash is left, which pandoc writes as "---".
    expect_true(all(codes <= 0xFF | codes == 0x2014), label = tab$title)
    expect_identical(nomologR:::nomo_apa_markdown(tab, latex = FALSE),
                     nomologR:::nomo_apa_markdown(tab))
  }
  fit <- paste(nomologR:::nomo_apa_markdown(tables[[1L]], latex = TRUE), collapse = "\n")
  expect_match(fit, "| $\\chi^2$ |", fixed = TRUE)
  inv_tex <- paste(nomologR:::nomo_apa_markdown(tables[[3L]], latex = TRUE), collapse = "\n")
  expect_match(inv_tex, "$\\Delta\\chi^2$ ($\\Delta$*df*)", fixed = TRUE)
  rt_tex <- paste(nomologR:::nomo_apa_markdown(tables[[4L]], latex = TRUE), collapse = "\n")
  expect_match(rt_tex, "1.96 $\\times$ $\\sqrt{2}$ $\\times$ *SEM*", fixed = TRUE)
  expect_identical(nomologR:::nomo_apa_tex("\u2264 \u2212 \u00b2"), "$\\leq$ $-$ $^2$")
})


test_that("the retest table italicizes SEM, cites in order, and capitalizes its stub (#145)", {
  rt <- nomo_retest(data.frame(a = c(1, 2, 3, 4, 5, 6), b = c(1.2, 2.1, 2.8, 4.3, 5.1, 5.7)),
                    scores = c("a", "b"), interval = "one week")
  tab <- nomo_apa_table(rt)
  expect_identical(tab$body$Composite, "Composite")
  expect_identical(names(tab$body)[[6L]], "*SEM*")
  expect_match(tab$notes$general, "(Koo & Li, 2016; McGraw & Wong, 1996)", fixed = TRUE)
  expect_match(tab$notes$general, "*SEM* = standard error of measurement", fixed = TRUE)
  expect_match(tab$notes$general, "CI = confidence interval", fixed = TRUE)
  expect_match(tab$notes$general, "Interval between occasions: one week.", fixed = TRUE)
})


test_that("specific-note markers follow reading order and share a cell (#145)", {
  body <- data.frame(A = c("x", "y"), B = c("1", "2"), stringsAsFactors = FALSE)
  marked <- nomologR:::nomo_apa_mark(body, list(
    list(rows = c(FALSE, TRUE), column = "A", note = "Second."),
    list(rows = c(TRUE, TRUE), column = "B", note = "First."),
    list(rows = c(FALSE, TRUE), column = "B", note = "Third."),
    list(rows = c(FALSE, FALSE), column = "A", note = "Never.")
  ))
  expect_identical(marked$specific, c("First.", "Second.", "Third."))
  expect_identical(marked$body$A, c("x", "y^b^"))
  expect_identical(marked$body$B, c("1^a^", "2^a,c^"))
})
