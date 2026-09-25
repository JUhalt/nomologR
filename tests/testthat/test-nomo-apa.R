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
  expect_match(tab$notes$general, "Grouping variable: group.", fixed = TRUE)
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
  # A path and a correlation between the same two factors cannot both be
  # estimated, so one of them has no estimate; APA marks that with a dash
  # rather than a zero or a blank that could be misread as a value.
  h <- nomo_hypotheses(
    "Agency -> Persistence" = positive(min = .20),
    "Agency <-> Persistence" = positive()
  )
  net <- nomo_network(apa_model, data = nomo_demo_network, hypotheses = h)
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
  expect_identical(md[[6L]], "| :--- | :---: | :---: |")
  expect_match(md[[length(md)]], "^\\*Note\\.\\*")
})


test_that("the loadings table is stable", {
  # ASCII only, so the snapshot does not depend on the platform's encoding.
  expect_snapshot(print(nomo_apa_table(apa_cfa(), "loadings", number = 1)))
})


test_that("unsupported objects and arguments are refused with an explanation", {
  expect_error(nomo_apa_table(list()), "No APA table is available")
  expect_error(nomo_apa_table(apa_cfa(), "loadings", number = 0), "whole number")
  expect_error(nomo_apa_table(apa_cfa(), "nonsense"), "should be one of")
})


# Remaining paths (#72) --------------------------------------------------------

test_that("a one-factor model has no factor-correlation table, and says why", {
  skip_on_cran()
  one <- nomo_cfa("Agency =~ ag1 + ag2 + ag3 + ag4", nomo_demo_network)
  expect_error(nomo_apa_table(one, "factor_correlations"), "one factor")
})


test_that("a reliability coefficient that was not computed is an empty cell", {
  skip_on_cran()
  rel <- nomo_reliability(apa_cfa(), include_alpha = FALSE)
  tab <- nomo_apa_table(rel)
  # Columns: construct, omega, alpha. Omega was computed; alpha was not.
  expect_false(any(tab$body[[2L]] == nomologR:::nomo_apa_dash))
  expect_true(all(tab$body[[3L]] == nomologR:::nomo_apa_dash))
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
