# Presentation methods for nomo_scores -----------------------------------------
#
# print() gives the score properties with a key to their columns, the first
# sentence of each flagged note, and the parallel-model test when it is not
# flagged; summary() adds the loading spread and every note in full, so that
# the parallel-model test, carried by its note, is printed once in each.


# Where each note's topic appears in the Flagged section.
nomo_scores_units <- c(
  estimand = "Ordered indicators",
  unit_weighting = "Unit weighting",
  validity = "Validity",
  correlational_accuracy = "Correlational accuracy",
  univocality = "Univocality",
  cross_loadings = "Cross-loadings",
  loading_signs = "Loading signs",
  unscored_cases = "Unscored cases"
)


# "Delta chi-square(12) = ..." and a cited page, "p. 260", kept whole when
# wrapped: nomo_present_bind() binds a test's degrees of freedom and value, but
# not "Delta" to the test's name, nor a page to its number.
nomo_scores_bind <- function(text) {
  text <- gsub("Delta chi-square", paste0("Delta", nomo_present_nbsp, "chi-square"), text,
               fixed = TRUE)
  gsub("\\bp\\. ([0-9])", paste0("p.", nomo_present_nbsp, "\\1"), text, perl = TRUE)
}


# A note's first sentence, which states what was found (see the notes in
# R/nomo_scores_guidance.R). A full stop inside a number or a citation's page
# ("p. 260") is not followed by a capital, so it does not end the sentence.
nomo_scores_first_sentence <- function(text) {
  sub("^(.*?[.!?])\\s+(?=[A-Z`]).*$", "\\1", text, perl = TRUE)
}


nomo_scores_present_facts <- function(method, weighting, n_factors, n_cases) {
  nomo_present_facts(c(
    sprintf("Method: %s (%s weighted)", method, weighting),
    sprintf("Factors: %d", n_factors),
    sprintf("Cases: %d", n_cases)
  ))
}


# Cases with a score on each factor, in the order of `diagnostics`.
nomo_scores_scored <- function(scores, factors) {
  vapply(factors, function(f) sum(is.finite(scores[[f]])), integer(1), USE.NAMES = FALSE)
}


# Grice's (2001) three criteria, as one table for print() and summary(). A
# validity just below Gorsuch's .90, or a discrepancy just short of .05, shows
# the decimals that tell it from the reference.
nomo_scores_present_properties <- function(diagnostics, scored, n_cases, digits = NULL) {
  nomo_present_section("Score properties (Grice, 2001)")
  shown <- diagnostics
  shown$scored <- scored
  columns <- c("Factor" = "factor", "Items" = "n_items", "Scored" = "scored",
               "Validity" = "validity", "Univocality" = "univocality",
               "Correlational accuracy" = "correlational_accuracy")
  # Every case scored on every factor needs no column.
  if (all(scored == n_cases)) columns <- columns[columns != "scored"]
  reference <- nomo_scores_discrepancy_reference
  nomo_present_table(
    shown, columns,
    formats = list(
      validity = function(v) nomo_present_stat(v, "r", digits, reference = 0.90),
      univocality = function(v) nomo_present_stat(v, "r", digits),
      correlational_accuracy = function(v) {
        nomo_present_stat(v, "r", digits, reference = sign(v) * reference, signed = TRUE)
      }
    ),
    more = "nomo_table(x, \"diagnostics\")"
  )
}


# The key to the properties table, and in summary() to the loading spread.
nomo_scores_key <- function(several, scored, n_cases, spread = FALSE) {
  entries <- c(
    Scored = if (!all(scored == n_cases)) {
      sprintf("Cases with a score on the factor, of the %d the model used", n_cases)
    } else {
      ""
    },
    Validity = "Correlation of the score with its own factor; higher is better",
    Univocality = if (several) {
      paste(
        "Largest correlation of the score with another factor. Through its own",
        "factor, a score reaches another by the factor correlation times its",
        "validity; a departure of .05 or more from that is noted"
      )
    } else {
      ""
    },
    "Correlational accuracy" = if (several) {
      paste(
        "Score correlation minus factor correlation, for the pair of factors",
        "where they differ most; 0 is best"
      )
    } else {
      ""
    },
    "Lowest, Highest" = if (spread) {
      "The weakest and strongest standardized loading on the factor"
    } else {
      ""
    },
    Ratio = if (spread) {
      paste(
        "The strongest standardized loading divided by the weakest, in absolute",
        "value; -- where the loadings differ in sign"
      )
    } else {
      ""
    }
  )
  nomo_present_key(entries)
}


# The parallel-model test as a section, when it ran and its note is not
# flagged. A flagged test is printed with the other flags instead.
nomo_scores_present_parallel <- function(parallel) {
  if (!isTRUE(parallel$available) || !is.finite(parallel$p_value) ||
      parallel$p_value < .05) {
    return(invisible(NULL))
  }
  nomo_present_section("Parallel model (what unit weighting assumes)")
  nomo_present_text(
    nomo_scores_bind(sprintf(
      paste(
        "It does not fit worse than the model you fitted (%s), so the",
        "constraints adding the items assumes are consistent with these data."
      ),
      nomo_present_chisq(parallel$chisq_diff, parallel$df_diff, parallel$p_value,
                         delta = TRUE)
    )),
    indent = 2L
  )
}


# The Flagged section: each review or concern note under the unit it is
# about, its first sentence in print() and in full in summary(). A unit gets
# one bullet (guide point 22), so notes on the same unit, such as the
# parallel-model test and the loading ratio under unit weighting, are joined in
# the order they were raised, under the more severe of their statuses.
nomo_scores_present_flagged <- function(notes, full = FALSE) {
  flagged <- notes[notes$severity %in% c("review", "concern"), , drop = FALSE]
  if (!nrow(flagged)) return(invisible(NULL))
  unit <- unname(nomo_scores_units[flagged$topic])
  text <- if (isTRUE(full)) flagged$note else nomo_scores_first_sentence(flagged$note)
  units <- unique(unit)
  nomo_present_flagged(
    unit = units,
    status = vapply(units, function(u) {
      c("review", "concern")[[1L + any(flagged$severity[unit == u] == "concern")]]
    }, character(1), USE.NAMES = FALSE),
    text = nomo_scores_bind(vapply(units, function(u) {
      paste(text[unit == u], collapse = " ")
    }, character(1), USE.NAMES = FALSE))
  )
}


nomo_scores_present_ordered <- function(ordered) {
  if (!length(ordered)) return(invisible(NULL))
  nomo_present_text(
    sprintf(
      paste(
        "With %s, these values approximate the properties of the scores",
        "returned; see the note on ordered indicators."
      ),
      nomo_present_count(length(ordered), "ordered indicator")
    ),
    indent = 2L
  )
}


# What the evidence does not decide (guide point 7).
nomo_scores_caveat <- function() {
  cat("\n")
  nomo_present_text(
    "No value here is a pass/fail threshold, and no scoring method is chosen for you."
  )
}


#' @export
print.nomo_scores <- function(x, digits = NULL, ...) {
  nomo_present_header("nomo_scores", "Scores from a measurement model")
  n_cases <- nrow(x$scores)
  nomo_scores_present_facts(x$method, x$weighting, nrow(x$diagnostics), n_cases)

  scored <- nomo_scores_scored(x$scores, x$diagnostics$factor)
  nomo_scores_present_properties(x$diagnostics, scored, n_cases, digits)
  nomo_scores_present_ordered(x$ordered)
  nomo_scores_key(nrow(x$diagnostics) > 1L, scored, n_cases)
  nomo_scores_present_parallel(x$parallel_test)
  nomo_scores_present_flagged(x$notes)

  nomo_scores_caveat()
  nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"diagnostics\")"),
    c("every note in full and the loading spread", "the score properties as a table")
  )
  invisible(x)
}


#' @export
summary.nomo_scores <- function(object, ...) {
  out <- list(
    call = object$call,
    method = object$method,
    weighting = object$weighting,
    ordered = object$ordered,
    n_scored = nrow(object$scores),
    scored = nomo_scores_scored(object$scores, object$diagnostics$factor),
    diagnostics = object$diagnostics,
    unit_weighting = object$unit_weighting,
    parallel_test = object$parallel_test,
    score_correlations = object$score_correlations,
    factor_correlations = object$factor_correlations,
    notes = object$notes
  )
  # "summary_nomo_scores" is the class, named as every other summary class in
  # the package is. "summary.nomo_scores" was its name before 1.0.0 and is kept
  # as a second class for one release, so that inherits() on it still holds.
  class(out) <- c("summary_nomo_scores", "summary.nomo_scores", "list")
  out
}


#' @export
print.summary_nomo_scores <- function(x, digits = NULL, ...) {
  nomo_present_header("nomo_scores", "Scores from a measurement model", summary = TRUE)
  nomo_scores_present_facts(x$method, x$weighting, nrow(x$diagnostics), x$n_scored)

  nomo_scores_present_properties(x$diagnostics, x$scored, x$n_scored, digits)
  nomo_scores_present_ordered(x$ordered)

  spread <- x$unit_weighting
  # A ratio taken in absolute value hides loadings of opposite sign, which a
  # note explains, so it is not shown for them.
  spread$loading_ratio[is.finite(spread$min_loading) & is.finite(spread$max_loading) &
                         sign(spread$min_loading) != sign(spread$max_loading)] <- NA_real_
  nomo_present_section("Standardized loading spread")
  loading <- function(v) nomo_present_stat(v, "loading", digits)
  nomo_present_table(
    spread,
    c("Factor" = "factor", "Lowest" = "min_loading", "Highest" = "max_loading",
      "Ratio" = "loading_ratio"),
    formats = list(
      min_loading = loading,
      max_loading = loading,
      loading_ratio = function(v) nomo_present_stat(v, "estimate", digits)
    ),
    more = "nomo_table(x, \"unit_weighting\")"
  )
  nomo_scores_key(nrow(x$diagnostics) > 1L, x$scored, x$n_scored, spread = TRUE)

  nomo_scores_present_flagged(x$notes, full = TRUE)
  info <- x$notes[!x$notes$severity %in% c("review", "concern"), , drop = FALSE]
  nomo_present_section("Notes")
  nomo_present_bullets(info$note)

  nomo_scores_caveat()
  nomo_present_pointer(
    c("nomo_table(x, \"notes\")", "nomo_table(x, \"unit_weighting\")"),
    c("the notes with their topics", "every loading statistic")
  )
  invisible(x)
}


#' @export
nomo_table.nomo_scores <- function(x,
                                   type = c(
                                     "scores", "diagnostics", "unit_weighting",
                                     "notes"
                                   ),
                                   ...) {
  type <- nomo_match_arg(type)
  if (type == "scores") return(x$scores)
  if (type == "diagnostics") return(x$diagnostics)
  if (type == "unit_weighting") return(x$unit_weighting)
  x$notes
}


# Each score's validity against Gorsuch's (1983) recommendations, which are
# drawn as references and never applied as rules. A validity below .90 is
# marked for review, as its note is.
#' @export
plot.nomo_scores <- function(x, type = c("validity"), ...) {
  type <- nomo_match_arg(type)
  dat <- as.data.frame(x$diagnostics)
  dat <- dat[is.finite(dat$validity), , drop = FALSE]
  dat$factor <- factor(dat$factor, levels = rev(dat$factor))
  dat$status <- nomo_plot_status(ifelse(dat$validity < 0.90, "review", "none"))
  ggplot2::ggplot(dat, ggplot2::aes(x = validity, y = factor)) +
    ggplot2::geom_vline(xintercept = c(0.80, 0.90), linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(shape = status, colour = status), size = 3) +
    nomo_plot_status_scales(dat$status) +
    ggplot2::scale_x_continuous(limits = c(min(0.5, dat$validity), 1),
                                labels = nomo_plot_bounded_labels) +
    nomo_plot_labs(
      title = "Validity of the scores (Grice, 2001)",
      subtitle = sprintf(
        "Correlation of each score with its own factor; method: %s (%s weighted).",
        x$method, x$weighting
      ),
      x = "Validity", y = NULL,
      caption = paste(
        "Dashed lines: Gorsuch's (1983) .80, and .90 for scores that stand in",
        "for the factors, reported as his recommendations, not applied as rules."
      )
    ) +
    ggplot2::theme_minimal()
}


utils::globalVariables(c("validity", "factor", "status"))
