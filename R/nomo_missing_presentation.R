# Presentation methods for nomo_missing ----------------------------------------
#
# print() gives the missingness, the strategies compared, the largest
# differences from the reference, and each flag's observation; summary() adds
# missing values by variable, fit and reliability by strategy, every
# difference, and each recorded decision with its recommendation.


# Abbreviations and terms the output shows, defined once in it (guide point
# 23). Each reads after "FIML -- " in the key.
nomo_missing_glossary <- c(
  CFA = "Confirmatory factor analysis",
  FIML = "Full-information maximum likelihood",
  ML = "Maximum likelihood",
  Needs = paste(
    "What the strategy requires of the missing data: MCAR, missing completely",
    "at random, or MAR, missing at random; -- where no requirement is stated",
    "for the method lavaan used"
  ),
  N = "Cases the strategy analyzed",
  "Covariance coverage" = paste(
    "Proportion of cases with both variables of a pair observed; the lowest",
    "pair is shown"
  ),
  "Difference (SE)" = paste(
    "The estimate minus the reference strategy's estimate, in the reference",
    "standard error (SE); beyond half of one is flagged for review"
  )
)


# Where each flagged decision appears in the Flagged section.
nomo_missing_units <- c(
  estimate_difference = "Estimates",
  concordance_changed = "Concordance",
  nonconvergence = "Convergence",
  inadmissible_solution = "Admissibility"
)


nomo_missing_present_facts <- function(x) {
  p <- x$pattern
  label_of <- function(s) nomo_missing_table_label(s, x$strategies)
  nomo_present_facts(c(
    sprintf("Model: %s", if (identical(x$object, "nomo_network")) "Network" else "CFA"),
    sprintf("Reference: %s", label_of(x$reference)),
    sprintf("Fitted with: %s", label_of(x$fitted_as))
  ))
  nomo_present_facts(c(
    sprintf("Cases: %d of %d incomplete (%s)", p$n_incomplete, p$n_cases,
            nomo_present_percent(p$pct_incomplete, base = p$n_cases)),
    sprintf("Patterns: %d", p$n_patterns),
    # With every value observed, no pair is lower than another.
    sprintf("Lowest covariance coverage: %s%s",
            nomo_present_stat(p$min_coverage, "proportion"),
            if (p$min_coverage < 1) {
              sprintf(" (%s)", gsub(", ", paste0(",", nomo_present_nbsp),
                                    p$min_coverage_variables, fixed = TRUE))
            } else {
              ""
            })
  ))
}


nomo_missing_present_strategies <- function(x) {
  nomo_present_section("Strategies")
  nomo_present_table(
    x$strategies,
    nomo_present_drop_constant(
      c("Strategy" = "label", "lavaan" = "lavaan_missing", "Needs" = "requires",
        "Role" = "role", "Available" = "available", "N" = "n_used",
        "Converged" = "converged", "Admissible" = "admissible"),
      "Available", x$strategies$available
    ),
    formats = list(n_used = function(v) nomo_present_stat(v, "count")),
    more = "nomo_table(x, \"strategies\")"
  )
  # Why a strategy was not fitted, such as the reference when nothing is missing.
  s <- x$strategies
  unfitted <- s[!(s$available %in% TRUE) & !is.na(s$note) & nzchar(s$note), , drop = FALSE]
  if (nrow(unfitted)) nomo_present_bullets(paste0(unfitted$label, ": ", unfitted$note))
}


# The comparisons with a difference from the reference, largest first, with
# each estimate in its kind (a loading keeps its leading zero, a correlation
# does not). Estimates that differ by a fraction of a standard error often
# agree to two decimals, so they have three, as the decision log gives them.
nomo_missing_present_differences <- function(x, title, limit = Inf) {
  compared <- x$estimates[x$estimates$role == "comparison" &
                            is.finite(x$estimates$difference_in_se), , drop = FALSE]
  if (!nrow(compared)) return(invisible(NULL))
  compared <- compared[order(-abs(compared$difference_in_se)), , drop = FALSE]
  shown <- utils::head(compared, limit)
  shown$strategy_label <- nomo_missing_table_label(shown$strategy, x$strategies)
  kinds <- nomo_missing_estimate_kind(shown$type)
  by_kind <- function(v, reference = NULL) {
    vapply(seq_along(v), function(i) {
      nomo_present_stat(v[[i]], kinds[[i]], digits = nomo_missing_estimate_digits,
                        reference = if (is.null(reference)) NULL else reference[[i]])
    }, character(1))
  }
  # A flagged estimate shows the decimals that tell it from the reference
  # (guide point 9).
  shown$estimate_text <- by_kind(
    shown$estimate, ifelse(nomo_missing_true(shown$beyond_half_se), shown$reference_estimate, NA)
  )
  shown$reference_text <- by_kind(shown$reference_estimate)
  # Right-aligned as the numbers they are.
  shown$estimate_shown <- shown$estimate
  shown$reference_shown <- shown$reference_estimate
  nomo_present_section(title)
  nomo_missing_present_by_strategy(shown, x$strategies, function(rows, strategy, indent) {
    columns <- c("Parameter" = "parameter", "Strategy" = "strategy_label",
                 "Estimate" = "estimate_shown", "Reference" = "reference_shown",
                 "Difference (SE)" = "difference_in_se")
    nomo_present_table(
      rows,
      columns[strategy | columns != "strategy_label"],
      formats = list(
        estimate_shown = function(v) rows$estimate_text,
        reference_shown = function(v) rows$reference_text,
        difference_in_se = function(v) nomo_present_stat(v, "stat", signed = TRUE)
      ),
      more = "nomo_table(x, \"estimates\")",
      indent = indent,
      # The difference is what the table is for, so a narrow console drops the
      # estimates before it.
      keep = c(nomo_present_keep, "Difference (SE)")
    )
  })
}


# Rows that differ only by strategy cannot be told apart once a narrow console
# drops the Strategy column, so with more than one comparison strategy each has
# a table of its own under its label, in the order of the strategies table.
# With one, a single table names it in a column. `draw(rows, strategy, indent)`
# prints a table, with the Strategy column when `strategy` is TRUE.
nomo_missing_present_by_strategy <- function(rows, strategies, draw) {
  groups <- unique(rows$strategy[order(match(rows$strategy, strategies$strategy))])
  if (length(groups) < 2L) return(draw(rows, TRUE, 2L))
  for (s in groups) {
    nomo_present_text(nomo_missing_table_label(s, strategies), indent = 2L)
    draw(rows[rows$strategy == s, , drop = FALSE], FALSE, 4L)
  }
  invisible(NULL)
}


# The key: the abbreviations and columns this output shows.
nomo_missing_key <- function(x, extra = character()) {
  labels <- x$strategies$label
  shown <- c(
    if (!identical(x$object, "nomo_network")) "CFA",
    if (any(grepl("FIML", labels, fixed = TRUE))) "FIML",
    if (any(grepl("\\bML\\b", labels))) "ML",
    "Needs", "N", "Covariance coverage",
    if (is.data.frame(x$estimates) && any(is.finite(x$estimates$difference_in_se))) {
      "Difference (SE)"
    }
  )
  nomo_present_key(c(nomo_missing_glossary[shown], extra), title = "What these terms mean")
}


nomo_missing_present_flagged <- function(log, recommendation = FALSE) {
  flagged <- log[log$severity %in% c("review", "concern"), , drop = FALSE]
  if (!nrow(flagged)) return(invisible(NULL))
  unit <- unname(nomo_missing_units[flagged$metric])
  unit[is.na(unit)] <- ""
  nomo_present_flagged(flagged, recommendation = recommendation, unit = unit)
}


nomo_missing_caveat <- function() {
  cat("\n")
  nomo_present_text(
    "Whether data are missing at random cannot be tested from these data, and ",
    "agreement between strategies does not show that either is unbiased."
  )
}


#' @export
print.nomo_missing <- function(x, ...) {
  nomo_present_header("nomo_missing", "Missing-data sensitivity")
  nomo_missing_present_facts(x)
  nomo_missing_present_strategies(x)
  nomo_missing_present_differences(
    x, "Largest differences from the reference (standardized estimates)", limit = 5L
  )
  nomo_missing_key(x)
  nomo_missing_present_flagged(x$decision_log)
  nomo_missing_caveat()
  nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"decision_log\")"),
    c("every difference and each recommendation", "every recorded decision")
  )
  invisible(x)
}


#' @export
summary.nomo_missing <- function(object, ...) {
  out <- object[c("object", "reference", "fitted_as", "ordered", "pattern", "variables",
                  "strategies", "fit", "estimates", "reliability", "decision_log")]
  class(out) <- c("summary_nomo_missing", "list")
  out
}


#' @export
print.summary_nomo_missing <- function(x, ...) {
  nomo_present_header("nomo_missing", "Missing-data sensitivity", summary = TRUE)
  nomo_missing_present_facts(x)

  incomplete <- x$variables[x$variables$n_missing > 0L, , drop = FALSE]
  if (nrow(incomplete)) {
    nomo_present_section("Missing values by variable")
    nomo_present_table(
      incomplete,
      c("Variable" = "variable", "Missing" = "n_missing", "Share" = "pct_missing"),
      formats = list(pct_missing = function(v) nomo_present_percent(v, base = x$pattern$n_cases)),
      more = "nomo_table(x, \"variables\")"
    )
  }

  nomo_missing_present_strategies(x)

  # The strategy the model was fitted with is always refitted, so there is
  # always a fit to show.
  fit <- x$fit
  fit$strategy_label <- nomo_missing_table_label(fit$strategy, x$strategies)
  formats <- nomo_cfa_fit_formats()
  nomo_present_section("Fit by strategy")
  nomo_present_table(
    fit,
    c("Strategy" = "strategy_label", "Chi-square" = "chi_square", "df" = "df",
      "p" = "p_value", "CFI" = "CFI", "TLI" = "TLI", "RMSEA" = "RMSEA", "SRMR" = "SRMR"),
    formats = list(chi_square = formats$chisq, df = formats$df, p_value = formats$pvalue,
                   CFI = formats$cfi, TLI = formats$tli, RMSEA = formats$rmsea,
                   SRMR = formats$srmr),
    more = "nomo_table(x, \"fit\")"
  )

  nomo_missing_present_differences(
    x, "Differences from the reference (standardized estimates)"
  )

  reliability <- x$reliability
  compared <- if (is.data.frame(reliability)) reliability[reliability$role == "comparison", ]
  if (NROW(compared)) {
    compared$strategy_label <- nomo_missing_table_label(compared$strategy, x$strategies)
    # Three decimals, as the estimates have: strategies' coefficients often
    # agree to two.
    coefficient <- function(v) nomo_present_stat(v, "reliability", digits = 3L)
    columns <- nomo_present_drop_constant(
      c("Construct" = "construct", "Block" = "block", "Coefficient" = "metric",
        "Strategy" = "strategy_label", "Estimate" = "estimate",
        "Reference" = "reference_estimate", "Difference" = "difference"),
      "Block", compared$block
    )
    nomo_present_section("Reliability by strategy")
    nomo_missing_present_by_strategy(compared, x$strategies, function(rows, strategy, indent) {
      nomo_present_table(
        rows,
        columns[strategy | columns != "strategy_label"],
        formats = list(estimate = coefficient, reference_estimate = coefficient,
                       difference = function(v) {
                         nomo_present_stat(v, "reliability", digits = 3L, signed = TRUE)
                       }),
        more = "nomo_table(x, \"reliability\")",
        indent = indent,
        keep = c(nomo_present_keep, "Difference")
      )
    })
  }

  nomo_missing_key(
    x, extra = c(nomo_cfa_key_entries(c("CFI", "TLI", "RMSEA", "SRMR")), df = "Degrees of freedom")
  )
  nomo_missing_present_flagged(x$decision_log, recommendation = TRUE)

  # The log always records the missingness itself, so there is always a note.
  info <- x$decision_log[!x$decision_log$severity %in% c("review", "concern"), , drop = FALSE]
  nomo_present_section("Notes")
  nomo_present_bullets(trimws(paste(info$observation, info$recommendation)))

  nomo_missing_caveat()
  nomo_present_pointer(
    c("nomo_table(x, \"estimates\")", "nomo_table(x, \"decision_log\")"),
    c("every estimate under every strategy", "every recorded decision")
  )
  invisible(x)
}


# Each comparison's difference from the reference, in reference standard
# errors, with the half-standard-error reference drawn on both sides. A
# difference beyond it is marked for review, as the decision log flags it.
#' @export
plot.nomo_missing <- function(x, type = c("differences"), ...) {
  type <- nomo_match_arg(type)
  dat <- as.data.frame(x$estimates)
  dat <- dat[dat$role == "comparison" & is.finite(dat$difference_in_se), , drop = FALSE]
  if (!nrow(dat)) {
    stop(
      paste(
        "No difference from the reference is available to plot: nothing was",
        "compared with it, as when no modeled variable has missing values."
      ),
      call. = FALSE
    )
  }
  dat$strategy_label <- nomo_missing_table_label(dat$strategy, x$strategies)
  dat$parameter <- factor(dat$parameter, levels = rev(unique(dat$parameter)))
  dat$status <- nomo_plot_status(ifelse(nomo_missing_true(dat$beyond_half_se), "review", "none"))
  reference <- nomo_missing_table_label(x$reference, x$strategies)
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = difference_in_se, y = parameter)) +
    ggplot2::geom_vline(xintercept = 0, colour = "grey70") +
    ggplot2::geom_vline(xintercept = c(-nomo_missing_half_se, nomo_missing_half_se),
                        linetype = 2) +
    ggplot2::geom_point(ggplot2::aes(shape = status, colour = status), size = 2.8) +
    nomo_plot_status_scales(dat$status) +
    nomo_plot_labs(
      title = "Missing-data sensitivity",
      subtitle = sprintf("Standardized estimates compared with %s.",
                         nomo_missing_inline(reference)),
      x = "Difference from the reference, in reference standard errors", y = NULL,
      caption = paste(
        "Dashed lines: half a reference standard error, adapted from Schafer and",
        "Graham (2002); a difference beyond it is flagged for review."
      )
    ) +
    ggplot2::theme_minimal()
  if (length(unique(dat$strategy_label)) > 1L) {
    p <- p + ggplot2::facet_wrap(stats::as.formula("~ strategy_label"))
  }
  p
}


utils::globalVariables(c("difference_in_se", "parameter", "status", "strategy_label"))


#' @export
nomo_table.nomo_missing <- function(x,
                                    type = c(
                                      "strategies", "estimates", "fit",
                                      "reliability", "pattern", "variables",
                                      "decision_log"
                                    ),
                                    ...) {
  type <- nomo_match_arg(type)
  if (type == "reliability" && is.null(x$reliability)) {
    stop(
      paste(
        "No reliability comparison: it is made for `nomo_cfa` objects, unless",
        "`reliability = FALSE` was requested."
      ),
      call. = FALSE
    )
  }
  x[[type]]
}
