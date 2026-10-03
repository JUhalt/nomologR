#' @export
print.nomo_missing <- function(x, digits = 3, ...) {
  nomo_present_header("nomo_missing", "Missing-data sensitivity")
  p <- x$pattern
  nomo_present_facts(c(
    sprintf("Model: %s", x$object),
    sprintf("Reference: %s", nomo_missing_label(x$reference)),
    sprintf("Fitted with: %s", nomo_missing_label(x$fitted_as))
  ))
  nomo_present_facts(c(
    sprintf("Cases: %d of %d incomplete (%.1f%%)", p$n_incomplete, p$n_cases,
            100 * p$pct_incomplete),
    sprintf("Patterns: %d", p$n_patterns),
    sprintf("Lowest covariance coverage: %s (%s)", nomo_present_number(p$min_coverage),
            p$min_coverage_variables)
  ))

  nomo_present_section("Strategies")
  nomo_present_table(
    x$strategies,
    nomo_present_drop_constant(
      c("Strategy" = "label", "lavaan" = "lavaan_missing", "Needs" = "requires",
        "Role" = "role", "Available" = "available", "N" = "n_used",
        "Converged" = "converged", "Admissible" = "admissible"),
      "Available", x$strategies$available
    ),
    formats = list(n_used = function(v) {
      ifelse(is.finite(v), format(v, trim = TRUE), nomo_present_missing)
    }),
    more = "nomo_table(x, \"strategies\")"
  )
  # Why a strategy was not fitted, such as the reference when nothing is missing.
  s <- x$strategies
  unfitted <- s[!(s$available %in% TRUE) & !is.na(s$note) & nzchar(s$note), , drop = FALSE]
  if (nrow(unfitted)) nomo_present_bullets(paste0(unfitted$label, ": ", unfitted$note))

  if (is.data.frame(x$estimates) && nrow(x$estimates)) {
    compared <- x$estimates[x$estimates$role == "comparison" &
                              is.finite(x$estimates$difference_in_se), , drop = FALSE]
    if (nrow(compared)) {
      compared <- compared[order(-abs(compared$difference_in_se)), , drop = FALSE]
      shown <- utils::head(compared, 5L)
      shown$strategy_label <- nomo_missing_label(shown$strategy)
      nomo_present_section(
        "Largest differences from the reference, in reference standard errors"
      )
      nomo_present_table(
        shown,
        c("Parameter" = "parameter", "Strategy" = "strategy_label",
          "Estimate" = "estimate", "Reference" = "reference_estimate",
          "Difference (SE)" = "difference_in_se"),
        formats = list(
          estimate = function(v) nomo_present_number(v, digits),
          reference_estimate = function(v) nomo_present_number(v, digits),
          difference_in_se = function(v) nomo_present_signed(v, 2L)
        ),
        more = "nomo_table(x, \"estimates\")"
      )
    }
  }

  flagged <- x$decision_log[x$decision_log$severity %in% c("review", "concern"), , drop = FALSE]
  if (nrow(flagged)) {
    nomo_present_section("For review")
    nomo_present_bullets(flagged$observation)
  }

  cat("\n")
  nomo_present_text(
    "Whether data are missing at random cannot be tested from these data; ",
    "see nomo_table(x, \"decision_log\")."
  )
  invisible(x)
}


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
