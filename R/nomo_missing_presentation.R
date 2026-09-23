#' @export
print.nomo_missing <- function(x, digits = 3, ...) {
  cat("<nomo_missing>\n")
  p <- x$pattern
  cat(sprintf(
    "Missing-data sensitivity for a %s | reference: %s | fitted with: %s\n",
    x$object, nomo_missing_label(x$reference), nomo_missing_label(x$fitted_as)
  ))
  cat(sprintf(
    "%d of %d cases incomplete (%.1f%%) in %d pattern(s); lowest covariance coverage %.3f (%s)\n",
    p$n_incomplete, p$n_cases, 100 * p$pct_incomplete, p$n_patterns,
    p$min_coverage, p$min_coverage_variables
  ))

  cat("\nStrategies\n")
  s <- x$strategies[, c(
    "label", "lavaan_missing", "requires", "role", "available", "n_used",
    "converged", "admissible"
  )]
  print(s, n = Inf, width = Inf)

  if (is.data.frame(x$estimates) && nrow(x$estimates)) {
    compared <- x$estimates[x$estimates$role == "comparison" &
                              is.finite(x$estimates$difference_in_se), , drop = FALSE]
    if (nrow(compared)) {
      compared <- compared[order(-abs(compared$difference_in_se)), , drop = FALSE]
      shown <- utils::head(compared, 5L)
      cat("\nLargest differences from the reference, in reference standard errors\n")
      view <- tibble::tibble(
        parameter = shown$parameter,
        strategy = nomo_missing_label(shown$strategy),
        estimate = round(shown$estimate, digits),
        reference = round(shown$reference_estimate, digits),
        difference_in_se = round(shown$difference_in_se, 2)
      )
      print(view, n = Inf, width = Inf)
    }
  }

  flagged <- x$decision_log[x$decision_log$severity %in% c("review", "concern"), , drop = FALSE]
  if (nrow(flagged)) {
    cat("\nFor review\n")
    cat(paste0("- ", flagged$observation), sep = "\n")
  }

  cat(
    "\nWhether data are missing at random cannot be tested from these data;",
    "see nomo_table(x, \"decision_log\").\n"
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
  type <- match.arg(type)
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
