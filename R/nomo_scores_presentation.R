#' @export
print.nomo_scores <- function(x, digits = 3, ...) {
  cat("<nomo_scores>\n")
  cat(sprintf(
    "%s weighting (method: %s) | %d factor(s) | %d scored case(s)\n",
    if (identical(x$weighting, "unit")) "Unit" else "Model",
    x$method, nrow(x$diagnostics), nrow(x$scores)
  ))

  diag <- x$diagnostics
  for (nm in c("validity", "univocality", "correlational_accuracy")) {
    diag[[nm]] <- round(diag[[nm]], digits)
  }
  cat("\nScore properties (Grice, 2001)\n")
  print(diag, n = Inf, width = Inf)

  if (isTRUE(x$parallel_test$available)) {
    cat(sprintf(
      "\nParallel model (what unit weighting assumes): chi-square difference %.2f on %s df, %s\n",
      x$parallel_test$chisq_diff,
      format(x$parallel_test$df_diff, trim = TRUE),
      nomo_compare_format_p(x$parallel_test$p_value)
    ))
  }

  flagged <- x$notes[x$notes$severity %in% c("review", "concern"), , drop = FALSE]
  if (nrow(flagged)) {
    cat("\nNotes\n")
    cat(paste0("- [", flagged$severity, "] ", flagged$note), sep = "\n")
  }

  cat("\nNo value here is a pass/fail threshold; see nomo_table(x, \"diagnostics\").\n")
  invisible(x)
}


#' @export
summary.nomo_scores <- function(object, ...) {
  out <- list(
    call = object$call,
    method = object$method,
    weighting = object$weighting,
    n_scored = nrow(object$scores),
    diagnostics = object$diagnostics,
    unit_weighting = object$unit_weighting,
    parallel_test = object$parallel_test,
    score_correlations = object$score_correlations,
    factor_correlations = object$factor_correlations,
    notes = object$notes
  )
  class(out) <- c("summary.nomo_scores", "list")
  out
}


#' @export
print.summary.nomo_scores <- function(x, digits = 3, ...) {
  cat("<summary.nomo_scores>\n")
  cat(sprintf(
    "%s weighting (method: %s), %d scored case(s)\n",
    if (identical(x$weighting, "unit")) "Unit" else "Model",
    x$method, x$n_scored
  ))

  cat("\nScore properties\n")
  for (i in seq_len(nrow(x$diagnostics))) {
    row <- x$diagnostics[i, ]
    cat(sprintf(
      "- %s (%d items): validity %.3f, univocality %+.3f, correlational accuracy %+.3f\n",
      row$factor, row$n_items, row$validity, row$univocality,
      row$correlational_accuracy
    ))
  }

  if (nrow(x$unit_weighting)) {
    cat("\nStandardized loading spread\n")
    for (i in seq_len(nrow(x$unit_weighting))) {
      row <- x$unit_weighting[i, ]
      cat(sprintf(
        "- %s: %.2f to %.2f (ratio %s)\n",
        row$factor, row$min_loading, row$max_loading,
        if (is.finite(row$loading_ratio)) sprintf("%.2f", row$loading_ratio) else "unavailable"
      ))
    }
  }

  if (nrow(x$notes)) {
    cat("\nNotes\n")
    cat(paste0("- [", x$notes$severity, "] ", x$notes$note), sep = "\n")
  }

  invisible(x)
}


#' @export
nomo_table.nomo_scores <- function(x,
                                   type = c(
                                     "scores", "diagnostics", "unit_weighting",
                                     "notes"
                                   ),
                                   ...) {
  type <- match.arg(type)
  if (type == "scores") return(x$scores)
  if (type == "diagnostics") return(x$diagnostics)
  if (type == "unit_weighting") return(x$unit_weighting)
  x$notes
}
