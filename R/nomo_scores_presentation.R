# Grice's (2001) three criteria, as one table for print() and summary().
nomo_scores_present_properties <- function(diagnostics, digits = 3L) {
  nomo_present_section("Score properties (Grice, 2001)")
  number <- function(v) nomo_present_number(v, digits)
  signed <- function(v) nomo_present_signed(v, digits)
  nomo_present_table(
    diagnostics,
    c("Factor" = "factor", "Items" = "n_items", "Validity" = "validity",
      "Univocality" = "univocality", "Correlational accuracy" = "correlational_accuracy"),
    formats = list(validity = number, univocality = signed,
                   correlational_accuracy = signed),
    more = "nomo_table(x, \"diagnostics\")"
  )
}


#' @export
print.nomo_scores <- function(x, digits = 3, ...) {
  nomo_present_header("nomo_scores", "Scores")
  nomo_present_facts(c(
    sprintf("%s weighting (method: %s)",
            if (identical(x$weighting, "unit")) "Unit" else "Model", x$method),
    nomo_present_count(nrow(x$diagnostics), "factor"),
    nomo_present_count(nrow(x$scores), "scored case")
  ))

  nomo_scores_present_properties(x$diagnostics, digits)

  if (isTRUE(x$parallel_test$available)) {
    nomo_present_section("Parallel model (what unit weighting assumes)")
    nomo_present_text(
      sprintf("chi-square difference %s on %s df, %s",
              nomo_present_number(x$parallel_test$chisq_diff, 2L),
              format(x$parallel_test$df_diff, trim = TRUE),
              nomo_present_p_clause(x$parallel_test$p_value)),
      indent = 2L
    )
  }

  flagged <- x$notes[x$notes$severity %in% c("review", "concern"), , drop = FALSE]
  if (nrow(flagged)) {
    nomo_present_section("Notes")
    nomo_present_notes(flagged)
  }

  cat("\n")
  nomo_present_text(
    "No value here is a pass/fail threshold; see nomo_table(x, \"diagnostics\")."
  )
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
  nomo_present_header("nomo_scores", "Scores", summary = TRUE)
  nomo_present_facts(c(
    sprintf("%s weighting (method: %s)",
            if (identical(x$weighting, "unit")) "Unit" else "Model", x$method),
    nomo_present_count(x$n_scored, "scored case")
  ))

  nomo_scores_present_properties(x$diagnostics, digits)

  if (nrow(x$unit_weighting)) {
    nomo_present_section("Standardized loading spread")
    nomo_present_table(
      x$unit_weighting,
      c("Factor" = "factor", "Lowest" = "min_loading", "Highest" = "max_loading",
        "Ratio" = "loading_ratio"),
      formats = list(
        min_loading = function(v) nomo_present_number(v, 2L),
        max_loading = function(v) nomo_present_number(v, 2L),
        loading_ratio = function(v) nomo_present_number(v, 2L)
      )
    )
  }

  if (nrow(x$notes)) {
    nomo_present_section("Notes")
    nomo_present_notes(x$notes)
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
