# Report-ready evidence tables -------------------------------------------------

#' Extract report-ready evidence tables
#'
#' `nomo_table()` returns compact tibbles intended for manuscripts, audit
#' appendices, teaching materials, and the future `nomo_report()` workflow.
#' It never rounds away the underlying object: full engine results remain stored
#' in the original `nomologR` object.
#'
#' @param x A supported `nomologR` result object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble.
#' @export
nomo_table <- function(x, ...) {
  UseMethod("nomo_table")
}


#' @export
nomo_table.nomo_hypotheses <- function(x, ...) {
  x$hypotheses
}


#' @export
nomo_table.nomo_network <- function(
    x,
    type = c(
      "hypotheses",
      "fit",
      "measurement",
      "relations",
      "replication",
      "decision_log"
    ),
    ...) {
  type <- match.arg(type)

  if (type == "hypotheses") {
    keep <- c(
      "id", "relation", "prediction", "theoretical_region", "scale",
      "estimate", "se", "ci_lower", "ci_upper", "p_value",
      "equivalence_ci_lower", "equivalence_ci_upper",
      "equivalence_supported", "concordance", "confirmatory_status",
      "evidence_scope", "measurement_attention"
    )
    return(x$hypothesis_evidence[, keep, drop = FALSE])
  }

  if (type == "fit") return(x$fit_evidence)

  if (type == "measurement") {
    return(x$measurement_context$summary)
  }

  if (type == "relations") return(x$model_relations)

  if (type == "replication") {
    if (!nrow(x$replication_evidence)) {
      return(tibble::tibble())
    }
    return(x$replication_evidence)
  }

  x$decision_log
}


#' @export
nomo_table.nomo_invariance <- function(
    x,
    type = c(
      "fit",
      "categories",
      "partial",
      "local_strain",
      "decision_log"
    ),
    ...) {
  type <- match.arg(type)

  if (type == "fit") return(x$fit_evidence)
  if (type == "categories") return(x$ordered_categories)

  if (type == "partial") {
    if (is.null(x$partial)) return(tibble::tibble())
    return(x$partial$releases)
  }

  if (type == "local_strain") return(x$local_strain)

  x$decision_log
}
