# Report-ready evidence tables -------------------------------------------------

#' Extract report-ready evidence tables
#'
#' `nomo_table()` returns compact tibbles intended for manuscripts, audit
#' appendices, teaching materials, and [nomo_report()]. It never rounds away
#' the underlying object: full engine results remain stored in the original
#' `nomologR` object.
#'
#' @details
#' Supported objects and `type` values:
#'
#' * `nomo_hypotheses`: the machine-readable hypothesis table (no `type`).
#' * `nomo_network`: `"hypotheses"` (default), `"fit"`, `"measurement"`,
#'   `"relations"`, `"replication"`, `"decision_log"`.
#' * `nomo_invariance`: `"fit"` (default), `"categories"`, `"partial"`,
#'   `"local_strain"`, `"decision_log"`. The local-strain table keeps lavaan's
#'   internal `constraint` label and adds a human-readable
#'   `constraint_display` column (for example, `Intercept: ag3 (online vs.
#'   paper)`).
#' * `nomo_compare`: `"comparisons"` (default), `"models"`, `"loadings"`,
#'   `"evidence"`, `"decision_log"`; see [nomo_compare()].
#' * `nomo_run`: `"stages"` (default), `"requests"`, `"decisions"`,
#'   `"component_log"`, `"scales"`, `"recipe"`, `"settings"`; see
#'   [nomo_run()].
#'
#' @param x A supported `nomologR` result object.
#' @param ... Additional arguments passed to methods, usually `type`.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' h <- nomo_hypotheses(
#'   "Agency -> Persistence" = positive(min = .20),
#'   "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15))
#' )
#' nomo_table(h)
#'
#' inv <- nomo_invariance(
#'   "Agency =~ ag1 + ag2 + ag3 + ag4",
#'   data = nomo_demo_network,
#'   group = "group",
#'   levels = c("configural", "metric", "scalar")
#' )
#' nomo_table(inv, "fit")
#' head(nomo_table(inv, "local_strain"))
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

  if (type == "local_strain") return(nomo_invariance_local_strain_display(x))

  x$decision_log
}


#' @export
nomo_table.nomo_compare <- function(
    x,
    type = c(
      "comparisons",
      "models",
      "loadings",
      "evidence",
      "decision_log"
    ),
    ...) {
  type <- match.arg(type)

  if (type == "comparisons") return(x$comparisons)
  if (type == "models") return(x$models)
  if (type == "loadings") return(x$loadings)
  if (type == "evidence") return(x$evidence)

  x$decision_log
}
