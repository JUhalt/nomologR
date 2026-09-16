# Guided workflow: revision lineage ---------------------------------------------

nomo_revise_empty_lineage <- function() {
  tibble::tibble(
    revision = integer(),
    change_type = character(),
    items_removed = character(),
    items_added = character(),
    parent_model = character(),
    revised_model = character(),
    origin = character(),
    rationale = character(),
    sample_design = character(),
    comparison = character()
  )
}


nomo_run_lineage <- function(x) {
  lineage <- x$lineage
  if (is.null(lineage) || !is.data.frame(lineage)) {
    return(nomo_revise_empty_lineage())
  }
  lineage
}


nomo_revise_validate_parent <- function(run) {
  if (!inherits(run, "nomo_run")) {
    stop("`run` must be a `nomo_run` object.", call. = FALSE)
  }
  if (identical(run$status, "blocked")) {
    stop(
      paste(
        "The parent workflow is blocked by a component error. Correct the",
        "input, model, or settings and start a new `nomo_run()` rather than",
        "revising a blocked workflow."
      ),
      call. = FALSE
    )
  }
  if (is.null(run$results$cfa) || is.null(run$decisions$cfa_model)) {
    stop(
      paste(
        "A revision needs a fitted measurement model. Supply the `cfa_model`",
        "decision in `nomo_run()` first, then revise the workflow."
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


nomo_revise_scales <- function(run, items) {
  scales <- run$scales
  if (is.null(items)) return(scales)

  if (!is.list(items) || !length(items) || is.null(names(items)) ||
      anyNA(names(items)) || any(!nzchar(names(items)))) {
    stop(
      "`items` must be a non-empty named list of item vectors, one element per revised scale.",
      call. = FALSE
    )
  }

  unknown <- setdiff(names(items), names(scales))
  if (length(unknown)) {
    stop(
      sprintf(
        paste(
          "Unknown scale(s) in `items`: %s. A revision changes the items of an",
          "existing scale; start a new `nomo_run()` to change which scales exist."
        ),
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (nm in names(items)) {
    value <- items[[nm]]
    if (!is.character(value) || !length(value) || anyNA(value) ||
        any(!nzchar(value)) || anyDuplicated(value)) {
      stop(
        sprintf("`items$%s` must be unique, non-missing item-column names.", nm),
        call. = FALSE
      )
    }
    scales[[nm]] <- value
  }

  scales
}


nomo_revise_item_changes <- function(parent_scales, revised_scales) {
  removed <- character()
  added <- character()
  for (nm in names(parent_scales)) {
    removed <- c(removed, setdiff(parent_scales[[nm]], revised_scales[[nm]]))
    added <- c(added, setdiff(revised_scales[[nm]], parent_scales[[nm]]))
  }
  list(removed = unique(removed), added = unique(added))
}


nomo_revise_inherited_factor_counts <- function(run, scales) {
  decision <- run$decisions$factor_count
  if (is.null(decision)) return(NULL)

  counts <- decision$value
  if (!is.numeric(counts) || is.null(names(counts))) return(NULL)
  if (!all(names(scales) %in% names(counts))) return(NULL)
  counts <- counts[names(scales)]

  item_counts <- vapply(scales, length, integer(1))
  too_many <- names(scales)[counts >= item_counts]
  if (length(too_many)) {
    stop(
      sprintf(
        paste(
          "The factor count inherited from the parent workflow is not smaller",
          "than the revised item count for %s. Supply an explicit",
          "`decisions = list(factor_count = ...)` for the revised scales."
        ),
        paste(too_many, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  parent_rationale <- decision$rationale
  if (!is.character(parent_rationale) || !length(parent_rationale)) {
    parent_rationale <- ""
  }
  list(
    value = counts,
    rationale = trimws(paste(
      "Inherited from the parent workflow.",
      paste(parent_rationale, collapse = " ")
    ))
  )
}


nomo_revise_holdout_note <- function(sample_design) {
  if (identical(sample_design, "calibration_validation")) {
    paste(
      "The confirmatory stages already used the validation rows, so this",
      "revision is no longer independent of them. Confirm the revised model in",
      "a new sample."
    )
  } else {
    paste(
      "The revision is evaluated on the same sample that motivated it. Confirm",
      "the revised model in independent data, for example with `nomo_split()`",
      "or a new sample."
    )
  }
}


nomo_revise_comparison_summary <- function(comparison, note) {
  if (is.null(comparison)) return(note)
  cmp <- comparison$comparisons
  if (!nrow(cmp)) return(note)

  if (isTRUE(cmp$test_available[[1L]])) {
    sprintf(
      "%s: chi-square difference = %.2f, df = %s, %s",
      cmp$test[[1L]], cmp$chisq_diff[[1L]],
      format(cmp$df_diff[[1L]], trim = TRUE),
      nomo_compare_format_p(cmp$p_value[[1L]])
    )
  } else {
    cmp$test_note[[1L]]
  }
}


#' Revise a guided workflow and keep its lineage
#'
#' `nomo_revise()` creates a child workflow from a parent `nomo_run()` with a
#' revised measurement model, a revised item set, or both. The child records
#' what changed, why, whether the change was prespecified or post hoc, and a
#' [nomo_compare()] result for the parent and revised measurement models. The
#' parent workflow is not modified.
#'
#' @details
#' Revising is always a researcher decision: a rationale is required and no
#' item is removed or parameter freed automatically. The child workflow reruns
#' the staged evidence from screening onward with the revised scales and model,
#' then pauses at the measurement review so the revised evidence is inspected
#' before any downstream branch runs.
#'
#' The factor-count decision is inherited from the parent unless `decisions`
#' supplies a new one, so the revision changes only what the researcher
#' changed.
#'
#' Because a revision prompted by results is evaluated on the data that
#' prompted it, the decision log records whether the change was `"post_hoc"`
#' and recommends confirming the revised model in independent data (Simmons,
#' Nelson, & Simonsohn, 2011; Wicherts et al., 2016; Flake & Fried, 2020).
#'
#' Removing an item changes the observed variables, so parent and revised
#' models are not nested and `nomo_compare()` reports descriptive evidence
#' only. To test whether an item is needed, keep it and fix its loading to zero
#' in the revised model; see [nomo_compare()].
#'
#' @param run A parent `nomo_run` object with a fitted measurement model.
#' @param cfa_model Optional revised measurement model: a lavaan syntax string
#'   or an object from [nomo_model()].
#' @param items Optional named list of revised item vectors, one element per
#'   revised scale. Scales that are not named keep the parent's items.
#' @param rationale Required character scalar recording why the workflow is
#'   revised.
#' @param origin `"post_hoc"` (default) when the revision was prompted by
#'   results, or `"a_priori"` when it was planned in advance.
#' @param decisions Optional named list of decisions for the child workflow,
#'   for example `list(factor_count = c(WellBeing = 1))`. Supplied decisions
#'   override inherited ones.
#' @param compare Logical. If `TRUE` (default), compare the parent and revised
#'   measurement models with [nomo_compare()].
#'
#' @return A new `nomo_run` object for the revised workflow, carrying
#'   `$lineage` (one row per revision), `$revision_comparison` (the
#'   `nomo_compare()` result, when available), and `$parent_summary`.
#'
#' @references
#' Flake, J. K., & Fried, E. I. (2020). Measurement schmeasurement:
#' Questionable measurement practices and how to avoid them. *Advances in
#' Methods and Practices in Psychological Science, 3*(4), 456-465.
#' \doi{10.1177/2515245920952393}
#'
#' MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
#' modifications in covariance structure analysis: The problem of
#' capitalization on chance. *Psychological Bulletin, 111*(3), 490-504.
#' \doi{10.1037/0033-2909.111.3.490}
#'
#' Simmons, J. P., Nelson, L. D., & Simonsohn, U. (2011). False-positive
#' psychology: Undisclosed flexibility in data collection and analysis allows
#' presenting anything as significant. *Psychological Science, 22*(11),
#' 1359-1366. \doi{10.1177/0956797611417632}
#'
#' Wicherts, J. M., Veldkamp, C. L. S., Augusteijn, H. E. M., Bakker, M., van
#' Aert, R. C. M., & van Assen, M. A. L. M. (2016). Degrees of freedom in
#' planning, running, analyzing, and reporting psychological studies: A
#' checklist to avoid p-hacking. *Frontiers in Psychology, 7*, 1832.
#' \doi{10.3389/fpsyg.2016.01832}
#'
#' @seealso [nomo_run()], [nomo_compare()]
#'
#' @examples
#' \donttest{
#' scales <- list(Agency = c("ag1", "ag2", "ag3", "ag4"))
#'
#' run <- nomo_run(
#'   data = nomo_demo_network,
#'   scales = scales,
#'   settings = list(factors = list(n_iter = 20, seed = 2026)),
#'   decisions = list(
#'     factor_count = c(Agency = 1),
#'     cfa_model = "Agency =~ ag1 + ag2 + ag3 + ag4"
#'   )
#' )
#'
#' revised <- nomo_revise(
#'   run,
#'   cfa_model = "Agency =~ ag1 + ag2 + ag3 + ag4\nag1 ~~ ag2",
#'   rationale = paste(
#'     "Residual diagnostics and item wording suggest ag1 and ag2 share",
#'     "method variance beyond the common factor."
#'   ),
#'   origin = "post_hoc"
#' )
#'
#' nomo_table(revised, "lineage")
#' nomo_table(revised$revision_comparison, "comparisons")
#' }
#' @export
nomo_revise <- function(run,
                        cfa_model = NULL,
                        items = NULL,
                        rationale,
                        origin = c("post_hoc", "a_priori"),
                        decisions = list(),
                        compare = TRUE) {
  nomo_revise_validate_parent(run)
  origin <- match.arg(origin)

  if (missing(rationale) || !is.character(rationale) || length(rationale) != 1L ||
      is.na(rationale) || !nzchar(trimws(rationale))) {
    stop(
      paste(
        "`rationale` is required: record why the workflow is revised (for",
        "example, the substantive reason for changing the model or item set)."
      ),
      call. = FALSE
    )
  }
  rationale <- trimws(rationale)

  if (is.null(cfa_model) && is.null(items)) {
    stop(
      "Supply `cfa_model`, `items`, or both: a revision must change the measurement model or the item set.",
      call. = FALSE
    )
  }
  if (!is.list(decisions)) {
    stop("`decisions` must be a named list of workflow decisions.", call. = FALSE)
  }
  if (!is.logical(compare) || length(compare) != 1L || is.na(compare)) {
    stop("`compare` must be TRUE or FALSE.", call. = FALSE)
  }

  scales <- nomo_revise_scales(run, items)
  changes <- nomo_revise_item_changes(run$scales, scales)
  items_changed <- length(changes$removed) > 0L || length(changes$added) > 0L

  parent_model <- as.character(run$decisions$cfa_model$value)[1L]
  revised_model <- if (is.null(cfa_model)) {
    parent_model
  } else {
    nomo_run_normalize_cfa_model(cfa_model)
  }
  model_changed <- !identical(revised_model, parent_model)

  if (!model_changed && !items_changed) {
    stop(
      "The revised workflow is identical to its parent; change the measurement model, the items, or both.",
      call. = FALSE
    )
  }

  change_type <- if (model_changed && items_changed) {
    "model_and_items"
  } else if (model_changed) {
    "model"
  } else {
    "items"
  }

  child_decisions <- list()
  inherited_counts <- nomo_revise_inherited_factor_counts(run, scales)
  if (!is.null(inherited_counts)) child_decisions$factor_count <- inherited_counts
  child_decisions$cfa_model <- list(value = revised_model, rationale = rationale)
  for (nm in names(decisions)) child_decisions[[nm]] <- decisions[[nm]]

  child <- nomo_run(
    data = run$source_data,
    scales = scales,
    mode = run$mode,
    guidance = run$guidance,
    settings = run$settings,
    decisions = child_decisions
  )

  comparison <- NULL
  comparison_note <- ""
  if (isTRUE(compare)) {
    if (is.null(child$results$cfa)) {
      comparison_note <- paste(
        "Model comparison unavailable: the revised workflow did not produce a",
        "fitted measurement model."
      )
    } else {
      result <- tryCatch(
        nomo_compare(
          parent = run$results$cfa,
          revised = child$results$cfa,
          rationale = rationale,
          origin = origin,
          guidance = run$guidance
        ),
        error = function(e) e
      )
      if (inherits(result, "error")) {
        comparison_note <- paste("Model comparison unavailable:", conditionMessage(result))
      } else {
        comparison <- result
      }
    }
  } else {
    comparison_note <- "Model comparison was not requested."
  }

  lineage <- nomo_run_lineage(run)
  row <- tibble::as_tibble(list(
    revision = nrow(lineage) + 1L,
    change_type = change_type,
    items_removed = paste(changes$removed, collapse = ", "),
    items_added = paste(changes$added, collapse = ", "),
    parent_model = parent_model,
    revised_model = revised_model,
    origin = origin,
    rationale = rationale,
    sample_design = as.character(run$sample_design),
    comparison = nomo_revise_comparison_summary(comparison, comparison_note)
  ))

  child$lineage <- dplyr::bind_rows(lineage, row)
  child$revision_comparison <- comparison
  child$parent_summary <- list(
    status = run$status,
    mode = run$mode,
    sample_design = run$sample_design,
    sample_n = run$sample_n,
    scales = run$scales,
    cfa_model = parent_model,
    stage_status = run$stage_status,
    decisions = run$decisions
  )

  holdout <- nomo_revise_holdout_note(run$sample_design)
  origin_text <- if (identical(origin, "post_hoc")) {
    paste(
      "This revision was prompted by results, so it is recorded as post hoc.",
      "Data-driven respecification capitalizes on chance."
    )
  } else {
    "This revision was prespecified rather than prompted by the results."
  }

  change_text <- c(
    if (model_changed) "the measurement model was revised" else NULL,
    if (length(changes$removed)) {
      paste0("item(s) removed: ", paste(changes$removed, collapse = ", "))
    } else {
      NULL
    },
    if (length(changes$added)) {
      paste0("item(s) added: ", paste(changes$added, collapse = ", "))
    } else {
      NULL
    }
  )

  child$decision_log <- nomo_run_workflow_log_add(
    child$decision_log,
    id = "revision",
    stage = "workflow",
    scope = "measurement_model",
    observation = sprintf(
      "Revision %d of the guided workflow: %s.",
      row$revision, paste(change_text, collapse = "; ")
    ),
    reason = "A revised measurement model or item set changes every downstream interpretation.",
    options = paste(
      "Retain the parent workflow, retain the revision, or report both and",
      "record the reasoning."
    ),
    consequence = paste(origin_text, holdout),
    decision = sprintf("revised measurement model: %s", revised_model),
    rationale = rationale,
    source = "researcher_decision"
  )

  child$decision_log <- nomo_run_workflow_log_add(
    child$decision_log,
    id = "revision_comparison",
    stage = "workflow",
    scope = "measurement_model",
    observation = row$comparison,
    reason = "Parent and revised models are compared so the revision is auditable.",
    options = "Interpret the comparison with theory and the measurement evidence for both models.",
    consequence = "No model is selected automatically by the comparison.",
    decision = "",
    rationale = rationale,
    source = "pipeline"
  )

  child
}
