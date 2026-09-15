# Guided workflow: provenance, recipes, and settings tables --------------------

nomo_run_component_logs <- function(x) {
  rows <- list()
  cursor <- 0L

  for (stage in nomo_run_stage_order()) {
    stage_results <- x$results[[stage]]
    if (is.null(stage_results)) next

    if (!is.list(stage_results) ||
        inherits(stage_results, c(
          "nomo_cfa",
          "nomo_reliability",
          "nomo_validity",
          "nomo_invariance",
          "nomo_network"
        ))) {
      stage_results <- list(workflow = stage_results)
    }

    if (is.null(names(stage_results))) {
      names(stage_results) <- rep("workflow", length(stage_results))
    }

    for (scope in names(stage_results)) {
      obj <- stage_results[[scope]]
      log <- obj$decision_log
      if (is.null(log) || !inherits(log, "data.frame") || !nrow(log)) next

      tab <- tibble::as_tibble(log)
      tab$pipeline_component <- stage
      tab$pipeline_scope <- scope
      tab <- tab[, c(
        "pipeline_component",
        "pipeline_scope",
        setdiff(names(tab), c("pipeline_component", "pipeline_scope"))
      ), drop = FALSE]

      cursor <- cursor + 1L
      rows[[cursor]] <- tab
    }
  }

  if (!length(rows)) return(tibble::tibble())
  dplyr::bind_rows(rows)
}


nomo_run_initial_log <- function(scales, roles) {
  log <- nomo_run_workflow_log_new()
  same_sample <- identical(roles$design, "same_sample")

  log <- nomo_run_workflow_log_add(
    log,
    id = "sample_design",
    stage = "design",
    scope = "sample",
    observation = if (same_sample) {
      sprintf(
        paste(
          "%d rows are available; exploratory and later confirmatory stages",
          "currently reference the same sample."
        ),
        roles$n_exploratory
      )
    } else {
      sprintf(
        paste(
          "%d calibration rows are reserved for exploratory work and %d",
          "validation rows for confirmatory work."
        ),
        roles$n_exploratory,
        roles$n_confirmatory
      )
    },
    reason = paste(
      "Sample independence changes how later confirmatory evidence should be",
      "interpreted and must remain visible."
    ),
    options = if (same_sample) {
      paste(
        "Continue with same-sample confirmation and label it honestly; or start",
        "with `nomo_split()` / external validation data when that tradeoff is justified."
      )
    } else {
      "Preserve the supplied calibration/validation roles unless a new workflow is started."
    },
    consequence = if (same_sample) {
      "Later CFA evidence must not be described as independent holdout confirmation."
    } else {
      paste(
        "Exploratory stages use calibration rows; confirmatory stages use the",
        "reserved validation rows."
      )
    },
    decision = roles$design,
    source = "researcher_input"
  )

  for (nm in names(scales)) {
    log <- nomo_run_workflow_log_add(
      log,
      id = paste0("scale_definition:", nm),
      stage = "design",
      scope = nm,
      observation = sprintf(
        "Scale `%s` contains %d explicitly supplied candidate item(s).",
        nm,
        length(scales[[nm]])
      ),
      reason = paste(
        "Item membership changes construct representation and cannot be chosen",
        "silently by the pipeline."
      ),
      options = "Review the supplied item membership; start a new run to change it.",
      consequence = paste(
        "Screening, factor-retention, and EFA evidence for this scale is",
        "restricted to the supplied item set."
      ),
      decision = paste(scales[[nm]], collapse = ", "),
      source = "researcher_input"
    )
  }

  memberships <- unlist(scales, use.names = FALSE)
  overlap <- names(which(table(memberships) > 1L))
  if (length(overlap)) {
    log <- nomo_run_workflow_log_add(
      log,
      id = "overlapping_items",
      stage = "design",
      scope = "scales",
      observation = sprintf(
        "The following item(s) occur in more than one supplied scale: %s.",
        paste(overlap, collapse = ", ")
      ),
      reason = paste(
        "Overlapping membership can be intentional, but it changes the meaning",
        "of scale-specific exploratory analyses."
      ),
      options = paste(
        "Retain the overlap intentionally or start a new run with revised",
        "scale definitions."
      ),
      consequence = paste(
        "Each named scale is analyzed exactly as supplied; no item is reassigned",
        "automatically."
      ),
      decision = "retain supplied overlapping membership",
      source = "researcher_input"
    )
  }

  log
}


nomo_run_recipe_table <- function(x) {
  rows <- list()
  cursor <- 0L

  add_row <- function(stage, scope, fun, data_role, researcher_control) {
    cursor <<- cursor + 1L
    status <- x$stage_status$status[
      x$stage_status$stage == stage
    ][[1L]]

    rows[[cursor]] <<- tibble::tibble(
      stage = stage,
      scope = scope,
      function_name = fun,
      data_role = data_role,
      status = status,
      researcher_control = researcher_control
    )
  }

  for (scope in names(x$scales)) {
    add_row(
      "screen",
      scope,
      "nomo_screen()",
      "exploratory",
      "candidate item membership supplied in `scales`"
    )
    add_row(
      "factors",
      scope,
      "nomo_factors()",
      "exploratory",
      "retention evidence only; factor count not auto-selected"
    )
    add_row(
      "efa",
      scope,
      "nomo_efa()",
      "exploratory",
      "explicit `factor_count` decision"
    )
  }

  add_row(
    "cfa",
    "measurement_model",
    "nomo_cfa()",
    "confirmatory",
    "explicit `cfa_model` decision"
  )
  add_row(
    "reliability",
    "measurement_model",
    "nomo_reliability()",
    "confirmatory",
    "evidence computed from retained CFA; no item decisions"
  )
  add_row(
    "validity",
    "measurement_model",
    "nomo_validity()",
    "confirmatory",
    "evidence computed from retained CFA; no valid/invalid verdict"
  )
  add_row(
    "invariance",
    "configured_branch",
    "nomo_invariance()",
    "confirmatory",
    "requested through `settings$invariance`; partial releases remain explicit"
  )
  add_row(
    "network",
    "configured_branch",
    "nomo_network()",
    if (identical(x$sample_design, "calibration_validation")) {
      "calibration + validation"
    } else {
      "same sample unless validation_data supplied"
    },
    "requested through `settings$network$hypotheses`"
  )

  dplyr::bind_rows(rows)
}


nomo_run_settings_table <- function(x) {
  stages <- nomo_run_stage_order()

  tibble::tibble(
    stage = stages,
    configured = vapply(
      stages,
      function(stage) {
        stage %in% names(x$settings) && length(x$settings[[stage]]) > 0L
      },
      logical(1)
    ),
    setting_names = vapply(
      stages,
      function(stage) {
        setting <- x$settings[[stage]]
        if (is.null(setting) || !length(setting)) return("")
        paste(names(setting), collapse = ", ")
      },
      character(1)
    )
  )
}
