# Guided workflow: provenance, recipes, and settings tables --------------------

nomo_run_component_logs <- function(x) {
  rows <- list()
  cursor <- 0L

  add <- function(component, scope, obj) {
    log <- obj$decision_log
    if (is.null(log) || !inherits(log, "data.frame") || !nrow(log)) return(invisible())
    tab <- tibble::as_tibble(log)
    tab$pipeline_component <- component
    tab$pipeline_scope <- scope
    tab <- tab[, c(
      "pipeline_component",
      "pipeline_scope",
      setdiff(names(tab), c("pipeline_component", "pipeline_scope"))
    ), drop = FALSE]
    cursor <<- cursor + 1L
    rows[[cursor]] <<- tab
  }

  for (stage in nomo_run_stage_order()) {
    stage_results <- x$results[[stage]]
    if (!is.null(stage_results)) {
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

      for (scope in names(stage_results)) add(stage, scope, stage_results[[scope]])
    }

    # Evidence attached to a stage (#73) follows it in the log: the
    # instrument-wide careless-responding screen after the per-scale audits,
    # scores and the measurement model's missing-data sensitivity after
    # validity, and the network's after the network.
    if (identical(stage, "screen") && !is.null(x$results$effort)) {
      add("screen", "careless_responding", x$results$effort)
    }
    if (identical(stage, "validity")) {
      if (!is.null(x$results$scores)) {
        add("scores", "measurement_model",
            list(decision_log = nomo_run_scores_log(x$results$scores)))
      }
      if (!is.null(x$results$missing$cfa)) {
        add("missing", "measurement_model", x$results$missing$cfa)
      }
    }
    if (identical(stage, "network") && !is.null(x$results$missing$network)) {
      add("missing", "theory_network", x$results$missing$network)
    }
  }

  if (!length(rows)) return(tibble::tibble())
  dplyr::bind_rows(rows)
}


# nomo_scores() reports notes rather than a decision log; as log rows they
# reach the component log and the report's evidence trace like any other.
nomo_run_scores_log <- function(scores) {
  log <- nomo_log_new()
  notes <- scores$notes
  for (i in seq_len(nrow(notes))) {
    log <- nomo_log_add(
      log, stage = "scores", object = "scores", metric = notes$topic[[i]],
      severity = notes$severity[[i]], observation = notes$note[[i]]
    )
  }
  log
}


nomo_run_initial_log <- function(scales, roles, handoff = NULL) {
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

  if (!is.null(handoff)) {
    log <- nomo_run_handoff_log(log, handoff)
  }

  for (nm in names(scales)) {
    log <- nomo_run_workflow_log_add(
      log,
      id = paste0("scale_definition:", nm),
      stage = "design",
      scope = nm,
      observation = if (is.null(handoff)) {
        sprintf(
          "Scale `%s` contains %s.",
          nm,
          nomo_present_count(length(scales[[nm]]), "explicitly supplied candidate item")
        )
      } else {
        sprintf(
          "Scale `%s` contains %s carried from content review in %s %s.",
          nm, nomo_present_count(length(scales[[nm]]), "item"),
          handoff$provenance$package, handoff$provenance$package_version
        )
      },
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
      source = if (is.null(handoff)) "researcher_input" else "content_review"
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
        "%s in more than one supplied scale: %s.",
        nomo_present_noun(length(overlap), "This item occurs", "These items occur"),
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


# The status of requested evidence attached to a stage (#73), in the stage
# table's words: completed when computed, not computed when the request failed
# (the design log says why), and otherwise not started.
nomo_run_attached_status <- function(x, result, id) {
  if (!is.null(result)) return("completed")
  failed <- x$decision_log$decision[x$decision_log$id == id]
  if (any(failed == "not computed")) "not_computed" else "not_started"
}


nomo_run_recipe_table <- function(x) {
  rows <- list()
  cursor <- 0L

  add_row <- function(stage, scope, fun, data_role, researcher_control,
                      status = nomo_run_stage_state(x, stage)) {
    cursor <<- cursor + 1L

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

  # Requested evidence that is not a stage of its own (#73, #145) has its own
  # rows, so the recipe names every component the run called.
  if (isTRUE(x$settings$screen$effort)) {
    add_row(
      "screen",
      "careless_responding",
      "nomo_screen(effort = TRUE)",
      "exploratory",
      "requested through `settings$screen$effort`; computed once over every item",
      status = if (is.null(x$results$effort)) nomo_run_stage_state(x, "screen") else "completed"
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
  if (nomo_run_attached_requested(x, "scores")) {
    add_row(
      "scores",
      "measurement_model",
      "nomo_scores()",
      "confirmatory",
      "requested through `settings$scores`; scoring method named by the researcher",
      status = nomo_run_attached_status(x, x$results$scores, "scores")
    )
  }
  if (nomo_run_attached_requested(x, "missing")) {
    add_row(
      "missing",
      "measurement_model",
      "nomo_missing()",
      "confirmatory",
      "requested through `settings$missing`; the fitted model is unchanged",
      status = nomo_run_attached_status(x, x$results$missing$cfa, "missing_data_cfa")
    )
  }
  add_row(
    "invariance",
    "configured_branch",
    "nomo_invariance()",
    "confirmatory",
    "requested through `settings$invariance`; partial releases remain explicit"
  )
  network_role <- if (identical(x$sample_design, "calibration_validation")) {
    "calibration + validation"
  } else {
    "same sample unless validation_data supplied"
  }
  add_row(
    "network",
    "configured_branch",
    "nomo_network()",
    network_role,
    "requested through `settings$network$hypotheses`"
  )
  if (nomo_run_attached_requested(x, "missing") &&
      nomo_run_branch_requested(x, "network")) {
    add_row(
      "missing",
      "theory_network",
      "nomo_missing()",
      network_role,
      "requested through `settings$missing`; the fitted network is unchanged",
      status = nomo_run_attached_status(x, x$results$missing$network, "missing_data_network")
    )
  }

  dplyr::bind_rows(rows)
}


# One row per stage, then one for each kind of attached evidence (#145), with
# the arguments set and their values.
nomo_run_settings_table <- function(x) {
  stages <- c(nomo_run_stage_order(), nomo_run_attached_settings())

  tibble::tibble(
    stage = stages,
    configured = vapply(
      stages,
      function(stage) {
        # An empty `missing` request asks for the comparison with its defaults.
        if (stage %in% nomo_run_attached_settings()) return(nomo_run_attached_requested(x, stage))
        stage %in% names(x$settings) && length(x$settings[[stage]]) > 0L
      },
      logical(1),
      USE.NAMES = FALSE
    ),
    setting_names = vapply(
      stages,
      function(stage) {
        setting <- x$settings[[stage]]
        if (is.null(setting) || !length(setting)) return("")
        paste(names(setting), collapse = ", ")
      },
      character(1),
      USE.NAMES = FALSE
    ),
    values = vapply(
      stages,
      function(stage) {
        setting <- x$settings[[stage]]
        if (is.null(setting)) return("")
        nomo_run_settings_text(setting)
      },
      character(1),
      USE.NAMES = FALSE
    )
  )
}


# The run's design log records where its item membership came from when a
# contentvalidR handoff defined the scales (#46): the producer and its carry
# rule, and each item content review held back, quoted in that package's words.
nomo_run_handoff_log <- function(log, handoff) {
  p <- handoff$provenance
  ev <- handoff$evidence

  log <- nomo_run_workflow_log_add(
    log,
    id = "content_review",
    stage = "design",
    scope = "scales",
    observation = sprintf(
      paste(
        "Scales and item membership come from content review in %s %s",
        "(workflow: %s; carry rule: %s): %d of %s %s carried."
      ),
      p$package, p$package_version, p$workflow, p$keep,
      sum(ev$carried), nomo_present_count(nrow(ev), "reviewed item"),
      nomo_present_noun(sum(ev$carried), "was", "were")
    ),
    reason = paste(
      "Content review is where the validity argument starts, so the archive",
      "records which review supplied the items and under which rule."
    ),
    options = paste(
      "Changing which items are analyzed is a researcher decision; start a new",
      "run with the revised membership and record why."
    ),
    consequence = paste(
      "Held-back items are not analyzed, and nomologR neither reinstates nor",
      "drops an item on the strength of these data."
    ),
    decision = paste(handoff$items, collapse = ", "),
    source = "content_review"
  )

  held <- ev[!ev$carried, , drop = FALSE]
  for (i in seq_len(nrow(held))) {
    # A review whose results had no status or recommendation records NA; it is
    # left out rather than quoted as the word "NA", as the handoff reader does
    # (#145).
    quoted <- c(
      if (!is.na(held$status[[i]])) sprintf("status \"%s\"", held$status[[i]]),
      if (!is.na(held$recommendation[[i]])) {
        sprintf("recommendation \"%s\"", held$recommendation[[i]])
      }
    )
    log <- nomo_run_workflow_log_add(
      log,
      id = paste0("held_back:", held$item[[i]]),
      stage = "design",
      scope = if (is.na(held$scale[[i]])) "scales" else as.character(held$scale[[i]]),
      observation = sprintf(
        "%s was held back by content review%s.",
        held$item[[i]],
        if (length(quoted)) paste0(": ", paste(quoted, collapse = ", ")) else ""
      ),
      reason = "Only items carried by content review are analyzed.",
      options = "Reinstating it is a researcher decision to record with its rationale.",
      consequence = "It is not screened, modeled, or scored in this run.",
      decision = "held back",
      source = "content_review"
    )
  }

  log
}
