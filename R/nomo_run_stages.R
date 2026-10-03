# Guided workflow: stage execution and blocking --------------------------------

nomo_run_component_call <- function(fun, fixed, extra, stage) {
  if (length(extra)) {
    overlap <- intersect(names(extra), names(fixed))
    if (length(overlap)) {
      stop(
        sprintf(
          "`settings$%s` cannot override pipeline-controlled argument(s): %s.",
          stage,
          paste(overlap, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  do.call(fun, c(fixed, extra))
}


nomo_run_safe_component <- function(fun, fixed, extra, stage) {
  tryCatch(
    list(
      ok = TRUE,
      value = nomo_run_component_call(
        fun = fun,
        fixed = fixed,
        extra = extra,
        stage = stage
      ),
      error = NULL,
      error_class = character()
    ),
    error = function(e) {
      list(
        ok = FALSE,
        value = NULL,
        error = conditionMessage(e),
        error_class = class(e)
      )
    }
  )
}


nomo_run_block_error <- function(x,
                                 stage,
                                 scope,
                                 message,
                                 error_class = "nomo_run_block") {
  x$status <- "blocked"
  x$next_stage <- stage
  x$blocked <- list(
    stage = stage,
    scope = scope,
    message = as.character(message),
    class = as.character(error_class)
  )

  x <- nomo_run_set_stage(
    x,
    stage,
    "blocked",
    sprintf("%s failed for `%s`: %s", stage, scope, message)
  )

  x$decision_requests <- tibble::tibble(
    id = paste0("blocked:", stage, ":", scope),
    stage = stage,
    scope = scope,
    observation = sprintf(
      "The `%s` stage could not complete for `%s`: %s",
      stage,
      scope,
      message
    ),
    reason = paste(
      "A later stage cannot be interpreted defensibly when an earlier required",
      "component did not complete."
    ),
    options = paste(
      "Inspect the underlying component result/error and stored earlier-stage",
      "evidence; correct the data, model, or settings and start a new `nomo_run()`."
    ),
    consequence = "No later workflow stage was run automatically.",
    example = "Start a new `nomo_run()` after correcting the blocking input."
  )

  x
}


nomo_run_block <- function(x, stage, scope, result) {
  nomo_run_block_error(
    x = x,
    stage = stage,
    scope = scope,
    message = result$error,
    error_class = result$error_class
  )
}


nomo_run_branch_requested <- function(x, stage) {
  stage %in% names(x$settings) && length(x$settings[[stage]]) > 0L
}


# Invariance and the network re-estimate the measurement model the researcher
# reviewed, so they are estimated as its CFA was (#145): each takes `ordered`,
# `estimator`, and `missing` from `settings$cfa` unless its own settings name
# them. A design-log row records what was inherited, and any setting of the
# stage's own that differs from the CFA's. Returns the stage's arguments and
# the log.
nomo_run_inherit_estimation <- function(x, stage, extra) {
  cfa <- x$settings$cfa
  own <- x$settings[[stage]]
  inherited <- character()
  differs <- character()
  for (arg in c("ordered", "estimator", "missing")) {
    if (arg %in% names(own)) {
      if (!identical(own[[arg]], cfa[[arg]])) differs <- c(differs, arg)
    } else if (!is.null(cfa[[arg]])) {
      extra[[arg]] <- cfa[[arg]]
      inherited <- c(inherited, arg)
    }
  }
  if (!length(inherited) && !length(differs)) {
    return(list(extra = extra, log = x$decision_log))
  }

  shown <- function(args, values) {
    paste(vapply(args, function(arg) {
      value <- values[[arg]]
      if (is.null(value)) return(sprintf("%s = NULL (the default)", arg))
      sprintf("%s = %s", arg, paste(deparse(value, width.cutoff = 500L), collapse = " "))
    }, character(1)), collapse = ", ")
  }
  what <- c(invariance = "Invariance testing", network = "The nomological network")[[stage]]

  observation <- c(
    if (length(inherited)) {
      sprintf(
        "%s re-estimates the measurement model with the CFA stage's %s, from `settings$cfa`.",
        what, shown(inherited, cfa)
      )
    },
    if (length(differs)) {
      sprintf(
        "`settings$%s` sets %s, where the CFA stage used %s.",
        stage, shown(differs, own), shown(differs, cfa)
      )
    }
  )

  log <- nomo_run_workflow_log_add(
    x$decision_log,
    id = paste0("estimation_settings:", stage),
    stage = stage,
    scope = "measurement_model",
    observation = paste(observation, collapse = " "),
    reason = paste(
      "Invariance and network results are read as evidence about the",
      "measurement model the researcher reviewed, so by default they are",
      "estimated as its CFA was."
    ),
    options = sprintf(
      paste(
        "Name `ordered`, `estimator`, or `missing` in `settings$%s` to estimate",
        "this stage differently, and record why."
      ),
      stage
    ),
    consequence = if (length(differs)) {
      paste(
        "This stage's results rest on estimation settings that differ from the",
        "reviewed CFA's; read them with that difference in mind."
      )
    } else {
      "This stage uses the reviewed CFA's estimation settings."
    },
    decision = shown(c(inherited, differs), extra),
    source = if (length(differs)) "researcher_input" else "pipeline"
  )
  list(extra = extra, log = log)
}


nomo_run_run_invariance <- function(x) {
  if (!nomo_run_branch_requested(x, "invariance")) {
    x$results$invariance <- NULL
    return(
      nomo_run_set_stage(
        x,
        "invariance",
        "not_requested",
        "Measurement invariance was not requested in this workflow."
      )
    )
  }

  settings <- x$settings$invariance
  group <- settings$group
  extra <- nomo_run_stage_settings(
    x$settings,
    "invariance",
    remove = "group"
  )
  inherited <- nomo_run_inherit_estimation(x, "invariance", extra)
  extra <- inherited$extra
  x$decision_log <- inherited$log
  roles <- nomo_run_data_roles(x$source_data)

  result <- nomo_run_safe_component(
    fun = nomo_invariance,
    fixed = list(
      model = x$decisions$cfa_model$value,
      data = roles$confirmatory,
      group = group,
      guidance = x$guidance
    ),
    extra = extra,
    stage = "invariance"
  )

  if (!result$ok) {
    return(nomo_run_block(x, "invariance", group, result))
  }

  x$results$invariance <- result$value
  nomo_run_set_stage(
    x,
    "invariance",
    "completed",
    paste(
      "Requested invariance evidence computed; diagnostics did not free",
      "parameters automatically."
    )
  )
}


nomo_run_run_network <- function(x) {
  if (!nomo_run_branch_requested(x, "network")) {
    x$results$network <- NULL
    return(
      nomo_run_set_stage(
        x,
        "network",
        "not_requested",
        "A theory-specified nomological network was not requested in this workflow."
      )
    )
  }

  settings <- x$settings$network
  hypotheses <- settings$hypotheses
  extra <- nomo_run_stage_settings(
    x$settings,
    "network",
    remove = "hypotheses"
  )
  inherited <- nomo_run_inherit_estimation(x, "network", extra)
  extra <- inherited$extra
  x$decision_log <- inherited$log

  result <- nomo_run_safe_component(
    fun = nomo_network,
    fixed = list(
      model = x$decisions$cfa_model$value,
      data = x$source_data,
      hypotheses = hypotheses,
      guidance = x$guidance
    ),
    extra = extra,
    stage = "network"
  )

  if (!result$ok) {
    return(nomo_run_block(x, "network", "theory_network", result))
  }

  x$results$network <- result$value
  nomo_run_set_stage(
    x,
    "network",
    "completed",
    paste(
      "Theory-specified network evidence computed without a one-number validity",
      "score or validation-sample respecification."
    )
  )
}


nomo_run_finish_downstream <- function(x) {
  x <- nomo_run_run_invariance(x)
  if (identical(x$status, "blocked")) return(x)

  x <- nomo_run_run_network(x)
  if (identical(x$status, "blocked")) return(x)
  x <- nomo_run_run_missing(x, "network")

  x$status <- "complete"
  x$next_stage <- NULL
  x$blocked <- NULL
  x$decision_requests <- nomo_run_empty_requests()

  x$decision_log <- nomo_run_workflow_log_add(
    x$decision_log,
    id = "workflow_complete",
    stage = "workflow",
    scope = "pipeline",
    observation = paste(
      "All requested workflow stages have completed or are explicitly marked",
      "not requested."
    ),
    reason = "The pipeline stops only after all supplied consequential decisions are resolved.",
    options = "Inspect component objects, report-ready tables, and the audit/provenance logs.",
    consequence = paste(
      "No additional model modification, item deletion, parameter freeing, or",
      "validity verdict was performed by `nomo_run()`."
    ),
    decision = "complete",
    source = "pipeline"
  )

  x
}
