# Guided workflow orchestration ------------------------------------------------

nomo_run_stage_order <- function() {
  c(
    "screen",
    "factors",
    "efa",
    "cfa",
    "reliability",
    "validity",
    "invariance",
    "network"
  )
}


nomo_run_stage_status_new <- function() {
  stages <- nomo_run_stage_order()
  tibble::tibble(
    stage = stages,
    status = rep("not_started", length(stages)),
    detail = rep("", length(stages))
  )
}


nomo_run_set_stage <- function(x, stage, status, detail = "") {
  idx <- match(stage, x$stage_status$stage)
  if (is.na(idx)) {
    stop(sprintf("Unknown workflow stage `%s`.", stage), call. = FALSE)
  }

  x$stage_status$status[[idx]] <- as.character(status)
  x$stage_status$detail[[idx]] <- as.character(detail)
  x
}


nomo_run_workflow_log_new <- function() {
  tibble::tibble(
    id = character(),
    stage = character(),
    scope = character(),
    observation = character(),
    reason = character(),
    options = character(),
    consequence = character(),
    decision = character(),
    rationale = character(),
    source = character()
  )
}


nomo_run_workflow_log_add <- function(log,
                                      id,
                                      stage,
                                      scope,
                                      observation = "",
                                      reason = "",
                                      options = "",
                                      consequence = "",
                                      decision = "",
                                      rationale = "",
                                      source = "pipeline") {
  dplyr::bind_rows(
    log,
    tibble::tibble(
      id = as.character(id),
      stage = as.character(stage),
      scope = as.character(scope),
      observation = as.character(observation),
      reason = as.character(reason),
      options = as.character(options),
      consequence = as.character(consequence),
      decision = as.character(decision),
      rationale = as.character(rationale),
      source = as.character(source)
    )
  )
}


nomo_run_empty_requests <- function() {
  tibble::tibble(
    id = character(),
    stage = character(),
    scope = character(),
    observation = character(),
    reason = character(),
    options = character(),
    consequence = character(),
    example = character()
  )
}


nomo_run_data_roles <- function(data) {
  if (inherits(data, "nomo_split")) {
    if (!is.data.frame(data$calibration) ||
        !is.data.frame(data$validation) ||
        nrow(data$calibration) < 1L ||
        nrow(data$validation) < 1L) {
      stop(
        paste(
          "A `nomo_split` supplied to `nomo_run()` must contain non-empty",
          "calibration and validation data frames."
        ),
        call. = FALSE
      )
    }

    return(list(
      exploratory = data$calibration,
      confirmatory = data$validation,
      design = "calibration_validation",
      source = "nomo_split",
      n_exploratory = nrow(data$calibration),
      n_confirmatory = nrow(data$validation)
    ))
  }

  if (!is.data.frame(data) || nrow(data) < 1L || ncol(data) < 1L) {
    stop(
      paste(
        "`data` must be a non-empty data frame or an object created by",
        "`nomo_split()`."
      ),
      call. = FALSE
    )
  }

  list(
    exploratory = data,
    confirmatory = data,
    design = "same_sample",
    source = "data_frame",
    n_exploratory = nrow(data),
    n_confirmatory = nrow(data)
  )
}


nomo_run_validate_scales <- function(scales, roles) {
  if (!is.list(scales) || !length(scales)) {
    stop(
      "`scales` must be a non-empty named list of item-name vectors.",
      call. = FALSE
    )
  }

  scale_names <- names(scales)
  if (is.null(scale_names) ||
      anyNA(scale_names) ||
      any(!nzchar(trimws(scale_names))) ||
      anyDuplicated(scale_names)) {
    stop("`scales` must have unique, non-empty names.", call. = FALSE)
  }

  for (nm in scale_names) {
    items <- scales[[nm]]

    if (!is.character(items) ||
        !length(items) ||
        anyNA(items) ||
        any(!nzchar(items)) ||
        anyDuplicated(items)) {
      stop(
        sprintf(
          paste(
            "Scale `%s` must contain one or more unique, non-missing",
            "item-column names."
          ),
          nm
        ),
        call. = FALSE
      )
    }

    absent_exploratory <- setdiff(items, names(roles$exploratory))
    absent_confirmatory <- setdiff(items, names(roles$confirmatory))
    absent <- unique(c(absent_exploratory, absent_confirmatory))

    if (length(absent)) {
      stop(
        sprintf(
          "Scale `%s` contains item column(s) unavailable in the workflow data: %s.",
          nm,
          paste(absent, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  scales
}


nomo_run_reserved_settings <- function() {
  list(
    screen = c("data", "items", "guidance"),
    factors = c("data", "items", "guidance"),
    efa = c("data", "items", "factors", "factor_count", "guidance"),
    cfa = c("model", "data", "guidance"),
    reliability = c("fit", "guidance"),
    validity = c("fit", "guidance"),
    invariance = c("model", "data", "guidance"),
    network = c("model", "data", "guidance")
  )
}


nomo_run_validate_settings <- function(settings, scales) {
  if (!is.list(settings)) {
    stop("`settings` must be a named list of stage-specific argument lists.", call. = FALSE)
  }

  if (!length(settings)) return(settings)

  nm <- names(settings)
  if (is.null(nm) ||
      anyNA(nm) ||
      any(!nzchar(nm)) ||
      anyDuplicated(nm)) {
    stop("`settings` must use unique stage names.", call. = FALSE)
  }

  bad <- setdiff(nm, nomo_run_stage_order())
  if (length(bad)) {
    stop(
      sprintf(
        "Unknown workflow setting stage(s): %s.",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  reserved <- nomo_run_reserved_settings()

  for (stage in nm) {
    x <- settings[[stage]]
    if (!is.list(x)) {
      stop(
        sprintf("`settings$%s` must be a list.", stage),
        call. = FALSE
      )
    }

    if (length(x) &&
        (is.null(names(x)) ||
         anyNA(names(x)) ||
         any(!nzchar(names(x))) ||
         anyDuplicated(names(x)))) {
      stop(
        sprintf("`settings$%s` must contain uniquely named arguments.", stage),
        call. = FALSE
      )
    }

    conflict <- intersect(names(x), reserved[[stage]])
    if (length(conflict)) {
      stop(
        sprintf(
          paste(
            "`settings$%s` cannot override pipeline-controlled argument(s): %s."
          ),
          stage,
          paste(conflict, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  all_items <- unique(unlist(scales, use.names = FALSE))
  for (stage in intersect(c("factors", "efa"), nm)) {
    types <- settings[[stage]]$types
    if (is.null(types)) next

    if (!is.character(types) ||
        is.null(names(types)) ||
        anyNA(names(types)) ||
        any(!nzchar(names(types))) ||
        anyDuplicated(names(types))) {
      stop(
        sprintf(
          "`settings$%s$types` must be a named character vector when supplied.",
          stage
        ),
        call. = FALSE
      )
    }

    bad_types <- setdiff(names(types), all_items)
    if (length(bad_types)) {
      stop(
        sprintf(
          "`settings$%s$types` names item(s) outside `scales`: %s.",
          stage,
          paste(bad_types, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if ("invariance" %in% nm && length(settings$invariance)) {
    group <- settings$invariance$group
    if (is.null(group) ||
        !is.character(group) ||
        length(group) != 1L ||
        is.na(group) ||
        !nzchar(trimws(group))) {
      stop(
        paste(
          "A requested invariance branch requires one grouping variable in",
          "`settings$invariance$group`."
        ),
        call. = FALSE
      )
    }
  }

  if ("network" %in% nm && length(settings$network)) {
    hypotheses <- settings$network$hypotheses
    if (is.null(hypotheses) || !inherits(hypotheses, "nomo_hypotheses")) {
      stop(
        paste(
          "A requested network branch requires `settings$network$hypotheses`",
          "created by `nomo_hypotheses()`."
        ),
        call. = FALSE
      )
    }
  }

  settings
}


nomo_run_validate_decisions <- function(decisions) {
  if (!is.list(decisions)) {
    stop("`decisions` must be a named list.", call. = FALSE)
  }

  if (!length(decisions)) return(decisions)

  nm <- names(decisions)
  if (is.null(nm) ||
      anyNA(nm) ||
      any(!nzchar(nm)) ||
      anyDuplicated(nm)) {
    stop("`decisions` must use unique non-empty names.", call. = FALSE)
  }

  allowed <- c("factor_count", "cfa_model", "measurement_model")
  bad <- setdiff(nm, allowed)
  if (length(bad)) {
    stop(
      sprintf(
        "Unsupported workflow decision name(s): %s.",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  decisions
}


nomo_run_merge_future_settings <- function(x, settings) {
  if (!length(settings)) return(x)

  settings <- nomo_run_validate_settings(settings, x$scales)

  for (stage in names(settings)) {
    current <- x$settings[[stage]]
    new <- settings[[stage]]
    stage_status <- x$stage_status$status[
      x$stage_status$stage == stage
    ][[1L]]

    locked <- stage_status %in% c("completed", "blocked")
    changed <- !identical(current, new)

    if (locked && changed) {
      stop(
        sprintf(
          paste(
            "Settings for completed/blocked stage `%s` cannot be changed while",
            "resuming. Start a new `nomo_run()` to change them."
          ),
          stage
        ),
        call. = FALSE
      )
    }

    if (!locked) {
      x$settings[[stage]] <- new
    }
  }

  x
}


nomo_run_scope_settings <- function(settings, stage, items, scales) {
  out <- settings[[stage]]
  if (is.null(out)) out <- list()

  if (!is.null(out$types)) {
    all_items <- unique(unlist(scales, use.names = FALSE))
    bad <- setdiff(names(out$types), all_items)
    if (length(bad)) {
      stop(
        sprintf(
          "`settings$%s$types` contains unknown pipeline item(s): %s.",
          stage,
          paste(bad, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    scoped <- out$types[names(out$types) %in% items]
    if (length(scoped)) {
      out$types <- scoped
    } else {
      out$types <- NULL
    }
  }

  out
}


nomo_run_stage_settings <- function(settings, stage, remove = character()) {
  out <- settings[[stage]]
  if (is.null(out)) out <- list()
  if (length(remove)) out[remove] <- NULL
  out
}


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


nomo_run_unpack_decision <- function(x) {
  if (is.list(x) &&
      !is.null(names(x)) &&
      "value" %in% names(x)) {
    extra <- setdiff(names(x), c("value", "rationale"))
    if (length(extra)) {
      stop(
        sprintf(
          "A structured decision may contain only `value` and `rationale`; found: %s.",
          paste(extra, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    return(list(
      value = x$value,
      rationale = if (is.null(x$rationale)) "" else x$rationale
    ))
  }

  list(value = x, rationale = "")
}


nomo_run_factor_requests <- function(x) {
  scale_names <- names(x$scales)
  multiple <- length(scale_names) > 1L

  rows <- lapply(scale_names, function(scope) {
    fac <- x$results$factors[[scope]]
    primary <- suppressWarnings(as.integer(fac$parallel$n_factors)[1L])
    plausible <- suppressWarnings(
      as.integer(unlist(fac$plausible_factors, use.names = FALSE))
    )
    plausible <- sort(unique(plausible[is.finite(plausible) & plausible >= 1L]))
    if (!length(plausible) && is.finite(primary)) plausible <- primary

    plausible_text <- if (length(plausible)) {
      paste(plausible, collapse = ", ")
    } else {
      "no compact plausible set was available"
    }

    factor_word <- if (is.finite(primary) && primary == 1L) "factor" else "factors"
    primary_text <- if (is.finite(primary)) as.character(primary) else "no usable number of"

    example <- if (multiple) {
      paste0(
        "decisions = list(factor_count = c(",
        paste(sprintf("%s = <integer>", scale_names), collapse = ", "),
        "))"
      )
    } else {
      sprintf(
        "decisions = list(factor_count = %dL)",
        if (is.finite(primary)) primary else 1L
      )
    }

    tibble::tibble(
      id = paste0("factor_count:", scope),
      stage = "efa",
      scope = scope,
      observation = sprintf(
        paste0(
          "Parallel analysis currently suggests %s %s; ",
          "the retained plausible set is %s. The pipeline has not adopted a factor count."
        ),
        primary_text,
        factor_word,
        plausible_text
      ),
      reason = paste(
        "The EFA factor count changes the fitted model. Retention evidence can",
        "inform that choice, but it does not authorize the pipeline to choose",
        "for the researcher."
      ),
      options = paste(
        "Inspect the full `nomo_factors` result, compare plausible neighboring",
        "solutions when appropriate, and supply a positive integer for this scale."
      ),
      consequence = paste(
        "No EFA is fitted until an explicit researcher factor-count decision",
        "is supplied."
      ),
      example = example
    )
  })

  dplyr::bind_rows(rows)
}


nomo_run_normalize_factor_counts <- function(value, scales) {
  scale_names <- names(scales)

  if (!is.numeric(value) ||
      !length(value) ||
      anyNA(value) ||
      any(!is.finite(value))) {
    stop(
      "`factor_count` must be a positive integer for each scale.",
      call. = FALSE
    )
  }

  if (length(scale_names) == 1L &&
      length(value) == 1L &&
      (is.null(names(value)) || !nzchar(names(value)[[1L]]))) {
    names(value) <- scale_names
  } else {
    if (is.null(names(value)) ||
        anyNA(names(value)) ||
        any(!nzchar(names(value))) ||
        anyDuplicated(names(value))) {
      stop(
        paste(
          "For multiple scales, `factor_count` must be a uniquely named numeric",
          "vector."
        ),
        call. = FALSE
      )
    }

    missing <- setdiff(scale_names, names(value))
    extra <- setdiff(names(value), scale_names)
    if (length(missing) || length(extra)) {
      stop(
        sprintf(
          "`factor_count` names must match `scales`. Missing: %s. Extra: %s.",
          if (length(missing)) paste(missing, collapse = ", ") else "none",
          if (length(extra)) paste(extra, collapse = ", ") else "none"
        ),
        call. = FALSE
      )
    }

    value <- value[scale_names]
  }

  whole <- abs(value - round(value)) <= sqrt(.Machine$double.eps)
  if (any(!whole) || any(value < 1)) {
    stop(
      "Every `factor_count` value must be a positive integer.",
      call. = FALSE
    )
  }

  counts <- as.integer(round(value))
  names(counts) <- scale_names

  for (nm in scale_names) {
    if (counts[[nm]] >= length(scales[[nm]])) {
      stop(
        sprintf(
          "Factor count for `%s` (%d) must be smaller than its %d supplied items.",
          nm,
          counts[[nm]],
          length(scales[[nm]])
        ),
        call. = FALSE
      )
    }
  }

  counts
}


nomo_run_normalize_rationale <- function(rationale, scopes) {
  if (is.null(rationale) || !length(rationale)) {
    return(stats::setNames(rep("", length(scopes)), scopes))
  }

  if (!is.character(rationale) || anyNA(rationale)) {
    stop("Decision `rationale` must be character text.", call. = FALSE)
  }

  if (length(rationale) == 1L &&
      (is.null(names(rationale)) || !nzchar(names(rationale)[[1L]]))) {
    return(stats::setNames(rep(rationale, length(scopes)), scopes))
  }

  if (is.null(names(rationale)) ||
      any(!nzchar(names(rationale))) ||
      anyDuplicated(names(rationale)) ||
      !setequal(names(rationale), scopes)) {
    stop(
      "A multi-scope `rationale` must be a named character vector matching the scopes.",
      call. = FALSE
    )
  }

  rationale[scopes]
}


nomo_run_apply_factor_decision <- function(x, decision) {
  unpacked <- nomo_run_unpack_decision(decision)
  counts <- nomo_run_normalize_factor_counts(unpacked$value, x$scales)
  rationale <- nomo_run_normalize_rationale(
    unpacked$rationale,
    names(x$scales)
  )

  x$decisions$factor_count <- list(
    value = counts,
    rationale = rationale
  )

  for (scope in names(x$scales)) {
    req <- x$decision_requests[
      x$decision_requests$id == paste0("factor_count:", scope),
      ,
      drop = FALSE
    ]

    x$decision_log <- nomo_run_workflow_log_add(
      x$decision_log,
      id = paste0("factor_count:", scope),
      stage = "efa",
      scope = scope,
      observation = if (nrow(req)) req$observation[[1L]] else "",
      reason = if (nrow(req)) req$reason[[1L]] else "",
      options = if (nrow(req)) req$options[[1L]] else "",
      consequence = sprintf(
        paste(
          "A %d-factor EFA will be fitted for `%s`; no item is removed",
          "automatically."
        ),
        counts[[scope]],
        scope
      ),
      decision = as.character(counts[[scope]]),
      rationale = rationale[[scope]],
      source = "researcher_decision"
    )
  }

  x$decision_requests <- nomo_run_empty_requests()
  roles <- nomo_run_data_roles(x$source_data)

  for (scope in names(x$scales)) {
    extra <- nomo_run_scope_settings(
      settings = x$settings,
      stage = "efa",
      items = x$scales[[scope]],
      scales = x$scales
    )

    result <- nomo_run_safe_component(
      fun = nomo_efa,
      fixed = list(
        data = roles$exploratory,
        items = x$scales[[scope]],
        factors = x$results$factors[[scope]],
        factor_count = counts[[scope]],
        guidance = x$guidance
      ),
      extra = extra,
      stage = "efa"
    )

    if (!result$ok) {
      return(nomo_run_block(x, "efa", scope, result))
    }

    x$results$efa[[scope]] <- result$value
  }

  x <- nomo_run_set_stage(
    x,
    "efa",
    "completed",
    paste(
      "EFA fitted using explicit researcher factor-count decisions;",
      "no items were removed automatically."
    )
  )

  x$status <- "paused"
  x$next_stage <- "cfa"
  x$blocked <- NULL

  x <- nomo_run_set_stage(
    x,
    "cfa",
    "awaiting_decision",
    "A researcher-specified confirmatory measurement model is required."
  )

  consequence <- if (identical(roles$design, "calibration_validation")) {
    paste(
      "The CFA will use the reserved validation subset.",
      "The pipeline will not derive or respecify the CFA model automatically",
      "from EFA results."
    )
  } else {
    paste(
      "The CFA will use the same sample unless a new workflow is started with",
      "a holdout/external design. Same-sample confirmation must remain labeled",
      "as such."
    )
  }

  x$decision_requests <- tibble::tibble(
    id = "cfa_model",
    stage = "cfa",
    scope = "measurement_model",
    observation = paste(
      "EFA has completed for every supplied scale.",
      "No confirmatory measurement model has been constructed or fitted."
    ),
    reason = paste(
      "CFA syntax encodes consequential choices about item retention, factor",
      "membership, cross-loadings, and correlated residuals; these cannot be",
      "chosen silently."
    ),
    options = paste(
      "Inspect the EFA evidence and theory, then supply a prespecified lavaan",
      "measurement model (or `nomo_model()` object)."
    ),
    consequence = consequence,
    example = paste(
      "decisions = list(cfa_model = list(value = model,",
      "rationale = \"Prespecified measurement model\"))"
    )
  )

  x
}


nomo_run_normalize_cfa_model <- function(value) {
  if (inherits(value, "nomo_model")) value <- as.character(value)

  if (!is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))) {
    stop(
      paste(
        "`cfa_model` must be one non-empty lavaan measurement-model string or",
        "an object created by `nomo_model()`."
      ),
      call. = FALSE
    )
  }

  trimws(value)
}


nomo_run_measurement_request <- function(x) {
  logs <- dplyr::bind_rows(
    lapply(
      list(
        cfa = x$results$cfa,
        reliability = x$results$reliability,
        validity = x$results$validity
      ),
      function(obj) {
        if (is.null(obj$decision_log)) return(tibble::tibble())
        tibble::as_tibble(obj$decision_log)
      }
    ),
    .id = "component"
  )

  concern_n <- if (nrow(logs) && "severity" %in% names(logs)) {
    sum(logs$severity == "concern", na.rm = TRUE)
  } else {
    0L
  }

  review_n <- if (nrow(logs) && "severity" %in% names(logs)) {
    sum(logs$severity == "review", na.rm = TRUE)
  } else {
    0L
  }

  heywood <- isTRUE(x$results$cfa$heywood_detected)

  observation <- sprintf(
    paste(
      "CFA converged and reliability/validity evidence was computed.",
      "Across these components, %d concern and %d review log entries are retained%s."
    ),
    concern_n,
    review_n,
    if (heywood) "; a CFA improper-solution/Heywood flag is also present" else ""
  )

  tibble::tibble(
    id = "measurement_model",
    stage = "measurement_review",
    scope = "measurement_model",
    observation = observation,
    reason = paste(
      "Invariance and nomological-network interpretations inherit the",
      "measurement model. Continuing downstream is therefore a researcher",
      "decision, not a fit-index side effect."
    ),
    options = paste(
      "Choose `proceed` to retain this prespecified model for configured",
      "downstream branches, or choose `revise` to stop here and start a new",
      "workflow with a substantively justified revised model."
    ),
    consequence = paste(
      "Proceeding does not declare the model valid and does not remove any",
      "review flags. Revising triggers no automatic parameter freeing, item",
      "deletion, or respecification."
    ),
    example = paste(
      "decisions = list(measurement_model = list(value = \"proceed\",",
      "rationale = \"Evidence reviewed; model retained for the planned analyses.\"))"
    )
  )
}


nomo_run_apply_cfa_model <- function(x, decision) {
  unpacked <- nomo_run_unpack_decision(decision)
  model <- nomo_run_normalize_cfa_model(unpacked$value)

  if (!is.character(unpacked$rationale) ||
      length(unpacked$rationale) > 1L ||
      anyNA(unpacked$rationale)) {
    stop("`cfa_model` rationale must be one character value.", call. = FALSE)
  }
  rationale <- if (length(unpacked$rationale)) unpacked$rationale else ""

  req <- x$decision_requests[
    x$decision_requests$id == "cfa_model",
    ,
    drop = FALSE
  ]

  x$decisions$cfa_model <- list(
    value = model,
    rationale = rationale
  )

  x$decision_log <- nomo_run_workflow_log_add(
    x$decision_log,
    id = "cfa_model",
    stage = "cfa",
    scope = "measurement_model",
    observation = if (nrow(req)) req$observation[[1L]] else "",
    reason = if (nrow(req)) req$reason[[1L]] else "",
    options = if (nrow(req)) req$options[[1L]] else "",
    consequence = if (nrow(req)) req$consequence[[1L]] else "",
    decision = model,
    rationale = rationale,
    source = "researcher_decision"
  )

  x$decision_requests <- nomo_run_empty_requests()
  roles <- nomo_run_data_roles(x$source_data)

  cfa_result <- nomo_run_safe_component(
    fun = nomo_cfa,
    fixed = list(
      model = model,
      data = roles$confirmatory,
      guidance = x$guidance
    ),
    extra = nomo_run_stage_settings(x$settings, "cfa"),
    stage = "cfa"
  )

  if (!cfa_result$ok) {
    return(nomo_run_block(x, "cfa", "measurement_model", cfa_result))
  }

  x$results$cfa <- cfa_result$value

  if (!isTRUE(x$results$cfa$converged)) {
    return(
      nomo_run_block_error(
        x,
        "cfa",
        "measurement_model",
        paste(
          "The CFA object was retained for diagnosis, but the fitted model did",
          "not converge. Reliability, validity, invariance, and network stages",
          "were not run."
        )
      )
    )
  }

  x <- nomo_run_set_stage(
    x,
    "cfa",
    "completed",
    paste(
      "Researcher-specified CFA estimated; the model was not respecified",
      "automatically."
    )
  )

  rel_result <- nomo_run_safe_component(
    fun = nomo_reliability,
    fixed = list(
      fit = x$results$cfa,
      guidance = x$guidance
    ),
    extra = nomo_run_stage_settings(x$settings, "reliability"),
    stage = "reliability"
  )

  if (!rel_result$ok) {
    return(nomo_run_block(x, "reliability", "measurement_model", rel_result))
  }

  x$results$reliability <- rel_result$value
  x <- nomo_run_set_stage(
    x,
    "reliability",
    "completed",
    "Reliability evidence computed from the retained CFA model."
  )

  val_result <- nomo_run_safe_component(
    fun = nomo_validity,
    fixed = list(
      fit = x$results$cfa,
      guidance = x$guidance
    ),
    extra = nomo_run_stage_settings(x$settings, "validity"),
    stage = "validity"
  )

  if (!val_result$ok) {
    return(nomo_run_block(x, "validity", "measurement_model", val_result))
  }

  x$results$validity <- val_result$value
  x <- nomo_run_set_stage(
    x,
    "validity",
    "completed",
    paste(
      "Convergent/discriminant evidence computed; no valid/invalid verdict",
      "was created."
    )
  )

  x$status <- "paused"
  x$next_stage <- "measurement_review"
  x$blocked <- NULL
  x$decision_requests <- nomo_run_measurement_request(x)

  x
}


nomo_run_normalize_measurement_decision <- function(value) {
  if (!is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))) {
    stop(
      "`measurement_model` must be either `\"proceed\"` or `\"revise\"`.",
      call. = FALSE
    )
  }

  value <- tolower(trimws(value))
  if (!value %in% c("proceed", "revise")) {
    stop(
      "`measurement_model` must be either `\"proceed\"` or `\"revise\"`.",
      call. = FALSE
    )
  }

  value
}


nomo_run_branch_requested <- function(x, stage) {
  stage %in% names(x$settings) && length(x$settings[[stage]]) > 0L
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


nomo_run_apply_measurement_decision <- function(x, decision) {
  unpacked <- nomo_run_unpack_decision(decision)
  value <- nomo_run_normalize_measurement_decision(unpacked$value)

  if (!is.character(unpacked$rationale) ||
      length(unpacked$rationale) > 1L ||
      anyNA(unpacked$rationale)) {
    stop("`measurement_model` rationale must be one character value.", call. = FALSE)
  }
  rationale <- if (length(unpacked$rationale)) unpacked$rationale else ""

  req <- x$decision_requests[
    x$decision_requests$id == "measurement_model",
    ,
    drop = FALSE
  ]

  x$decisions$measurement_model <- list(
    value = value,
    rationale = rationale
  )

  x$decision_log <- nomo_run_workflow_log_add(
    x$decision_log,
    id = "measurement_model",
    stage = "measurement_review",
    scope = "measurement_model",
    observation = if (nrow(req)) req$observation[[1L]] else "",
    reason = if (nrow(req)) req$reason[[1L]] else "",
    options = if (nrow(req)) req$options[[1L]] else "",
    consequence = if (nrow(req)) req$consequence[[1L]] else "",
    decision = value,
    rationale = rationale,
    source = "researcher_decision"
  )

  x$decision_requests <- nomo_run_empty_requests()

  if (identical(value, "revise")) {
    x$status <- "paused"
    x$next_stage <- "restart"
    x$decision_requests <- tibble::tibble(
      id = "restart_with_revised_model",
      stage = "cfa",
      scope = "measurement_model",
      observation = paste(
        "The researcher chose not to carry the fitted measurement model",
        "forward."
      ),
      reason = paste(
        "Changing the CFA model would invalidate the current downstream",
        "provenance if the pipeline silently refit in place."
      ),
      options = paste(
        "Start a new `nomo_run()` with a revised, substantively justified CFA",
        "model decision. The current object remains an auditable record of this run."
      ),
      consequence = paste(
        "No invariance or nomological-network analysis was run and no model",
        "revision was performed automatically."
      ),
      example = "Start a new workflow and retain this object as the prior analysis record."
    )
    return(x)
  }

  nomo_run_finish_downstream(x)
}


nomo_run_process_decisions <- function(x, decisions) {
  decisions <- nomo_run_validate_decisions(decisions)
  if (!length(decisions)) return(x)

  remaining <- decisions

  repeat {
    consumed <- FALSE

    if (identical(x$next_stage, "efa") &&
        "factor_count" %in% names(remaining)) {
      x <- nomo_run_apply_factor_decision(
        x,
        remaining$factor_count
      )
      remaining$factor_count <- NULL
      consumed <- TRUE
    } else if (identical(x$next_stage, "cfa") &&
               "cfa_model" %in% names(remaining)) {
      x <- nomo_run_apply_cfa_model(
        x,
        remaining$cfa_model
      )
      remaining$cfa_model <- NULL
      consumed <- TRUE
    } else if (identical(x$next_stage, "measurement_review") &&
               "measurement_model" %in% names(remaining)) {
      x <- nomo_run_apply_measurement_decision(
        x,
        remaining$measurement_model
      )
      remaining$measurement_model <- NULL
      consumed <- TRUE
    }

    if (!consumed || !length(remaining) || identical(x$status, "blocked")) {
      break
    }
  }

  if (identical(x$status, "blocked")) {
    return(x)
  }

  if (length(remaining)) {
    stop(
      sprintf(
        paste(
          "Decision(s) could not be consumed at the current workflow state",
          "`%s`: %s."
        ),
        if (is.null(x$next_stage)) "complete" else x$next_stage,
        paste(names(remaining), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  x
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


nomo_run_fresh <- function(data,
                           scales,
                           mode,
                           guidance,
                           decisions,
                           settings,
                           call) {
  roles <- nomo_run_data_roles(data)

  if (!is.list(guidance)) {
    stop(
      "`guidance` must be a list, typically returned by `nomo_defaults()`.",
      call. = FALSE
    )
  }

  scales <- nomo_run_validate_scales(scales, roles)
  settings <- nomo_run_validate_settings(settings, scales)
  decisions <- nomo_run_validate_decisions(decisions)

  x <- list(
    call = call,
    call_history = list(call),
    mode = mode,
    status = "running",
    next_stage = "screen",
    sample_design = roles$design,
    sample_source = roles$source,
    sample_n = tibble::tibble(
      role = c("exploratory", "confirmatory"),
      n = c(roles$n_exploratory, roles$n_confirmatory)
    ),
    scales = scales,
    guidance = guidance,
    settings = settings,
    decisions = list(),
    results = list(
      screen = list(),
      factors = list(),
      efa = list(),
      cfa = NULL,
      reliability = NULL,
      validity = NULL,
      invariance = NULL,
      network = NULL
    ),
    stage_status = nomo_run_stage_status_new(),
    decision_requests = nomo_run_empty_requests(),
    decision_log = nomo_run_initial_log(scales, roles),
    blocked = NULL,
    source_data = data,
    state_version = 2L
  )
  class(x) <- c("nomo_run", "list")

  for (scope in names(scales)) {
    extra <- nomo_run_scope_settings(
      settings = settings,
      stage = "screen",
      items = scales[[scope]],
      scales = scales
    )

    result <- nomo_run_safe_component(
      fun = nomo_screen,
      fixed = list(
        data = roles$exploratory,
        items = scales[[scope]],
        guidance = guidance
      ),
      extra = extra,
      stage = "screen"
    )

    if (!result$ok) {
      return(nomo_run_block(x, "screen", scope, result))
    }

    x$results$screen[[scope]] <- result$value
  }

  x <- nomo_run_set_stage(
    x,
    "screen",
    "completed",
    paste(
      "Candidate items audited exactly as supplied; no data or item membership",
      "changed."
    )
  )

  for (scope in names(scales)) {
    extra <- nomo_run_scope_settings(
      settings = settings,
      stage = "factors",
      items = scales[[scope]],
      scales = scales
    )

    result <- nomo_run_safe_component(
      fun = nomo_factors,
      fixed = list(
        data = roles$exploratory,
        items = scales[[scope]],
        guidance = guidance
      ),
      extra = extra,
      stage = "factors"
    )

    if (!result$ok) {
      return(nomo_run_block(x, "factors", scope, result))
    }

    x$results$factors[[scope]] <- result$value
  }

  x <- nomo_run_set_stage(
    x,
    "factors",
    "completed",
    paste(
      "Factor-retention evidence computed; no factor count was adopted",
      "automatically."
    )
  )

  x$status <- "paused"
  x$next_stage <- "efa"
  x <- nomo_run_set_stage(
    x,
    "efa",
    "awaiting_decision",
    "Explicit researcher factor-count decisions are required before EFA."
  )
  x$decision_requests <- nomo_run_factor_requests(x)

  nomo_run_process_decisions(x, decisions)
}


nomo_run_resume <- function(resume,
                            data,
                            scales,
                            mode,
                            guidance,
                            settings,
                            decisions,
                            call,
                            mode_missing,
                            guidance_missing,
                            settings_missing) {
  if (!inherits(resume, "nomo_run")) {
    stop("`resume` must be a `nomo_run` object.", call. = FALSE)
  }

  if (!is.null(data) || !is.null(scales)) {
    stop(
      paste(
        "Do not supply new `data` or `scales` when resuming. Start a new",
        "`nomo_run()` to change them."
      ),
      call. = FALSE
    )
  }

  decisions <- nomo_run_validate_decisions(decisions)

  x <- resume
  x$call <- call
  x$call_history <- c(x$call_history, list(call))

  if (!mode_missing) x$mode <- mode

  if (!guidance_missing && !identical(guidance, x$guidance)) {
    stop(
      paste(
        "Guidance cannot be changed mid-workflow; start a new `nomo_run()`",
        "to change it."
      ),
      call. = FALSE
    )
  }

  if (!settings_missing) {
    x <- nomo_run_merge_future_settings(x, settings)
  }

  if (identical(x$status, "blocked")) {
    stop(
      paste(
        "This workflow is blocked by an earlier component error.",
        "Correct the input/model/settings and start a new `nomo_run()` rather",
        "than silently continuing."
      ),
      call. = FALSE
    )
  }

  if (identical(x$status, "complete") && length(decisions)) {
    stop(
      "The workflow is already complete; start a new `nomo_run()` to change decisions.",
      call. = FALSE
    )
  }

  nomo_run_process_decisions(x, decisions)
}


#' Run the guided nomologR workflow
#'
#' `nomo_run()` coordinates the package's staged evidence workflow while
#' stopping at consequential researcher decisions. Evidence can be computed
#' automatically when doing so does not change the researcher's data or model,
#' but `nomo_run()` does not silently select factor counts, remove items,
#' construct a CFA model, free parameters, or respecify a model.
#'
#' The guided workflow is resumable. Completed component objects are retained
#' rather than recomputed. Future-stage settings may be added or revised until
#' that stage has completed; settings for completed/blocked stages are locked.
#'
#' Consequential decisions are currently:
#'
#' 1. `factor_count`: the EFA factor count for each named scale;
#' 2. `cfa_model`: the prespecified confirmatory measurement model;
#' 3. `measurement_model`: `"proceed"` or `"revise"` after reviewing CFA,
#'    reliability, and convergent/discriminant evidence.
#'
#' Optional invariance and nomological-network branches are requested explicitly
#' through `settings$invariance` and `settings$network`. A network branch must
#' include a `nomo_hypotheses` object. Invariance diagnostics never free
#' parameters automatically, and network results never collapse to a one-number
#' validity score.
#'
#' @param data A non-empty data frame or a [nomo_split()] object. A split uses
#'   calibration rows for exploratory stages and validation rows for CFA,
#'   reliability, validity, and invariance. A network branch receives the split
#'   object so the same prespecified network can be evaluated across calibration
#'   and validation samples.
#' @param scales A non-empty named list. Each element is a character vector of
#'   candidate item-column names for one scale/construct.
#' @param mode Presentation mode: `"teaching"` or `"research"`. Mode changes
#'   presentation, not statistical behavior.
#' @param guidance Guidance settings from [nomo_defaults()].
#' @param decisions Named list of explicit researcher decisions. Decisions may
#'   be supplied one pause at a time or all at once for a fully prespecified
#'   one-call run. A structured decision can be written as
#'   `list(value = ..., rationale = "...")`.
#' @param settings Named list of stage-specific argument lists. Examples include
#'   `list(factors = list(n_iter = 200, seed = 2026))`,
#'   `list(invariance = list(group = "group"))`, or
#'   `list(network = list(hypotheses = h))`. Pipeline-controlled arguments such
#'   as component data/model inputs cannot be overridden through `settings`.
#'   When resuming, settings for future stages may be supplied without
#'   recomputing completed stages.
#' @param resume Optional prior `nomo_run` object. When supplied, the existing
#'   source data, scales, guidance, completed component results, decisions, and
#'   provenance are reused.
#'
#' @return A `nomo_run` object containing component results, stage status,
#'   outstanding decision requests, researcher decisions/rationales, stage
#'   settings, workflow provenance, component decision logs, and source inputs
#'   required for reproducibility.
#'
#' @examples
#' \dontrun{
#' h <- nomo_hypotheses(
#'   "WellBeing -> criterion" = positive(min = .20)
#' )
#'
#' run <- nomo_run(
#'   data = dat,
#'   scales = list(WellBeing = c("w1", "w2", "w3", "w4")),
#'   settings = list(
#'     factors = list(seed = 2026),
#'     network = list(hypotheses = h)
#'   )
#' )
#'
#' run <- nomo_run(
#'   resume = run,
#'   decisions = list(factor_count = 1L)
#' )
#'
#' run <- nomo_run(
#'   resume = run,
#'   decisions = list(
#'     cfa_model = list(
#'       value = "WellBeing =~ w1 + w2 + w3 + w4",
#'       rationale = "Prespecified one-factor measurement model."
#'     )
#'   )
#' )
#'
#' run <- nomo_run(
#'   resume = run,
#'   decisions = list(
#'     measurement_model = list(
#'       value = "proceed",
#'       rationale = "Measurement evidence reviewed before downstream analyses."
#'     )
#'   )
#' )
#' }
#'
#' @export
nomo_run <- function(data = NULL,
                     scales = NULL,
                     mode = c("teaching", "research"),
                     guidance = nomo_defaults(),
                     decisions = list(),
                     settings = list(),
                     resume = NULL) {
  mode_missing <- missing(mode)
  guidance_missing <- missing(guidance)
  settings_missing <- missing(settings)

  if (!mode_missing) {
    mode <- match.arg(mode)
  } else if (is.null(resume)) {
    mode <- "teaching"
  }

  call <- match.call()

  if (!is.null(resume)) {
    return(
      nomo_run_resume(
        resume = resume,
        data = data,
        scales = scales,
        mode = if (mode_missing) resume$mode else mode,
        guidance = if (guidance_missing) resume$guidance else guidance,
        settings = if (settings_missing) list() else settings,
        decisions = decisions,
        call = call,
        mode_missing = mode_missing,
        guidance_missing = guidance_missing,
        settings_missing = settings_missing
      )
    )
  }

  mode <- match.arg(mode)

  nomo_run_fresh(
    data = data,
    scales = scales,
    mode = mode,
    guidance = guidance,
    decisions = decisions,
    settings = settings,
    call = call
  )
}
