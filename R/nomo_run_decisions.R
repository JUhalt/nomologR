# Guided workflow: researcher decisions ----------------------------------------

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
