# Guided workflow: state and input validation ----------------------------------

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


# Settings for evidence attached to a stage rather than being a stage of its
# own (#73): scores and missing-data sensitivity follow the CFA, and the latter
# the network too. They are opt-in and never appear in the stage table, so a
# run that does not request them has exactly the shape it always had.
nomo_run_attached_settings <- function() c("scores", "missing")


nomo_run_reserved_settings <- function() {
  list(
    scores = c("fit", "guidance"),
    missing = c("x", "data"),
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

  bad <- setdiff(nm, c(nomo_run_stage_order(), nomo_run_attached_settings()))
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

  # Checked here, before any stage runs, since an unreadable value would
  # otherwise surface only after every per-scale screen had been computed.
  effort <- settings$screen$effort
  if (!is.null(effort) && (!is.logical(effort) || length(effort) != 1L || is.na(effort))) {
    stop("`settings$screen$effort` must be TRUE or FALSE.", call. = FALSE)
  }

  if ("scores" %in% nm) {
    method <- settings$scores$method
    methods <- c("sum", "mean", "regression", "bartlett")
    if (is.null(method) || !is.character(method) || length(method) != 1L ||
          !method %in% methods) {
      stop(
        paste0(
          "Requested scores need `settings$scores$method`, one of ",
          paste0("\"", methods, "\"", collapse = ", "),
          ". nomologR does not choose a scoring method."
        ),
        call. = FALSE
      )
    }
  }

  if ("missing" %in% nm) {
    strategies <- settings$missing$strategies
    if (!is.null(strategies) && (!is.character(strategies) || !length(strategies) ||
                                   anyNA(strategies))) {
      stop("`settings$missing$strategies` must be a character vector.", call. = FALSE)
    }
    reliability <- settings$missing$reliability
    if (!is.null(reliability) &&
          (!is.logical(reliability) || length(reliability) != 1L || is.na(reliability))) {
      stop("`settings$missing$reliability` must be TRUE or FALSE.", call. = FALSE)
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

    # Attached evidence has no stage status; it is locked once computed.
    locked <- if (stage %in% nomo_run_attached_settings()) {
      !is.null(x$results[[stage]])
    } else {
      x$stage_status$status[x$stage_status$stage == stage][[1L]] %in%
        c("completed", "blocked")
    }
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
