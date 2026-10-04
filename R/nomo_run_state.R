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
          "Scale `%s` contains %s unavailable in the workflow data: %s.",
          nm,
          nomo_present_noun(length(absent), "an item column", "item columns"),
          paste(absent, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    # Factor-retention evidence needs three items, so a shorter scale would
    # only block the run after its item audit (#145).
    if (length(items) < 3L) {
      stop(
        sprintf(
          paste(
            "Scale `%s` has %s; a guided run needs at least three items per",
            "scale, since `nomo_factors()` needs three candidate items."
          ),
          nm, nomo_present_count(length(items), "item")
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


# The shape of a settings list: unique, known stage names, each holding a list
# of uniquely named arguments. Checked on its own when resuming, before the
# given arguments are merged into those already set.
nomo_run_check_settings_shape <- function(settings) {
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
        "Unknown workflow setting %s: %s.",
        nomo_present_noun(length(bad), "stage", "stages"),
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

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
  }

  settings
}


# The lavaan `missing` options, with lavaan's aliases, that a missing-data
# comparison can request. A name outside them would otherwise be accepted and
# reported as a strategy compared (#145).
nomo_run_missing_options <- function() {
  c("listwise", "pairwise", "available.cases", "ml", "fiml", "direct", "ml.x",
    "fiml.x", "direct.x", "two.stage", "two.step", "robust.two.stage",
    "robust.two.step", "doubly.robust", "default")
}


# `roles`, when given, are the run's data roles, so a setting that names a
# column is checked against the data before any stage runs (#145).
nomo_run_validate_settings <- function(settings, scales, roles = NULL) {
  settings <- nomo_run_check_settings_shape(settings)
  if (!length(settings)) return(settings)

  nm <- names(settings)
  reserved <- nomo_run_reserved_settings()

  for (stage in nm) {
    conflict <- intersect(names(settings[[stage]]), reserved[[stage]])
    if (length(conflict)) {
      stop(
        sprintf(
          "`settings$%s` cannot override pipeline-controlled %s: %s.",
          stage,
          nomo_present_noun(length(conflict), "argument", "arguments"),
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
    unknown <- setdiff(strategies, nomo_run_missing_options())
    if (length(unknown)) {
      stop(
        sprintf(
          "`settings$missing$strategies` must name lavaan `missing` options, %s, not %s.",
          nomo_present_or(paste0("\"", nomo_run_missing_options(), "\"")),
          nomo_present_or(paste0("\"", unknown, "\""), "and")
        ),
        call. = FALSE
      )
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
    # A misspelled group would otherwise surface only after every stage and
    # every decision before it (#145).
    if (!is.null(roles) && !group %in% names(roles$confirmatory)) {
      stop(
        sprintf(
          paste(
            "`settings$invariance$group` is \"%s\", which is not a column of the",
            "data the confirmatory stages use."
          ),
          group
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
        "Unsupported workflow decision %s: %s.",
        nomo_present_noun(length(bad), "name", "names"),
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  decisions
}


# The status of one stage, or "" for a run without a stage table.
nomo_run_stage_state <- function(x, stage) {
  status <- x$stage_status$status[x$stage_status$stage == stage]
  if (length(status)) status[[1L]] else ""
}


# Why settings for `stage` can no longer change when a run is resumed, or NULL
# while they can. Settings stored after their stage's moment has passed would
# never run (#145): a stage that completed or blocked, a branch the completed
# run marked not requested, and scores or missing-data sensitivity once the
# CFA they run with has been fitted.
nomo_run_settings_lock <- function(x, stage) {
  if (stage %in% nomo_run_attached_settings()) {
    if (!is.null(x$results[[stage]]) ||
        nomo_run_stage_state(x, "cfa") %in% c("completed", "blocked")) {
      return("they run with the CFA, which has already been fitted, so they would never run")
    }
    return(NULL)
  }
  state <- nomo_run_stage_state(x, stage)
  if (state %in% c("completed", "blocked")) {
    return(if (state == "blocked") "the stage is blocked" else "the stage has already completed")
  }
  if (identical(state, "not_requested")) {
    return("the completed workflow marked the stage not requested, so they would never run")
  }
  NULL
}


# One setting as R code, for the decision log: `levels = "configural"`, and an
# object by its class, such as `hypotheses = <nomo_hypotheses>`.
nomo_run_settings_text <- function(setting) {
  if (!length(setting)) return("list()")
  paste(vapply(names(setting), function(arg) {
    value <- setting[[arg]]
    shown <- if (is.object(value)) {
      sprintf("<%s>", class(value)[[1L]])
    } else {
      paste(deparse(value, width.cutoff = 500L), collapse = " ")
    }
    sprintf("%s = %s", arg, shown)
  }, character(1)), collapse = ", ")
}


# Settings given when resuming are merged into those already set, argument by
# argument (#145): a named argument is added or replaced, and the others keep
# their values. A change is recorded in the decision log.
nomo_run_merge_future_settings <- function(x, settings) {
  if (!length(settings)) return(x)

  settings <- nomo_run_check_settings_shape(settings)
  merged <- x$settings
  for (stage in names(settings)) {
    current <- merged[[stage]]
    if (is.null(current)) current <- list()
    # Single brackets keep an argument given as NULL, which asks a component
    # for its default.
    current[names(settings[[stage]])] <- settings[[stage]]
    merged[[stage]] <- current
  }
  roles <- if (is.null(x$source_data)) NULL else nomo_run_data_roles(x$source_data)
  merged <- nomo_run_validate_settings(merged, x$scales, roles)

  for (stage in names(settings)) {
    current <- x$settings[[stage]]
    new <- merged[[stage]]
    if (identical(current, new)) next

    lock <- nomo_run_settings_lock(x, stage)
    if (!is.null(lock)) {
      stop(
        sprintf(
          "Settings for `%s` cannot be changed while resuming: %s. Start a new `nomo_run()` to change them.",
          stage, lock
        ),
        call. = FALSE
      )
    }

    x$settings[[stage]] <- new
    given <- settings[[stage]]
    x$decision_log <- nomo_run_workflow_log_add(
      x$decision_log,
      id = paste0("settings:", stage),
      stage = if (stage %in% nomo_run_attached_settings()) "cfa" else stage,
      scope = "settings",
      observation = sprintf(
        "The workflow was resumed with `settings$%s`: %s.",
        stage, nomo_run_settings_text(given)
      ),
      reason = paste(
        "Settings given when a workflow is resumed change how a later stage",
        "runs, so the change is recorded."
      ),
      options = paste(
        "Arguments given when resuming are added or replace earlier values;",
        "arguments not named keep theirs."
      ),
      consequence = sprintf("`settings$%s` is now: %s.", stage, nomo_run_settings_text(new)),
      decision = nomo_run_settings_text(new),
      source = "researcher_input"
    )
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
