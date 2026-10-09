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
    # A suggestion of 0 is not a count to retain (#145).
    zero <- is.finite(primary) && primary < 1L
    if (!length(plausible) && is.finite(primary) && !zero) plausible <- primary

    plausible_text <- if (length(plausible)) {
      paste(plausible, collapse = ", ")
    } else {
      "no compact plausible set was available"
    }

    factor_word <- if (is.finite(primary) && primary == 1L) "factor" else "factors"
    primary_text <- if (is.finite(primary)) as.character(primary) else "no usable number of"
    observation <- if (zero) {
      # The wording of nomo_efa(), which fits a count given in `factor_count`
      # when parallel analysis suggested 0 (#145, efa-2).
      paste0(
        "Parallel analysis suggests 0 factors: it found no factor above the null ",
        "reference, so there is no count to adopt",
        if (length(plausible)) sprintf("; the retained plausible set is %s", plausible_text) else "",
        ". To fit an EFA anyway, give the count in `factor_count`, with the ",
        "substantive reason as its rationale. The pipeline has not adopted a factor count."
      )
    } else {
      sprintf(
        paste0(
          "Parallel analysis currently suggests %s %s; ",
          "the retained plausible set is %s. The pipeline has not adopted a factor count."
        ),
        primary_text,
        factor_word,
        plausible_text
      )
    }

    example <- if (multiple) {
      paste0(
        "decisions = list(factor_count = c(",
        paste(sprintf("%s = <integer>", scale_names), collapse = ", "),
        "))"
      )
    } else {
      # A suggestion of 0 factors is not a count nomo_efa() can fit (#145,
      # efa-2), so the example then shows 1.
      sprintf(
        "decisions = list(factor_count = %dL)",
        if (is.finite(primary) && primary >= 1L) primary else 1L
      )
    }

    tibble::tibble(
      id = paste0("factor_count:", scope),
      stage = "efa",
      scope = scope,
      observation = observation,
      reason = paste(
        "The EFA factor count changes the fitted model. Retention evidence can",
        "inform that choice, but it does not authorize the pipeline to choose",
        "for the researcher."
      ),
      options = paste(
        "Inspect the full `nomo_factors` result, compare plausible neighboring",
        "solutions when appropriate, and supply a positive integer for each scale."
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


# The items the exploratory stages treated as categorical (#145): those
# modeled as binary or ordinal where nomo_factors() or nomo_efa() used
# tetrachoric, polychoric, or mixed correlations, with those methods. `only`
# keeps the items a measurement model contains, and the methods used for them.
nomo_run_categorical_items <- function(x, only = NULL) {
  items <- character()
  methods <- character()
  for (r in c(unname(x$results$factors), unname(x$results$efa))) {
    if (is.null(r$correlation) || identical(r$correlation, "pearson")) next
    types <- r$modeling_types
    found <- types$item[types$model_type %in% c("binary", "ordinal")]
    if (!is.null(only)) found <- intersect(found, only)
    if (!length(found)) next
    items <- c(items, found)
    methods <- c(methods, r$correlation)
  }
  list(items = unique(items), methods = unique(methods))
}


# A sentence for the CFA request when the exploratory stages treated items as
# categorical and the CFA, as configured, would treat them as continuous; ""
# otherwise. nomologR does not choose for the researcher. Before a model is
# given, every such item is named; `indicators`, the model's once it is, keeps
# those the CFA contains (#145).
nomo_run_item_type_note <- function(x, indicators = NULL) {
  categorical <- nomo_run_categorical_items(x, only = indicators)
  if (!length(categorical$items) || !is.null(x$settings$cfa$ordered)) return("")
  sprintf(
    paste(
      "The exploratory stages treated %s as categorical (%s correlations), while",
      "the CFA treats every indicator as continuous unless `settings$cfa$ordered`",
      "names it."
    ),
    paste(categorical$items, collapse = ", "),
    nomo_present_or(categorical$methods, "and")
  )
}


# The indicators and factors a measurement model names, or NULL when it cannot
# be parsed; the CFA then reports lavaan's own error.
nomo_run_model_names <- function(model) {
  pt <- tryCatch(suppressWarnings(lavaan::lavaanify(model)), error = function(e) NULL)
  if (is.null(pt)) return(NULL)
  list(indicators = lavaan::lavNames(pt, "ov.ind"), factors = lavaan::lavNames(pt, "lv"))
}


# The items of the measurement model compared with the screened scales (#145):
# screened items it leaves out, and indicators no scale supplied, which no
# item audit, retention, or EFA evidence covers. NULL when the model cannot be
# parsed.
nomo_run_model_item_set <- function(model, scales) {
  parsed <- nomo_run_model_names(model)
  if (is.null(parsed)) return(NULL)
  indicators <- parsed$indicators
  screened <- unique(unlist(scales, use.names = FALSE))
  list(omitted = setdiff(screened, indicators), added = setdiff(indicators, screened),
       indicators = indicators)
}


# Design-log rows recorded once the CFA has been fitted, so never for a model
# lavaan refused: a measurement model whose items differ from the screened
# scales, and items of the model that the exploratory stages treated as
# categorical while the CFA treats them as continuous (#145). Neither blocks
# the run.
nomo_run_cfa_model_log <- function(x, model, rationale) {
  set <- nomo_run_model_item_set(model, x$scales)
  if (!is.null(set) && (length(set$omitted) || length(set$added))) {
    parts <- c(
      if (length(set$omitted)) {
        sprintf("leaves out %s, which the earlier stages screened and explored",
                paste(set$omitted, collapse = ", "))
      },
      if (length(set$added)) {
        sprintf(
          "includes %s, which no supplied scale contains, so no item audit, retention, or EFA evidence covers %s",
          paste(set$added, collapse = ", "), nomo_present_noun(length(set$added), "it", "them")
        )
      }
    )
    x$decision_log <- nomo_run_workflow_log_add(
      x$decision_log,
      id = "cfa_item_set",
      stage = "cfa",
      scope = "measurement_model",
      observation = paste0("The measurement model ", paste(parts, collapse = ", and "), "."),
      reason = paste(
        "Screening, retention, and EFA evidence describe the supplied scales, while",
        "CFA, reliability, and validity evidence describe the measurement model, so",
        "a difference between the two item sets must stay visible."
      ),
      options = paste(
        "Report the difference and the reason for it, or start a new run whose",
        "scales match the measurement model."
      ),
      consequence = paste(
        "The difference does not block the run: leaving out or adding items at the",
        "CFA is the researcher's decision, and nomologR changes neither the model",
        "nor the scales."
      ),
      decision = paste(c(
        if (length(set$omitted)) paste("left out:", paste(set$omitted, collapse = ", ")),
        if (length(set$added)) paste("added:", paste(set$added, collapse = ", "))
      ), collapse = "; "),
      rationale = rationale,
      source = "researcher_decision"
    )
  }

  note <- nomo_run_item_type_note(x, indicators = set$indicators)
  if (nzchar(note)) {
    x$decision_log <- nomo_run_workflow_log_add(
      x$decision_log,
      id = "item_types",
      stage = "cfa",
      scope = "measurement_model",
      observation = note,
      reason = paste(
        "Normal-theory estimation of binary or few-category items attenuates",
        "loadings and distorts fit, so the exploratory and confirmatory evidence",
        "would rest on different assumptions about the same items."
      ),
      options = paste(
        "Start a new run that names them in `ordered` within `settings$cfa` to",
        "treat them as the exploratory stages did, or record why continuous",
        "treatment is defensible for these items."
      ),
      consequence = paste(
        "The CFA, reliability, and validity evidence treat as continuous items",
        "that the exploratory evidence treated as categorical."
      ),
      source = "pipeline"
    )
  }
  x
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

  item_types <- nomo_run_item_type_note(x)
  x$decision_requests <- tibble::tibble(
    id = "cfa_model",
    stage = "cfa",
    scope = "measurement_model",
    observation = trimws(paste(
      "EFA has completed for every supplied scale.",
      "No confirmatory measurement model has been constructed or fitted.",
      item_types
    )),
    reason = paste(
      "CFA syntax encodes consequential choices about item retention, factor",
      "membership, cross-loadings, and correlated residuals; these cannot be",
      "chosen silently."
    ),
    options = paste0(
      "Inspect the EFA evidence and theory, then supply a prespecified lavaan ",
      "measurement model (or `nomo_model()` object).",
      if (nzchar(item_types)) {
        paste(
          " To keep the CFA consistent with the exploratory evidence, name the",
          "categorical items in `ordered` within `settings$cfa` when supplying the",
          "model."
        )
      } else {
        ""
      }
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

  # The design-log rows the CFA added are named here too, since they bear on
  # whether to carry the model forward (#145).
  ids <- x$decision_log$id
  observation <- paste(c(
    sprintf(
      paste(
        "CFA converged and reliability and validity evidence was computed.",
        "Their logs hold %s and %s%s."
      ),
      nomo_present_count(concern_n, "concern entry", "concern entries"),
      nomo_present_count(review_n, "review entry", "review entries"),
      if (heywood) "; a CFA improper-solution (Heywood) flag is also present" else ""
    ),
    if ("cfa_item_set" %in% ids) {
      "The measurement model's items differ from the screened scales (decision-log row cfa_item_set)."
    },
    if ("item_types" %in% ids) {
      paste(
        "The CFA treats as continuous items the exploratory stages treated as",
        "categorical (decision-log row item_types)."
      )
    }
  ), collapse = " ")

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
      "downstream branches, or choose `revise` to stop here and continue with",
      "`nomo_revise()`, which records a substantively justified revised model",
      "and keeps this workflow as its parent."
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

  # Recorded for the model lavaan fitted, not for one it refused (#145).
  x <- nomo_run_cfa_model_log(x, model, rationale)
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

  # Requested scores and missing-data sensitivity are measurement evidence, so
  # they are shown before the researcher decides whether to proceed (#73).
  x <- nomo_run_run_scores(x)
  x <- nomo_run_run_missing(x, "cfa")

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
        "Use `nomo_revise()` to create a revised workflow that keeps this run",
        "as its parent and records what changed, or start a new `nomo_run()`.",
        "Either way, this object remains an auditable record of this run."
      ),
      consequence = paste(
        "No invariance or nomological-network analysis was run and no model",
        "revision was performed automatically. A revision records what changed,",
        "why, and whether the change was prespecified or post hoc."
      ),
      example = "nomo_revise(run, cfa_model = \"...\", rationale = \"...\", origin = \"post_hoc\")"
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
          "%s could not be consumed at the current workflow state",
          "`%s`: %s."
        ),
        nomo_present_noun(length(remaining), "A decision", "Decisions"),
        if (is.null(x$next_stage)) "complete" else x$next_stage,
        paste(names(remaining), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  x
}
