# Guided workflow presentation -------------------------------------------------

nomo_run_scale_table <- function(x) {
  tibble::tibble(
    scale = names(x$scales),
    n_items = vapply(x$scales, length, integer(1)),
    items = vapply(
      x$scales,
      paste,
      collapse = ", ",
      FUN.VALUE = character(1)
    )
  )
}


nomo_run_decision_table <- function(x) {
  if (!nrow(x$decision_log)) return(tibble::tibble())

  out <- x$decision_log[
    nzchar(x$decision_log$decision),
    ,
    drop = FALSE
  ]
  tibble::as_tibble(out)
}


# Shared by print() and summary(): status, sample, and progress.
nomo_run_present_facts <- function(x) {
  completed <- x$stage_status$stage[x$stage_status$status == "completed"]
  nomo_present_facts(c(
    sprintf("Status: %s", toupper(x$status)),
    sprintf("Mode: %s", x$mode),
    sprintf("Sample design: %s", gsub("_", " ", x$sample_design))
  ))
  nomo_present_facts(c(
    sprintf("Exploratory N = %d", x$sample_n$n[x$sample_n$role == "exploratory"]),
    sprintf("Confirmatory N = %d", x$sample_n$n[x$sample_n$role == "confirmatory"]),
    sprintf("Scales: %d", length(x$scales))
  ))
  nomo_present_facts(c(
    sprintf("Completed: %s",
            if (length(completed)) paste(completed, collapse = " -> ") else "none"),
    sprintf("Next: %s", if (is.null(x$next_stage)) "none" else x$next_stage)
  ))
  lineage <- nomo_run_lineage(x)
  if (nrow(lineage)) {
    nomo_present_facts(sprintf(
      "Revisions: %d (%s); see nomo_table(x, \"lineage\")",
      nrow(lineage), paste(unique(gsub("_", "-", lineage$origin)), collapse = ", ")
    ))
  }
}


# One line per computed component, so a run's print says what it found rather
# than only that it finished (#89). Each line points at evidence the summary
# or nomo_table() shows in full.
nomo_run_key_evidence <- function(x) {
  r <- x$results
  out <- character()

  screens <- Filter(Negate(is.null), r$screen)
  if (length(screens)) {
    flags <- unlist(lapply(screens, function(s) nomo_screen_item_review(s)$attention))
    out <- c(out, sprintf("Item audit: %d items; flags: %s", length(flags),
                          nomo_present_flag_counts(flags)))
  }
  factors <- Filter(Negate(is.null), r$factors)
  if (length(factors)) {
    suggested <- vapply(factors, function(f) {
      n <- f$parallel$n_factors
      if (length(n)) format(n) else "-"
    }, character(1))
    out <- c(out, paste0("Parallel analysis suggests: ",
                         paste(names(suggested), suggested, collapse = ", ")))
  }
  efas <- Filter(Negate(is.null), r$efa)
  if (length(efas)) {
    flags <- unlist(lapply(efas, function(e) e$item_summary$attention))
    out <- c(out, paste0("EFA item flags: ", nomo_present_flag_counts(flags)))
  }
  if (!is.null(r$cfa)) {
    fe <- r$cfa$fit_evidence
    fit <- fe[fe$metric %in% c("CFI", "RMSEA", "SRMR") & is.finite(fe$value), , drop = FALSE]
    problem <- nomo_cfa_df_problem(fe)
    shown <- if (!is.null(problem)) {
      paste0("fit not testable (", problem$label, ")")
    } else if (nrow(fit)) {
      paste(fit$metric, nomo_present_number(fit$value), collapse = ", ")
    } else {
      "fit unavailable"
    }
    out <- c(out, paste0(
      "CFA: ", shown,
      "; loading flags: ", nomo_present_flag_counts(r$cfa$standardized_loadings$attention)
    ))
  }
  if (!is.null(r$reliability)) {
    omega <- nomo_reliability_summary_table(r$reliability)$omega
    omega <- omega[is.finite(omega)]
    if (length(omega)) {
      out <- c(out, sprintf("Reliability: omega %s to %s", nomo_present_number(min(omega)),
                            nomo_present_number(max(omega))))
    }
  }
  if (!is.null(r$validity)) {
    out <- c(out, sprintf(
      "Validity: convergent flags %s; separation flags %s",
      nomo_present_flag_counts(nomo_validity_convergent_table(r$validity)$signal),
      nomo_present_flag_counts(nomo_validity_discriminant_table(r$validity)$signal)
    ))
  }
  if (!is.null(r$invariance)) {
    out <- c(out, paste0("Invariance: completed ",
                         paste(r$invariance$completed_levels, collapse = " -> ")))
  }
  if (!is.null(r$network)) {
    n_hyp <- nrow(r$network$hypothesis_evidence)
    concordance <- table(nomo_network_pretty_status(r$network$hypothesis_evidence$concordance))
    out <- c(out, sprintf("Network: %d %s; %s", n_hyp, ifelse(n_hyp == 1L, "hypothesis", "hypotheses"),
                          paste(concordance, tolower(names(concordance)), collapse = ", ")))
  }
  if (!is.null(r$effort) && !is.null(r$effort$effort)) {
    flagged <- sum(r$effort$effort$n_flags > 0L)
    out <- c(out, sprintf("Careless responding: %d %s flagged, none removed",
                          flagged, ifelse(flagged == 1L, "case", "cases")))
  }
  if (!is.null(r$scores)) {
    out <- c(out, sprintf("Scores: %s method", r$scores$method))
  }
  if (length(r$missing)) {
    out <- c(out, paste0("Missing-data sensitivity: computed for ",
                         paste(names(r$missing), collapse = " and ")))
  }
  out
}


# Requests that differ only by scope, such as one factor count per scale, share
# their reason, options, and consequence; they are shown once, with each
# scope's own observation, instead of once per scale (#89).
nomo_run_present_requests <- function(requests, compact = FALSE) {
  key <- paste(requests$stage, requests$reason, requests$options,
               requests$consequence, requests$example, sep = "\r")
  for (rows in split(seq_len(nrow(requests)), factor(key, levels = unique(key)))) {
    r <- requests[rows, , drop = FALSE]
    nomo_present_section(sprintf(
      "Researcher decision required: %s (%s)", r$stage[[1L]],
      paste(r$scope, collapse = ", ")
    ))
    if (!compact) {
      nomo_present_text("Reason: ", r$reason[[1L]], indent = 2L)
      nomo_present_text("Options: ", r$options[[1L]], indent = 2L)
      nomo_present_text("Consequence: ", r$consequence[[1L]], indent = 2L)
    }
    nomo_present_bullets(paste0(r$scope, ": ", r$observation))
    if (nzchar(r$example[[1L]])) {
      nomo_present_text("Example: ", r$example[[1L]], indent = 2L)
    }
  }
}


#' @export
print.nomo_run <- function(x, ...) {
  research <- identical(x$mode, "research")
  nomo_present_header("nomo_run", "Guided workflow")
  nomo_run_present_facts(x)

  evidence <- nomo_run_key_evidence(x)
  if (length(evidence)) {
    nomo_present_section("Key evidence")
    nomo_present_bullets(evidence)
  }

  if (nrow(x$decision_requests)) {
    nomo_run_present_requests(x$decision_requests, compact = research)
    if (!research) {
      cat("\n")
      nomo_present_text(
        "No later stage has been run automatically while this consequential ",
        "decision is unresolved."
      )
    }
  }

  if (identical(x$status, "blocked") && !is.null(x$blocked)) {
    cat("\n")
    nomo_present_text(sprintf("Blocked at %s / %s: %s", x$blocked$stage,
                              x$blocked$scope, x$blocked$message))
    if (!research) {
      nomo_present_text(
        "The workflow is blocked, not silently skipped. Correct the ",
        "input, model, or settings, and start a new workflow."
      )
    }
  }

  if (identical(x$status, "complete")) {
    cat("\n")
    if (research) {
      nomo_present_text(
        "Requested workflow complete. summary(x) shows the stages and decisions; ",
        "nomo_table(x, \"recipe\") maps the components."
      )
    } else {
      nomo_present_text(
        "All requested stages are complete or explicitly marked not requested. ",
        "No hidden item deletion, model respecification, parameter freeing, or ",
        "validity verdict was performed. summary(x) shows the stages and ",
        "decisions, and nomo_report(x) archives the evidence."
      )
    }
  }

  invisible(x)
}


#' @export
summary.nomo_run <- function(object, ...) {
  out <- list(
    mode = object$mode,
    status = object$status,
    next_stage = object$next_stage,
    sample_design = object$sample_design,
    sample_n = object$sample_n,
    scales = nomo_run_scale_table(object),
    stage_status = object$stage_status,
    decision_requests = object$decision_requests,
    decisions = nomo_run_decision_table(object),
    component_log = nomo_run_component_logs(object),
    recipe = nomo_run_recipe_table(object),
    settings = nomo_run_settings_table(object),
    methods = nomo_methods(object),
    blocked = object$blocked
  )

  class(out) <- c("summary_nomo_run", "list")
  out
}


#' @export
print.summary_nomo_run <- function(x, ...) {
  nomo_present_header("nomo_run", "Guided workflow", summary = TRUE)
  nomo_present_facts(c(
    sprintf("Status: %s", toupper(x$status)),
    sprintf("Mode: %s", x$mode),
    sprintf("Sample design: %s", gsub("_", " ", x$sample_design)),
    sprintf("Next: %s", if (is.null(x$next_stage)) "none" else x$next_stage)
  ))

  stages <- x$stage_status
  stages$status_shown <- gsub("_", " ", stages$status)
  nomo_present_section("Stages")
  nomo_present_table(stages, c("Stage" = "stage", "Status" = "status_shown"))
  detailed <- stages[nzchar(stages$detail), , drop = FALSE]
  nomo_present_bullets(paste0(detailed$stage, ": ", detailed$detail))

  nomo_present_section("Scales")
  nomo_present_bullets(sprintf("%s (%d items): %s", x$scales$scale, x$scales$n_items,
                               x$scales$items))

  if (nrow(x$decision_requests)) {
    nomo_run_present_requests(x$decision_requests)
  }

  if (nrow(x$decisions)) {
    nomo_present_section("Recorded decisions")
    d <- x$decisions
    decision <- gsub("\\s*\n\\s*", "; ", d$decision)
    rationale <- ifelse(nzchar(d$rationale), paste0(" Rationale: ", d$rationale), "")
    nomo_present_bullets(sprintf("%s (%s, %s): %s.%s", d$id, d$stage,
                                 gsub("_", " ", d$source), decision, rationale))
  }

  nomo_present_section("Component recipe")
  recipe <- x$recipe
  recipe$scope_shown <- gsub("_", " ", recipe$scope)
  recipe$status_shown <- gsub("_", " ", recipe$status)
  nomo_present_table(
    recipe,
    c("Stage" = "stage", "Scope" = "scope_shown", "Function" = "function_name",
      "Status" = "status_shown"),
    more = "nomo_table(x, \"recipe\")"
  )
  nomo_present_text(
    "Data roles and researcher control for each step: nomo_table(x, \"recipe\").",
    indent = 2L
  )

  if (!is.null(x$methods) && nrow(x$methods)) {
    m <- x$methods
    stage_order <- unique(m$stage)
    counts <- data.frame(
      stage = stage_order,
      n = vapply(stage_order, function(s) sum(m$stage == s), integer(1)),
      primary = vapply(stage_order, function(s) sum(m$stage == s & m$role == "primary"), integer(1)),
      historical = vapply(stage_order, function(s) {
        sum(m$stage == s & m$lineage == "historical")
      }, integer(1)),
      stringsAsFactors = FALSE
    )
    nomo_present_section(sprintf("Methods used: %d", nrow(m)))
    nomo_present_table(
      counts,
      c("Stage" = "stage", "Methods" = "n", "Primary" = "primary",
        "Historical" = "historical")
    )
    primary <- m[m$role == "primary", , drop = FALSE]
    if (nrow(primary)) {
      nomo_present_text("Primary methods:", indent = 2L)
      nomo_present_bullets(vapply(unique(primary$stage), function(s) {
        paste0(s, ": ", paste(primary$method[primary$stage == s], collapse = "; "))
      }, character(1)))
    }
    nomo_present_text("Full entries and references: nomo_methods(x).", indent = 2L)
  }

  if (nrow(x$component_log)) {
    cat("\n")
    nomo_present_text(sprintf(
      "Component decision and evidence-log rows retained: %d; see ",
      nrow(x$component_log)
    ), "nomo_table(x, \"component_log\").")
  }

  invisible(x)
}


#' @export
nomo_table.nomo_run <- function(
    x,
    type = c(
      "stages",
      "requests",
      "decisions",
      "component_log",
      "scales",
      "recipe",
      "settings",
      "lineage",
      "methods"
    ),
    ...) {
  type <- match.arg(type)

  if (type == "stages") return(x$stage_status)
  if (type == "requests") return(x$decision_requests)
  if (type == "decisions") return(nomo_run_decision_table(x))
  if (type == "component_log") return(nomo_run_component_logs(x))
  if (type == "scales") return(nomo_run_scale_table(x))
  if (type == "recipe") return(nomo_run_recipe_table(x))
  if (type == "lineage") return(nomo_run_lineage(x))
  if (type == "methods") return(nomo_methods(x))

  nomo_run_settings_table(x)
}
