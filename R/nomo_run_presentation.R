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


#' @export
print.nomo_run <- function(x, ...) {
  completed <- x$stage_status$stage[
    x$stage_status$status == "completed"
  ]
  completed_text <- if (length(completed)) {
    paste(completed, collapse = " -> ")
  } else {
    "none"
  }

  if (identical(x$mode, "research")) {
    cat(sprintf(
      "<nomo_run> mode=research | status=%s | design=%s\n",
      x$status,
      x$sample_design
    ))
    cat(sprintf(
      "Completed: %s | Next: %s\n",
      completed_text,
      if (is.null(x$next_stage)) "none" else x$next_stage
    ))
    cat(sprintf(
      "Scales: %d | Exploratory N: %d | Confirmatory N: %d\n",
      length(x$scales),
      x$sample_n$n[x$sample_n$role == "exploratory"],
      x$sample_n$n[x$sample_n$role == "confirmatory"]
    ))

    if (nrow(x$decision_requests)) {
      cat("\nDecision requests\n")
      print(
        x$decision_requests[, c(
          "stage",
          "scope",
          "reason",
          "consequence"
        ), drop = FALSE],
        n = Inf,
        width = Inf
      )
    }

    if (identical(x$status, "blocked") && !is.null(x$blocked)) {
      cat(sprintf(
        "\nBlocked at %s / %s: %s\n",
        x$blocked$stage,
        x$blocked$scope,
        x$blocked$message
      ))
    }

    if (identical(x$status, "complete")) {
      cat(
        "\nRequested workflow complete. ",
        "Use `nomo_table(x, \"recipe\")` for the component map and ",
        "`nomo_table(x, \"component_log\")` for retained evidence provenance.\n",
        sep = ""
      )
    }

    return(invisible(x))
  }

  cat("<nomo_run>\n")
  cat("Guided nomologR workflow | teaching mode\n")
  cat(sprintf("Status: %s\n", toupper(x$status)))
  cat(sprintf(
    "Sample design: %s | Exploratory N = %d | Confirmatory N = %d\n",
    x$sample_design,
    x$sample_n$n[x$sample_n$role == "exploratory"],
    x$sample_n$n[x$sample_n$role == "confirmatory"]
  ))
  cat(sprintf("Completed stages: %s\n", completed_text))
  cat(sprintf(
    "Next stage: %s\n",
    if (is.null(x$next_stage)) "none" else x$next_stage
  ))

  if (nrow(x$decision_requests)) {
    cat("\nResearcher decision required\n")

    for (i in seq_len(nrow(x$decision_requests))) {
      row <- x$decision_requests[i, , drop = FALSE]
      cat(sprintf("\n[%s / %s]\n", row$stage[[1L]], row$scope[[1L]]))
      cat("Observation: ", row$observation[[1L]], "\n", sep = "")
      cat("Reason: ", row$reason[[1L]], "\n", sep = "")
      cat("Options: ", row$options[[1L]], "\n", sep = "")
      cat("Consequence: ", row$consequence[[1L]], "\n", sep = "")
      if (nzchar(row$example[[1L]])) {
        cat("Example: ", row$example[[1L]], "\n", sep = "")
      }
    }

    cat(
      "\nNo later stage has been run automatically while this consequential ",
      "decision is unresolved.\n",
      sep = ""
    )
  }

  if (identical(x$status, "blocked") && !is.null(x$blocked)) {
    cat(
      "\nThe workflow is blocked, not silently skipped.\n",
      "Inspect the component error above, correct the input/model/settings, and ",
      "start a new workflow.\n",
      sep = ""
    )
  }

  if (identical(x$status, "complete")) {
    cat(
      "\nAll requested stages are complete or explicitly marked not requested.\n",
      "No hidden item deletion, model respecification, parameter freeing, or ",
      "validity verdict was performed.\n",
      sep = ""
    )
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
    blocked = object$blocked
  )

  class(out) <- c("summary_nomo_run", "list")
  out
}


#' @export
print.summary_nomo_run <- function(x, ...) {
  cat("nomologR guided-workflow summary\n")
  cat(sprintf(
    "Status: %s | Mode: %s | Sample design: %s\n",
    x$status,
    x$mode,
    x$sample_design
  ))
  cat(sprintf(
    "Next stage: %s\n\n",
    if (is.null(x$next_stage)) "none" else x$next_stage
  ))

  cat("Stage status\n")
  print(x$stage_status, n = Inf, width = Inf)

  cat("\nScale definitions\n")
  print(x$scales, n = Inf, width = Inf)

  if (nrow(x$decision_requests)) {
    cat("\nOutstanding researcher decisions\n")
    print(x$decision_requests, n = Inf, width = Inf)
  }

  if (nrow(x$decisions)) {
    cat("\nRecorded workflow decisions\n")
    print(x$decisions, n = Inf, width = Inf)
  }

  cat("\nComponent recipe\n")
  print(x$recipe, n = Inf, width = Inf)

  if (nrow(x$component_log)) {
    cat(sprintf(
      "\nComponent decision/evidence-log rows retained: %d\n",
      nrow(x$component_log)
    ))
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
      "settings"
    ),
    ...) {
  type <- match.arg(type)

  if (type == "stages") return(x$stage_status)
  if (type == "requests") return(x$decision_requests)
  if (type == "decisions") return(nomo_run_decision_table(x))
  if (type == "component_log") return(nomo_run_component_logs(x))
  if (type == "scales") return(nomo_run_scale_table(x))
  if (type == "recipe") return(nomo_run_recipe_table(x))

  nomo_run_settings_table(x)
}
