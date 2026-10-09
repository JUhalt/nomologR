# Guided workflow presentation -------------------------------------------------
#
# print() gives where the run is, one line of key evidence per component, and
# the decision it waits for, or why it is blocked; summary() gives every stage,
# the scales, the recorded decisions, the component recipe, the methods used,
# and each flag the components raised. Both follow the output style shared
# with contentvalidR (#144): stored values, such as the stage statuses, never
# change, and only their display does.

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


# Words -------------------------------------------------------------------------

# A stage, a pause, or a log's stage as a reader sees it: the codes with EFA
# and CFA capitalized and underscores as spaces; with `cell = TRUE`, starting
# with a capital for a table cell or the start of a line.
nomo_run_stage_words <- function(stage, cell = FALSE) {
  words <- c(
    efa = "EFA", cfa = "CFA", measurement_review = "measurement review",
    restart = "revision", missing = "missing data"
  )
  stage <- as.character(stage)
  out <- unname(words[stage])
  out[is.na(out)] <- gsub("_", " ", stage[is.na(out)])
  if (isTRUE(cell)) out <- paste0(toupper(substr(out, 1L, 1L)), substring(out, 2L))
  out
}


# A stored status (of the run, a stage, or a recipe row) in sentence case for
# a cell or a fact: "Completed", "Not started", "Awaiting decision".
nomo_run_status_words <- function(status) {
  nomo_present_status(gsub("_", " ", as.character(status)))
}


# Where a paused or blocked run goes next, as a fact; "" for a completed run.
nomo_run_next_fact <- function(x) {
  if (identical(x$status, "blocked")) {
    return(sprintf("Blocked: %s", nomo_run_stage_words(x$next_stage)))
  }
  if (is.null(x$next_stage)) return("")
  if (identical(x$next_stage, "restart")) {
    return("Next: revision with nomo_revise() or a new run")
  }
  sprintf("Next: %s", nomo_run_stage_words(x$next_stage))
}


# The abbreviations a run's output can show, its own and those of the
# component notes its summary quotes, defined once in it (guide point 23). The
# fit indices and estimators are worded as nomo_cfa() words them, the
# retention criteria as nomo_factors() does. SE is left out, since a scale may
# well be named SE. A function, so it never depends on the order the package's
# files are collated in.
nomo_run_glossary <- function() c(
  EFA = "exploratory factor analysis",
  MINRES = "minimum residual",
  PAF = "principal axis factoring",
  MAP = "minimum average partial criterion (Velicer)",
  TR2 = "the original MAP, averaging squared partial correlations",
  TR4 = "the revised MAP, averaging fourth powers of the partial correlations",
  EKC = "empirical Kaiser criterion",
  NEST = "next eigenvalue sufficiency test",
  CAF = "common part accounted for, the fit index Hull compares",
  KMO = "Kaiser-Meyer-Olkin measure of sampling adequacy",
  MSA = "measure of sampling adequacy",
  RMSR = "root mean square of the off-diagonal residual correlations",
  SMC = "squared multiple correlation",
  HTMT = "heterotrait-monotrait ratio of correlations",
  SEM = "structural equation modeling",
  FIML = "full-information maximum likelihood",
  MAR = "missing at random",
  MCAR = "missing completely at random",
  SESOI = "smallest effect size of interest",
  nomo_cfa_glossary[setdiff(names(nomo_cfa_glossary), c("SE", "MI", "EPC", "Std. EPC"))]
)


# The abbreviations of the glossary that `lines` show, in order of first
# appearance. A name is matched whole, so "HTMT" is not found in "HTMT2", nor
# "CFA" in nomo_cfa().
nomo_run_abbreviations <- function(lines) {
  text <- paste(lines, collapse = "\n")
  at <- vapply(names(nomo_run_glossary()), function(abbr) {
    pattern <- paste0("(?<![A-Za-z0-9_.])", gsub(".", "\\.", abbr, fixed = TRUE),
                      "(?![A-Za-z0-9_])")
    as.integer(regexpr(pattern, text, perl = TRUE))
  }, integer(1))
  names(sort(at[at > 0L]))
}


# Facts -------------------------------------------------------------------------

# Shared by print() and summary(): status, sample, progress, and revisions.
nomo_run_present_facts <- function(x) {
  cases <- function(role) nomo_present_stat(x$sample_n$n[x$sample_n$role == role], "count")
  completed <- x$stage_status$stage[x$stage_status$status == "completed"]
  nomo_present_facts(c(
    sprintf("Status: %s", nomo_run_status_words(x$status)),
    sprintf("Mode: %s", x$mode),
    sprintf("Sample design: %s", gsub("_", " ", x$sample_design))
  ))
  # A summary holds the scales as a table.
  n_scales <- if (is.data.frame(x$scales)) nrow(x$scales) else length(x$scales)
  nomo_present_facts(c(
    sprintf("Exploratory cases: %s", cases("exploratory")),
    sprintf("Confirmatory cases: %s", cases("confirmatory")),
    sprintf("Scales: %d", n_scales)
  ))
  nomo_present_facts(c(
    sprintf("Completed: %s", if (length(completed)) {
      paste(nomo_run_stage_words(completed), collapse = " -> ")
    } else {
      "none"
    }),
    nomo_run_next_fact(x)
  ))
  lineage <- nomo_run_lineage(x)
  if (nrow(lineage)) {
    nomo_present_facts(sprintf(
      "Revisions: %d (%s)", nrow(lineage),
      paste(unique(nomo_present_origin(lineage$origin)), collapse = ", ")
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
    reviews <- lapply(screens, nomo_screen_item_review)
    flags <- unlist(lapply(reviews, function(s) s$attention))
    # An item in two scales is audited in each, so items and audits differ
    # (#145).
    n_items <- length(unique(unlist(lapply(reviews, function(s) s$item))))
    out <- c(out, sprintf(
      "Item audit: %s%s; flags: %s", nomo_present_count(n_items, "item"),
      if (length(flags) > n_items) {
        sprintf(" (%d audits, since an item in two scales is audited in each)", length(flags))
      } else {
        ""
      },
      nomo_present_flag_counts(flags)
    ))
  }
  factors <- Filter(Negate(is.null), r$factors)
  if (length(factors)) {
    suggested <- vapply(factors, function(f) {
      n <- f$parallel$n_factors
      if (length(n)) nomo_present_stat(n, "count") else nomo_present_missing
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
    # Each index says which version it is, as the CFA's own print does (#145).
    parts <- nomo_cfa_fit_parts(fe, c("CFI", "RMSEA", "SRMR"), tag = TRUE)
    problem <- nomo_cfa_df_problem(fe)
    shown <- if (!is.null(problem)) {
      paste0("fit not testable (", problem$label, ")")
    } else if (length(parts)) {
      paste(parts, collapse = ", ")
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
      range <- unique(nomo_present_stat(c(min(omega), max(omega)), "reliability"))
      out <- c(out, paste("Reliability: omega", paste(range, collapse = " to ")))
    }
  }
  if (!is.null(r$validity)) {
    out <- c(out, sprintf(
      "Validity: convergent flags %s; separation flags %s",
      nomo_present_flag_counts(nomo_validity_convergent_table(r$validity)[["signal"]]),
      # A single construct has no pairs, so the table has no columns.
      nomo_present_flag_counts(nomo_validity_discriminant_table(r$validity)[["signal"]])
    ))
  }
  if (!is.null(r$invariance)) {
    out <- c(out, paste0("Invariance: completed ",
                         nomo_invariance_level_path(r$invariance$completed_levels)))
  }
  if (!is.null(r$network)) {
    n_hyp <- nrow(r$network$hypothesis_evidence)
    concordance <- table(nomo_network_pretty_status(r$network$hypothesis_evidence$concordance))
    # Lowercase in prose, keeping an abbreviation such as SESOI.
    words <- paste0(tolower(substr(names(concordance), 1L, 1L)), substring(names(concordance), 2L))
    out <- c(out, sprintf("Network: %s; %s", nomo_present_count(n_hyp, "hypothesis", "hypotheses"),
                          paste(concordance, words, collapse = ", ")))
  }
  if (!is.null(r$effort) && !is.null(r$effort$effort)) {
    flagged <- sum(r$effort$effort$n_flags > 0L)
    out <- c(out, sprintf("Careless responding: %s flagged, none removed",
                          nomo_present_count(flagged, "case")))
  }
  if (!is.null(r$scores)) {
    out <- c(out, sprintf("Scores: %s method", r$scores$method))
  }
  if (length(r$missing)) {
    what <- c(cfa = "the measurement model", network = "the network")[names(r$missing)]
    compared <- vapply(r$missing, function(m) sum(m$strategies$available) > 1L, logical(1))
    out <- c(out, paste0("Missing-data sensitivity: ", paste(c(
      if (any(compared)) paste("strategies compared for", nomo_present_or(what[compared], "and")),
      if (any(!compared)) {
        paste("only one strategy could be fitted for", nomo_present_or(what[!compared], "and"))
      }
    ), collapse = "; ")))
  }
  out
}


# The key evidence with each value bound to what it measures, so that a
# narrow console never splits "CFI .996 (robust)" or "Agency 1" (guide point
# 24).
nomo_run_bind_evidence <- function(evidence) {
  nb <- nomo_present_nbsp
  evidence <- gsub("\\b(CFI|TLI|RMSEA|SRMR) ([-.0-9]+)", paste0("\\1", nb, "\\2"), evidence,
                   perl = TRUE)
  evidence <- gsub("([0-9]) \\((robust|scaled)\\)", paste0("\\1", nb, "(\\2)"), evidence,
                   perl = TRUE)
  pa <- startsWith(evidence, "Parallel analysis suggests: ")
  evidence[pa] <- gsub("(\\S+) ([0-9]+|--)(?=,|$)", paste0("\\1", nb, "\\2"), evidence[pa],
                       perl = TRUE)
  evidence
}


# Requests and blocks ---------------------------------------------------------------

# The title of a group of requests, by the decision it asks for.
nomo_run_request_title <- function(id, n) {
  what <- if (startsWith(id, "factor_count:")) {
    paste("on the", nomo_present_noun(n, "factor count", "factor counts"))
  } else if (identical(id, "cfa_model")) {
    "on the measurement model"
  } else if (identical(id, "measurement_model")) {
    "after the measurement review"
  } else {
    "on a revision"
  }
  paste("Researcher decision required", what)
}


# Requests that differ only by scope, such as one factor count per scale, share
# their reason, options, and consequence; they are shown once, with each
# scope's own observation, instead of once per scale (#89).
nomo_run_present_requests <- function(requests, compact = FALSE) {
  key <- paste(requests$stage, requests$reason, requests$options,
               requests$consequence, requests$example, sep = "\r")
  for (rows in split(seq_len(nrow(requests)), factor(key, levels = unique(key)))) {
    r <- requests[rows, , drop = FALSE]
    nomo_present_section(nomo_run_request_title(r$id[[1L]], nrow(r)))
    if (!compact) {
      nomo_present_text("Reason: ", r$reason[[1L]], indent = 2L)
      nomo_present_text("Options: ", r$options[[1L]], indent = 2L)
      nomo_present_text("Consequence: ", r$consequence[[1L]], indent = 2L)
    }
    # A request for one scale names it; one for the whole model needs no label.
    per_scope <- grepl(":", r$id, fixed = TRUE)
    nomo_present_bullets(ifelse(per_scope, paste0(r$scope, ": ", r$observation), r$observation))
    if (nzchar(r$example[[1L]])) {
      nomo_present_text("Example: ", r$example[[1L]], indent = 2L)
    }
  }
}


# A blocked run has no decision to supply, so it is shown as one block: what
# failed, where, and what to do (#145).
nomo_run_present_blocked <- function(x, research = FALSE) {
  b <- x$blocked
  nomo_present_section(sprintf("Blocked at the %s stage (%s)", nomo_run_stage_words(b$stage),
                               gsub("_", " ", b$scope)))
  message <- trimws(b$message)
  if (!grepl("[.!?]$", message)) message <- paste0(message, ".")
  nomo_present_text(message, indent = 2L)
  nomo_present_text(
    if (research) {
      "No later stage was run. Correct the input and start a new nomo_run()."
    } else {
      paste(
        "The workflow is blocked, not silently skipped: no later stage was run.",
        "Correct the input, model, or settings, and start a new nomo_run(); a",
        "blocked workflow cannot be resumed or revised."
      )
    },
    indent = 2L
  )
}


# The abbreviations shown in `lines`, as a one-line note in print() or a key
# section in summary().
nomo_run_present_abbreviations <- function(lines, key = FALSE) {
  abbr <- nomo_run_abbreviations(lines)
  if (!length(abbr)) return(invisible(NULL))
  text <- unname(nomo_run_glossary()[abbr])
  if (isTRUE(key)) {
    nomo_present_key(stats::setNames(paste0(toupper(substr(text, 1L, 1L)), substring(text, 2L)),
                                     abbr), title = "Abbreviations")
  } else {
    cat("\n")
    nomo_present_text(paste(abbr, "=", text, collapse = "; "), ".")
  }
}


# Print -------------------------------------------------------------------------

nomo_run_print_body <- function(x, research) {
  nomo_present_header("nomo_run", "Guided workflow")
  nomo_run_present_facts(x)

  evidence <- nomo_run_key_evidence(x)
  if (length(evidence)) {
    nomo_present_section("Key evidence")
    nomo_present_bullets(nomo_run_bind_evidence(evidence))
  }

  blocked <- identical(x$status, "blocked") && !is.null(x$blocked)
  if (blocked) {
    nomo_run_present_blocked(x, research)
  } else if (nrow(x$decision_requests)) {
    nomo_run_present_requests(x$decision_requests, compact = research)
    if (!research) {
      cat("\n")
      nomo_present_text(
        "No later stage has been run automatically while this consequential ",
        "decision is unresolved."
      )
    }
  }

  if (identical(x$status, "complete")) {
    cat("\n")
    if (research) {
      nomo_present_text(
        "Requested workflow complete; no item was deleted and no model respecified."
      )
    } else {
      nomo_present_text(
        "All requested stages are complete or explicitly marked not requested. ",
        "No hidden item deletion, model respecification, parameter freeing, or ",
        "validity verdict was performed."
      )
    }
  }
}


#' @export
print.nomo_run <- function(x, ...) {
  research <- identical(x$mode, "research")
  body <- utils::capture.output(nomo_run_print_body(x, research))
  nomo_present_cat(body)
  nomo_run_present_abbreviations(body)

  calls <- "summary(x)"
  what <- "the stages and recorded decisions"
  if (identical(x$status, "complete")) {
    calls <- c(calls, "nomo_report(x, file = \"report.html\")")
    what <- c(what, "an archived report")
  } else if (identical(x$status, "blocked")) {
    calls <- c(calls, "nomo_table(x, \"component_log\")")
    what <- c(what, "the component logs")
  } else if (nrow(x$decision_requests)) {
    calls <- c(calls, "nomo_table(x, \"requests\")")
    what <- c(what, "the decision requests")
  }
  if (nrow(nomo_run_lineage(x))) {
    calls <- c(calls, "nomo_table(x, \"lineage\")")
    what <- c(what, "the revisions")
  }
  nomo_present_pointer(calls, what)
  invisible(x)
}


# Summary -----------------------------------------------------------------------

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
    blocked = object$blocked,
    lineage = nomo_run_lineage(object)
  )

  class(out) <- c("summary_nomo_run", "list")
  out
}


# Where in the run a component's log row comes from: "the Agency item audit",
# "the CFA", "validity".
nomo_run_flag_place <- function(component, scope) {
  per_scale <- !scope %in% c("workflow", "measurement_model", "theory_network",
                             "careless_responding")
  words <- c(screen = "item audit", factors = "factor retention", efa = "EFA",
             cfa = "CFA", reliability = "reliability", validity = "validity",
             invariance = "invariance", network = "network", scores = "scores",
             missing = "missing-data comparison")
  place <- unname(words[component])
  place[is.na(place)] <- gsub("_", " ", component[is.na(place)])
  place <- ifelse(per_scale, paste(scope, place), place)
  place[scope == "careless_responding"] <- "careless-responding screen"
  place[scope == "theory_network" & component == "missing"] <- "network's missing-data comparison"
  # A noun phrase takes the article; the names of evidence do not.
  ifelse(per_scale | component %in% c("efa", "cfa", "network", "missing") |
           scope == "careless_responding", paste("the", place), place)
}


# The names a flag can be about: the scales and their items, and the
# indicators and factors of the measurement models, which may differ from
# them (#145).
nomo_run_flag_names <- function(scales, models = character()) {
  out <- c(unlist(strsplit(scales$items, ", ", fixed = TRUE)), scales$scale)
  for (model in models) {
    parsed <- nomo_run_model_names(model)
    out <- c(out, parsed$indicators, parsed$factors)
  }
  unique(out)
}


# Whether a log row's object names what it is about: one of `names`, or a pair
# of them, such as "AG =~ sd1", "AG ~~ PE", or "AG vs PE".
nomo_run_flag_named <- function(object, names) {
  parts <- strsplit(as.character(object), " (=~|~~|~|vs) ")
  vapply(parts, function(p) length(p) > 0L && all(p %in% names), logical(1))
}


# Each flag the components raised, concern before review (guide point 22). A
# flag on an item, a factor, or a pair of them names it and where it was
# raised, "b5 in the CFA"; any other flag names where it was raised, "The
# Agency factor retention". `models` are the measurement models the run
# fitted, whose indicators and factors need not be the scales' (#145).
nomo_run_present_flagged <- function(log, scales, models = character()) {
  if (!nrow(log) || !all(c("severity", "object", "observation") %in% names(log))) {
    return(invisible(NULL))
  }
  place <- nomo_run_flag_place(log$pipeline_component, log$pipeline_scope)
  named <- nomo_run_flag_named(log$object, nomo_run_flag_names(scales, models))
  unit <- ifelse(named, paste(gsub(" vs ", " vs. ", log$object, fixed = TRUE), "in", place),
                 place)
  # Units with the same flag share a bullet, so only the first of them starts
  # the line with a capital: "The Agency EFA, the Persistence EFA (Review)".
  text <- trimws(log$observation)
  text[is.na(text)] <- ""
  text <- ifelse(nzchar(text) & !grepl("[.!?]$", text), paste0(text, "."), text)
  key <- paste(nomo_present_status(log$severity), text, sep = "\r")
  lead <- !named & unit == unit[match(key, key)]
  unit[lead] <- paste0(toupper(substr(unit[lead], 1L, 1L)), substring(unit[lead], 2L))
  nomo_present_flagged(unit = unit, status = log$severity, text = log$observation)
}


nomo_run_summary_body <- function(x) {
  nomo_present_header("nomo_run", "Guided workflow", summary = TRUE)
  nomo_run_present_facts(x)

  stages <- x$stage_status
  stages$stage_shown <- nomo_run_stage_words(stages$stage, cell = TRUE)
  stages$status_shown <- nomo_run_status_words(stages$status)
  nomo_present_section("Stages")
  nomo_present_table(stages, c("Stage" = "stage_shown", "Status" = "status_shown"),
                     more = "nomo_table(x, \"stages\")")
  detailed <- stages[nzchar(stages$detail), , drop = FALSE]
  nomo_present_bullets(paste0(detailed$stage_shown, ": ", detailed$detail))

  nomo_present_section("Scales")
  nomo_present_bullets(sprintf("%s (%s): %s", x$scales$scale,
                               nomo_present_count(x$scales$n_items, "item"), x$scales$items))

  if (identical(x$status, "blocked") && !is.null(x$blocked)) {
    nomo_run_present_blocked(x)
  } else if (nrow(x$decision_requests)) {
    nomo_run_present_requests(x$decision_requests)
  }

  if (nrow(x$decisions)) {
    nomo_present_section("Recorded decisions")
    d <- x$decisions
    decision <- gsub("\\s*\n\\s*", "; ", d$decision)
    # A model's operator stays on one line with its two sides: "ag1 ~~ ag2".
    nb <- nomo_present_nbsp
    decision <- gsub(" (=~|~~|~) ", paste0(nb, "\\1", nb), decision)
    decision[d$id == "sample_design"] <- gsub("_", " ", decision[d$id == "sample_design"])
    rationale <- ifelse(nzchar(d$rationale), paste0(" Rationale: ", d$rationale), "")
    nomo_present_bullets(sprintf("%s (%s, %s): %s.%s", d$id, nomo_run_stage_words(d$stage),
                                 gsub("_", " ", d$source), decision, rationale))
  }

  nomo_present_section("Component recipe")
  recipe <- x$recipe
  recipe$stage_shown <- nomo_run_stage_words(recipe$stage, cell = TRUE)
  recipe$scope_shown <- gsub("_", " ", recipe$scope)
  recipe$status_shown <- nomo_run_status_words(recipe$status)
  nomo_present_table(
    recipe,
    c("Stage" = "stage_shown", "Scope" = "scope_shown", "Function" = "function_name",
      "Status" = "status_shown"),
    more = "nomo_table(x, \"recipe\")"
  )

  if (!is.null(x$methods) && nrow(x$methods)) {
    m <- x$methods
    stage_order <- unique(m$stage)
    counts <- data.frame(
      stage = nomo_run_stage_words(stage_order, cell = TRUE),
      n = vapply(stage_order, function(s) sum(m$stage == s), integer(1)),
      primary = vapply(stage_order, function(s) sum(m$stage == s & m$role == "primary"), integer(1)),
      historical = vapply(stage_order, function(s) {
        sum(m$stage == s & m$lineage == "historical")
      }, integer(1)),
      stringsAsFactors = FALSE
    )
    nomo_present_section("Methods")
    nomo_present_text(sprintf("Methods used: %d (%d primary, %d historical)", nrow(m),
                              sum(counts$primary), sum(counts$historical)), indent = 2L)
    nomo_present_table(
      counts,
      c("Stage" = "stage", "Methods" = "n", "Primary" = "primary",
        "Historical" = "historical"),
      more = "nomo_table(x, \"methods\")"
    )
    primary <- m[m$role == "primary", , drop = FALSE]
    if (nrow(primary)) {
      nomo_present_section("Primary methods")
      nomo_present_bullets(vapply(unique(primary$stage), function(s) {
        paste0(nomo_run_stage_words(s, cell = TRUE), ": ",
               paste(primary$method[primary$stage == s], collapse = "; "))
      }, character(1)))
    }
  }

  # A run with no recorded decision has an empty decision table, so its
  # columns are read without `$`.
  models <- x$decisions[["decision"]][x$decisions[["id"]] %in% "cfa_model"]
  nomo_run_present_flagged(x$component_log, x$scales, models)
}


#' @export
print.summary_nomo_run <- function(x, ...) {
  body <- utils::capture.output(nomo_run_summary_body(x))
  nomo_present_cat(body)
  nomo_run_present_abbreviations(body, key = TRUE)

  calls <- c("nomo_table(x, \"recipe\")", "nomo_methods(x)")
  what <- c("each step's data role and researcher control",
            "every method with its references")
  if (nrow(x$component_log)) {
    calls <- c(calls, "nomo_table(x, \"component_log\")")
    what <- c(what, sprintf("the %s of the component logs",
                            nomo_present_count(nrow(x$component_log), "row")))
  }
  nomo_present_pointer(calls, what)
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
  type <- nomo_match_arg(type)

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
