# Guided workflow orchestration ------------------------------------------------

nomo_run_fresh <- function(data,
                           scales,
                           mode,
                           guidance,
                           decisions,
                           settings,
                           call,
                           handoff = NULL) {
  roles <- nomo_run_data_roles(data)
  if (!is.null(handoff)) {
    nomo_handoff_check_data(
      handoff, intersect(names(roles$exploratory), names(roles$confirmatory))
    )
  }

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
    decision_log = nomo_run_initial_log(scales, roles, handoff),
    blocked = NULL,
    source_data = data,
    state_version = 2L
  )
  # Added only when present, so a run without a handoff has exactly the shape
  # it always had.
  if (!is.null(handoff)) x$handoff <- handoff
  class(x) <- c("nomo_run", "list")

  for (scope in names(scales)) {
    extra <- nomo_run_scope_settings(
      settings = settings,
      stage = "screen",
      items = scales[[scope]],
      scales = scales
    )
    # Careless-responding indices describe a respondent across the whole
    # instrument, so they are computed once below, never per scale (#73).
    extra[nomo_run_effort_arguments()] <- NULL

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

  effort <- nomo_run_effort_request(settings, scales, handoff)
  if (!is.null(effort)) {
    result <- nomo_run_safe_component(
      fun = nomo_screen,
      fixed = list(
        data = roles$exploratory,
        items = unique(unlist(scales, use.names = FALSE)),
        guidance = guidance
      ),
      extra = effort$arguments,
      stage = "screen"
    )

    if (!result$ok) {
      return(nomo_run_block(x, "screen", "careless_responding", result))
    }

    x$results$effort <- result$value
    x$decision_log <- nomo_run_effort_log(x$decision_log, effort, result$value)
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
#'   candidate item-column names for one scale/construct. May also be a handoff
#'   from `contentvalidR`'s `content_handoff()`, whose carried items and
#'   construct mapping then define the scales, as described for
#'   [nomo_screen()]. A handoff from a review with no construct mapping is
#'   refused, because a guided run needs scales and nomologR does not invent
#'   them.
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
#'
#'   `list(screen = list(effort = TRUE))` adds careless-responding indices (see
#'   [nomo_screen()]). They describe a respondent across the whole instrument,
#'   and even-odd consistency cannot be computed within one scale. So they are
#'   computed once, over every item in the run with the run's scales, and never
#'   inside the per-scale item audits. `reverse`, `scale_range`,
#'   `pair_magnitude`, and `scales` may be given alongside `effort`. When the
#'   scales came from a `contentvalidR` handoff that declares keying, its
#'   keying is used unless `reverse` or `scale_range` is given here.
#' @param resume Optional prior `nomo_run` object. When supplied, the existing
#'   source data, scales, guidance, completed component results, decisions, and
#'   provenance are reused.
#'
#' @return A `nomo_run` object containing component results, stage status,
#'   outstanding decision requests, researcher decisions/rationales, stage
#'   settings, workflow provenance, component decision logs, and source inputs
#'   required for reproducibility.
#'
#' @references
#' Boateng, G. O., Neilands, T. B., Frongillo, E. A., Melgar-Quiñonez, H. R.,
#' & Young, S. L. (2018). Best practices for developing and validating scales
#' for health, social, and behavioral research: A primer. *Frontiers in Public
#' Health, 6*, 149. \doi{10.3389/fpubh.2018.00149}
#'
#' Clark, L. A., & Watson, D. (1995). Constructing validity: Basic issues in
#' objective scale development. *Psychological Assessment, 7*(3), 309-319.
#' \doi{10.1037/1040-3590.7.3.309}
#'
#' Flake, J. K., & Fried, E. I. (2020). Measurement schmeasurement:
#' Questionable measurement practices and how to avoid them. *Advances in
#' Methods and Practices in Psychological Science, 3*(4), 456-465.
#' \doi{10.1177/2515245920952393}
#'
#' Flake, J. K., Pek, J., & Hehman, E. (2017). Construct validation in social
#' and personality research: Current practice and recommendations. *Social
#' Psychological and Personality Science, 8*(4), 370-378.
#' \doi{10.1177/1948550617693063}
#'
#' Hinkin, T. R. (1998). A brief tutorial on the development of measures for
#' use in survey questionnaires. *Organizational Research Methods, 1*(1),
#' 104-121. \doi{10.1177/109442819800100106}
#'
#' Simmons, J. P., Nelson, L. D., & Simonsohn, U. (2011). False-positive
#' psychology: Undisclosed flexibility in data collection and analysis allows
#' presenting anything as significant. *Psychological Science, 22*(11),
#' 1359-1366. \doi{10.1177/0956797611417632}
#'
#' @examples
#' \donttest{
#' scales <- list(
#'   Agency = c("ag1", "ag2", "ag3", "ag4"),
#'   Persistence = c("pe1", "pe2", "pe3", "pe4")
#' )
#'
#' # Evidence is computed, then the run pauses for the factor-count decision.
#' run <- nomo_run(
#'   data = nomo_demo_network,
#'   scales = scales,
#'   settings = list(factors = list(n_iter = 20, seed = 2026))
#' )
#' run
#' nomo_table(run, "requests")
#'
#' run <- nomo_run(
#'   resume = run,
#'   decisions = list(
#'     factor_count = list(
#'       value = c(Agency = 1, Persistence = 1),
#'       rationale = "Each scale was written to measure one construct."
#'     )
#'   )
#' )
#'
#' run <- nomo_run(
#'   resume = run,
#'   decisions = list(
#'     cfa_model = list(
#'       value = "Agency =~ ag1 + ag2 + ag3 + ag4; Persistence =~ pe1 + pe2 + pe3 + pe4",
#'       rationale = "Prespecified two-construct measurement model."
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
#' nomo_table(run, "stages")
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

  # A contentvalidR handoff defines the scales from content review (#46). A
  # review with no construct mapping cannot, and the workflow does not invent
  # one.
  handoff <- NULL
  if (nomo_handoff_is(scales)) {
    handoff <- nomo_handoff_read(scales)
    if (is.null(handoff$scales)) {
      stop(
        sprintf(
          paste(
            "This handoff comes from a content review with no construct mapping",
            "(workflow: %s), so it cannot define the scales a guided run needs,",
            "and nomologR does not invent construct membership. Assign the",
            "carried items to scales yourself, for example",
            "`scales = list(Construct = c(...))`, or screen them with",
            "`nomo_screen(data, items = handoff)`. Carried items: %s."
          ),
          handoff$provenance$workflow, paste(handoff$items, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    scales <- handoff$scales
  }

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
    call = call,
    handoff = handoff
  )
}
