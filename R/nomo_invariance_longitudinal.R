# Longitudinal measurement invariance ------------------------------------------------

#' Evaluate measurement invariance across occasions
#'
#' `nomo_invariance_longitudinal()` asks whether the same measures mean the same
#' thing each time the same people answer them. It fits the measurement model
#' at every occasion in one model and imposes increasingly strict equality
#' constraints across occasions, as [nomo_invariance()] does across groups.
#'
#' @details
#' **Why.** A change in scores is a change in the construct only if the
#' measurement relations hold across occasions. Equal loadings let changes in
#' the construct's variance and relations be compared, and equal intercepts let
#' changes in its mean be compared (Widaman, Ferrer, & Conger, 2010). An item
#' whose meaning drifts, through practice, an intervention, or development,
#' otherwise shows up as change in the construct.
#'
#' **The model.** `model` describes one occasion, in the items' own names.
#' `columns` says how an item and an occasion name a column of `data`: with the
#' default `"{item}_{occasion}"`, item `w1` at occasion `t2` is column `w1_t2`.
#' Each factor is measured at every occasion, and the occasion factors are
#' correlated. Each item's unique factor is correlated with the same item's
#' unique factor on the other occasions, because what an item measures besides
#' the construct is usually stable too, and leaving those covariances out
#' biases the estimates (Widaman et al., 2010). `auto` sets how many lags they
#' span.
#'
#' **The sequence.** The levels, the ordered-item identification, partial
#' releases, and score diagnostics are those of [nomo_invariance()], applied
#' across occasions instead of groups. A partial release names the item as in
#' `model`, and frees that parameter across all occasions: `"w3 ~ 1"` releases
#' the intercept of `w3`. A release naming a column, such as `"w3_t2 ~ 1"`, is
#' an error, as is any release that frees no parameter. For ordered items, Wu
#' and Estabrook's (2016) identification is the default; `ID.cat = "millsap"`,
#' with `ID.fac = "UL"`, applies Millsap and Tein's (2004) conditions, which
#' Liu et al. (2017) extend to repeated measures. An item whose columns are
#' stored as ordered factors
#' is modeled as ordered on every occasion, whether or not `ordered` names it.
#'
#' **Latent change.** Once intercepts are invariant, fully or partially, the
#' construct's mean can be compared across occasions. Under the default
#' `ID.fac = "std.lv"` the first occasion's latent mean is 0 and its variance
#' 1, so `latent_means` gives each later occasion's latent mean as a change in
#' the first occasion's latent standard deviations, with its interval.
#'
#' @inheritParams nomo_invariance
#' @param model A measurement model for one occasion, in the items' own names:
#'   a lavaan model string or a [nomo_model()] object. Loadings are free; the
#'   model contains factors and their items only.
#' @param data A data frame with one row per person and one column per item and
#'   occasion.
#' @param occasions Two or more occasion labels, in time order. The first is the
#'   reference for latent change.
#' @param columns A pattern for the column names, containing `{item}` and
#'   `{occasion}`.
#' @param ordered Optional names of ordered items, as in `model`. Items whose
#'   columns are stored as ordered factors are modeled as ordered whether or
#'   not they are named here, and the decision log lists them for review.
#' @param partial Optional researcher-specified releases from [nomo_partial()],
#'   naming items and factors as in `model`.
#' @param auto The lags over which each item's unique factors are correlated:
#'   `"all"` (default) or a positive whole number, such as `1` for adjacent
#'   occasions only.
#'
#' @return A `nomo_invariance_longitudinal` object, which is also a
#'   `nomo_invariance` object, so [nomo_table()], [nomo_apa_table()], and
#'   `plot()` work as they do for groups. The fields to read are those of
#'   [nomo_invariance()], with `occasions` in place of `groups`, and:
#'
#'   * `latent_means`: at each level that holds intercepts equal, each later
#'     occasion's latent mean relative to the first, in the first occasion's
#'     latent standard deviations, with its interval.
#'   * `longitudinal_model`: the configural model across occasions, before
#'     equality constraints.
#'
#'   Other fields record the call, the settings used, and intermediate engine
#'   results. They may change between releases and are not part of the stable
#'   interface (see `?nomologR`).
#'
#' @references
#' Liu, Y., Millsap, R. E., West, S. G., Tein, J.-Y., Tanaka, R., & Grimm, K. J.
#' (2017). Testing measurement invariance in longitudinal data with
#' ordered-categorical measures. *Psychological Methods, 22*(3), 486-506.
#' \doi{10.1037/met0000075}
#'
#' Millsap, R. E., & Tein, J.-Y. (2004). Assessing factorial invariance in
#' ordered-categorical measures. *Multivariate Behavioral Research, 39*(3),
#' 479-515. \doi{10.1207/S15327906MBR3903_4}
#'
#' Widaman, K. F., Ferrer, E., & Conger, R. D. (2010). Factorial invariance
#' within longitudinal structural equation models: Measuring the same construct
#' across time. *Child Development Perspectives, 4*(1), 10-18.
#' \doi{10.1111/j.1750-8606.2009.00110.x}
#'
#' Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor
#' analysis models of different levels of invariance for ordered categorical
#' outcomes. *Psychometrika, 81*(4), 1014-1045.
#' \doi{10.1007/s11336-016-9506-0}
#'
#' @seealso [nomo_invariance()] across groups, and [nomo_partial()] for
#'   researcher-specified releases.
#'
#' @examples
#' \donttest{
#' long <- nomo_invariance_longitudinal(
#'   "Wellbeing =~ w1 + w2 + w3 + w4",
#'   data = nomo_demo_longitudinal,
#'   occasions = c("t1", "t2", "t3")
#' )
#' long
#' head(nomo_table(long, "local_strain"))
#'
#' # The w3 intercept drifts upward over time. Releasing it, as a documented
#' # decision, leaves the latent change to the other items.
#' release <- nomo_partial(
#'   level = "scalar",
#'   syntax = "w3 ~ 1",
#'   rationale = "The local-strain diagnostics point to the w3 intercept."
#' )
#' long_partial <- nomo_invariance_longitudinal(
#'   "Wellbeing =~ w1 + w2 + w3 + w4",
#'   data = nomo_demo_longitudinal,
#'   occasions = c("t1", "t2", "t3"),
#'   levels = c("configural", "metric", "scalar"),
#'   partial = release
#' )
#' nomo_table(long_partial, "latent_means")
#' }
#' @export
nomo_invariance_longitudinal <- function(model,
                                         data,
                                         occasions,
                                         columns = "{item}_{occasion}",
                                         ordered = NULL,
                                         levels = NULL,
                                         partial = NULL,
                                         localize = TRUE,
                                         auto = "all",
                                         estimator = NULL,
                                         missing = NULL,
                                         ID.fac = "std.lv",
                                         ID.cat = "Wu.Estabrook.2016",
                                         parameterization = "theta",
                                         guidance = nomo_defaults()) {
  if (inherits(model, "nomo_model")) model <- as.character(model)
  if (!is.character(model) || length(model) != 1L || is.na(model) ||
      !nzchar(trimws(model))) {
    stop("`model` must be one non-empty lavaan measurement-model string.", call. = FALSE)
  }
  if (!is.data.frame(data) || nrow(data) < 1L) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (!(is.character(occasions) || is.numeric(occasions)) || length(occasions) < 2L ||
      anyNA(occasions) || anyDuplicated(occasions) ||
      any(!nzchar(trimws(as.character(occasions))))) {
    stop("`occasions` must name two or more distinct occasions, in time order.", call. = FALSE)
  }
  occasions <- trimws(as.character(occasions))
  if (!is.character(columns) || length(columns) != 1L || is.na(columns) ||
      !grepl("{item}", columns, fixed = TRUE) ||
      !grepl("{occasion}", columns, fixed = TRUE)) {
    stop(
      "`columns` must be one pattern containing {item} and {occasion}, such as \"{item}_{occasion}\".",
      call. = FALSE
    )
  }
  if (!(identical(auto, "all") ||
          (is.numeric(auto) && length(auto) == 1L && is.finite(auto) && auto >= 1 &&
             auto == round(auto)))) {
    stop("`auto` must be \"all\" or a positive whole number of lags.", call. = FALSE)
  }

  structure <- nomo_invariance_longitudinal_structure(model)
  items <- unique(structure$item)
  factors <- unique(structure$factor)
  if (!is.null(ordered)) {
    if (!is.character(ordered) || anyNA(ordered)) {
      stop("`ordered` must be NULL or a character vector of item names.", call. = FALSE)
    }
    unknown <- setdiff(ordered, items)
    if (length(unknown)) {
      stop(
        sprintf(
          "`ordered` names items that are not in `model`: %s.",
          paste(unknown, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  name_at <- function(name, occasion) {
    gsub("{occasion}", occasion, gsub("{item}", name, columns, fixed = TRUE), fixed = TRUE)
  }
  long_items <- stats::setNames(
    lapply(items, function(item) vapply(occasions, name_at, character(1), name = item, USE.NAMES = FALSE)),
    items
  )
  long_factors <- stats::setNames(
    lapply(factors, function(f) vapply(occasions, name_at, character(1), name = f, USE.NAMES = FALSE)),
    factors
  )
  if (anyDuplicated(unlist(long_items)) || anyDuplicated(unlist(long_factors))) {
    stop("`columns` must give each item and occasion its own name.", call. = FALSE)
  }
  clash <- intersect(unlist(long_factors), c(names(data), unlist(long_items)))
  if (length(clash)) {
    stop(
      sprintf(
        "The occasion factors would be named like columns of `data`: %s. Rename the factors in `model`.",
        paste(clash, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  longitudinal_model <- paste(
    unlist(lapply(seq_along(occasions), function(t) {
      vapply(factors, function(f) {
        own <- structure$item[structure$factor == f]
        paste(
          long_factors[[f]][[t]], "=~",
          paste(vapply(own, function(i) long_items[[i]][[t]], character(1)), collapse = " + ")
        )
      }, character(1))
    })),
    collapse = "\n"
  )

  # An item stored as an ordered factor is ordered on every occasion, as
  # lavaan would fit its columns (#145); an item, not a column, is what
  # `ordered` names.
  found <- nomo_ordered_indicators(
    longitudinal_model, data, unlist(long_items[ordered])
  )
  ordered_detected <- items[vapply(
    long_items, function(columns) any(columns %in% found$detected), logical(1)
  )]
  ordered <- c(ordered, ordered_detected)

  prepared <- nomo_invariance_prepare(
    data = data,
    ordered = if (length(ordered)) unname(unlist(long_items[ordered])),
    levels = levels,
    partial = partial,
    localize = localize,
    estimator = estimator,
    missing = missing,
    ID.fac = ID.fac,
    ID.cat = ID.cat,
    parameterization = parameterization,
    guidance = guidance,
    structure = structure,
    release_hint = paste(
      " Across occasions, name items and factors as in the one-occasion",
      "`model`; a release applies on every occasion."
    )
  )
  ordered_columns <- prepared$ordered
  sequence_info <- prepared$sequence_info
  levels <- prepared$levels
  partial <- prepared$partial
  nomo_check_model_variables(longitudinal_model, data)

  engine <- nomo_invariance_engine_args(
    syntax_base = list(
      configural.model = longitudinal_model,
      data = data,
      longFacNames = long_factors,
      longIndNames = long_items,
      auto = auto,
      ID.fac = prepared$ID.fac,
      meanstructure = TRUE,
      return.fit = FALSE
    ),
    fit_base = list(data = data),
    ordered = ordered_columns,
    parameterization = prepared$parameterization,
    ID.cat = prepared$ID.cat,
    estimator = prepared$estimator_requested,
    missing = prepared$missing
  )
  run <- nomo_invariance_fit_levels(
    levels = levels,
    constraints = sequence_info$constraints,
    partial = partial,
    sequence_info = sequence_info,
    syntax_base = engine$syntax,
    fit_base = engine$fit,
    equal = "long.equal",
    release = "long.partial",
    localize = localize,
    names_map = c(long_items, long_factors)
  )
  fit_evidence <- run$fit_evidence
  ordered_used <- length(ordered_columns) > 0L

  decision_log <- nomo_invariance_decision_log(
    group = "occasions",
    groups = occasions,
    requested_levels = levels,
    fit_evidence = fit_evidence,
    estimator = prepared$estimator_requested,
    missing = prepared$missing,
    ID.fac = prepared$ID.fac,
    ID.cat = if (ordered_used) prepared$ID.cat else NA_character_,
    parameterization = if (ordered_used) prepared$parameterization else NA_character_,
    ordered = ordered_columns,
    category_table = prepared$category_table,
    sequence_note = sequence_info$identification_note,
    partial = partial,
    localize = localize,
    score_diagnostics = run$score_diagnostics,
    design = "occasions",
    ordered_detected = ordered_detected,
    partial_declared = prepared$partial_declared
  )
  latent_means <- nomo_invariance_longitudinal_means(
    fits = run$fits,
    constraints = sequence_info$constraints,
    fit_evidence = fit_evidence,
    ID.fac = prepared$ID.fac,
    long_factors = long_factors,
    occasions = occasions
  )
  decision_log <- dplyr::bind_rows(
    decision_log[1L, , drop = FALSE],
    nomo_invariance_longitudinal_residual_log(auto),
    decision_log[-1L, , drop = FALSE],
    nomo_invariance_longitudinal_means_log(latent_means)
  )

  out <- list(
    call = match.call(),
    design = "occasions",
    model = model,
    longitudinal_model = longitudinal_model,
    data_n = nrow(data),
    group = NA_character_,
    groups = occasions,
    occasions = occasions,
    columns = columns,
    long_factors = long_factors,
    long_items = long_items,
    auto = auto,
    indicator_type = sequence_info$type,
    identification_note = sequence_info$identification_note,
    ordered = ordered_columns,
    ordered_detected = ordered_detected,
    ordered_categories = prepared$category_table,
    requested_levels = levels,
    completed_levels = fit_evidence$level,
    constraints = sequence_info$constraints[fit_evidence$level],
    partial = partial,
    partial_requested = nomo_invariance_partial_cumulative(
      partial = partial,
      levels = levels,
      sequence = sequence_info$sequence
    ),
    localize = localize,
    score_diagnostics = run$score_diagnostics,
    local_strain = dplyr::bind_rows(lapply(run$score_diagnostics, function(x) x$table)),
    estimator = if (is.null(prepared$estimator_requested)) {
      NA_character_
    } else {
      prepared$estimator_requested
    },
    estimator_source = prepared$estimator_source,
    missing = prepared$missing,
    ID.fac = prepared$ID.fac,
    ID.cat = if (ordered_used) prepared$ID.cat else NA_character_,
    parameterization = if (ordered_used) prepared$parameterization else NA_character_,
    syntax = run$syntax,
    syntax_text = run$syntax_text,
    fits = run$fits,
    fit_measures = run$fit_measures,
    fit_evidence = fit_evidence,
    latent_means = latent_means,
    engine_warnings = run$engine_warnings,
    comparison_warnings = run$comparison_warnings,
    decision_log = decision_log,
    guidance = guidance
  )
  class(out) <- c("nomo_invariance_longitudinal", "nomo_invariance", "list")
  out
}


# The factors and items of the one-occasion model, which may contain only free
# loadings: each is repeated at every occasion under the occasion's name.
nomo_invariance_longitudinal_structure <- function(model) {
  partable <- nomo_network_model_table(model)
  user <- partable[partable$user == 1L, , drop = FALSE]
  if (!nrow(user) || any(user$op != "=~") || any(user$rhs %in% user$lhs)) {
    stop(
      "`model` must be a measurement model for one occasion: factors and their items only.",
      call. = FALSE
    )
  }
  if (grepl("*", model, fixed = TRUE)) {
    stop(
      "`model` cannot fix or label loadings; the invariance levels constrain them.",
      call. = FALSE
    )
  }
  tibble::tibble(factor = user$lhs, item = user$rhs)
}


# Latent change: at each level holding intercepts equal, each later occasion's
# latent mean. Under std.lv the first occasion's latent mean is 0 and its
# variance 1, so the means are changes in its latent standard deviations.
nomo_invariance_longitudinal_means <- function(fits, constraints, fit_evidence, ID.fac,
                                               long_factors, occasions) {
  out <- tibble::tibble(
    level = character(),
    occasion = character(),
    reference_occasion = character(),
    factor = character(),
    estimate = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    p_value = numeric()
  )
  if (!identical(tolower(ID.fac), "std.lv")) return(out)

  converged <- fit_evidence$level[fit_evidence$converged %in% TRUE]
  rows <- lapply(converged, function(level) {
    if (!"intercepts" %in% constraints[[level]]) return(NULL)
    pe <- lavaan::parameterEstimates(fits[[level]], ci = TRUE)
    dplyr::bind_rows(lapply(names(long_factors), function(f) {
      later <- long_factors[[f]][-1L]
      hit <- pe[pe$op == "~1" & pe$lhs %in% later, , drop = FALSE]
      hit <- hit[match(later, hit$lhs), , drop = FALSE]
      tibble::tibble(
        level = level,
        occasion = occasions[-1L],
        reference_occasion = occasions[[1L]],
        factor = f,
        estimate = hit$est,
        se = hit$se,
        ci_lower = hit$ci.lower,
        ci_upper = hit$ci.upper,
        p_value = hit$pvalue
      )
    }))
  })
  dplyr::bind_rows(out, rows)
}


nomo_invariance_longitudinal_residual_log <- function(auto) {
  nomo_log_add(
    nomo_log_new(), stage = "invariance", object = "unique_factors",
    metric = "residual_autocorrelation",
    reference = "Widaman, Ferrer, & Conger (2010)",
    severity = "info",
    observation = if (identical(auto, "all")) {
      "Each item's unique factor is correlated with its own on every other occasion."
    } else {
      sprintf(
        "Each item's unique factor is correlated with its own on occasions up to %d apart.",
        as.integer(auto)
      )
    },
    recommendation = paste(
      "What an item measures besides the construct is usually stable too;",
      "leaving these covariances out biases the estimates."
    )
  )
}


nomo_invariance_longitudinal_means_log <- function(latent_means) {
  log <- nomo_log_new()
  if (!nrow(latent_means)) return(log)
  # The most constrained level with latent means, the one change would usually
  # be reported from.
  level <- latent_means$level[[nrow(latent_means)]]
  rows <- latent_means[latent_means$level == level, , drop = FALSE]
  number <- function(x) nomo_present_number(x, 2L)
  nomo_log_add(
    log, stage = "invariance", object = "latent_means",
    metric = "latent_change",
    value = nrow(rows),
    reference = paste(
      "Latent change is comparable only with invariant intercepts, fully or",
      "partially (Widaman, Ferrer, & Conger, 2010)"
    ),
    severity = "info",
    observation = sprintf(
      "At the %s level, latent means relative to %s, in its latent standard deviations: %s.",
      level, rows$reference_occasion[[1L]],
      paste0(
        rows$factor, " at ", rows$occasion, " ", number(rows$estimate), " [",
        number(rows$ci_lower), ", ", number(rows$ci_upper), "]",
        collapse = "; "
      )
    ),
    recommendation = paste(
      "Compare the change with the one theory predicts. If the intercepts are",
      "not invariant, release those the local-strain table points to with",
      "nomo_partial() before interpreting the change."
    )
  )
}
