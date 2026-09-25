#' Audit item-level data before factor modeling
#'
#' `nomo_screen()` performs a conservative audit of candidate item data before
#' factor-retention, EFA, or CFA decisions are made. It summarizes item storage
#' and observed response patterns, missingness, response concentration, and
#' case-level completeness. It also creates a decision log that distinguishes
#' observations from recommendations.
#'
#' The function never removes rows or items, changes scores, reverse-keys items,
#' or decides whether a scale is valid.
#'
#' @param data A data frame containing candidate items.
#' @param items Optional character vector identifying item columns. If `NULL`,
#'   all columns are audited and the decision log reminds the user to verify that
#'   identifiers, demographics, and other non-item columns were not included.
#'   May also be a handoff from `contentvalidR`'s `content_handoff()`; see
#'   **Items from content review**.
#' @param guidance Guidance settings from [nomo_defaults()].
#' @param effort Logical. If `TRUE`, case-level indices of careless or
#'   insufficient-effort responding are added. See **Careless responding**.
#' @param scales Optional named list of character vectors assigning items to
#'   scales. Needed for even-odd consistency and for the within-scale versions
#'   of long-string and inter-item standard deviation.
#' @param reverse Optional character vector naming reverse-keyed items. Used
#'   only to recode an internal copy for the indices that need it; the data is
#'   never recoded.
#' @param scale_range Numeric `c(min, max)` of the response scale. Required
#'   whenever `reverse` is supplied, and never inferred from the data.
#' @param pair_magnitude Minimum absolute between-person correlation for an
#'   item pair to count as a psychometric antonym or synonym. Curran (2016)
#'   suggests .60 while saying there is no firm basis for it, so it is an
#'   argument rather than a constant.
#'
#' @details
#' Item-type labels are descriptive, not modeling decisions. In particular,
#' `numeric_discrete` means that the observed numeric values are integer-like
#' with 10 or fewer distinct observed values. It does **not** automatically mean
#' that the item should be treated as ordinal in later analyses.
#'
#' A `constant` item has only one distinct observed value. An `all_missing` item
#' has no observed values. These are hard data conditions rather than
#' psychometric cutoff rules.
#'
#' Response concentration and near-zero-variance flags use configurable
#' teaching references from [nomo_defaults()]. They are screening heuristics,
#' not psychometric laws or automatic item-retention rules. Ordered and
#' numeric-discrete items also receive descriptive boundary concentration
#' summaries. Continuous-like numeric indicators receive descriptive skewness
#' and excess-kurtosis summaries without a pass/fail normality judgment.
#'
#' **Careless responding.** With `effort = TRUE`, each case receives the indices
#' Meade and Craig (2012), Huang et al. (2012), and Curran (2016) describe:
#' long-string, inter-item standard deviation (Marjanovic et al., 2015),
#' Mahalanobis distance, even-odd consistency, and psychometric antonym and
#' synonym correlations. Curran recommends these be used in series because each
#' has blind spots, and the output is built to show where they disagree rather
#' than to combine them into one score.
#'
#' They disagree for a reason. Inter-item standard deviation detects random
#' responding and gives a respondent who answers every item identically the
#' best possible score; long-string detects exactly that respondent. When the
#' two disagree about a case, the decision log says so.
#'
#' A case is flagged only where a source states a rule, and each flag carries
#' the source's own qualification: long-string at half the number of items,
#' which Curran offers as a conservative starting point and says is not the best
#' cut score for every scale, and a positive antonym or negative synonym
#' correlation. The other indices have no stated cut score and are reported
#' without a flag. Huang et al. found the indices they recommended identified
#' attentive respondents well and random responders poorly, so an unflagged case
#' is not thereby shown to be attentive.
#'
#' Each respondent's antonym, synonym, and even-odd value is a correlation whose
#' N is the number of pairs or scales. With two, every value is exactly +1 or
#' -1, so at least three are required; with fewer than five the log says the
#' flags are coarse. Cases are flagged, never removed.
#'
#' **Items from content review.** `items` may be the handoff that
#' `contentvalidR`'s `content_handoff()` produces after content review. Only
#' items it marks as carried are screened. Every item it held back is listed in
#' the decision log with its status and recommendation quoted in
#' `contentvalidR`'s own words, and is never analyzed or reinstated here. The log
#' also records the producing version, workflow, and carry rule, and that item
#' membership came from content review rather than from these data.
#'
#' A carried item that is not a column of `data` is refused, never dropped.
#' Where the call leaves `scales`, `reverse`, or `scale_range` unset, the
#' handoff's scales and declared keying fill them. An item is never treated as
#' forward keyed because keying was undeclared, and a response scale the
#' handoff did not record is never inferred. A handoff with a schema version
#' this release does not read is refused, naming both package versions. The
#' interface is specified in nomologR issue #46, and `contentvalidR` is not
#' needed to read it.
#'
#' @return An object of class `nomo_screen` containing item summaries, response
#'   distributions, case-level completeness diagnostics, an evidence-guided
#'   decision log, and the guidance settings used.
#'
#' @references
#' Curran, P. G. (2016). Methods for the detection of carelessly invalid
#' responses in survey data. *Journal of Experimental Social Psychology, 66*,
#' 4-19. \doi{10.1016/j.jesp.2015.07.006}
#'
#' Huang, J. L., Curran, P. G., Keeney, J., Poposki, E. M., & DeShon, R. P.
#' (2012). Detecting and deterring insufficient effort responding to surveys.
#' *Journal of Business and Psychology, 27*(1), 99-114.
#' \doi{10.1007/s10869-011-9231-8}
#'
#' Marjanovic, Z., Holden, R., Struthers, W., Cribbie, R., & Greenglass, E.
#' (2015). The inter-item standard deviation (ISD): An index that discriminates
#' between conscientious and random responders. *Personality and Individual
#' Differences, 84*, 79-83. \doi{10.1016/j.paid.2014.08.021}
#'
#' Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in
#' survey data. *Psychological Methods, 17*(3), 437-455.
#' \doi{10.1037/a0028085}
#'
#' Clark, L. A., & Watson, D. (2019). Constructing validity: New developments
#' in creating objective measuring instruments. *Psychological Assessment,
#' 31*(12), 1412-1427. \doi{10.1037/pas0000626}
#'
#' Kuhn, M., & Johnson, K. (2013). *Applied predictive modeling*. Springer.
#' \doi{10.1007/978-1-4614-6849-3}
#'
#' Nunnally, J. C., & Bernstein, I. H. (1994). *Psychometric theory* (3rd ed.).
#' McGraw-Hill.
#'
#' @examples
#' dat <- data.frame(
#'   item1 = c(1, 2, 3, 4, 5),
#'   item2 = c(1, 2, NA, 4, 5),
#'   item3 = c(3, 3, 3, 3, 3)
#' )
#'
#' out <- nomo_screen(dat)
#' out$item_summary
#' out$decision_log
#'
#' # Simulated scale-development data with known teaching features
#' scr <- nomo_screen(nomo_demo_continuous)
#' summary(scr)
#'
#' @export
nomo_screen <- function(data,
                        items = NULL,
                        guidance = nomo_defaults(),
                        effort = FALSE,
                        scales = NULL,
                        reverse = NULL,
                        scale_range = NULL,
                        pair_magnitude = 0.60) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }

  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }

  if (ncol(data) == 0L) {
    stop("`data` must contain at least one column.", call. = FALSE)
  }

  if (!is.list(guidance)) {
    stop("`guidance` must be a list, typically returned by `nomo_defaults()`.", call. = FALSE)
  }

  # A contentvalidR handoff supplies the carried items, and, where the call
  # leaves them unset, its scales and declared keying (#46). Arguments given in
  # the call are the researcher's and take precedence; the log says so.
  handoff <- NULL
  keying_override <- FALSE
  if (nomo_handoff_is(items)) {
    handoff <- nomo_handoff_read(items)
    nomo_handoff_check_data(handoff, names(data))
    items <- handoff$items
    if (is.null(scales)) scales <- handoff$scales
    keying_override <- handoff$keying$declared &&
      (!is.null(reverse) || !is.null(scale_range))
    if (is.null(reverse)) reverse <- handoff$keying$reverse
    if (is.null(scale_range)) scale_range <- handoff$keying$scale_range
  }

  used_all_columns <- is.null(items)

  if (used_all_columns) {
    items <- names(data)
  } else {
    if (!is.character(items) || length(items) == 0L) {
      stop("`items` must be `NULL` or a non-empty character vector.", call. = FALSE)
    }

    if (anyNA(items) || any(items == "")) {
      stop("`items` cannot contain missing or empty names.", call. = FALSE)
    }

    if (anyDuplicated(items)) {
      stop("`items` must not contain duplicate names.", call. = FALSE)
    }

    missing_items <- setdiff(items, names(data))
    if (length(missing_items) > 0L) {
      stop(
        sprintf(
          "Unknown item column%s: %s.",
          if (length(missing_items) == 1L) "" else "s",
          paste(missing_items, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  selected <- data[items]

  effort_args <- nomo_screen_effort_args(
    effort = effort, selected = selected, items = items, scales = scales,
    reverse = reverse, scale_range = scale_range,
    pair_magnitude = pair_magnitude
  )

  item_summary <- dplyr::bind_rows(
    lapply(items, function(item) {
      nomo_screen_item_summary(selected[[item]], item)
    })
  )

  response_distribution <- dplyr::bind_rows(
    lapply(items, function(item) {
      nomo_screen_distribution(selected[[item]], item)
    })
  )

  descriptives <- nomo_screen_descriptives(
    selected = selected,
    item_summary = item_summary,
    guidance = guidance
  )
  item_summary <- descriptives$item_summary

  missing_matrix <- is.na(selected)
  n_missing_case <- rowSums(missing_matrix)
  n_items <- length(items)

  case_summary <- tibble::tibble(
    row = seq_len(nrow(data)),
    n_missing = as.integer(n_missing_case),
    pct_missing = as.numeric(n_missing_case / n_items),
    complete = n_missing_case == 0L,
    all_missing = n_missing_case == n_items
  )

  relationships <- nomo_screen_relationships(
    selected = selected,
    item_summary = item_summary,
    guidance = guidance
  )

  decision_log <- nomo_log_new()

  if (used_all_columns) {
    decision_log <- nomo_log_add(
      decision_log,
      stage = "screen",
      object = "item_selection",
      metric = "all_columns_selected",
      value = length(items),
      severity = "info",
      observation = paste(
        "Because `items = NULL`, all columns in `data` were audited as candidate items."
      ),
      recommendation = paste(
        "Verify that identifiers, demographics, grouping variables, and other",
        "non-item columns are not being interpreted as scale items."
      )
    )
  }

  for (i in seq_len(nrow(item_summary))) {
    row <- item_summary[i, , drop = FALSE]
    item <- row$item[[1L]]

    if (isTRUE(row$all_missing[[1L]])) {
      decision_log <- nomo_log_add(
        decision_log,
        stage = "screen",
        object = item,
        metric = "all_missing",
        value = row$pct_missing[[1L]],
        reference = "No observed responses",
        severity = "concern",
        observation = sprintf("`%s` contains no observed responses.", item),
        recommendation = paste(
          "Inspect data import, skip logic, eligibility rules, and variable coding",
          "before using this item in later psychometric analyses."
        )
      )
      next
    }

    if (isTRUE(row$constant[[1L]])) {
      decision_log <- nomo_log_add(
        decision_log,
        stage = "screen",
        object = item,
        metric = "constant",
        value = row$n_unique[[1L]],
        reference = "At least two observed values are required for variance/covariance",
        severity = "concern",
        observation = sprintf(
          "`%s` has only one distinct observed value and therefore has zero observed variance.",
          item
        ),
        recommendation = paste(
          "Inspect coding and data provenance. Do not delete the item automatically;",
          "document why it is constant in this sample before deciding what to do."
        )
      )
    }

    if (row$n_missing[[1L]] > 0L) {
      decision_log <- nomo_log_add(
        decision_log,
        stage = "screen",
        object = item,
        metric = "missingness",
        value = row$pct_missing[[1L]],
        reference = "Descriptive only; no universal deletion threshold",
        severity = "info",
        observation = sprintf(
          "`%s` has %d missing response%s (%.1f%%).",
          item,
          row$n_missing[[1L]],
          if (row$n_missing[[1L]] == 1L) "" else "s",
          100 * row$pct_missing[[1L]]
        ),
        recommendation = paste(
          "Inspect the pattern and cause of missingness before choosing a later",
          "missing-data strategy."
        )
      )
    }

    item_type <- row$item_type[[1L]]

    if (item_type %in% c("nominal", "text", "other")) {
      decision_log <- nomo_log_add(
        decision_log,
        stage = "screen",
        object = item,
        metric = "item_type",
        value = NA_real_,
        reference = "Later factor models require an intentional measurement scale",
        severity = "review",
        observation = sprintf(
          "`%s` is stored as %s and was classified descriptively as `%s`.",
          item,
          row$storage[[1L]],
          item_type
        ),
        recommendation = paste(
          "Verify whether this column is truly a scale item and whether its coding",
          "should be changed intentionally before factor modeling."
        )
      )
    } else if (item_type == "numeric_discrete") {
      decision_log <- nomo_log_add(
        decision_log,
        stage = "screen",
        object = item,
        metric = "item_type",
        value = row$n_unique[[1L]],
        reference = "Descriptive classification only",
        severity = "info",
        observation = sprintf(
          "`%s` has integer-like numeric responses with %d distinct observed values.",
          item,
          row$n_unique[[1L]]
        ),
        recommendation = paste(
          "Do not infer ordinal versus continuous treatment from storage alone;",
          "make that modeling decision explicitly in the factor-analysis stage."
        )
      )
    }
  }

  if (any(case_summary$all_missing)) {
    n_all_missing_cases <- sum(case_summary$all_missing)
    decision_log <- nomo_log_add(
      decision_log,
      stage = "screen",
      object = "cases",
      metric = "all_items_missing",
      value = n_all_missing_cases,
      reference = "No observed candidate-item responses",
      severity = "concern",
      observation = sprintf(
        "%d case%s have no observed responses on the selected candidate items.",
        n_all_missing_cases,
        if (n_all_missing_cases == 1L) "" else "s"
      ),
      recommendation = paste(
        "Inspect these cases and their study-flow context before deciding whether",
        "they belong in later analyses."
      )
    )
  }

  effort_result <- NULL
  effort_log <- NULL
  if (isTRUE(effort)) {
    effort_result <- nomo_effort_screen(
      selected,
      scales = effort_args$scales,
      reverse = effort_args$reverse,
      scale_range = effort_args$scale_range,
      pair_magnitude = pair_magnitude
    )
    effort_log <- nomo_effort_log(effort_result, n_items = length(items))
  }

  handoff_log <- NULL
  if (!is.null(handoff)) {
    handoff_log <- nomo_handoff_log(handoff)
    if (keying_override) {
      handoff_log <- nomo_log_add(
        handoff_log, stage = "screen", object = "content_review",
        metric = "keying_override", severity = "info",
        observation = paste(
          "`reverse` or `scale_range` was supplied in the call, so it was used",
          "in place of the keying declared in the handoff."
        ),
        recommendation = "Record why the declared keying was set aside."
      )
    }
  }

  decision_log <- dplyr::bind_rows(
    handoff_log,
    decision_log,
    descriptives$decision_log,
    relationships$decision_log,
    effort_log
  )

  out <- list(
    call = match.call(),
    n_cases = nrow(data),
    items = items,
    item_summary = item_summary,
    response_distribution = response_distribution,
    case_summary = case_summary,
    relationship_summary = relationships$relationship_summary,
    inter_item_correlations = relationships$inter_item_correlations,
    relationship_method = relationships$relationship_method,
    decision_log = decision_log,
    guidance = guidance
  )

  # Added only when requested, so a screen that did not ask for careless-
  # responding indices has exactly the shape it always had.
  if (!is.null(effort_result)) {
    tail_names <- c("decision_log", "guidance")
    head <- out[setdiff(names(out), tail_names)]
    out <- c(
      head,
      list(
        effort = effort_result$effort,
        effort_pairs = list(
          antonym = effort_result$antonym_pairs,
          synonym = effort_result$synonym_pairs
        ),
        effort_settings = list(
          scales = effort_args$scales,
          reverse = effort_args$reverse,
          scale_range = effort_args$scale_range,
          pair_magnitude = pair_magnitude,
          long_string_limit = effort_result$long_string_limit
        )
      ),
      out[tail_names]
    )
  }

  # Also added only when present, for the same reason.
  if (!is.null(handoff)) {
    tail_names <- c("decision_log", "guidance")
    out <- c(out[setdiff(names(out), tail_names)], list(handoff = handoff),
             out[tail_names])
  }

  class(out) <- c("nomo_screen", "list")
  out
}


#' Print a nomo_screen object
#'
#' @param x A `nomo_screen` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return `x`, invisibly.
#' @export
print.nomo_screen <- function(x, ...) {
  n_items <- length(x$items)
  n_missing_items <- sum(x$item_summary$n_missing > 0L)
  n_constant <- sum(x$item_summary$constant)
  n_all_missing <- sum(x$item_summary$all_missing)

  cat("<nomo_screen>\n")
  cat(sprintf("Cases: %d | Candidate items: %d\n", x$n_cases, n_items))
  cat(sprintf(
    "Items with missing responses: %d | Constant: %d | All missing: %d\n",
    n_missing_items,
    n_constant,
    n_all_missing
  ))

  if (!is.null(x$relationship_summary)) {
    n_relationship_eligible <-
      sum(x$relationship_summary$relationship_eligible)

    n_item_rest <-
      sum(!is.na(x$relationship_summary$corrected_item_rest_r))

    cat(sprintf(
      "Relationship diagnostics: %d eligible items | %d item-rest estimates\n",
      n_relationship_eligible,
      n_item_rest
    ))
  }

  n_concentration <- sum(
    x$decision_log$metric %in% c(
      "response_concentration",
      "floor_concentration",
      "ceiling_concentration"
    )
  )
  n_nzv <- sum(x$item_summary$near_zero_variance)
  cat(sprintf(
    "Response concentration flags: %d | Near-zero variance: %d\n",
    n_concentration,
    n_nzv
  ))

  if (!is.null(x$effort) && nrow(x$effort)) {
    e <- x$effort
    flagged <- sum(e$n_flags > 0L)
    by_rule <- vapply(
      c(long_string = "flag_long_string", antonym = "flag_antonym",
        synonym = "flag_synonym"),
      function(col) sum(e[[col]]),
      integer(1)
    )
    cat(sprintf(
      "Careless-responding flags: %d case%s (long-string %d | antonym %d | synonym %d)\n",
      flagged, if (flagged == 1L) "" else "s",
      by_rule[["long_string"]], by_rule[["antonym"]], by_rule[["synonym"]]
    ))
    cat("  Cases are flagged, never removed. Indices disagree by design; see the decision log.\n")
  }

  if (nrow(x$decision_log) > 0L) {
    severity_counts <- table(
      factor(
        x$decision_log$severity,
        levels = c("info", "review", "concern")
      )
    )
    cat(sprintf(
      "Decision log: %d info | %d review | %d concern\n",
      severity_counts[["info"]],
      severity_counts[["review"]],
      severity_counts[["concern"]]
    ))
  } else {
    cat("Decision log: no entries\n")
  }

  cat("No rows or items were removed or modified.\n")
  invisible(x)
}


nomo_screen_item_summary <- function(x, item) {
  n <- length(x)
  observed <- x[!is.na(x)]
  n_observed <- length(observed)
  n_missing <- n - n_observed
  n_unique <- if (n_observed == 0L) 0L else length(unique(observed))
  all_missing <- n_observed == 0L
  constant <- !all_missing && n_unique == 1L
  item_type <- nomo_screen_item_type(x)

  mode_n <- 0L
  mode_prop <- NA_real_

  if (!all_missing) {
    counts <- table(observed, useNA = "no")
    mode_n <- max(as.integer(counts))
    mode_prop <- mode_n / n_observed
  }

  numeric_observed <- if (is.numeric(x)) observed else numeric(0)

  tibble::tibble(
    item = item,
    storage = paste(class(x), collapse = "/"),
    item_type = item_type,
    n = as.integer(n),
    n_observed = as.integer(n_observed),
    n_missing = as.integer(n_missing),
    pct_missing = as.numeric(n_missing / n),
    n_unique = as.integer(n_unique),
    mode_n = as.integer(mode_n),
    mode_prop = as.numeric(mode_prop),
    min = if (length(numeric_observed) > 0L) min(numeric_observed) else NA_real_,
    max = if (length(numeric_observed) > 0L) max(numeric_observed) else NA_real_,
    mean = if (length(numeric_observed) > 0L) mean(numeric_observed) else NA_real_,
    sd = if (length(numeric_observed) > 1L) stats::sd(numeric_observed) else NA_real_,
    constant = constant,
    all_missing = all_missing
  )
}


nomo_screen_item_type <- function(x) {
  observed <- x[!is.na(x)]

  if (length(observed) == 0L) {
    return("empty")
  }

  n_unique <- length(unique(observed))

  if (is.logical(x)) {
    return("binary")
  }

  if (is.ordered(x)) {
    if (length(levels(x)) <= 2L) {
      return("binary")
    }
    return("ordered")
  }

  if (is.factor(x)) {
    if (length(levels(x)) <= 2L) {
      return("binary")
    }
    return("nominal")
  }

  if (is.numeric(x)) {
    if (n_unique <= 2L) {
      return("binary")
    }

    finite <- is.finite(observed)
    integer_like <- all(finite) &&
      all(abs(observed - round(observed)) < sqrt(.Machine$double.eps))

    if (integer_like && n_unique <= 10L) {
      return("numeric_discrete")
    }

    return("numeric_continuous")
  }

  if (is.character(x)) {
    if (n_unique <= 2L) {
      return("binary")
    }
    return("text")
  }

  "other"
}


nomo_screen_distribution <- function(x, item) {
  n_total <- length(x)
  n_missing <- sum(is.na(x))
  n_observed <- n_total - n_missing

  if (is.factor(x)) {
    lev <- levels(x)
    counts <- as.integer(table(factor(x, levels = lev), useNA = "no"))

    out <- tibble::tibble(
      item = item,
      response = as.character(lev),
      n = counts,
      proportion_observed = if (n_observed > 0L) {
        counts / n_observed
      } else {
        NA_real_
      },
      proportion_total = counts / n_total,
      missing = FALSE
    )
  } else {
    observed <- x[!is.na(x)]

    if (length(observed) == 0L) {
      out <- tibble::tibble(
        item = character(),
        response = character(),
        n = integer(),
        proportion_observed = numeric(),
        proportion_total = numeric(),
        missing = logical()
      )
    } else {
      response_chr <- as.character(observed)
      response_levels <- unique(response_chr)

      if (is.numeric(x)) {
        numeric_levels <- sort(unique(observed))
        response_levels <- as.character(numeric_levels)
      } else if (is.logical(x)) {
        response_levels <- intersect(
          c("FALSE", "TRUE"),
          unique(response_chr)
        )
      } else {
        response_levels <- sort(response_levels)
      }

      counts <- vapply(
        response_levels,
        function(value) sum(response_chr == value),
        integer(1)
      )

      out <- tibble::tibble(
        item = item,
        response = response_levels,
        n = as.integer(counts),
        proportion_observed = as.numeric(counts / n_observed),
        proportion_total = as.numeric(counts / n_total),
        missing = FALSE
      )
    }
  }

  if (n_missing > 0L) {
    out <- dplyr::bind_rows(
      out,
      tibble::tibble(
        item = item,
        response = NA_character_,
        n = as.integer(n_missing),
        proportion_observed = NA_real_,
        proportion_total = as.numeric(n_missing / n_total),
        missing = TRUE
      )
    )
  }

  out
}



# Validates the careless-responding arguments before any index is computed.
# Keying and response range are never inferred: a reverse-keyed item recoded
# against a range guessed from the data is recoded wrongly whenever a category
# went unused, and the index would then report carelessness that is not there.
nomo_screen_effort_args <- function(effort, selected, items, scales, reverse,
                                    scale_range, pair_magnitude) {
  if (!is.logical(effort) || length(effort) != 1L || is.na(effort)) {
    stop("`effort` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!isTRUE(effort)) {
    return(list(scales = NULL, reverse = NULL, scale_range = NULL))
  }

  numeric_items <- vapply(selected, is.numeric, logical(1))
  if (!all(numeric_items)) {
    stop(
      paste0(
        "Careless-responding indices need numeric responses. Not numeric: ",
        paste(items[!numeric_items], collapse = ", "), ". Convert the ",
        "responses to their numeric codes, or leave `effort = FALSE`."
      ),
      call. = FALSE
    )
  }

  if (!is.numeric(pair_magnitude) || length(pair_magnitude) != 1L ||
        !is.finite(pair_magnitude) || pair_magnitude <= 0 || pair_magnitude >= 1) {
    stop("`pair_magnitude` must be a single number between 0 and 1.", call. = FALSE)
  }

  if (!is.null(scales)) {
    if (!is.list(scales) || !length(scales) ||
          !all(vapply(scales, is.character, logical(1)))) {
      stop("`scales` must be a list of character vectors of item names.", call. = FALSE)
    }
    unknown <- setdiff(unlist(scales, use.names = FALSE), items)
    if (length(unknown)) {
      stop(
        paste0("`scales` names item(s) not being screened: ",
               paste(unknown, collapse = ", "), "."),
        call. = FALSE
      )
    }
  }

  if (!is.null(reverse)) {
    if (!is.character(reverse)) {
      stop("`reverse` must be a character vector of item names.", call. = FALSE)
    }
    unknown <- setdiff(reverse, items)
    if (length(unknown)) {
      stop(
        paste0("`reverse` names item(s) not being screened: ",
               paste(unknown, collapse = ", "), "."),
        call. = FALSE
      )
    }
    if (length(reverse) && is.null(scale_range)) {
      stop(
        paste(
          "Recoding reverse-keyed items needs the response scale's minimum and",
          "maximum, supplied as `scale_range = c(min, max)`. It is not inferred",
          "from the data, because an unused category would make the inferred",
          "range wrong and every recoded response with it."
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(scale_range)) {
    if (!is.numeric(scale_range) || length(scale_range) != 2L ||
          anyNA(scale_range) || scale_range[[1L]] >= scale_range[[2L]]) {
      stop("`scale_range` must be `c(min, max)` with min below max.", call. = FALSE)
    }
  }

  list(scales = scales, reverse = reverse, scale_range = scale_range)
}


#' @export
nomo_table.nomo_screen <- function(x,
                                   type = c(
                                     "items", "distribution", "cases",
                                     "relationships", "effort", "decision_log"
                                   ),
                                   ...) {
  type <- match.arg(type)
  if (type == "items") return(x$item_summary)
  if (type == "distribution") return(x$response_distribution)
  if (type == "cases") return(x$case_summary)
  if (type == "relationships") return(x$relationship_summary)
  if (type == "effort") {
    if (is.null(x$effort)) {
      stop(
        "No careless-responding indices were computed. Rerun with `effort = TRUE`.",
        call. = FALSE
      )
    }
    return(x$effort)
  }
  x$decision_log
}
