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
#' @param guidance Guidance settings from [nomo_defaults()]. A list that sets
#'   `auto_delete` or `auto_respecify` to `TRUE` is refused, since nomologR
#'   never deletes an item on its own.
#' @param effort Logical. If `TRUE`, case-level indices of careless or
#'   insufficient-effort responding are added. See **Careless responding**.
#' @param scales Optional named list of character vectors assigning items to
#'   scales. Needed for even-odd consistency and for the within-scale versions
#'   of long-string and inter-item standard deviation. With two or more scales,
#'   each item's corrected item-rest correlation is also computed against the
#'   rest of its own scale, and the item review uses that value. Negative
#'   inter-item correlations are then reviewed only within a scale, since items
#'   of different constructs need not correlate positively. A `contentvalidR`
#'   handoff supplies its scales here.
#' @param reverse Optional character vector naming reverse-keyed items, each of
#'   which must be an item being screened. Used only to recode an internal copy:
#'   for the careless-responding indices that need it, and to say whether a
#'   declared item's negative item-rest correlation is the sign expected before
#'   recoding. The data is never recoded.
#' @param scale_range Numeric `c(min, max)` of the response scale, with `min`
#'   below `max`. It is needed to use `reverse` and is never inferred from the
#'   data. With `effort = TRUE`, naming reverse-keyed items without it is
#'   refused. With `effort = FALSE` the audit runs, and the decision log
#'   records that the declared keying was not used. When it is given, each
#'   numeric item's responses are compared with it: an item with responses
#'   outside it gets a concern in the decision log, and with `effort = TRUE` a
#'   reverse-keyed item with responses outside it is refused, since recoding
#'   it on that scale would give wrong values. It also sets the floor and
#'   ceiling of numeric-discrete items (see Details).
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
#' summaries, `floor_prop` and `ceiling_prop`. An ordered item's boundaries are
#' its first and last levels. A numeric-discrete item's are the ends of
#' `scale_range` when it is given, and otherwise its lowest and highest observed
#' values, since a numeric item shows only the values used: a pile-up in the
#' middle of a scale whose lower categories went unused would read as a floor
#' effect, and the decision log says so. Continuous-like numeric indicators
#' receive descriptive skewness and excess-kurtosis summaries without a
#' pass/fail normality judgment.
#'
#' An item in a declared scale is reviewed on its item-rest correlation within
#' that scale. When it has none there, because it is the only item of its scale
#' in the diagnostics or too few cases are complete on the scale's items, it is
#' not reviewed on the pooled value, which would judge it against other
#' constructs; the decision log says why there is no value.
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
#' Long-string reads the items in the order given: the order of `items`, or
#' of the columns of `data` when `items` is `NULL`, which should be the order
#' in which they were administered. A `contentvalidR` handoff of schema version
#' 1 records no administration order, so its carried items are read in the
#' order the handoff lists them, which need not be the order of administration.
#'
#' The long-string rule is applied only when at least
#' `guidance$long_string_min_items` items are screened (20 in
#' [nomo_defaults()]). Half the length of a shorter item set is a run of a few
#' responses, which attentive respondents give often: with two items every case
#' reaches it. On a shorter set each case's longest run is still reported in
#' `long_string`, no case is flagged on it, and the decision log states the
#' number of items and the share of cases that reached half the length.
#'
#' Each respondent's antonym, synonym, and even-odd value is a correlation whose
#' N is the number of pairs or scales. With two, every value is exactly +1 or
#' -1, so at least three are required; with fewer than five the log says the
#' flags are coarse. The same holds for each respondent: one who answered fewer
#' than three of the pairs, or both halves of fewer than three scales, has no
#' value. Even-odd consistency is Spearman-Brown corrected, and the correction
#' has no meaning below -1, so the value is bounded there: it lies between -1
#' and 1. Cases are flagged, never removed.
#'
#' **Items from content review.** `items` may be the handoff that
#' `contentvalidR`'s `content_handoff()` produces after content review. Only
#' items it marks as carried are screened. Every item it held back is listed in
#' the decision log with its status and recommendation quoted in
#' `contentvalidR`'s own words, and is never analyzed or reinstated here. The log
#' also records the producing version, workflow, and carry rule, and that the
#' items came from content review rather than from these data, as did their
#' construct membership when the review assigned one.
#'
#' A carried item that is not a column of `data` is refused, never dropped, and
#' a handoff that carries no items is refused with the review's status counts.
#' Where the call leaves `scales`, `reverse`, or `scale_range` unset, the
#' handoff's scales and declared keying fill them. The log records what the
#' call changed: `scales` that replace the handoff's, a `reverse` or
#' `scale_range` that replaces the declared keying, and a `scale_range` that
#' completes keying declared without a response scale. An item is never
#' treated as forward keyed because keying was undeclared, and a response scale
#' the handoff did not record is never inferred. A handoff with a schema version
#' this release does not read is refused, naming both package versions. The
#' interface is specified in nomologR issue #46, and `contentvalidR` is not
#' needed to read it.
#'
#' @return An object of class `nomo_screen`. The fields to read are:
#'
#'   * `items`: the items screened, and `n_cases`, the number of rows.
#'   * `item_summary`: one row per item, with its storage, inferred type,
#'     missingness, most common response, descriptive statistics, and
#'     near-zero-variance indicators.
#'   * `response_distribution`: counts and proportions of each response.
#'   * `case_summary`: missingness per row.
#'   * `relationship_summary`: each item's corrected item-rest correlation and
#'     summary of its inter-item correlations. With two or more declared
#'     scales, `scale`, `scale_item_rest_r`, and `scale_item_rest_n` give each
#'     item's scale and its item-rest correlation within that scale, and
#'     `scale_negative_interitem_n` counts the negative correlations the review
#'     reads: with items of the same scale, and with any screened item that is
#'     outside every declared scale. Otherwise they are `NA`.
#'   * `inter_item_correlations`: one row per item pair.
#'   * `decision_log`: the evidence and its explanations (see [nomo_table()]).
#'   * `effort`, `effort_pairs`, and `effort_settings`: the careless-responding
#'     indices per row, the pairs they used, and their settings, when
#'     `effort = TRUE`. The settings include `long_string_rule_applied`, which
#'     is `FALSE` when too few items were screened for the long-string rule.
#'   * `handoff`: the content-review handoff read from `items`, when one was
#'     supplied.
#'
#'   `pct_missing`, in `item_summary` and `case_summary`, is a proportion
#'   between 0 and 1, as are the `*_prop` and `proportion_*` columns.
#'   `percent_unique` is a percentage, 0 to 100, as the near-zero-variance rule
#'   states it. See **Conventions in returned tables** in `?nomologR`.
#'
#'   Other fields record the call, the settings used, and intermediate engine
#'   results. They may change between releases and are not part of the stable
#'   interface (see `?nomologR`).
#'
#'   Printed, the object shows the counts of cases and items by data
#'   condition and how many decision-log entries are flagged. [summary()]
#'   shows each item's review in a table, with the reason for each flag in a
#'   Flagged section.
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
  nomo_defaults_check_safeguards(guidance)

  # A contentvalidR handoff supplies the carried items, and, where the call
  # leaves them unset, its scales and declared keying (#46). Arguments given in
  # the call are the researcher's and take precedence; the log records which
  # were replaced or completed, from what was actually used (#145).
  handoff <- NULL
  given <- list(scales = scales, reverse = reverse, scale_range = scale_range)
  if (nomo_handoff_is(items)) {
    handoff <- nomo_handoff_read(items)
    nomo_handoff_check_data(handoff, names(data))
    items <- handoff$items
    if (is.null(scales)) scales <- handoff$scales
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

  # A name shared by two columns cannot say which column is the item, whether
  # the items were selected or every column was (#145).
  repeated <- intersect(items, names(data)[duplicated(names(data))])
  if (length(repeated)) {
    stop(
      sprintf(
        paste(
          "`data` has more than one column named %s. Give each column a unique",
          "name before screening; nomologR does not choose between them."
        ),
        nomo_present_or(paste0("`", repeated, "`"))
      ),
      call. = FALSE
    )
  }

  selected <- data[items]

  effort_args <- nomo_screen_effort_args(
    effort = effort, selected = selected, items = items, scales = scales,
    reverse = reverse, scale_range = scale_range,
    pair_magnitude = pair_magnitude
  )
  long_string_min_items <- if (isTRUE(effort)) nomo_screen_long_string_min(guidance)

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
    guidance = guidance,
    scale_range = scale_range
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
    guidance = guidance,
    reverse = reverse,
    scale_range = scale_range,
    scales = scales
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

  # Declared keying is used only on a copy recoded against the response scale,
  # and the scale is never inferred. This is reached only without the effort
  # indices, which refuse the same call. Nothing is computed from the keying
  # here, so the audit proceeds and says the keying went unused, rather than
  # leaving its explanation silently absent.
  if (length(reverse) && is.null(scale_range)) {
    decision_log <- nomo_log_add(
      decision_log,
      stage = "screen",
      object = "item_keying",
      metric = "keying_not_used",
      value = length(reverse),
      reference = "The response scale is never inferred from the data",
      severity = "info",
      observation = sprintf(
        paste(
          "%s %s %s declared without `scale_range`, so the",
          "declared keying was not used: a negative item-rest correlation of",
          "such an item is reported without the keying explanation."
        ),
        nomo_present_noun(length(reverse), "Reverse-keyed item", "Reverse-keyed items"),
        paste(reverse, collapse = ", "),
        nomo_present_noun(length(reverse), "was", "were")
      ),
      recommendation = paste(
        "Supply `scale_range = c(min, max)` to have a declared item's negative",
        "item-rest correlation checked against its recoded value. With",
        "`effort = TRUE` the range is required."
      )
    )
  }

  # The declared response scale is compared with the data. A 0-based coding, a
  # missing-value code, or a different response format puts responses outside
  # it, and every value recoded on the wrong scale is wrong (#145).
  decision_log <- dplyr::bind_rows(
    decision_log, nomo_screen_range_log(selected, scale_range, reverse)
  )

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
          "`%s` has %s (%s).",
          item,
          nomo_present_count(row$n_missing[[1L]], "missing response"),
          nomo_present_percent(row$pct_missing[[1L]], base = row$n[[1L]])
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
        "%s no observed responses on the selected candidate items.",
        nomo_present_count(n_all_missing_cases, "case has", "cases have")
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
      pair_magnitude = pair_magnitude,
      long_string_min_items = long_string_min_items
    )
    effort_log <- nomo_effort_log(effort_result, n_items = length(items))
  }

  handoff_log <- NULL
  if (!is.null(handoff)) {
    handoff_log <- nomo_handoff_log(handoff, given = given)
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
          long_string_limit = effort_result$long_string_limit,
          long_string_min_items = effort_result$long_string_min_items,
          long_string_rule_applied = effort_result$long_string_rule_applied
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
#' `print()` shows the counts of cases and items by data condition, how many
#' items entered the correlation diagnostics, and how many decision-log
#' entries are flagged; [summary.nomo_screen()] shows each item's review.
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

  nomo_present_header("nomo_screen", "Item and data audit")
  nomo_present_facts(c(
    sprintf("Cases: %d", x$n_cases),
    sprintf("Candidate items: %d", n_items)
  ))
  nomo_present_facts(c(
    sprintf("Items with missing responses: %d", n_missing_items),
    sprintf("Constant: %d", n_constant),
    sprintf("All missing: %d", n_all_missing)
  ))

  if (!is.null(x$relationship_summary)) {
    rel <- x$relationship_summary
    nomo_present_facts(c(
      paste0("Items in correlation diagnostics: ",
             nomo_screen_of(sum(rel$relationship_eligible), n_items)),
      sprintf("Item-rest correlations: %d", sum(!is.na(rel$corrected_item_rest_r)))
    ))
    nomo_screen_present_skipped(x$item_summary, rel)
  }

  n_concentration <- sum(
    x$decision_log$metric %in% c(
      "response_concentration",
      "floor_concentration",
      "ceiling_concentration"
    )
  )
  nomo_present_facts(c(
    sprintf("Response concentration flags: %d", n_concentration),
    sprintf("Near-zero variance: %d", sum(x$item_summary$near_zero_variance))
  ))

  if (!is.null(x$effort) && nrow(x$effort)) {
    e <- x$effort
    flagged <- sum(e$n_flags > 0L)
    # On a short item set the long-string rule is not applied, and a count of
    # zero would read as a finding about the sample. A screen saved before the
    # setting existed applied the rule.
    long_applied <- !isFALSE(x$effort_settings$long_string_rule_applied)
    nomo_present_facts(c(
      paste0("Careless-responding flags: ", nomo_present_count(flagged, "case")),
      paste0("Long-string: ", if (long_applied) sum(e$flag_long_string) else "not applied"),
      sprintf("Antonym: %d", sum(e$flag_antonym)),
      sprintf("Synonym: %d", sum(e$flag_synonym))
    ))
    nomo_present_text(
      "Cases are flagged, never removed. Indices disagree by design; see the ",
      "decision log.", indent = 2L
    )
  }

  # Entries are counted by the display vocabulary: an informational entry is
  # not a flag (#145, cons-4).
  n_log <- nrow(x$decision_log)
  nomo_present_facts(c(
    paste0("Decision log: ", if (n_log) nomo_present_count(n_log, "entry", "entries") else "no entries"),
    if (n_log) paste0("Flagged: ", nomo_present_flag_counts(x$decision_log$severity))
  ))

  nomo_present_text("No rows or items were removed or modified.")
  nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"decision_log\")",
      if (!is.null(x$effort)) "nomo_table(x, \"effort\")"),
    c("each item's review", "every log entry",
      if (!is.null(x$effort)) "each case's indices")
  )
  invisible(x)
}


# "10", or "8 of 10" when not all were used.
nomo_screen_of <- function(k, n) {
  if (k < n) sprintf("%d of %d", k, n) else as.character(k)
}


# One line saying why items were left out of the correlation diagnostics when
# they are stored as categories: nomologR does not turn labels into scores, and
# without the line the counts read as a fault (#145, clarity-30).
nomo_screen_present_skipped <- function(item_summary, relationships) {
  skipped <- relationships$item[
    relationships$relationship_reason %in% "not_explicitly_scored_numeric"
  ]
  if (!length(skipped)) return(invisible(NULL))
  types <- item_summary$item_type[match(skipped, item_summary$item)]
  counts <- table(factor(types, levels = unique(types)))
  kinds <- nomo_present_or(paste(as.integer(counts), names(counts)), "and")
  nomo_present_text(
    sprintf(
      paste(
        "Correlations were not computed for %s %s: nomologR does not turn",
        "category labels into scores. Supply numeric scores to include %s."
      ),
      kinds,
      nomo_present_noun(length(skipped), "item", "items"),
      nomo_present_noun(length(skipped), "it", "them")
    ),
    indent = 2L
  )
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

      # Numeric responses are counted on their values and labeled afterwards:
      # two values that differ only past the 15 digits as.character() keeps
      # (0.3 and 0.1 + 0.2) would otherwise each be counted twice (#145).
      if (is.numeric(x)) {
        numeric_levels <- sort(unique(observed))
        counts <- tabulate(match(observed, numeric_levels), length(numeric_levels))
        response_levels <- as.character(numeric_levels)
      } else {
        response_levels <- if (is.logical(x)) {
          intersect(c("FALSE", "TRUE"), unique(response_chr))
        } else {
          sort(unique(response_chr))
        }
        counts <- vapply(
          response_levels,
          function(value) sum(response_chr == value),
          integer(1)
        )
      }

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

  # `scales` also sets the within-scale item-rest correlations, so it is
  # checked whether or not the effort indices are requested.
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

  # `reverse` and `scale_range` also explain a negative item-rest correlation
  # in the item audit, so they too are checked whether or not the effort
  # indices are requested. Unchecked, a misspelled item or a reversed range
  # was a silent no-op: the explanation never appeared and nothing said why.
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
  }

  if (!is.null(scale_range)) {
    if (!is.numeric(scale_range) || length(scale_range) != 2L ||
          !all(is.finite(scale_range)) || scale_range[[1L]] >= scale_range[[2L]]) {
      stop("`scale_range` must be `c(min, max)` with min below max.", call. = FALSE)
    }
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

  # The indices are computed on recoded responses, so reverse-keyed items with
  # no range are refused. Without the indices nothing is computed from the
  # keying, and the screen logs that it went unused (see nomo_screen()).
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

  # A reverse-keyed response outside the declared scale has no recoded value on
  # it: recoding 0 on a 1 to 5 scale gives 6. Either the data or the range is
  # wrong, and the indices are not computed from a guess about which (#145).
  outside <- nomo_screen_out_of_range(selected[reverse], scale_range)
  if (nrow(outside)) {
    stop(
      sprintf(
        paste(
          "Recoding reverse-keyed items needs every response inside the declared",
          "response scale, %s. Outside it: %s. Correct `scale_range`, or recode",
          "the data (a 0-based coding, or a missing-value code to `NA`), before",
          "computing the careless-responding indices."
        ),
        nomo_handoff_range_text(scale_range),
        paste(sprintf("%s (observed %s)", outside$item,
                      nomo_screen_range_text(outside$min, outside$max)),
              collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(scales = scales, reverse = reverse, scale_range = scale_range)
}


# A range of response values as prose: "1 to 5", "0 to 4.5". Each value keeps
# the digits it has, since a response code is not a statistic to round.
nomo_screen_range_text <- function(lo, hi) {
  value <- function(v) {
    vapply(v, function(a) format(a, digits = 15L, trim = TRUE, drop0trailing = TRUE),
           character(1))
  }
  paste(value(lo), "to", value(hi))
}


# The numeric items with a finite response outside the declared response
# scale: the count outside, and the lowest and highest observed values.
nomo_screen_out_of_range <- function(selected, scale_range) {
  empty <- data.frame(item = character(), n_outside = integer(), min = numeric(),
                      max = numeric(), stringsAsFactors = FALSE)
  if (is.null(scale_range) || !length(selected)) return(empty)
  rows <- lapply(names(selected), function(item) {
    x <- selected[[item]]
    if (!is.numeric(x)) return(NULL)
    x <- x[is.finite(x)]
    outside <- x < scale_range[[1L]] | x > scale_range[[2L]]
    if (!any(outside)) return(NULL)
    data.frame(item = item, n_outside = sum(outside), min = min(x), max = max(x),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  if (is.null(out)) empty else out
}


# A concern row for each item with responses outside the declared response
# scale. Every index or note computed on a recoded copy assumes the scale.
nomo_screen_range_log <- function(selected, scale_range, reverse = NULL) {
  log <- nomo_log_new()
  outside <- nomo_screen_out_of_range(selected, scale_range)
  for (i in seq_len(nrow(outside))) {
    item <- outside$item[[i]]
    log <- nomo_log_add(
      log,
      stage = "screen",
      object = item,
      metric = "out_of_range",
      value = outside$n_outside[[i]],
      reference = sprintf("Declared response scale %s", nomo_handoff_range_text(scale_range)),
      severity = "concern",
      observation = sprintf(
        "`%s` has %s outside the declared response scale of %s (observed %s).",
        item, nomo_present_count(outside$n_outside[[i]], "response"),
        nomo_handoff_range_text(scale_range),
        nomo_screen_range_text(outside$min[[i]], outside$max[[i]])
      ),
      recommendation = paste0(
        "Check the coding against the declared scale: a 0-based coding, a ",
        "missing-value code, or a different response format puts responses ",
        "outside it. Correct the data or `scale_range` before relying on any ",
        "value computed from the scale",
        if (item %in% reverse) {
          "; this item is declared reverse-keyed, and recoded on the wrong scale every recoded value is wrong."
        } else {
          "."
        }
      )
    )
  }
  log
}


# The fewest screened items at which the long-string rule becomes a flag. A
# guidance list without the setting, such as one built before it existed, gets
# the default; a setting that is not a number of items is refused, because a
# silent fallback would change which cases are flagged.
nomo_screen_long_string_min <- function(guidance) {
  value <- guidance$long_string_min_items
  if (is.null(value)) return(nomo_defaults()$long_string_min_items)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 1 || value != round(value)) {
    stop(
      "`guidance$long_string_min_items` must be a single whole number of items.",
      call. = FALSE
    )
  }
  as.integer(value)
}


#' @export
nomo_table.nomo_screen <- function(x,
                                   type = c(
                                     "items", "distribution", "cases",
                                     "relationships", "effort", "decision_log"
                                   ),
                                   ...) {
  type <- nomo_match_arg(type)
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
