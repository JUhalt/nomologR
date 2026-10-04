nomo_screen_relationships <- function(selected, item_summary, guidance,
                                      reverse = NULL, scale_range = NULL,
                                      scales = NULL) {
  items <- names(selected)
  summary_index <- match(items, item_summary$item)

  reasons <- vapply(
    seq_along(items),
    function(i) {
      x <- selected[[i]]
      idx <- summary_index[[i]]

      if (isTRUE(item_summary$all_missing[[idx]])) {
        return("all_missing")
      }

      if (isTRUE(item_summary$constant[[idx]])) {
        return("constant")
      }

      if (!(is.numeric(x) || is.logical(x))) {
        return("not_explicitly_scored_numeric")
      }

      if (is.numeric(x)) {
        observed <- x[!is.na(x)]

        if (any(!is.finite(observed))) {
          return("non_finite_values")
        }
      }

      "eligible"
    },
    character(1)
  )

  relationship_summary <- tibble::tibble(
    item = items,
    relationship_eligible = reasons == "eligible",
    relationship_reason = reasons,
    corrected_item_rest_r = NA_real_,
    item_rest_n = NA_integer_,
    scale = NA_character_,
    scale_item_rest_r = NA_real_,
    scale_item_rest_n = NA_integer_,
    n_interitem_estimable = 0L,
    mean_interitem_r = NA_real_,
    median_interitem_r = NA_real_,
    min_interitem_r = NA_real_,
    max_interitem_r = NA_real_,
    negative_interitem_n = 0L,
    scale_negative_interitem_n = NA_integer_
  )

  inter_item_correlations <- tibble::tibble(
    item1 = character(),
    item2 = character(),
    r = numeric(),
    n_pair = integer()
  )

  eligible_items <- items[reasons == "eligible"]

  relationship_method <- paste(
    "Pearson correlations on explicitly numeric/logical candidate-item scores.",
    "Corrected item-rest correlations use cases complete on all",
    "relationship-eligible items; inter-item correlations use",
    "pairwise-complete cases. Ordered/factor labels are not silently",
    "converted to numeric scores."
  )

  decision_log <- nomo_log_new()

  skipped <- items[reasons == "not_explicitly_scored_numeric"]

  if (length(skipped) > 0L) {
    decision_log <- nomo_log_add(
      decision_log,
      stage = "screen",
      object = "relationship_diagnostics",
      metric = "unscored_items_skipped",
      value = length(skipped),
      reference = "Relationship diagnostics require intentional numeric scoring",
      severity = "info",
      observation = sprintf(
        "%s %s not included in item-rest/inter-item diagnostics: %s.",
        nomo_present_count(length(skipped), "candidate item"),
        nomo_present_noun(length(skipped), "was", "were"),
        paste(skipped, collapse = ", ")
      ),
      recommendation = paste(
        "Do not coerce ordered, nominal, or text labels to numbers automatically.",
        "If these are scored items, encode their scores intentionally.",
        "Correlation-matrix choice will be handled explicitly during factor analysis."
      )
    )
  }

  # One row per item, named by the item as `constant` and `all_missing` rows
  # are, so the item review and the evidence map show the item's concern
  # (#145). A pooled row left the one item with a hard data problem unflagged.
  for (item in items[reasons == "non_finite_values"]) {
    x <- selected[[item]]
    n_bad <- sum(!is.na(x) & !is.finite(x))
    decision_log <- nomo_log_add(
      decision_log,
      stage = "screen",
      object = item,
      metric = "non_finite_scores",
      value = n_bad,
      reference = "Finite numeric scores required",
      severity = "concern",
      observation = sprintf(
        paste(
          "`%s` contains %s (Inf or -Inf), so it was left out of the item-rest",
          "and inter-item diagnostics."
        ),
        item, nomo_present_count(n_bad, "non-finite value")
      ),
      recommendation = paste(
        "Inspect data import and coding for Inf/-Inf values.",
        "Do not silently recode these values as missing."
      )
    )
  }

  if (length(eligible_items) < 2L) {
    decision_log <- nomo_log_add(
      decision_log,
      stage = "screen",
      object = "relationship_diagnostics",
      metric = "insufficient_scored_items",
      value = length(eligible_items),
      reference = "At least two explicitly scored, nonconstant items",
      severity = "info",
      observation = if (length(eligible_items)) {
        "Only 1 candidate item is eligible for relationship diagnostics."
      } else {
        "No candidate item is eligible for relationship diagnostics."
      },
      recommendation = paste(
        "Relationship diagnostics were not estimated.",
        "Verify item selection and intentional scoring."
      )
    )

    return(list(
      relationship_summary = relationship_summary,
      inter_item_correlations = inter_item_correlations,
      relationship_method = relationship_method,
      decision_log = decision_log
    ))
  }

  scores <- data.frame(
    lapply(selected[eligible_items], function(x) {
      if (is.logical(x)) {
        as.numeric(x)
      } else {
        as.numeric(x)
      }
    }),
    check.names = FALSE
  )

  names(scores) <- eligible_items

  pair_list <- utils::combn(
    eligible_items,
    2L,
    simplify = FALSE
  )

  inter_item_correlations <- dplyr::bind_rows(
    lapply(pair_list, function(pair) {
      x <- scores[[pair[[1L]]]]
      y <- scores[[pair[[2L]]]]

      complete <- is.finite(x) & is.finite(y)
      n_pair <- sum(complete)
      r <- NA_real_

      if (n_pair >= 3L) {
        x_complete <- x[complete]
        y_complete <- y[complete]

        if (
          stats::sd(x_complete) > 0 &&
          stats::sd(y_complete) > 0
        ) {
          r <- stats::cor(
            x_complete,
            y_complete,
            method = "pearson"
          )
        }
      }

      tibble::tibble(
        item1 = pair[[1L]],
        item2 = pair[[2L]],
        r = as.numeric(r),
        n_pair = as.integer(n_pair)
      )
    })
  )

  complete_all <- stats::complete.cases(scores)
  n_complete_all <- sum(complete_all)

  for (item in eligible_items) {
    others <- setdiff(eligible_items, item)
    idx <- match(item, relationship_summary$item)

    relationship_summary$item_rest_n[[idx]] <-
      as.integer(n_complete_all)

    if (n_complete_all >= 3L) {
      item_values <- scores[[item]][complete_all]

      rest_values <- rowSums(
        scores[complete_all, others, drop = FALSE]
      )

      if (
        stats::sd(item_values) > 0 &&
        stats::sd(rest_values) > 0
      ) {
        relationship_summary$corrected_item_rest_r[[idx]] <-
          stats::cor(
            item_values,
            rest_values,
            method = "pearson"
          )
      }
    }

    pair_values <- c(
      inter_item_correlations$r[
        inter_item_correlations$item1 == item
      ],
      inter_item_correlations$r[
        inter_item_correlations$item2 == item
      ]
    )

    pair_values <- pair_values[!is.na(pair_values)]

    relationship_summary$n_interitem_estimable[[idx]] <-
      as.integer(length(pair_values))

    relationship_summary$negative_interitem_n[[idx]] <-
      as.integer(sum(pair_values < 0))

    if (length(pair_values) > 0L) {
      relationship_summary$mean_interitem_r[[idx]] <-
        mean(pair_values)

      relationship_summary$median_interitem_r[[idx]] <-
        stats::median(pair_values)

      relationship_summary$min_interitem_r[[idx]] <-
        min(pair_values)

      relationship_summary$max_interitem_r[[idx]] <-
        max(pair_values)
    }
  }

  # With two or more declared scales, an item's corrected item-rest correlation
  # is also computed against the rest of its own scale, the total it is scored
  # on (Nunnally & Bernstein, 1994; Clark & Watson, 2019). The pooled value
  # stays in `corrected_item_rest_r`; the review uses the within-scale one.
  sets <- nomo_screen_scale_sets(scales, eligible_items)
  for (set in names(sets)) {
    within <- nomo_screen_item_rest(scores, sets[[set]])
    idx <- match(sets[[set]], relationship_summary$item)
    relationship_summary$scale[idx] <- set
    relationship_summary$scale_item_rest_r[idx] <- within$r
    relationship_summary$scale_item_rest_n[idx] <- within$n
  }

  # Inter-item signs are read the same way. Two items of one scale should
  # correlate positively, so a negative pair is a keying or wording clue; items
  # of different constructs need not correlate at all, and a pair near zero is
  # as often negative as positive. A pair is reviewed when its items share a
  # scale, or when either has none declared.
  scale_of <- stats::setNames(
    rep(as.character(names(sets)), lengths(sets)),
    as.character(unlist(sets, use.names = FALSE))
  )
  pair_scale_1 <- unname(scale_of[inter_item_correlations$item1])
  pair_scale_2 <- unname(scale_of[inter_item_correlations$item2])
  reviewed_pair <- is.na(pair_scale_1) | is.na(pair_scale_2) | pair_scale_1 == pair_scale_2
  negative_pair <- !is.na(inter_item_correlations$r) & inter_item_correlations$r < 0
  for (item in names(scale_of)) {
    involved <- inter_item_correlations$item1 == item | inter_item_correlations$item2 == item
    relationship_summary$scale_negative_interitem_n[relationship_summary$item == item] <-
      as.integer(sum(negative_pair & involved & reviewed_pair))
  }

  # Declared keying never recodes the data. When an item is declared
  # reverse-keyed, its item-rest correlation is also computed on an internal
  # copy recoded as declared, so a negative sign can be told apart from a coding
  # error (#60). The same copy gives every other item its item-rest value with
  # the declared items recoded, so an item whose rest score holds a declared
  # item not yet recoded is told so, rather than sent to inspect its own content
  # (#145). Only the log's wording uses these values; no returned value changes.
  keyed_all <- nomo_screen_keyed_item_rest(scores, reverse, scale_range, sets)
  declared <- intersect(if (is.character(reverse)) reverse else character(), eligible_items)
  keyed_r <- keyed_all[intersect(names(keyed_all), declared)]

  item_total_reference <- guidance$item_total_reference

  if (
    !is.numeric(item_total_reference) ||
    length(item_total_reference) != 1L ||
    !is.finite(item_total_reference)
  ) {
    item_total_reference <- NA_real_
  }

  # The value each item's review reads: within its declared scale when two or
  # more scales were declared, otherwise the pooled one. An item alone in its
  # scale, or in a scale with too few complete cases, has no within-scale value,
  # and it is not judged on the pooled one instead, which mixes in the items of
  # other constructs (#113, #145).
  review <- relationship_summary[match(eligible_items, relationship_summary$item), ]
  in_scale <- stats::setNames(!is.na(review$scale), eligible_items)
  review_r <- stats::setNames(
    ifelse(in_scale, review$scale_item_rest_r, review$corrected_item_rest_r), eligible_items
  )
  review_n <- stats::setNames(
    ifelse(in_scale, review$scale_item_rest_n, review$item_rest_n), eligible_items
  )
  # Declared items showing the sign of an item not yet recoded: negative as
  # answered, positive once recoded as declared.
  unrecoded <- declared[vapply(declared, function(item) {
    isTRUE(review_r[[item]] < 0 && keyed_r[item] > 0)
  }, logical(1))]
  r_text <- function(value) {
    nomo_present_stat(value, "r", reference = item_total_reference)
  }
  recode_first <- paste(
    "Recode the declared reverse-keyed items in the data first, then read",
    "this item again; do not reverse-score or delete it from this value."
  )

  for (item in eligible_items) {
    i <- match(item, review$item)
    item_rest_r <- review_r[[item]]
    item_rest_n <- review_n[[item]]
    scale <- review$scale[[i]]
    within_text <- if (in_scale[[item]]) sprintf(" within its scale `%s`", scale) else ""

    if (is.na(item_rest_r)) {
      if (in_scale[[item]]) {
        decision_log <- nomo_screen_no_scale_rest_row(
          decision_log, item, scale, sum(review$scale %in% scale), item_rest_n
        )
      }
      next
    }

    # The unrecoded declared items in this item's rest score.
    rest_items <- if (in_scale[[item]]) {
      review$item[review$scale %in% scale]
    } else {
      eligible_items
    }
    partners <- setdiff(intersect(unrecoded, rest_items), item)
    unrecoded_note <- nomo_screen_unrecoded_note(partners, keyed_all[item])

    if (item_rest_r < 0) {
      decision_log <- nomo_log_add(
        decision_log,
        stage = "screen",
        object = item,
        metric = "corrected_item_rest",
        value = item_rest_r,
        reference = if (is.na(item_total_reference)) {
          "Negative sign requires coding/structure review"
        } else {
          sprintf(
            "Teaching reference %s; negative sign requires coding/structure review",
            nomo_present_stat(item_total_reference, "r")
          )
        },
        severity = "review",
        observation = paste0(
          sprintf(
            "`%s` has a negative corrected item-rest correlation%s (r = %s, n = %d).",
            item,
            within_text,
            r_text(item_rest_r),
            item_rest_n
          ),
          nomo_screen_keyed_note(item, keyed_r, scale_range),
          unrecoded_note
        ),
        recommendation = if (item %in% names(keyed_r)) {
          paste(
            "Recode the item in the data before modeling it, and record that",
            "you did; nomologR never recodes data. If the data were already",
            "recoded, the declared keying and the data disagree, and one of",
            "them is wrong."
          )
        } else if (length(partners)) {
          recode_first
        } else {
          paste(
            "Inspect intended keying, reverse-worded item coding, data entry,",
            "and possible multidimensional structure. Do not reverse-score",
            "or delete the item automatically from this diagnostic alone."
          )
        }
      )
    } else if (
      !is.na(item_total_reference) &&
      item_rest_r < item_total_reference
    ) {
      decision_log <- nomo_log_add(
        decision_log,
        stage = "screen",
        object = item,
        metric = "corrected_item_rest",
        value = item_rest_r,
        reference = sprintf(
          "Teaching reference %s; not a retention rule",
          nomo_present_stat(item_total_reference, "r")
        ),
        severity = "review",
        observation = paste0(
          sprintf(
            paste(
              "`%s` has a corrected item-rest correlation%s of r = %s (n = %d),",
              "below the teaching reference of %s."
            ),
            item,
            within_text,
            r_text(item_rest_r),
            item_rest_n,
            nomo_present_stat(item_total_reference, "r")
          ),
          unrecoded_note
        ),
        recommendation = if (length(partners)) {
          recode_first
        } else {
          paste(
            "Inspect item content, scoring, and anticipated dimensional structure.",
            "A low item-rest value can reflect multidimensionality as well as weak",
            "alignment; do not delete the item automatically."
          )
        }
      )
    }
  }

  # Negative pairs within a declared scale and pairs with an item outside every
  # declared scale are both reviewed, and each kind is named as what it is
  # (#145): a pair with an undeclared item is no evidence about a declared
  # scale's keying.
  reviewed_negative <- negative_pair & reviewed_pair
  negative_pairs <- inter_item_correlations[reviewed_negative, , drop = FALSE]
  n_between <- sum(negative_pair & !reviewed_pair)

  if (nrow(negative_pairs) > 0L) {
    unscaled <- is.na(pair_scale_1) | is.na(pair_scale_2)
    n_within <- sum(reviewed_negative & !unscaled)
    n_unscaled <- sum(reviewed_negative & unscaled)
    pairs_text <- function(n) nomo_present_count(n, "estimable inter-item correlation")
    kinds <- if (!length(sets)) {
      pairs_text(nrow(negative_pairs))
    } else if (!n_unscaled) {
      paste(pairs_text(n_within), "within a declared scale")
    } else if (!n_within) {
      paste(pairs_text(n_unscaled), "involving an item with no declared scale")
    } else {
      sprintf("%s within a declared scale and %d involving an item with no declared scale",
              pairs_text(n_within), n_unscaled)
    }
    with_unrecoded <- sum(negative_pairs$item1 %in% unrecoded |
                            negative_pairs$item2 %in% unrecoded)
    decision_log <- nomo_log_add(
      decision_log,
      stage = "screen",
      object = "inter_item_correlations",
      metric = "negative_pairs",
      value = nrow(negative_pairs),
      reference = paste(
        "Expected signs depend on scoring",
        "and dimensional structure"
      ),
      severity = "review",
      observation = paste0(
        sprintf("%s %s negative.", kinds,
                nomo_present_noun(nrow(negative_pairs), "is", "are")),
        if (with_unrecoded) {
          sprintf(
            " %s %s, declared reverse-keyed and not yet recoded in the data.",
            if (with_unrecoded == nrow(negative_pairs)) {
              if (with_unrecoded == 1L) "It involves" else "All involve"
            } else {
              sprintf("%d of them %s", with_unrecoded,
                      nomo_present_noun(with_unrecoded, "involves", "involve"))
            },
            nomo_present_or(unrecoded)
          )
        }
      ),
      recommendation = paste(
        c(
          if (with_unrecoded) {
            "Recode the declared reverse-keyed items in the data first, then read the pairs again."
          },
          "Inspect reverse-keying, miscoding, item wording, and whether",
          "the selected pool contains more than one dimension.",
          "Negative pairs are diagnostic clues, not automatic instructions",
          "to reverse or remove items."
        ),
        collapse = " "
      )
    )
  }

  if (n_between > 0L) {
    decision_log <- nomo_log_add(
      decision_log,
      stage = "screen",
      object = "inter_item_correlations",
      metric = "negative_pairs_between_scales",
      value = n_between,
      reference = "Items of different constructs need not correlate positively",
      severity = "info",
      observation = sprintf(
        paste(
          "%s between items of different declared scales %s",
          "negative. They are not reviewed as keying clues."
        ),
        nomo_present_count(n_between, "correlation"),
        nomo_present_noun(n_between, "is", "are")
      ),
      recommendation = paste(
        "Read correlations between scales as evidence about how the",
        "constructs relate, in the measurement model rather than here."
      )
    )
  }

  list(
    relationship_summary = relationship_summary,
    inter_item_correlations = inter_item_correlations,
    relationship_method = relationship_method,
    decision_log = decision_log
  )
}

# Corrected item-rest correlations on a copy with the items declared
# reverse-keyed recoded as declared: the scale's minimum plus its maximum, minus
# the response. Every item gets its value, across all the items and then within
# its declared scale where that value exists, matching the value the review
# reads. Empty unless the keying is usable, meaning item names and a two-number
# response range.
nomo_screen_keyed_item_rest <- function(scores, reverse, scale_range, sets = list()) {
  declared <- intersect(if (is.character(reverse)) reverse else character(), names(scores))
  usable <- length(declared) > 0L && is.numeric(scale_range) &&
    length(scale_range) == 2L && all(is.finite(scale_range))
  if (!usable) return(numeric())
  keyed <- scores
  keyed[declared] <- lapply(keyed[declared], function(v) sum(scale_range) - v)
  out <- nomo_screen_item_rest(keyed, names(keyed))$r
  for (items in sets) {
    within <- nomo_screen_item_rest(keyed, items)$r
    within <- within[is.finite(within)]
    out[names(within)] <- within
  }
  out[is.finite(out)]
}


# Corrected item-rest correlations within one set of items, on the cases
# complete on all of them.
nomo_screen_item_rest <- function(scores, items) {
  vals <- scores[items]
  complete <- stats::complete.cases(vals)
  if (sum(complete) < 3L) {
    return(list(r = stats::setNames(rep(NA_real_, length(items)), items),
                n = as.integer(sum(complete))))
  }
  r <- vapply(items, function(item) {
    rest <- rowSums(vals[complete, setdiff(items, item), drop = FALSE])
    suppressWarnings(stats::cor(vals[[item]][complete], rest))
  }, numeric(1))
  list(r = r, n = as.integer(sum(complete)))
}


# The declared scales restricted to the items screened for relationships. An
# item counts in the first scale that lists it, and an unnamed scale is named
# by its position. Empty unless at least two scales remain, since one scale is
# the pool itself.
nomo_screen_scale_sets <- function(scales, eligible) {
  if (!is.list(scales) || !length(scales)) return(list())
  labels <- names(scales)
  if (is.null(labels)) labels <- character(length(scales))
  unnamed <- is.na(labels) | !nzchar(labels)
  labels[unnamed] <- paste("Scale", which(unnamed))
  sets <- lapply(scales, function(v) intersect(as.character(unlist(v)), eligible))
  names(sets) <- labels
  seen <- character()
  for (i in seq_along(sets)) {
    sets[[i]] <- setdiff(sets[[i]], seen)
    seen <- c(seen, sets[[i]])
  }
  sets <- sets[lengths(sets) > 0L]
  if (length(sets) < 2L) return(list())
  sets
}


# What a negative item-rest correlation means for an item declared
# reverse-keyed: the expected sign of an item not yet recoded, or, if the
# recoded correlation is still negative, a sign the keying does not explain.
nomo_screen_keyed_note <- function(item, keyed_r, scale_range) {
  if (!item %in% names(keyed_r)) return("")
  r <- keyed_r[[item]]
  if (r > 0) {
    sprintf(paste(
      " It is declared reverse-keyed, and this is the sign such an item shows",
      "before it is recoded: recoded on the declared %s scale, its",
      "item-rest correlation is r = %s. The data were not recoded."
    ), nomo_handoff_range_text(scale_range), nomo_present_stat(r, "r"))
  } else {
    sprintf(paste(
      " It is declared reverse-keyed, but recoded as declared its item-rest",
      "correlation is still r = %s, so the keying does not explain the sign."
    ), nomo_present_stat(r, "r"))
  }
}


# What an item's low or negative item-rest value means when its rest score
# holds declared reverse-keyed items that read as not yet recoded: the value
# with them recoded as declared. Empty when there are none.
nomo_screen_unrecoded_note <- function(partners, keyed) {
  if (!length(partners)) return("")
  one <- length(partners) == 1L
  sprintf(
    paste(
      " Its rest score includes %s, declared reverse-keyed and not yet recoded",
      "in the data; with %s recoded as declared, its item-rest correlation is",
      "r = %s."
    ),
    nomo_present_or(partners, "and"),
    if (one) partners else "them",
    nomo_present_stat(keyed[[1L]], "r")
  )
}


# An item in a declared scale with no item-rest value within it is not reviewed
# on the pooled value; the log says why there is none: it is the scale's only
# item in the diagnostics, or too few cases are complete on the scale's items.
nomo_screen_no_scale_rest_row <- function(log, item, scale, n_items, n_complete) {
  nomo_log_add(
    log,
    stage = "screen",
    object = item,
    metric = "item_rest_not_computed",
    value = n_complete,
    reference = "Within-scale item-rest correlation; needs a rest score and three complete cases",
    severity = "info",
    observation = if (n_items < 2L) {
      sprintf(
        paste(
          "`%s` is the only item of its scale `%s` in the relationship",
          "diagnostics, so it has no rest score and no item-rest correlation."
        ),
        item, scale
      )
    } else if (n_complete < 3L) {
      sprintf(
        paste(
          "`%s` has no item-rest correlation within its scale `%s`: %s",
          "complete on the scale's items, and at least three are needed."
        ),
        item, scale, nomo_present_count(n_complete, "case is", "cases are")
      )
    } else {
      sprintf(
        paste(
          "`%s` has no item-rest correlation within its scale `%s`: the item",
          "or the rest of its scale does not vary among the %d cases complete",
          "on the scale's items."
        ),
        item, scale, n_complete
      )
    },
    recommendation = paste(
      "It is not reviewed on the pooled item-rest value instead, which mixes in",
      "the items of other constructs. Read the item against its construct in",
      "the measurement model."
    )
  )
}
