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
        "%d candidate item%s were not included in item-rest/inter-item diagnostics: %s.",
        length(skipped),
        if (length(skipped) == 1L) "" else "s",
        paste(skipped, collapse = ", ")
      ),
      recommendation = paste(
        "Do not coerce ordered, nominal, or text labels to numbers automatically.",
        "If these are scored items, encode their scores intentionally.",
        "Correlation-matrix choice will be handled explicitly during factor analysis."
      )
    )
  }

  non_finite <- items[reasons == "non_finite_values"]

  if (length(non_finite) > 0L) {
    decision_log <- nomo_log_add(
      decision_log,
      stage = "screen",
      object = "relationship_diagnostics",
      metric = "non_finite_scores",
      value = length(non_finite),
      reference = "Finite numeric scores required",
      severity = "concern",
      observation = sprintf(
        "%d candidate item%s contain non-finite numeric values: %s.",
        length(non_finite),
        if (length(non_finite) == 1L) "" else "s",
        paste(non_finite, collapse = ", ")
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
      observation = sprintf(
        "Only %d candidate item%s are eligible for relationship diagnostics.",
        length(eligible_items),
        if (length(eligible_items) == 1L) "" else "s"
      ),
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
  # error (#60). Only the log's wording uses it; no returned value changes.
  keyed_r <- nomo_screen_keyed_item_rest(scores, reverse, scale_range, sets)

  item_total_reference <- guidance$item_total_reference

  if (
    !is.numeric(item_total_reference) ||
    length(item_total_reference) != 1L ||
    !is.finite(item_total_reference)
  ) {
    item_total_reference <- NA_real_
  }

  for (item in eligible_items) {
    row <- relationship_summary[
      relationship_summary$item == item,
      ,
      drop = FALSE
    ]

    # The within-scale value, when the item's scale was declared.
    in_scale <- !is.na(row$scale_item_rest_r[[1L]])
    item_rest_r <- if (in_scale) row$scale_item_rest_r[[1L]] else row$corrected_item_rest_r[[1L]]
    item_rest_n <- if (in_scale) row$scale_item_rest_n[[1L]] else row$item_rest_n[[1L]]
    within_text <- if (in_scale) sprintf(" within its scale `%s`", row$scale[[1L]]) else ""

    if (is.na(item_rest_r)) {
      next
    }

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
            "Teaching reference %.2f; negative sign requires coding/structure review",
            item_total_reference
          )
        },
        severity = "review",
        observation = paste0(
          sprintf(
            "`%s` has a negative corrected item-rest correlation%s (r = %.2f, n = %d).",
            item,
            within_text,
            item_rest_r,
            item_rest_n
          ),
          nomo_screen_keyed_note(item, keyed_r, scale_range)
        ),
        recommendation = if (item %in% names(keyed_r)) {
          paste(
            "Recode the item in the data before modeling it, and record that",
            "you did; nomologR never recodes data. If the data were already",
            "recoded, the declared keying and the data disagree, and one of",
            "them is wrong."
          )
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
          "Teaching/reference value %.2f; not a retention rule",
          item_total_reference
        ),
        severity = "review",
        observation = sprintf(
          "`%s` has a corrected item-rest correlation%s of r = %.2f (n = %d), below the teaching reference.",
          item,
          within_text,
          item_rest_r,
          item_rest_n
        ),
        recommendation = paste(
          "Inspect item content, scoring, and anticipated dimensional structure.",
          "A low item-rest value can reflect multidimensionality as well as weak",
          "alignment; do not delete the item automatically."
        )
      )
    }
  }

  negative_pairs <- inter_item_correlations[
    negative_pair & reviewed_pair,
    ,
    drop = FALSE
  ]
  n_between <- sum(negative_pair & !reviewed_pair)

  if (nrow(negative_pairs) > 0L) {
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
      observation = sprintf(
        "%d estimable inter-item correlation%s%s %s negative.",
        nrow(negative_pairs),
        if (nrow(negative_pairs) == 1L) "" else "s",
        if (length(sets)) " within a declared scale" else "",
        if (nrow(negative_pairs) == 1L) "is" else "are"
      ),
      recommendation = paste(
        "Inspect reverse-keying, miscoding, item wording, and whether",
        "the selected pool contains more than one dimension.",
        "Negative pairs are diagnostic clues, not automatic instructions",
        "to reverse or remove items."
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
          "%d correlation%s between items of different declared scales %s",
          "negative. They are not reviewed as keying clues."
        ),
        n_between,
        if (n_between == 1L) "" else "s",
        if (n_between == 1L) "is" else "are"
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

# Corrected item-rest correlations of the items declared reverse-keyed, on a
# copy recoded as declared: the scale's minimum plus its maximum, minus the
# response. Empty unless the keying is usable, meaning item names and a
# two-number response range.
nomo_screen_keyed_item_rest <- function(scores, reverse, scale_range, sets = list()) {
  declared <- intersect(if (is.character(reverse)) reverse else character(), names(scores))
  usable <- length(declared) > 0L && is.numeric(scale_range) &&
    length(scale_range) == 2L && all(is.finite(scale_range))
  if (!usable) return(numeric())
  keyed <- scores
  keyed[declared] <- lapply(keyed[declared], function(v) sum(scale_range) - v)
  # Across all the items, then within each declared scale where that value
  # exists, matching the value the review reads.
  out <- nomo_screen_item_rest(keyed, names(keyed))$r[declared]
  for (items in sets) {
    within <- nomo_screen_item_rest(keyed, items)$r[intersect(declared, items)]
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
      "before it is recoded: recoded on the declared %s to %s scale, its",
      "item-rest correlation is r = %.2f. The data were not recoded."
    ), format(min(scale_range)), format(max(scale_range)), r)
  } else {
    sprintf(paste(
      " It is declared reverse-keyed, but recoded as declared its item-rest",
      "correlation is still r = %.2f, so the keying does not explain the sign."
    ), r)
  }
}
