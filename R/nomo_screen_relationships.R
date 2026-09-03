nomo_screen_relationships <- function(selected, item_summary, guidance) {
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
    n_interitem_estimable = 0L,
    mean_interitem_r = NA_real_,
    median_interitem_r = NA_real_,
    min_interitem_r = NA_real_,
    max_interitem_r = NA_real_,
    negative_interitem_n = 0L
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

    item_rest_r <- row$corrected_item_rest_r[[1L]]

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
        observation = sprintf(
          "`%s` has a negative corrected item-rest correlation (r = %.2f, n = %d).",
          item,
          item_rest_r,
          row$item_rest_n[[1L]]
        ),
        recommendation = paste(
          "Inspect intended keying, reverse-worded item coding, data entry,",
          "and possible multidimensional structure. Do not reverse-score",
          "or delete the item automatically from this diagnostic alone."
        )
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
          "`%s` has a corrected item-rest correlation of r = %.2f (n = %d), below the teaching reference.",
          item,
          item_rest_r,
          row$item_rest_n[[1L]]
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
    !is.na(inter_item_correlations$r) &
      inter_item_correlations$r < 0,
    ,
    drop = FALSE
  ]

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
        "%d estimable inter-item correlation%s are negative.",
        nrow(negative_pairs),
        if (nrow(negative_pairs) == 1L) "" else "s"
      ),
      recommendation = paste(
        "Inspect reverse-keying, miscoding, item wording, and whether",
        "the selected pool contains more than one dimension.",
        "Negative pairs are diagnostic clues, not automatic instructions",
        "to reverse or remove items."
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
