# Item descriptives and the concentration and near-zero-variance rows. The
# floor and ceiling of a numeric item are the ends of the declared response
# scale when `scale_range` is given. Otherwise they are the lowest and highest
# observed values, since a numeric item shows only the values used: a pile-up
# in the middle of a scale whose lower categories went unused would read as a
# floor effect (#145).
nomo_screen_descriptives <- function(selected, item_summary, guidance,
                                     scale_range = NULL) {
  items <- names(selected)

  get_scalar_guidance <- function(name, fallback) {
    value <- guidance[[name]]
    if (
      is.null(value) ||
        !is.numeric(value) ||
        length(value) != 1L ||
        !is.finite(value)
    ) {
      return(fallback)
    }
    as.numeric(value)
  }

  concentration_reference <- get_scalar_guidance(
    "response_concentration_reference",
    0.80
  )
  nzv_frequency_reference <- get_scalar_guidance(
    "nzv_frequency_ratio_reference",
    19
  )
  nzv_percent_unique_reference <- get_scalar_guidance(
    "nzv_percent_unique_reference",
    10
  )

  item_summary$percent_unique <- NA_real_
  item_summary$frequency_ratio <- NA_real_
  item_summary$near_zero_variance <- FALSE
  item_summary$floor_prop <- NA_real_
  item_summary$ceiling_prop <- NA_real_
  item_summary$skewness <- NA_real_
  item_summary$excess_kurtosis <- NA_real_

  decision_log <- nomo_log_new()

  for (item in items) {
    idx <- match(item, item_summary$item)
    x <- selected[[item]]

    n_observed <- item_summary$n_observed[[idx]]
    n_unique <- item_summary$n_unique[[idx]]
    item_type <- item_summary$item_type[[idx]]
    constant <- isTRUE(item_summary$constant[[idx]])
    all_missing <- isTRUE(item_summary$all_missing[[idx]])

    if (all_missing || n_observed == 0L) {
      next
    }

    item_summary$percent_unique[[idx]] <-
      100 * n_unique / n_observed

    observed <- x[!is.na(x)]

    counts <- as.integer(table(observed, useNA = "no"))
    counts <- sort(counts[counts > 0L], decreasing = TRUE)

    if (length(counts) >= 2L) {
      item_summary$frequency_ratio[[idx]] <-
        counts[[1L]] / counts[[2L]]
    } else if (length(counts) == 1L) {
      item_summary$frequency_ratio[[idx]] <- Inf
    }

    if (!constant && length(counts) >= 2L) {
      item_summary$near_zero_variance[[idx]] <-
        is.finite(item_summary$frequency_ratio[[idx]]) &&
        item_summary$frequency_ratio[[idx]] >= nzv_frequency_reference &&
        item_summary$percent_unique[[idx]] <= nzv_percent_unique_reference
    }

    if (item_type == "ordered" && length(levels(x)) > 2L) {
      lev <- levels(x)
      observed_chr <- as.character(observed)

      item_summary$floor_prop[[idx]] <-
        mean(observed_chr == lev[[1L]])

      item_summary$ceiling_prop[[idx]] <-
        mean(observed_chr == lev[[length(lev)]])
    } else if (
      item_type == "numeric_discrete" &&
        is.numeric(x) &&
        n_unique > 2L
    ) {
      # A numeric-discrete item's values are all finite.
      ends <- if (is.null(scale_range)) range(observed) else scale_range
      item_summary$floor_prop[[idx]] <- mean(observed == ends[[1L]])
      item_summary$ceiling_prop[[idx]] <- mean(observed == ends[[2L]])
    }

    if (
      item_type == "numeric_continuous" &&
        is.numeric(x)
    ) {
      finite_observed <- observed[is.finite(observed)]

      if (
        length(finite_observed) >= 3L &&
          stats::sd(finite_observed) > 0
      ) {
        centered <- finite_observed - mean(finite_observed)
        m2 <- mean(centered^2)

        if (m2 > 0) {
          item_summary$skewness[[idx]] <-
            mean(centered^3) / (m2^(3 / 2))
        }
      }

      if (
        length(finite_observed) >= 4L &&
          stats::sd(finite_observed) > 0
      ) {
        centered <- finite_observed - mean(finite_observed)
        m2 <- mean(centered^2)

        if (m2 > 0) {
          item_summary$excess_kurtosis[[idx]] <-
            mean(centered^4) / (m2^2) - 3
        }
      }
    }

    if (
      !constant &&
        is.finite(item_summary$mode_prop[[idx]]) &&
        item_summary$mode_prop[[idx]] >= concentration_reference
    ) {
      floor_prop <- item_summary$floor_prop[[idx]]
      ceiling_prop <- item_summary$ceiling_prop[[idx]]

      metric <- "response_concentration"
      boundary_note <- ""
      # Where the ends come from: an ordered item's levels, the declared
      # scale, or only the values observed.
      end_word <- if (item_type == "ordered") {
        "response category"
      } else if (is.null(scale_range)) {
        "observed value"
      } else {
        "category of the declared response scale"
      }

      if (
        is.finite(floor_prop) &&
          floor_prop >= concentration_reference
      ) {
        metric <- "floor_concentration"
        boundary_note <- sprintf(" at the lowest %s", end_word)
      } else if (
        is.finite(ceiling_prop) &&
          ceiling_prop >= concentration_reference
      ) {
        metric <- "ceiling_concentration"
        boundary_note <- sprintf(" at the highest %s", end_word)
      }

      decision_log <- nomo_log_add(
        decision_log,
        stage = "screen",
        object = item,
        metric = metric,
        value = item_summary$mode_prop[[idx]],
        reference = sprintf(
          "Teaching reference %s of responses in one category; not a deletion rule",
          nomo_present_percent(concentration_reference, base = n_observed)
        ),
        severity = "review",
        observation = sprintf(
          "`%s` places %s of observed responses in one category%s.",
          item,
          nomo_present_percent(item_summary$mode_prop[[idx]], base = n_observed),
          boundary_note
        ),
        recommendation = paste(
          "Inspect response-option use, item wording, sample range restriction,",
          "skip/display logic, and construct targeting. High concentration can",
          "reduce information, but it does not by itself justify deleting an item.",
          if (identical(end_word, "observed value")) {
            paste(
              "A numeric item shows only the values used, so an unused end",
              "category is not visible; supply `scale_range` to compare with the",
              "ends of the response scale."
            )
          }
        )
      )
    }

    if (isTRUE(item_summary$near_zero_variance[[idx]])) {
      decision_log <- nomo_log_add(
        decision_log,
        stage = "screen",
        object = item,
        metric = "near_zero_variance",
        value = item_summary$frequency_ratio[[idx]],
        # The ratio and the percentage print as their references do, each
        # with one precision.
        reference = sprintf(
          paste0(
            "Screening heuristic: frequency ratio >= %s and ",
            "percent unique <= %s; not a psychometric law"
          ),
          nomo_present_stat(nzv_frequency_reference, "estimate"),
          nomo_present_percent(nzv_percent_unique_reference / 100)
        ),
        severity = "review",
        observation = sprintf(
          paste0(
            "`%s` has highly imbalanced observed responses ",
            "(frequency ratio %s; %s unique values)."
          ),
          item,
          nomo_present_stat(item_summary$frequency_ratio[[idx]], "estimate"),
          nomo_present_percent(item_summary$percent_unique[[idx]] / 100)
        ),
        recommendation = paste(
          "Inspect coding, sampling, item targeting, and whether this response",
          "pattern is expected for the population. Treat near-zero variance as a",
          "diagnostic condition rather than an automatic item-removal rule."
        )
      )
    }
  }

  list(
    item_summary = item_summary,
    decision_log = decision_log,
    references = list(
      response_concentration = concentration_reference,
      nzv_frequency_ratio = nzv_frequency_reference,
      nzv_percent_unique = nzv_percent_unique_reference
    )
  )
}
