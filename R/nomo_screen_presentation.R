#' Summarize a nomo_screen audit
#'
#' `summary.nomo_screen()` integrates the descriptive, relationship, and
#' decision-log evidence from [nomo_screen()] into an item-level review table.
#' The resulting `attention` field is deliberately phrased as `none`, `review`,
#' or `concern`; it is not an item-retention decision.
#'
#' @param object A `nomo_screen` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return An object of class `summary_nomo_screen` containing an overview,
#'   integrated item-review table, decision log, relationship-method note, and
#'   guidance settings. Printed, it shows the item review table and a Flagged
#'   section giving the reason for each flag in the decision log's words.
#' @export
summary.nomo_screen <- function(object, ...) {
  if (!inherits(object, "nomo_screen")) {
    stop("`object` must inherit from `nomo_screen`.", call. = FALSE)
  }

  item_review <- nomo_screen_item_review(object)

  overview <- tibble::tibble(
    n_cases = object$n_cases,
    n_items = length(object$items),
    n_no_review_flag = sum(item_review$attention == "none"),
    n_review = sum(item_review$attention == "review"),
    n_concern = sum(item_review$attention == "concern"),
    n_items_with_missing = sum(object$item_summary$n_missing > 0L),
    n_constant = sum(object$item_summary$constant),
    n_all_missing = sum(object$item_summary$all_missing),
    n_relationship_eligible = sum(
      object$relationship_summary$relationship_eligible
    ),
    n_decision_log_entries = nrow(object$decision_log)
  )

  out <- list(
    overview = overview,
    item_review = item_review,
    decision_log = object$decision_log,
    relationship_method = object$relationship_method,
    guidance = object$guidance,
    # The reviewed negative pairs, so the Flagged section can name the items an
    # item correlates negatively with (#145).
    negative_pairs = nomo_screen_reviewed_negative_pairs(object)
  )

  class(out) <- c("summary_nomo_screen", "list")
  out
}


# The inter-item correlations the review reads as negative: within a declared
# scale, or with an item outside every declared scale, or all of them when no
# scales were declared.
nomo_screen_reviewed_negative_pairs <- function(x) {
  pairs <- x$inter_item_correlations
  rel <- x$relationship_summary
  scale <- rel[["scale"]]
  if (is.null(scale)) scale <- rep(NA_character_, nrow(rel))
  s1 <- scale[match(pairs$item1, rel$item)]
  s2 <- scale[match(pairs$item2, rel$item)]
  reviewed <- is.na(s1) | is.na(s2) | s1 == s2
  pairs[!is.na(pairs$r) & pairs$r < 0 & reviewed, c("item1", "item2", "r"), drop = FALSE]
}


#' Print a summary_nomo_screen object
#'
#' @param x A `summary_nomo_screen` object.
#' @param ... Additional arguments, currently ignored.
#'
#' @return `x`, invisibly.
#' @export
print.summary_nomo_screen <- function(x, ...) {
  overview <- x$overview[1L, , drop = FALSE]

  nomo_present_header("nomo_screen", "Item and data audit", summary = TRUE)
  nomo_present_facts(c(
    sprintf("Cases: %d", overview$n_cases),
    sprintf("Items: %d", overview$n_items),
    paste0("Item flags: ", nomo_present_flag_counts(x$item_review$attention))
  ))
  nomo_present_facts(c(
    sprintf("Items with missing responses: %d", overview$n_items_with_missing),
    sprintf("Constant: %d", overview$n_constant),
    sprintf("All missing: %d", overview$n_all_missing),
    paste0("Items in correlation diagnostics: ",
           nomo_screen_of(overview$n_relationship_eligible, overview$n_items))
  ))

  review <- x$item_review
  nomo_screen_present_skipped(review, review)
  # A column with one observed value is stored as "binary" (two or fewer
  # values), but on screen it is what it is: constant.
  review$type <- ifelse(review$constant %in% TRUE, "constant",
                        sub("^numeric_", "", review$item_type))
  review$flag <- nomo_present_status(review$attention)
  review$item_rest <- nomo_screen_review_item_rest(review)
  reference <- nomo_screen_item_rest_reference(x$guidance)
  nomo_present_section("Item review")
  nomo_present_table(
    review,
    c("Item" = "item", "Scale" = "scale", "Type" = "type",
      "Missing" = "pct_missing", "Top share" = "mode_prop",
      "Item-rest r" = "item_rest", "Flag" = "flag"),
    formats = list(
      pct_missing = function(v) nomo_present_percent(v, base = review$n),
      mode_prop = function(v) nomo_present_percent(v, base = review$n_observed),
      item_rest = function(v) nomo_present_stat(v, "r", reference = reference)
    ),
    more = "summary(x)$item_review"
  )
  shown_rest <- any(is.finite(review$item_rest))
  nomo_present_text(
    c(
      "Top share is the share of observed responses in the most common category.",
      if (shown_rest) {
        paste0(
          "Item-rest r is the correlation of an item with the sum of the other ",
          if (any(!is.na(review[["scale_item_rest_r"]]))) {
            "items of its scale; the pooled value is kept as corrected_item_rest_r."
          } else {
            "items."
          }
        )
      },
      if (any(!is.finite(review$mode_prop)) ||
            (shown_rest && any(!is.finite(review$item_rest)))) {
        "-- marks a value that was not computed; the decision log says why."
      }
    ),
    indent = 2L
  )

  # Items first, then the log's other flagged entries, such as negative pairs
  # or careless-responding counts, each under a plain name.
  log <- x$decision_log
  other <- log[!log$object %in% review$item & log$severity %in% c("review", "concern"), ,
               drop = FALSE]
  nomo_present_flagged(
    unit = c(review$item, nomo_screen_unit_label(other$object)),
    status = c(as.character(review$attention), other$severity),
    text = c(nomo_screen_flag_text(review, log, x$negative_pairs), other$observation)
  )

  cat("\n")
  nomo_present_text(
    "Flags are review aids, not decisions to keep or delete an item."
  )
  nomo_present_pointer(
    c("nomo_table(x, \"decision_log\")", "plot(x)"),
    c("every log entry", "the item evidence map")
  )
  invisible(x)
}


# The teaching reference for item-rest correlations, or NA when the guidance
# has no usable one.
nomo_screen_item_rest_reference <- function(guidance) {
  reference <- guidance$item_total_reference
  if (is.numeric(reference) && length(reference) == 1L && is.finite(reference)) {
    reference
  } else {
    NA_real_
  }
}


# Each item's reasons for its flag: the decision log's own explanations, then
# what the item review adds without a log row of its own, negative inter-item
# pairs and unused response categories, as complete sentences.
nomo_screen_flag_text <- function(review, log, negative_pairs = NULL) {
  vapply(seq_len(nrow(review)), function(i) {
    item <- review$item[[i]]
    reasons <- log$observation[log$object == item & log$severity %in% c("review", "concern")]
    metrics <- strsplit(review$review_metrics[[i]], ", ", fixed = TRUE)[[1L]]
    if ("negative_interitem_pairs" %in% metrics) {
      reasons <- c(reasons, nomo_screen_pair_text(item, negative_pairs))
    }
    if ("unused_response_categories" %in% metrics) {
      reasons <- c(reasons, sprintf(
        "It leaves %s unused.",
        nomo_present_count(review$n_unused_response_categories[[i]],
                           "declared response category", "declared response categories")
      ))
    }
    if (!length(reasons)) reasons <- "The decision log has the details."
    paste(reasons, collapse = " ")
  }, character(1))
}


# "Its correlation with TF2 is negative." The partners are named, up to four.
nomo_screen_pair_text <- function(item, pairs) {
  # A summary made before the pairs were kept, or edited, has none to name.
  mine <- if (!is.null(pairs)) pairs[pairs$item1 == item | pairs$item2 == item, , drop = FALSE]
  if (!NROW(mine)) {
    return("Some of its inter-item correlations are negative; see x$inter_item_correlations.")
  }
  partners <- ifelse(mine$item1 == item, mine$item2, mine$item1)
  shown <- if (length(partners) > 4L) {
    c(partners[1:3], sprintf("%d other items", length(partners) - 3L))
  } else {
    partners
  }
  if (length(partners) == 1L) {
    sprintf("Its correlation with %s is negative.", partners)
  } else {
    sprintf("Its correlations with %s are negative.", nomo_present_or(shown, "and"))
  }
}


# A decision-log object that is not an item, as a unit in the Flagged section.
nomo_screen_unit_label <- function(object) {
  labels <- c(
    cases = "Cases",
    inter_item_correlations = "Inter-item correlations",
    content_review = "Content review",
    relationship_diagnostics = "Correlation diagnostics"
  )
  out <- unname(labels[object])
  plain <- gsub("_", " ", object)
  ifelse(is.na(out), paste0(toupper(substr(plain, 1L, 1L)), substring(plain, 2L)), out)
}


#' Plot a nomo_screen audit
#'
#' Visual diagnostics complement the numerical screening output. The default
#' evidence map integrates multiple diagnostic signals without converting them
#' into a pass/fail scale. Additional plot types display item-rest relationships,
#' inter-item correlations, response-category use, or missingness. The evidence
#' map and the item-rest plot show each flag by shape, with color repeating it:
#' a filled circle for no flag, an open circle for review, a filled square for
#' concern, and a cross where a diagnostic was not computed.
#'
#' @param x A `nomo_screen` object.
#' @param y Ignored; included for compatibility with the base `plot()` generic.
#' @param type Plot type: `"evidence"`, `"item_rest"`, `"interitem"`,
#'   `"responses"`, or `"missingness"`. `"responses"` draws a bar for each
#'   response category of a categorical item, and a histogram of observed
#'   values for a continuous item (`item_type` `"numeric_continuous"`). When the
#'   selected items mix the two, the categorical items are drawn and the caption
#'   names the continuous ones, which can be plotted by passing them as `items`.
#' @param items Optional character vector of candidate items to display.
#' @param show_values Logical; add numerical labels where useful.
#' @param ... Additional arguments, currently ignored.
#'
#' @return A `ggplot2` plot object.
#' @export
plot.nomo_screen <- function(x,
                             y = NULL,
                             type = c(
                               "evidence",
                               "item_rest",
                               "interitem",
                               "responses",
                               "missingness"
                             ),
                             items = NULL,
                             show_values = TRUE,
                             ...) {
  if (!inherits(x, "nomo_screen")) {
    stop("`x` must inherit from `nomo_screen`.", call. = FALSE)
  }

  type <- nomo_match_arg(type)
  items <- nomo_screen_plot_items(x, items)

  if (type == "evidence") {
    return(nomo_screen_plot_evidence(x, items))
  }

  if (type == "item_rest") {
    return(nomo_screen_plot_item_rest(x, items, show_values))
  }

  if (type == "interitem") {
    return(nomo_screen_plot_interitem(x, items, show_values))
  }

  if (type == "responses") {
    return(nomo_screen_plot_responses(x, items, show_values))
  }

  nomo_screen_plot_missingness(x, items, show_values)
}


nomo_screen_item_review <- function(x) {
  relationship <- x$relationship_summary

  category_summary <- dplyr::bind_rows(
    lapply(x$items, function(item) {
      d <- x$response_distribution[
        x$response_distribution$item == item &
          !x$response_distribution$missing,
        ,
        drop = FALSE
      ]

      used <- d$n > 0L

      tibble::tibble(
        item = item,
        n_response_categories_listed = as.integer(nrow(d)),
        n_response_categories_used = as.integer(sum(used)),
        n_unused_response_categories = as.integer(sum(!used)),
        smallest_used_category_prop = if (any(used)) {
          min(d$proportion_observed[used], na.rm = TRUE)
        } else {
          NA_real_
        }
      )
    })
  )

  out <- dplyr::left_join(
    x$item_summary,
    relationship,
    by = "item"
  )

  out <- dplyr::left_join(
    out,
    category_summary,
    by = "item"
  )

  negative_n <- nomo_screen_review_negative_n(out)
  attention <- character(nrow(out))
  review_count <- integer(nrow(out))
  concern_count <- integer(nrow(out))
  info_count <- integer(nrow(out))
  review_metrics <- character(nrow(out))

  for (i in seq_len(nrow(out))) {
    item <- out$item[[i]]

    log_rows <- x$decision_log[
      x$decision_log$object == item,
      ,
      drop = FALSE
    ]

    info_count[[i]] <- sum(log_rows$severity == "info")
    review_count[[i]] <- sum(log_rows$severity == "review")
    concern_count[[i]] <- sum(log_rows$severity == "concern")

    metrics <- unique(
      log_rows$metric[
        log_rows$severity %in% c("review", "concern")
      ]
    )

    if (
      !is.na(negative_n[[i]]) &&
        negative_n[[i]] > 0L
    ) {
      review_count[[i]] <- review_count[[i]] + 1L
      metrics <- c(metrics, "negative_interitem_pairs")
    }

    if (
      identical(out$item_type[[i]], "ordered") &&
        out$n_unused_response_categories[[i]] > 0L
    ) {
      review_count[[i]] <- review_count[[i]] + 1L
      metrics <- c(metrics, "unused_response_categories")
    }

    metrics <- unique(metrics)

    attention[[i]] <- if (concern_count[[i]] > 0L) {
      "concern"
    } else if (review_count[[i]] > 0L) {
      "review"
    } else {
      "none"
    }

    review_metrics[[i]] <- if (length(metrics) == 0L) {
      ""
    } else {
      paste(metrics, collapse = ", ")
    }
  }

  out$info_count <- info_count
  out$review_count <- review_count
  out$concern_count <- concern_count
  out$attention <- factor(
    attention,
    levels = c("none", "review", "concern"),
    ordered = TRUE
  )
  out$review_metrics <- review_metrics

  out
}


nomo_screen_plot_items <- function(x, items) {
  if (is.null(items)) {
    return(x$items)
  }

  if (
    !is.character(items) ||
      length(items) == 0L ||
      anyNA(items) ||
      any(items == "")
  ) {
    stop(
      "`items` must be `NULL` or a non-empty character vector of item names.",
      call. = FALSE
    )
  }

  unknown <- setdiff(items, x$items)

  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Unknown item%s: %s.",
        if (length(unknown) == 1L) "" else "s",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  unique(items)
}


nomo_screen_evidence_data <- function(x, items) {
  metrics <- c(
    "all_missing",
    "constant",
    "missingness",
    "item_type",
    "coding",
    "response_concentration",
    "near_zero_variance",
    "corrected_item_rest",
    "negative_interitem_pairs",
    "unused_response_categories"
  )

  grid <- expand.grid(
    item = items,
    metric = metrics,
    stringsAsFactors = FALSE
  )

  grid$severity <- "none"

  # Several log metrics share one column: floor and ceiling concentration are
  # response concentration, and values outside the declared scale or not
  # finite are both coding problems. An item-rest value that could not be
  # computed within the item's scale is a not-computed item-rest cell.
  aliases <- c(
    floor_concentration = "response_concentration",
    ceiling_concentration = "response_concentration",
    non_finite_scores = "coding",
    out_of_range = "coding",
    item_rest_not_computed = "corrected_item_rest"
  )
  map_metric <- function(metric) {
    if (metric %in% names(aliases)) aliases[[metric]] else metric
  }

  # "unavailable" is a cell whose diagnostic was not computed; it outranks an
  # informational entry but never a flag.
  severity_rank <- c(
    none = 0L,
    info = 1L,
    unavailable = 2L,
    review = 3L,
    concern = 4L
  )

  item_log <- x$decision_log[
    x$decision_log$object %in% items,
    ,
    drop = FALSE
  ]
  item_log$severity[item_log$metric == "item_rest_not_computed"] <- "unavailable"

  if (nrow(item_log) > 0L) {
    for (i in seq_len(nrow(item_log))) {
      metric <- map_metric(item_log$metric[[i]])

      if (!metric %in% metrics) {
        next
      }

      idx <- grid$item == item_log$object[[i]] &
        grid$metric == metric

      old <- grid$severity[idx]

      if (
        length(old) == 1L &&
          severity_rank[[item_log$severity[[i]]]] >
            severity_rank[[old]]
      ) {
        grid$severity[idx] <- item_log$severity[[i]]
      }
    }
  }

  review <- nomo_screen_item_review(x)
  review <- review[review$item %in% items, , drop = FALSE]
  negative_n <- nomo_screen_review_negative_n(review)

  for (i in seq_len(nrow(review))) {
    item <- review$item[[i]]

    if (
      !is.na(negative_n[[i]]) &&
        negative_n[[i]] > 0L
    ) {
      grid$severity[
        grid$item == item &
          grid$metric == "negative_interitem_pairs"
      ] <- "review"
    }

    if (
      identical(review$item_type[[i]], "ordered") &&
        review$n_unused_response_categories[[i]] > 0L
    ) {
      grid$severity[
        grid$item == item &
          grid$metric == "unused_response_categories"
      ] <- "review"
    }

    # An item left out of the correlation diagnostics has neither value, and
    # the map says so rather than showing it as unflagged (#145).
    if (isFALSE(review$relationship_eligible[[i]])) {
      cells <- grid$item == item &
        grid$metric %in% c("corrected_item_rest", "negative_interitem_pairs") &
        grid$severity %in% c("none", "info")
      grid$severity[cells] <- "unavailable"
    }
  }

  labels <- c(
    all_missing = "All missing",
    constant = "Constant",
    missingness = "Missingness",
    item_type = "Item type",
    coding = "Coding",
    response_concentration = "Response concentration",
    near_zero_variance = "Near-zero variance",
    corrected_item_rest = "Item-rest",
    negative_interitem_pairs = "Negative inter-item",
    unused_response_categories = "Unused categories"
  )

  grid$metric_label <- unname(labels[grid$metric])
  grid$item <- factor(grid$item, levels = rev(items))
  grid$metric_label <- factor(
    grid$metric_label,
    levels = unname(labels[metrics])
  )
  grid$severity <- factor(
    grid$severity,
    levels = names(severity_rank)
  )
  # The status the plot draws: an informational entry is not a flag.
  grid$status <- nomo_plot_status(grid$severity)

  grid
}


# One status per cell, drawn with the shared status shapes and colors (guide
# point 27): a small gray dot where nothing was flagged, so the flags stand out
# while every cell still shows that it was checked (#145, cons-4, clarity-14).
nomo_screen_plot_evidence <- function(x, items) {
  dat <- nomo_screen_evidence_data(x, items)
  dat$point_size <- ifelse(dat$status == "none", 1.2, 3)

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = metric_label,
      y = item
    )
  ) +
    ggplot2::geom_tile(
      fill = "#f2f2f2",
      linewidth = 0.4,
      colour = "white"
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = status, colour = status, size = point_size)
    ) +
    ggplot2::scale_size_identity() +
    nomo_plot_status_scales(dat$status, name = "Flag") +
    nomo_plot_labs(
      title = "Item evidence map",
      subtitle = paste(
        "Cells summarize diagnostic attention;",
        "they are not retention decisions."
      ),
      caption = paste(
        "Informational log entries are not flags. summary(x) gives the reason",
        "for each flag."
      ),
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 40,
        hjust = 1
      )
    )
}


# The item-rest value a review reads: within the item's scale when two or more
# scales were declared, otherwise the pooled one. An item in a declared scale
# with no value there, such as the only item of its scale, has none: the
# pooled value would judge it against other constructs (#145).
nomo_screen_review_item_rest <- function(review) {
  within <- review[["scale_item_rest_r"]]
  if (is.null(within)) return(review$corrected_item_rest_r)
  scale <- review[["scale"]]
  in_scale <- if (is.null(scale)) !is.na(within) else !is.na(scale)
  ifelse(in_scale, within, review$corrected_item_rest_r)
}


# Likewise the count of negative inter-item pairs: those within the item's
# scale when scales were declared, otherwise all of them.
nomo_screen_review_negative_n <- function(review) {
  within <- review[["scale_negative_interitem_n"]]
  if (is.null(within)) return(review$negative_interitem_n)
  ifelse(is.na(within), review$negative_interitem_n, within)
}


nomo_screen_plot_item_rest <- function(x, items, show_values) {
  review <- nomo_screen_item_review(x)
  review$item_rest <- nomo_screen_review_item_rest(review)
  dat <- review[
    review$item %in% items &
      !is.na(review$item_rest),
    ,
    drop = FALSE
  ]

  if (nrow(dat) == 0L) {
    stop(
      paste(
        "No corrected item-rest correlations are available",
        "for the selected items. Items stored as categories, constant, or with",
        "non-finite values are left out; nomo_table(x, \"decision_log\") says which."
      ),
      call. = FALSE
    )
  }

  dat$item_rest_attention <- "none"

  for (i in seq_len(nrow(dat))) {
    log_rows <- x$decision_log[
      x$decision_log$object == dat$item[[i]] &
        x$decision_log$metric == "corrected_item_rest",
      ,
      drop = FALSE
    ]

    if (any(log_rows$severity == "concern")) {
      dat$item_rest_attention[[i]] <- "concern"
    } else if (any(log_rows$severity == "review")) {
      dat$item_rest_attention[[i]] <- "review"
    }
  }

  dat$item_rest_attention <- factor(
    dat$item_rest_attention,
    levels = c("none", "review", "concern"),
    ordered = TRUE
  )
  # The flag is carried by the point's shape, with color repeating it, so it
  # reads in grayscale (guide point 27; #145, pres-6).
  dat$status <- nomo_plot_status(dat$item_rest_attention)
  dat$item <- factor(dat$item, levels = rev(dat$item))

  reference <- nomo_screen_item_rest_reference(x$guidance)
  has_reference <- is.finite(reference)
  within <- any(!is.na(dat[["scale_item_rest_r"]]))

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = item,
      y = item_rest
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.4
    ) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = item, y = 0, yend = item_rest),
      colour = "#BFBFBF",
      linewidth = 0.7
    ) +
    ggplot2::geom_point(
      ggplot2::aes(shape = status, colour = status),
      size = 2.8
    ) +
    ggplot2::coord_flip() +
    nomo_plot_status_scales(dat$status, name = "Flag") +
    ggplot2::scale_y_continuous(labels = nomo_plot_bounded_labels) +
    nomo_plot_labs(
      title = "Corrected item-rest relationships",
      subtitle = if (within) {
        "Each item against the sum of the other items of its declared scale."
      } else {
        "Each item against the sum of the other items."
      },
      caption = if (has_reference) {
        sprintf(
          paste(
            "The dashed line marks the %s teaching reference, which prompts",
            "review and does not determine item retention."
          ),
          nomo_present_stat(reference, "r")
        )
      },
      x = NULL,
      y = "Corrected item-rest correlation"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  if (has_reference) {
    p <- p + ggplot2::geom_hline(
      yintercept = reference,
      linetype = 2,
      linewidth = 0.5
    )
  }

  if (isTRUE(show_values)) {
    # The values sit in a column past the longest bar and the reference line.
    # Beside each bar, a flagged item's value, just under the reference, was
    # drawn across the line.
    label_at <- max(c(dat$item_rest, if (has_reference) reference, 0)) + 0.03
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(label = nomo_present_stat(item_rest, "r", reference = reference)),
        y = label_at,
        hjust = 0,
        size = 3
      ) +
      ggplot2::expand_limits(y = label_at + 0.08)
  }

  p
}


nomo_screen_plot_interitem <- function(x, items, show_values) {
  pairs <- x$inter_item_correlations[
    x$inter_item_correlations$item1 %in% items &
      x$inter_item_correlations$item2 %in% items,
    ,
    drop = FALSE
  ]

  eligible <- x$relationship_summary$item[
    x$relationship_summary$relationship_eligible &
      x$relationship_summary$item %in% items
  ]

  if (length(eligible) < 2L || nrow(pairs) == 0L) {
    stop(
      "At least two relationship-eligible selected items are required.",
      call. = FALSE
    )
  }

  reverse_pairs <- tibble::tibble(
    item1 = pairs$item2,
    item2 = pairs$item1,
    r = pairs$r,
    n_pair = pairs$n_pair
  )

  diagonal <- tibble::tibble(
    item1 = eligible,
    item2 = eligible,
    r = 1,
    n_pair = NA_integer_
  )

  dat <- dplyr::bind_rows(
    pairs,
    reverse_pairs,
    diagonal
  )

  dat$item1 <- factor(dat$item1, levels = items)
  dat$item2 <- factor(dat$item2, levels = rev(items))

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = item1,
      y = item2,
      fill = r
    )
  ) +
    ggplot2::geom_tile(
      linewidth = 0.35,
      colour = "white"
    ) +
    ggplot2::scale_fill_gradient2(
      low = "#b2182b",
      mid = "white",
      high = "#2166ac",
      midpoint = 0,
      limits = c(-1, 1),
      labels = nomo_plot_bounded_labels,
      na.value = "#eeeeee"
    ) +
    ggplot2::coord_fixed() +
    nomo_plot_labs(
      title = "Inter-item correlation map",
      subtitle = "Pearson relationships are descriptive screening evidence.",
      x = NULL,
      y = NULL,
      fill = "Pearson r"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1
      )
    )

  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        label = nomo_present_stat(r, "r")
      ),
      size = 3
    )
  }

  p
}


# A continuous item has as many distinct values as respondents, so a bar per
# value is unreadable; it is shown as a histogram instead (#89). A selection
# that mixes continuous and categorical items cannot share one axis type, so
# the category bars are drawn and the caption names the continuous items.
nomo_screen_plot_responses <- function(x, items, show_values) {
  types <- x$item_summary$item_type[match(items, x$item_summary$item)]
  continuous <- items[types %in% "numeric_continuous"]
  if (length(continuous) == length(items)) {
    return(nomo_screen_plot_histograms(x, items))
  }
  items <- setdiff(items, continuous)

  dat <- x$response_distribution[
    x$response_distribution$item %in% items &
      !x$response_distribution$missing,
    ,
    drop = FALSE
  ]

  if (nrow(dat) == 0L) {
    stop(
      paste(
        "No observed/declared response categories are available",
        "for the selected items."
      ),
      call. = FALSE
    )
  }

  dat$item <- factor(dat$item, levels = items)
  response_key <- paste0(
    seq_len(nrow(dat)),
    "___NOMO___",
    dat$response
  )
  dat$response_key <- factor(
    response_key,
    levels = response_key
  )

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = response_key,
      y = proportion_observed
    )
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::facet_wrap(
      ~item,
      scales = "free_x"
    ) +
    ggplot2::scale_x_discrete(
      labels = function(z) sub(
        "^[0-9]+___NOMO___",
        "",
        z
      )
    ) +
    ggplot2::scale_y_continuous(
      labels = function(z) paste0(round(100 * z), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.12))
    ) +
    nomo_plot_labs(
      title = "Item response profiles",
      subtitle = "Declared but unused factor levels remain visible at 0%.",
      x = "Response category",
      y = "Observed proportion"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%.0f%%", 100 * proportion_observed)
      ),
      vjust = -0.35,
      size = 3
    )
  }

  if (length(continuous)) {
    p <- p + nomo_plot_labs(caption = paste0(
      "Continuous items are not shown here: ", paste(continuous, collapse = ", "),
      ". See plot(x, type = \"responses\", items = c(",
      paste0("\"", continuous, "\"", collapse = ", "), "))."
    ))
  }

  p
}


nomo_screen_plot_histograms <- function(x, items, bins = 20L) {
  dat <- x$response_distribution[
    x$response_distribution$item %in% items &
      !x$response_distribution$missing,
    ,
    drop = FALSE
  ]

  if (nrow(dat) == 0L) {
    stop("No observed responses are available for the selected items.", call. = FALSE)
  }

  dat$item <- factor(dat$item, levels = items)
  dat$value <- as.numeric(dat$response)

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = value,
      weight = n,
      y = ggplot2::after_stat(density * width)
    )
  ) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::facet_wrap(~item, scales = "free_x") +
    ggplot2::scale_y_continuous(
      labels = function(z) paste0(round(100 * z), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    nomo_plot_labs(
      title = "Item response distributions",
      subtitle = sprintf(
        "Continuous items: share of observed responses in each of %d equal-width bins.",
        bins
      ),
      x = "Response",
      y = "Observed proportion"
    ) +
    ggplot2::theme_minimal(base_size = 11)
}


nomo_screen_plot_missingness <- function(x, items, show_values) {
  dat <- x$item_summary[
    x$item_summary$item %in% items,
    ,
    drop = FALSE
  ]

  dat$item <- factor(dat$item, levels = rev(items))

  max_missing <- max(c(dat$pct_missing, 0.01), na.rm = TRUE)
  label_pad <- max(0.0025, max_missing * 0.025)
  dat$missingness_label_position <- ifelse(
    dat$pct_missing == 0,
    label_pad,
    dat$pct_missing + label_pad
  )

  upper <- min(
    1,
    max(c(dat$missingness_label_position, 0.01), na.rm = TRUE) * 1.12
  )

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = item,
      y = pct_missing
    )
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(
      labels = function(z) paste0(round(100 * z), "%"),
      limits = c(0, upper),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    nomo_plot_labs(
      title = "Item missingness",
      subtitle = paste(
        "Missingness is described here without",
        "a universal deletion cutoff."
      ),
      x = NULL,
      y = "Missing responses"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        y = missingness_label_position,
        label = nomo_present_percent(pct_missing, base = n)
      ),
      hjust = 0,
      size = 3
    )
  }

  p
}

utils::globalVariables(c(
  "metric_label",
  "status",
  "point_size",
  "item",
  "severity",
  "item_rest",
  "attention",
  "item_rest_attention",
  "r",
  "item1",
  "item2",
  "response",
  "response_key",
  "proportion_observed",
  "pct_missing",
  "missingness_label_position",
  "value",
  "n",
  "density",
  "width"
))
