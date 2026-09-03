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
#'   guidance settings.
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
    guidance = object$guidance
  )

  class(out) <- c("summary_nomo_screen", "list")
  out
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

  cat("<summary_nomo_screen>\n")
  cat(sprintf(
    "Cases: %d | Items: %d | No review flag: %d | Review: %d | Concern: %d\n",
    overview$n_cases,
    overview$n_items,
    overview$n_no_review_flag,
    overview$n_review,
    overview$n_concern
  ))
  cat(sprintf(
    paste0(
      "Missingness flags: %d | Constant: %d | All missing: %d | ",
      "Relationship eligible: %d\n"
    ),
    overview$n_items_with_missing,
    overview$n_constant,
    overview$n_all_missing,
    overview$n_relationship_eligible
  ))

  display <- x$item_review[
    ,
    c(
      "item",
      "item_type",
      "attention",
      "pct_missing",
      "mode_prop",
      "corrected_item_rest_r",
      "review_metrics"
    ),
    drop = FALSE
  ]

  cat("\nIntegrated item review:\n")
  print(display, n = nrow(display), width = Inf)

  cat(
    "\n`attention` is a review aid, not an automatic retention/deletion decision.\n"
  )
  invisible(x)
}


#' Plot a nomo_screen audit
#'
#' Visual diagnostics complement the numerical screening output. The default
#' evidence map integrates multiple diagnostic signals without converting them
#' into a pass/fail scale. Additional plot types display item-rest relationships,
#' inter-item correlations, response-category use, or missingness.
#'
#' @param x A `nomo_screen` object.
#' @param y Ignored; included for compatibility with the base `plot()` generic.
#' @param type Plot type: `"evidence"`, `"item_rest"`, `"interitem"`,
#'   `"responses"`, or `"missingness"`.
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

  type <- match.arg(type)
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
      !is.na(out$negative_interitem_n[[i]]) &&
        out$negative_interitem_n[[i]] > 0L
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

  map_metric <- function(metric) {
    if (
      metric %in% c(
        "response_concentration",
        "floor_concentration",
        "ceiling_concentration"
      )
    ) {
      return("response_concentration")
    }

    metric
  }

  severity_rank <- c(
    none = 0L,
    info = 1L,
    review = 2L,
    concern = 3L
  )

  item_log <- x$decision_log[
    x$decision_log$object %in% items,
    ,
    drop = FALSE
  ]

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

  for (i in seq_len(nrow(review))) {
    item <- review$item[[i]]

    if (
      !is.na(review$negative_interitem_n[[i]]) &&
        review$negative_interitem_n[[i]] > 0L
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
  }

  labels <- c(
    all_missing = "All missing",
    constant = "Constant",
    missingness = "Missingness",
    item_type = "Item type",
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
    levels = c("none", "info", "review", "concern")
  )

  grid
}


nomo_screen_plot_evidence <- function(x, items) {
  dat <- nomo_screen_evidence_data(x, items)

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = metric_label,
      y = item,
      fill = severity
    )
  ) +
    ggplot2::geom_tile(
      linewidth = 0.4,
      colour = "white"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = ifelse(
          severity == "none",
          "",
          toupper(substr(as.character(severity), 1L, 1L))
        )
      ),
      size = 3
    ) +
    ggplot2::scale_fill_manual(
      values = c(
        none = "#f2f2f2",
        info = "#56B4E9",
        review = "#E69F00",
        concern = "#D55E00"
      ),
      drop = TRUE
    ) +
    ggplot2::labs(
      title = "nomologR item evidence map",
      subtitle = paste(
        "Cells summarize diagnostic attention;",
        "they are not retention decisions."
      ),
      x = NULL,
      y = NULL,
      fill = "Attention"
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


nomo_screen_plot_item_rest <- function(x, items, show_values) {
  review <- nomo_screen_item_review(x)
  dat <- review[
    review$item %in% items &
      !is.na(review$corrected_item_rest_r),
    ,
    drop = FALSE
  ]

  if (nrow(dat) == 0L) {
    stop(
      paste(
        "No corrected item-rest correlations are available",
        "for the selected items."
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
  dat$item <- factor(dat$item, levels = rev(dat$item))

  p <- ggplot2::ggplot(
    dat,
    ggplot2::aes(
      x = item,
      y = corrected_item_rest_r,
      fill = item_rest_attention
    )
  ) +
    ggplot2::geom_col(width = 0.72) +
    ggplot2::geom_hline(
      yintercept = 0,
      linewidth = 0.4
    ) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      values = c(
        none = "#009E73",
        review = "#E69F00",
        concern = "#D55E00"
      ),
      drop = TRUE
    ) +
    ggplot2::labs(
      title = "Corrected item-rest relationships",
      subtitle = paste(
        "Reference lines guide inspection;",
        "they do not determine item retention."
      ),
      x = NULL,
      y = "Corrected item-rest correlation",
      fill = "Item-rest"
    ) +
    ggplot2::theme_minimal(base_size = 11)

  reference <- x$guidance$item_total_reference

  if (
    is.numeric(reference) &&
      length(reference) == 1L &&
      is.finite(reference)
  ) {
    p <- p + ggplot2::geom_hline(
      yintercept = reference,
      linetype = 2,
      linewidth = 0.5
    )
  }

  if (isTRUE(show_values)) {
    p <- p + ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%.2f", corrected_item_rest_r)
      ),
      hjust = -0.15,
      size = 3
    )
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
      na.value = "#eeeeee"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = "Inter-item correlation map",
      subtitle = "Pearson relationships are descriptive screening evidence.",
      x = NULL,
      y = NULL,
      fill = "r"
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
        label = ifelse(
          is.na(r),
          "",
          sprintf("%.2f", r)
        )
      ),
      size = 3
    )
  }

  p
}


nomo_screen_plot_responses <- function(x, items, show_values) {
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
    ggplot2::labs(
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

  p
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
    ggplot2::labs(
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
        label = sprintf("%.1f%%", 100 * pct_missing)
      ),
      hjust = 0,
      size = 3
    )
  }

  p
}

utils::globalVariables(c(
  "metric_label",
  "item",
  "severity",
  "corrected_item_rest_r",
  "attention",
  "item_rest_attention",
  "r",
  "item1",
  "item2",
  "response",
  "response_key",
  "proportion_observed",
  "pct_missing",
  "missingness_label_position"
))
