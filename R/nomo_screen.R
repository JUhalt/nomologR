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
#' @param guidance Guidance settings from [nomo_defaults()].
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
#' @return An object of class `nomo_screen` containing item summaries, response
#'   distributions, case-level completeness diagnostics, an evidence-guided
#'   decision log, and the guidance settings used.
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
#' @export
nomo_screen <- function(data, items = NULL, guidance = nomo_defaults()) {
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

  out <- list(
    call = match.call(),
    n_cases = nrow(data),
    items = items,
    item_summary = item_summary,
    response_distribution = response_distribution,
    case_summary = case_summary,
    decision_log = decision_log,
    guidance = guidance
  )

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
