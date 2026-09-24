# Insufficient-effort responding indices ---------------------------------------
#
# Case-level indices from the careless-responding literature (Meade & Craig,
# 2012; Huang et al., 2012; Curran, 2016). Every index is reported; none is a
# rule. Curran's own advice is that these techniques be used in series, because
# each has blind spots, and his worked example of inter-item standard deviation
# shows why: a respondent who gives the same answer to every item receives the
# best possible score on that index while being the clearest straight-liner.


# Longest run of identical consecutive responses. Curran gives the count of
# sequential matches as bounded by 1 and the length of the assessment.
# Callers always pass at least one item, so every row has a run.
nomo_effort_long_string <- function(responses) {
  apply(responses, 1L, function(row) max(rle(as.numeric(row))$lengths))
}


nomo_effort_long_string_mean <- function(responses, scales) {
  if (is.null(scales) || !length(scales)) return(rep(NA_real_, nrow(responses)))

  per_scale <- vapply(scales, function(items) {
    items <- intersect(items, colnames(responses))
    if (length(items) < 2L) return(rep(NA_real_, nrow(responses)))
    nomo_effort_long_string(responses[, items, drop = FALSE])
  }, numeric(nrow(responses)))

  if (is.null(dim(per_scale))) per_scale <- matrix(per_scale, nrow = nrow(responses))
  # No scale with two items gives no value, not the NaN of a mean over nothing.
  if (all(is.na(per_scale))) return(rep(NA_real_, nrow(responses)))
  rowMeans(per_scale, na.rm = TRUE)
}


# Inter-item standard deviation (Marjanovic, Holden, Struthers, Cribbie, &
# Greenglass, 2015): the within-person standard deviation of responses about
# that person's own mean. A random responder answers across the whole response
# range and so produces a large ISD; a consistent responder produces a small
# one.
nomo_effort_inter_item_sd <- function(responses) {
  apply(responses, 1L, function(row) {
    row <- row[is.finite(row)]
    if (length(row) < 2L) return(NA_real_)
    stats::sd(row)
  })
}


# Marjanovic et al. do not compute one ISD across every item. They compute an
# ISD within each subscale and average them, and it is that mean ISD that
# carried their highest classification accuracy. Averaging within subscales
# matters because a person scoring high on one construct and low on another
# produces a large spread across the combined item set while being perfectly
# consistent within each scale.
nomo_effort_inter_item_sd_mean <- function(responses, scales) {
  if (is.null(scales) || !length(scales)) return(rep(NA_real_, nrow(responses)))

  per_scale <- vapply(scales, function(items) {
    items <- intersect(items, colnames(responses))
    if (length(items) < 2L) return(rep(NA_real_, nrow(responses)))
    nomo_effort_inter_item_sd(responses[, items, drop = FALSE])
  }, numeric(nrow(responses)))

  if (is.null(dim(per_scale))) {
    per_scale <- matrix(per_scale, nrow = nrow(responses))
  }
  if (all(is.na(per_scale))) return(rep(NA_real_, nrow(responses)))
  rowMeans(per_scale, na.rm = TRUE)
}


# Mahalanobis distance of each response vector from the sample mean vector.
# Reported as the distance itself, not its square, following Curran's
# definition. Refuses rather than guesses when the covariance is singular.
nomo_effort_mahalanobis <- function(responses) {
  complete <- stats::complete.cases(responses)
  out <- rep(NA_real_, nrow(responses))
  if (sum(complete) <= ncol(responses)) return(out)

  block <- responses[complete, , drop = FALSE]
  centre <- colMeans(block)
  covariance <- stats::cov(block)

  inverse <- tryCatch(solve(covariance), error = function(e) NULL)
  if (is.null(inverse)) return(out)

  squared <- stats::mahalanobis(block, center = centre, cov = inverse,
                                inverted = TRUE)
  squared[squared < 0] <- NA_real_
  out[complete] <- sqrt(squared)
  out
}


# Psychometric antonyms and synonyms: item pairs are selected by their
# between-person correlations, then each person's responses to the first
# members of those pairs are correlated with their responses to the second.
# Curran reports .60 as his own opinion of a reasonable magnitude while saying
# plainly that "there is no firm basis for this rule of thumb", so the value is
# an argument rather than a constant.
nomo_effort_pairs <- function(responses, direction = c("antonym", "synonym"),
                              min_magnitude = 0.60) {
  direction <- match.arg(direction)
  items <- colnames(responses)
  out <- list(
    values = rep(NA_real_, nrow(responses)),
    pairs = tibble::tibble(
      first = character(), second = character(), correlation = numeric()
    )
  )
  if (length(items) < 4L) return(out)

  correlations <- suppressWarnings(
    stats::cor(responses, use = "pairwise.complete.obs")
  )
  if (anyNA(correlations)) correlations[is.na(correlations)] <- 0

  candidates <- which(upper.tri(correlations), arr.ind = TRUE)
  values <- correlations[upper.tri(correlations)]
  keep <- if (identical(direction, "antonym")) {
    values <= -abs(min_magnitude)
  } else {
    values >= abs(min_magnitude)
  }
  if (!any(keep)) return(out)

  candidates <- candidates[keep, , drop = FALSE]
  values <- values[keep]

  # An item may appear in only one pair, so a single strongly correlated item
  # cannot dominate the index. Strongest pairs are taken first.
  ordering <- order(abs(values), decreasing = TRUE)
  candidates <- candidates[ordering, , drop = FALSE]
  values <- values[ordering]

  used <- character()
  chosen <- integer()
  # The first candidate is always free, so at least one pair is chosen.
  for (i in seq_len(nrow(candidates))) {
    pair <- items[candidates[i, ]]
    if (any(pair %in% used)) next
    used <- c(used, pair)
    chosen <- c(chosen, i)
  }

  candidates <- candidates[chosen, , drop = FALSE]
  values <- values[chosen]

  # Each person's index is a correlation across the pairs, so its N is the
  # number of pairs (Curran, 2016). With two pairs every correlation is exactly
  # +1 or -1, which would flag attentive respondents by arithmetic alone. Three
  # is the least that yields a correlation at all; Meade and Craig (2012) used
  # five. The pairs found are always returned, so a reader can see why no value
  # was computed.
  out$pairs <- tibble::tibble(
    first = items[candidates[, 1L]],
    second = items[candidates[, 2L]],
    correlation = as.numeric(values)
  )
  if (nrow(candidates) < 3L) return(out)

  first <- responses[, items[candidates[, 1L]], drop = FALSE]
  second <- responses[, items[candidates[, 2L]], drop = FALSE]

  out$values <- vapply(seq_len(nrow(responses)), function(i) {
    a <- as.numeric(first[i, ])
    b <- as.numeric(second[i, ])
    ok <- is.finite(a) & is.finite(b)
    if (sum(ok) < 2L) return(NA_real_)
    if (stats::sd(a[ok]) == 0 || stats::sd(b[ok]) == 0) return(NA_real_)
    stats::cor(a[ok], b[ok])
  }, numeric(1))

  out
}


# Even-odd consistency: each scale is split into its odd-numbered and
# even-numbered items, each half averaged, and the two resulting vectors
# correlated within the person across scales, then corrected with the
# Spearman-Brown formula (Meade & Craig, 2012; Curran, 2016). Needs at least
# two scales, because a correlation over one pair of points is not defined.
nomo_effort_even_odd <- function(responses, scales) {
  out <- rep(NA_real_, nrow(responses))
  if (is.null(scales)) return(out)

  # The within-person correlation runs across scales, so its N is the number of
  # scales (Curran, 2016). With two scales it is always exactly +1 or -1, and
  # the Spearman-Brown correction then divides by zero. Three is the least that
  # yields a correlation at all.
  usable <- Filter(function(items) {
    length(intersect(items, colnames(responses))) >= 2L
  }, scales)
  if (length(usable) < 3L) return(out)

  odd_halves <- vapply(usable, function(items) {
    items <- intersect(items, colnames(responses))
    rowMeans(responses[, items[seq(1L, length(items), by = 2L)], drop = FALSE],
             na.rm = TRUE)
  }, numeric(nrow(responses)))

  # Usable scales have at least two items, so each has an even half.
  even_halves <- vapply(usable, function(items) {
    items <- intersect(items, colnames(responses))
    rowMeans(responses[, items[seq(2L, length(items), by = 2L)], drop = FALSE],
             na.rm = TRUE)
  }, numeric(nrow(responses)))

  for (i in seq_len(nrow(responses))) {
    a <- as.numeric(odd_halves[i, ])
    b <- as.numeric(even_halves[i, ])
    ok <- is.finite(a) & is.finite(b)
    if (sum(ok) < 2L) next
    if (stats::sd(a[ok]) == 0 || stats::sd(b[ok]) == 0) next
    if (sum(ok) < 3L) next
    r <- stats::cor(a[ok], b[ok])
    # Spearman-Brown correction for the halved scale length. At r = -1 it is
    # undefined, and near it the correction diverges, so those are NA rather
    # than a very large negative number that means nothing.
    if (!is.finite(r) || (1 + r) < 1e-8) next
    out[[i]] <- (2 * r) / (1 + r)
  }

  out
}



# Assembly ---------------------------------------------------------------------
#
# Which responses each index reads matters, and it is not the same for all of
# them. Long-string reads the raw responses, because a respondent who gives the
# same answer throughout is straight-lining whatever the items' keying, and
# recoding would break the run it exists to find. Psychometric antonyms and
# synonyms read the raw responses too, since reverse-keyed items are what give
# antonym pairs their negative correlations. Mahalanobis distance is unchanged
# by recoding a column. Even-odd consistency and inter-item standard deviation
# read the recoded responses when the researcher declares keying, because an
# attentive respondent answering a reversed item correctly would otherwise look
# inconsistent. Recoding happens on an internal copy only; the data returned
# and the data supplied are never altered.
nomo_effort_recode <- function(responses, reverse, scale_range) {
  if (is.null(reverse) || !length(reverse)) return(responses)
  reverse <- intersect(reverse, colnames(responses))
  recoded <- responses
  for (item in reverse) {
    recoded[, item] <- scale_range[[1L]] + scale_range[[2L]] - recoded[, item]
  }
  recoded
}


nomo_effort_screen <- function(selected, scales = NULL, reverse = NULL,
                               scale_range = NULL, pair_magnitude = 0.60) {
  responses <- as.matrix(selected)
  storage.mode(responses) <- "double"
  n_items <- ncol(responses)
  recoded <- nomo_effort_recode(responses, reverse, scale_range)

  antonyms <- nomo_effort_pairs(responses, "antonym", pair_magnitude)
  synonyms <- nomo_effort_pairs(responses, "synonym", pair_magnitude)

  effort <- tibble::tibble(
    row = seq_len(nrow(responses)),
    long_string = as.numeric(nomo_effort_long_string(responses)),
    long_string_mean = as.numeric(nomo_effort_long_string_mean(responses, scales)),
    inter_item_sd = as.numeric(nomo_effort_inter_item_sd(recoded)),
    inter_item_sd_mean = as.numeric(nomo_effort_inter_item_sd_mean(recoded, scales)),
    mahalanobis = as.numeric(nomo_effort_mahalanobis(responses)),
    even_odd = as.numeric(nomo_effort_even_odd(recoded, scales)),
    antonym_r = as.numeric(antonyms$values),
    synonym_r = as.numeric(synonyms$values)
  )

  # Only rules a source states explicitly become flags, and each carries the
  # source's own qualification. Curran (2016) offers half the scale length as a
  # "conservative rule of thumb" for long-string, saying it is "not the best
  # cut score for all scales", and treats a positive within-person antonym
  # correlation or a negative synonym correlation as close to certain evidence
  # of careless responding. The other indices have no stated rule, so they are
  # reported without a flag.
  long_limit <- ceiling(n_items / 2)
  effort$flag_long_string <- is.finite(effort$long_string) &
    effort$long_string >= long_limit
  effort$flag_antonym <- is.finite(effort$antonym_r) & effort$antonym_r > 0
  effort$flag_synonym <- is.finite(effort$synonym_r) & effort$synonym_r < 0

  flag_names <- c(
    flag_long_string = "long_string",
    flag_antonym = "antonym",
    flag_synonym = "synonym"
  )
  flags <- as.matrix(effort[, names(flag_names)])
  effort$n_flags <- as.integer(rowSums(flags))
  effort$flagged_by <- apply(flags, 1L, function(r) {
    paste(unname(flag_names)[r], collapse = ", ")
  })

  list(
    effort = effort,
    antonym_pairs = antonyms$pairs,
    synonym_pairs = synonyms$pairs,
    long_string_limit = long_limit,
    # NULL means nobody said; character(0) means someone checked and nothing is
    # reversed. Those are different facts and are reported differently.
    keying_declared = !is.null(reverse),
    n_reversed = length(reverse)
  )
}



# Decision log -----------------------------------------------------------------

nomo_effort_log <- function(result, n_items) {
  log <- nomo_log_new()
  e <- result$effort
  n <- nrow(e)
  pct <- function(k) sprintf("%.1f%%", 100 * k / max(n, 1L))

  n_long <- sum(e$flag_long_string)
  log <- nomo_log_add(
    log, stage = "screen", object = "cases", metric = "long_string",
    value = n_long,
    reference = sprintf(
      "Curran (2016): a run of at least half the items (%d of %d), a conservative rule of thumb",
      result$long_string_limit, n_items
    ),
    severity = if (n_long > 0L) "review" else "info",
    observation = sprintf(
      "%d case%s (%s) gave the same response to at least %d consecutive items.",
      n_long, if (n_long == 1L) "" else "s", pct(n_long), result$long_string_limit
    ),
    recommendation = paste(
      "Curran offers half the scale length as a conservative starting point and",
      "says it is not the best cut score for every scale: a scale whose items",
      "barely vary in intensity invites long runs from careful respondents.",
      "Inspect these cases before deciding anything about them."
    )
  )

  for (kind in c("antonym", "synonym")) {
    pairs <- result[[paste0(kind, "_pairs")]]
    n_pairs <- nrow(pairs)
    column <- paste0(kind, "_r")
    flag <- paste0("flag_", kind)

    if (n_pairs < 3L) {
      log <- nomo_log_add(
        log, stage = "screen", object = "cases",
        metric = paste0("psychometric_", kind),
        value = n_pairs,
        reference = "Curran (2016); Meade & Craig (2012)",
        severity = "info",
        observation = sprintf(
          "Only %d psychometric %s pair%s met the correlation threshold, so the index was not computed.",
          n_pairs, kind, if (n_pairs == 1L) "" else "s"
        ),
        recommendation = paste(
          "Each respondent's value is a correlation across the pairs, so it needs",
          "at least three; with two, every value is exactly +1 or -1 and would",
          "flag attentive respondents by arithmetic alone."
        )
      )
      next
    }

    n_flag <- sum(e[[flag]])
    rule <- if (identical(kind, "antonym")) {
      "a positive within-person correlation across antonym pairs"
    } else {
      "a negative within-person correlation across synonym pairs"
    }
    coarse <- n_pairs < 5L

    log <- nomo_log_add(
      log, stage = "screen", object = "cases",
      metric = paste0("psychometric_", kind),
      value = n_flag,
      reference = sprintf(
        "Curran (2016): %s, computed here over %d pair%s",
        rule, n_pairs, if (n_pairs == 1L) "" else "s"
      ),
      severity = if (n_flag > 0L) "review" else "info",
      observation = sprintf(
        "%d case%s (%s) showed %s.",
        n_flag, if (n_flag == 1L) "" else "s", pct(n_flag), rule
      ),
      recommendation = if (coarse) {
        sprintf(paste(
          "Only %d pairs were available. Each value rests on that many points,",
          "so attentive respondents will cross zero by chance and some of these",
          "flags are arithmetic rather than behavior. Meade and Craig (2012) used",
          "five pairs. Read these flags alongside the other indices, not alone."
        ), n_pairs)
      } else {
        paste(
          "Curran treats this as close to certain evidence of careless",
          "responding. Read it alongside the other indices before acting."
        )
      }
    )
  }

  if (any(is.finite(e$even_odd))) {
    negative <- sum(is.finite(e$even_odd) & e$even_odd < 0)
    log <- nomo_log_add(
      log, stage = "screen", object = "cases", metric = "even_odd",
      value = negative,
      reference = "Meade & Craig (2012); Curran (2016); no stated cut score",
      severity = "info",
      observation = sprintf(
        "%d case%s had a negative even-odd consistency, meaning the halves of each scale disagreed.",
        negative, if (negative == 1L) "" else "s"
      ),
      recommendation = paste(
        "No source states a cut score, so no case is flagged on this index.",
        "The Spearman-Brown correction extrapolates for negative correlations",
        "and can fall below -1; a value below zero indicates inconsistency and",
        "its exact size does not carry further meaning.",
        if (isTRUE(result$keying_declared) && result$n_reversed > 0L) {
          "Reverse-keyed items were recoded for this index only."
        } else if (isTRUE(result$keying_declared)) {
          "Keying was declared with no reverse-keyed items, so nothing was recoded."
        } else {
          paste(
            "No reverse keying was declared, so if any items are reverse keyed",
            "this index is wrong for attentive respondents: declare them with",
            "`reverse`."
          )
        }
      )
    )
  }

  # Curran's warning, made concrete with the researcher's own data: the cases
  # long-string flags are the ones inter-item standard deviation rates most
  # consistent, so sorting on that index alone would keep the worst responders.
  straight <- e$flag_long_string & is.finite(e$inter_item_sd)
  if (any(straight)) {
    threshold <- stats::quantile(e$inter_item_sd, 0.10, na.rm = TRUE, names = FALSE)
    inverted <- sum(straight & e$inter_item_sd <= threshold)
    if (inverted > 0L) {
      log <- nomo_log_add(
        log, stage = "screen", object = "cases", metric = "index_disagreement",
        value = inverted,
        reference = "Curran (2016): these methods should be used in series",
        severity = "review",
        observation = sprintf(
          paste(
            "%d case%s flagged by long-string %s among the lowest tenth on",
            "inter-item standard deviation, which rates them as the most",
            "consistent respondents in the sample."
          ),
          inverted, if (inverted == 1L) "" else "s",
          if (inverted == 1L) "is" else "are"
        ),
        recommendation = paste(
          "The indices disagree because they detect different failures.",
          "Inter-item standard deviation detects random responding (Marjanovic",
          "et al., 2015) and gives a respondent who answers every item",
          "identically the best possible score. Sorting on it alone would keep",
          "exactly the cases long-string flags."
        )
      )
    }
  }

  log
}
