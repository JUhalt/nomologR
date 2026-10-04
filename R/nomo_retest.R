# Test-retest reliability ---------------------------------------------------------

#' Test-retest reliability, measurement error, and reliable change
#'
#' `nomo_retest()` estimates how stable a composite's scores are when the same
#' people answer the same measure on two or more occasions, how large a
#' person's measurement error is, and which people changed by more than that
#' error.
#'
#' @details
#' **Which intraclass correlation.** Shrout and Fleiss (1979) and McGraw and
#' Wong (1996) define several intraclass correlations, which differ in their
#' model, type, and definition. For test-retest reliability, Koo and Li (2016)
#' recommend a two-way mixed-effects model with absolute agreement: the
#' occasions are not a random sample of occasions, and scores that do not agree
#' across occasions are not reliable however well they rank people. That is
#' ICC(A,1) in McGraw and Wong's notation, for a single measurement. The
#' consistency form, ICC(C,1), ignores a shift in the mean between occasions and
#' is reported beside it, so the gap between them shows how much of the
#' disagreement is a systematic change. The mean change from the first to the
#' last occasion, with its interval, is reported too.
#'
#' **Reading the interval.** Koo and Li (2016) describe reliability as poor
#' below .50, moderate from .50 to .75, good from .75 to .90, and excellent
#' above .90, and ask that the description come from the 95% confidence
#' interval rather than the estimate. `koo_li` gives the description of the
#' interval, such as "good to excellent". These are reference values, not a pass
#' or a fail, and a reliability depends on the interval between occasions and on
#' whether the construct itself changed.
#'
#' **Measurement error and change.** The standard error of measurement is
#' \eqn{\sqrt{MS_E}}{sqrt(MS_E)}, the square root of the residual mean square
#' of the two-way model (Weir, 2005): how far a person's scores spread across
#' occasions once the shift in the mean between occasions is removed. It does
#' not depend on which ICC is chosen. When the occasions do not differ in
#' mean, it is close to \eqn{SD \sqrt{1 - ICC}}{SD * sqrt(1 - ICC)}
#' (Nunnally & Bernstein, 1994), with ICC(A,1) and the standard deviation
#' pooled over occasions; when they do, that form counts the shift as
#' measurement error and overstates it. The smallest detectable change is
#' \eqn{1.96 \sqrt{2} \, SEM}{1.96 * sqrt(2) * SEM}: a change in a person's
#' score smaller than that is within measurement error at 95% (Weir, 2005).
#' Jacobson and Truax's (1991) reliable change index divides a person's change
#' by \eqn{\sqrt{2} \, SEM}{sqrt(2) * SEM}, so it exceeds 1.96 exactly when
#' the change exceeds the smallest detectable change. The change compared
#' includes any shift in the mean, so a shift that everyone shares can make
#' many changes reliable. A reliable change is not necessarily a meaningful
#' one.
#'
#' @param data A data frame with one row per person.
#' @param scores The columns holding the same composite on successive
#'   occasions, in order: a character vector of two or more column names, or a
#'   named list of such vectors, one per composite.
#' @param interval Optional text describing the time between occasions, such as
#'   `"two weeks"`. It is recorded with the results, because a test-retest
#'   reliability depends on it.
#'
#' @return A `nomo_retest` object. The fields to read are:
#'
#'   * `icc`: one row per composite, with the number of complete cases and of
#'     occasions, ICC(A,1) with its 95% interval and Koo and Li's description
#'     of that interval (`koo_li`), ICC(C,1) with its interval, the mean change
#'     from the first to the last occasion with its interval, the pooled
#'     standard deviation (`sd`, the standard deviation of the scores pooled
#'     over occasions, not of the change), the standard error of measurement
#'     (`sem`, the square root of the residual mean square), and the smallest
#'     detectable change (`sdc`).
#'   * `reliable_change`: one row per person and composite, with the first and
#'     last scores, the change, the reliable change index (`rci`), and whether
#'     the change is a reliable increase, a reliable decrease, or neither. A
#'     person whose score did not change has an `rci` of 0, even when there is
#'     no measurement error at all.
#'   * `interval` and `decision_log`.
#'
#'   A case without a score on every occasion of a composite is left out of
#'   that composite, and the decision log says how many were. Scores must be
#'   finite numbers or `NA`.
#'
#'   Other fields record the call and the columns used (`scores`). They may
#'   change between releases and are not part of the stable interface (see
#'   `?nomologR`).
#'
#'   `print()` shows each composite's ICC(A,1) with its interval and Koo and
#'   Li's description, the measurement error, and how many people changed
#'   reliably; `summary()` adds the consistency form ICC(C,1) and the mean
#'   change with its interval, and the full reason for each flag.
#'
#' @references
#' Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical
#' approach to defining meaningful change in psychotherapy research. *Journal
#' of Consulting and Clinical Psychology, 59*(1), 12-19.
#' \doi{10.1037/0022-006X.59.1.12}
#'
#' Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting
#' intraclass correlation coefficients for reliability research. *Journal of
#' Chiropractic Medicine, 15*(2), 155-163. \doi{10.1016/j.jcm.2016.02.012}
#'
#' McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some intraclass
#' correlation coefficients. *Psychological Methods, 1*(1), 30-46.
#' \doi{10.1037/1082-989X.1.1.30}
#'
#' Nunnally, J. C., & Bernstein, I. H. (1994). *Psychometric theory* (3rd ed.).
#' McGraw-Hill.
#'
#' Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in
#' assessing rater reliability. *Psychological Bulletin, 86*(2), 420-428.
#' \doi{10.1037/0033-2909.86.2.420}
#'
#' Weir, J. P. (2005). Quantifying test-retest reliability using the intraclass
#' correlation coefficient and the SEM. *Journal of Strength and Conditioning
#' Research, 19*(1), 231-240. \doi{10.1519/15184.1}
#'
#' @examples
#' # Agency scores on two occasions, two weeks apart (simulated).
#' set.seed(2026)
#' true <- stats::rnorm(150)
#' panel <- data.frame(
#'   agency_t1 = 3 + true + stats::rnorm(150, sd = .45),
#'   agency_t2 = 3.1 + true + stats::rnorm(150, sd = .45)
#' )
#' rt <- nomo_retest(panel, scores = c("agency_t1", "agency_t2"),
#'                   interval = "two weeks")
#' rt
#' nomo_table(rt, "reliable_change")
#' @export
nomo_retest <- function(data, scores, interval = NULL) {
  if (!is.data.frame(data) || !nrow(data)) {
    stop("`data` must be a non-empty data frame.", call. = FALSE)
  }
  if (!is.null(interval) &&
        (!is.character(interval) || length(interval) != 1L || is.na(interval) ||
           !nzchar(trimws(interval)))) {
    stop("`interval` must be NULL or one non-empty character string.", call. = FALSE)
  }

  sets <- nomo_retest_sets(scores, data)
  parts <- lapply(names(sets), function(label) {
    nomo_retest_one(data, sets[[label]], label)
  })
  icc <- dplyr::bind_rows(lapply(parts, `[[`, "icc"))
  reliable_change <- dplyr::bind_rows(lapply(parts, `[[`, "reliable_change"))

  out <- list(
    call = match.call(),
    scores = sets,
    n_cases = nrow(data),
    interval = if (is.null(interval)) NA_character_ else trimws(interval),
    icc = icc,
    reliable_change = reliable_change,
    decision_log = nomo_retest_log(icc, reliable_change, interval, nrow(data))
  )
  class(out) <- c("nomo_retest", "list")
  out
}


# The composites and their occasion columns, checked against the data.
nomo_retest_sets <- function(scores, data) {
  # A plain vector is one composite; its errors name the argument itself.
  single <- is.character(scores)
  if (single) scores <- list(composite = scores)
  labels <- names(scores)
  if (!is.list(scores) || !length(scores) || is.null(labels) ||
        any(is.na(labels) | !nzchar(labels)) || anyDuplicated(labels)) {
    stop(
      paste(
        "`scores` must be a character vector of column names, or a named",
        "list of them with one entry per composite."
      ),
      call. = FALSE
    )
  }
  for (label in labels) {
    cols <- scores[[label]]
    if (!is.character(cols) || length(cols) < 2L || anyNA(cols) ||
          anyDuplicated(cols)) {
      stop(
        sprintf(
          "`%s` needs two or more distinct columns, one per occasion.",
          if (single) "scores" else label
        ),
        call. = FALSE
      )
    }
    absent <- setdiff(cols, names(data))
    if (length(absent)) {
      stop(
        sprintf(
          "`scores` column%s not in `data`: %s.",
          if (length(absent) == 1L) "" else "s", paste(absent, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    numeric <- vapply(data[cols], is.numeric, logical(1))
    if (!all(numeric)) {
      stop(
        sprintf(
          "`scores` columns must be numeric. Not numeric: %s.",
          paste(cols[!numeric], collapse = ", ")
        ),
        call. = FALSE
      )
    }
    # An infinite score is not missing, so it would reach the ICC and fail
    # there with a message that names no column (#145).
    infinite <- vapply(data[cols], function(v) any(is.infinite(v)), logical(1))
    if (any(infinite)) {
      stop(
        sprintf(
          "`scores` columns must hold finite numbers or NA. Infinite values in: %s.",
          paste(cols[infinite], collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  scores
}


# One composite: the two intraclass correlations, the change between the first
# and last occasions, measurement error, and each person's reliable change.
nomo_retest_one <- function(data, cols, label) {
  x <- data[cols]
  complete <- stats::complete.cases(x)
  x <- x[complete, , drop = FALSE]
  n <- nrow(x)
  if (n < 3L) {
    stop(
      sprintf(
        "`%s` has %d complete case%s across its occasions; at least 3 are needed.",
        label, n, if (n == 1L) "" else "s"
      ),
      call. = FALSE
    )
  }
  if (all(vapply(x, function(v) stats::var(v) == 0, logical(1)))) {
    stop(
      sprintf("`%s` does not vary, so its reliability cannot be estimated.", label),
      call. = FALSE
    )
  }

  fit <- suppressWarnings(suppressMessages(
    psych::ICC(as.matrix(x), lmer = FALSE)
  ))
  results <- fit$results
  agreement <- results[results$type == "ICC2", , drop = FALSE]
  consistency <- results[results$type == "ICC3", , drop = FALSE]
  icc_a <- agreement$ICC[[1L]]

  sd_pooled <- sqrt(mean(vapply(x, stats::var, numeric(1))))
  # The standard error of measurement is the square root of the residual mean
  # square of the two-way model: each person's spread across occasions once
  # the shift in the mean between occasions is removed. Pairing ICC(A,1), which
  # counts that shift as error, with the within-occasion SD inflated the SEM,
  # the SDC, and the RCI's denominator whenever scores shifted (#145).
  m <- as.matrix(x)
  residual <- sweep(sweep(m, 1L, rowMeans(m)), 2L, colMeans(m)) + mean(m)
  sem <- sqrt(sum(residual^2) / ((n - 1L) * (length(cols) - 1L)))
  critical <- stats::qnorm(0.975)
  sdc <- critical * sqrt(2) * sem

  first <- x[[1L]]
  last <- x[[length(cols)]]
  change <- last - first
  mean_change <- mean(change)
  half_width <- stats::qt(0.975, n - 1L) * stats::sd(change) / sqrt(n)

  rci <- change / (sqrt(2) * sem)
  # With no random error at all, a person whose score did not change has not
  # changed reliably.
  rci[change == 0] <- 0
  status <- ifelse(
    rci > critical, "reliable_increase",
    ifelse(rci < -critical, "reliable_decrease", "no_reliable_change")
  )

  list(
    icc = tibble::tibble(
      composite = label,
      first = cols[[1L]],
      last = cols[[length(cols)]],
      n_occasions = length(cols),
      n = n,
      icc_agreement = icc_a,
      agreement_ci_lower = agreement$`lower bound`[[1L]],
      agreement_ci_upper = agreement$`upper bound`[[1L]],
      koo_li = nomo_retest_koo_li(
        agreement$`lower bound`[[1L]], agreement$`upper bound`[[1L]]
      ),
      icc_consistency = consistency$ICC[[1L]],
      consistency_ci_lower = consistency$`lower bound`[[1L]],
      consistency_ci_upper = consistency$`upper bound`[[1L]],
      mean_change = mean_change,
      change_ci_lower = mean_change - half_width,
      change_ci_upper = mean_change + half_width,
      sd = sd_pooled,
      sem = sem,
      sdc = sdc
    ),
    reliable_change = tibble::tibble(
      composite = label,
      row = which(complete),
      first_score = first,
      last_score = last,
      change = change,
      rci = rci,
      status = status
    )
  )
}


# Koo and Li's (2016) description of an interval: poor below .50, moderate to
# .75, good to .90, excellent above .90, from its lower to its upper bound.
nomo_retest_koo_li <- function(lower, upper) {
  describe <- function(v) {
    if (v < 0.50) "poor" else if (v < 0.75) "moderate" else if (v <= 0.90) "good" else "excellent"
  }
  if (!is.finite(lower) || !is.finite(upper)) return(NA_character_)
  from <- describe(lower)
  to <- describe(upper)
  if (identical(from, to)) from else paste(from, "to", to)
}


nomo_retest_log <- function(icc, reliable_change, interval, n_cases = NA_integer_) {
  log <- nomo_log_new()
  # Each quantity prints as its kind does (#144): an ICC as a reliability,
  # score-unit quantities as estimates.
  icc_text <- function(x) nomo_present_stat(x, "reliability")
  score <- function(x) nomo_present_stat(x, "estimate")

  if (is.null(interval)) {
    log <- nomo_log_add(
      log, stage = "retest", object = "occasions",
      metric = "retest_interval",
      severity = "info",
      observation = "The time between occasions was not recorded.",
      recommendation = paste(
        "Record it: a test-retest reliability describes stability over that",
        "interval, and a longer one leaves more room for real change."
      )
    )
  }

  for (k in seq_len(nrow(icc))) {
    row <- icc[k, , drop = FALSE]
    label <- row$composite[[1L]]

    # Listwise deletion is recorded, not silent (#145).
    dropped <- n_cases - row$n[[1L]]
    if (is.finite(dropped) && dropped > 0L) {
      log <- nomo_log_add(
        log, stage = "retest", object = label,
        metric = "incomplete_cases",
        value = dropped,
        reference = "Listwise deletion across the composite's occasions",
        severity = "info",
        observation = sprintf(
          "`%s`: %d of %d cases were left out because they lack a score on at least one occasion; n = %d.",
          label, dropped, n_cases, row$n[[1L]]
        ),
        recommendation = paste(
          "Report the number of cases used. If the missing scores are not",
          "missing completely at random, the reliability describes the cases",
          "that remained."
        )
      )
    }

    poor_possible <- is.finite(row$agreement_ci_lower) && row$agreement_ci_lower < 0.50
    log <- nomo_log_add(
      log, stage = "retest", object = label,
      metric = "icc_agreement",
      value = row$icc_agreement[[1L]],
      reference = paste(
        "Koo & Li (2016): poor < .50, moderate .50-.75, good .75-.90,",
        "excellent > .90, read from the 95% interval"
      ),
      severity = if (poor_possible) "review" else "info",
      observation = sprintf(
        paste(
          "`%s`: ICC(A,1) = %s, 95%% CI %s, which Koo and Li (2016)",
          "would describe as %s (two-way mixed effects, absolute agreement,",
          "single measurement; n = %d, %d occasions)."
        ),
        label, icc_text(row$icc_agreement),
        nomo_present_ci(row$agreement_ci_lower, row$agreement_ci_upper, kind = "reliability"),
        row$koo_li[[1L]], row$n[[1L]], row$n_occasions[[1L]]
      ),
      recommendation = paste(
        if (poor_possible) {
          "The interval reaches the range described as poor."
        } else {
          ""
        },
        "Report the model, type, and definition with the estimate and its",
        "interval (Koo & Li, 2016)."
      )
    )

    shifted <- is.finite(row$change_ci_lower) &&
      (row$change_ci_lower > 0 || row$change_ci_upper < 0)
    log <- nomo_log_add(
      log, stage = "retest", object = label,
      metric = "mean_change",
      value = row$mean_change[[1L]],
      reference = "Systematic change between occasions (Weir, 2005)",
      severity = if (shifted) "review" else "info",
      observation = sprintf(
        "`%s` changed by %s on average from `%s` to `%s`, 95%% CI %s.",
        label, nomo_present_stat(row$mean_change, "estimate", signed = TRUE),
        row$first[[1L]], row$last[[1L]],
        nomo_present_ci(row$change_ci_lower, row$change_ci_upper, kind = "estimate")
      ),
      recommendation = if (shifted) {
        paste(
          "Scores shifted systematically, as practice or real change would",
          "make them. ICC(A,1) counts the shift as disagreement and ICC(C,1)",
          "does not; ICC(C,1) is", icc_text(row$icc_consistency), "here.",
          "The SEM leaves the shift out, so the shift counts toward each",
          "person's change when reliable change is classified."
        )
      } else {
        "The interval includes no change in the mean between occasions."
      }
    )

    rc <- reliable_change[reliable_change$composite == label, , drop = FALSE]
    log <- nomo_log_add(
      log, stage = "retest", object = label,
      metric = "reliable_change",
      value = row$sdc[[1L]],
      reference = "Jacobson & Truax (1991); Weir (2005)",
      severity = "info",
      observation = sprintf(
        paste(
          "The standard error of measurement of `%s` is %s, so a change",
          "smaller than %s is within measurement error. %d of %d people",
          "changed reliably: %d up and %d down."
        ),
        label, score(row$sem), score(row$sdc),
        sum(rc$status != "no_reliable_change"), nrow(rc),
        sum(rc$status == "reliable_increase"), sum(rc$status == "reliable_decrease")
      ),
      recommendation = paste(
        "A reliable change is larger than measurement error; whether it is",
        "meaningful is a separate question."
      )
    )
  }
  log
}


#' @export
print.nomo_retest <- function(x, ...) {
  nomo_retest_present_header(x, summary = FALSE)
  nomo_retest_present_table(x$icc)
  nomo_retest_present_change(x$reliable_change)
  nomo_present_flagged(x$decision_log)
  nomo_retest_present_key(summary = FALSE)
  nomo_retest_present_note()
  nomo_present_pointer(
    c("summary(x)", "nomo_table(x, \"reliable_change\")"),
    c("the consistency form and the mean change", "each person's change")
  )
  invisible(x)
}


#' @export
summary.nomo_retest <- function(object, ...) {
  out <- list(
    icc = object$icc,
    reliable_change = object$reliable_change,
    interval = object$interval,
    n_cases = object$n_cases,
    decision_log = object$decision_log
  )
  class(out) <- c("summary_nomo_retest", "list")
  out
}


#' @export
print.summary_nomo_retest <- function(x, ...) {
  nomo_retest_present_header(x, summary = TRUE)
  nomo_retest_present_table(x$icc)

  show <- x$icc
  show$consistency_ci <- nomo_present_ci(show$consistency_ci_lower, show$consistency_ci_upper,
                                         kind = "reliability")
  show$change_ci <- nomo_present_ci(show$change_ci_lower, show$change_ci_upper,
                                    kind = "estimate")
  nomo_present_section("Consistency and change")
  nomo_present_table(
    show,
    c("Composite" = "composite", "ICC(C,1)" = "icc_consistency", "95% CI" = "consistency_ci",
      "Mean change" = "mean_change", "95% CI" = "change_ci"),
    formats = list(
      icc_consistency = function(v) nomo_present_stat(v, "reliability"),
      mean_change = function(v) nomo_present_stat(v, "estimate", signed = TRUE)
    ),
    more = "nomo_table(x, \"icc\")"
  )

  nomo_retest_present_change(x$reliable_change)
  nomo_present_flagged(x$decision_log, recommendation = TRUE)
  nomo_retest_present_key(summary = TRUE)
  nomo_retest_present_note()
  nomo_present_pointer("nomo_table(x, \"reliable_change\")", "each person's change")
  invisible(x)
}


nomo_retest_present_header <- function(x, summary) {
  nomo_present_header("nomo_retest", "Test-retest reliability", summary = summary,
                      source = "McGraw & Wong (1996); Koo & Li (2016)")
  nomo_present_facts(c(
    sprintf("Composites: %d", nrow(x$icc)),
    # Absent from an object made before the count was recorded.
    if (length(x$n_cases)) sprintf("Cases: %d", x$n_cases) else "",
    if (is.na(x$interval)) "Interval: not recorded" else paste0("Interval: ", x$interval)
  ))
  # Cases without a score on every occasion are left out, and the print says
  # how many (#145).
  dropped <- if (length(x$n_cases)) x$n_cases - x$icc$n else integer()
  if (any(dropped > 0L)) {
    nomo_present_text(
      "Cases without a score on every occasion are left out: ",
      nomo_present_or(sprintf(
        "%d of %d for %s", dropped[dropped > 0L], x$n_cases,
        x$icc$composite[dropped > 0L]
      ), "and"),
      "."
    )
  }
}


nomo_retest_present_table <- function(icc) {
  show <- icc
  show$agreement_ci <- nomo_present_ci(show$agreement_ci_lower, show$agreement_ci_upper,
                                       kind = "reliability")
  score <- function(v) nomo_present_stat(v, "estimate")
  nomo_present_section("Reliability across occasions")
  # The pooled SD sits beside the SEM it is compared with; it is the scores'
  # spread, not the change scores' (#145).
  nomo_present_table(
    show,
    c("Composite" = "composite", "n" = "n", "ICC(A,1)" = "icc_agreement",
      "95% CI" = "agreement_ci", "Koo and Li" = "koo_li", "Pooled SD" = "sd",
      "SEM" = "sem", "SDC" = "sdc"),
    formats = list(icc_agreement = function(v) nomo_present_stat(v, "reliability"),
                   sd = score, sem = score, sdc = score),
    more = "nomo_table(x, \"icc\")"
  )
}


nomo_retest_present_change <- function(reliable_change) {
  composites <- unique(reliable_change$composite)
  nomo_present_section("Reliable change, first to last occasion")
  nomo_present_bullets(vapply(composites, function(label) {
    rc <- reliable_change[reliable_change$composite == label, , drop = FALSE]
    sprintf(
      "%s: %d up, %d down, and %d within measurement error, of %s.",
      label,
      sum(rc$status == "reliable_increase"),
      sum(rc$status == "reliable_decrease"),
      sum(rc$status == "no_reliable_change"),
      nomo_present_count(nrow(rc), "person", "people")
    )
  }, character(1)))
}


nomo_retest_present_key <- function(summary = FALSE) {
  key <- c(
    n = "Cases with a score on every occasion",
    "ICC(A,1)" = "Intraclass correlation from a two-way mixed-effects model, absolute agreement, single measurement",
    "ICC(C,1)" = "The same with consistency, which ignores a shift in the mean between occasions",
    CI = "Confidence interval",
    "Koo and Li" = "Their description of the ICC(A,1) interval: poor below .50, moderate to .75, good to .90, excellent above",
    "Pooled SD" = "Standard deviation of the scores pooled over occasions, not of the change",
    SEM = "Standard error of measurement, the square root of the residual mean square (Weir, 2005)",
    SDC = "Smallest detectable change, 1.96 x sqrt(2) x SEM (Weir, 2005)"
  )
  if (!isTRUE(summary)) key <- key[names(key) != "ICC(C,1)"]
  nomo_present_key(key)
}


nomo_retest_present_note <- function() {
  cat("\n")
  nomo_present_text(
    "Reference ranges describe the interval; they are not a pass or a fail. ",
    "A reliable change is larger than measurement error, which does not make ",
    "it a meaningful one."
  )
}
