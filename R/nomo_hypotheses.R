# Theory-specified nomological expectations -----------------------------------

nomo_expectation_scalar <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    stop(sprintf("`%s` must be one finite numeric value.", name), call. = FALSE)
  }
  as.numeric(x)
}


nomo_expectation_new <- function(prediction,
                                 lower,
                                 upper,
                                 lower_inclusive,
                                 upper_inclusive,
                                 scale,
                                 origin,
                                 magnitude_specified,
                                 confirmable) {
  structure(
    list(
      prediction = prediction,
      lower = lower,
      upper = upper,
      lower_inclusive = lower_inclusive,
      upper_inclusive = upper_inclusive,
      scale = scale,
      origin = origin,
      magnitude_specified = magnitude_specified,
      confirmable = confirmable
    ),
    class = c("nomo_expectation", "list")
  )
}


#' Specify directional or negligible theoretical expectations
#'
#' These helpers create machine-readable theoretical expectations for use with
#' [nomo_hypotheses()]. They describe what theory predicts before the empirical
#' network is evaluated.
#'
#' `positive()` and `negative()` can express direction alone or add a
#' researcher-specified magnitude boundary. `negligible()` deliberately
#' distinguishes an unquantified null-like expectation from a quantitative
#' smallest effect size of interest (SESOI) region.
#'
#' A bare `negligible()` expectation is **not confirmable as negligible** from a
#' non-significant p-value. Supply `within = c(lower, upper)` when theory or the
#' study design provides a defensible negligible-effect region.
#'
#' @param min Optional finite lower bound. For `positive()` it must be greater
#'   than zero. For `negative()` it must be less than zero.
#' @param max Optional finite upper bound. For `positive()` it must be greater
#'   than zero. For `negative()` it must be less than zero.
#' @param within Optional length-two finite numeric vector defining the
#'   negligible-effect region. The interval must contain zero.
#' @param scale Scale on which the expectation is defined. Standardized
#'   coefficients are the default because magnitude expectations such as `.20`
#'   are otherwise not portable across arbitrary raw units.
#' @param origin Whether the expectation was specified `a_priori` or added
#'   `post_hoc`. Post-hoc expectations remain machine-readable but are never
#'   presented as confirmatory evidence.
#'
#' @return An object of class `nomo_expectation`.
#' @name nomo_expectations
#'
#' @examples
#' positive()
#' positive(min = .20)
#' negative(max = -.20)
#' negligible()
#' negligible(within = c(-.10, .10))
NULL


#' @rdname nomo_expectations
#' @export
positive <- function(min = NULL,
                     max = NULL,
                     scale = c("standardized", "unstandardized"),
                     origin = c("a_priori", "post_hoc")) {
  scale <- match.arg(scale)
  origin <- match.arg(origin)

  if (!is.null(min)) {
    min <- nomo_expectation_scalar(min, "min")
    if (min <= 0) {
      stop("`min` for `positive()` must be greater than zero.", call. = FALSE)
    }
  }

  if (!is.null(max)) {
    max <- nomo_expectation_scalar(max, "max")
    if (max <= 0) {
      stop("`max` for `positive()` must be greater than zero.", call. = FALSE)
    }
  }

  lower <- if (is.null(min)) 0 else min
  upper <- if (is.null(max)) Inf else max

  if (upper <= lower) {
    stop("For `positive()`, `max` must be greater than `min`.", call. = FALSE)
  }

  nomo_expectation_new(
    prediction = "positive",
    lower = lower,
    upper = upper,
    lower_inclusive = !is.null(min),
    upper_inclusive = !is.null(max),
    scale = scale,
    origin = origin,
    magnitude_specified = !is.null(min) || !is.null(max),
    confirmable = TRUE
  )
}


#' @rdname nomo_expectations
#' @export
negative <- function(max = NULL,
                     min = NULL,
                     scale = c("standardized", "unstandardized"),
                     origin = c("a_priori", "post_hoc")) {
  scale <- match.arg(scale)
  origin <- match.arg(origin)

  if (!is.null(max)) {
    max <- nomo_expectation_scalar(max, "max")
    if (max >= 0) {
      stop("`max` for `negative()` must be less than zero.", call. = FALSE)
    }
  }

  if (!is.null(min)) {
    min <- nomo_expectation_scalar(min, "min")
    if (min >= 0) {
      stop("`min` for `negative()` must be less than zero.", call. = FALSE)
    }
  }

  lower <- if (is.null(min)) -Inf else min
  upper <- if (is.null(max)) 0 else max

  if (upper <= lower) {
    stop("For `negative()`, `max` must be greater than `min`.", call. = FALSE)
  }

  nomo_expectation_new(
    prediction = "negative",
    lower = lower,
    upper = upper,
    lower_inclusive = !is.null(min),
    upper_inclusive = !is.null(max),
    scale = scale,
    origin = origin,
    magnitude_specified = !is.null(min) || !is.null(max),
    confirmable = TRUE
  )
}


#' @rdname nomo_expectations
#' @export
negligible <- function(within = NULL,
                       scale = c("standardized", "unstandardized"),
                       origin = c("a_priori", "post_hoc")) {
  scale <- match.arg(scale)
  origin <- match.arg(origin)

  if (is.null(within)) {
    return(
      nomo_expectation_new(
        prediction = "negligible",
        lower = NA_real_,
        upper = NA_real_,
        lower_inclusive = NA,
        upper_inclusive = NA,
        scale = scale,
        origin = origin,
        magnitude_specified = FALSE,
        confirmable = FALSE
      )
    )
  }

  if (!is.numeric(within) || length(within) != 2L ||
      anyNA(within) || any(!is.finite(within))) {
    stop("`within` must be two finite numeric bounds.", call. = FALSE)
  }

  within <- as.numeric(within)
  if (within[[1L]] >= within[[2L]]) {
    stop("`within` must be ordered from a lower to a higher bound.", call. = FALSE)
  }
  if (within[[1L]] > 0 || within[[2L]] < 0) {
    stop("A negligible-effect region must include zero.", call. = FALSE)
  }

  nomo_expectation_new(
    prediction = "negligible",
    lower = within[[1L]],
    upper = within[[2L]],
    lower_inclusive = TRUE,
    upper_inclusive = TRUE,
    scale = scale,
    origin = origin,
    magnitude_specified = TRUE,
    confirmable = TRUE
  )
}


nomo_parse_relation <- function(label) {
  if (!is.character(label) || length(label) != 1L || is.na(label) ||
      !nzchar(trimws(label))) {
    stop("Each hypothesis name must be one non-empty relation.", call. = FALSE)
  }

  label <- trimws(label)

  if (grepl("<->", label, fixed = TRUE)) {
    pieces <- strsplit(label, "<->", fixed = TRUE)[[1L]]
    operator <- "<->"
    relation_type <- "association"
  } else if (grepl("->", label, fixed = TRUE)) {
    pieces <- strsplit(label, "->", fixed = TRUE)[[1L]]
    operator <- "->"
    relation_type <- "directed"
  } else {
    stop(
      sprintf(
        "Hypothesis `%s` must use `->` for a directed path or `<->` for an association.",
        label
      ),
      call. = FALSE
    )
  }

  pieces <- trimws(pieces)
  if (length(pieces) != 2L || any(!nzchar(pieces))) {
    stop(
      sprintf("Hypothesis `%s` must contain exactly two non-empty nodes.", label),
      call. = FALSE
    )
  }

  if (identical(pieces[[1L]], pieces[[2L]])) {
    stop("A nomological relation must connect two different nodes.", call. = FALSE)
  }

  canonical <- if (identical(operator, "<->")) {
    paste(sort(pieces), collapse = "<->")
  } else {
    paste(pieces, collapse = "->")
  }

  list(
    source = pieces[[1L]],
    target = pieces[[2L]],
    operator = operator,
    relation_type = relation_type,
    canonical = canonical
  )
}


nomo_expectation_region_label <- function(expectation) {
  if (identical(expectation$prediction, "negligible") &&
      !isTRUE(expectation$magnitude_specified)) {
    return("negligible region not specified")
  }

  left <- if (isTRUE(expectation$lower_inclusive)) "[" else "("
  right <- if (isTRUE(expectation$upper_inclusive)) "]" else ")"

  lower <- if (is.infinite(expectation$lower) && expectation$lower < 0) {
    "-Inf"
  } else {
    format(expectation$lower, trim = TRUE, digits = 4)
  }

  upper <- if (is.infinite(expectation$upper) && expectation$upper > 0) {
    "+Inf"
  } else {
    format(expectation$upper, trim = TRUE, digits = 4)
  }

  paste0(left, lower, ", ", upper, right)
}


#' Specify a priori expectations for a nomological network
#'
#' `nomo_hypotheses()` converts named theoretical predictions into a
#' machine-readable object that can later be evaluated by [nomo_network()].
#' Relations use `A -> B` for directed structural paths and `A <-> B` for
#' associations/covariances.
#'
#' The function records theory; it does not inspect data, fit a model, or infer
#' predictions from statistical significance.
#'
#' @param ... Named [nomo_expectations] created by [positive()], [negative()],
#'   or [negligible()]. Names must specify relations using `->` or `<->`.
#'
#' @return A `nomo_hypotheses` object containing a tidy, machine-readable
#'   hypothesis table.
#' @export
#'
#' @examples
#' h <- nomo_hypotheses(
#'   "GSE -> Spirituality" = positive(),
#'   "GSE -> Religiosity" = negligible(within = c(-.10, .10)),
#'   "Religiosity <-> Spirituality" = positive(min = .20)
#' )
#' h
nomo_hypotheses <- function(...) {
  dots <- list(...)

  if (!length(dots)) {
    stop("Supply at least one named theoretical expectation.", call. = FALSE)
  }

  labels <- names(dots)
  if (is.null(labels) || anyNA(labels) || any(!nzchar(trimws(labels)))) {
    stop(
      "Every expectation supplied to `nomo_hypotheses()` must be named with its relation.",
      call. = FALSE
    )
  }

  labels <- trimws(labels)
  if (anyDuplicated(labels)) {
    stop("Hypothesis relation names must be unique.", call. = FALSE)
  }

  bad <- !vapply(dots, inherits, logical(1), what = "nomo_expectation")
  if (any(bad)) {
    stop(
      "Every hypothesis value must be created by `positive()`, `negative()`, or `negligible()`.",
      call. = FALSE
    )
  }

  parsed <- lapply(labels, nomo_parse_relation)
  canonical <- vapply(parsed, `[[`, character(1), "canonical")

  if (anyDuplicated(canonical)) {
    stop(
      paste0(
        "The same logical relation was specified more than once. ",
        "For associations, `A <-> B` and `B <-> A` are the same relation."
      ),
      call. = FALSE
    )
  }

  rows <- lapply(seq_along(dots), function(i) {
    e <- dots[[i]]
    p <- parsed[[i]]

    tibble::tibble(
      id = paste0("H", i),
      relation = labels[[i]],
      source = p$source,
      target = p$target,
      operator = p$operator,
      relation_type = p$relation_type,
      prediction = e$prediction,
      lower = e$lower,
      upper = e$upper,
      lower_inclusive = e$lower_inclusive,
      upper_inclusive = e$upper_inclusive,
      region = nomo_expectation_region_label(e),
      scale = e$scale,
      origin = e$origin,
      magnitude_specified = isTRUE(e$magnitude_specified),
      confirmable = isTRUE(e$confirmable)
    )
  })

  table <- dplyr::bind_rows(rows)

  out <- list(
    call = match.call(),
    hypotheses = table,
    n = nrow(table)
  )
  class(out) <- c("nomo_hypotheses", "list")
  out
}


#' @export
print.nomo_hypotheses <- function(x, ...) {
  cat("<nomo_hypotheses>\n")
  cat(sprintf("%d theory-specified relation(s)\n\n", x$n))

  show <- x$hypotheses[, c(
    "id", "relation", "prediction", "region", "scale", "origin"
  ), drop = FALSE]

  print(show, n = Inf, width = Inf)

  if (any(!x$hypotheses$confirmable)) {
    cat(
      "\nNote: at least one negligible prediction has no quantitative SESOI ",
      "region and cannot be confirmed merely because p > .05.\n",
      sep = ""
    )
  }

  invisible(x)
}


#' @export
summary.nomo_hypotheses <- function(object, ...) {
  out <- list(
    n = object$n,
    a_priori = sum(object$hypotheses$origin == "a_priori"),
    post_hoc = sum(object$hypotheses$origin == "post_hoc"),
    quantitatively_confirmable = sum(object$hypotheses$confirmable),
    hypotheses = object$hypotheses
  )
  class(out) <- c("summary_nomo_hypotheses", "list")
  out
}


#' @export
print.summary_nomo_hypotheses <- function(x, ...) {
  cat("nomologR theory specification\n")
  cat(sprintf("Relations: %d\n", x$n))
  cat(sprintf("A priori: %d | Post hoc: %d\n", x$a_priori, x$post_hoc))
  cat(sprintf(
    "Quantitatively confirmable with the supplied specification: %d/%d\n\n",
    x$quantitatively_confirmable,
    x$n
  ))
  print(x$hypotheses, n = Inf, width = Inf)
  invisible(x)
}
