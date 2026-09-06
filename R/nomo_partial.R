# Researcher-specified partial invariance --------------------------------------

#' Specify researcher-controlled partial-invariance releases
#'
#' `nomo_partial()` records equality constraints that the researcher has decided
#' to release at a specific invariance level. It does not inspect modification
#' indices, search for a better-fitting model, or choose releases automatically.
#'
#' Releases are written using ordinary lavaan parameter syntax accepted by the
#' `group.partial` argument of `semTools::measEq.syntax()`, for example
#' `"F =~ x2"`, `"x3 ~ 1"`, or `"u2 | t1"`.
#'
#' A release first requested at one level is carried forward to later,
#' more-restrictive levels so that a loading freed at the metric level does not
#' silently become constrained again at the scalar level.
#'
#' @param level Character vector naming the first invariance level at which each
#'   release should apply. Supported labels are `thresholds`, `metric`, `scalar`,
#'   `strong`, and `strict`; the applicable subset depends on the indicator
#'   category structure.
#' @param syntax Character vector of lavaan parameter expressions to exclude from
#'   the corresponding equality constraints.
#' @param rationale Character vector documenting why each release was chosen.
#'   A single rationale may be recycled across multiple releases.
#'
#' @return A `nomo_partial` object with a report-ready release table.
#' @export
#'
#' @examples
#' partial <- nomo_partial(
#'   level = c("metric", "scalar"),
#'   syntax = c("F =~ x2", "x3 ~ 1"),
#'   rationale = c(
#'     "Loading difference was theoretically anticipated.",
#'     "Intercept difference was prespecified from prior evidence."
#'   )
#' )
#' partial
nomo_partial <- function(level, syntax, rationale) {
  if (!is.character(level) || !length(level) || anyNA(level)) {
    stop("`level` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.character(syntax) || !length(syntax) || anyNA(syntax)) {
    stop("`syntax` must be a non-empty character vector.", call. = FALSE)
  }
  if (!is.character(rationale) || !length(rationale) || anyNA(rationale)) {
    stop("`rationale` must be a non-empty character vector.", call. = FALSE)
  }

  level <- tolower(trimws(level))
  syntax <- trimws(syntax)
  rationale <- trimws(rationale)

  if (any(!nzchar(level)) || any(!nzchar(syntax)) || any(!nzchar(rationale))) {
    stop("`level`, `syntax`, and `rationale` may not contain blank values.", call. = FALSE)
  }

  allowed <- c("thresholds", "metric", "scalar", "strong", "strict")
  bad <- setdiff(unique(level), allowed)
  if (length(bad)) {
    stop(
      sprintf(
        "Unknown partial-invariance level(s): %s.",
        paste(bad, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  n <- max(length(level), length(syntax))
  if (!length(level) %in% c(1L, n) || !length(syntax) %in% c(1L, n)) {
    stop("`level` and `syntax` must have length 1 or a common length.", call. = FALSE)
  }
  if (!length(rationale) %in% c(1L, n)) {
    stop("`rationale` must have length 1 or match the number of releases.", call. = FALSE)
  }

  level <- rep(level, length.out = n)
  syntax <- rep(syntax, length.out = n)
  rationale <- rep(rationale, length.out = n)

  releases <- tibble::tibble(
    release_id = paste0("P", seq_len(n)),
    level = level,
    syntax = syntax,
    rationale = rationale
  )

  key <- paste(releases$level, releases$syntax, sep = "::")
  if (anyDuplicated(key)) {
    stop("The same partial-invariance release was specified more than once.", call. = FALSE)
  }

  out <- list(
    call = match.call(),
    releases = releases,
    n = nrow(releases)
  )
  class(out) <- c("nomo_partial", "list")
  out
}


#' @export
print.nomo_partial <- function(x, ...) {
  cat("<nomo_partial>\n")
  cat(sprintf("%d researcher-specified release(s)\n\n", x$n))
  print(x$releases, n = Inf, width = Inf)
  cat(
    "\nNo release was selected automatically by nomologR.\n",
    sep = ""
  )
  invisible(x)
}
