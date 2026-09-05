#' Build simple confirmatory factor-analysis syntax
#'
#' `nomo_model()` is a small convenience helper for ordinary reflective CFA
#' models. It converts a named list of factor-to-indicator assignments into
#' `lavaan` measurement-model syntax. It deliberately does not add residual
#' covariances, cross-loadings, equality constraints, or other post-hoc changes.
#'
#' @param factors A named list. Each element name is a latent-factor name and
#'   each element value is a character vector of observed indicators.
#'
#' @return A character scalar of class `nomo_model` that can be passed directly
#'   to [nomo_cfa()] or to `lavaan::cfa()`.
#' @export
#'
#' @examples
#' model <- nomo_model(list(
#'   engagement = c("e1", "e2", "e3"),
#'   belonging = c("b1", "b2", "b3")
#' ))
#' model
nomo_model <- function(factors) {
  if (!is.list(factors) || !length(factors)) {
    stop("`factors` must be a non-empty named list.", call. = FALSE)
  }

  factor_names <- names(factors)
  if (is.null(factor_names) || anyNA(factor_names)) {
    stop("`factors` must have unique, non-empty factor names.", call. = FALSE)
  }
  factor_names <- trimws(factor_names)
  if (any(!nzchar(factor_names)) || anyDuplicated(factor_names)) {
    stop("`factors` must have unique, non-empty factor names.", call. = FALSE)
  }

  cleaned <- lapply(factors, function(x) {
    if (!is.character(x) || !length(x) || anyNA(x)) {
      stop(
        "Each factor must contain one or more non-missing indicator names.",
        call. = FALSE
      )
    }
    x <- trimws(x)
    if (any(!nzchar(x))) {
      stop(
        "Each factor must contain one or more non-missing indicator names.",
        call. = FALSE
      )
    }
    if (anyDuplicated(x)) {
      stop("Indicator names may not be duplicated within a factor.", call. = FALSE)
    }
    x
  })

  syntax <- vapply(
    seq_along(cleaned),
    function(i) {
      paste0(
        factor_names[[i]],
        " =~ ",
        paste(cleaned[[i]], collapse = " + ")
      )
    },
    character(1)
  )

  out <- paste(syntax, collapse = "\n")
  attr(out, "factors") <- stats::setNames(cleaned, factor_names)
  class(out) <- c("nomo_model", "character")
  out
}


#' @export
print.nomo_model <- function(x, ...) {
  cat(as.character(x), "\n", sep = "")
  invisible(x)
}
