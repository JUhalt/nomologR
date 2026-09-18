#' Build confirmatory factor-analysis syntax
#'
#' `nomo_model()` is a small convenience helper for reflective CFA models. It
#' converts a named list of factor-to-indicator assignments into `lavaan`
#' measurement-model syntax. It deliberately does not add residual covariances,
#' cross-loadings, equality constraints, or other post-hoc changes.
#'
#' @details
#' Three structures are available, and the researcher chooses among them. The
#' helper never derives a structure from exploratory results.
#'
#' * `"correlated"` (default): one factor per element of `factors`, with
#'   factors free to correlate.
#' * `"higher_order"`: the elements of `factors` become first-order factors,
#'   and a second-order factor named by `general` explains their correlations.
#'   At least three first-order factors are required, because with two the
#'   second-order loadings are not identified without further constraints.
#'   With exactly three, the second-order part is just identified: the model
#'   fits exactly as well as the correlated-factors model, so fit cannot
#'   distinguish the two.
#' * `"bifactor"`: a general factor named by `general` is measured by every
#'   indicator, and each element of `factors` becomes a group factor. All
#'   factors are orthogonal. Identification is written into the syntax (each
#'   factor's first loading is freed and its variance fixed to 1), so the model
#'   is identified the same way whatever `std.lv` is used to fit it. At least
#'   two group factors with at least two indicators each are required;
#'   configurations known to be fragile are noted.
#'
#' Any identification notes are attached as the `"notes"` attribute and printed
#' with the syntax. Use [nomo_hierarchical()] to evaluate a fitted higher-order
#' or bifactor model.
#'
#' @param factors A named list. Each element name is a latent-factor name and
#'   each element value is a character vector of observed indicators. For
#'   hierarchical structures these are the first-order or group factors.
#' @param structure One of `"correlated"`, `"higher_order"`, or `"bifactor"`.
#' @param general Name of the second-order or general factor. Used only when
#'   `structure` is `"higher_order"` or `"bifactor"`.
#'
#' @return A character scalar of class `nomo_model` that can be passed directly
#'   to [nomo_cfa()] or to `lavaan::cfa()`. Attributes record the `factors`,
#'   `structure`, `general` factor, and any identification `notes`.
#'
#' @references
#' Holzinger, K. J., & Swineford, F. (1937). The bi-factor method.
#' *Psychometrika, 2*(1), 41-54. \doi{10.1007/BF02287965}
#'
#' Reise, S. P. (2012). The rediscovery of bifactor measurement models.
#' *Multivariate Behavioral Research, 47*(5), 667-696.
#' \doi{10.1080/00273171.2012.715555}
#'
#' Yung, Y.-F., Thissen, D., & McLeod, L. D. (1999). On the relationship
#' between the higher-order factor model and the hierarchical factor model.
#' *Psychometrika, 64*(2), 113-128. \doi{10.1007/BF02294531}
#' @export
#'
#' @examples
#' factors <- list(
#'   engagement = c("e1", "e2", "e3"),
#'   belonging = c("b1", "b2", "b3"),
#'   efficacy = c("f1", "f2", "f3")
#' )
#'
#' nomo_model(factors)
#' nomo_model(factors, structure = "higher_order", general = "Wellbeing")
#' nomo_model(factors, structure = "bifactor", general = "Wellbeing")
nomo_model <- function(factors,
                       structure = c("correlated", "higher_order", "bifactor"),
                       general = "G") {
  structure <- match.arg(structure)

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
  names(cleaned) <- factor_names

  notes <- character()

  if (identical(structure, "correlated")) {
    syntax <- nomo_model_first_order(cleaned)
  } else {
    general <- nomo_model_validate_general(general, cleaned)

    if (identical(structure, "higher_order")) {
      built <- nomo_model_higher_order(cleaned, general)
    } else {
      built <- nomo_model_bifactor(cleaned, general)
    }
    syntax <- built$syntax
    notes <- built$notes
  }

  out <- paste(syntax, collapse = "\n")
  attr(out, "factors") <- cleaned
  attr(out, "structure") <- structure
  attr(out, "general") <- if (identical(structure, "correlated")) NULL else general
  attr(out, "notes") <- notes
  class(out) <- c("nomo_model", "character")
  out
}


nomo_model_first_order <- function(factors, free_first = FALSE) {
  vapply(
    names(factors),
    function(f) {
      indicators <- factors[[f]]
      if (free_first) indicators[[1L]] <- paste0("NA*", indicators[[1L]])
      paste0(f, " =~ ", paste(indicators, collapse = " + "))
    },
    character(1),
    USE.NAMES = FALSE
  )
}


nomo_model_validate_general <- function(general, factors) {
  if (!is.character(general) || length(general) != 1L || is.na(general) ||
      !nzchar(trimws(general))) {
    stop("`general` must be a single, non-empty factor name.", call. = FALSE)
  }
  general <- trimws(general)
  if (general %in% names(factors)) {
    stop(
      "`general` must differ from the names in `factors`; '", general,
      "' is already a factor.",
      call. = FALSE
    )
  }
  if (general %in% unlist(factors, use.names = FALSE)) {
    stop(
      "`general` must differ from the indicator names; '", general,
      "' is an indicator.",
      call. = FALSE
    )
  }
  general
}


nomo_model_higher_order <- function(factors, general) {
  k <- length(factors)
  if (k < 3L) {
    stop(
      paste(
        "A higher-order model needs at least three first-order factors.",
        sprintf("With %d, the second-order loadings are not identified", k),
        "without further constraints. Fit the correlated-factors model, or",
        "write the constrained syntax yourself if a substantive argument",
        "supports it."
      ),
      call. = FALSE
    )
  }

  items <- unlist(factors, use.names = FALSE)
  if (anyDuplicated(items)) {
    stop(
      "An indicator may belong to only one first-order factor in a higher-order model.",
      call. = FALSE
    )
  }

  syntax <- c(
    nomo_model_first_order(factors),
    paste0(general, " =~ NA*", paste(names(factors), collapse = " + ")),
    paste0(general, " ~~ 1*", general)
  )

  notes <- character()
  if (k == 3L) {
    notes <- paste(
      "With three first-order factors the second-order part is just",
      "identified: this model fits exactly as well as the correlated-factors",
      "model, so model fit cannot distinguish the two."
    )
  }

  list(syntax = syntax, notes = notes)
}


nomo_model_bifactor <- function(factors, general) {
  k <- length(factors)
  if (k < 2L) {
    stop(
      paste(
        "A bifactor model needs at least two group factors. With one, the",
        "group factor cannot be separated from the general factor."
      ),
      call. = FALSE
    )
  }

  too_small <- names(factors)[lengths(factors) < 2L]
  if (length(too_small)) {
    stop(
      "Each group factor needs at least two indicators; ",
      paste(too_small, collapse = ", "),
      " has one. A single-indicator group factor cannot be separated from ",
      "that indicator's residual.",
      call. = FALSE
    )
  }

  items <- unlist(factors, use.names = FALSE)
  if (anyDuplicated(items)) {
    stop(
      "An indicator may belong to only one group factor in a bifactor model.",
      call. = FALSE
    )
  }

  all_factors <- c(general, names(factors))
  general_line <- paste0(
    general, " =~ NA*", paste(items, collapse = " + ")
  )
  variances <- paste0(all_factors, " ~~ 1*", all_factors)

  orthogonal <- character()
  for (i in seq_len(length(all_factors) - 1L)) {
    rest <- all_factors[(i + 1L):length(all_factors)]
    orthogonal <- c(
      orthogonal,
      paste0(all_factors[[i]], " ~~ ", paste0("0*", rest, collapse = " + "))
    )
  }

  syntax <- c(
    general_line,
    nomo_model_first_order(factors, free_first = TRUE),
    variances,
    orthogonal
  )

  notes <- character()
  if (k == 2L) {
    notes <- c(notes, paste(
      "With only two group factors, a bifactor model can be empirically",
      "under-identified or unstable. Inspect lavaan's warnings and the",
      "standard errors before interpreting the solution."
    ))
  }
  two_item <- names(factors)[lengths(factors) == 2L]
  if (length(two_item)) {
    notes <- c(notes, paste0(
      "Group factor(s) with two indicators (",
      paste(two_item, collapse = ", "),
      ") are weakly identified; their loadings can be unstable."
    ))
  }

  list(syntax = syntax, notes = notes)
}


#' @export
print.nomo_model <- function(x, ...) {
  cat(as.character(x), "\n", sep = "")
  notes <- attr(x, "notes")
  if (length(notes)) {
    cat("\nIdentification notes:\n")
    cat(paste0("- ", notes), sep = "\n")
  }
  invisible(x)
}
