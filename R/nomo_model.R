#' Build confirmatory factor-analysis syntax
#'
#' `nomo_model()` is a small convenience helper for reflective CFA models. It
#' converts a named list of factor-to-indicator assignments into `lavaan`
#' measurement-model syntax. It deliberately does not add residual covariances,
#' cross-loadings, equality constraints, or other post hoc changes.
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
#'   two group factors with at least two indicators each are required. Two
#'   accepted configurations are not identified without an added constraint,
#'   which `nomo_model()` does not write: a group factor with two indicators
#'   (its two loadings enter the covariances only through their product), and
#'   two group factors with no more than three indicators each. Both are noted
#'   as concerns: `lavaan` may still report convergence, but the loadings are
#'   arbitrary. Two group factors with more indicators are identified but can
#'   be empirically unstable, which is noted for review.
#'
#' Any identification notes are attached as the `"notes"` attribute and printed
#' with the syntax. Use [nomo_hierarchical()] to evaluate a fitted higher-order
#' or bifactor model.
#'
#' @param factors A named list. Each element name is a latent-factor name and
#'   each element value is a character vector of observed indicators. For
#'   hierarchical structures these are the first-order or group factors.
#'   Factor and indicator names must be names `lavaan` can read: letters,
#'   digits, `.`, and `_`, starting with a letter, as [make.names()] leaves
#'   them. A name such as `"self-efficacy"` is refused, because `lavaan` would
#'   fit it as a factor called `efficacy`.
#' @param structure One of `"correlated"`, `"higher_order"`, or `"bifactor"`.
#' @param general Name of the second-order or general factor. Used only when
#'   `structure` is `"higher_order"` or `"bifactor"`.
#'
#' @return A character scalar of class `nomo_model` that can be passed directly
#'   to [nomo_cfa()] or to `lavaan::cfa()`. Attributes record the `factors`,
#'   `structure`, `general` factor, and any identification `notes`, a character
#'   vector named by severity (`"review"` or `"concern"`). `print()` shows the
#'   syntax as it can be copied, then any identification notes.
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
  structure <- nomo_match_arg(structure)

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
  nomo_model_check_names(factor_names, "Factor")
  nomo_model_check_names(unlist(cleaned, use.names = FALSE), "Indicator")

  notes <- character()

  if (identical(structure, "correlated")) {
    syntax <- nomo_model_first_order(cleaned)
  } else {
    general <- nomo_model_validate_general(general, cleaned)
    nomo_model_check_names(general, "The `general` factor")

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


# lavaan reads a name only when it is a syntactic R name: "self-efficacy" is
# fitted as a factor called "efficacy", "1st" as "st", and "my factor" with a
# deprecation warning, so such a name is refused before anything is fitted
# (#145). make.names() leaves exactly the names lavaan accepts unchanged.
nomo_model_check_names <- function(names, what) {
  bad <- unique(names[names != make.names(names)])
  if (!length(bad)) return(invisible(TRUE))
  stop(
    sprintf(
      paste(
        "%s %s cannot be read by lavaan: %s. Use letters, digits, `.`, and `_`,",
        "starting with a letter, for example %s."
      ),
      what, nomo_present_noun(length(bad), "name", "names"),
      paste(sprintf('"%s"', bad), collapse = ", "),
      paste(sprintf('"%s"', make.names(bad)), collapse = ", ")
    ),
    call. = FALSE
  )
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
    notes <- c(review = paste(
      "With three first-order factors the second-order part is just",
      "identified: this model fits exactly as well as the correlated-factors",
      "model, so model fit cannot distinguish the two."
    ))
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

  # Notes are named by severity (#145). Two configurations are accepted but not
  # identified: an orthogonal group factor with two indicators enters the
  # covariances only through the product of its two loadings, and with two
  # group factors of three indicators each one combination of loadings cannot
  # be recovered, although the model has positive degrees of freedom. No
  # constraint is added; the researcher decides.
  notes <- character()
  if (k == 2L && all(lengths(factors) <= 3L)) {
    notes <- c(notes, concern = paste(
      "With two group factors of no more than three indicators each, the",
      "bifactor model is not identified without an added constraint, although",
      "its degrees of freedom are positive: one combination of its loadings",
      "cannot be determined from the covariances. lavaan may still report",
      "convergence, but the loadings are arbitrary and their standard errors",
      "cannot be computed."
    ))
  } else if (k == 2L) {
    notes <- c(notes, review = paste(
      "With only two group factors, a bifactor model can be empirically",
      "under-identified or unstable. Inspect lavaan's warnings and the",
      "standard errors before interpreting the solution."
    ))
  }
  two_item <- names(factors)[lengths(factors) == 2L]
  if (length(two_item)) {
    notes <- c(notes, concern = paste0(
      "Group ", nomo_present_noun(length(two_item), "factor ", "factors "),
      paste(two_item, collapse = ", "), " ",
      nomo_present_noun(length(two_item), "has two indicators", "have two indicators each"),
      ", so the model is not identified without an added constraint, such as",
      " equal loadings: a two-indicator group factor's loadings enter the",
      " covariances between items only through their product."
    ))
  }

  list(syntax = syntax, notes = notes)
}


#' @export
print.nomo_model <- function(x, ...) {
  # The syntax is printed bare, so it can be copied as it is; the notes are
  # wrapped like every other note (#145).
  nomo_present_header("nomo_model", "Measurement model syntax")
  cat(as.character(x), "\n", sep = "")
  notes <- attr(x, "notes")
  if (length(notes)) {
    # A note is prefixed with its severity, as other notes in the package are.
    nomo_present_section("Identification notes")
    nomo_present_notes(data.frame(
      severity = if (is.null(names(notes))) rep("", length(notes)) else names(notes),
      note = unname(notes), stringsAsFactors = FALSE
    ))
  }
  nomo_present_pointer("nomo_cfa(x, data)", "a guided fit of this model")
  invisible(x)
}
