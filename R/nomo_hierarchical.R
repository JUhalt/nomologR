# Hierarchical measurement evidence: bifactor and higher-order models ----------

#' Evaluate general-factor strength in bifactor and higher-order models
#'
#' `nomo_hierarchical()` answers the question behind many published scales that
#' report both a total score and subscale scores: how much of each score
#' reflects a general factor, and how much reliable variance does each subscale
#' carry beyond it? It evaluates a fitted bifactor or higher-order CFA and
#' reports omega total, omega hierarchical, explained common variance (ECV),
#' the percentage of uncontaminated correlations (PUC), and subscale omega and
#' omega hierarchical subscale, each with its estimand stated.
#'
#' @details
#' **Structure.** The structure is read from the fitted model, so syntax written
#' by hand works as well as syntax from [nomo_model()]:
#'
#' * *Higher-order:* one second-order factor measured by first-order factors,
#'   each measured by its own items. The first-order factors' disturbances are
#'   the group sources. Their loadings are the Schmid-Leiman decomposition of
#'   the higher-order solution, which is exact for a confirmatory model.
#' * *Bifactor:* one general factor measured by every item, plus group factors
#'   measured by disjoint subsets of items. Items may load on the general factor
#'   alone.
#'
#' The general and group sources must be orthogonal, because variance can be
#' attributed to a source only when sources do not covary. A model that allows
#' them to correlate is refused with an explanation.
#'
#' **Estimands.** Every omega is the proportion of the variance of a
#' unit-weighted composite that a set of sources explains. With continuous
#' indicators the composite is the observed sum score. With ordered indicators
#' the indices describe the *latent-response* composite, an upper bound on the
#' reliability of the observed ordinal sum score; the observed ordinal-scale
#' version is not provided. ECV and PUC describe the item set and do not depend
#' on the denominator.
#'
#' **No verdicts.** The indices are reported with plain-language
#' interpretation, but no value is treated as a pass/fail threshold. Reise
#' (2012) notes that no benchmark value of ECV establishes when a general factor
#' is strong enough, and that when PUC is very high, even modest ECV can yield
#' relatively unbiased estimates from a unidimensional model.
#'
#' **Choosing a structure.** A bifactor model will usually fit at least as well
#' as a correlated-factors or higher-order model of the same items, including
#' when it is not the model that generated the data (Reise, 2012), and a
#' higher-order model is a constrained version of the bifactor model (Yung,
#' Thissen, & McLeod, 1999). Compare the alternatives with [nomo_compare()] and
#' choose on substantive grounds, not on fit alone. `nomo_hierarchical()` does
#' not choose.
#'
#' @param fit A `nomo_cfa` object or fitted `lavaan` model containing a
#'   bifactor or higher-order measurement model. Single-group, single-level
#'   models only.
#' @param general Optional name of the general (bifactor) or second-order
#'   (higher-order) factor. If `NULL`, it is identified from the model and the
#'   call is refused if that is ambiguous.
#' @param obs.var Logical. `TRUE` (default, matching [nomo_reliability()]) uses
#'   observed covariances in each omega's denominator; `FALSE` uses
#'   model-implied covariances, which reproduces the formulas in Rodriguez,
#'   Reise, and Haviland (2016).
#' @param guidance Guidance settings returned by [nomo_defaults()].
#'
#' @return A `nomo_hierarchical` object with the detected `structure`, the
#'   `general` factor, the `groups` and their items, an `indices` table, a
#'   `subscales` table, an item-level `loadings` table, identification and
#'   estimand notes, and a `decision_log`.
#'
#' @references
#' Holzinger, K. J., & Swineford, F. (1937). The bi-factor method.
#' *Psychometrika, 2*(1), 41-54. \doi{10.1007/BF02287965}
#'
#' Reise, S. P. (2012). The rediscovery of bifactor measurement models.
#' *Multivariate Behavioral Research, 47*(5), 667-696.
#' \doi{10.1080/00273171.2012.715555}
#'
#' Reise, S. P., Bonifay, W. E., & Haviland, M. G. (2013). Scoring and modeling
#' psychological measures in the presence of multidimensionality. *Journal of
#' Personality Assessment, 95*(2), 129-140.
#' \doi{10.1080/00223891.2012.725437}
#'
#' Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating bifactor
#' models: Calculating and interpreting statistical indices. *Psychological
#' Methods, 21*(2), 137-150. \doi{10.1037/met0000045}
#'
#' Schmid, J., & Leiman, J. M. (1957). The development of hierarchical factor
#' solutions. *Psychometrika, 22*(1), 53-61. \doi{10.1007/BF02289209}
#'
#' Yung, Y.-F., Thissen, D., & McLeod, L. D. (1999). On the relationship
#' between the higher-order factor model and the hierarchical factor model.
#' *Psychometrika, 64*(2), 113-128. \doi{10.1007/BF02294531}
#'
#' @export
#'
#' @examples
#' set.seed(2026)
#' n <- 500
#' g <- rnorm(n)
#' s <- matrix(rnorm(n * 3), n, 3)
#' dat <- as.data.frame(sapply(1:9, function(i) {
#'   .6 * g + .45 * s[, ceiling(i / 3)] + rnorm(n, sd = .65)
#' }))
#' names(dat) <- paste0("x", 1:9)
#'
#' factors <- list(
#'   A = c("x1", "x2", "x3"),
#'   B = c("x4", "x5", "x6"),
#'   C = c("x7", "x8", "x9")
#' )
#'
#' bf <- nomo_cfa(nomo_model(factors, structure = "bifactor"), data = dat)
#' h <- nomo_hierarchical(bf)
#' h
#' nomo_table(h, "subscales")
nomo_hierarchical <- function(fit,
                              general = NULL,
                              obs.var = TRUE,
                              guidance = nomo_defaults()) {
  if (!is.logical(obs.var) || length(obs.var) != 1L || is.na(obs.var)) {
    stop("`obs.var` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.null(general) &&
      (!is.character(general) || length(general) != 1L || is.na(general) ||
         !nzchar(general))) {
    stop("`general` must be NULL or a single factor name.", call. = FALSE)
  }

  input <- nomo_hierarchical_input(fit)
  structure <- nomo_hierarchical_structure(input, general)
  matrices <- nomo_hierarchical_matrices(input$fit, structure, obs.var)
  computed <- nomo_hierarchical_compute(matrices, structure)

  notes <- nomo_hierarchical_notes(input, structure, matrices, computed, obs.var)

  out <- list(
    call = match.call(),
    source = input$source,
    structure = structure$type,
    general = structure$general,
    groups = structure$groups,
    items = structure$items,
    ordered = input$ordered,
    obs.var = obs.var,
    estimand = if (length(input$ordered)) "latent_response" else "observed_composite",
    indices = computed$indices,
    subscales = computed$subscales,
    loadings = computed$loadings,
    notes = notes,
    decision_log = nomo_hierarchical_decision_log(structure, notes, computed),
    fit = input$fit,
    guidance = guidance
  )
  class(out) <- c("nomo_hierarchical", "list")
  out
}


# Input validation -------------------------------------------------------------

nomo_hierarchical_input <- function(x) {
  source <- "lavaan"
  if (inherits(x, "nomo_cfa")) {
    x <- x$fit
    source <- "nomo_cfa"
  }

  is_lavaan <- inherits(x, "lavaan") ||
    tryCatch(methods::is(x, "lavaan"), error = function(e) FALSE)
  if (!is_lavaan) {
    stop("`fit` must be a `nomo_cfa` object or fitted `lavaan` model.", call. = FALSE)
  }

  if (!isTRUE(tryCatch(lavaan::lavInspect(x, "converged"), error = function(e) FALSE))) {
    stop(
      "`fit` did not converge. Hierarchical indices are not computed from a nonconverged model.",
      call. = FALSE
    )
  }

  ngroups <- tryCatch(lavaan::lavInspect(x, "ngroups"), error = function(e) 1L)
  nlevels <- tryCatch(lavaan::lavInspect(x, "nlevels"), error = function(e) 1L)
  if (!identical(as.integer(ngroups), 1L) || !identical(as.integer(nlevels), 1L)) {
    stop(
      "`nomo_hierarchical()` supports single-group, single-level models only.",
      call. = FALSE
    )
  }

  pe <- lavaan::parameterEstimates(x)
  lv <- as.character(lavaan::lavNames(x, type = "lv"))
  ov <- as.character(lavaan::lavNames(x, type = "ov"))

  structural <- pe$op == "~" & pe$lhs %in% lv & pe$rhs %in% lv
  if (any(structural)) {
    stop(
      paste(
        "`fit` contains latent regressions. `nomo_hierarchical()` evaluates",
        "measurement models; fit the bifactor or higher-order model on its own."
      ),
      call. = FALSE
    )
  }

  ordered <- unique(as.character(unlist(
    tryCatch(lavaan::lavInspect(x, "ordered"), error = function(e) character()),
    use.names = FALSE
  )))

  list(fit = x, source = source, pe = pe, lv = lv, ov = ov, ordered = ordered)
}


# Structure detection ----------------------------------------------------------

nomo_hierarchical_structure <- function(input, general = NULL) {
  pe <- input$pe
  lv <- input$lv
  ov <- input$ov
  loadings <- pe[pe$op == "=~", c("lhs", "rhs"), drop = FALSE]

  second_order <- unique(loadings$lhs[loadings$rhs %in% lv])

  if (length(second_order)) {
    return(nomo_hierarchical_structure_higher(loadings, lv, ov, second_order, general))
  }
  nomo_hierarchical_structure_bifactor(loadings, lv, ov, general)
}


nomo_hierarchical_structure_higher <- function(loadings, lv, ov, second_order, general) {
  if (length(second_order) > 1L) {
    stop(
      "`fit` has more than one second-order factor (",
      paste(second_order, collapse = ", "),
      "). `nomo_hierarchical()` supports a single second-order factor.",
      call. = FALSE
    )
  }
  if (!is.null(general) && !identical(general, second_order)) {
    stop(
      "`general` = '", general, "' is not the second-order factor; the model's ",
      "second-order factor is '", second_order, "'.",
      call. = FALSE
    )
  }
  general <- second_order

  if (any(loadings$lhs == general & loadings$rhs %in% ov)) {
    stop(
      "The second-order factor '", general, "' also loads directly on items. ",
      "Mixed higher-order and bifactor structures are not supported.",
      call. = FALSE
    )
  }

  first_order <- loadings$rhs[loadings$lhs == general]
  others <- setdiff(lv, c(general, first_order))
  if (length(others)) {
    stop(
      "Every first-order factor must load on '", general, "'; ",
      paste(others, collapse = ", "), " does not.",
      call. = FALSE
    )
  }

  groups <- lapply(first_order, function(f) {
    loadings$rhs[loadings$lhs == f & loadings$rhs %in% ov]
  })
  names(groups) <- first_order

  items <- unlist(groups, use.names = FALSE)
  if (anyDuplicated(items)) {
    stop(
      "An item loads on more than one first-order factor (",
      paste(unique(items[duplicated(items)]), collapse = ", "),
      "). Cross-loadings are not supported in a higher-order model here.",
      call. = FALSE
    )
  }

  list(
    type = "higher_order",
    general = general,
    groups = groups,
    items = ov[ov %in% items]
  )
}


nomo_hierarchical_structure_bifactor <- function(loadings, lv, ov, general) {
  item_loadings <- loadings[loadings$rhs %in% ov, , drop = FALSE]
  items <- ov[ov %in% item_loadings$rhs]

  covers_all <- vapply(lv, function(f) {
    all(items %in% item_loadings$rhs[item_loadings$lhs == f])
  }, logical(1))

  if (!is.null(general)) {
    if (!general %in% lv) {
      stop("`general` = '", general, "' is not a factor in `fit`.", call. = FALSE)
    }
    if (!covers_all[[general]]) {
      stop(
        "'", general, "' does not load on every item, so it is not a general ",
        "factor of a bifactor model.",
        call. = FALSE
      )
    }
  } else {
    candidates <- lv[covers_all]
    if (!length(candidates)) {
      stop(
        paste(
          "`fit` is neither a higher-order model (no factor is measured by",
          "other factors) nor a bifactor model (no factor loads on every item).",
          "For correlated first-order factors use `nomo_reliability()`."
        ),
        call. = FALSE
      )
    }
    if (length(candidates) > 1L) {
      stop(
        "More than one factor loads on every item (",
        paste(candidates, collapse = ", "),
        "). Name the general factor with `general`.",
        call. = FALSE
      )
    }
    general <- candidates
  }

  group_names <- setdiff(lv, general)
  if (!length(group_names)) {
    stop(
      paste(
        "`fit` has a single factor and no group factors, so there is no",
        "hierarchical structure to evaluate. Use `nomo_reliability()`."
      ),
      call. = FALSE
    )
  }

  groups <- lapply(group_names, function(f) {
    ov[ov %in% item_loadings$rhs[item_loadings$lhs == f]]
  })
  names(groups) <- group_names

  in_groups <- unlist(groups, use.names = FALSE)
  if (anyDuplicated(in_groups)) {
    stop(
      "An item loads on more than one group factor (",
      paste(unique(in_groups[duplicated(in_groups)]), collapse = ", "),
      "). Each item may belong to at most one group factor.",
      call. = FALSE
    )
  }

  list(type = "bifactor", general = general, groups = groups, items = items)
}


# Matrices ---------------------------------------------------------------------

# Writes the model in reduced form so both structures are handled by one
# engine. With A = (I - B)^-1, the loading of item i on source j is
# (Lambda A)[i, j], and when the sources are orthogonal the variance that
# source j contributes to a unit-weighted composite w is psi_jj * (w' Lambda A_j)^2.
# For a higher-order model the sources are the second-order factor and the
# first-order disturbances, which is the Schmid-Leiman decomposition.
nomo_hierarchical_matrices <- function(fit, structure, obs.var) {
  est <- lavaan::lavInspect(fit, "est")
  lambda <- est$lambda
  psi <- est$psi
  beta <- est$beta
  latent <- colnames(lambda)

  A <- if (is.null(beta)) diag(length(latent)) else solve(diag(length(latent)) - beta)
  dimnames(A) <- list(latent, latent)
  source_loadings <- lambda %*% A

  sources <- c(structure$general, names(structure$groups))
  source_psi <- psi[sources, sources, drop = FALSE]
  off <- source_psi
  diag(off) <- 0
  if (any(abs(off) > 1e-8)) {
    stop(
      paste(
        "The general and group sources are not orthogonal in `fit`. Variance",
        "can be attributed to a source only when sources do not covary. Fix",
        "the covariances among the general and group factors to zero",
        "(`nomo_model(structure = \"bifactor\")` does this) and refit."
      ),
      call. = FALSE
    )
  }

  implied <- as.matrix(lavaan::lavInspect(fit, "implied")$cov)
  denominator_cov <- if (isTRUE(obs.var)) {
    as.matrix(lavaan::lavInspect(fit, "sampstat")$cov)
  } else {
    implied
  }

  theta <- est$theta

  list(
    source_loadings = source_loadings,
    psi = psi,
    implied = implied,
    denominator_cov = denominator_cov,
    theta = theta,
    sources = sources
  )
}


# Computation ------------------------------------------------------------------

nomo_hierarchical_compute <- function(matrices, structure) {
  items <- structure$items
  general <- structure$general
  groups <- structure$groups
  LA <- matrices$source_loadings
  psi <- matrices$psi
  S <- matrices$denominator_cov

  contribution <- function(weight_items, source) {
    w <- as.numeric(rownames(LA) %in% weight_items)
    psi[source, source] * sum(w * LA[, source])^2
  }
  denominator <- function(weight_items) sum(S[weight_items, weight_items])

  total_denominator <- denominator(items)
  general_total <- contribution(items, general)
  common_total <- general_total + sum(vapply(
    names(groups), function(k) contribution(items, k), numeric(1)
  ))

  omega_total <- common_total / total_denominator
  omega_h <- general_total / total_denominator

  sd_items <- sqrt(diag(matrices$implied)[items])
  general_std <- LA[items, general] * sqrt(psi[general, general]) / sd_items
  group_of <- vapply(items, function(i) {
    k <- names(groups)[vapply(groups, function(g) i %in% g, logical(1))]
    if (length(k)) k else NA_character_
  }, character(1))
  group_std <- vapply(items, function(i) {
    k <- group_of[[i]]
    if (is.na(k)) return(0)
    LA[i, k] * sqrt(psi[k, k]) / sd_items[[i]]
  }, numeric(1))

  ecv <- sum(general_std^2) / sum(general_std^2 + group_std^2)

  n <- length(items)
  within_pairs <- sum(vapply(groups, function(g) choose(length(g), 2), numeric(1)))
  puc <- 1 - within_pairs / choose(n, 2)

  subscales <- tibble::tibble(
    subscale = names(groups),
    n_items = as.integer(lengths(groups)),
    omega_subscale = vapply(names(groups), function(k) {
      (contribution(groups[[k]], general) + contribution(groups[[k]], k)) /
        denominator(groups[[k]])
    }, numeric(1), USE.NAMES = FALSE),
    omega_hierarchical_subscale = vapply(names(groups), function(k) {
      contribution(groups[[k]], k) / denominator(groups[[k]])
    }, numeric(1), USE.NAMES = FALSE),
    items = unname(vapply(groups, paste, character(1), collapse = ", "))
  )
  subscales$omega_hs_relative <- subscales$omega_hierarchical_subscale /
    subscales$omega_subscale
  subscales <- subscales[, c(
    "subscale", "n_items", "omega_subscale", "omega_hierarchical_subscale",
    "omega_hs_relative", "items"
  )]

  loadings <- tibble::tibble(
    item = items,
    subscale = unname(group_of),
    general_loading = unname(general_std),
    group_loading = unname(group_std),
    communality = unname(general_std^2 + group_std^2),
    item_ecv = unname(general_std^2 / (general_std^2 + group_std^2))
  )

  indices <- tibble::tibble(
    index = c(
      "omega_total", "omega_hierarchical", "omega_hierarchical_relative",
      "ecv", "puc"
    ),
    estimate = c(omega_total, omega_h, omega_h / omega_total, ecv, puc),
    estimand = c(
      "Proportion of total-score variance explained by all common sources",
      "Proportion of total-score variance explained by the general factor",
      "Share of the total score's reliable variance that is general",
      "Share of common item variance explained by the general factor",
      "Share of item correlations influenced only by the general factor"
    )
  )
  indices$interpretation <- nomo_hierarchical_interpretation(indices, structure)

  list(indices = indices, subscales = subscales, loadings = loadings)
}


nomo_hierarchical_interpretation <- function(indices, structure) {
  value <- function(name) indices$estimate[indices$index == name]
  general_label <- if (identical(structure$type, "higher_order")) {
    sprintf("the second-order factor (%s)", structure$general)
  } else {
    sprintf("the general factor (%s)", structure$general)
  }
  capitalize <- function(x) paste0(toupper(substr(x, 1L, 1L)), substring(x, 2L))

  c(
    sprintf(
      "Common sources explain %.2f of the variance of the unit-weighted total score.",
      value("omega_total")
    ),
    sprintf(
      "%s explains %.2f of the variance of the unit-weighted total score.",
      capitalize(general_label),
      value("omega_hierarchical")
    ),
    sprintf(
      "Of the total score's reliable variance, %.2f reflects %s and the rest reflects group factors.",
      value("omega_hierarchical_relative"),
      general_label
    ),
    sprintf(
      paste(
        "%s explains %.2f of the common variance across items. Higher values",
        "indicate a stronger general factor relative to the group factors; no",
        "benchmark value establishes that the items are unidimensional."
      ),
      capitalize(general_label),
      value("ecv")
    ),
    sprintf(
      paste(
        "%.2f of item correlations are influenced only by the general factor.",
        "When this is very high, even a modest ECV can yield relatively",
        "unbiased estimates from a unidimensional model."
      ),
      value("puc")
    )
  )
}


# Notes and decision log -------------------------------------------------------

nomo_hierarchical_notes <- function(input, structure, matrices, computed, obs.var) {
  notes <- tibble::tibble(
    topic = character(),
    severity = character(),
    note = character()
  )
  add <- function(notes, topic, severity, note) {
    tibble::add_row(notes, topic = topic, severity = severity, note = note)
  }

  k <- length(structure$groups)

  if (identical(structure$type, "higher_order")) {
    notes <- add(notes, "structure", "info", sprintf(
      paste(
        "Higher-order model: %s explains the correlations among %d first-order",
        "factors (%s). Group sources are the first-order disturbances, so the",
        "loadings are the Schmid-Leiman decomposition of this solution."
      ),
      structure$general, k, paste(names(structure$groups), collapse = ", ")
    ))
    if (k == 3L) {
      notes <- add(notes, "identification", "review", paste(
        "With three first-order factors the second-order part is just",
        "identified. This model fits exactly as well as the correlated-factors",
        "model, so fit cannot distinguish them."
      ))
    }
  } else {
    only_general <- setdiff(structure$items, unlist(structure$groups, use.names = FALSE))
    notes <- add(notes, "structure", "info", sprintf(
      paste(
        "Bifactor model: %s is measured by all %d items, with %d group",
        "factor(s) (%s).%s"
      ),
      structure$general, length(structure$items), k,
      paste(names(structure$groups), collapse = ", "),
      if (length(only_general)) {
        sprintf(" Item(s) %s load on the general factor only.", paste(only_general, collapse = ", "))
      } else {
        ""
      }
    ))
    if (k == 2L) {
      notes <- add(notes, "identification", "review", paste(
        "With only two group factors, a bifactor model can be empirically",
        "under-identified or unstable. Check lavaan's warnings and standard",
        "errors before interpreting the group factors."
      ))
    }
  }

  notes <- add(notes, "model_choice", "review", paste(
    "A bifactor model will usually fit at least as well as correlated-factors",
    "or higher-order models of the same items, even when it did not generate",
    "the data (Reise, 2012), and a higher-order model is a constrained version",
    "of it (Yung, Thissen, & McLeod, 1999). Compare the alternatives with",
    "nomo_compare() and choose on substantive grounds, not on fit alone."
  ))

  if (length(input$ordered)) {
    notes <- add(notes, "estimand", "info", paste(
      "Indicators are ordered, so each omega describes the latent-response",
      "composite: an upper bound on the reliability of the observed ordinal sum",
      "score, which is not provided here."
    ))
  } else {
    notes <- add(notes, "estimand", "info", sprintf(
      "Each omega describes a unit-weighted observed composite, with %s covariances in the denominator.",
      if (isTRUE(obs.var)) "observed" else "model-implied"
    ))
  }

  mixed <- vapply(names(structure$groups), function(g) {
    l <- computed$loadings$group_loading[computed$loadings$subscale %in% g]
    any(l > 0) && any(l < 0)
  }, logical(1))
  if (any(mixed)) {
    notes <- add(notes, "group_loadings", "review", paste0(
      "Group factor(s) ", paste(names(mixed)[mixed], collapse = ", "),
      " have loadings of mixed sign, so what the group factor represents ",
      "after the general factor is removed is unclear. Inspect the item ",
      "content before interpreting its subscale score."
    ))
  }

  theta <- matrices$theta
  negative <- rownames(theta)[diag(theta) < 0]
  if (length(negative)) {
    notes <- add(notes, "improper_solution", "concern", paste0(
      "Negative residual variance for ", paste(negative, collapse = ", "),
      ". The solution is improper and the indices should not be interpreted ",
      "until the cause is understood."
    ))
  }

  notes
}


nomo_hierarchical_decision_log <- function(structure, notes, computed) {
  log <- nomo_log_new()

  for (i in seq_len(nrow(computed$indices))) {
    row <- computed$indices[i, ]
    log <- nomo_log_add(
      log,
      stage = "hierarchical",
      object = structure$general,
      metric = row$index,
      value = row$estimate,
      reference = "Descriptive index; no pass/fail threshold is applied",
      severity = "info",
      observation = row$interpretation
    )
  }

  for (i in seq_len(nrow(computed$subscales))) {
    row <- computed$subscales[i, ]
    log <- nomo_log_add(
      log,
      stage = "hierarchical",
      object = row$subscale,
      metric = "omega_hierarchical_subscale",
      value = row$omega_hierarchical_subscale,
      reference = "Descriptive index; no pass/fail threshold is applied",
      severity = "info",
      observation = sprintf(
        paste(
          "After removing the general factor, %.2f of the %s composite's",
          "variance is reliable variance specific to %s (omega subscale = %.2f)."
        ),
        row$omega_hierarchical_subscale, row$subscale, row$subscale,
        row$omega_subscale
      ),
      recommendation = paste(
        "Report a subscale score only if the reliable variance it carries",
        "beyond the general factor supports its intended use."
      )
    )
  }

  for (i in seq_len(nrow(notes))) {
    log <- nomo_log_add(
      log,
      stage = "hierarchical",
      object = structure$general,
      metric = notes$topic[[i]],
      reference = "Model structure and estimand",
      severity = notes$severity[[i]],
      observation = notes$note[[i]]
    )
  }

  log
}
