# Shared measurement-model helpers --------------------------------------------

# A model that names a variable the data lack would otherwise reach lavaan,
# whose error names its internal step rather than the argument to fix (#89).
# The check runs before fitting and names each absent variable. A model lavaan
# cannot parse is left to lavaan, which explains the syntax error.
nomo_check_model_variables <- function(model, data, data_arg = "data") {
  observed <- tryCatch(
    as.character(lavaan::lavNames(lavaan::lavaanify(model), type = "ov")),
    error = function(e) character()
  )
  absent <- setdiff(observed, names(data))
  if (length(absent)) {
    stop(
      sprintf(
        "`model` names variable(s) not found in `%s`: %s. Check the spelling against names(%s).",
        data_arg, paste(absent, collapse = ", "), data_arg
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


# A model indicator stored as an ordered factor is categorical to lavaan
# whether or not `ordered` names it. Such columns are treated as declared, so
# what the result records is what was fitted (#145). `detected` holds the
# columns added here, for the decision log.
nomo_ordered_indicators <- function(model, data, ordered = character()) {
  ordered <- as.character(ordered)
  indicators <- tryCatch(
    as.character(lavaan::lavNames(lavaan::lavaanify(model), type = "ov")),
    error = function(e) character()
  )
  present <- intersect(indicators, names(data))
  stored <- present[vapply(data[present], is.ordered, logical(1))]
  detected <- setdiff(stored, ordered)
  list(ordered = c(ordered, detected), detected = detected)
}


nomo_ordered_detected_log <- function(log, detected, stage) {
  if (!length(detected)) return(log)
  nomo_log_add(
    log, stage = stage, object = paste(detected, collapse = ", "),
    metric = "ordered_detected",
    value = length(detected),
    reference = "Ordered factors are modeled as ordered-categorical indicators",
    severity = "review",
    observation = sprintf(
      "%s stored as %s and not named in `ordered`: %s. %s modeled as ordered, as lavaan fits %s.",
      nomo_present_count(length(detected), "indicator is", "indicators are"),
      nomo_present_noun(length(detected), "an ordered factor", "ordered factors"),
      paste(detected, collapse = ", "),
      nomo_present_noun(length(detected), "It is", "They are"),
      nomo_present_noun(length(detected), "it", "them")
    ),
    recommendation = paste(
      "Name these columns in `ordered` to make the choice explicit, or convert",
      "them to numeric to model them as continuous."
    )
  )
}


nomo_measurement_fit <- function(x, arg = "fit", allow_cross_loadings = FALSE) {
  wrapper <- NULL
  source <- "lavaan"

  if (inherits(x, "nomo_cfa")) {
    wrapper <- x
    x <- x$fit
    source <- "nomo_cfa"
  }

  is_lavaan <- inherits(x, "lavaan") ||
    tryCatch(methods::is(x, "lavaan"), error = function(e) FALSE)

  if (!is_lavaan) {
    stop(
      sprintf("`%s` must be a `nomo_cfa` object or fitted `lavaan` model.", arg),
      call. = FALSE
    )
  }

  converged <- isTRUE(tryCatch(
    lavaan::lavInspect(x, "converged"),
    error = function(e) FALSE
  ))

  if (!converged) {
    stop(
      sprintf(
        "`%s` did not converge. Measurement evidence is not computed from a nonconverged model.",
        arg
      ),
      call. = FALSE
    )
  }

  pe <- tryCatch(
    lavaan::parameterEstimates(x),
    error = function(e) NULL
  )
  lv <- tryCatch(
    as.character(lavaan::lavNames(x, type = "lv")),
    error = function(e) character()
  )

  if (!is.null(pe) && length(lv)) {
    structural <- pe$op == "~" & pe$lhs %in% lv & pe$rhs %in% lv
    if (any(structural, na.rm = TRUE)) {
      stop(
        paste(
          "`fit` contains latent structural regressions.",
          "Reliability is intentionally limited to CFA measurement models;",
          "fit or pass the measurement model separately."
        ),
        call. = FALSE
      )
    }

    higher_order <- pe$op == "=~" & pe$rhs %in% lv
    if (any(higher_order, na.rm = TRUE)) {
      stop(
        paste(
          "`fit` contains a higher-order measurement model.",
          "This function evaluates first-order CFA composites; use",
          "`nomo_hierarchical()` for omega hierarchical, subscale omegas,",
          "explained common variance, and PUC in higher-order and bifactor models."
        ),
        call. = FALSE
      )
    }

    loading_cols <- intersect(c("lhs", "rhs", "group", "level"), names(pe))
    loadings <- pe[pe$op == "=~", loading_cols, drop = FALSE]
    block_cols <- intersect(c("group", "level"), names(loadings))
    if (length(block_cols)) {
      block_key <- apply(loadings[block_cols], 1L, paste, collapse = ":")
    } else {
      block_key <- rep("overall", nrow(loadings))
    }
    item_key <- paste(block_key, loadings$rhs, sep = "::")
    factor_count <- vapply(
      split(loadings$lhs, item_key),
      function(z) length(unique(z)),
      integer(1)
    )
    cross_keys <- names(factor_count)[factor_count > 1L]
    cross_loaded_items <- unique(loadings$rhs[item_key %in% cross_keys])

    if (length(cross_loaded_items) && !isTRUE(allow_cross_loadings)) {
      stop(
        paste0(
          "`fit` contains cross-loaded indicator(s): ",
          paste(cross_loaded_items, collapse = ", "),
          ". Reliability requires a simple first-order CFA so the target composite is unambiguous.",
          " If this is a bifactor model, use `nomo_hierarchical()`."
        ),
        call. = FALSE
      )
    }
  } else {
    cross_loaded_items <- character()
  }

  # semTools reports a composite only for a factor with more than one
  # indicator, and leaves a lone composite unnamed. Its coefficients are named
  # from these factors, in model order, so a single-indicator factor beside
  # one scale does not leave that scale's omega under an invented name (#145).
  composite_names <- lv
  if (!is.null(pe) && length(lv)) {
    indicator_count <- vapply(lv, function(f) {
      length(unique(pe$rhs[pe$op == "=~" & pe$lhs == f]))
    }, integer(1))
    composite_names <- lv[indicator_count > 1L]
  }

  # lavaan fits a column stored as an ordered factor as categorical whether or
  # not `ordered` named it, and lavInspect(x, "ordered") lists only the named
  # ones. lavNames(x, "ov.ord") lists what was fitted (#145).
  ordered <- unique(tryCatch(
    as.character(lavaan::lavNames(x, type = "ov.ord")),
    error = function(e) character()
  ))

  ngroups <- suppressWarnings(as.integer(tryCatch(
    lavaan::lavInspect(x, "ngroups"),
    error = function(e) 1L
  ))[1L])
  nlevels <- suppressWarnings(as.integer(tryCatch(
    lavaan::lavInspect(x, "nlevels"),
    error = function(e) 1L
  ))[1L])
  if (!is.finite(ngroups) || ngroups < 1L) ngroups <- 1L
  if (!is.finite(nlevels) || nlevels < 1L) nlevels <- 1L

  post_check <- tryCatch(
    lavaan::lavInspect(x, "post.check"),
    error = function(e) NA
  )

  list(
    fit = x,
    wrapper = wrapper,
    source = source,
    ordered = ordered,
    post_check = post_check,
    parameter_estimates = pe,
    # The names semTools results are matched to, not every latent variable.
    latent_names = composite_names,
    cross_loaded_items = cross_loaded_items,
    ngroups = ngroups,
    nlevels = nlevels
  )
}


nomo_guidance_value <- function(guidance, name) {
  if (!is.list(guidance) || !name %in% names(guidance)) {
    stop(
      sprintf("`guidance` is missing required setting `%s`.", name),
      call. = FALSE
    )
  }

  value <- suppressWarnings(as.numeric(guidance[[name]])[1L])
  if (!is.finite(value)) {
    stop(
      sprintf("`guidance$%s` must be one finite numeric value.", name),
      call. = FALSE
    )
  }
  value
}


nomo_reliability_tidy <- function(x, metric, construct_names = NULL) {
  fallback_names <- function(existing, n) {
    if (!is.null(existing) && length(existing) == n && all(nzchar(existing))) {
      return(as.character(existing))
    }
    if (!is.null(construct_names) && length(construct_names) == n &&
        all(nzchar(construct_names))) {
      return(as.character(construct_names))
    }
    paste0("construct_", seq_len(n))
  }

  # semTools returns NA as a logical vector when no value is defined, as AVE
  # is for a factor with a cross-loaded indicator; it keeps its row (#145).
  values_like <- function(v) is.numeric(v) || is.logical(v)

  if (is.atomic(x) && is.null(dim(x)) && values_like(x)) {
    values <- as.numeric(x)
    blocks <- names(x)
    # One composite in a multi-group model comes back named by group.
    if (length(construct_names) == 1L && length(values) > 1L &&
        !is.null(blocks) && !any(blocks %in% construct_names)) {
      return(tibble::tibble(
        construct = as.character(construct_names),
        block = as.character(blocks),
        metric = metric,
        estimate = values
      ))
    }
    nm <- fallback_names(blocks, length(values))
    return(tibble::tibble(
      construct = nm,
      block = "overall",
      metric = metric,
      estimate = values
    ))
  }

  if (is.data.frame(x)) {
    meta_names <- intersect(names(x), c("group", "level", "block"))
    value_names <- setdiff(names(x), meta_names)
    value_names <- value_names[vapply(x[value_names], values_like, logical(1))]

    if (!length(value_names)) return(tibble::tibble())

    # Without a group column, a multi-group result names its groups in the
    # row names (#145).
    if (length(meta_names)) {
      block <- apply(x[meta_names], 1L, function(z) paste(z, collapse = ":"))
    } else if (nrow(x) == 1L) {
      block <- "overall"
    } else if (.row_names_info(x) > 0L) {
      block <- rownames(x)
    } else {
      block <- paste0("block_", seq_len(nrow(x)))
    }

    rows <- lapply(value_names, function(v) {
      tibble::tibble(
        construct = v,
        block = as.character(block),
        metric = metric,
        estimate = as.numeric(x[[v]])
      )
    })
    return(dplyr::bind_rows(rows))
  }

  if (is.list(x)) {
    nm <- fallback_names(names(x), length(x))

    rows <- lapply(seq_along(x), function(i) {
      vals <- suppressWarnings(as.numeric(x[[i]]))
      if (!length(vals)) return(NULL)
      inner <- names(x[[i]])
      # A multilevel result lists each level, with that level's factors
      # inside it.
      if (!is.null(inner) && all(inner %in% construct_names)) {
        return(tibble::tibble(
          construct = inner,
          block = nm[[i]],
          metric = metric,
          estimate = vals
        ))
      }
      blocks <- inner
      if (is.null(blocks) || any(!nzchar(blocks))) {
        blocks <- if (length(vals) == 1L) "overall" else paste0("block_", seq_along(vals))
      }
      tibble::tibble(
        construct = nm[[i]],
        block = as.character(blocks),
        metric = metric,
        estimate = vals
      )
    })
    return(dplyr::bind_rows(rows))
  }

  tibble::tibble()
}


nomo_reliability_item_types <- function(fit_info) {
  pe <- fit_info$parameter_estimates
  if (is.null(pe)) return(tibble::tibble())

  # A multi-group model lists each loading once per group; an indicator is
  # counted once.
  loads <- unique(pe[pe$op == "=~", c("lhs", "rhs"), drop = FALSE])
  if (!nrow(loads)) return(tibble::tibble())

  loads$ordered <- loads$rhs %in% fit_info$ordered
  pieces <- split(loads, loads$lhs)

  tibble::tibble(
    construct = names(pieces),
    n_items = vapply(pieces, nrow, integer(1)),
    n_ordered = vapply(pieces, function(z) sum(z$ordered), integer(1)),
    indicator_type = vapply(pieces, function(z) {
      n_ordered <- sum(z$ordered)
      if (n_ordered == 0L) {
        "continuous"
      } else if (n_ordered == nrow(z)) {
        "ordered"
      } else {
        "mixed"
      }
    }, character(1))
  )
}



nomo_reliability_fit_context <- function(fit, guidance) {
  measures <- tryCatch(lavaan::fitMeasures(fit), error = function(e) numeric())
  if (!length(measures)) return(tibble::tibble())

  tryCatch(
    nomo_cfa_fit_evidence(measures, guidance = guidance),
    error = function(e) tibble::tibble()
  )
}


# Validity-evidence helpers ----------------------------------------------------

nomo_validity_ave_tidy <- function(x, construct_names = NULL) {
  out <- nomo_reliability_tidy(x, metric = "AVE", construct_names = construct_names)
  if (!nrow(out)) return(tibble::tibble())
  out$metric <- NULL
  out
}


nomo_matrix_pairs <- function(mat, value_name = "estimate", block = "overall") {
  if (!is.matrix(mat) || nrow(mat) < 2L || ncol(mat) < 2L) {
    return(tibble::tibble())
  }

  # A matrix with at least two rows and columns always has at least one
  # lower-triangle pair.
  idx <- which(lower.tri(mat), arr.ind = TRUE)

  rn <- rownames(mat)
  cn <- colnames(mat)
  if (is.null(rn)) rn <- paste0("V", seq_len(nrow(mat)))
  if (is.null(cn)) cn <- paste0("V", seq_len(ncol(mat)))

  out <- tibble::tibble(
    construct_1 = rn[idx[, "row"]],
    construct_2 = cn[idx[, "col"]],
    block = block
  )
  out[[value_name]] <- as.numeric(mat[idx])
  out
}


nomo_validity_latent_correlations <- function(fit) {
  lv <- tryCatch(
    as.character(lavaan::lavNames(fit, type = "lv")),
    error = function(e) character()
  )

  standardized <- tryCatch(
    tibble::as_tibble(lavaan::standardizedSolution(fit, type = "std.all")),
    error = function(e) tibble::tibble()
  )

  if (length(lv) && nrow(standardized) &&
      all(c("lhs", "op", "rhs", "est.std") %in% names(standardized))) {
    rows <- standardized[
      standardized$op == "~~" &
        standardized$lhs %in% lv &
        standardized$rhs %in% lv &
        standardized$lhs != standardized$rhs,
      ,
      drop = FALSE
    ]

    if (nrow(rows)) {
      meta_names <- intersect(c("group", "level"), names(rows))
      # standardizedSolution() numbers the groups. They are named as the AVE
      # table and the data name them, so the two tables' blocks agree.
      if ("group" %in% meta_names) {
        rows$group <- lavaan::lavInspect(fit, "group.label")[rows$group]
      }
      block <- if (length(meta_names)) {
        apply(rows[meta_names], 1L, function(z) paste(z, collapse = ":"))
      } else rep("overall", nrow(rows))

      get_num <- function(name) {
        if (name %in% names(rows)) suppressWarnings(as.numeric(rows[[name]]))
        else rep(NA_real_, nrow(rows))
      }

      return(tibble::tibble(
        construct_1 = as.character(rows$lhs),
        construct_2 = as.character(rows$rhs),
        block = as.character(block),
        correlation = get_num("est.std"),
        ci_lower = get_num("ci.lower"),
        ci_upper = get_num("ci.upper")
      ))
    }
  }

  cor_lv <- tryCatch(lavaan::lavInspect(fit, "cor.lv"), error = function(e) NULL)

  add_ci <- function(x) {
    if (!nrow(x)) return(x)
    x$ci_lower <- NA_real_
    x$ci_upper <- NA_real_
    x
  }

  if (is.matrix(cor_lv)) {
    return(add_ci(nomo_matrix_pairs(cor_lv, value_name = "correlation")))
  }

  if (is.list(cor_lv)) {
    nms <- names(cor_lv)
    if (is.null(nms) || any(!nzchar(nms))) nms <- paste0("block_", seq_along(cor_lv))
    return(dplyr::bind_rows(lapply(seq_along(cor_lv), function(i) {
      add_ci(nomo_matrix_pairs(
        cor_lv[[i]], value_name = "correlation", block = nms[[i]]
      ))
    })))
  }

  tibble::tibble()
}

nomo_validity_standardized_loadings <- function(fit, guidance) {
  standardized <- tryCatch(
    tibble::as_tibble(lavaan::standardizedSolution(fit, type = "std.all")),
    error = function(e) tibble::tibble()
  )
  nomo_cfa_loadings(standardized, guidance = guidance)
}


nomo_validity_htmt_inputs <- function(fit_info) {
  if (fit_info$ngroups != 1L || fit_info$nlevels != 1L) {
    return(list(
      available = FALSE,
      reason = paste(
        "HTMT/HTMT2 is not silently pooled across groups or levels.",
        "nomologR reports it only for single-group, single-level measurement models."
      )
    ))
  }

  if (length(fit_info$cross_loaded_items)) {
    return(list(
      available = FALSE,
      reason = paste0(
        "HTMT/HTMT2 was not computed because cross-loaded indicator(s) make simple trait membership ambiguous: ",
        paste(fit_info$cross_loaded_items, collapse = ", "),
        "."
      )
    ))
  }

  pe <- fit_info$parameter_estimates
  if (is.null(pe)) {
    return(list(
      available = FALSE,
      reason = "Measurement-model loadings could not be recovered from the fitted model."
    ))
  }

  loads <- pe[pe$op == "=~", c("lhs", "rhs"), drop = FALSE]
  if (!nrow(loads) || length(unique(loads$lhs)) < 2L) {
    return(list(
      available = FALSE,
      reason = "HTMT/HTMT2 requires at least two latent constructs."
    ))
  }

  split_items <- split(loads$rhs, loads$lhs)
  model <- paste(
    vapply(
      names(split_items),
      function(f) paste0(f, " =~ ", paste(unique(split_items[[f]]), collapse = " + ")),
      character(1)
    ),
    collapse = "\n"
  )

  dat <- tryCatch(
    lavaan::lavInspect(fit_info$fit, "data"),
    error = function(e) NULL
  )
  if (is.list(dat) && length(dat) == 1L) dat <- dat[[1L]]
  if (!is.matrix(dat) && !is.data.frame(dat)) {
    return(list(
      available = FALSE,
      reason = "Raw observed data could not be recovered from the fitted lavaan object."
    ))
  }

  dat <- as.data.frame(dat)
  ov <- tryCatch(
    as.character(lavaan::lavNames(fit_info$fit, type = "ov")),
    error = function(e) character()
  )
  if (length(ov) == ncol(dat)) names(dat) <- ov

  required <- unique(loads$rhs)
  if (!all(required %in% names(dat))) {
    return(list(
      available = FALSE,
      reason = "Recovered model data could not be aligned with the indicators required for HTMT/HTMT2."
    ))
  }

  list(
    available = TRUE,
    reason = "",
    model = model,
    data = dat[, required, drop = FALSE],
    ordered = intersect(fit_info$ordered, required)
  )
}


# The missing-data handling of the fitted model, in the terms semTools::htmt()
# passes to lavaan::lavCor(): full information when the model used maximum
# likelihood (or a two-stage estimate of the same saturated moments), pairwise
# when it used pairwise deletion, and listwise otherwise (#145).
nomo_validity_htmt_missing <- function(fit) {
  mode <- tolower(as.character(
    tryCatch(lavaan::lavInspect(fit, "options")$missing, error = function(e) "")
  )[1L])
  if (mode %in% c("ml", "ml.x", "two.stage", "robust.two.stage")) {
    "fiml"
  } else if (mode %in% c("pairwise", "available.cases", "doubly.robust")) {
    "pairwise"
  } else {
    "listwise"
  }
}


# The cases the HTMT correlations rest on under each handling: the complete
# cases, the smallest pairwise count, or every case with any indicator observed.
nomo_validity_htmt_n <- function(data, missing) {
  observed <- !is.na(as.matrix(data))
  as.integer(switch(
    missing,
    listwise = sum(rowSums(!observed) == 0L),
    pairwise = min(crossprod(observed)),
    sum(rowSums(observed) > 0L)
  ))
}


nomo_validity_fornell_larcker <- function(fit, ave) {
  cor_lv <- tryCatch(
    lavaan::lavInspect(fit, "cor.lv"),
    error = function(e) NULL
  )
  if (!is.matrix(cor_lv)) {
    return(list(
      matrix = NULL,
      pairs = tibble::tibble(),
      reason = "A single latent-correlation matrix was not available."
    ))
  }

  ave_overall <- ave[ave$block == "overall", , drop = FALSE]
  if (!nrow(ave_overall)) {
    return(list(
      matrix = NULL,
      pairs = tibble::tibble(),
      reason = "Single-block AVE values were not available."
    ))
  }

  ave_named <- stats::setNames(ave_overall$estimate, ave_overall$construct)
  factors <- intersect(rownames(cor_lv), names(ave_named))
  if (length(factors) < 2L) {
    return(list(
      matrix = NULL,
      pairs = tibble::tibble(),
      reason = "AVE and latent-correlation factor names could not be aligned for at least two constructs."
    ))
  }

  mat <- cor_lv[factors, factors, drop = FALSE]
  diag_values <- ave_named[factors]
  diag_values[!is.finite(diag_values) | diag_values < 0] <- NA_real_
  diag(mat) <- sqrt(diag_values)

  corr_pairs <- nomo_matrix_pairs(
    cor_lv[factors, factors, drop = FALSE],
    value_name = "latent_correlation"
  )
  if (nrow(corr_pairs)) {
    corr_pairs$sqrt_ave_1 <- sqrt(ave_named[corr_pairs$construct_1])
    corr_pairs$sqrt_ave_2 <- sqrt(ave_named[corr_pairs$construct_2])
    corr_pairs$abs_latent_correlation <- abs(corr_pairs$latent_correlation)
    corr_pairs$min_sqrt_ave <- pmin(
      corr_pairs$sqrt_ave_1,
      corr_pairs$sqrt_ave_2,
      na.rm = FALSE
    )
    corr_pairs$difference <- corr_pairs$min_sqrt_ave - corr_pairs$abs_latent_correlation
    corr_pairs$attention <- ifelse(
      !is.finite(corr_pairs$difference),
      "unavailable",
      ifelse(corr_pairs$difference <= 0, "review", "info")
    )
  }

  list(matrix = mat, pairs = corr_pairs, reason = "")
}
