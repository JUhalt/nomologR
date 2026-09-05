# Reliability uncertainty helpers ---------------------------------------------

nomo_reliability_boot_stat <- function(fit,
                                       expected_keys,
                                       construct_names,
                                       type_context,
                                       obs.var,
                                       ordinal_scale,
                                       include_alpha) {
  out <- stats::setNames(rep(NA_real_, length(expected_keys)), expected_keys)

  omega_engine <- tryCatch(
    semTools::compRelSEM(
      fit,
      obs.var = obs.var,
      tau.eq = FALSE,
      ord.scale = ordinal_scale,
      simplify = TRUE
    ),
    error = function(e) NULL
  )
  omega_tbl <- if (is.null(omega_engine)) {
    tibble::tibble()
  } else {
    nomo_reliability_tidy(
      omega_engine,
      metric = "omega",
      construct_names = construct_names
    )
  }

  alpha_tbl <- tibble::tibble(
    construct = character(),
    block = character(),
    metric = character(),
    estimate = numeric()
  )

  if (isTRUE(include_alpha)) {
    ordered_constructs <- type_context$construct[
      type_context$indicator_type == "ordered"
    ]
    continuous_constructs <- type_context$construct[
      type_context$indicator_type == "continuous"
    ]

    if (length(ordered_constructs) && isTRUE(ordinal_scale)) {
      if (length(continuous_constructs)) {
        alpha_engine <- tryCatch(
          semTools::compRelSEM(
            fit,
            obs.var = obs.var,
            tau.eq = continuous_constructs,
            ord.scale = TRUE,
            simplify = TRUE
          ),
          error = function(e) NULL
        )
        if (!is.null(alpha_engine)) {
          alpha_tbl <- nomo_reliability_tidy(
            alpha_engine,
            metric = "alpha",
            construct_names = construct_names
          )
          if (nrow(alpha_tbl)) {
            alpha_tbl <- alpha_tbl[
              alpha_tbl$construct %in% continuous_constructs,
              ,
              drop = FALSE
            ]
          }
        }
      }
    } else {
      alpha_engine <- tryCatch(
        semTools::compRelSEM(
          fit,
          obs.var = obs.var,
          tau.eq = TRUE,
          ord.scale = ordinal_scale,
          simplify = TRUE
        ),
        error = function(e) NULL
      )
      if (!is.null(alpha_engine)) {
        alpha_tbl <- nomo_reliability_tidy(
          alpha_engine,
          metric = "alpha",
          construct_names = construct_names
        )
      }
    }
  }

  tidy <- dplyr::bind_rows(omega_tbl, alpha_tbl)
  if (!nrow(tidy)) return(out)

  keys <- paste(tidy$metric, tidy$construct, tidy$block, sep = "::")
  idx <- match(keys, expected_keys)
  keep <- !is.na(idx) & is.finite(tidy$estimate)
  out[idx[keep]] <- tidy$estimate[keep]
  out
}


nomo_reliability_bootstrap_ci <- function(fit_info,
                                          evidence,
                                          type_context,
                                          obs.var,
                                          ordinal_scale,
                                          include_alpha,
                                          level,
                                          R,
                                          seed) {
  expected_keys <- paste(
    evidence$metric, evidence$construct, evidence$block, sep = "::"
  )

  empty_intervals <- tibble::tibble(
    construct = evidence$construct,
    block = evidence$block,
    metric = evidence$metric,
    ci_lower = rep(NA_real_, nrow(evidence)),
    ci_upper = rep(NA_real_, nrow(evidence)),
    n_success = rep(0L, nrow(evidence))
  )

  status <- function(available, min_success, reason) {
    tibble::tibble(
      method = "bootstrap",
      level = level,
      requested_draws = R,
      min_successful_draws = as.integer(min_success),
      available = isTRUE(available),
      seed = if (is.null(seed)) NA_integer_ else as.integer(seed),
      reason = reason
    )
  }

  draws <- tryCatch(
    lavaan::bootstrapLavaan(
      fit_info$fit,
      R = R,
      type = "ordinary",
      FUN = nomo_reliability_boot_stat,
      parallel = "no",
      iseed = seed,
      expected_keys = expected_keys,
      construct_names = fit_info$latent_names,
      type_context = type_context,
      obs.var = obs.var,
      ordinal_scale = ordinal_scale,
      include_alpha = include_alpha
    ),
    error = function(e) e
  )

  if (inherits(draws, "error")) {
    return(list(
      intervals = empty_intervals,
      status = status(FALSE, 0L, paste0("Bootstrap failed: ", conditionMessage(draws)))
    ))
  }

  draws <- as.matrix(draws)
  if (!nrow(draws) || ncol(draws) != length(expected_keys)) {
    return(list(
      intervals = empty_intervals,
      status = status(
        FALSE, 0L,
        "Bootstrap draws could not be aligned with the reported reliability coefficients."
      )
    ))
  }

  if (is.null(colnames(draws)) || any(!nzchar(colnames(draws)))) {
    colnames(draws) <- expected_keys
  }

  alpha <- (1 - level) / 2
  probs <- c(alpha, 1 - alpha)

  vals_by_key <- lapply(expected_keys, function(key) {
    j <- match(key, colnames(draws))
    vals <- if (is.na(j)) numeric() else draws[, j]
    vals <- vals[is.finite(vals)]
    if (length(vals) >= 10L) {
      q <- stats::quantile(vals, probs = probs, names = FALSE, type = 7)
      c(lo = q[[1L]], hi = q[[2L]], n = length(vals))
    } else {
      c(lo = NA_real_, hi = NA_real_, n = length(vals))
    }
  })
  mat <- do.call(rbind, vals_by_key)

  intervals <- tibble::tibble(
    construct = evidence$construct,
    block = evidence$block,
    metric = evidence$metric,
    ci_lower = as.numeric(mat[, "lo"]),
    ci_upper = as.numeric(mat[, "hi"]),
    n_success = as.integer(mat[, "n"])
  )

  min_success <- if (nrow(intervals)) min(intervals$n_success) else 0L
  available <- nrow(intervals) > 0L &&
    all(is.finite(intervals$ci_lower)) &&
    all(is.finite(intervals$ci_upper))

  reason <- if (available) {
    if (min_success < ceiling(0.8 * R)) {
      paste0(
        "Intervals were computed, but at least one coefficient used only ",
        min_success, " of ", R, " finite bootstrap draws."
      )
    } else ""
  } else {
    paste0(
      "At least one coefficient had too few finite bootstrap draws; minimum successful draws = ",
      min_success, " of ", R, "."
    )
  }

  list(
    intervals = intervals,
    status = status(available, min_success, reason)
  )
}
