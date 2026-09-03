#' Evaluate evidence about the number of latent factors
#'
#' `nomo_factors()` combines multiple pieces of factor-retention evidence rather
#' than treating any single rule as definitive. The primary retention evidence is
#' a common-factor parallel analysis, complemented by Velicer's original and
#' revised MAP criteria and, under the default `criterion_set = "core"`, the
#' empirical Kaiser criterion (EKC). Extended criterion sets can add NEST, Hull
#' (CAF), comparison data, and the legacy Kaiser-Guttman rule when their
#' assumptions are compatible with the analyzed data. Scree information,
#' Kaiser-Meyer-Olkin (KMO) sampling adequacy, and Bartlett's test are returned
#' as supporting diagnostics.
#'
#' Correlation choice is explicit and visible. With `correlation = "auto"`,
#' continuous indicators use Pearson correlations, all-binary indicators use
#' tetrachoric correlations, ordinal/binary indicators use polychoric
#' correlations, and genuinely mixed indicator sets use mixed correlations.
#' Numeric variables with a small number of integer-like response values are
#' treated conservatively as continuous unless the user explicitly overrides
#' their modeling type through `types`.
#'
#' The function does not claim that a scale "has exactly" a particular number of
#' factors. It reports which factor counts deserve investigation and records
#' disagreements among retention methods.
#'
#' @param data A data frame containing candidate items.
#' @param items Optional character vector identifying item columns. If `NULL`,
#'   all columns are treated as candidate items.
#' @param correlation Correlation strategy: `"auto"`, `"pearson"`,
#'   `"polychoric"`, `"tetrachoric"`, or `"mixed"`.
#' @param types Optional named character vector overriding modeling types for
#'   selected items. Allowed values are `"continuous"`, `"ordinal"`, and
#'   `"binary"`. For example, `c(item1 = "ordinal", item2 = "ordinal")`.
#' @param missing Missing-data handling for correlation estimation. `"pairwise"`
#'   uses pairwise-complete observations; `"complete"` restricts the analysis to
#'   cases complete on all selected items.
#' @param criterion_set Retention-criterion bundle. `"minimal"` uses parallel
#'   analysis plus original MAP; `"core"` (default) adds revised MAP and EKC;
#'   `"extended"` adds NEST and Hull where supported; `"all"` additionally
#'   requests comparison data and the legacy Kaiser-Guttman rule. Criteria whose
#'   assumptions are not compatible with the current data are explicitly marked
#'   as skipped rather than silently substituted.
#' @param parallel_rule Parallel-analysis decision rule: `"percentile"`
#'   (default), `"mean"`, or `"crawford"`. All three rules are computed from
#'   the same null simulations and retained in the result as sensitivity evidence.
#' @param n_iter Number of null-data iterations used for parallel analysis. If
#'   `NULL`, the value in `guidance$factor_parallel_iterations` is used.
#' @param quantile Quantile of null eigenvalues used as the parallel-analysis
#'   reference. If `NULL`, the value in
#'   `guidance$factor_parallel_quantile` is used.
#' @param max_factors Maximum number of factors/components evaluated for MAP. If
#'   `NULL`, up to 10 or `p - 1`, whichever is smaller, are evaluated.
#' @param seed Integer seed for the null-data simulation. The caller's random
#'   number state is restored before return.
#' @param fm Common-factor extraction method passed to [psych::fa()] when
#'   obtaining factor eigenvalues. The default is `"minres"`.
#' @param smooth Logical. If `FALSE` (default), a non-positive-definite observed
#'   correlation matrix blocks factor-retention analysis. If `TRUE`, smoothing
#'   is explicit, recorded, and performed with [psych::cor.smooth()].
#' @param guidance Guidance settings from [nomo_defaults()].
#'
#' @return An object of class `nomo_factors` containing the analyzed correlation
#'   matrix, item modeling types, convenience aliases `correlation` and
#'   `modeling_types`, KMO and Bartlett diagnostics, parallel-analysis results,
#'   MAP results, criterion availability/status, method- and family-level concordance,
#'   scree information, a cautious retention synthesis, and a decision log.
#'
#' @examples
#' set.seed(42)
#' f <- rnorm(150)
#' dat <- data.frame(
#'   i1 = 0.8 * f + rnorm(150, sd = 0.6),
#'   i2 = 0.8 * f + rnorm(150, sd = 0.6),
#'   i3 = 0.7 * f + rnorm(150, sd = 0.7),
#'   i4 = 0.7 * f + rnorm(150, sd = 0.7)
#' )
#'
#' fac <- nomo_factors(dat, n_iter = 10, seed = 2026)
#' fac
#' summary(fac)
#'
#' @export
nomo_factors <- function(data,
                         items = NULL,
                         correlation = c(
                           "auto",
                           "pearson",
                           "polychoric",
                           "tetrachoric",
                           "mixed"
                         ),
                         types = NULL,
                         missing = c("pairwise", "complete"),
                         criterion_set = c("core", "minimal", "extended", "all"),
                         parallel_rule = c("percentile", "mean", "crawford"),
                         n_iter = NULL,
                         quantile = NULL,
                         max_factors = NULL,
                         seed = 1234L,
                         fm = "minres",
                         smooth = FALSE,
                         guidance = nomo_defaults()) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("`data` must contain at least one row.", call. = FALSE)
  }
  if (ncol(data) == 0L) {
    stop("`data` must contain at least one column.", call. = FALSE)
  }
  if (!is.list(guidance)) {
    stop(
      "`guidance` must be a list, typically returned by `nomo_defaults()`.",
      call. = FALSE
    )
  }

  correlation <- match.arg(correlation)
  missing <- match.arg(missing)
  criterion_set <- match.arg(criterion_set)
  parallel_rule <- match.arg(parallel_rule)

  if (is.null(items)) {
    items <- names(data)
  } else {
    if (!is.character(items) || length(items) == 0L) {
      stop(
        "`items` must be `NULL` or a non-empty character vector.",
        call. = FALSE
      )
    }
    if (anyNA(items) || any(items == "")) {
      stop("`items` cannot contain missing or empty names.", call. = FALSE)
    }
    if (anyDuplicated(items)) {
      stop("`items` must not contain duplicate names.", call. = FALSE)
    }
    missing_items <- setdiff(items, names(data))
    if (length(missing_items) > 0L) {
      stop(
        sprintf(
          "Unknown item column%s: %s.",
          if (length(missing_items) == 1L) "" else "s",
          paste(missing_items, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  if (length(items) < 3L) {
    stop(
      "`nomo_factors()` requires at least three candidate items.",
      call. = FALSE
    )
  }

  if (is.null(n_iter)) {
    n_iter <- nomo_null_default(guidance$factor_parallel_iterations, 100L)
  }
  if (
    length(n_iter) != 1L ||
      is.na(n_iter) ||
      !is.numeric(n_iter) ||
      n_iter < 10 ||
      abs(n_iter - round(n_iter)) > sqrt(.Machine$double.eps)
  ) {
    stop("`n_iter` must be a single integer of at least 10.", call. = FALSE)
  }
  n_iter <- as.integer(n_iter)

  if (is.null(quantile)) {
    quantile <- nomo_null_default(guidance$factor_parallel_quantile, 0.95)
  }
  if (
    length(quantile) != 1L ||
      is.na(quantile) ||
      !is.numeric(quantile) ||
      quantile <= 0 ||
      quantile >= 1
  ) {
    stop("`quantile` must be a single number strictly between 0 and 1.", call. = FALSE)
  }

  if (
    length(seed) != 1L ||
      is.na(seed) ||
      !is.numeric(seed) ||
      abs(seed - round(seed)) > sqrt(.Machine$double.eps)
  ) {
    stop("`seed` must be a single integer.", call. = FALSE)
  }
  seed <- as.integer(seed)

  if (!is.character(fm) || length(fm) != 1L || is.na(fm) || fm == "") {
    stop("`fm` must be a single non-empty character value.", call. = FALSE)
  }
  if (!is.logical(smooth) || length(smooth) != 1L || is.na(smooth)) {
    stop("`smooth` must be `TRUE` or `FALSE`.", call. = FALSE)
  }

  selected <- data[items]

  screen_types <- vapply(
    selected,
    nomo_screen_item_type,
    character(1)
  )

  item_types <- nomo_factors_model_types(
    selected = selected,
    items = items,
    screen_types = screen_types,
    types = types
  )

  hard_bad <- vapply(
    selected,
    function(x) {
      observed <- x[!is.na(x)]
      length(observed) == 0L || length(unique(observed)) < 2L
    },
    logical(1)
  )
  if (any(hard_bad)) {
    stop(
      sprintf(
        "Factor-retention analysis requires nonconstant observed data. Inspect: %s.",
        paste(items[hard_bad], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  analysis_data <- nomo_factors_numeric_data(
    selected = selected,
    item_types = item_types
  )

  if (missing == "complete") {
    complete <- stats::complete.cases(analysis_data)
    analysis_data <- analysis_data[complete, , drop = FALSE]
    if (nrow(analysis_data) < 3L) {
      stop(
        "Fewer than three complete cases remain after `missing = \"complete\"`.",
        call. = FALSE
      )
    }
  }

  pairwise_n <- nomo_factors_pairwise_n(analysis_data)
  off_diag <- pairwise_n[row(pairwise_n) != col(pairwise_n)]
  min_pairwise_n <- if (length(off_diag) > 0L) min(off_diag) else nrow(analysis_data)

  if (min_pairwise_n < 3L) {
    stop(
      "At least one item pair has fewer than three jointly observed cases.",
      call. = FALSE
    )
  }

  method <- nomo_factors_choose_correlation(
    requested = correlation,
    model_types = item_types$model_type
  )

  corr <- nomo_factors_correlation(
    x = analysis_data,
    model_types = item_types$model_type,
    method = method,
    use = if (missing == "pairwise") "pairwise" else "complete"
  )

  if (any(!is.finite(corr))) {
    stop(
      paste(
        "The estimated correlation matrix contains non-finite values.",
        "Inspect sparse categories, missingness, constant pairwise subsets,",
        "or coding before factor-retention analysis."
      ),
      call. = FALSE
    )
  }

  original_min_eigen <- min(
    eigen(corr, symmetric = TRUE, only.values = TRUE)$values
  )
  smoothed <- FALSE

  if (original_min_eigen <= 1e-08) {
    if (!smooth) {
      stop(
        paste(
          "The observed correlation matrix is not positive definite.",
          "Inspect redundant items, sparse categories, missingness, or coding.",
          "If smoothing is substantively justified, rerun with `smooth = TRUE`",
          "so that the intervention is explicit and recorded."
        ),
        call. = FALSE
      )
    }
    corr <- psych::cor.smooth(corr)
    smoothed <- TRUE
  }

  post_min_eigen <- min(
    eigen(corr, symmetric = TRUE, only.values = TRUE)$values
  )

  kmo <- nomo_factors_kmo(corr, items)

  any_missing <- anyNA(analysis_data)
  bartlett <- nomo_factors_bartlett(
    corr = corr,
    n = nrow(analysis_data),
    available = missing == "complete" || !any_missing
  )

  if (is.null(max_factors)) {
    max_factors <- min(10L, ncol(analysis_data) - 1L)
  }
  if (
    length(max_factors) != 1L ||
      is.na(max_factors) ||
      !is.numeric(max_factors) ||
      max_factors < 1 ||
      abs(max_factors - round(max_factors)) > sqrt(.Machine$double.eps)
  ) {
    stop("`max_factors` must be a positive integer.", call. = FALSE)
  }
  max_factors <- min(as.integer(max_factors), ncol(analysis_data) - 1L)

  observed_fa <- nomo_factors_factor_eigenvalues(corr, fm = fm)

  pa <- nomo_factors_parallel(
    x = analysis_data,
    model_types = item_types$model_type,
    method = method,
    use = if (missing == "pairwise") "pairwise" else "complete",
    observed = observed_fa,
    n_iter = n_iter,
    quantile = quantile,
    parallel_rule = parallel_rule,
    seed = seed,
    fm = fm
  )

  map <- nomo_factors_map(
    corr = corr,
    max_factors = max_factors
  )

  component_values <- eigen(
    corr,
    symmetric = TRUE,
    only.values = TRUE
  )$values

  scree <- tibble::tibble(
    index = seq_along(component_values),
    component_eigenvalue = as.numeric(component_values),
    factor_eigenvalue = as.numeric(observed_fa)
  )

  common_n_available <- missing == "complete" || !anyNA(analysis_data)

  criteria <- nomo_factors_build_criteria(
    criterion_set = criterion_set,
    pa = pa,
    map = map,
    corr = corr,
    analysis_data = analysis_data,
    item_types = item_types,
    correlation_method = method,
    common_n_available = common_n_available,
    max_factors = max_factors,
    n_iter = n_iter,
    quantile = quantile,
    seed = seed,
    guidance = guidance,
    component_values = component_values
  )

  synthesis <- nomo_factors_synthesis(
    evidence = criteria$evidence,
    parallel_n = pa$n_factors,
    status = criteria$status
  )

  decision_log <- nomo_factors_log(
    item_types = item_types,
    requested_correlation = correlation,
    correlation_method = method,
    missing = missing,
    min_pairwise_n = min_pairwise_n,
    smoothed = smoothed,
    original_min_eigen = original_min_eigen,
    kmo = kmo,
    bartlett = bartlett,
    pa = pa,
    map = map,
    criteria = criteria,
    synthesis = synthesis,
    guidance = guidance
  )

  out <- list(
    call = match.call(),
    n_cases = nrow(analysis_data),
    n_items = length(items),
    items = items,
    item_types = item_types,
    modeling_types = item_types,
    correlation_method = method,
    correlation = method,
    correlation_requested = correlation,
    correlation_matrix = corr,
    pairwise_n = pairwise_n,
    missing = missing,
    smoothed = smoothed,
    original_min_eigen = original_min_eigen,
    min_eigen = post_min_eigen,
    kmo = kmo,
    bartlett = bartlett,
    parallel = pa,
    map = map,
    scree = scree,
    criterion_set = criterion_set,
    criterion_status = criteria$status,
    criterion_details = criteria$details,
    evidence = criteria$evidence,
    family_evidence = synthesis$family_evidence,
    concordance = synthesis$concordance,
    family_concordance = synthesis$family_concordance,
    method_concordance = synthesis$method_concordance,
    plausible_factors = synthesis$plausible_factors,
    recommendation = synthesis$text,
    decision_log = decision_log,
    fm = fm,
    seed = seed,
    n_iter = n_iter,
    quantile = quantile,
    parallel_rule = parallel_rule,
    guidance = guidance
  )

  class(out) <- c("nomo_factors", "list")
  out
}


nomo_factors_model_types <- function(selected,
                                     items,
                                     screen_types,
                                     types = NULL) {
  model_type <- vapply(
    screen_types,
    function(x) {
      switch(
        x,
        numeric_continuous = "continuous",
        numeric_discrete = "continuous",
        ordered = "ordinal",
        binary = "binary",
        NA_character_
      )
    },
    character(1)
  )

  source <- ifelse(
    screen_types == "numeric_discrete",
    "conservative_numeric_default",
    "inferred_from_storage"
  )

  invalid <- is.na(model_type)
  if (any(invalid)) {
    stop(
      paste0(
        "Some selected columns do not have a defensible default factor-modeling ",
        "type. Exclude, intentionally recode, or override appropriate columns: ",
        paste(items[invalid], collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  if (!is.null(types)) {
    if (
      !is.character(types) ||
        is.null(names(types)) ||
        any(names(types) == "") ||
        anyDuplicated(names(types))
    ) {
      stop(
        "`types` must be a named character vector with unique item names.",
        call. = FALSE
      )
    }

    bad_names <- setdiff(names(types), items)
    if (length(bad_names) > 0L) {
      stop(
        sprintf(
          "`types` contains item names not selected for analysis: %s.",
          paste(bad_names, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    allowed <- c("continuous", "ordinal", "binary")
    bad_values <- setdiff(unique(types), allowed)
    if (length(bad_values) > 0L) {
      stop(
        sprintf(
          "Unsupported modeling type%s: %s. Use continuous, ordinal, or binary.",
          if (length(bad_values) == 1L) "" else "s",
          paste(bad_values, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    for (item in names(types)) {
      idx <- match(item, items)
      model_type[[idx]] <- types[[item]]
      source[[idx]] <- "user_override"
    }
  }

  for (i in seq_along(items)) {
    x <- selected[[i]]
    type <- model_type[[i]]
    observed <- x[!is.na(x)]

    if (type == "continuous" && !is.numeric(x)) {
      stop(
        sprintf(
          "`%s` is marked continuous but is not stored numerically.",
          items[[i]]
        ),
        call. = FALSE
      )
    }

    if (type == "ordinal" && !(is.numeric(x) || is.factor(x) || is.logical(x))) {
      stop(
        sprintf(
          "`%s` is marked ordinal but cannot be converted to ordered scores safely.",
          items[[i]]
        ),
        call. = FALSE
      )
    }

    if (type == "binary" && length(unique(observed)) != 2L) {
      stop(
        sprintf(
          "`%s` is marked binary but does not have exactly two observed values.",
          items[[i]]
        ),
        call. = FALSE
      )
    }
  }

  tibble::tibble(
    item = items,
    screen_type = unname(screen_types),
    model_type = unname(model_type),
    source = unname(source)
  )
}


nomo_factors_numeric_data <- function(selected, item_types) {
  out <- lapply(seq_len(ncol(selected)), function(i) {
    x <- selected[[i]]
    type <- item_types$model_type[[i]]

    if (type == "continuous") {
      return(as.numeric(x))
    }

    if (type == "ordinal") {
      return(as.numeric(x))
    }

    if (type == "binary") {
      if (is.factor(x) || is.logical(x)) {
        return(as.numeric(x))
      }

      observed <- sort(unique(x[!is.na(x)]))
      ans <- rep(NA_real_, length(x))
      ans[!is.na(x)] <- match(x[!is.na(x)], observed) - 1L
      return(ans)
    }

    stop("Unsupported modeling type reached numeric conversion.", call. = FALSE)
  })

  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- names(selected)
  out
}


nomo_factors_choose_correlation <- function(requested, model_types) {
  if (requested != "auto") {
    if (requested == "tetrachoric" && !all(model_types == "binary")) {
      stop(
        "`correlation = \"tetrachoric\"` requires all selected items to be binary.",
        call. = FALSE
      )
    }
    if (
      requested == "polychoric" &&
        !all(model_types %in% c("ordinal", "binary"))
    ) {
      stop(
        paste(
          "`correlation = \"polychoric\"` requires ordinal/binary indicators.",
          "Use `types` if numeric Likert items should be modeled as ordinal."
        ),
        call. = FALSE
      )
    }
    return(requested)
  }

  unique_types <- unique(model_types)

  if (all(unique_types == "continuous")) {
    return("pearson")
  }
  if (all(unique_types == "binary")) {
    return("tetrachoric")
  }
  if (all(unique_types %in% c("ordinal", "binary"))) {
    return("polychoric")
  }

  "mixed"
}


nomo_factors_correlation <- function(x, model_types, method, use) {
  use_cor <- if (use == "pairwise") {
    "pairwise.complete.obs"
  } else {
    "complete.obs"
  }

  result <- tryCatch(
    {
      if (method == "pearson") {
        stats::cor(x, use = use_cor)
      } else if (method == "polychoric") {
        suppressWarnings(
          suppressMessages(
            psych::polychoric(
              x,
              correct = 0.5,
              smooth = FALSE,
              global = FALSE
            )$rho
          )
        )
      } else if (method == "tetrachoric") {
        suppressWarnings(
          suppressMessages(
            psych::tetrachoric(
              x,
              correct = 0.5,
              smooth = FALSE
            )$rho
          )
        )
      } else if (method == "mixed") {
        c_idx <- which(model_types == "continuous")
        p_idx <- which(model_types == "ordinal")
        d_idx <- which(model_types == "binary")

        suppressWarnings(
          suppressMessages(
            psych::mixedCor(
              data = x,
              c = if (length(c_idx)) c_idx else NULL,
              p = if (length(p_idx)) p_idx else NULL,
              d = if (length(d_idx)) d_idx else NULL,
              smooth = FALSE,
              correct = 0.5,
              global = FALSE,
              use = use_cor
            )$rho
          )
        )
      } else {
        stop("Unknown correlation method.", call. = FALSE)
      }
    },
    error = function(e) {
      stop(
        sprintf(
          "Could not estimate the %s correlation matrix: %s",
          method,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  result <- as.matrix(result)
  dimnames(result) <- list(names(x), names(x))
  result
}


nomo_factors_pairwise_n <- function(x) {
  observed <- !is.na(as.matrix(x))
  out <- crossprod(observed)
  storage.mode(out) <- "integer"
  dimnames(out) <- list(names(x), names(x))
  out
}


nomo_factors_factor_eigenvalues <- function(corr, fm) {
  fit <- tryCatch(
    suppressWarnings(
      suppressMessages(
        psych::fa(
          corr,
          nfactors = 1,
          rotate = "none",
          fm = fm,
          warnings = FALSE
        )
      )
    ),
    error = function(e) {
      stop(
        sprintf(
          "Could not obtain common-factor eigenvalues with `fm = \"%s\"`: %s",
          fm,
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  as.numeric(fit$values)
}


nomo_factors_parallel <- function(x,
                                  model_types,
                                  method,
                                  use,
                                  observed,
                                  n_iter,
                                  quantile,
                                  parallel_rule,
                                  seed,
                                  fm) {
  old_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit(
    {
      if (old_exists) {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  set.seed(seed)

  p <- ncol(x)
  random_values <- matrix(NA_real_, nrow = n_iter, ncol = p)
  smoothed_null <- 0L
  attempts <- 0L
  accepted <- 0L
  max_attempts <- max(2L * n_iter, n_iter + 10L)

  while (accepted < n_iter && attempts < max_attempts) {
    attempts <- attempts + 1L

    permuted <- as.data.frame(
      lapply(x, function(v) sample(v, length(v), replace = FALSE)),
      stringsAsFactors = FALSE
    )
    names(permuted) <- names(x)

    r_null <- tryCatch(
      nomo_factors_correlation(
        x = permuted,
        model_types = model_types,
        method = method,
        use = use
      ),
      error = function(e) NULL
    )

    if (is.null(r_null) || any(!is.finite(r_null))) {
      next
    }

    eig <- tryCatch(
      eigen(r_null, symmetric = TRUE, only.values = TRUE)$values,
      error = function(e) NULL
    )
    if (is.null(eig)) {
      next
    }

    if (min(eig) <= 1e-08) {
      r_null <- tryCatch(psych::cor.smooth(r_null), error = function(e) NULL)
      if (is.null(r_null)) {
        next
      }
      smoothed_null <- smoothed_null + 1L
    }

    values <- tryCatch(
      nomo_factors_factor_eigenvalues(r_null, fm = fm),
      error = function(e) NULL
    )
    if (is.null(values) || length(values) != p || any(!is.finite(values))) {
      next
    }

    accepted <- accepted + 1L
    random_values[accepted, ] <- values
  }

  if (accepted < n_iter && accepted > 0L) {
    random_values <- random_values[seq_len(accepted), , drop = FALSE]
  }

  minimum_valid <- max(10L, ceiling(0.80 * n_iter))
  if (accepted < minimum_valid) {
    stop(
      sprintf(
        paste(
          "Parallel analysis produced only %d usable null iterations out of %d.",
          "Inspect sparse categories or correlation instability."
        ),
        accepted,
        n_iter
      ),
      call. = FALSE
    )
  }

  reference_mean <- colMeans(random_values, na.rm = TRUE)
  reference_percentile <- apply(
    random_values,
    2,
    stats::quantile,
    probs = quantile,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )
  reference_crawford <- reference_mean
  reference_crawford[[1L]] <- reference_percentile[[1L]]

  references <- list(
    mean = as.numeric(reference_mean),
    percentile = as.numeric(reference_percentile),
    crawford = as.numeric(reference_crawford)
  )

  stopping_count <- function(reference) {
    exceeds <- observed > reference
    first_failure <- which(!exceeds)[1L]
    if (is.na(first_failure)) length(exceeds) else first_failure - 1L
  }

  counts <- vapply(references, stopping_count, integer(1))
  selected_reference <- references[[parallel_rule]]
  n_factors <- counts[[parallel_rule]]
  exceeds_reference <- observed > selected_reference
  retained <- seq_along(exceeds_reference) <= n_factors

  table <- tibble::tibble(
    factor = seq_along(observed),
    observed_eigenvalue = as.numeric(observed),
    random_mean = as.numeric(reference_mean),
    random_percentile = as.numeric(reference_percentile),
    random_crawford = as.numeric(reference_crawford),
    random_reference = as.numeric(selected_reference),
    exceeds_reference = as.logical(exceeds_reference),
    retained = as.logical(retained)
  )

  sensitivity <- tibble::tibble(
    rule = c("percentile", "mean", "crawford"),
    n_factors = as.integer(counts[c("percentile", "mean", "crawford")]),
    selected = c("percentile", "mean", "crawford") == parallel_rule
  )

  list(
    n_factors = as.integer(n_factors),
    rule = parallel_rule,
    sensitivity = sensitivity,
    table = table,
    random_eigenvalues = random_values,
    n_requested = as.integer(n_iter),
    n_valid = as.integer(accepted),
    n_attempted = as.integer(attempts),
    n_smoothed_null = as.integer(smoothed_null),
    quantile = as.numeric(quantile),
    seed = as.integer(seed)
  )
}


nomo_factors_map <- function(corr, max_factors) {
  # Velicer MAP includes m = 0. TR2 is the original average squared partial
  # correlation criterion; TR4 is the revised fourth-power criterion. Both are
  # evaluated over the same sequence of residual partial-correlation matrices.
  p <- ncol(corr)
  max_factors <- min(as.integer(max_factors), p - 1L)

  eig <- eigen(corr, symmetric = TRUE)
  values <- pmax(eig$values, 0)
  loadings <- sweep(eig$vectors, 2, sqrt(values), `*`)

  map_values <- function(partial_corr) {
    # TR2 is algebraically equal to (tr(M^2) - p) / p(p - 1), but
    # calculating it directly from the off-diagonal squared partial
    # correlations avoids subtractive cancellation when correlations are tiny.
    # TR4 is genuinely based on the fourth *matrix* power and is therefore
    # retained in trace form rather than as element-wise fourth powers.
    off_diagonal <- partial_corr[row(partial_corr) != col(partial_corr)]
    m2 <- partial_corr %*% partial_corr
    m4 <- m2 %*% m2
    c(
      tr2 = mean(off_diagonal^2),
      tr4 = (sum(diag(m4)) - p) / (p * (p - 1))
    )
  }

  factor_grid <- 0:max_factors
  criteria <- matrix(
    NA_real_,
    nrow = length(factor_grid),
    ncol = 2L,
    dimnames = list(NULL, c("tr2", "tr4"))
  )
  criteria[1L, ] <- map_values(corr)
  m_last <- 0L

  if (max_factors >= 1L) {
    for (m in seq_len(max_factors)) {
      a_m <- loadings[, seq_len(m), drop = FALSE]
      residual_cov <- corr - tcrossprod(a_m)
      residual_var <- diag(residual_cov)

      if (any(!is.finite(residual_var)) || any(residual_var <= 1e-08)) {
        break
      }

      inv_sd <- 1 / sqrt(residual_var)
      partial_corr <- residual_cov * tcrossprod(inv_sd)
      diag(partial_corr) <- 1
      criteria[m + 1L, ] <- map_values(partial_corr)
      m_last <- m
    }
  }

  usable_tr2 <- is.finite(criteria[, "tr2"])
  usable_tr4 <- is.finite(criteria[, "tr4"])
  if (!any(usable_tr2) || !any(usable_tr4)) {
    stop("Velicer MAP did not return usable values.", call. = FALSE)
  }

  best_tr2 <- factor_grid[usable_tr2][which.min(criteria[usable_tr2, "tr2"])]
  best_tr4 <- factor_grid[usable_tr4][which.min(criteria[usable_tr4, "tr4"])]

  list(
    n_factors = as.integer(best_tr2),
    n_factors_original = as.integer(best_tr2),
    n_factors_revised = as.integer(best_tr4),
    m_last = as.integer(m_last),
    truncated = m_last < min(max_factors, p - 2L),
    table = tibble::tibble(
      n_factors = as.integer(factor_grid),
      # Backward-compatible aliases from the M2A object schema. `map` and
      # `minimum` always refer to the original TR2 criterion.
      map = as.numeric(criteria[, "tr2"]),
      map_original = as.numeric(criteria[, "tr2"]),
      map_revised = as.numeric(criteria[, "tr4"]),
      minimum = factor_grid == best_tr2,
      minimum_original = factor_grid == best_tr2,
      minimum_revised = factor_grid == best_tr4
    )
  )
}

nomo_factors_kmo <- function(corr, items) {
  # KMO requires an invertible correlation matrix. Avoid calling psych::KMO()
  # when the matrix is singular or numerically near-singular because the engine
  # can emit low-level LAPACK/inversion chatter before nomologR can recover.
  # Returning KMO as unavailable is more transparent than silently repairing a
  # matrix for this supporting diagnostic.
  bad_condition <- any(!is.finite(corr))
  if (!bad_condition) {
    reciprocal_condition <- tryCatch(
      rcond(corr),
      error = function(e) 0
    )
    rank_corr <- tryCatch(
      qr(corr, tol = 1e-10)$rank,
      error = function(e) 0L
    )
    bad_condition <- !is.finite(reciprocal_condition) ||
      reciprocal_condition < 1e-10 ||
      rank_corr < ncol(corr)
  }

  if (bad_condition) {
    return(
      list(
        available = FALSE,
        overall = NA_real_,
        item = tibble::tibble(
          item = items,
          msa = NA_real_
        )
      )
    )
  }

  # psych::KMO() can still print inversion diagnostics for edge-case matrices
  # even when the failure is handled. Capture ordinary output and suppress
  # warnings/messages so nomologR reports through its structured result.
  result <- NULL
  invisible(
    utils::capture.output(
      result <- tryCatch(
        suppressMessages(suppressWarnings(psych::KMO(corr))),
        error = function(e) NULL
      ),
      type = "output"
    )
  )

  if (is.null(result)) {
    return(
      list(
        available = FALSE,
        overall = NA_real_,
        item = tibble::tibble(
          item = items,
          msa = NA_real_
        )
      )
    )
  }

  list(
    available = TRUE,
    overall = as.numeric(result$MSA),
    item = tibble::tibble(
      item = items,
      msa = as.numeric(result$MSAi)
    )
  )
}


nomo_factors_bartlett <- function(corr, n, available) {
  if (!available) {
    return(
      list(
        available = FALSE,
        chisq = NA_real_,
        df = NA_real_,
        p_value = NA_real_,
        n = NA_integer_,
        reason = paste(
          "Bartlett's test was not computed because pairwise missing-data",
          "handling does not provide one common sample size for the full matrix."
        )
      )
    )
  }

  result <- tryCatch(
    suppressWarnings(psych::cortest.bartlett(corr, n = n)),
    error = function(e) NULL
  )

  if (is.null(result)) {
    return(
      list(
        available = FALSE,
        chisq = NA_real_,
        df = NA_real_,
        p_value = NA_real_,
        n = as.integer(n),
        reason = "Bartlett's test could not be computed from this matrix."
      )
    )
  }

  list(
    available = TRUE,
    chisq = as.numeric(result$chisq),
    df = as.numeric(result$df),
    p_value = as.numeric(result$p.value),
    n = as.integer(n),
    reason = ""
  )
}


nomo_factors_log <- function(item_types,
                             requested_correlation,
                             correlation_method,
                             missing,
                             min_pairwise_n,
                             smoothed,
                             original_min_eigen,
                             kmo,
                             bartlett,
                             pa,
                             map,
                             criteria,
                             synthesis,
                             guidance) {
  log <- nomo_log_new()

  log <- nomo_log_add(
    log,
    stage = "factors",
    object = "correlation_matrix",
    metric = "correlation_method",
    value = NA_real_,
    reference = "Indicator type should inform the correlation model",
    severity = "info",
    observation = sprintf(
      "Correlation method: %s%s.",
      correlation_method,
      if (requested_correlation == "auto") " (selected automatically)" else ""
    ),
    recommendation = paste(
      "Confirm that the modeling types reflect the intended measurement level",
      "rather than relying on storage format alone."
    )
  )

  ambiguous <- item_types$item[
    item_types$source == "conservative_numeric_default"
  ]
  if (length(ambiguous) > 0L) {
    log <- nomo_log_add(
      log,
      stage = "factors",
      object = paste(ambiguous, collapse = ", "),
      metric = "numeric_discrete_assumption",
      value = length(ambiguous),
      reference = "Numeric storage does not establish ordinal measurement",
      severity = "review",
      observation = sprintf(
        paste(
          "%d numeric-discrete item%s were conservatively modeled as continuous:",
          "%s."
        ),
        length(ambiguous),
        if (length(ambiguous) == 1L) "" else "s",
        paste(ambiguous, collapse = ", ")
      ),
      recommendation = paste(
        "If these are Likert/ordered indicators, rerun with `types` marking",
        "them as ordinal and compare the retention evidence."
      )
    )
  }

  if (missing == "pairwise") {
    log <- nomo_log_add(
      log,
      stage = "factors",
      object = "cases",
      metric = "pairwise_n",
      value = min_pairwise_n,
      reference = "Minimum pairwise N; pairwise deletion yields varying Ns",
      severity = "info",
      observation = sprintf(
        "Pairwise correlations used at least %d jointly observed cases per item pair.",
        min_pairwise_n
      ),
      recommendation = paste(
        "Inspect missingness before treating the resulting correlation matrix",
        "as equivalent to one estimated from a single complete sample."
      )
    )
  }

  small_ref <- nomo_null_default(guidance$factor_small_n_reference, 100L)
  if (min_pairwise_n < small_ref) {
    log <- nomo_log_add(
      log,
      stage = "factors",
      object = "sample",
      metric = "small_sample_prompt",
      value = min_pairwise_n,
      reference = sprintf(
        "%d is a teaching prompt, not a universal factor-analysis minimum",
        small_ref
      ),
      severity = "review",
      observation = sprintf(
        "The effective sample size is below the teaching reference of %d.",
        small_ref
      ),
      recommendation = paste(
        "Interpret retention evidence cautiously and consider stability across",
        "resamples or additional data; adequacy depends on communalities,",
        "loadings, item count, and model complexity."
      )
    )
  }

  if (smoothed) {
    log <- nomo_log_add(
      log,
      stage = "factors",
      object = "correlation_matrix",
      metric = "correlation_smoothing",
      value = original_min_eigen,
      reference = "Smoothing must be explicit; never silently repair the matrix",
      severity = "concern",
      observation = paste(
        "The observed correlation matrix was non-positive-definite and was",
        "smoothed because `smooth = TRUE`."
      ),
      recommendation = paste(
        "Treat factor-retention results as sensitivity evidence and investigate",
        "the source of non-positive-definiteness before substantive modeling."
      )
    )
  }

  if (isTRUE(kmo$available)) {
    review_ref <- nomo_null_default(guidance$factor_kmo_review_reference, 0.60)
    concern_ref <- nomo_null_default(guidance$factor_kmo_concern_reference, 0.50)
    severity <- if (kmo$overall < concern_ref) {
      "concern"
    } else if (kmo$overall < review_ref) {
      "review"
    } else {
      "info"
    }

    log <- nomo_log_add(
      log,
      stage = "factors",
      object = "correlation_matrix",
      metric = "kmo",
      value = kmo$overall,
      reference = sprintf(
        "%.2f/%.2f are teaching review/concern prompts, not universal laws",
        review_ref,
        concern_ref
      ),
      severity = severity,
      observation = sprintf("Overall KMO = %.3f.", kmo$overall),
      recommendation = paste(
        "Use KMO as supporting evidence about shared-factor structure and",
        "inspect item-level MSA values; do not use it to choose factor count."
      )
    )
  } else {
    log <- nomo_log_add(
      log,
      stage = "factors",
      object = "correlation_matrix",
      metric = "kmo",
      value = NA_real_,
      reference = "KMO could not be computed",
      severity = "review",
      observation = "KMO sampling adequacy was unavailable for this matrix.",
      recommendation = "Inspect singularity, redundancy, and correlation stability."
    )
  }

  if (isTRUE(bartlett$available)) {
    log <- nomo_log_add(
      log,
      stage = "factors",
      object = "correlation_matrix",
      metric = "bartlett",
      value = bartlett$p_value,
      reference = "Supporting test; strongly sample-size sensitive",
      severity = "info",
      observation = sprintf(
        "Bartlett's test: chi-square(%d) = %.2f, p = %s.",
        as.integer(bartlett$df),
        bartlett$chisq,
        format.pval(bartlett$p_value, digits = 3, eps = 0.001)
      ),
      recommendation = paste(
        "Treat this as evidence about whether the matrix differs from an",
        "identity matrix, not as evidence for a particular factor count."
      )
    )
  } else {
    log <- nomo_log_add(
      log,
      stage = "factors",
      object = "correlation_matrix",
      metric = "bartlett",
      value = NA_real_,
      reference = "Requires one common N for the tested matrix",
      severity = "info",
      observation = bartlett$reason,
      recommendation = paste(
        "If Bartlett's test is needed, consider rerunning with",
        "`missing = \"complete\"` and compare conclusions."
      )
    )
  }

  log <- nomo_log_add(
    log,
    stage = "factors",
    object = "retention",
    metric = "parallel_analysis",
    value = pa$n_factors,
    reference = sprintf(
      "Common-factor PA using the selected %s rule; %.0fth percentile also retained as sensitivity evidence",
      pa$rule,
      100 * pa$quantile
    ),
    severity = "info",
    observation = sprintf(
      "Parallel analysis (%s rule) suggests investigating %d factor%s.",
      pa$rule,
      pa$n_factors,
      if (pa$n_factors == 1L) "" else "s"
    ),
    recommendation = paste(
      "Use parallel analysis as primary retention evidence and inspect the",
      "mean/percentile/Crawford rule sensitivity before treating the count as stable."
    )
  )

  pa_unique <- length(unique(pa$sensitivity$n_factors))
  log <- nomo_log_add(
    log,
    stage = "factors",
    object = "retention",
    metric = "parallel_rule_sensitivity",
    value = pa_unique,
    reference = "Agreement across PA decision rules strengthens rule robustness",
    severity = if (pa_unique == 1L) "info" else "review",
    observation = paste0(
      "PA rule suggestions: ",
      paste(
        sprintf("%s=%d", pa$sensitivity$rule, pa$sensitivity$n_factors),
        collapse = "; "
      ),
      "."
    ),
    recommendation = if (pa_unique == 1L) {
      "The selected PA count is insensitive to the three reported decision rules."
    } else {
      paste(
        "Treat the PA count as rule-sensitive and compare the competing",
        "factor solutions rather than hiding the analytical choice."
      )
    }
  )

  log <- nomo_log_add(
    log,
    stage = "factors",
    object = "retention",
    metric = "map_original",
    value = map$n_factors_original,
    reference = "Velicer original MAP (TR2), including m = 0",
    severity = "info",
    observation = sprintf(
      "Original MAP (TR2) reaches its minimum at %d factor%s.",
      map$n_factors_original,
      if (map$n_factors_original == 1L) "" else "s"
    ),
    recommendation = "Treat original MAP as complementary retention evidence."
  )

  log <- nomo_log_add(
    log,
    stage = "factors",
    object = "retention",
    metric = "map_revised",
    value = map$n_factors_revised,
    reference = "Velicer revised MAP (TR4), including m = 0",
    severity = if (map$n_factors_revised == map$n_factors_original) "info" else "review",
    observation = sprintf(
      "Revised MAP (TR4) reaches its minimum at %d factor%s.",
      map$n_factors_revised,
      if (map$n_factors_revised == 1L) "" else "s"
    ),
    recommendation = paste(
      "Compare TR2 and TR4. Disagreement is sensitivity evidence, not a reason",
      "to select whichever count is more convenient."
    )
  )

  if (isTRUE(map$truncated)) {
    log <- nomo_log_add(
      log,
      stage = "factors",
      object = "retention",
      metric = "map_truncated",
      value = map$m_last,
      reference = "MAP should be interpreted over the factor counts it could evaluate",
      severity = "review",
      observation = sprintf(
        "MAP could be evaluated only through %d partialled component%s.",
        map$m_last,
        if (map$m_last == 1L) "" else "s"
      ),
      recommendation = paste(
        "Inspect correlation-matrix stability; the minima are over the evaluated",
        "range rather than the full requested grid."
      )
    )
  }

  if (is.data.frame(criteria$status) && nrow(criteria$status) > 0L) {
    extra <- criteria$status[!criteria$status$criterion %in% c("parallel", "map_original", "map_revised"), , drop = FALSE]
    if (nrow(extra) > 0L) {
      for (i in seq_len(nrow(extra))) {
        row_i <- extra[i, , drop = FALSE]
        if (identical(row_i$status[[1L]], "available")) {
          ev <- criteria$evidence[criteria$evidence$criterion == row_i$criterion[[1L]], , drop = FALSE]
          ekc_approx <- identical(row_i$criterion[[1L]], "ekc") &&
            !identical(correlation_method, "pearson")
          severity_i <- if (identical(ev$role[[1L]], "legacy") || ekc_approx) {
            "review"
          } else {
            "info"
          }
          recommendation_i <- if (identical(ev$role[[1L]], "legacy")) {
            "Use this legacy rule for historical context only, not as a recommended retention criterion."
          } else if (ekc_approx) {
            paste(
              "Treat EKC as sensitivity evidence here: its reference distribution is",
              "derived for product-moment correlations, so use with non-Pearson",
              "correlation matrices is approximate."
            )
          } else {
            "Treat this as one additional piece of retention evidence and inspect its assumptions."
          }

          log <- nomo_log_add(
            log,
            stage = "factors",
            object = "retention",
            metric = row_i$criterion[[1L]],
            value = ev$n_factors[[1L]],
            reference = ev$reference[[1L]],
            severity = severity_i,
            observation = sprintf(
              "%s suggests %d factor%s%s.",
              ev$method[[1L]],
              ev$n_factors[[1L]],
              if (ev$n_factors[[1L]] == 1L) "" else "s",
              if (ekc_approx) " (approximate under the current non-Pearson correlation model)" else ""
            ),
            recommendation = recommendation_i
          )
        } else {
          log <- nomo_log_add(
            log,
            stage = "factors",
            object = "retention",
            metric = paste0(row_i$criterion[[1L]], "_skipped"),
            value = NA_real_,
            reference = "Criterion compatibility is checked explicitly",
            severity = "info",
            observation = sprintf("%s was skipped. %s", row_i$method[[1L]], row_i$reason[[1L]]),
            recommendation = "Do not substitute a different correlation or missing-data strategy silently just to force this criterion to run."
          )
        }
      }
    }
  }

  log <- nomo_log_add(
    log,
    stage = "factors",
    object = "retention",
    metric = "retention_family_concordance",
    value = synthesis$support_for_primary,
    reference = "Convergence across criterion families strengthens a candidate; disagreement motivates sensitivity analysis",
    severity = if (synthesis$agreement == "convergent") "info" else "review",
    observation = synthesis$text,
    recommendation = paste(
      "Carry plausible neighboring solutions into EFA when retention evidence",
      "does not converge cleanly."
    )
  )

  log
}


nomo_null_default <- function(x, fallback) {
  if (is.null(x) || length(x) == 0L) fallback else x
}
