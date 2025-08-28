#' Orchestrate the full content-validity → measurement → nomological pipeline
#'
#' @param data A data.frame with item columns
#' @param scales A named list: scale_name -> character vector of item names
#' @param holdout Proportion to hold out for CFA
#' @param cutoffs Output of [cv_defaults()]
#' @param nomo_model Optional lavaan syntax for structural model
#' @param groups Optional factor for multi-group invariance
#' @export
cv_pipeline <- function(data, scales, holdout = 0.30, cutoffs = cv_defaults(),
                        nomo_model = NULL, groups = NULL) {
  stopifnot(is.list(scales), is.numeric(holdout), holdout >= 0, holdout < 1)
  set.seed(1234)
  n <- nrow(data); idx <- sample.int(n)
  split <- ceiling((1 - holdout) * n)
  train <- data[idx[1:split], , drop=FALSE]
  test  <- data[idx[(split+1):n], , drop=FALSE]

  log <- cv_new_log()

  # 1) Screen
  # TODO: call cv_screen_items()
  # 2) EFA per scale
  # TODO: call cv_efa()
  # 3) Build measurement model from EFA and run CFA
  # TODO: call cv_cfa()
  # 4) Reliability/validity
  # TODO: call cv_reliability(), cv_validity()
  # 5) Structural (nomological) if provided
  # TODO: call cv_nomological()
  # 6) Invariance if groups provided
  # TODO: call cv_invariance()
  # 7) Report
  # TODO: call cv_report()

  structure(list(train=train, test=test, decision_log = log, cutoffs = cutoffs),
            class = "nomologr_pipeline")
}
