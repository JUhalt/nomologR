# as.data.frame() for result objects ---------------------------------------------
#
# A result is a list that keeps the call that made it. Base R's method for a
# list hands each element to data.frame() as an argument, so the stored call is
# evaluated again. It returns another result, which is converted the same way,
# without end: as.data.frame() on a nomo_partial object stopped with "node
# stack overflow" after several hundred calls, and on a fitted object whose
# arguments are in scope it fitted the model again and again (#145).
#
# One function refuses the conversion and says where the tables are. It is
# registered in NAMESPACE as the as.data.frame() method of every class whose
# objects keep their call, and of the two summary classes that carry it along.
# Classes that keep no call, such as nomo_single_indicator and nomo_apa_table,
# have no method here and convert as base R converts a list. No class loses
# "list", so is.list() and inherits(x, "list") still hold for every result.
# nomo_partial has a method of its own, in R/nomo_partial.R, which returns its
# releases.
#
#' @rawNamespace S3method(as.data.frame, nomo_screen, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_factors, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_efa, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_cfa, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_esem, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_compare, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_split, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_reliability, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_retest, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_validity, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_method_variance, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_hierarchical, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_scores, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_missing, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_invariance, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_hypotheses, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_network, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_power, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, nomo_run, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, summary_nomo_scores, nomo_as_data_frame_refuse)
#' @rawNamespace S3method(as.data.frame, summary_nomo_power, nomo_as_data_frame_refuse)
nomo_as_data_frame_refuse <- function(x, row.names = NULL, optional = FALSE, ...) {
  # An object without a stored call converts as before: nomo_power_rmsea()
  # keeps none, though nomo_power_simulate(), of the same class, does.
  if (is.null(x[["call"]])) return(NextMethod())

  where <- if (inherits(x, "nomo_split")) {
    "Its samples are the data frames `x$calibration` and `x$validation`."
  } else if (startsWith(class(x)[[1L]], "summary_")) {
    "Use `nomo_table()` on the result it summarizes to get one of its tables."
  } else {
    "Use `nomo_table()` to get one of its tables."
  }
  stop(sprintf(
    "A `%s` object is not a table, so it cannot be converted to a data frame. %s",
    class(x)[[1L]], where
  ), call. = FALSE)
}
