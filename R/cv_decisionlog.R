#' Initialize a decision log
#' @return tibble with columns step, object, metric, value, rule, status, suggestion
#' @export
cv_new_log <- function() {
  tibble::tibble(step = character(), object = character(), metric = character(),
                 value = numeric(), rule = character(), status = character(),
                 suggestion = character())
}

#' Append a decision record
#' @export
cv_log_add <- function(log, step, object, metric, value, rule, status, suggestion = "") {
  dplyr::bind_rows(log, tibble::tibble(
    step=step, object=object, metric=metric, value=value, rule=rule, status=status, suggestion=suggestion
  ))
}
