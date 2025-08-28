#' Exploratory factor analysis helper
#' @param data data.frame of items for a scale
#' @param rotation default 'oblimin'
#' @param cutoffs list from [cv_defaults()]
#' @export
cv_efa <- function(data, rotation = "oblimin", cutoffs = cv_defaults()) {
  # TODO: run parallel analysis, choose factors, return loadings and suggestions
  list(loadings = NULL, suggestion = "Implement EFA; consider parallel analysis via psych::fa.parallel")
}
