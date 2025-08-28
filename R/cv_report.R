#' Generate an HTML report
#' @param pipeline an object returned by [cv_pipeline()]
#' @param file output path
#' @export
cv_report <- function(pipeline, file = "scale_dev_report.html") {
  # TODO: implement Quarto/Rmd rendering with decision log and key tables
  message("Report generation not yet implemented.")
  invisible(file)
}
