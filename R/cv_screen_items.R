#' Screen items (item-total, missingness, basic distribution checks)
#' @param data data.frame
#' @param scales named list
#' @param cutoffs list from [cv_defaults()]
#' @export
cv_screen_items <- function(data, scales, cutoffs = cv_defaults()) {
  # TODO: implement item-total correlations and flagging
  tibble::tibble(item = unlist(scales), item_total = NA_real_, flag = NA_character_)
}
