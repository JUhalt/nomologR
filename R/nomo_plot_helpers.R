# Shared plot helpers (#89) -------------------------------------------------------
#
# ggplot2 does not wrap titles, so a long subtitle or caption ran off the edge of
# a plot drawn at an ordinary size. Every plot sets its text through
# nomo_plot_labs(), which wraps each at a width that fits a 7-inch plot whose
# panel begins after the axis labels: about 60 characters for the title, 75 for
# the subtitle, and 85 for the smaller caption. Text that already contains a
# line break is left as written.
nomo_plot_labs <- function(...) {
  args <- list(...)
  widths <- c(title = 60L, subtitle = 75L, caption = 85L)
  for (nm in intersect(names(widths), names(args))) {
    text <- args[[nm]]
    if (is.character(text) && length(text) == 1L && !is.na(text) &&
        !grepl("\n", text, fixed = TRUE)) {
      args[[nm]] <- paste(strwrap(text, width = widths[[nm]]), collapse = "\n")
    }
  }
  do.call(ggplot2::labs, args)
}
