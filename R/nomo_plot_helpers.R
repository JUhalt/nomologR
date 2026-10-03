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


# Status in plots (#144) ----------------------------------------------------------
#
# Every plot that marks a status draws it the same way, as contentvalidR does
# (guide points 27 to 29). The shape carries the status and the color repeats
# it, so neither is needed alone and the plot reads in grayscale. Only a status
# that needs attention is warm; no flag is a neutral gray. The two warm colors
# are from the Okabe-Ito colorblind-safe palette.
#
#   status        shape               color
#   no flag       filled circle (16)  neutral gray
#   review        open circle (1)     #E69F00 (orange)
#   concern       filled square (15)  #D55E00 (vermillion)
#   not computed  cross (4)           light gray
#
# Usage, for a plot of rows with a stored status column:
#
#   dat$status <- nomo_plot_status(dat$attention)
#   ggplot2::ggplot(dat, ggplot2::aes(x, y, shape = status, colour = status)) +
#     ggplot2::geom_point() +
#     nomo_plot_status_scales(dat$status, name = "Flag") +
#     ggplot2::scale_x_continuous(labels = nomo_plot_bounded_labels)
#
# nomo_plot_status_scales() gives one legend for shape and color, listing only
# the statuses drawn, and leaves it out only when every point has no flag: a
# legend appears whenever a non-default status is drawn, even a single one.
# nomo_plot_bounded_labels() labels the axis of a statistic that cannot exceed
# 1 without the leading zero (0, .25, .50, .75, 1.00), with a third or fourth
# decimal only when a break needs it (.100, .125, .150).

nomo_plot_status_levels <- c("none", "review", "concern", "not computed")

nomo_plot_status_labels <- c(
  "none" = "No flag", "review" = "Review", "concern" = "Concern",
  "not computed" = "Not computed"
)

nomo_plot_status_shapes <- c("none" = 16, "review" = 1, "concern" = 15, "not computed" = 4)

nomo_plot_status_colors <- c(
  "none" = "#595959", "review" = "#E69F00", "concern" = "#D55E00",
  "not computed" = "#8C8C8C"
)


# A stored status in any of the package's vocabularies as a plot status: a
# factor with the four levels in order of the table above. A word outside them
# is missing.
nomo_plot_status <- function(x) {
  flag <- nomo_present_flag(x)
  flag[!nzchar(flag)] <- "none"
  factor(flag, levels = nomo_plot_status_levels)
}


# Whether a legend is needed: some point carries a status other than no flag.
nomo_plot_show_legend <- function(status) {
  status <- as.character(status)
  any(!is.na(status) & status != "none")
}


# The shape and color scales for a status, with one legend titled `name` that
# lists the statuses drawn in order, or no legend when nothing is flagged.
nomo_plot_status_scales <- function(status, name = "Flag") {
  if (!is.factor(status)) status <- nomo_plot_status(status)
  drawn <- unique(as.character(status[!is.na(status)]))
  breaks <- nomo_plot_status_levels[nomo_plot_status_levels %in% drawn]
  labels <- unname(nomo_plot_status_labels[breaks])
  scales <- list(
    ggplot2::scale_shape_manual(name = name, values = nomo_plot_status_shapes,
                                breaks = breaks, labels = labels, drop = TRUE),
    ggplot2::scale_colour_manual(name = name, values = nomo_plot_status_colors,
                                 breaks = breaks, labels = labels, drop = TRUE)
  )
  if (!nomo_plot_show_legend(status)) {
    scales <- c(scales, list(ggplot2::guides(shape = "none", colour = "none")))
  }
  scales
}


# Axis labels for a bounded statistic in APA style: 0, .25, .50, .75, 1.00.
# The breaks take the fewest decimals, at least `digits` and at most 4, that
# show every break as it is, so a gridline at .125 reads .125, not .13. The
# tolerance absorbs the binary error of computed breaks such as 0.1 + 0.05.
nomo_plot_bounded_labels <- function(x, digits = 2L) {
  x <- suppressWarnings(as.numeric(x))
  breaks <- x[is.finite(x)]
  inexact <- function(d) any(abs(nomo_present_round(breaks, d) - breaks) > 1e-8)
  while (digits < 4L && inexact(digits)) digits <- digits + 1L
  out <- nomo_present_number(x, digits, bounded = TRUE)
  out[is.finite(x) & nomo_present_round(x, digits) == 0] <- "0"
  out[!is.finite(x)] <- NA_character_
  out
}
