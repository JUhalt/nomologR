# Output gallery (#89) ----------------------------------------------------------
#
# Regenerates every print() and summary() a user sees, at a fixed console
# width, and every plot type, then flags presentation faults:
#
#   over_width  lines wider than the console
#   truncated   cells cut off with an ellipsis
#   hidden      tibble columns hidden behind "n more variables"
#   type_rows   tibble column-type rows such as <chr> and <dbl>
#
# and, for plots, a title, subtitle, or caption long enough to be clipped at
# 7 x 5 inches.
#
# Usage, from the package root:
#   Rscript dev/output-gallery.R [output directory] [console width]
#
# The directory receives gallery.txt (all output), faults.csv (one row per
# output), plots/ (PNG files), and plot-text.csv.

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args) >= 1L) args[[1L]] else file.path(tempdir(), "nomo_gallery")
width <- if (length(args) >= 2L) as.integer(args[[2L]]) else 80L

pkgload::load_all(".", quiet = TRUE)
unlink(out, recursive = TRUE)
dir.create(file.path(out, "plots"), recursive = TRUE)
options(width = width)
set.seed(2026)

gallery <- file.path(out, "gallery.txt")
faults <- list()
show <- function(label, expr) {
  lines <- tryCatch(
    utils::capture.output(expr),
    error = function(e) paste("ERROR:", conditionMessage(e))
  )
  faults[[label]] <<- data.frame(
    output = label,
    lines = length(lines),
    over_width = sum(nchar(lines, type = "width") > width),
    truncated = sum(grepl("…|\\.\\.\\.$", lines)),
    hidden = sum(grepl("more variable", lines)),
    type_rows = sum(grepl("<(chr|dbl|int|lgl|ord|fct)>", lines))
  )
  cat(sprintf("\n\n######## %s ########\n", label), file = gallery, append = TRUE)
  cat(lines, sep = "\n", file = gallery, append = TRUE)
}

# Fixtures -----------------------------------------------------------------------
cont <- nomo_demo_continuous
ab <- list(A = paste0("a", 1:5), B = paste0("b", 1:5))
scales3 <- list(
  Agency = paste0("ag", 1:4),
  Persistence = paste0("pe", 1:4),
  SocialDesirability = paste0("sd", 1:3)
)
h <- nomo_hypotheses(
  "Agency -> Persistence" = positive(min = .20),
  "Agency <-> SocialDesirability" = negligible(within = c(-.15, .15)),
  "Agency -> Performance" = positive()
)

scr <- nomo_screen(cont)
fac <- nomo_factors(cont, seed = 2026)
efa <- nomo_efa(cont, factors = fac)
cfa <- nomo_cfa(nomo_model(ab), data = cont)
rel <- nomo_reliability(cfa)
val <- nomo_validity(cfa)
sc <- nomo_scores(cfa, method = "sum")
mis <- nomo_missing(cfa, data = cont)
no_b5 <- nomo_cfa("A =~ a1 + a2 + a3 + a4 + a5\nB =~ b1 + b2 + b3 + b4 + 0*b5", data = cont)
cmp <- nomo_compare(full = cfa, no_b5 = no_b5, rationale = "Is b5 needed?")
inv <- nomo_invariance("Agency =~ ag1 + ag2 + ag3 + ag4", data = nomo_demo_network,
                       group = "group", levels = c("configural", "metric", "scalar"))
part <- nomo_partial(level = "scalar", syntax = "ag3 ~ 1",
                     rationale = "Anticipated mode difference.")
net <- nomo_network(nomo_model(scales3), data = nomo_demo_network, hypotheses = h)
sp <- nomo_split(nomo_demo_network, validation_prop = 0.4, seed = 2026)

set.seed(2026)
n <- 500
g <- rnorm(n)
s <- matrix(rnorm(n * 3), n, 3)
hd <- as.data.frame(sapply(1:9, function(i) .6 * g + .45 * s[, ceiling(i / 3)] + rnorm(n, sd = .65)))
names(hd) <- paste0("x", 1:9)
hf <- list(A = c("x1", "x2", "x3"), B = c("x4", "x5", "x6"), C = c("x7", "x8", "x9"))
hier <- nomo_hierarchical(nomo_cfa(nomo_model(hf, structure = "bifactor"), data = hd))

run_paused <- nomo_run(nomo_demo_network, scales = scales3,
                       settings = list(factors = list(seed = 2026)))
run_done <- nomo_run(
  nomo_demo_network, scales = scales3,
  decisions = list(
    factor_count = c(Agency = 1, Persistence = 1, SocialDesirability = 1),
    cfa_model = nomo_model(scales3), measurement_model = "proceed"
  ),
  settings = list(
    factors = list(seed = 2026), network = list(hypotheses = h),
    screen = list(effort = TRUE), scores = list(method = "sum"), missing = list()
  )
)

# Console output -----------------------------------------------------------------
show("screen print", print(scr))
show("screen summary", print(summary(scr)))
show("screen effort print", print(nomo_screen(cont, effort = TRUE, scales = ab,
                                              scale_range = c(-4, 4))))
show("factors print", print(fac))
show("factors summary", print(summary(fac)))
show("efa print", print(efa))
show("efa summary", print(summary(efa)))
show("cfa print", print(cfa))
show("cfa summary", print(summary(cfa)))
show("reliability print", print(rel))
show("reliability summary", print(summary(rel)))
show("validity print", print(val))
show("validity summary", print(summary(val)))
show("scores print", print(sc))
show("scores summary", print(summary(sc)))
show("missing print", print(mis))
show("compare print", print(cmp))
show("compare summary", print(summary(cmp)))
show("invariance print", print(inv))
show("invariance summary", print(summary(inv)))
show("partial print", print(part))
show("hypotheses print", print(h))
show("hypotheses summary", print(summary(h)))
show("network print", print(net))
show("network summary", print(summary(net)))
show("model print", print(nomo_model(scales3)))
show("split print", print(sp))
show("hierarchical print", print(hier))
show("hierarchical summary", print(summary(hier)))
show("apa loadings", print(nomo_apa_table(cfa, "loadings", number = 1)))
show("run paused print", print(run_paused))
show("run paused summary", print(summary(run_paused)))
show("run complete print", print(run_done))
show("run complete summary", print(summary(run_done)))
show("run research print", print(nomo_run(resume = run_done, mode = "research")))

# Plots --------------------------------------------------------------------------
plot_text <- list()
objects <- list(
  nomo_screen = scr, nomo_factors = fac, nomo_efa = efa, nomo_cfa = cfa,
  nomo_reliability = rel, nomo_validity = val, nomo_compare = cmp,
  nomo_invariance = inv, nomo_network = net, nomo_hierarchical = hier
)
for (cls in names(objects)) {
  method <- utils::getS3method("plot", cls)
  types <- eval(formals(method)$type)
  if (!is.character(types)) types <- "default"
  for (type in types) {
    label <- paste(cls, type, sep = "__")
    p <- tryCatch(
      if (identical(type, "default")) plot(objects[[cls]]) else plot(objects[[cls]], type = type),
      error = function(e) NULL
    )
    if (is.null(p)) next
    ggplot2::ggsave(file.path(out, "plots", paste0(label, ".png")), p,
                    width = 7, height = 5, dpi = 96)
    labs <- p$labels
    text <- vapply(c("title", "subtitle", "caption"), function(k) {
      v <- labs[[k]]
      if (is.null(v)) "" else paste(v, collapse = " ")
    }, character(1))
    # About 95 characters fit across 7 inches at ggplot2's default sizes.
    plot_text[[label]] <- data.frame(
      plot = label, title = nchar(text[["title"]]), subtitle = nchar(text[["subtitle"]]),
      caption = nchar(text[["caption"]]),
      long = any(nchar(text) > 95 & !grepl("\n", text))
    )
  }
}

fault_table <- do.call(rbind, faults)
plot_table <- do.call(rbind, plot_text)
utils::write.csv(fault_table, file.path(out, "faults.csv"), row.names = FALSE)
utils::write.csv(plot_table, file.path(out, "plot-text.csv"), row.names = FALSE)

cat("Console outputs:", nrow(fault_table), "| with faults:",
    sum(rowSums(fault_table[, c("over_width", "truncated", "hidden", "type_rows")]) > 0), "\n")
print(fault_table[rowSums(fault_table[, c("over_width", "truncated", "hidden", "type_rows")]) > 0, ],
      row.names = FALSE)
cat("\nPlots:", nrow(plot_table), "| with long titles:", sum(plot_table$long), "\n")
print(plot_table[plot_table$long, ], row.names = FALSE)
cat("\nWritten to", out, "\n")
