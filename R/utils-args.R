# Argument helpers ---------------------------------------------------------------

# match.arg() with an error that names the argument (#89). Base R's error says
# "'arg' should be one of ...", which leaves the user to work out which
# argument was meant. Like match.arg(), it takes the choices from the calling
# function's default when they are not given.
nomo_match_arg <- function(arg, choices) {
  name <- deparse(substitute(arg))
  caller <- sys.parent()
  if (missing(choices)) {
    choices <- eval(formals(sys.function(caller))[[name]], envir = sys.frame(caller))
  }
  tryCatch(
    match.arg(arg, choices),
    error = function(e) {
      stop(sprintf(
        "`%s` must be one of %s, not %s.",
        name, nomo_present_or(sprintf('"%s"', choices)),
        paste(sprintf('"%s"', as.character(arg)), collapse = ", ")
      ), call. = FALSE)
    }
  )
}
