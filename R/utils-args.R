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


# The shared whole-number check for counts, seeds, and degrees of freedom:
# TRUE for one finite whole number that fits R's integer range. Testing the
# range first keeps as.integer() from turning 1e10 into NA, which made the
# argument checks fail with a base-R error rather than their own (#145).
nomo_is_whole_number <- function(x) {
  is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
    abs(x) <= .Machine$integer.max && x == round(x)
}
