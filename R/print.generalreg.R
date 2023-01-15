#' @export print.geralreg
#' @export

print.geralreg <-
  function(x, digits = max(3, getOption("digits") - 3), ...)
  {
    cat("\nCall:", deparse(x$call, width.cutoff = floor(getOption("width") * 0.85)), "", sep = "\n")

  }
