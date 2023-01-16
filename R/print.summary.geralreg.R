#' @export print.summary.geralreg
#' @export
#'
#'
print.summary.geralreg <-
  function(x, digits = max(3, getOption("digits") - 3), ...)
  {
    cat("\nCall:\n",
        paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")


    cat("\nResiduals:\n")
    print(structure((as.vector((x$res))),
                    .Names = c("Min", "1Q", "Median", "3Q", "Max")))

    cat("\nCoefficients:\n")
    print((x$coefficients))

  }
