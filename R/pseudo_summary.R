
#' @export
geral_summary = function(x){
  cat("\nCall:\n",
      paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")


cat("\nResiduals:\n")
    res = x$residuals
    nam <- c("Min", "1Q", "Median", "3Q", "Max")
    res = c(min(res), quantile(res, 0.1), quantile(res, .5), quantile(res, .75), max(res))
    names(res) = nam
    res

cat("\nCoefficients:\n")

}
