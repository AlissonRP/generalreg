#' @export summary.geralreg
#' @export

summary.geralreg <-
  function(x){




      x$res = c(min(x$residuals), quantile(x$residuals, 0.1), quantile(x$residuals, .5), quantile(x$residuals, .75), max(x$residuals))
      x$coefficients = data.frame(row.names = x$names, Estimate = x$coefficients)
      class(x) <- "summary.geralreg"
      x

  }
