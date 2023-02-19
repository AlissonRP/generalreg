#' @export coef.generalreg
#' @export
#'
coef.generalreg <-
  function(x){
    return(structure((as.vector((x$coefficients))),
                    .Names = x$names))
  }
