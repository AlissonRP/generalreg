#' generalreg
#'
#' @param mu_formula a model formula including variables and parameters
#' @param var_formula a  model formula for the diagonal of covariance matrix
#' @param data A data frame in which to evaluate the variables in \code{formula} and \code{formula_var}.
#' Can also be a list or an environment, but not a matrix
#' @param dist Inform the distribution of your data, at the moment it can be 'normal' or 'logistic'
#' @return generalreg returns an object of class `lm`
#' For more information on class `lm` type ?lm on your console
#'
 #' @examples
#' library(generalreg)
#' X <- data.frame(x1 = runif(50), x2 = runif(50))
#' y <- 2 + 3 * X$x1 + 2 * X$x2
#' data <- data.frame(y, X)
#' generalreg(data, mu_formula = y ~ beta0 + beta1*x1 + beta2*x2, dist='normal')
#'
#'
#'
#'generalreg(data = mtcars, mu_formula = mpg ~ alfa+1/(beta*disp))
#' @export

generalreg <- function(data, mu_formula, var_formula = NULL, dist = "normal") {
  dist = tolower(dist)
  attach(data)
  data <- data[, c(
    which(colnames(data) == mu_formula[[2]]),
    which(colnames(data) != mu_formula[[2]])
  )]
  y <- data[, 1] %>%
    unlist()
  X <- data[, definir_parametros(as.character(mu_formula[[3]]), data = data)$covar]
  parameters <- definir_parametros(as.character(mu_formula[[3]]), data = data)$parametros
  if (is.null(var_formula)) {
    sigma <- rep(rpois(1, lambda = rpois(1, lambda = 3)), length(y))
    cov_sigma <- matrix(diag(sigma), ncol = length(sigma))
    par <- c(runif(length(parameters)), unique(sigma))
  } else {
    cov_par = definir_parametros(as.character(var_formula[[3]]), data = data)$parametros
    parameters = c(parameters, cov_par)
    par <- runif(length(parameters))
    for (i in 1:length(cov_par)) {
      assign(cov_par[i], par[i])
    } # initial values for parameters of var_formula
    sigma <- parse(text = as.character(var_formula)[3]) |> eval()
    cov_sigma <- matrix(diag(sigma), ncol = length(y))
  }

  #return(sigma)
  model <- c()



  logvero <- function(par) {
    for (i in 1:length(parameters)) {
      assign(parameters[i], par[i]) #initial values of parameters in formula
    }
    mu <- parse(text = as.character(mu_formula)[3]) |> eval()
    sol1 <- function() {
      return(((t(matrix(c(y - mu))) %*% solve(cov_sigma, tol = 1e-10000)) %*% matrix(c(y - mu))))
    }
    quadratic_part <- sapply(sol1(), \(x) ifelse(x == 0, x + 0.001, x))
    determinant <- det(as.matrix(cov_sigma))
    return(1 / 2 * sum(log(determinant)) -  sum(choose_dist(dist, quadratic_part)))
  }
  coefficients <- nlminb(par, logvero,
    gradient = NULL, hessian = NULL,
    scale = 1, control = list(),
    lower = c(-Inf, -Inf, -Inf, 0, 0),
    upper = c(Inf, Inf, Inf, Inf, Inf)
  )$par

  muhat = function(){
    for (i in 1:length(coefficients)) {
      assign(parameters[i], coefficients[i]) #initial values of parameters in formula
    }
    return(suppressWarnings(parse(text = as.character(mu_formula)[3]) |> eval()))
  }
  model$fitted.values <- muhat()
  detach(data)
  class(model) <- c(if (is.numeric(y)) "mlm", "lm")
  model$coefficients <- coefficients
  names(coefficients) <- parameters
  model$names <- parameters
  model$serie <- y
  model$X <- X
  model$residuals = y - model$fitted.values
  model$rank <- ncol(X)
  if (is.null(var_formula)){
    model$names <- c(parameters, 'sigma')
    names(coefficients)[is.na(names(coefficients))] <- 'sigma'

  }


  #model_presentation <- data.frame(model$names, model$coefficients, par)
  #names(model_presentation) <- c("Parameters", "Estimates", "initial")



  model$call <- match.call()

  print.lm <- function(digits = max(3L, getOption("digits") - 3L)) {
    cat("\nCall:\n",
        paste(deparse(model$call), sep = "\n", collapse = "\n"), "\n\n",
        sep = ""
    )
    cat("Coefficients:\n")
    print.default(format(coefficients, digits = digits),
                  print.gap = 2L, quote = FALSE
    )
  }





  model

}














