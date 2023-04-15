#' generalreg
#'
#' @param mu_formula a model formula including variables and parameters
#' @param var_formula a  model formula for the diagonal of covariance matrix
#' @param data A data frame in which to evaluate the variables in \code{formula} and \code{formula_var}.
#' Can also be a list or an environment, but not a matrix
#' @param dist Inform the distribution of your data, can be: "normal", "logistic", "t" or "power exp"
#' @return generalreg returns an object of class `lm`
#' For more information on class `lm` type ?lm on your console
#'
 #' @examples
#' library(generalreg)
#' set.seed(3)
#' X <- data.frame(x1 = rnorm(1000), x2 = rnorm(1000), x3 = rnorm(1000))
#' #e = rt(1000, df = 3)
#' e = rlogis(1000, scale = 2, location = 3)
#' #e = rnorm(1000, sd = 5)
#' y <- 2 + 3 * X$x1 + 7 * X$x2 + e
#' data <- data.frame(y, X)
#' generalreg(data, mu_formula = y ~ beta0 + beta1 * x1 + beta2 * x2, dist='normal')
#'
#'
#' y <- (1 / 2 * X$x1) + e
#' data <- data.frame(y, X)
#' generalreg(data, mu_formula = y ~ beta0 / beta1 * x1  , dist='normal')
#'
#'generalreg(data = mtcars, mu_formula = mpg ~ alfa + 1/ (beta * disp))
#'
#'
#' @export
#'
generalreg <- function(data, mu_formula, var_formula = NULL, dist = "normal", alpha = NULL, beta = NULL) {
  call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  dist = tolower(dist)

  data <- data[, c(
    which(colnames(data) == mu_formula[[2]]),
    which(colnames(data) != mu_formula[[2]])
  )]
  y <- data[, 1] %>%
    unlist()
  X <- data[, -1]
  parameters <- definir_parametros(as.character(mu_formula[[3]]), data = data)$parametros

  initial_mu = extract_covariates(X, mu_formula)
  initial_var = extract_covariates(X, var_formula)


  if (is.null(var_formula)) {
    sigma <- rep(0.1, length(y))
    cov_sigma <- matrix(diag(sigma), ncol = length(sigma))
    par <- c(runif(length(parameters)), unique(sigma))
  } else {
    cov_par = definir_parametros(as.character(var_formula[[3]]), data = data)$parametros
    parameters = c(parameters, cov_par)
    par <- runif(length(parameters))
    for (i in 1:length(cov_par)) {
      assign(cov_par[i], par[i])
    } # initial values for parameters of var_formula
  }






  logvero <- function(par) {
    parameters = c(parameters, "sigma")
    for (i in 1:(length(parameters) + 1)) {
      assign(parameters[i], par[i]) #initial values of parameters in formula
    }
    mu <- parse(text = initial_mu) |> eval()
    if (is.null(var_formula)) {
      zi <- (matrix(t(c(y - mu))* (1 / sigma), ncol = length(y), byrow = F))  %*% matrix(c(y - mu))
      determinant <- det(as.matrix(sigma))
      quadratic_part <- sapply(zi, \(x) ifelse(x == 0, x + 0.001, x))
      return((1 / 2) * (length(mu)) * (log(determinant)) -  sum(choose_dist(dist, quadratic_part, alpha, beta)))
    } else {
      sigma <- parse(text = initial_var) |> eval()
      cov_sigma <- matrix(diag(sigma), ncol = length(y))
      zi <- ((t(matrix(c(y - mu))) %*% solve(cov_sigma, tol = 1e-10000)) %*% matrix(c(y - mu)))
      quadratic_part <- sapply(zi, \(x) ifelse(x == 0, x + 0.001, x))
      determinant <- det(as.matrix(cov_sigma))
     return(1 / 2 * sum(log(determinant)) -  sum(choose_dist(dist, quadratic_part, alpha, beta)))
    }
  }
  coefficients <- nlminb(par, logvero,
                         gradient = NULL, hessian = NULL,
                         scale = 1, control = list(),
                         lower = c(-Inf, -Inf, -Inf, 0, 0),
                         upper = c(Inf, Inf, Inf, Inf, Inf)
  )$par

  fit = list()
  muhat = function(){
    for (i in 1:length(coefficients)) {
      assign(parameters[i], coefficients[i]) #initial values of parameters in formula
    }
    return(suppressWarnings(parse(text = initial_mu) |> eval()))
  }
  fit$fitted.values <- muhat()

  fit$parameters <- parameters
  fit$mu_formula <- mu_formula
  fit$initial <- par
  fit$coefficients <- coefficients
  fit$call <- call
  names(coefficients) <- parameters
  fit$names <- parameters
  fit$serie <- y
  fit$X <- X
  fit$residuals = y - fit$fitted.values
  fit$rank <- ncol(X)
  if (is.null(var_formula)){
    fit$names <- c(parameters, 'variance')
    names(coefficients)[is.na(names(coefficients))] <- 'variance'

  }


  class(fit) <- "generalreg"
  return(fit)
}










