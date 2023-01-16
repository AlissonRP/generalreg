#' geralreg
#'
#'
#'
#' @export
#'
geralreg <- function(data, mu_formula, var_formula = NULL, dist = "normal", alpha = NULL, beta = NULL) {
  call <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("data"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
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
    return(1 / 2 * sum(log(determinant)) -  sum(choose_dist(dist, quadratic_part, alpha, beta)))
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
    return(suppressWarnings(parse(text = as.character(mu_formula)[3]) |> eval()))
  }
  fit$fitted.values <- muhat()
  detach(data)

  fit$coefficients <- coefficients
  fit$call <- call
  names(coefficients) <- parameters
  fit$names <- parameters
  fit$serie <- y
  fit$X <- X
  fit$residuals = y - fit$fitted.values
  fit$rank <- ncol(X)
  if (is.null(var_formula)){
    fit$names <- c(parameters, 'sigma')
    names(coefficients)[is.na(names(coefficients))] <- 'sigma'

  }


  class(fit) <- "geralreg"
  return(fit)
}










