
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
    }
    sigma <- parse(text = as.character(var_formula)[3]) |> eval()
    cov_sigma <- matrix(diag(sigma), ncol = length(y))
  }

  #return(parameters)
  model <- c()


  logvero <- function(par) {
    for (i in 1:length(parameters)) {
      assign(parameters[i], par[i])
    }
    mu <- parse(text = as.character(mu_formula)[3]) |> eval()
    n <- length(y)
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

  model$coefficients <- coefficients
  model$names <- parameters
  if (is.null(var_formula)){
    model$names <- c(parameters, 'sigma')
  }


  model_presentation <- data.frame(model$names, model$coefficients, par)
  names(model_presentation) <- c("Parameters", "Estimates", "initial")
  detach(data)
  return(model_presentation)
}

X <- data.frame(x1 = runif(50), x2 = runif(50))

y <- 2 + 3 * X$x1 + 2 * X$x2
dados <- data.frame(y, X)


generalreg(dados, mu_formula = y ~ beta0 + beta1*x1 + beta2*x2, dist='logistic')










