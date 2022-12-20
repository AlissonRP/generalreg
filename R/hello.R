X <- data.frame(x1 = runif(50), x2 = runif(50))

y <- 2 + 3 * X$x1 + 2 * X$x2
dados <- data.frame(y, X)
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
      return(((t(matrix(c(y - mu))) %*% cov_sigma) %*% matrix(c(y - mu))))
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


  model_presentation <- data.frame(model$names, model$coefficients)
  names(model_presentation) <- c("Parameters", "Estimates")
  detach(data)
  return(model_presentation)
}

dados = data.frame(X=c(405.65,498.75,567.25,618.3,681.45,405.65,498.75,567.25,618.3,681.45,681.45,681.45,681.45,681.45,681.45,681.45,681.45,681.45),Y=c(90.5,161.6,246.743,422.936,868.662,113.383,207.65,309.514,460.686,972.383,999.633,1034,1047,1072.022,1133.287,1141.883,1266.290,1169.767))



generalreg(dados, mu_formula = Y~beta1*exp(beta2*X), dist='normal', var_formula = t ~ sigma1 * exp(sigma2*X))










