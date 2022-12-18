X <- data.frame(x1 = runif(50), x2 = runif(50))

y <- 2  + 3*X$x1  + 2*X$x2
dados <- data.frame(y, X)
generalreg <- function(data, mu_formula, var_formula = NULL, dist = "normal") {
  attach(data)
  y <- dados[, 1]
  X <- dados[, -1]
  if (is.null(var_formula)) {
    sigma <- rep(rpois(1, lambda = 15), length(y))
    cov_sigma <- matrix(diag(sigma), ncol = length(sigma))
  } else {
    sigma <- var_formula(X)
    cov_sigma <- matrix(diag(sigma), ncol = length(y))
  }
  parameters <- definir_parametros(as.character(mu_formula[[3]]), data = data)$parametros
  par <- runif(length(parameters))


model <- c()


  logvero <- function(par) {
    y <- dados[, 1]
    X <- dados[, -1]
    for (i in 1:length(parameters)) {
      assign(parameters[i], par[i])
    }
    mu <- parse(text = as.character(mu_formula)[3]) |> eval()
    n <- length(y)
    sol1 <- function() {
      return(((t(matrix(c(y - mu))) %*% cov_sigma) %*% matrix(c(y - mu))))
    }
    det1 <- function() {
      return(det(as.matrix(cov_sigma)))
    }
    quadratic_part <- sapply(sol1(), \(x) ifelse(x==0, x+0.001, x))
    determinant <- det1()
    return(1 / 2 * sum(log(determinant)) + 1 / 2 * sum(quadratic_part))
  }

  coefficients = nlminb(par, logvero,
    gradient = NULL, hessian = NULL,
    scale = 1, control = list(),
    lower = c(-Inf, -Inf, -Inf, 0, 0),
    upper = c(Inf, Inf, Inf, Inf, Inf)
  )$par

  model$coefficients <- coefficients
  model$names <- parameters
  model_presentation = data.frame(model$names, model$coefficients)
  names(model_presentation) =  c("Parameters", "Estimates")
  return(model_presentation)

}




generalreg(dados, mu_formula = y ~ beta0 + beta1 * x1 + beta2*x2)



