X <- data.frame(x1 = runif(50), x2 = runif(50))

y <- 3 + 1 * X$x1 + 2 * X$x2
dados <- data.frame(y, X)
generalreg <- function(data, mu_formula, var_formula = NULL) {
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
  par = runif(length(parameters))

#  par <- NULL
 # for (i in 1:length(parameters)) {
  #  par[i] <- parse(text = parameters[i]) |> eval() # initial points

  #}




  logvero <- function(par) {
    y <- dados[, 1]
    X <- dados[, -1]
    for (i in 1:length(parameters)) {
      assign(parameters[i], par[i])
    }
    mu <- parse(text = mu_formula[[3]]) |> eval()
    n <- length(y)
    sol1 <- function() {
      return(((t(matrix(c(y - mu))) %*% cov_sigma) %*% matrix(c(y - mu))))
    }
    det1 <- function() {
      return(det(as.matrix(cov_sigma)))
    }
    quadratic_part <- sol1()
    determinant <- det1()
    return(1 / 2 * sum(log(determinant)) + 1 / 2 * sum(quadratic_part))
  }

  c(nlminb(par, logvero,
         gradient = NULL, hessian = NULL,
         scale = 1, control = list(),
         lower = c(-Inf, -Inf, -Inf, 0, 0),
         upper = c(Inf, Inf, Inf, Inf, Inf)
  )$par, par)
}



generalreg(dados, mu_formula = y ~ beta0 + beta1 * x1 + beta2 * x2)










formula <- mpg ~ alfa + 1 / (beta * disp)
