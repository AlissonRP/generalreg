X <- data.frame(x1 = runif(50), x2 = runif(50))

y <- 3 + 1 * X$x1 + 2 * X$x2

generalreg <- function(data, mu_formula, var_formula = NULL) {
  y <- dados[, 1]
  X <- dados[, -1]
  if (is.null(var_formula)) {
    sigma <- rep(rpois(1, lambda = 15), length(y))
    cov_sigma <- matrix(diag(sigma), ncol = length(sigma))
  } else {
    sigma <- var_formula(X)
    cov_sigma <- matrix(diag(sigma), ncol = length(y))
  }


  par <- c(runif(ncol(X) + 1), sigma)

  dados <- data.frame(y, X)


  logvero <- function(par) {
    y <- dados[, 1]
    X <- dados[, -1]
    beta0 <- par[1]
    beta1 <- par[2]
    beta2 <- par[3]
    mu <- parse(text = mu_formula) |> eval()
    n <- length(y)
    sol1 <- function() {
      return(((t(matrix(c(y - mu))) %*% cov_sigma) %*% matrix(c(y - mu))))
    }
    det1 <- function() {
      return(det(as.matrix(cov_sigma)))
    }
    TT1 <- sol1()
    TT2 <- det1()
    return(1 / 2 * sum(log(TT2)) + 1 / 2 * sum(TT1))
  }

  nlminb(par, logvero,
    gradient = NULL, hessian = NULL,
    scale = 1, control = list(),
    lower = c(-Inf, -Inf, -Inf, 0, 0),
    upper = c(Inf, Inf, Inf, Inf, Inf)
  )$par
}
















generalreg(dados, mu_formula = "beta0 + beta1 * X$x1 + beta2 * X$x2")
