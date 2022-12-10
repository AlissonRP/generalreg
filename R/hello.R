X = data.frame(x1 = runif(50), x2 = runif(50))

y = 3 + 1*X$x1 + 2*X$x2



par = c(runif(1), runif(1), runif(1), rpois(1, lambda = 15))

dados = data.frame(y, X)

logvero <- function(par) {
  y <- dados[, 1]
  X <- dados[, -1]
  beta0 <- par[1]
  beta1<- par[2]
  beta2 <- par[3]
  sigma = par[4]
  mu = beta0 + beta1 * X$x1 + beta2 * X$x2
  n <- length(y)
  sol1 <- function() {
    return(((t(matrix(c(y - mu))) * (1 / sigma)) %*% matrix(c(y - mu))))
  }
  det1 <- function() {
    return(det(as.matrix(sigma)))
  }
  TT1 <- sol1()
  TT2 <- det1()
  return(1 / 2 * sum(log(TT2)) + 1 / 2 * sum(TT1))
}




theta.max <- nlminb(par, logvero, gradient = NULL, hessian = NULL, scale = 1, control = list(), lower = c(-Inf, -Inf, -Inf, 0, 0), upper = c(Inf, Inf, Inf, Inf, Inf))


