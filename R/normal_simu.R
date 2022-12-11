# nohup R CMD BATCH n50.A.R n50.A.txt &
tempo1 <- Sys.time()
library(moments)
require(expm)
require(mvtnorm)

R <- 5000 # n?mero de r?licas
vn <- c(500) # tamanho amostral
set.seed(10)
sigma2 <- 10
sigmax2 <- 4
mux <- -2
beta0 <- 2
beta1 <- 1
mug <- c(beta0 + beta1 * mux, mux)
theta <- c(beta0, beta1, mux, sigmax2, sigma2)
resultados <- list()
resultadosp <- list()

for (n in vn)
{
  k <- 1
  mreplicas <- matrix(rep(NA, (5) * R), ncol = 5)
  mreplicasp <- matrix(rep(NA, (5) * R), ncol = 5)
  contconv <- 0
  continv <- 0
  continvconv <- 0
  j <- 1
  jj <- 1
  while (j <= R) {
    Y <- matrix(rep(NA, (2) * n), ncol = 2)
    taux <- NULL # variance of x
    tauy <- NULL
    taux <- (runif(n, 0.5, 1.5))^2
    tauy <- (runif(n, 0.5, 4))^2
    s11 <- (beta1^2) * sigmax2 + tauy + sigma2
    s12 <- beta1 * sigmax2
    s22 <- sigmax2 + taux
    Sg <- cbind(s11, s12, s12, s22) # each row is a cov matrix
    aux <- function(zf) {
      mvtnorm::rmvnorm(1, mean = mug, sigma = matrix(Sg[zf, ], 2))
    }
    Y <- t(sapply(1:n, aux))

    # escore de Fisher ## ISSO VEIO DO ALÉM
    muxi <- mean(Y[, 2])
    beta1i <- (var(Y)[1, 2]) / (var(Y[, 2]))
    beta0i <- mean(Y[, 1]) + beta1i * muxi
    sigmax2i <- var(Y[, 2])
    sigma2i <- var(Y[, 2]) + beta1i^2 * sigmax2i

    dados <- cbind(Y, taux, tauy)
    Par <- as.vector(c(beta0i, beta1i, muxi, sigmax2i, sigma2i))

    logvero2 <- function(Par) {
      y <- dados[, 1]
      X <- dados[, 2]
      taux <- dados[, 3]
      tauy <- dados[, 4]
      alpha <- Par[1]
      beta <- Par[2]
      mu <- Par[3]
      sigmax <- Par[4]
      sigma <- Par[5]
      n <- length(y)
      Sigma <- matrix(c(sigmax, beta * sigmax, beta * sigmax, sigma + beta^2 * sigmax), 2, 2)
      TT <- matrix(rep(c(Sigma), n), n, byrow = T) + cbind(taux, 0, 0, tauy) # MATRIX DE COV??
      sol1 <- function(f) {
        t(matrix(c(X[f] - mu, y[f] - alpha - beta * mu))) %*% solve(matrix(TT[f, ], 2), tol = 1e-10000) %*% matrix(c(X[f] - mu, y[f] - alpha - beta * mu)) # PQ A GENTE PRECISA DO X[F]
      }
      det1 <- function(f) {
        det(matrix(TT[f, ], 2))
      }
      TT1 <- sapply(1:n, sol1)
      TT2 <- sapply(1:n, det1)
      return(1 / 2 * sum(log(TT2)) + 1 / 2 * sum(TT1))
    }

    theta.max <- nlminb(Par, logvero2, gradient = NULL, hessian = NULL, scale = 1, control = list(), lower = c(-Inf, -Inf, -Inf, 0, 0), upper = c(Inf, Inf, Inf, Inf, Inf))

    beta0est <- theta.max$par[1]
    betaest <- theta.max$par[2]
    muxest <- theta.max$par[3]
    sigmaxest <- theta.max$par[4]
    sigmaest <- theta.max$par[5]
    muyest <- beta0est + betaest * muxest
    muiest <- rbind(muyest, muxest)
    lambdasoma <- matrix(rep(0, (5 * 5)), ncol = 5)

    # soma das contribuiÃ§Ãµes na IF de Fisher
    Fi <- matrix(c(
      1, rep(0, 5), muxest, 0, 2 * betaest * sigmaxest, sigmaxest, sigmaxest, 0, betaest, 1, rep(0, 6),
      betaest^2, betaest, betaest, 1, rep(0, 2), 1, rep(0, 3)
    ), 6)

    # fecha as replicas de Monte Carlo
}
