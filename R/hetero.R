#library(moments)
#require(expm)
#require(mvtnorm)

R<-5000 #n?mero de r?licas
vn <-c(50) # tamanho amostral
set.seed(10)
n<-500

beta0<-0.5
beta1<-1
beta<-c(beta0, beta1)
gamma0<-1.5
gamma1<-2
gamma<-c(gamma0, gamma1)
X<-cbind(x0=rep(1,n), x1=runif(n))
W<-cbind(w0=rep(1,n), w1=runif(n))
mui<-exp(X%*%beta)/(1+exp(X%*%beta))
sigmai<-W%*%gamma

theta=c(beta0, beta1, gamma0, gamma1)
Par<-theta
p<-length(beta)
k<-length(gamma)
q<-length(theta)



y=rnorm(n, mui, sigmai)

data<-data.frame(y, X, W)

#generalreg(data, mu_formula = y ~ exp(beta0+x1*beta1)/(1+exp(beta0+x1*beta1)),var_formula = y~gamma0 +w1*gamma1)
