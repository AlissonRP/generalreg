real_normal <- function(data, media, sigma) {
  pdf = (1 / sqrt(det(sigma)))  * g * t((y - media)) * solve(sigma) * (y - media)

}

data <- data.frame(
  X = c(405.65, 498.75, 567.25, 618.3, 681.45, 405.65, 498.75, 567.25, 618.3, 681.45, 681.45, 681.45, 681.45, 681.45, 681.45, 681.45, 681.45, 681.45),
  Y = c(90.5, 161.6, 246.743, 422.936, 868.662, 113.383, 207.65, 309.514, 460.686, 972.383, 999.633, 1034, 1047, 1072.022, 1133.287, 1141.883, 1266.290, 1169.767)
)







g = \(x) exp(-x/2)

real = \(x) 2 + 4 * x
y = real(data$X)
y_hat = lm(y ~ X, data = data)$fitted.values + rnorm(length(y), mean = 5, sd=3)
sigma = 0.5

pdf = (1 / sqrt(det(as.matrix(sigma))))  * g(data$X) * (t((y - y_hat)) * as.vector(solve(sigma)) * (y - y_hat))

log_vero = sum(log(pdf))


real_normal <- function(media, sigma, data) {
  y = media(data$X)
  g = \(x) exp(-x/2)
  y_hat = lm(y ~ X, data = data)$fitted.values + rnorm(length(y), mean = 5, sd = 3)
  pdf =  (1 / sqrt(det(as.matrix(sigma))))  * g(data$X) * (t((y - y_hat)) * as.vector(solve(sigma)) * (y - y_hat))
  return(pdf |> log() |> sum())
}

real_normal((media = \(x) 2+ 4 * x), sigma = 0.5, data = data)


optim(lm(y ~ X, data = data)$coefficients, real_normal)



