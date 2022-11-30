real_normal = function(x){
  y = 2 + 3 * x[1] + 4 * x[2] + 9*x[3]
  return(y)
}


#real_normal(c(3, 4, 6))

x = rnorm(50)
y = rnorm(50)
z = rnorm(50)




data.frame(x, y, z) |>
    cor() |>
      det() |>
      log()

data = data.frame(x)

log_vero = function(data){
log_det = data
          cor() |>
          det() |>
          log()

centered_data = scale(data, scale = F)
  ((-1 / 2) * log_det) - ((1 / 2) * trace(
    scale(data, scale = F) %*% solve(cor(data)) %*% as.vector(t(scale(data, scale = F)))))
}





