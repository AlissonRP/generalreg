# 'choose_dist
choose_dist <- function(dist, y, alpha = NULL, beta = NULL) {
  d <- length(y)
  if (dist == "normal") {
    g <- \(y) -y / 2
  } else if (dist == "logistic") {
    g <- \(y) -y - 2 * log(1 + exp(-y))
  } else if (dist == "cauchy") {
    g <- \(y) (-(d + 1) / 2) * log(1 + y)
  } else if (dist == "t") {
    if (is.null(alpha)) {
      stop("t distribution requires degrees of freedom (alpha)")
    }
    g <- \(y)  (-(alpha + 2) / 2) * log(1 + (y / alpha))
  } else if (dist == "pow exp") {
    if (is.null(alpha)) {
      stop("power exponetial distribution requires alpha parameter")
    }
    g <- \(y) -(y^alpha) / 2
  } #else if (dist == "cont normal"){
    #if (is.null(alpha) | is.null(beta)) {
      #stop("contaminated normal distribution requires alpha and beta parameters")
    #}
    #g <- \(y) log(((1 - alpha) * exp(- y / 2)) + (alpha * beta^(d / 2)) * exp(-y / 2 * beta))
  }
  return(g(y))
}
