choose_dist <- function(dist, y) {
  if (dist == "normal") {
    g <- \(y) -y / 2
  } else if (dist == "logistic") {
    g <- \(y) -y - 2 * log(1 + exp(-y))
  }
  return(g(y))
}
