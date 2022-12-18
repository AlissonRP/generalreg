choose_dist = function(dist, y){
  if (dist == "normal") {
    g = \(y) exp(-y/2)
  } else if (dist == "t") {
    g = \(y) exp(-y) / (1 + exp(-y)) ** 2
  }
  return(g(y))
}
