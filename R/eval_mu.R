eval_mu = function(x){
  mu_formula = x$mu_formula
  coefficients = x$coefficients
  parameters = x$parameters
  for (i in 1:length(coefficients)) {
    assign(parameters[i], coefficients[i]) #initial values of parameters in formula
  }
  return(suppressWarnings(parse(text = as.character(mu_formula)[3]) |> eval()))
}
