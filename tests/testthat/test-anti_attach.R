formula = y ~ beta0 + beta1*x1   + beta2*x2

true_formula = (y ~ beta0 + beta1* X$x1   + beta2 * X$x2) |>  as.character()

X <- data.frame(x1 = rnorm(1000), x2 = rnorm(1000), x3 = rnorm(1000))

covs = X |> names()

teste = sapply(covs, \(x) covs |> stringr::str_replace(x, paste0("X$", x))) |> diag()



map_df <- data.frame(num=covs, nuc=teste)

stringr::str_replace_all(as.character(formula)[3], setNames(map_df$nuc, map_df$num))

test_that("multiplication works", {
})
