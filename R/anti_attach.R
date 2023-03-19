

extract_covariates = function(X_matrix, mu){
covs = X_matrix |> names()
teste = sapply(covs, \(x) covs |> stringr::str_replace(x, paste0("X$", x))) |> diag()


map_df <- data.frame(num=covs, nuc=teste)
return (stringr::str_replace_all(as.character(mu)[3], setNames(map_df$nuc, map_df$num)))

}


