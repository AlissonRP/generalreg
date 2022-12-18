definir_parametros <- function(char, data) {
  termos <- char %>%
    stringr::str_replace_all(" ", "") %>%
    stringr::str_replace_all(stringr::fixed(")"), "+") %>%
    stringr::str_replace_all(stringr::fixed("("), "+") %>%
    stringr::str_split("[(*),(+),(\\-),(^),(\\/)]") %>%
    unlist()
  excluir <- c("log", "exp", "sqrt", "sen", "cos", "")
  covar <- termos[termos %in% names(data)] %>% unique()
  parametros <- termos[!termos %in% c(names(data), excluir)]
  parametros <- suppressWarnings(parametros[is.na(as.numeric(parametros))] %>% unique())
  return(list(covar = covar, parametros = parametros))
  }


# From
# https://github.com/AGPatriota/generalReg/blob/master/R/auxi.R#L2
