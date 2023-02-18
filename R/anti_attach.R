library(stringr)
formula = y ~ beta0 + beta1X1


media = as.character(formula)[3] |> stringr::str_replace_all(" ", "") %>%
  stringr::str_replace_all(stringr::fixed(")"), "+") %>%
  stringr::str_replace_all(stringr::fixed("("), "+") %>%
  stringr::str_split("[(*),(+),(\\-),(^),(\\/)]") %>%
  unlist()


media |>
stringr::str_replace("X1", "X$X1")
