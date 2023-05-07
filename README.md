
<!-- README.md is generated from README.Rmd. Please edit that file -->

# generalreg

<!-- badges: start -->

[![R-CMD-check](https://github.com/AlissonRP/generalreg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AlissonRP/generalreg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of generalreg is to …

## Installation

You can install the development version of generalreg from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AlissonRP/generalreg")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(generalreg)
X <- data.frame(x1 = rnorm(1000), x2 = rnorm(1000), x3 = rnorm(1000))
#e = rt(1000, df = 3)
#e = rlogis(1000, scale = 3) # var = pi^2  / 3 * scale ^ 2
e = rnorm(1000, sd = 5)
y <- 2 + 3 * X$x1 + 7 * X$x2 + e
data <- data.frame(y, X)
generalreg(data, mu_formula = y ~ beta0 + beta1 * x1 + beta2 * x2, dist='normal')
#> 
#> Call:
#> generalreg(data = data, mu_formula = y ~ beta0 + beta1 * x1 + beta2 * 
#>     x2, dist = "normal")
#> 
#> 
#> Coefficients:
#>     beta0     beta1     beta2  variance 
#>  1.817156  2.784757  6.931398 23.468615 
#> 
#> Initial Values:
#>      beta0      beta1      beta2   variance 
#> 0.07929463 0.10442887 0.01348300 0.10000000
```
