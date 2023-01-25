
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
X <- data.frame(x1 = runif(50), x2 = runif(50))
y <- 2 + 3 * X$x1 + 2 * X$x2
data <- data.frame(y, X)
fit <- generalreg(data, mu_formula = y ~ beta0 + beta1*x1 + beta2*x2, dist='normal')

fit |> summary()
#> 
#> Call:
#> generalreg(data = data, mu_formula = y ~ beta0 + beta1 * x1 + 
#>     beta2 * x2, dist = "normal")
#> 
#> Residuals:
#>           Min            1Q        Median            3Q           Max 
#> -1.585824e-06 -1.147532e-06 -8.675139e-08  6.185132e-07  1.847270e-06 
#> 
#> Coefficients:
#>       Estimate Std_Error P_Value
#> beta0 1.999998    No Sei  No Sei
#> beta1 2.999999    No Sei  No Sei
#> beta2 2.000003    No Sei  No Sei
#> sigma 2.000000    No Sei  No Sei
#> ---
#> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```
