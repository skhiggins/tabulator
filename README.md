# tabulator
## Overview
tabulator provides efficient tabulation with big data in R, with Stata-like output. It contains three distinct functions: 

  - `tab()` shows, for each unique value of a selected variable,  the number of observations, proportion of observations, and cumulative proportion.
  - `quantiles()` shows quantile values.
  - `tabcount()` shows the number of unique categories for a selected variable.

All functions work with data frames, data tables, and tibbles.

## Installation

``` r
# The easiest way to get tabulator is to install from CRAN:
install.packages("tabulator")

```

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of tabulator from GitHub.

``` r
# install.packages("remotes")
remotes::install_github("skhiggins/tabulator")
```

## Usage

``` r
library(tabulator)

data(iris)

iris %>% tab(Sepal.Length)
#>     Sepal.Length  N prop cum_prop
#>  1:          5.0 10 0.07     0.07
#>  2:          5.1  9 0.06     0.13
#>  3:          6.3  9 0.06     0.19
#>  4:          5.7  8 0.05     0.24
#>  5:          6.7  8 0.05     0.29
#>  6:          5.8  7 0.05     0.34
#>  7:          5.5  7 0.05     0.39
#>  8:          6.4  7 0.05     0.43
#>  9:          4.9  6 0.04     0.47
#> 10:          5.4  6 0.04     0.51
#> 11:          6.0  6 0.04     0.55
#> 12:          6.1  6 0.04     0.59
#> 13:          5.6  6 0.04     0.63
#> 14:          4.8  5 0.03     0.67
#> 15:          6.5  5 0.03     0.70
#> 16:          4.6  4 0.03     0.73
#> 17:          5.2  4 0.03     0.75
#> 18:          6.9  4 0.03     0.78
#> 19:          6.2  4 0.03     0.81
#> 20:          7.7  4 0.03     0.83
#> 21:          4.4  3 0.02     0.85
#> 22:          5.9  3 0.02     0.87
#> 23:          6.8  3 0.02     0.89
#> 24:          7.2  3 0.02     0.91
#> 25:          4.7  2 0.01     0.93
#> 26:          6.6  2 0.01     0.94
#> 27:          4.3  1 0.01     0.95
#> 28:          4.5  1 0.01     0.95
#> 29:          5.3  1 0.01     0.96
#> 30:          7.0  1 0.01     0.97
#> 31:          7.1  1 0.01     0.97
#> 32:          7.6  1 0.01     0.98
#> 33:          7.3  1 0.01     0.99
#> 34:          7.4  1 0.01     0.99
#> 35:          7.9  1 0.01     1.00
#>     Sepal.Length  N prop cum_prop

iris %>% tabcount(Sepal.Length)
#> [1] 35

iris %>% quantiles(Petal.Width)
#>       p Petal.Width
#>  1: 0.0        0.10
#>  2: 0.1        0.20
#>  3: 0.2        0.20
#>  4: 0.3        0.40
#>  5: 0.4        1.16
#>  6: 0.5        1.30
#>  7: 0.6        1.50
#>  8: 0.7        1.80
#>  9: 0.8        1.90
#> 10: 0.9        2.20
#> 11: 1.0        2.50

```






