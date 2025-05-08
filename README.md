
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Track numerical optimization

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/trackopt)](https://CRAN.R-project.org/package=trackopt)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/trackopt)](https://CRAN.R-project.org/package=trackopt)
[![R-CMD-check](https://github.com/loelschlaeger/trackopt/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/trackopt/actions)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/trackopt/branch/main/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/trackopt?branch=main)
<!-- badges: end -->

The `{trackopt}` package tracks parameter values, gradients, and
Hessians at each iteration of numerical optimizers in `R`. This can be
useful for analyzing optimization progress, diagnosing issues, and
studying convergence behavior.

## Installation

You can install the released package version from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("trackopt")
```

## Example

The following is the `nlm` minimization track of the [Himmelblau’s
function](https://en.wikipedia.org/wiki/Himmelblau%27s_function):

``` r
library("trackopt")
himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
track <- nlm_track(f = himmelblau, p = c(0, 0))
print(track)
#> # A tibble: 17 × 7
#>    iteration         value     step parameter gradient  hessian        seconds
#>  *     <dbl>         <dbl>    <dbl> <list>    <list>    <list>           <dbl>
#>  1         0 170            0       <dbl [2]> <dbl [1]> <dbl [1]>     0       
#>  2         1  47.4         -1.23e+2 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.0251  
#>  3         2  14.0         -3.34e+1 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00121 
#>  4         3   4.91        -9.08e+0 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00158 
#>  5         4   2.26        -2.65e+0 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00118 
#>  6         5   0.951       -1.31e+0 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.000990
#>  7         6   0.272       -6.79e-1 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00102 
#>  8         7   0.0650      -2.07e-1 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00101 
#>  9         8   0.0168      -4.82e-2 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.000991
#> 10         9   0.00400     -1.28e-2 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.000996
#> 11        10   0.000948    -3.06e-3 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00101 
#> 12        11   0.000221    -7.28e-4 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.000986
#> 13        12   0.0000512   -1.69e-4 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00100 
#> 14        13   0.0000118   -3.94e-5 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00100 
#> 15        14   0.00000275  -9.05e-6 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.000987
#> 16        15   0.000000628 -2.13e-6 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00100 
#> 17        16   0.000000152 -4.76e-7 <dbl [2]> <dbl [2]> <dbl [2 × 2]> 0.00102
summary(track)
#> Iterations: 16
#> Function improvement: 170 -> 1.521e-07
#> Computation time: 0.04111 seconds
#> Initial parameter: 0, 0
#> Final parameter: 3, 2
ggplot2::autoplot(track)
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Contact

If you have any questions, found a bug, need a feature, [just file an
issue on
GitHub](https://github.com/loelschlaeger/trackopt/issues/new/choose).
