
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Track numerical optimization <a href="https://loelschlaeger.de/trackopt/"><img src="man/figures/logo.png" align="right" height="124" alt="trackopt website" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/trackopt)](https://CRAN.R-project.org/package=trackopt)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/grand-total/trackopt)](https://CRAN.R-project.org/package=trackopt)
[![R-CMD-check](https://github.com/loelschlaeger/trackopt/workflows/R-CMD-check/badge.svg)](https://github.com/loelschlaeger/trackopt/actions)
[![Codecov test
coverage](https://codecov.io/gh/loelschlaeger/trackopt/branch/master/graph/badge.svg)](https://app.codecov.io/gh/loelschlaeger/trackopt?branch=master)
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

## Examples

The following example tracks `nlm` while it minimizes [Himmelblau’s
function](https://en.wikipedia.org/wiki/Himmelblau%27s_function):

``` r
library("trackopt")
himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
track <- nlm_track(f = himmelblau, p = c(0, 0))
summary(track)
#> Optimizer: stats::nlm
#> Iterations: 16
#> Function improvement: 170 -> 1.521e-07
#> Best value: 1.521e-07
#> Computation time: 0.08447 seconds
#> Initial parameter: 0, 0
#> Final parameter: 3, 2
#> Convergence: function value tolerance
ggplot2::autoplot(track)
```

<img src="man/figures/README-himmelblau-1.png" alt="" width="100%" />

The next example tracks `optim` while it minimizes a quartic polynomial:

``` r
polynomial <- function(x) 5 * x^4 + 4 * x^3 + x^2 + 3 * x + 2
gradient <- function(x) 20 * x^3 + 12 * x^2 + 2 * x + 3
track <- optim_track(
  f = polynomial,
  p = 0,
  gradient = gradient,
  method = "BFGS"
)
print(track)
#> # A tibble: 9 × 13
#>   iteration value         step parameter gradient hessian       seconds
#> *     <dbl> <dbl>        <dbl>     <dbl>    <dbl> <list>          <dbl>
#> 1         0 2      0               0      3       <dbl [1]>     0      
#> 2         1 0.344 -1.66           -0.6    1.80    <dbl [1 × 1]> 0.00727
#> 3         2 0.241 -0.103          -0.672  1.01    <dbl [1 × 1]> 0.00156
#> 4         3 0.212 -0.0293         -0.712  0.437   <dbl [1 × 1]> 0.00151
#> 5         4 0.207 -0.00524        -0.730  0.159   <dbl [1 × 1]> 0.00166
#> 6         5 0.206 -0.000679       -0.736  0.0533  <dbl [1 × 1]> 0.00144
#> 7         6 0.206 -0.0000752      -0.738  0.0172  <dbl [1 × 1]> 0.00225
#> 8         7 0.206 -0.00000784     -0.739  0.00551 <dbl [1 × 1]> 0.00156
#> 9         8 0.206 -0.000000802    -0.739  0.00176 <dbl [1 × 1]> 0.00150
#> # ℹ 6 more variables: parameter_step_norm <dbl>, relative_improvement <dbl>,
#> #   gradient_norm <dbl>, hessian_det <dbl>, hessian_condition <dbl>,
#> #   hessian_positive_definite <lgl>
ggplot2::autoplot(track)
```

<img src="man/figures/README-polynomial-1.png" alt="" width="100%" />

## Contact

If you have questions, find a bug, or need a feature, please [file an
issue on
GitHub](https://github.com/loelschlaeger/trackopt/issues/new/choose).
