# trackopt: Track Numerical Optimization

Tracks parameter values, gradients, and Hessians at each iteration of
numerical optimizers. Useful for analyzing optimization progress,
diagnosing issues, and studying convergence behavior.

## See also

Useful links:

- <https://github.com/loelschlaeger/trackopt>

- Report bugs at <https://github.com/loelschlaeger/trackopt/issues>

## Author

**Maintainer**: Lennart Oelschläger <oelschlaeger.lennart@gmail.com>

## Examples

``` r
himmelblau <- function(x) {
  (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
}
track <- nlm_track(f = himmelblau, p = c(0, 0))
summary(track)
#> Iterations: 16
#> Function improvement: 170 -> 1.521e-07
#> Computation time: 0.04848 seconds
#> Initial parameter: 0, 0
#> Final parameter: 3, 2
ggplot2::autoplot(track)
```
