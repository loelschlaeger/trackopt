# Track numerical optimization

- `nlm_track()`: track [`nlm`](https://rdrr.io/r/stats/nlm.html)
  iterations

- `optim_track()`: track [`optim`](https://rdrr.io/r/stats/optim.html)
  iterations

- [`summary()`](https://rdrr.io/r/base/summary.html): summarize an
  optimization track

- `autoplot()`: visualize a track with one or two parameters

## Usage

``` r
nlm_track(
  f,
  p,
  target = NULL,
  npar = NULL,
  gradient = NULL,
  hessian = NULL,
  ...,
  iterations_max = 100,
  tolerance = 1e-06,
  typsize = rep(1, length(p)),
  fscale = 1,
  ndigit = 12,
  stepmax = max(1000 * sqrt(sum((p/typsize)^2)), 1000),
  steptol = 1e-06,
  minimize = TRUE,
  verbose = FALSE
)

optim_track(
  f,
  p,
  target = NULL,
  npar = NULL,
  gradient = NULL,
  ...,
  iterations_max = 100,
  tolerance = 1e-06,
  lower = NULL,
  upper = NULL,
  method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
  control = list(),
  minimize = TRUE,
  verbose = FALSE
)

# S3 method for class 'trackopt'
summary(object, ...)

# S3 method for class 'trackopt'
autoplot(object, iteration = NULL, xlim = NULL, xlim2 = NULL, ...)
```

## Arguments

- f:

  \[`function`\]  
  A `function` to optimize. It must return a single `numeric` value.

  By default, the first argument of `f` is optimized. That argument
  should be a `numeric` vector of the same length as `p`. Additional
  arguments can be supplied through `...`.

  Use `target` when `f` should be optimized over another argument or
  over multiple arguments.

- p:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  The starting parameter values for the target argument(s).

- target:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  The names of the numeric argument(s) to optimize.

  If `NULL` (default), the first argument of `f` is used.

- npar:

  \[[`integer()`](https://rdrr.io/r/base/integer.html)\]  
  The length of each target argument.

  Specify `npar` when optimizing over multiple target arguments so `p`
  can be split correctly.

  If `target` contains a single argument and `npar` is `NULL`,
  `length(p)` is used.

- gradient:

  \[`function` \| `NULL`\]  
  Optionally a `function` that returns the gradient of `f`.

  Its arguments must match the arguments of `f`.

- hessian:

  \[`function` \| `NULL`\]  
  Optionally a `function` that returns the Hessian of `f`.

  Its arguments must match the arguments of `f`.

- ...:

  Additional arguments passed to `f`, and to `gradient` and `hessian`
  when they are specified.

- iterations_max:

  \[`integer(1)`\]  
  The maximum number of tracked optimization steps before termination.

- tolerance:

  \[`numeric(1)`\]  
  Tracking stops when the absolute change in function value between two
  consecutive iterations is less than this value.

- typsize, fscale, ndigit, stepmax, steptol:

  Arguments passed on to [`nlm`](https://rdrr.io/r/stats/nlm.html).

- minimize:

  \[`logical(1)`\]  
  If `TRUE`, minimize `f`; otherwise maximize it.

- verbose:

  \[`logical(1)`\]  
  If `TRUE`, print progress after each iteration.

- lower, upper:

  \[[`numeric()`](https://rdrr.io/r/base/numeric.html) \| `NULL`\]  
  Optional lower and upper parameter bounds. Scalars are recycled to the
  number of parameters.

- method, control:

  Arguments passed on to [`optim`](https://rdrr.io/r/stats/optim.html).

  Elements `trace` and `maxit` are ignored in `control` because
  `optim_track()` controls tracing and iteration limits directly.

- object:

  \[`trackopt`\]  
  A `trackopt` object.

- iteration:

  \[`integer(1)`\]  
  The iteration to plot.

  If `NULL`, the last iteration is plotted.

  This option is useful for creating animations with the R Markdown
  `animation` chunk option.

- xlim, xlim2:

  \[`numeric(2)`\]  
  Ranges for the first and second parameter to plot.

  If `NULL`, they are derived from the parameter ranges in `object`.

## Value

A `tibble` with one row per stored iteration, including the starting
point.

## Examples

``` r
himmelblau <- function(x) {
  (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
}
track <- nlm_track(f = himmelblau, p = c(0, 0))
summary(track)
#> Iterations: 16
#> Function improvement: 170 -> 1.521e-07
#> Computation time: 0.06022 seconds
#> Initial parameter: 0, 0
#> Final parameter: 3, 2
ggplot2::autoplot(track)
```
