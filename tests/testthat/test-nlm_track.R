test_that("one-dim nlm track works", {
  f <- function(x) 5*x^4 + 4*x^3 + x^2 + 3*x + 2
  gradient <- function(x) 20*x^3 + 12*x^2 + 2*x + 3
  hessian <- function(x) matrix(60*x^2 + 24*x + 2, 1, 1)
  track <- nlm_track(f = f, p = 2, gradient = gradient, hessian = hessian)
  checkmate::expect_tibble(track, ncols = 7)
  checkmate::expect_class(summary(track), "summary.trackopt")
  checkmate::expect_class(invisible(autoplot(track)), "ggplot")
})

test_that("two-dim nlm track works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  oeli::quiet(
    track <- nlm_track(f = himmelblau, p = c(0, 0), verbose = TRUE)
  )
  checkmate::expect_tibble(track, ncols = 7)
  checkmate::expect_class(summary(track), "summary.trackopt")
  checkmate::expect_class(invisible(autoplot(track)), "ggplot")
})

test_that("deals gracefully with errors", {
  f <- function(x) {
    if (x < -1) stop("my error")
    x
  }
  expect_warning(
    track <- nlm_track(f = f, p = 2, minimize = TRUE),
    "Optimization failed: Function evaluation threw an error: my error"
  )
  checkmate::expect_tibble(track, ncols = 7)
  checkmate::expect_class(summary(track), "summary.trackopt")
  checkmate::expect_class(invisible(autoplot(track)), "ggplot")
})
