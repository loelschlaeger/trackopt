test_that("one-dim optim track works", {
  f <- function(x) 5*x^4 + 4*x^3 + x^2 + 3*x + 2
  gradient <- function(x) 20*x^3 + 12*x^2 + 2*x + 3
  track <- optim_track(
    f = f, p = 5, gradient = gradient, method = "Brent", lower = -10, upper = 10
  )
  checkmate::expect_tibble(track, ncols = 6)
  checkmate::expect_class(summary(track), "summary.trackopt")
  checkmate::expect_class(invisible(autoplot(track)), "ggplot")
})

test_that("two-dim optim track works", {
  himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
  oeli::quiet(
    track <- optim_track(f = himmelblau, p = c(0, 0), verbose = TRUE)
  )
  checkmate::expect_tibble(track, ncols = 6)
  checkmate::expect_class(summary(track), "summary.trackopt")
  checkmate::expect_class(invisible(autoplot(track)), "ggplot")
})
