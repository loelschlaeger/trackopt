#' Track numerical optimization
#'
#' @description
#' - `nlm_track()`: track \code{\link[stats]{nlm}} iterations
#' - `optim_track()`: track \code{\link[stats]{optim}} iterations
#' - `summary()`: summary of optimization track
#' - `autoplot()`: visualization of optimization for one or two parameters
#'
#' @param f \[`function`\]\cr
#' A `function` to be optimized, returning a single `numeric` value.
#'
#' The first argument of `f` should be a `numeric` of the same length
#' as `p`, optionally followed by any other arguments specified by
#' the `...` argument.
#'
#' If `f` is to be optimized over an argument other than the first, or more
#' than one argument, this has to be specified via the `target` argument.
#'
#' @param p \[`numeric()`\]\cr
#' The starting parameter values for the target argument(s).
#'
#' @param target \[`character()` | `NULL`\]\cr
#' The name(s) of the argument(s) over which `f` gets optimized.
#'
#' This can only be `numeric` arguments.
#'
#' Can be `NULL` (default), then it is the first argument of `f`.
#'
#' @param npar \[`integer()`\]\cr
#' The length(s) of the target argument(s).
#'
#' Must be specified if more than two target arguments are specified via
#' the `target` argument.
#'
#' Can be `NULL` if there is only one target argument, in which case `npar` is
#' set to be `length(p)`.
#'
#' @param gradient \[`function` | `NULL`\]\cr
#' Optionally a `function` that returns the gradient of `f`.
#'
#' The function call of `gradient` must be identical to `f`.
#'
#' @param hessian \[`function` | `NULL`\]\cr
#' Optionally a `function` that returns the Hessian of `f`.
#'
#' The function call of `hessian` must be identical to `f`.
#'
#' @param ...
#' Additional arguments to be passed to `f` (and `gradient`,
#' `hessian` if specified).
#'
#' @param iterations_max \[`integer(1)`\]\cr
#' The maximum number of iterations before termination.
#'
#' @param tolerance \[`numeric(1)`\]\cr
#' The minimum allowed absolute change in function value between two iterations
#' before termination.
#'
#' @param typsize,fscale,ndigit,stepmax,steptol
#' Arguments passed on to \code{\link[stats]{nlm}}.
#'
#' @param minimize \[`logical(1)`\]\cr
#' Minimize?
#'
#' @param verbose \[`logical(1)`\]\cr
#' Print progress?
#'
#' @param object \[`trackopt`\]\cr
#' A `trackopt` object.
#'
#' @return
#' A `tibble` with iterations in rows.
#'
#' @export
#'
#' @examples
#' himmelblau <- function(x) (x[1]^2 + x[2] - 11)^2 + (x[1] + x[2]^2 - 7)^2
#' track <- nlm_track(f = himmelblau, p = c(0, 0))
#' summary(track)
#' ggplot2::autoplot(track)

nlm_track <- function(
    f, p, target = NULL, npar = NULL, gradient = NULL, hessian = NULL,
    ..., iterations_max = 100, tolerance = 1e-6,
    typsize = rep(1, length(p)), fscale = 1, ndigit = 12,
    stepmax = max(1000 * sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-6,
    minimize = TRUE, verbose = FALSE
  ) {

  ### input checks and building of objects
  oeli::input_check_response(
    check = oeli::check_missing(f),
    var_name = "f"
  )
  oeli::input_check_response(
    check = oeli::check_missing(p),
    var_name = "p"
  )
  oeli::input_check_response(
    check = oeli::check_numeric_vector(p, any.missing = FALSE),
    var_name = "p"
  )
  if (is.null(npar)) {
    npar <- length(p)
  }
  objective <- optimizeR::Objective$new(f = f, target = target, npar = npar, ...)
  if (!is.null(gradient)) {
    oeli::input_check_response(
      check = checkmate::check_function(gradient),
      var_name = "gradient"
    )
    objective$set_gradient(gradient = gradient, .verbose = FALSE)
  }
  if (!is.null(hessian)) {
    oeli::input_check_response(
      check = checkmate::check_function(hessian),
      var_name = "hessian"
    )
    objective$set_hessian(hessian = hessian, .verbose = FALSE)
  }
  npar <- sum(objective$npar)
  oeli::input_check_response(
    check = oeli::check_numeric_vector(p, len = npar),
    var_name = "p"
  )
  oeli::input_check_response(
    check = checkmate::check_count(iterations_max, positive = FALSE),
    var_name = "iterations_max"
  )
  oeli::input_check_response(
    check = checkmate::check_number(tolerance, lower = 0),
    var_name = "tolerance"
  )
  optimizer <- optimizeR::Optimizer$new(
    which = "stats::nlm",
    typsize = typsize, fscale = fscale, ndigit = ndigit, stepmax = stepmax,
    steptol = steptol, iterlim = 1, hessian = TRUE,
    .verbose = FALSE
  )
  oeli::input_check_response(
    check = checkmate::check_flag(minimize),
    var_name = "minimize"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(verbose),
    var_name = "verbose"
  )

  ### start values
  if (verbose) {
    cli::cli_h1("Start tracing {.fun nlm}")
    cli::cli_h3("Iteration {0}")
  }
  current_initial <- p
  current_value <- objective$evaluate(.at = p)
  current_step <- 0
  oeli::input_check_response(
    check = checkmate::check_number(current_value, finite = TRUE),
    prefix = "{.fun {objective$objective_name}} evaluated at {.var p} is bad:"
  )
  if (verbose) {
    cli::cat_line("Step:")
    cli::cat_print(current_step)
    cli::cat_line("Parameter:")
    cli::cat_print(current_initial)
    cli::cat_line("Function value:")
    cli::cat_print(current_value)
  }

  ### prepare output tibble
  current_gradient <- if (isTRUE(objective$gradient_specified)) {
    objective$evaluate_gradient(.at = p)
  } else {
    NA_real_
  }
  current_hessian <- if (isTRUE(objective$hessian_specified)) {
    objective$evaluate_hessian(.at = p)
  } else {
    NA_real_
  }
  out <- tibble::tibble(iteration = 0) |>
    assign_to_cell(1, "value", current_value) |>
    assign_to_cell(1, "step", current_step) |>
    assign_to_cell(1, "parameter", p) |>
    assign_to_cell(1, "gradient", current_gradient) |>
    assign_to_cell(1, "hessian", current_hessian) |>
    assign_to_cell(1, "seconds", 0)

  ### start optimization
  for (i in seq_len(iterations_max)) {

    ### progress status
    if (verbose) {
      cli::cli_h3("Iteration {i}")
    }

    ### one iteration ahead
    step <- optimizer$optimize(
      objective = objective,
      initial = current_initial,
      direction = ifelse(minimize, "min", "max")
    )
    current_step <- step$value - current_value

    ### progress status
    if (verbose) {
      cli::cat_line("Step:")
      cli::cat_print(current_step)
      cli::cat_line("Parameter:")
      cli::cat_print(current_initial)
      cli::cat_line("Function value:")
      cli::cat_print(current_value)
    }

    ### save results
    out <- out |>
      assign_to_cell(i + 1, "iteration", i) |>
      assign_to_cell(i + 1, "value", step$value) |>
      assign_to_cell(i + 1, "step", current_step) |>
      assign_to_cell(i + 1, "parameter", step$parameter) |>
      assign_to_cell(i + 1, "gradient", step$gradient) |>
      assign_to_cell(i + 1, "hessian", step$hessian) |>
      assign_to_cell(i + 1, "seconds", step$seconds)

    ### check termination
    if (isTRUE(step$error)) {
      cli::cli_warn("Optimization failed: {step$error_message}")
      if (verbose) {
        cli::cli_h3("Termination")
        cli::cli_alert_warning("An error occurred")
        cli::cli_alert_info("{step$error_message}")
      }
      break
    }
    if (abs(current_step) < tolerance) {
      if (verbose) {
        cli::cli_h3("Termination")
        cli::cli_alert_success("Absolute change in function value < {tolerance}")
      }
      break
    }
    if (i == iterations_max && verbose) {
      cli::cli_h3("Termination")
      cli::cli_alert_warning("Iteration limit of {iterations_max} reached")
    }

    ### new start values
    current_initial <- step$parameter
    current_value <- step$value
  }

  ### return
  structure(
    out,
    class = c("trackopt", class(out)),
    objective = objective,
    npar = npar
  )
}

