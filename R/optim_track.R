#' @rdname nlm_track
#'
#' @param lower,upper \[`numeric()` | `NULL`\]\cr
#' Optionally lower and upper parameter bounds.
#'
#' @param method,control
#' Arguments passed on to \code{\link[stats]{optim}}.
#'
#' Elements `trace` and `maxit` are ignored in `control`.
#'
#' @export

optim_track <- function(
    f, p, target = NULL, npar = NULL, gradient = NULL,
    ..., iterations_max = 100, tolerance = 1e-6,
    lower = NULL, upper = NULL,
    method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
    control = list(), minimize = TRUE, verbose = FALSE
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
  if (!is.null(lower)) {
    if (length(lower) == 1) {
      lower <- rep(lower, npar)
    }
    oeli::input_check_response(
      check = oeli::check_numeric_vector(lower, len = npar),
      var_name = "lower"
    )
    oeli::input_check_response(
      check = if (any(p < lower)) "Must respect lower limit" else TRUE,
      var_name = "p"
    )
  }
  oeli::input_check_response(
    check = oeli::check_numeric_vector(upper, any.missing = FALSE, null.ok = TRUE),
    var_name = "upper"
  )
  if (!is.null(upper)) {
    if (length(upper) == 1) {
      upper <- rep(upper, npar)
    }
    oeli::input_check_response(
      check = oeli::check_numeric_vector(upper, len = npar),
      var_name = "upper"
    )
    oeli::input_check_response(
      check = if (any(p > upper)) "Must respect upper limit" else TRUE,
      var_name = "p"
    )
  }
  oeli::input_check_response(
    check = checkmate::check_list(control, names = "strict"),
    var_name = "control"
  )
  control[["maxit"]] <- 1
  control[["trace"]] <- NULL
  optimizer <- optimizeR::Optimizer$new(
    which = "stats::optim",
    method = method, control = control, hessian = TRUE,
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
    cli::cli_h1("Start tracing {.fun optim}")
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
  current_hessian <- if (isTRUE(objective$hessian_specified)) {
    objective$evaluate_hessian(.at = p)
  } else {
    NA_real_
  }
  out <- tibble::tibble(iteration = 0) |>
    assign_to_cell(1, "value", current_value) |>
    assign_to_cell(1, "step", current_step) |>
    assign_to_cell(1, "parameter", p) |>
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
      lower = if (!is.null(lower)) lower else NA,
      upper = if (!is.null(upper)) upper else NA,
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

