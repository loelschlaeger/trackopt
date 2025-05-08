#' @rdname nlm_track
#' @exportS3Method summary trackopt

summary.trackopt <- function(object, ...) {

  ### input checks
  oeli::input_check_response(
    check = oeli::check_missing(object),
    var_name = "object"
  )
  oeli::input_check_response(
    check = checkmate::check_class(object, "trackopt"),
    var_name = "object"
  )
  oeli::input_check_response(
    check = checkmate::check_tibble(object, min.rows = 1, min.cols = 1),
    var_name = "object"
  )
  cols <- colnames(object)

  ### build summary
  n <- nrow(object)
  summary <- list(
    "iterations" = n - 1,
    "start_value" = object[[1, "value"]],
    "end_value" = object[[n, "value"]],
    "seconds_total" = sum(object[, "seconds"], na.rm = TRUE),
    "start_parameter" = object[[1, "parameter"]][[1]],
    "end_parameter" = object[[n, "parameter"]][[1]]
  )

  ### return
  structure(summary, class = c("summary.trackopt", "list"))

}

#' @keywords internal
#' @exportS3Method print summary.trackopt

print.summary.trackopt <- function(x, ...) {
  cli::cat_line(
    "Iterations: ", x$iterations,
    "\nFunction improvement: ", sprintf("%.4g -> %.4g", x$start_value, x$end_value),
    "\nComputation time: ", sprintf("%.4g seconds", x$seconds_total),
    "\nInitial parameter: ", paste(sprintf("%.4g", x$start_parameter), collapse = ", "),
    "\nFinal parameter: ", paste(sprintf("%.4g", x$end_parameter), collapse = ", ")
  )
  invisible(x)
}

#' @rdname nlm_track
#'
#' @param xlim,xlim2 \[`numeric(2)`\]\cr
#' Ranges for the first and second parameter to plot.
#'
#' If `NULL`, they are derived from the parameter ranges in `object`.
#'
#' @param iteration \[`integer(1)`\]\cr
#' The iteration to plot.
#'
#' If `NULL`, the last iteration is plotted.
#'
#' This option is useful for creating animations, see
#' \url{https://bookdown.org/yihui/rmarkdown-cookbook/animation.html#ref-R-animation}.
#'
#' @export

autoplot.trackopt <- function(
    object, iteration = NULL, xlim = NULL, xlim2 = NULL, ...
  ) {

  ### input checks
  oeli::input_check_response(
    check = oeli::check_missing(object),
    var_name = "object"
  )
  oeli::input_check_response(
    check = checkmate::check_class(object, "trackopt"),
    var_name = "object"
  )
  npar <- attr(object, "npar")
  oeli::input_check_response(
    check = checkmate::check_count(npar, positive = TRUE),
    prefix = "Attribute {.var npar} of {.var object} is bad:"
  )
  iterations <- object$iteration
  oeli::input_check_response(
    check = checkmate::check_integerish(
      iterations, lower = 0, any.missing = FALSE, min.len = 0, unique = TRUE,
      sorted = TRUE
    ),
    prefix = "Element {.var object$iteration} is bad:"
  )
  if (is.null(iteration)) {
    iteration <- max(iterations)
  }
  oeli::input_check_response(
    check = checkmate::check_int(iteration, lower = 0, upper = max(iterations)),
    var_name = "iteration"
  )
  oeli::input_check_response(
    check = checkmate::check_numeric(
      xlim, any.missing = FALSE, len = 2, sorted = TRUE, null.ok = TRUE,
      finite = TRUE
    ),
    var_name = "xlim"
  )
  oeli::input_check_response(
    check = checkmate::check_numeric(
      xlim2, any.missing = FALSE, len = 2, sorted = TRUE, null.ok = TRUE,
      finite = TRUE
    ),
    var_name = "xlim2"
  )

  ### call 1D or 2D autoplot
  if (npar == 1) {
    autoplot.trackopt_1d(
      object,
      iteration = iteration,
      xlim = xlim,
      ...
    )
  } else if (npar == 2) {
    autoplot.trackopt_2d(
      object,
      iteration = iteration,
      xlim = xlim,
      xlim2 = xlim2,
      ...
    )
  } else {
    cli::cli_warn("Currently not available for more than 2 parameter")
    return(NULL)
  }

}

#' @keywords internal

autoplot.trackopt_1d <- function(object, iteration, xlim, ...) {
  if (is.null(xlim)) {
    range <- range(object$parameter)
    xlim <- c(floor(range[1]), ceiling(range[2]))
  }
  objective <- attr(object, "objective")
  objective_name <- objective$objective_name
  target_name <- objective$target
  f <- function(x) objective$evaluate(x)
  data <- object[seq_len(iteration + 1), ]
  iteration_info <- paste("Iteration:", iteration)
  gradient_value <- data[[iteration + 1, "gradient"]]
  gradient_info <- NULL
  hessian_info <- NULL
  if (checkmate::test_number(gradient_value)) {
    gradient_info <- paste("\nGradient:", format(round(gradient_value, 2), scientific = FALSE))
    hessian_value <- data[[iteration + 1, "hessian"]]
    if (checkmate::test_number(hessian_value)) {
      hessian_info <- paste("\nHessian:", format(round(hessian_value, 2), scientific = FALSE))
    }
  }
  ggplot2::ggplot() +
    ggplot2::geom_function(fun = function(x) sapply(x, f), xlim = xlim) +
    ggplot2::geom_point(
      data = data,
      ggplot2::aes(x = parameter, y = value), color = "red", size = 2
    ) +
    ggplot2::geom_path(
      data = data,
      ggplot2::aes(x = parameter, y = value), color = "red"
    ) +
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::labs(
      x = target_name,
      y = paste0(objective_name, "(", target_name, ")"),
      title = "Optimization path",
      subtitle = paste(iteration_info, gradient_info, hessian_info)
    )
}

#' @keywords internal

autoplot.trackopt_2d <- function(object, iteration, xlim, xlim2, ...) {
  if (is.null(xlim)) {
    range <- range(sapply(object$parameter, `[`, 1))
    xlim <- c(floor(range[1]), ceiling(range[2]))
  }
  if (is.null(xlim2)) {
    range <- range(sapply(object$parameter, `[`, 2))
    xlim2 <- c(floor(range[1]), ceiling(range[2]))
  }
  objective <- attr(object, "objective")
  objective_name <- objective$objective_name
  target_name <- objective$target
  f <- function(x, y) objective$evaluate(c(x, y))
  data <- object[seq_len(iteration + 1), ]
  x_seq <- seq(xlim[1], xlim[2], length.out = 100)
  y_seq <- seq(xlim2[1], xlim2[2], length.out = 100)
  grid <- expand.grid(x = x_seq, y = y_seq)
  grid$z <- apply(grid, 1, function(row) f(row["x"], row["y"]))
  param_matrix <- do.call(rbind, data$parameter)
  data_plot <- data.frame(x = param_matrix[, 1], y = param_matrix[, 2], value = data$value)
  iteration_info <- paste("Iteration:", iteration)
  gradient_info <- NULL
  hessian_info <- NULL
  gradient_value <- data[[iteration + 1, "gradient"]]
  if (checkmate::test_numeric(gradient_value, len = 2)) {
    grad_norm <- sqrt(sum(gradient_value^2))
    gradient_info <- paste("\nGradient norm:", format(round(grad_norm, 2), scientific = FALSE))
    hessian_value <- data[[iteration + 1, "hessian"]]
    if (checkmate::test_matrix(hessian_value, nrows = 2, ncols = 2)) {
      hessian_info <- paste("\nHessian determinant:", format(round(det(hessian_value), 2), scientific = FALSE))
    }
  }
  ggplot2::ggplot() +
    ggplot2::geom_contour_filled(
      data = grid,
      aes(x = x, y = y, z = z)
    ) +
    ggplot2::geom_point(
      data = data_plot,
      ggplot2::aes(x = x, y = y)
    ) +
    ggplot2::geom_path(
      data = data_plot,
      ggplot2::aes(x = x, y = y)
    ) +
    ggplot2::scale_x_continuous(limits = xlim) +
    ggplot2::scale_y_continuous(limits = xlim2) +
    ggplot2::labs(
      x = paste0(target_name, "[1]"),
      y = paste0(target_name, "[2]"),
      fill = paste0(objective_name, "(", target_name, ")"),
      title = "Optimization path",
      subtitle = paste(iteration_info, gradient_info, hessian_info)
    )
}
