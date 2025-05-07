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
  iterations <- n - 1
  start_value <- end_value <- NA_real_
  if ("value" %in% cols) {
    start_value <- object[[1, "value"]]
    end_value <- object[[n, "value"]]
  }
  seconds_total <- NA_real_
  if ("seconds" %in% cols) {
    seconds_total <- sum(object[, "seconds"], na.rm = TRUE)
  }
  start_parameter <- NA_real_
  end_parameter <- NA_real_
  if ("parameter" %in% cols) {
    start_parameter <- object[[1, "parameter"]][[1]]
    end_parameter <- object[[n, "parameter"]][[1]]
  }
  summary <- list(
    "iterations" = iterations,
    "start_value" = start_value,
    "end_value" = end_value,
    "seconds_total" = seconds_total,
    "start_parameter" = start_parameter,
    "end_parameter" = end_parameter
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
    "\nSeconds: ", sprintf("%.4g", x$seconds_total),
    "\nInitial parameter: ", paste(sprintf("%.4g", x$start_parameter), collapse = ", "),
    "\nFinal parameter: ", paste(sprintf("%.4g", x$end_parameter), collapse = ", ")
  )
  invisible(x)
}

#' @rdname nlm_track
#' @export

autoplot.trackopt <- function(object, ...) {

  ### input checks
  oeli::input_check_response(
    check = oeli::check_missing(object),
    var_name = "object"
  )
  oeli::input_check_response(
    check = checkmate::check_class(object, "trackopt"),
    var_name = "object"
  )

  ### pass on
  if (inherits(object, "trackopt1d")) {
    autoplot.trackopt1d(object, ...)
  } else {
    cli::cli_warn(
      "Currently not available for more than 1 parameter"
    )
  }

}

#' @keywords internal
#' @exportS3Method ggplot2::autoplot trackopt1d

autoplot.trackopt1d <- function(object, xlim, r, ...) {
  animate <- T
  range <- range(object$parameter)
  xlim <- c(floor(range[1]), ceiling(range[2]))
  objective <- attr(object, "objective")
  objective_name <- objective$objective_name
  target_name <- objective$target
  data <- object
  suppressMessages(print(
    ggplot() +
      geom_function(
        fun = function(x) sapply(x, objective$evaluate), xlim = xlim
      ) +
      geom_point(
        data = data[1:r, ],
        aes(x = parameter, y = value), color = "red", size = 2
      ) +
      geom_path(
        data = data[1:r, ],
        aes(x = parameter, y = value), color = "red"
      ) +
      labs(
        x = target_name,
        y = paste0(objective_name, "(", target_name, ")"),
        title = "Optimization path",
        subtitle = paste(
          "Iteration:", r,
          "\nGradient", format(round(data[[r, "gradient"]], 2), scientific = FALSE),
          "\nHessian", format(round(data[[r, "hessian"]], 2), scientific = FALSE)
        )
      )
  ))
}
