#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate check_class
#' @importFrom checkmate check_count
#' @importFrom checkmate check_flag
#' @importFrom checkmate check_function
#' @importFrom checkmate check_int
#' @importFrom checkmate check_integerish
#' @importFrom checkmate check_list
#' @importFrom checkmate check_number
#' @importFrom checkmate check_numeric
#' @importFrom checkmate check_tibble
#' @importFrom checkmate test_matrix
#' @importFrom checkmate test_number
#' @importFrom checkmate test_numeric
#' @importFrom cli cat_line
#' @importFrom cli cat_print
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_h1
#' @importFrom cli cli_h3
#' @importFrom cli cli_warn
#' @importFrom cli style_hyperlink
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 autoplot
#' @importFrom ggplot2 geom_contour_filled
#' @importFrom ggplot2 geom_function
#' @importFrom ggplot2 geom_path
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom oeli check_missing
#' @importFrom oeli check_numeric_vector
#' @importFrom oeli input_check_response
#' @importFrom optimizeR Objective
#' @importFrom optimizeR Optimizer
#' @importFrom stats nlm
#' @importFrom stats optim
#' @importFrom tibble tibble
#' @importFrom utils globalVariables
#' @importFrom utils packageVersion
## usethis namespace: end
NULL

#' @keywords internal

assign_to_cell <- function(tb, row, col, value) {
  if (length(value) == 1) {
    tb[row, col] <- value
  } else {
    if (col %in% colnames(tb) && !is.list(tb[[col]])) {
      tb[[col]] <- lapply(tb[[col]], identity)
    }
    tb[[row, col]] <- list(value)
  }
  tb
}

#' @keywords internal

.onAttach <- function(lib, pkg) {
  doc_link <- "https://loelschlaeger.de/trackopt"
  issues_link <- "https://github.com/loelschlaeger/trackopt/issues"
  msg <- c(
    paste0("This is {trackopt} ", utils::packageVersion("trackopt")),
    ", happy optimization tracking!\n",
    "Documentation? ",
    cli::style_hyperlink(doc_link, doc_link), "\n",
    "Any issues? ",
    cli::style_hyperlink(issues_link, issues_link)
  )
  packageStartupMessage(msg)
  invisible()
}

utils::globalVariables(c("parameter", "value", "x", "y", "z"))
