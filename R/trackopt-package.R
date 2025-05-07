#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom checkmate check_class
#' @importFrom checkmate check_count
#' @importFrom checkmate check_flag
#' @importFrom checkmate check_function
#' @importFrom checkmate check_number
#' @importFrom checkmate check_subset
#' @importFrom checkmate check_tibble
#' @importFrom cli cat_line
#' @importFrom cli cat_print
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_alert_success
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_h1
#' @importFrom cli cli_h3
#' @importFrom cli cli_warn
#' @importFrom cli style_hyperlink
#' @importFrom ggplot2 autoplot
#' @importFrom oeli check_missing
#' @importFrom oeli check_numeric_vector
#' @importFrom oeli input_check_response
#' @importFrom optimizeR Objective
#' @importFrom optimizeR Optimizer
#' @importFrom stats nlm
#' @importFrom tibble tibble
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
