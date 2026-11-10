# Feature-local dependency discovery: ordinary trade use does not load the GUI.
.trade_toolbox_launcher <- function(
    available = requireNamespace("competitiontoolbox", quietly = TRUE),
    version = if (available) utils::packageVersion("competitiontoolbox") else NULL) {
  if (!available || version < "0.9.0") {
    stop(paste("The current GUI requires competitiontoolbox >= 0.9.0.",
      "Install it with the public installer:",
      'source("https://raw.githubusercontent.com/luciu5/competitiontoolbox/master/tools/install_iovrse.R");',
      'install_iovrse(packages = "competitiontoolbox")'), call. = FALSE)
  }
  getExportedValue("competitiontoolbox", "ct_shiny")
}

#'A Shiny Interface to the trade Package
#'
#' Launch the current competitiontoolbox interface
#'

#' @details
#'
#' This deprecated launcher forwards to \code{competitiontoolbox::ct_shiny()}.
#' Install competitiontoolbox 0.9.0 or newer using its public installer.
#' The UI remains an optional dependency of trade.
#'
#' @name trade_shiny-deprecated
#' @seealso \code{\link{trade-deprecated}}
#' @keywords internal
NULL

#' @rdname trade-deprecated
#' @section \code{trade_shiny}:
#' For \code{trade_shiny}, use \code{\link[competitiontoolbox]{ct_shiny}}.
#'
#'
#' @param ... Arguments passed to \code{competitiontoolbox::ct_shiny()}.
#' @export


trade_shiny <- function(...) {
  .Deprecated("competitiontoolbox::ct_shiny")
  .trade_toolbox_launcher()(...)
}
