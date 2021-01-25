#'A Shiny Interface to the trade Package
#'
#' Launch a shiny interface to simulate the effects of tariffs
#'

#' @details
#'
#' \code{trade_shiny} launches a shiny interface for the \code{trade} package. The shiny interface provides users with the
#' ability to calibrate model parameters and simulate tariff effects using many of the supply and demand models
#'  included in the \code{trade} package.
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
#' @export


trade_shiny <- function() {
  .Deprecated("ct_shiny' in the 'competitiontoolbox' package")
  # requireNamespace("rhandsontable")
  # shiny::runApp(system.file('trade_shiny', package='trade'))
}
