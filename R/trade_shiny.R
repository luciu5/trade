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
#'
#' @export


trade_shiny <- function() {
  requireNamespace("rhandsontable")
  shiny::runApp(system.file('trade_shiny', package='trade'))
}
