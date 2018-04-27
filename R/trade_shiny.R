#'A Shiny Interface to the trade Package
#'
#' Launch a shiny interface to simulate the effects of tariffs
#'
#' @param demand A character vector indicating which demand system to use. Currently allows logit (default), ces, or aids.
#' @param prices  A length k vector product prices. Default is missing, in which case demand intercepts are not calibrated.
#' @param quantities A length k vector of product quantities.
#' @param margins A length k vector of product margins. All margins must be either be between 0 and 1, or NA.
#' @param owner EITHER a vector of length k whose values indicate which firm produced a product before the merger OR a k x k matrix of pre-merger ownership shares.
#' @param diversions  A k x k matrix of diversion ratios with diagonal elements equal to -1. Default is missing, in which case diversion according to revenue share is assumed.
#' @param mktElast A negative number equal to the industry pre-merger price elasticity. Default is NA .
#' @param insideSize Size of all units included in the market. For logit, this defaults to total quantity, while for aids and ces this defaults to total revenues.
#' @param tariff  A vector of length k where each element equals the \emph{ad valorem} tariff (expressed as a proportion) imposed on each product. Default is 0, which assumes no tariff.
#' @param parmStart \code{aids} only. A vector of length 2 who elements equal to an initial guess for "known" element of the diagonal of the demand matrix and the market elasticity.
#' @param priceOutside A vector of length k who elements equal to an initial guess of the proportional change in price caused by the merger. For aids, the default is to draw k random elements from a [0,1] uniform distribution. For ces and logit, the default is prices.
#' @param isMax  If TRUE, checks to see whether computed price equilibrium locally maximizes firm profits and returns a warning if not. Default is FALSE.
#' @param control.slopes A list of  \code{\link{optim}}  control parameters passed to the calibration routine optimizer (typically the \code{calcSlopes} method).
#' @param control.equ A list of  \code{\link[BB]{BBsolve}} control parameters passed to the non-linear equation solver (typically the \code{calcPrices} method).
#' @param labels A k-length vector of labels.
#' @param ... Additional options to feed to the \code{\link[BB]{BBsolve}} optimizer used to solve for equilibrium prices.
#'
#' @details
#'
#' \code{trade_shiny} launches a shiny interface for the antitrust package. The shiny interface provides users with the
#' ability to calibrate model parameters and simulate tariff effects using many of the supply and demand models
#'  included in the \code{\link{trade}} package.
#'
#'
#' @return \code{bertrand_tariff} returns an instance of class \code{\linkS4class{LogitALM}}, \code{\linkS4class{CESALM}}, or \code{\linkS4class{AIDS}} from package \code{\link{antitrust}}, depending upon the value of the ``demand'' argument.
#'
#' @examples
#' ## Calibration and simulation results from a 10% tariff on non-US beers "OTHER-LITE"
#' ## and "OTHER-REG"
#' ## Source: Epstein/Rubenfeld 2004, pg 80
#'
#' prodNames <- c("BUD","OLD STYLE","MILLER","MILLER-LITE","OTHER-LITE","OTHER-REG")
#' owner <-c("BUD","OLD STYLE","MILLER","MILLER","OTHER-LITE","OTHER-REG")
#' price    <- c(.0441,.0328,.0409,.0396,.0387,.0497)
#' quantities   <- c(.066,.172,.253,.187,.099,.223)*100
#' margins <- c(.3830,.5515,.5421,.5557,.4453,.3769)
#' tariff <- c(0,0,0,0,.1,.1)
#'
#' names(price) <-
#'  names(quantities) <-
#'  names(margins) <-
#'  prodNames
#'
#'
#' result.logit <- bertrand_tariff(demand = "logit",prices=price,quantities=quantities,margins = margins,owner=owner, tariff = tariff, labels=prodNames)
#'
#' print(result.logit)           # return predicted price change
#' summary(result.logit)         # summarize merger simulation
#'
#' @export


trade_shiny <- function() {
  shiny::runApp(system.file('trade_shiny', package='trade'))
}
