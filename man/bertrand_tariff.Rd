% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bertrand_tariff.R
\name{bertrand_tariff}
\alias{bertrand_tariff}
\title{Tariff Simulation With A Bertrand Pricing Game}
\usage{
bertrand_tariff(
  demand = c("logit", "ces", "aids"),
  prices,
  quantities,
  margins,
  owner = NULL,
  mktElast = NA_real_,
  diversions,
  tariffPre = rep(0, length(quantities)),
  tariffPost = rep(0, length(quantities)),
  priceOutside = ifelse(demand == "logit", 0, 1),
  priceStart,
  isMax = FALSE,
  parmStart,
  control.slopes,
  control.equ,
  labels = paste("Prod", 1:length(quantities), sep = ""),
  ...
)
}
\arguments{
\item{demand}{A character vector indicating which demand system to use. Currently allows logit (default), ces, or aids.}

\item{prices}{A length k vector product prices. Default is missing, in which case demand intercepts are not calibrated.}

\item{quantities}{A length k vector of product quantities.}

\item{margins}{A length k vector of product margins. All margins must be either be between 0 and 1, or NA.}

\item{owner}{EITHER a vector of length k whose values indicate which firm produced a product before the tariff OR a k x k matrix of pre-merger ownership shares.}

\item{mktElast}{A negative number equal to the industry pre-merger price elasticity. Default is NA .}

\item{diversions}{A k x k matrix of diversion ratios with diagonal elements equal to -1. Default is missing, in which case diversion according to revenue share is assumed.}

\item{tariffPre}{A vector of length k where each element equals the \strong{current} \emph{ad valorem} tariff
(expressed as a proportion of the consumer price) imposed on each product. Default is 0, which assumes no tariff.}

\item{tariffPost}{A vector of length k where each element equals the \strong{new}  \emph{ad valorem} tariff
(expressed as a proportion of the consumer price) imposed on each product. Default is 0, which assumes no tariff.}

\item{priceOutside}{price of the outside good. Equals 0 for logit and 1 for ces. Not used for aids.}

\item{priceStart}{For aids, a vector of length k who elements equal to an initial guess of the proportional change in price caused by the merger.
The default is to draw k random elements from a [0,1] uniform distribution. For ces and logit, the default is prices.}

\item{isMax}{If TRUE, checks to see whether computed price equilibrium locally maximizes firm profits and returns a warning if not. Default is FALSE.}

\item{parmStart}{\code{aids} only. A vector of length 2 whose elements equal to an initial guess for each "known" element of the diagonal of the demand matrix and the market elasticity.}

\item{control.slopes}{A list of  \code{\link{optim}}  control parameters passed to the calibration routine optimizer (typically the \code{calcSlopes} method).}

\item{control.equ}{A list of  \code{\link[BB]{BBsolve}} control parameters passed to the non-linear equation solver (typically the \code{calcPrices} method).}

\item{labels}{A k-length vector of labels.}

\item{...}{Additional options to feed to the \code{\link[BB]{BBsolve}} optimizer used to solve for equilibrium prices.}
}
\value{
\code{bertrand_tariff} returns an instance of class \code{\linkS4class{TariffLogit}}, \code{\linkS4class{TariffCES}}, or \code{\linkS4class{TariffAIDS}}, depending upon the value of the ``demand'' argument.
}
\description{
Simulate the effect of tariffs when firms play a Bertrand pricing game and consumer demand is either Logit, CES, or AIDS
}
\details{
Let k denote the number of products produced by all firms.
Using price, and quantity, information for all products
in each market, as well as margin information for at least
one products in each market, \code{bertrand_tariff} is able to
recover the slopes and intercepts of either a Logit, CES, or AIDS demand
system. These parameters are then used to simulate the price
effects of an \emph{ad valorem} tariff under the assumption that the firms are playing a
simultaneous price setting game.
}
\examples{
\donttest{
## Calibration and simulation results from a 10\% tariff on non-US beers "OTHER-LITE"
## and "OTHER-REG"
## Source: Epstein/Rubenfeld 2004, pg 80

prodNames <- c("BUD","OLD STYLE","MILLER","MILLER-LITE","OTHER-LITE","OTHER-REG")
owner <-c("BUD","OLD STYLE","MILLER","MILLER","OTHER-LITE","OTHER-REG")
price    <- c(.0441,.0328,.0409,.0396,.0387,.0497)
quantities   <- c(.066,.172,.253,.187,.099,.223)*100
margins <- c(.3830,.5515,.5421,.5557,.4453,.3769)
tariff <- c(0,0,0,0,.1,.1)

names(price) <-
 names(quantities) <-
 names(margins) <-
 prodNames


result.logit <- bertrand_tariff(demand = "logit",prices=price,quantities=quantities,
                                margins = margins,owner=owner,
                                 tariffPost = tariff, labels=prodNames)

print(result.logit)           # return predicted price change
summary(result.logit)         # summarize merger simulation
}
}
\references{
Simon P. Anderson, Andre de Palma, Brent Kreider, Tax incidence in differentiated product oligopoly,
Journal of Public Economics, Volume 81, Issue 2, 2001, Pages 173-192.
}
\seealso{
\code{\link{monopolistic_competition_tariff}} to simulate the effects of a tariff under monopolistic competition.
}
