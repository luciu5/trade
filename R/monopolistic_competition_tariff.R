#'Tariff Simulation With A Monopolistic Competition Pricing Game
#'
#' Simulate the effect of tariffs when firms play a Monopolistic Competition game and consumer demand is either Logit or CES
#'
#' @param demand A character vector indicating which demand system to use. Currently allows ``logit" or ``ces" .
#' @param prices  A length k vector product prices. Default is missing, in which case demand intercepts are not calibrated.
#' @param quantities A length k vector of product quantities.
#' @param margins A length k vector of product margins. All margins must be either be between 0 and 1, or NA.
#' @param mktElast A negative number no greater than -1 equal to the industry pre-tariff price elasticity. Default is NA .
#' @param mktSize  A positive number equal to the industry pre-tariff market size. Market size equals total quantity sold,\emph{including sales to the outside good}.
#' @param tariffPre  A vector of length k where each element equals the \strong{current} \emph{ad valorem} tariff
#' (expressed as a proportion of the consumer price) imposed on each product. Default is 0, which assumes no tariff.
#' @param tariffPost  A vector of length k where each element equals the \strong{new}  \emph{ad valorem} tariff
#' (expressed as a proportion of the consumer price) imposed on each product. Default is 0, which assumes no tariff.
#' @param priceOutside price of the outside good. Default 0 for logit and 1 for ces. Not used for aids.
#' @param labels A k-length vector of labels.
#'
#' @details
#'
#' Let k denote the number of products produced by all firms.
#' Using price, and quantity, information for all products
#'in each market, as well as margin information for at least
#' one products in each market, \code{monopolistic_competition_tariff} is able to
#' recover the slopes and intercepts of a Logit demand
#' system. These parameters are then used to simulate the price
#' effects of an \emph{ad valorem} tariff under the assumption that the firms are playing a monopolisitcally competitive pricing game
#'
#' @seealso \code{\link{bertrand_tariff}} to simulate the effects of a tariff under a Bertrand pricing game.
#'
#'
#' @return \code{monopolistic_competition_tariff} returns an instance of class \code{\linkS4class{TariffMonComLogit}} , depending upon the value of the ``demand'' argument.
#' @references Simon P. Anderson, Andre de Palma, Brent Kreider, Tax incidence in differentiated product oligopoly,
#' Journal of Public Economics, Volume 81, Issue 2, 2001, Pages 173-192.
#' Anderson, Simon P., and André De Palma. Economic distributions and primitive distributions in monopolistic competition. Centre for Economic Policy Research, 2015.
#' @examples
#' ## Calibration and simulation results from a 10% tariff on non-US beers "OTHER-LITE"
#' ## and "OTHER-REG"
#' ## Source: Epstein/Rubenfeld 2004, pg 80
#'
#' prodNames <- c("BUD","OLD STYLE","MILLER","MILLER-LITE","OTHER-LITE","OTHER-REG")
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
#' result.logit <- monopolistic_competition_tariff(demand = "logit",prices=price,quantities=quantities,
#'                                 margins = margins,
#'                                  tariffPost = tariff, labels=prodNames)
#'
#' print(result.logit)           # return predicted price change
#' summary(result.logit)         # summarize merger simulation
#'
#' result.ces <- monopolistic_competition_tariff(demand = "ces",prices=price,quantities=quantities,
#'                                 margins = margins,
#'                                  tariffPost = tariff, labels=prodNames)
#'
#' print(result.ces)           # return predicted price change
#' summary(result.ces)         # summarize merger simulation
#'
#' @include ps-methods.R summary-methods.R
#' @export


monopolistic_competition_tariff <- function(
  demand = c("logit","ces"),
  prices,quantities,margins,
  mktElast = NA_real_,
  mktSize,
  tariffPre=rep(0,length(quantities)),
  tariffPost=rep(0,length(quantities)),
  priceOutside=ifelse(demand== "logit",0, 1),
  labels=paste("Prod",1:length(quantities),sep="")
  ){


demand <- match.arg(demand)


nprods <- length(quantities)

insideSize = ifelse(demand == "logit",sum(quantities,na.rm=TRUE), sum(prices*quantities,na.rm=TRUE))

if(missing(mktSize)) mktSize <-  insideSize
else{
  if(demand == "ces"){
    mktSize <-  sum(quantities*prices) + (mktSize - sum(quantities,na.rm=TRUE))*priceOutside
  }
}

if(length(mktSize)>1) stop("'mktSize' must be length 1")


subset= rep(TRUE,nprods)

tariffPre[is.na(tariffPre)] <- 0
tariffPost[is.na(tariffPost)] <- 0


owner <-  diag(nprods)

mcDelta <- (tariffPost - tariffPre)/(1 - tariffPost)

if(demand == "logit"){ shares <-  quantities/mktSize}
else {shares <- prices*quantities/mktSize}

if(any(shares>1)) {stop("some shares are greater than 1. Check that 'mktSize' is set correctly.")}







result <-   switch(demand,

         logit=  new("TariffMonComLogit",prices=prices, shares=shares,
                     margins=margins,
                     ownerPre=owner,
                     ownerPost=owner,
                     mktElast = mktElast,
                     mcDelta=mcDelta,
                     subset=subset,
                     priceOutside=priceOutside,
                     priceStart=prices,
                     normIndex=ifelse(sum(shares) == 1,1,NA),
                     shareInside= sum(shares),
                     tariffPre=tariffPre,
                     tariffPost=tariffPost,
                     insideSize = insideSize,
                     mktSize = mktSize,
                     labels=labels),
         ces=  new("TariffMonComCES",prices=prices, shares=shares,
                     margins=margins,
                     ownerPre=owner,
                     ownerPost=owner,
                     mktElast = mktElast,
                     mcDelta=mcDelta,
                     subset=subset,
                     priceOutside=priceOutside,
                     priceStart=prices,
                     normIndex=ifelse(sum(shares) == 1,1,NA),
                     shareInside= sum(shares),
                     tariffPre=tariffPre,
                     tariffPost=tariffPost,
                     insideSize = insideSize,
                     mktSize = mktSize,
                     labels=labels)

  )






## Convert ownership vectors to ownership matrices
result@ownerPre  <- owner
result@ownerPost <- owner

## Calculate Demand Slope Coefficients
result <- calcSlopes(result)

## Calculate marginal cost
result@mcPre <-  calcMC(result,TRUE)
result@mcPost <- calcMC(result,FALSE)

## Solve Non-Linear System for Price Changes
result@pricePre  <- calcPrices(result,preMerger=TRUE)
result@pricePost <- calcPrices(result,preMerger=FALSE)

return(result)

}
