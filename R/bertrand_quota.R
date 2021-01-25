#'quota Simulation With A Bertrand Pricing Game
#'
#' Simulate the effect of quotas when firms play a Bertrand pricing game and consumer demand is either Logit, CES, or AIDS
#'
#' @param demand A character vector indicating which demand system to use. Currently allows logit (default), ces, or aids.
#' @param prices  A length k vector product prices. Default is missing, in which case demand intercepts are not calibrated.
#' @param quantities A length k vector of product quantities.
#' @param margins A length k vector of product margins. All margins must be either be between 0 and 1, or NA.
#' @param owner EITHER a vector of length k whose values indicate which firm produced a product before the merger OR a k x k matrix of pre-merger ownership shares.
#' @param diversions  A k x k matrix of diversion ratios with diagonal elements equal to -1. Default is missing, in which case diversion according to revenue share is assumed.
#' @param mktElast A negative number equal to the industry pre-merger price elasticity. Default is NA .
#' @param quotaPre  A vector of length k where each element equals the \strong{current}  quota (expressed as a proportion of pre-merger quantities)
#'                  imposed on each product. Default is Inf, which assumes no quota.
#' @param quotaPost  A vector of length k where each element equals the \strong{new}  quota
#' (expressed as a proportion of pre-merger quantities) imposed on each product. Default is Inf, which assumes no quota.
#' @param parmStart \code{aids} only. A vector of length 2 who elements equal to an initial guess for "known" element of the diagonal of the demand matrix and the market elasticity.
#' @param priceStart For aids, a vector of length k who elements equal to an initial guess of the proportional change in price caused by the merger.
#'  The default is to draw k random elements from a [0,1] uniform distribution. For ces and logit, the default is prices.
#' @param priceOutside price of the outside good. Equals 0 for logit and 1 for ces. Not used for aids.
#' @param isMax  If TRUE, checks to see whether computed price equilibrium locally maximizes firm profits and returns a warning if not. Default is FALSE.
#' @param control.slopes A list of  \code{\link{optim}}  control parameters passed to the calibration routine optimizer (typically the \code{calcSlopes} method).
#' @param control.equ A list of  \code{\link[BB]{BBsolve}} control parameters passed to the non-linear equation solver (typically the \code{calcPrices} method).
#' @param labels A k-length vector of labels.
#' @param ... Additional options to feed to the \code{\link[BB]{BBsolve}} optimizer used to solve for equilibrium prices.
#'
#' @details
#'
#' Let k denote the number of products produced by all firms.
#' Using price, and quantity, information for all products
#'in each market, as well as margin information for at least
#' one products in each market, \code{bertrand_quota} is able to
#' recover the slopes and intercepts of the Logit, demand
#' system. These parameters are then used to simulate the price
#' effects of a quota under the assumption that the firms are playing a
#' simultaneous price setting game.
#'
#'
#'
#' @return \code{bertrand_quota} returns an instance of class \code{\linkS4class{QuotaLogit}}.
#' @references Simon P. Anderson, Andre de Palma, Brent Kreider, Tax incidence in differentiated product oligopoly,
#' Journal of Public Economics, Volume 81, Issue 2, 2001, Pages 173-192.
#' @examples
#' \donttest{
#' ## Calibration and simulation results from a 80% quota on non-US beers "OTHER-LITE"
#' ## and "OTHER-REG"
#' ## Source: Epstein/Rubenfeld 2004, pg 80
#'
#' prodNames <- c("BUD","OLD STYLE","MILLER","MILLER-LITE","OTHER-LITE","OTHER-REG")
#' owner <-c("BUD","OLD STYLE","MILLER","MILLER","OTHER-LITE","OTHER-REG")
#' price    <- c(.0441,.0328,.0409,.0396,.0387,.0497)
#' quantities   <- c(.066,.172,.253,.187,.099,.223)*100
#' margins <- c(.3830,.5515,.5421,.5557,.4453,.3769)
#' quota <- c(Inf,Inf,Inf,Inf,.8,.8)
#'
#' names(price) <-
#'  names(quantities) <-
#'  names(margins) <-
#'  prodNames
#'
#'
#' result.logit <- bertrand_quota(demand = "logit",prices=price,quantities=quantities,
#'                                 margins = margins,owner=owner, quotaPost = quota, labels=prodNames)
#'
#' print(result.logit)           # return predicted price change
#' summary(result.logit)         # summarize merger simulation
#' }
#' @include ps-methods.R summary-methods.R
#' @export


bertrand_quota <- function(
  demand = c("logit"),
  prices,quantities,margins,
  owner=NULL,
  mktElast = NA_real_,
  diversions,
  quotaPre=rep(Inf,length(quantities)),
  quotaPost,
  priceOutside=ifelse(demand== "logit",0, 1),
  priceStart,
  isMax=FALSE,
  parmStart,
  control.slopes,
  control.equ,
  labels=paste("Prod",1:length(quantities),sep=""),
  ...){


demand <- match.arg(demand)


nprods <- length(quantities)

subset= rep(TRUE,nprods)

insideSize = sum(quantities,na.rm=TRUE)

quotaPre[is.na(quotaPre)] <- Inf
quotaPost[is.na(quotaPost)] <- Inf

capacitiesPre <- quotaPre*quantities
capacitiesPost <- quotaPost*quantities

if(is.null(owner)){

    warning("'owner' is NULL. Assuming each product is owned by a single firm.")
  ownerPre <-  diag(nprods)

}


ownerPre <- ownerPost <- owner

mcDelta <- rep(0, length(quantities))

shares_revenue <- shares_quantity <- quantities/sum(quantities)



if(all(!is.na(prices))) shares_revenue <- prices*shares_quantity/sum(prices*shares_quantity)

if(demand == "aids"){

  if(missing(prices)){ prices <- rep(NA_real_,nprods)}

  if(missing(parmStart)) parmStart <- rep(NA_real_,2)

  if(missing(priceStart)) priceStart <- runif(nprods)

  if(missing(diversions)){
    diversions <- tcrossprod(1/(1-shares_revenue),shares_revenue)
    diag(diversions) <- -1


  }

}

else if (demand %in% c("logit","ces")){

  if(missing(parmStart)){
    parmStart <- rep(.1,2)
    nm <- which(!is.na(margins))[1]
    if(demand == "logit"){
    parmStart[1] <- -1/(margins[nm]*prices[nm]*(1-shares_quantity[nm])) #ballpark alpha for starting values
    }
    else{parmStart[1] <- 1/(margins[nm]*(1-shares_revenue[nm])) - shares_revenue[nm]/(1-shares_revenue[nm])} #ballpark gamma for starting values
    }
  if(missing(priceStart)) priceStart <- prices
}






result <-   switch(demand,
         aids=new("QuotaAIDS",shares=shares_revenue,mcDelta=mcDelta,subset=subset,
                  margins=margins, prices=prices, quantities=shares_revenue,  mktElast = mktElast,
                  insideSize = insideSize,
                  ownerPre=ownerPre,ownerPost=ownerPost, parmStart=parmStart,
                  diversion=diversions,
                  capacitiesPre=capacitiesPre,
                  capacitiesPost=capacitiesPost,
                  priceStart=priceStart,labels=labels),

         logit=  new("QuotaLogit",prices=prices, shares=shares_quantity,
                     margins=margins,
                     ownerPre=ownerPre,
                     ownerPost=ownerPost,
                     mktElast = mktElast,
                     mcDelta=mcDelta,
                     subset=subset,
                     priceOutside=priceOutside,
                     priceStart=priceStart,
                     shareInside= sum(shares_quantity),
                     parmsStart=parmStart,
                     capacitiesPre=capacitiesPre,
                     capacitiesPost=capacitiesPost,
                     quotaPre = quotaPre,
                     quotaPost= quotaPost,
                     insideSize = insideSize,
                     labels=labels),

         ces = new("QuotaCES",prices=prices, shares=shares_revenue,
                   margins=margins,
                   ownerPre=ownerPre,
                   ownerPost=ownerPost,
                   mktElast = mktElast,
                   mcDelta=mcDelta,
                   subset=subset,
                   priceOutside=priceOutside,
                   priceStart=priceStart,
                   shareInside=sum(shares_revenue),
                   parmsStart=parmStart,
                   insideSize =insideSize,
                   capacitiesPre=capacitiesPre,
                   capacitiesPost=capacitiesPost,
                   labels=labels)
  )






if(!missing(control.slopes)){
  result@control.slopes <- control.slopes
}
if(!missing(control.equ)){
  result@control.equ <- control.equ
}



## Convert ownership vectors to ownership matrices
result@ownerPre  <- ownerToMatrix(result,TRUE)
result@ownerPost <- ownerToMatrix(result,FALSE)

## Calculate Demand Slope Coefficients
result <- calcSlopes(result)

## Solve Non-Linear System for Price Changes (AIDS only)
if (demand == "aids"){
result@priceDelta <- calcPriceDelta(result,isMax=isMax,subset=subset,...)
}


## Calculate marginal cost
result@mcPre <-  calcMC(result,TRUE)
result@mcPost <- calcMC(result,FALSE)



## Solve Non-Linear System for Price Changes
result@pricePre  <- calcPrices(result,preMerger=TRUE,isMax=isMax,...)
result@pricePost <- calcPrices(result,preMerger=FALSE,isMax=isMax,subset=subset,...)

return(result)

}
