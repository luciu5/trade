#' @title Methods For Implementing The Hypothetical Monopolist Test
#' @name defineMarketTools-methods
#' @docType methods
#' @aliases HypoMonTest
#' @description An adaptation of the Hypothetical Monopolist Test described in the 2010 Horizontal Merger Guidelines for use in non-merger settings.
#' @description \code{\link{HypoMonTest}} implements the Hypothetical Monopolist Test for a given \sQuote{ssnip}.
#' \sQuote{...} may be used to pass arguments to the optimizer.
#' @param object An instance of one of the classes listed above.
#' @param prodIndex A vector of product indices that are to be placed under the control of the Hypothetical Monopolist.
#' @param plantIndex A vector of plant indices that are to be placed under the control of the Hypothetical Monopolist (Cournot).
#' @param ssnip A number between 0 and 1 that equals the threshold for a \dQuote{Small but Significant and
#' Non-transitory Increase in Price} (SSNIP). Default is .05, or 5\%.
#' @param ... Pass options to the optimizer used to solve for equilibrium prices.
#'
#' @details
#' \code{HypoMonTest} is an implementation of the Hypothetical Monopolist Test
#' on the products indexed by \sQuote{prodIndex} for a \sQuote{ssnip}. The
#' Hypothetical Monopolist Test determines whether a profit-maximizing
#' Hypothetical Monopolist who controls the products indexed by
#' \sQuote{prodIndex} would increase the price of at least one of the products in \sQuote{prodIndex} by a
#' small, significant, and non-transitory amount (i.e. impose a SSNIP). The  main difference between this implementation and
#' \code{\link[antitrust:HypoMonTest]{antitrust::HypoMonTest()}} is this implementation does not check to see if \sQuote{prodIndex} contains a merging party's product.
#' @return
#' \code{HypoMonTest} returns TRUE if a profit-maximizing Hypothetical Monopolist who controls the products indexed by
#' \sQuote{prodIndex} would increase the price of at least one of the products in \sQuote{prodIndex} by a \sQuote{ssnip}, and
#' FALSE otherwise.
#'
#' @references U.S. Department of Justice and Federal Trade Commission,
#' \emph{Horizontal Merger Guidelines}. Washington DC: U.S. Department of Justice, 2010.
#' \url{https://www.justice.gov/atr/horizontal-merger-guidelines-08192010} (accessed July 29, 2011).
#'
#' @include ps-methods.R
#' @keywords methods
NULL



#'@rdname defineMarketTools-methods
#'@export
setMethod(
  f= "HypoMonTest",
  signature= "TariffBertrand",
  definition=function(object,prodIndex,ssnip=.05,...){

    ownerPre <- object@ownerPre
    nprods   <- ncol(ownerPre)
    pricesDelta <- rep(0,nprods)

    if(missing(prodIndex) || any(prodIndex>nprods | prodIndex <1 ) ){
      stop("'prodIndex' must be a vector of product indices between 1 and ",nprods)
    }

    if(length(ssnip)>1 || ssnip<0 | ssnip>1 ){stop("'ssnip' must be a number between 0 and 1")}



    pricesDelta[prodIndex] <-  calcPriceDeltaHypoMon(object,prodIndex,...)


    result <- max(pricesDelta) > ssnip

    return( result)
  }

)


#'@rdname defineMarketTools-methods
#'@export
setMethod(
  f= "HypoMonTest",
  signature= "TariffCournot",
  definition=function(object,plantIndex,prodIndex,ssnip=.05,...){

    ownerPre <- object@ownerPre
    nprods   <- ncol(ownerPre)
    nplants <- nrow(ownerPre)


    if(missing(plantIndex) || any(plantIndex>nplants | plantIndex <1 ) ){
      stop("'plantIndex' must be a vector of plant indices between 1 and ",nplants)
    }

    if(missing(prodIndex) || length(prodIndex) != 1 || any(prodIndex>nprods | prodIndex <1 ) ){
      stop("'prodIndex' must be between between 1 and ",nprods)
    }
    if(length(ssnip)>1 || ssnip<0 | ssnip>1 ){stop("'ssnip' must be a number between 0 and 1")}


    pricesDelta <-  calcPriceDeltaHypoMon(object,prodIndex=prodIndex,plantIndex=plantIndex,...)


    result <-pricesDelta > ssnip

    return( result)
  }

)

