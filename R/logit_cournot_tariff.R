#' Tariff Simulation With A Differentiated-Product Logit Cournot Game
#'
#' Simulate the effect of tariffs when firms play a differentiated-product
#' Cournot quantity game and demand is Logit.
#'
#' @param prices A length k vector of product prices.
#' @param quantities A length k vector of product quantities. Used to calculate
#'   shares when \code{shares} is missing and to set \code{insideSize} when
#'   \code{insideSize} is missing.
#' @param shares A length k vector of product quantity shares. If missing,
#'   shares are calculated as \code{quantities / sum(quantities)}.
#' @param margins A length k vector of product margins. All margins must be
#'   positive or \code{NA}.
#' @param owner Required. EITHER a vector of length k whose values indicate
#'   which firm produced a product before the tariff OR a k x k matrix of
#'   pre-tariff ownership shares.
#' @param diversions A k x k matrix of diversion ratios with diagonal elements
#'   equal to -1. Used for the \code{LogitCournot} calibration.
#' @param mktElast A negative number equal to the industry pre-tariff price
#'   elasticity. Used for the \code{LogitCournotALM} calibration.
#' @param calibration Which antitrust Logit Cournot calibration to use.
#'   \code{"diversion"} creates a \code{\linkS4class{TariffLogitCournot}}
#'   object. \code{"alm"} creates a
#'   \code{\linkS4class{TariffLogitCournotALM}} object. \code{"auto"} uses
#'   \code{"alm"} when \code{mktElast} is supplied and \code{diversions} is
#'   missing; otherwise it uses \code{"diversion"}.
#' @param tariffPre A vector of length k where each element equals the
#'   \strong{current} \emph{ad valorem} tariff, expressed as a proportion of
#'   the consumer price, imposed on each product. Default is 0.
#' @param tariffPost A vector of length k where each element equals the
#'   \strong{new} \emph{ad valorem} tariff, expressed as a proportion of the
#'   consumer price, imposed on each product. Default is 0.
#' @param weights A length k vector of non-negative product weights used in
#'   minimum-distance calibration.
#' @param normIndex An integer specifying the product index against which the
#'   mean values of all other products are normalized. Use \code{NA} when an
#'   outside good is included.
#' @param subset A vector of length k where each element equals TRUE if the
#'   product indexed by that element should be included in the post-tariff
#'   simulation and FALSE if it should be excluded.
#' @param insideSize A positive number equal to total pre-tariff quantities for
#'   all products included in the simulation.
#' @param priceOutside Price of the outside good. Default is 0.
#' @param priceStart A length k vector of starting values used to solve for
#'   equilibrium prices. Default is \code{prices}.
#' @param isMax If TRUE, checks to see whether computed price equilibrium
#'   locally maximizes firm profits and returns a warning if not. Default is
#'   FALSE.
#' @param parmsStart A length 2 vector whose first element equals an initial
#'   guess of the price coefficient and whose second element equals an initial
#'   guess of the outside share. Used only for \code{calibration = "alm"}.
#' @param control.slopes A list of \code{\link{optim}} control parameters
#'   passed to the calibration routine optimizer.
#' @param control.equ A list of \code{\link[BB]{BBsolve}} control parameters
#'   passed to the non-linear equation solver.
#' @param labels A k-length vector of labels.
#' @param output A length 1 logical vector equal to TRUE if the simulation is
#'   for an output market. Default is TRUE.
#' @param ... Additional options to feed to the optimizer used to solve for
#'   equilibrium prices.
#'
#' @details
#' This function extends \code{\link[antitrust]{logit.cournot}} and
#' \code{\link[antitrust]{logit.cournot.alm}} with the same ad valorem tariff
#' treatment used by the other differentiated-product tariff simulations in
#' this package. The tariff scales the conduct matrix by
#' \code{1 - tariff} and changes marginal cost by
#' \code{(tariffPost - tariffPre) / (1 - tariffPost)}.
#'
#' Unlike \code{\link{cournot_tariff}}, which models homogeneous-product
#' Cournot competition with plant-level quantities, this function models
#' differentiated products with Logit demand.
#'
#' @return \code{logit_cournot_tariff} returns an instance of class
#'   \code{\linkS4class{TariffLogitCournot}} or
#'   \code{\linkS4class{TariffLogitCournotALM}}, depending on
#'   \code{calibration}.
#'
#' @examples
#' \donttest{
#' prodNames <- c("BUD","OLD STYLE","MILLER","MILLER-LITE","OTHER-LITE","OTHER-REG")
#' owner <- c("BUD","OLD STYLE","MILLER","MILLER","OTHER-LITE","OTHER-REG")
#' price <- c(.0441,.0328,.0409,.0396,.0387,.0497)
#' quantities <- c(.066,.172,.253,.187,.099,.223) * 100
#' margins <- c(.3830,.5515,.5421,.5557,.4453,.3769)
#' tariff <- c(0,0,0,0,.1,.1)
#'
#' result <- logit_cournot_tariff(prices = price, quantities = quantities,
#'                                margins = margins, owner = owner,
#'                                tariffPost = tariff, labels = prodNames)
#'
#' print(result)
#' summary(result)
#' calcDiagnostics(result)
#' }
#' @include ps-methods.R summary-methods.R
#' @export
logit_cournot_tariff <- function(prices,
                                 quantities,
                                 shares,
                                 margins,
                                 owner=NULL,
                                 diversions,
                                 mktElast=NA_real_,
                                 calibration=c("auto","diversion","alm"),
                                 tariffPre,
                                 tariffPost,
                                 weights,
                                 normIndex,
                                 subset,
                                 insideSize,
                                 priceOutside=0,
                                 priceStart,
                                 isMax=FALSE,
                                 parmsStart,
                                 control.slopes,
                                 control.equ,
                                 labels,
                                 output=TRUE,
                                 ...){

  calibration <- match.arg(calibration)

  if(missing(shares)){
    if(missing(quantities)){
      stop("either 'shares' or 'quantities' must be supplied")
    }
    shares <- quantities/sum(quantities, na.rm=TRUE)
  }

  nprods <- length(shares)

  if(missing(prices) || length(prices) != nprods){
    stop("'prices' must be a length-k vector")
  }

  if(missing(margins) || length(margins) != nprods){
    stop("'margins' must be a length-k vector")
  }

  if(!missing(quantities) && length(quantities) != nprods){
    stop("'quantities' must be a length-k vector")
  }

  if(missing(tariffPre)) tariffPre <- rep(0,nprods)
  if(missing(tariffPost)) tariffPost <- rep(0,nprods)

  tariffPre <- .normalize_tariff(tariffPre, nprods, "tariffPre")
  tariffPost <- .normalize_tariff(tariffPost, nprods, "tariffPost")

  if(missing(weights)) weights <- rep(1,nprods)
  if(missing(subset)) subset <- rep(TRUE,nprods)
  if(missing(priceStart)) priceStart <- prices
  if(missing(labels)) labels <- paste("Prod",1:nprods,sep="")

  if(missing(insideSize)){
    insideSize <- if(missing(quantities)) NA_real_ else sum(quantities, na.rm=TRUE)
  }

  if(missing(normIndex)){
    normIndex <- ifelse(isTRUE(all.equal(sum(shares),1,check.names=FALSE,tolerance=1e-3)),1,NA)
  }

  owner <- .owner_to_matrix(owner, nprods,
                            "'owner' must be supplied as a length-k vector or k x k ownership matrix")

  if(calibration == "auto"){
    calibration <- if(!missing(mktElast) && !is.na(mktElast) && missing(diversions)) "alm" else "diversion"
  }

  ownerPre <- .apply_tariff_to_owner(owner, tariffPre)
  ownerPost <- .apply_tariff_to_owner(owner, tariffPost)
  mcDelta <- .tariff_mc_delta(tariffPre, tariffPost)

  if(calibration == "diversion"){
    if(missing(diversions)){
      diversions <- matrix(NA_real_, nrow=nprods, ncol=nprods)
    }

    result <- new("TariffLogitCournot",
                  prices=prices,
                  shares=shares,
                  margins=margins,
                  diversion=diversions,
                  normIndex=normIndex,
                  ownerPre=ownerPre,
                  ownerPost=ownerPost,
                  insideSize=insideSize,
                  output=output,
                  mcDelta=mcDelta,
                  subset=subset,
                  weights=weights,
                  priceOutside=priceOutside,
                  priceStart=priceStart,
                  shareInside=ifelse(isTRUE(all.equal(sum(shares),1,check.names=FALSE,tolerance=1e-3)),1,sum(shares)),
                  tariffPre=tariffPre,
                  tariffPost=tariffPost,
                  labels=labels)
  }
  else{
    if(!isTRUE(all.equal(sum(shares),1,check.names=FALSE,tolerance=1e-3))){
      stop("'shares' must sum to 1 when calibration = 'alm'")
    }

    if(missing(parmsStart)){
      parmsStart <- rep(.1,2)
      nm <- which(!is.na(margins))[1]
      parmsStart[1] <- (ifelse(output,-1,1))/(margins[nm]*prices[nm]*(1-shares[nm]))
    }

    result <- new("TariffLogitCournotALM",
                  prices=prices,
                  shares=shares,
                  margins=margins,
                  ownerPre=ownerPre,
                  ownerPost=ownerPost,
                  mktElast=mktElast,
                  insideSize=insideSize,
                  output=output,
                  mcDelta=mcDelta,
                  subset=subset,
                  weights=weights,
                  priceOutside=priceOutside,
                  priceStart=priceStart,
                  shareInside=1,
                  parmsStart=parmsStart,
                  tariffPre=tariffPre,
                  tariffPost=tariffPost,
                  labels=labels)
  }

  if(!missing(control.slopes)){
    result@control.slopes <- control.slopes
  }
  if(!missing(control.equ)){
    result@control.equ <- control.equ
  }

  result@ownerPre <- ownerToMatrix(result,TRUE)
  result@ownerPost <- ownerToMatrix(result,FALSE)

  result <- calcSlopes(result)

  result@mcPre <- calcMC(result,TRUE)
  result@mcPost <- calcMC(result,FALSE)

  result@pricePre <- calcPrices(result,preMerger=TRUE,isMax=isMax,...)
  result@pricePost <- calcPrices(result,preMerger=FALSE,isMax=isMax,...)

  return(result)
}
