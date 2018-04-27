#'Tariff Simulation With A Cournot Quantity Setting Game
#'
#' Simulate the effect of tariffs when firms play a cournot quantity setting game and consumer demand is either linear or log-linear
#'

#' @param prices  A length k vector product prices.
#' @param quantities An n x k matrix of product quantities. All quantities must either be positive, or if the product is
#' not produced by a plant, NA
#' @param margins An n x k matrix of product margins. All margins must be either be between 0 and 1, or NA.
#' @param demand A length k character vector equal to "linear" if a product's demand curve is assumed to be linear or "log"
#' if a product's demand curve is assumed to be log-linear.
#' @param cost A length k character vector equal to "linear" if a plant's marginal cost curve is assumed to be linear or
#'  "constant" if a plant's marginal curve is assumed to be constant. Returns an error if a multi-plant firm with constant
#'   marginal costs does not have capacity constraints.
#' @param owner EITHER a vector of length n whose values indicate which plants are commonly owned OR an n x n matrix of ownership shares.
#' @param diversions  A k x k matrix of diversion ratios with diagonal elements equal to -1. Default is missing, in which
#'  case diversion according to revenue share is assumed.
#' @param mktElast A length k vector of product elasticities. Default is a length k vector of NAs
#' @param insideSize Size of all units included in the market. For logit, this defaults to total quantity, while for aids
#'  and ces this defaults to total revenues.
#' @param tariff  A length k vector where each element equals the \emph{ad valorem} tariff (expressed as a proportion) imposed
#'  on each product. Default is 0, which assumes no tariff.
#' @param control.slopes A list of  \code{\link{optim}}  control parameters passed to the calibration routine optimizer
#'  (typically the \code{calcSlopes} method).
#' @param control.equ A list of  \code{\link[BB]{BBsolve}} control parameters passed to the non-linear equation solver
#'  (typically the \code{calcPrices} method).
#' @param labels A k-length vector of labels.
#' @param ... Additional options to feed to the \code{\link[BB]{BBsolve}} optimizer used to solve for equilibrium quantities.
#'
#' @details
#'
#' Using price, and quantity, information for all products in each market, as well as
#' margin information for at least one products in each market, \code{cournot_tariff} is able to recover the
#' slopes and intercepts of either a Linear or Log-linear demand system. These parameters are then used
#' to simulate the price effects of a merger between two firms under the assumption that the firms are playing a
#' homogeneous products simultaneous quantity setting game.
#'
#'
#'
#' @return \code{cournot_tariff} returns an instance of class \code{\linkS4class{Cournot}} from package \code{\link{antitrust}}, depending upon the value of the ``demand'' argument.
#'
#' @examples
#' ## Simulate the effect of a 75% ad valorem tariff in a
#' ## 5-firm, single-product market with linear demand and quadratic costs
#' ## Firm 1 is assumed to be foreign, and so subject to a tariff
#'
#'
#' n <- 5 #number of firms in market
#' cap <- rnorm(n,mean = .5, sd = .1)
#' int <- 10
#' slope <- -.25
#' tariff <- rep(0, n)
#' tariff[1] <- .75
#'
#' B.pre.c = matrix(slope,nrow=n,ncol=n)
#' diag(B.pre.c) = 2* diag(B.pre.c) - 1/cap
#' quantity.pre.c = rowSums(solve(B.pre.c) * -int)
#' price.pre.c = int + slope * sum(quantity.pre.c)
#' mc.pre.c = quantity.pre.c/cap
#' vc.pre.c = quantity.pre.c^2/(2*cap)
#' margin.pre.c = 1 - mc.pre.c/price.pre.c
#'
#' #prep inputs for Cournot
#' owner.pre <- diag(n)
#'
#'
#'
#' result.c <- cournot_tariff(prices = price.pre.c,quantities = as.matrix(quantity.pre.c),
#'                     margins=as.matrix(margin.pre.c),
#'                     owner=owner.pre,
#'                     tariff = tariff)
#'
#' summary(result.c, market = TRUE)         # summarize merger simulation (high-level)
#' summary(result.c, market = FALSE)         # summarize merger simulation (detailed)
#'
#'
#' @export


cournot_tariff <- function(
  prices,quantities,
  margins = matrix(NA_real_ , nrow(quantities),ncol(quantities)),
  demand = rep("linear",length(prices)),
  cost   =   rep("linear",nrow(quantities)),
  tariff =rep(0,nrow(quantities)),
  mcfunPre=list(),
  mcfunPost=mcfunPre,
  vcfunPre=list(),
  vcfunPost=vcfunPre,
  capacitiesPre = rep(Inf,nrow(quantities)),
  capacitiesPost = capacitiesPre,
  productsPre=!is.na(quantities),
  productsPost=productsPre,
  owner,
  mktElast = rep(NA_real_, length(prices)),
  quantityStart=as.vector(quantities),
  control.slopes,
  control.equ,
  labels,
  ...){

  shares <- as.vector(quantities/sum(quantities))



  if(missing(labels)){
    if(is.null(dimnames(quantities))){
      rname <- paste0("O",1:nrow(quantities))
      cname <- paste0("P",1:ncol(quantities))
    }
    else{rname <- rownames(quantities)
    cname <- colnames(quantities)
    }
    labels <- list(rname,cname)

  }


  nprods <- length(quantities)

  subset= rep(TRUE,nprods)

  if(missing(owner)){

    warning("'owner' is missing. Assuming each product is owned by a single firm.")
    owner <-  diag(nprods)

  }


  if(!is.matrix(owner)){

    owner <- factor(owner, levels = unique(owner))
    owner = model.matrix(~-1+owner)
    owner = tcrossprod(owner)



  }


  ownerPost <- owner/(1+tariff)


  result <- new("TariffCournot",prices=prices, quantities=quantities,margins=margins,
                shares=shares,mcDelta=tariff, subset= rep(TRUE,length(shares)), demand = demand, cost=cost,
                mcfunPre=mcfunPre, mcfunPost=mcfunPost,vcfunPre=vcfunPre, vcfunPost=vcfunPost,
                capacitiesPre=capacitiesPre,capacitiesPost=capacitiesPost,
                ownerPre=owner, mktElast = mktElast,productsPre=productsPre,productsPost=productsPost,
                ownerPost=ownerPost, quantityStart=quantityStart,labels=labels)


  if(!missing(control.slopes)){
    result@control.slopes <- control.slopes
  }


  ## Convert ownership vectors to ownership matrices
  result@ownerPre  <- ownerToMatrix(result,TRUE)
  result@ownerPost <- ownerToMatrix(result,FALSE)

  ## Calculate Demand Slope Coefficients and Intercepts
  result <- calcSlopes(result)


  result@quantityPre  <- calcQuantities(result, preMerger = TRUE,...)
  result@quantityPost <- calcQuantities(result,preMerger = FALSE,...)

  result@pricePre  <- calcPrices(result, preMerger = TRUE)
  result@pricePost <- calcPrices(result,preMerger = FALSE)

  return(result)

}
