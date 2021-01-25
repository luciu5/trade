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
#' @param mktElast A length k vector of product elasticities. Default is a length k vector of NAs
#' @param tariffPre  An n x k matrix  where each element equals the \strong{current } \emph{ad valorem} tariff (expressed as a proportion of consumer price) imposed
#'  on each product. Default is 0, which assumes no tariff.
#' @param tariffPost  An n x k matrix  where each element equals the \strong{new}  \emph{ad valorem} tariff (expressed as a proportion of consumer price) imposed
#'  on each product. Default is 0, which assumes no tariff.
#'@param mcfunPre a length n list of functions that calculate a plant's  marginal cost under the current tariff structure.
#'If empty (the default), assumes quadratic costs.
#'@param mcfunPost a length n list of functions that calculate a plant's marginal cost under the new tariff structure.
#'If empty (the default), assumes quadratic costs.
#'@param vcfunPre a length n list of functions that calculate a plant's  variable cost under the current tariff structure.
#'If empty (the default), assumes quadratic costs.
#'@param vcfunPost a length n list of functions that calculate a plant's variable cost under the new tariff structure.
#'If empty (the default), assumes quadratic costs.
#'@param capacitiesPre A length n numeric vector of plant capacities under the current tariff regime. Default is Inf.
#'@param capacitiesPost A length n numeric vector of plant capacities under the new tariff regime. Default is Inf.
#'@param productsPre An n x k matrix that equals TRUE if under the current tariff regime, a plant produces a product. Default is TRUE if 'quantities' is not NA.
#'@param productsPost An n x k matrix that equals TRUE if under the new tariff regime, a plant produces a product. Default equals 'productsPre'.
#'@param quantityStart A length k vector of quantities used as the initial guess in the nonlinear equation solver. Default is 'quantities'.
#'@param control.slopes A list of  \code{\link{optim}}  control parameters passed to the calibration routine optimizer
#'  (typically the \code{calcSlopes} method).
#' @param control.equ A list of  \code{\link[BB]{BBsolve}} control parameters passed to the non-linear equation solver
#'  (typically the \code{calcPrices} method).
#' @param labels A k-length vector of labels.
#' @param ... Additional options to feed to the \code{\link[BB]{BBsolve}} optimizer used to solve for equilibrium quantities.
#'
#' @details
#'
#' Let k denote the number of products and n denote the number of plants. Using price, and quantity, information for all products in each market, as well as
#' margin information for at least one products in each market, \code{cournot_tariff} is able to recover the
#' slopes and intercepts of either a Linear or Log-linear demand system. These parameters are then used
#' to simulate the price effects of a tariff under the assumption that the firms are playing a
#' homogeneous products simultaneous quantity setting game.
#'
#'
#'
#'@return \code{cournot_tariff} returns an instance of class \code{\linkS4class{Cournot}} from package \pkg{antitrust}, depending upon the value of the ``demand'' argument.
#'@references Simon P. Anderson, Andre de Palma, Brent Kreider,
#'The efficiency of indirect taxes under imperfect competition,
#'Journal of Public Economics,
#'Volume 81, Issue 2, 2001,Pages 231-251.
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
#' tariffPre <- tariffPost <- rep(0, n)
#' tariffPost[1] <- .75
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
#'                     tariffPre =  as.matrix(tariffPre),
#'                     tariffPost = as.matrix(tariffPost))
#'
#' summary(result.c, market = TRUE)         # summarize tariff (high-level)
#' summary(result.c, market = FALSE)         # summarize tariff (detailed)
#'
#' @include ps-methods.R summary-methods.R TariffCournot-methods.R
#' @export


cournot_tariff <- function(
  prices,quantities,
  margins = matrix(NA_real_ , nrow(quantities),ncol(quantities)),
  demand = rep("linear",length(prices)),
  cost   =   rep("linear",nrow(quantities)),
  tariffPre =matrix(0,nrow=nrow(quantities), ncol= ncol(quantities)),
  tariffPost =tariffPre,
  mcfunPre=list(),
  mcfunPost=mcfunPre,
  vcfunPre=list(),
  vcfunPost=vcfunPre,
  capacitiesPre = rep(Inf,nrow(quantities)),
  capacitiesPost = capacitiesPre,
  productsPre=!is.na(quantities),
  productsPost=productsPre,
  owner=NULL,
  mktElast = rep(NA_real_, length(prices)),
  quantityStart=as.vector(quantities),
  control.slopes,
  control.equ,
  labels,
  ...){

  shares <- as.vector(quantities/sum(quantities))

  nprods <- length(prices)

  tariffPre[is.na(tariffPre)] <- 0
  tariffPost[is.na(tariffPost)] <- 0

  mcDelta <- rep(0, nrow(quantities))

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


  if(is.null(owner)){

    warning("'owner' is NULL. Assuming each product is owned by a single firm.")
    owner <-  diag(nprods)

  }


  else if(!is.matrix(owner)){

    owner <- factor(owner, levels = unique(owner))
    owner = model.matrix(~-1+owner)
    owner = tcrossprod(owner)



  }


  ownerPre <- owner
  ownerPost <- owner

  result <- new("TariffCournot",prices=prices, quantities=quantities,margins=margins,
                shares=shares,mcDelta=mcDelta, subset= rep(TRUE,length(shares)), demand = demand, cost=cost,
                mcfunPre=mcfunPre, mcfunPost=mcfunPost,vcfunPre=vcfunPre, vcfunPost=vcfunPost,
                capacitiesPre=capacitiesPre,capacitiesPost=capacitiesPost,
                tariffPre=tariffPre,tariffPost=tariffPost,
                ownerPre=ownerPre, mktElast = mktElast,productsPre=productsPre,productsPost=productsPost,
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
