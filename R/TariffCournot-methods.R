#'@title Additional methods for TariffCournot Class
#'@description Producer Surplus methods for the \code{TariffBertrand} and \code{TariffCournot} classes
#' @name TariffCournot-methods
#' @param object an instance of class \code{TariffCournot}
#' @param preMerger when TRUE, computes result  under the existing tariff regime. When FALSE, calculates
#' tariffs under the new tariff regime. Default is TRUE.
#' @param market when TRUE, computes market-wide results. When FALSE, calculates
#' plant-specific results.
#' @return \code{calcSlopes} return a TariffCournot object containing estimated slopes. \code{CalcQuantities} returns
#' a matrix of equilbrium quantities under either the current or new tariff.
#'@include TariffClasses.R
NULL
#' @rdname TariffCournot-methods
#' @export
setMethod(
  f= "calcSlopes",
  signature= "TariffCournot",
  definition=function(object){

    prices <- object@prices
    quantities <- object@quantities
    quantities[is.na(quantities)] <- 0
    margins <- object@margins
    mktElast <- object@mktElast
    tariff <- object@tariffPre


    cap <- object@capacitiesPre

    mc <- t(t(1 - margins) * prices)/( 1 + tariff) #assumes costs include tariff

    products <- object@productsPre
    demand <- object@demand
    owner <- object@ownerPre
    mcfunPre <- object@mcfunPre
    nprods <- ncol(quantities)
    nplants <- nrow(quantities)

    noCosts <- length(mcfunPre) == 0
    isLinearD <- demand=="linear"
    isLinearC <- object@cost=="linear"

    quantTot <- colSums(quantities, na.rm = TRUE)
    quantPlants <- rowSums(quantities, na.rm = TRUE)
    quantOwner <- owner %*% quantities

    isConstrained <- quantPlants >= cap

    if(!noCosts){
      mcPre <- sapply(1:nplants, function(i){object@mcfunPre[[i]](quantities[i,])})
    }

    sharesOwner <- t(t(quantOwner)/quantTot)

    minDemand <- function(theta){

      if(noCosts){

        thiscap <- theta[1:nplants]
        theta <- theta[-(1:nplants)]
        mcPre <- ifelse(isLinearC, quantPlants/thiscap, thiscap)

      }

      thisints <- theta[1:nprods]
      thisslopes <- theta[-(1:nprods)]

      thisprices <- ifelse(isLinearD, thisints + thisslopes*quantTot,
                           exp(thisints)*quantTot^thisslopes)

      thisPartial <- ifelse(isLinearD,
                            thisslopes,
                            exp(thisints)*thisslopes*quantTot^(thisslopes - 1))


      thisFOC <- (t(quantities/(1 + tariff)) * thisPartial ) %*% owner  + (thisprices / t(1 + tariff))
      thisFOC <- t(thisFOC)/mcPre - 1
      thisFOC <- thisFOC[!isConstrained,]

      dist <- c(thisFOC,thisprices/prices -1 , (1/mktElast)/(thisPartial*quantTot/prices) - 1 )

      if(noCosts){ dist <- c(dist, mcPre/mc - 1)}

      return(sum((dist*10)^2,na.rm=TRUE))
    }


    margGuess <- margins
    margGuess[is.na(margGuess)] <- -t(t(sharesOwner)/mktElast)[is.na(margGuess)]

    bStart      =   ifelse(isLinearD,
                           colMeans(-(prices*margGuess)/(sharesOwner*quantTot),na.rm=TRUE),
                           colMeans(-margGuess/sharesOwner,na.rm=TRUE))
    intStart    =   ifelse(isLinearD,
                           prices - bStart*quantTot,
                           log(prices/(quantTot^bStart)))
    intStart    =   abs(intStart)

    parmStart   =   c( intStart,bStart)

    lowerB <- c(rep(0, nprods), rep(-Inf, nprods))
    upperB <- c(rep(Inf,  nprods), rep(0, nprods))

    if(noCosts){

      if(isLinearD) {margStart <- rowMeans(-(sharesOwner*quantTot)/(prices/bStart),na.rm=TRUE) }
      else{margStart <-  rowMeans(-sharesOwner*bStart,na.rm=TRUE)}

      mcStart  <- abs(prices*(margStart - 1))
      capStart <- ifelse(isLinearC, quantPlants/mcStart, mcStart)
      parmStart <- c(capStart,parmStart)

      lowerB <- c(rep(0, nplants), lowerB)
      upperB <- c(rep(Inf,nplants), upperB)
    }



    bestParms=optim(parmStart,minDemand, method="L-BFGS-B", lower=lowerB, upper= upperB)$par

    if(isTRUE(all.equal(bestParms[1:nplants],rep(0, nplants),check.names=FALSE))){warning("Some plant-level cost parameters are close to 0.")}
    if(isTRUE(all.equal(bestParms[-(1:nplants)],rep(0, nprods),check.names=FALSE))){warning("Some demand parameters are close to 0.")}



    ## if no marginal cost functions are supplied
    ## assume that plant i's marginal cost is
    ## q_i/k_i, where k_i is calculated from FOC

    if(noCosts){

      mcparm <- bestParms[1:nplants]
      bestParms <- bestParms[-(1:nplants)]


      mcdef <- ifelse(isLinearC,"function(q,mcparm = %f){ val <- sum(q, na.rm=TRUE) / mcparm; return(val)}",
                      "function(q,mcparm = %f){ val <- mcparm; return(val)}")
      mcdef <- sprintf(mcdef,mcparm)
      mcdef <- lapply(mcdef, function(x){eval(parse(text=x ))})

      object@mcfunPre <- mcdef
      names(object@mcfunPre) <- object@labels[[1]]

      vcdef <- ifelse(isLinearC,"function(q,mcparm = %f){  val <-  sum(q, na.rm=TRUE)^2 / (mcparm * 2); return(val)}",
                      "function(q,mcparm = %f){  val <-  sum(q, na.rm=TRUE) * mcparm; return(val)}")
      vcdef <- sprintf(vcdef,mcparm)
      vcdef <- lapply(vcdef, function(x){eval(parse(text=x ))})

      object@vcfunPre <- vcdef
      names(object@vcfunPre) <- object@labels[[1]]

    }
    if(length(object@mcfunPost)==0){
      object@mcfunPost <- object@mcfunPre
      object@vcfunPost <- object@vcfunPre}

    intercepts = bestParms[1:nprods]
    slopes = bestParms[-(1:nprods)]


    object@intercepts <- intercepts
    object@slopes <-     slopes


    return(object)

  })


#' @rdname TariffCournot-methods
#' @export
setMethod(
  f= "calcQuantities",
  signature= "TariffCournot",
  definition=function(object,preMerger=TRUE,market=FALSE){

    slopes <- object@slopes
    intercepts <- object@intercepts
    quantityStart <- object@quantityStart
    #quantityStart[is.na(quantityStart)] = 0

    if(preMerger){
      if(market) return(sum(object@quantityPre, na.rm=TRUE))
      owner  <- object@ownerPre
      products  <- object@productsPre
      cap <- object@capacitiesPre
      tariff <- object@tariffPre
    }
    else{
      if(market) return(sum(object@quantityPost, na.rm=TRUE))
      owner <-  object@ownerPost
      products <-  object@productsPost
      cap <- object@capacitiesPost
      tariff <- object@tariffPost
    }

    nprods <- ncol(products)
    #isProducts <- rowSums(products) > 0
    products <- as.vector(products)

    isConstrained <- is.finite(cap)

    FOC <- function(quantCand){

      #quantCand <- quantCand^2 # constrain positive

      thisQuant <- rep(0,length(products))
      thisQuant[products] = quantCand
      thisQuant = matrix(thisQuant,ncol=nprods)

      if(preMerger){ object@quantityPre  <- thisQuant}
      else{          object@quantityPost <- thisQuant}

      thisPrice <- calcPrices(object, preMerger= preMerger)

      thisMC <- calcMC(object, preMerger= preMerger)


      mktQuant <- colSums(thisQuant, na.rm=TRUE)
      plantQuant <- rowSums(thisQuant, na.rm=TRUE)

      thisPartial <- ifelse(object@demand=="linear",
                            slopes,
                            exp(intercepts)*slopes*mktQuant^(slopes - 1))


      thisFOC <- (t(thisQuant / (1 + tariff )) * thisPartial) %*% owner + thisPrice / t(1 + tariff)
      thisFOC <- t(thisFOC)/thisMC - 1
      #thisFOC <- t(t(thisFOC)/thisPrice) # rescale
      #thisCons <- (plantQuant - cap)/cap # rescale
      #thisFOC[isConstrained,] <- thisFOC[isConstrained,] +
      #   thisCons[isConstrained] +
      #  sqrt(thisFOC[isConstrained,]^2 +
      #         thisCons[isConstrained]^2)
      thisFOC <- as.vector(thisFOC)[products]
      return(thisFOC)
    }


    #quantityStart <- sqrt(object@quantityStart[products]) #constrain positive
    quantityStart <- ifelse(quantityStart >= cap, cap-1, quantityStart)
    quantityStart <- quantityStart[products]


    ## Find price changes that set FOCs equal to 0
    minResult <- BB::BBsolve( quantityStart,FOC, quiet=TRUE,control=object@control.equ)

    if(minResult$convergence != 0){warning("'calcQuantities' nonlinear solver may not have successfully converged. 'BBsolve' reports: '",minResult$message,"'")}

    quantEst        <- rep(0, length(products))
    quantEst[products] <- minResult$par#^2
    quantEst <- matrix(quantEst,ncol = nprods)

    dimnames(quantEst) <- object@labels

    return(quantEst)
  })


