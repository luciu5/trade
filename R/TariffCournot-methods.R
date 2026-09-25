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
#' \code{calcMC} reports physical marginal production cost, without artificial
#' capacity or nonnegativity penalties. Constraints enter the quantity solver.
#'@include TariffClasses.R
NULL
#' @rdname TariffCournot-methods
#' @export
setMethod("calcMC", "TariffCournot", function(object, preMerger = TRUE) {
  quantity <- if (preMerger) object@quantityPre else object@quantityPost
  mcfun <- if (preMerger) object@mcfunPre else object@mcfunPost
  ## Capacity and nonnegativity are constraints in calcQuantities, not
  ## artificial additions to physical production cost (especially at q=0).
  mc <- vapply(seq_len(nrow(quantity)), function(i) mcfun[[i]](quantity[i, ]),
               numeric(1))
  if (!preMerger) mc <- mc * (1 + object@mcDelta)
  stats::setNames(mc, object@labels[[1L]])
})

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

    retention <- 1 - tariff
    mc <- t(t(1 - margins) * prices) * retention

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


      thisFOC <- (t(quantities * retention) * thisPartial) %*% owner +
        thisprices * t(retention)
      thisFOC <- t(thisFOC)/mcPre - 1
      thisFOC[!products] <- NA_real_
      atCorner <- products & quantities == 0
      thisFOC[atCorner] <- pmax(0, thisFOC[atCorner])
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



    bestParms=stats::optim(parmStart,minDemand, method="L-BFGS-B", lower=lowerB, upper= upperB)$par

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

    promotion <- attr(object, "trade_promotion", exact = TRUE)
    if (preMerger && !is.null(promotion)) {
      if (market) return(sum(object@quantityPre, na.rm = TRUE))
      return(object@quantityPre)
    }

    slopes <- object@slopes
    intercepts <- object@intercepts
    quantityStart <- object@quantityStart

    if(preMerger){
      if(market) return(sum(object@quantityPre, na.rm=TRUE))
      owner <- object@ownerPre
      productMask <- object@productsPre
      cap <- object@capacitiesPre
      tariff <- object@tariffPre
      mcfun <- object@mcfunPre
    }
    else{
      if(market) return(sum(object@quantityPost, na.rm=TRUE))
      owner <- object@ownerPost
      productMask <- object@productsPost
      cap <- object@capacitiesPost
      tariff <- object@tariffPost
      mcfun <- object@mcfunPost
    }

    retention <- 1 - tariff
    nplants <- nrow(productMask)
    nprods <- ncol(productMask)
    linearDemand <- rep(object@demand, length.out = nprods) == "linear"
    available <- which(as.vector(productMask) &
                         rep(cap > 0, times = nprods))
    if (!length(available)) {
      quantEst <- matrix(0, nrow=nplants, ncol=nprods,
                         dimnames=object@labels)
      return(quantEst)
    }

    start <- matrix(as.numeric(quantityStart), nrow=nplants, ncol=nprods)
    start[!productMask | !is.finite(start) | start < 0] <- 0
    for (i in seq_len(nplants)) {
      if (is.finite(cap[i]) && sum(start[i, ]) > cap[i] && sum(start[i, ]) > 0) {
        start[i, ] <- start[i, ] * cap[i] / sum(start[i, ])
      }
    }
    start <- as.vector(start)
    active <- available[start[available] > 0]
    if (!length(active)) active <- available
    binding <- integer()
    latest <- NULL
    latestActive <- integer()
    failed <- FALSE
    maxIterations <- max(20L, 6L * length(available))
    quantityTolerance <- 1e-9
    kktTolerance <- 1e-7

    evaluate <- function(activeQuantities) {
      quantity <- matrix(0, nrow=nplants, ncol=nprods)
      quantity[active] <- activeQuantities
      marketQuantity <- colSums(quantity)
      safeQuantity <- pmax(marketQuantity, .Machine$double.eps)
      price <- ifelse(linearDemand, intercepts + slopes * marketQuantity,
                      exp(intercepts) * safeQuantity^slopes)
      partial <- ifelse(linearDemand, slopes,
                        exp(intercepts) * slopes * safeQuantity^(slopes - 1))

      netPrice <- matrix(price, nrow=nplants, ncol=nprods, byrow=TRUE) *
        retention
      marginalRevenue <- (t(quantity * retention) * partial) %*% owner +
        t(netPrice)
      gradient <- t(marginalRevenue)
      mc <- vapply(seq_len(nplants), function(i) {
        mcfun[[i]](quantity[i, ])
      }, numeric(1))
      if (!preMerger) mc <- mc * (1 + object@mcDelta)
      gradient <- sweep(gradient, 1L, mc, "-")

      ## A log-linear inverse demand curve has unbounded marginal revenue at
      ## zero aggregate output, so a supplied plant with available capacity
      ## cannot satisfy the zero-output KKT condition there.
      if (any(!linearDemand)) {
        zeroMarket <- !linearDemand & marketQuantity <= 0
        if (any(zeroMarket)) {
          gradient[, zeroMarket] <- Inf
        }
      }
      list(quantity = quantity, gradient = gradient, mc = mc,
           price = price, marketQuantity = marketQuantity)
    }

    lastState <- NULL
    for (iteration in seq_len(maxIterations)) {
      ## A repeated active set signals a numerical cycle; return the best
      ## feasible set found and surface the solver status below.
      state <- paste0(paste(sort(active), collapse = "."), "/",
                      paste(sort(binding), collapse = "."))
      if (!is.null(lastState) && identical(state, lastState)) break
      lastState <- state

      if (!length(active)) {
        atZero <- evaluate(numeric())
        gains <- atZero$gradient[available]
        if (!any(gains > kktTolerance, na.rm = TRUE)) {
          latest <- NULL
          latestActive <- integer()
          break
        }
        active <- available[which.max(gains)]
        next
      }

      plantOf <- ((active - 1L) %% nplants) + 1L
      if (length(binding) && !any(plantOf %in% binding)) binding <- integer()
      initialQ <- start[active]
      initialQ[!is.finite(initialQ) | initialQ <= 0] <- 1e-3
      initial <- c(initialQ, rep(0, length(binding)))

      residual <- function(candidate) {
        nactive <- length(active)
        q <- candidate[seq_len(nactive)]
        evaluated <- evaluate(q)
        stationarity <- evaluated$gradient[active]
        lambda <- numeric(nplants)
        if (length(binding)) {
          lambda[binding] <- candidate[nactive + seq_along(binding)]
        }
        stationarity <- stationarity - lambda[plantOf]
        netPrice <- matrix(evaluated$price, nrow=nplants, ncol=nprods,
                           byrow=TRUE) * retention
        scale <- pmax(1, abs(evaluated$mc[plantOf]),
                      abs(netPrice[active]))
        stationarity <- stationarity / scale
        if (!length(binding)) return(stationarity)
        capacity <- rowSums(evaluated$quantity)[binding] - cap[binding]
        c(stationarity, capacity / pmax(1, abs(cap[binding])))
      }

      solved <- tryCatch(
        BB::BBsolve(initial, residual, quiet=TRUE,
                    control=object@control.equ),
        error=function(e) {
          failed <<- TRUE
          NULL
        }
      )
      if (is.null(solved) || length(solved$par) != length(initial) ||
          any(!is.finite(solved$par))) break
      latest <- solved
      latestActive <- active
      if (solved$convergence != 0) failed <- TRUE

      nactive <- length(active)
      qhat <- solved$par[seq_len(nactive)]
      if (any(qhat <= quantityTolerance)) {
        drop <- which.min(qhat)
        active <- active[-drop]
        binding <- binding[binding %in% (((active - 1L) %% nplants) + 1L)]
        next
      }

      evaluated <- evaluate(qhat)
      plantQuantity <- rowSums(evaluated$quantity)
      overflow <- which(is.finite(cap) & plantQuantity >
                          cap + 1e-8 * pmax(1, abs(cap)) &
                          !(seq_len(nplants) %in% binding))
      if (length(overflow)) {
        binding <- c(binding, overflow[which.max(plantQuantity[overflow] - cap[overflow])])
        next
      }

      if (length(binding)) {
        lambda <- solved$par[nactive + seq_along(binding)]
        if (any(lambda < -kktTolerance)) {
          binding <- binding[-which.min(lambda)]
          next
        }
      } else {
        lambda <- numeric()
      }
      lambdaByPlant <- numeric(nplants)
      if (length(binding)) lambdaByPlant[binding] <- lambda

      inactive <- setdiff(available, active)
      if (length(inactive)) {
        inactiveRows <- ((inactive - 1L) %% nplants) + 1L
        gains <- evaluated$gradient[inactive] - lambdaByPlant[inactiveRows]
        if (any(gains > kktTolerance, na.rm=TRUE)) {
          active <- c(active, inactive[which.max(gains)])
          next
        }
      }

      ## The active FOCs, inactive KKT inequalities, and plant capacity
      ## multipliers now agree to solver tolerance.
      break
    }

    quantity <- matrix(0, nrow=nplants, ncol=nprods)
    if (!is.null(latest) && length(latestActive)) {
      qhat <- latest$par[seq_along(latestActive)]
      quantity[latestActive] <- pmax(0, qhat)
    }
    quantEst <- quantity
    dimnames(quantEst) <- object@labels

    ## Never turn an unconverged root into a reported equilibrium by clipping
    ## negative output. Audit physical feasibility and KKT conditions at the
    ## actual returned quantities, including inactive products and capacities.
    active <- available
    audit <- evaluate(quantity[available])
    plantTotals <- rowSums(quantity)
    shadow <- numeric(nplants)
    for (i in seq_len(nplants)) {
      positive <- productMask[i, ] & quantity[i, ] > quantityTolerance
      if (is.finite(cap[i]) && abs(plantTotals[i] - cap[i]) <=
          1e-7 * max(1, abs(cap[i])) && any(positive)) {
        shadow[i] <- max(0, mean(audit$gradient[i, positive]))
      }
    }
    reduced <- sweep(audit$gradient, 1L, shadow, "-")
    scale <- pmax(1, abs(audit$mc),
                   abs(matrix(audit$price, nplants, nprods, byrow = TRUE) * retention))
    residual <- ifelse(quantity > quantityTolerance, abs(reduced), pmax(0, reduced)) / scale
    if (any(!is.finite(quantity)) ||
        any(plantTotals > cap + 1e-7 * pmax(1, abs(cap))) ||
        any(!is.finite(residual[available])) ||
        any(residual[available] > 1e-5)) {
      stop("'calcQuantities' failed to find a feasible Cournot equilibrium satisfying the KKT conditions")
    }

    if (failed) {
      warning("'calcQuantities' nonlinear solver may not have successfully converged.")
    }
    quantEst
  })


