
## compute producer surplus
setMethod(

  f= "calcProducerSurplus",
  signature= "TariffBertrand",
  definition=function(object,preMerger=TRUE){



    if( preMerger) {
      prices <- object@pricePre
      mc     <- object@mcPre
    }
    else{prices <- object@pricePost
    mc     <- object@mcPost
    }


    output <- calcQuantities(object,preMerger)

    if (all(is.na(output))){
      warning("'calcQuantities' yielded all NAs. Using 'calcShares' instead")
      output <- calcShares(object,preMerger,revenue=FALSE)
    }

    ps <- (prices - mc) * output

    if(!preMerger) ps <- ps / (1 + object@mcDelta)

    names(ps) <- object@labels

    return(ps)

  }

)


