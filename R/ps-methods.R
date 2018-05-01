#'@title Methods To Calculate Producer Surplus
#'@description Producer Surplus methods for the \code{TariffBertrand} and \code{TariffCournot} classes
#' @name ps-methods
#' @param preMerger when TRUE, calculates producer surplus under the existing tariff regime. When FALSE, calculates
#' tariffs under the new tariff regime. Default is TRUE.
#' @return product-level (or in the case of Cournot, plant-level) producer surplus
#' @export
NULL
#' @rdname ps-methods
setMethod(

  f= "calcProducerSurplus",
  signature= "TariffBertrand",
  definition=function(object,preMerger=TRUE){



    if( preMerger) {
      prices <- object@pricePre
      mc     <- object@mcPre
      tariff <-  object@tariffPre
    }
    else{prices <- object@pricePost
    mc     <- object@mcPost
    tariff <-  object@tariffPost
    }


    output <- calcQuantities(object,preMerger)

    if (all(is.na(output))){
      warning("'calcQuantities' yielded all NAs. Using 'calcShares' instead")
      output <- calcShares(object,preMerger,revenue=FALSE)
    }

    ps <- (prices - mc) * output

    if(!preMerger) ps <- ps / (1 + tariff)

    names(ps) <- object@labels

    return(ps)

  }

)

#' @rdname ps-methods
setMethod(
  f= "calcProducerSurplus",
  signature= "TariffCournot",
  definition=function(object,preMerger=TRUE){


    if( preMerger) {
      prices <- object@pricePre
      quantities <- object@quantityPre
      tariff <-  object@tariffPre

    }
    else{prices <- object@pricePost
    quantities <- object@quantityPost
    tariff <-  object@tariffPost

    }



    vc <- calcVC(object, preMerger= preMerger)

    ps <- colSums(prices*t(quantities), na.rm=TRUE) - vc
    ps <- ps / (1 + tariff)

    names(ps) <- object@labels[[1]]

    return(ps)
  }

)
