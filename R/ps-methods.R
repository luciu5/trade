#'@title Methods To Calculate Producer Surplus
#'@description Producer Surplus methods for the \code{TariffBertrand} and \code{TariffCournot} classes
#' @name ps-methods
#' @param object an instance of class \code{TariffBertrand} or \code{TariffCournot}
#' @param preMerger when TRUE, calculates producer surplus under the existing tariff regime. When FALSE, calculates
#' tariffs under the new tariff regime. Default is TRUE.
#' @return product-level (or in the case of Cournot, plant-level) producer surplus
#'@include TariffClasses.R
NULL
#' @rdname ps-methods
#' @export
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

    ps <- ps * (1 - tariff)

    names(ps) <- object@labels

    return(ps)

  }

)

#' @rdname ps-methods
#' @export
setMethod(
  f= "calcProducerSurplus",
  signature= "TariffCournot",
  definition=function(object,preMerger=TRUE){


    if( preMerger) {

      tariff <-    object@tariffPre

    }
    else{
      tariff <-   object@tariffPost

    }


    rev <- calcRevenues(object, preMerger= preMerger)
    rev <- rev * (1 - tariff)

    vc <- calcVC(object, preMerger= preMerger)

    ps <- rowSums(rev, na.rm=TRUE) - vc


    names(ps) <- object@labels[[1]]

    return(ps)
  }

)



