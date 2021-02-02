#'@title Additional methods for TariffMonComLogit, TariffMonComCES Classes
#'@description \code{calcSlopes}, \code{Prices}, \code{Margins} methods for the \code{TariffMonComLogit} and \code{TariffMonComCES} classes
#' @name TariffMonComRUM-methods
#' @param object an instance of class \code{TariffMonComLogit} or class  \code{TariffMonComCES}
#' @param preMerger when TRUE, computes result  under the existing tariff regime. When FALSE, calculates
#' tariffs under the new tariff regime. Default is TRUE.
#' @param level when TRUE, computes margins in dollars. When FALSE, calculates
#' margins as a proportion of prices. Default is FALSE.
#' @return \code{calcSlopes} return a  \code{TariffMonComLogit} or  \code{TariffMonComCES} object containing estimated slopes. \code{CalcQuantities} returns
#' a matrix of equilbrium quantities under either the current or new tariff.
#'@include TariffClasses.R
NULL
#' @rdname TariffMonComRUM-methods
#' @export
setMethod(
  f= "calcSlopes",
  signature= "TariffMonComLogit",
  definition=function(object){

    ## Uncover Demand Coefficients


    ownerPre     <-  object@ownerPre
    shares       <-  object@shares
    margins      <-  object@margins
    prices       <-  object@prices
    idx          <-  object@normIndex
    mktElast     <-  object@mktElast
    shareInside  <-  object@shareInside


    if(is.na(idx)){
      idxShare <- 1 - shareInside
      idxPrice <- object@priceOutside
    }
    else{
      idxShare <- shares[idx]
      idxPrice <- prices[idx]
    }

    ## Uncover price coefficient and mean valuation from margins and revenue shares


    nprods <- length(shares)

    avgPrice <- sum(shares * prices, na.rm=TRUE) / sum(shares)

    ## identify which products have enough margin information
    ##  to impute Bertrand margins
    #isMargin    <- matrix(margins,nrow=nprods,ncol=nprods,byrow=TRUE)
    #isMargin[ownerPre==0]=0
    #isMargin    <- !is.na(rowSums(isMargin))


    ## Minimize the distance between observed and predicted margins
    minD <- function(alpha){


      if(!is.na(mktElast) && shareInside < 1 ){
        shareInside <-   1 - mktElast/( alpha * avgPrice )

      }
      else{shareInside <- NA_real_}

      marginsCand <- -1/(alpha* prices)

      m1 <- margins - marginsCand
      m2 <- mktElast - alpha*avgPrice*( 1- shareInside)
      measure <- sum(c(m1, m2 )^2,na.rm=TRUE)

      return(measure)
    }

    alphaBounds <- c(-1e6,0)
    if(!is.na(mktElast)){

      alphaBounds[2] <- mktElast/ avgPrice
      alphaBounds[1] <- alphaBounds[2]/1e-4
      }

    minAlpha <- optimize(minD, alphaBounds,
                         tol=object@control.slopes$reltol)$minimum

    if(!is.na(mktElast)){


      object@shareInside <-    1 - mktElast/(minAlpha * avgPrice )
      idxShare <-  1 - object@shareInside
      idxPrice <- object@priceOutside
      object@normIndex <- NA_integer_


    }

    meanval <- log(shares) - log(idxShare) - minAlpha * (prices - idxPrice)





    names(meanval)   <- object@labels



    object@slopes    <- list(alpha=minAlpha,meanval=meanval)
    object@priceOutside <- idxPrice
    object@mktSize <- object@insideSize / sum(shares)


    return(object)
  }

  )

#' @rdname TariffMonComRUM-methods
#' @export
setMethod(
  f= "calcSlopes",
  signature= "TariffMonComCES",
  definition=function(object){

## Uncover Demand Coefficents


ownerPre     <-  object@ownerPre
shares       <-  object@shares
margins      <-  object@margins
prices       <-  object@prices
idx          <-  object@normIndex
shareInside  <-  object@shareInside

mktElast     <-  object@mktElast

mktSize     <-  object@mktSize
insideSize   <-  object@insideSize

## uncover Numeraire Coefficients
if(shareInside <= 1 && shareInside>0) {alpha <- 1/shareInside - 1}
else{alpha <- NULL}

## if sum of shares is less than 1, add numeraire
if(is.na(idx)){
  idxShare <- 1 - sum(shares)
  idxPrice <- object@priceOutside
}
else{
  idxShare <- shares[idx]
  idxPrice <- prices[idx]
}


## Uncover price coefficient and mean valuation from margins and revenue shares


nprods <- length(shares)



#avgPrice <- sum(shares * prices, na.rm=TRUE) / sum(shares)


## Minimize the distance between observed and predicted margins
minD <- function(gamma){

  if(!is.na(mktElast) && shareInside < 1 ){
    sOut <- (mktElast + 1)/(1-gamma)
  }

  else{sOut <- NA_real_}

  marginsCand <- 1/(gamma)

  m1 <- margins - marginsCand
  m2 <- (1-shareInside) - sOut
  measure <- sum(c(m1, m2 )^2,na.rm=TRUE)

  return(measure)
}


gammaBounds <- c(1,1e6)
if(!is.na(mktElast) && mktElast < -1){
  gammaBounds[1] <- -mktElast
  gammaBounds[2] <-  1 - (mktElast+1)/1e-4
  }


minGamma <- optimize(minD,gammaBounds,
                     tol=object@control.slopes$reltol)$minimum


if(!is.na(mktElast) && mktElast < -1){


  object@shareInside <-    1 - (mktElast + 1)/(1-minGamma )
  idxShare <-  1 - object@shareInside
  idxPrice <- object@priceOutside
  object@normIndex <- NA_integer_

}

meanval <- log(shares) - log(idxShare) + (minGamma - 1) * (log(prices) - log(idxPrice))
meanval <- exp(meanval)

names(meanval)   <- object@labels

object@slopes    <- list(alpha=alpha,gamma=minGamma,meanval=meanval)
object@priceOutside <- idxPrice
object@mktSize <- insideSize*(1+alpha)


return(object)
}
)


#' @rdname TariffMonComRUM-methods
#' @export
setMethod(
  f= "calcMargins",
  signature= "TariffMonComLogit",
  definition=function(object,preMerger=TRUE,level=FALSE){

    if(preMerger){prices <- object@pricePre}
    else{ prices <- object@pricePost}

    result <- -1/(object@slopes$alpha * prices)

    if(level)result <- result*prices
    names(result) <- object@labels
    return(result)
  })

#' @rdname TariffMonComRUM-methods
#' @export
setMethod(
  f= "calcMargins",
  signature= "TariffMonComCES",
  definition=function(object,preMerger=TRUE,level=FALSE){

    labels <- object@labels
    result <- rep(1/(object@slopes$gamma), length(labels))



    if(level){
      if(preMerger){prices <- object@pricePre}
      else{ prices <- object@pricePost}

      result <- result*prices
    }
    names(result) <- labels

    return(result)
  })

#' @rdname TariffMonComRUM-methods
#' @export
setMethod(
  f= "calcPrices",
  signature= "TariffMonComLogit",
  definition=function(object,preMerger=TRUE,...){

    if(preMerger){mc <- object@mcPre}
    else{         mc <- object@mcPost}

    result <- mc - 1/object@slopes$alpha
    names(result) <- object@labels
    return(result)
  })

#' @rdname TariffMonComRUM-methods
#' @export
setMethod(
  f= "calcPrices",
  signature= "TariffMonComCES",
  definition=function(object,preMerger=TRUE,...){

    if(preMerger){mc <- object@mcPre}
    else{         mc <- object@mcPost}

    result <- mc/(1 - 1/object@slopes$gamma)
    names(result) <- object@labels
    return(result)
  })

