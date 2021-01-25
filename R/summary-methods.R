#'@title Summary Methods
#'@description Summary methods for the \code{TariffBertrand}, \code{QuotaBertrand}, and \code{TariffCournot} classes
#' @name summary-methods
#' @param object an instance of class  \code{TariffBertrand}, \code{QuotaBertrand}, or \code{TariffCournot}
#' @param revenue When TRUE, returns revenues, when FALSE returns quantitities. Default is FALSE.
#' @param levels When TRUE returns changes in levels rather than percents and quantities rather than shares, when FALSE, returns
#' changes as a parcent and shares rather than quantities. Default is FALSE.
#' @param parameters When TRUE, displays demand and cost parameters. Default is FALSE.
#' @param market When TRUE, displays aggregate information about the effect of a tariff.
#' When FALSE displays product-specific (or in the case of Cournot, plant-specific) effects.
#' Default is FALSE
#' @param insideOnly When TRUE, rescales shares on inside goods to sum to 1. Default is FALSE.
#' @param digits Number of digits to report. Default is 2.
#' @return Prints either market or product/plant-level summary and invisibly returns a data frame containing the same information.
#'@include TariffClasses.R
NULL
#' @rdname summary-methods
#' @export
setMethod(
  f= "summary",
  signature= "TariffBertrand",
  definition=function(object,revenue=FALSE,levels=FALSE, parameters = FALSE, market=FALSE,insideOnly = TRUE,digits=2){

    curWidth <-  getOption("width")


    pricePre   <-  object@pricePre
    pricePost  <-  object@pricePost

    tariffPre <- object@tariffPre *100
    tariffPost <- object@tariffPost *100

    if(grepl("aids",class(object),ignore.case=TRUE)){

      priceDelta <-  object@priceDelta
    }
    else{ priceDelta <- calcPriceDelta(object,levels=levels)}

    if(!levels) priceDelta <- priceDelta *100

    if(levels){
      outPre  <-  calcQuantities(object,preMerger=TRUE)
      outPost <-  calcQuantities(object,preMerger=FALSE)

      if(revenue){
        outPre <- pricePre*outPre
        outPost <- pricePost*outPost
      }

      sumlabels=paste("quantity",c("Pre","Post"),sep="")
    }

    else{

      outPre  <-  calcShares(object,preMerger=TRUE,revenue=revenue) * 100
      outPost <-  calcShares(object,preMerger=FALSE,revenue=revenue) * 100

      if(insideOnly){
        outPre <- outPre/sum(outPre)* 100
        outPost <- outPost/sum(outPost)* 100
      }

      sumlabels=paste("shares",c("Pre","Post"),sep="")
    }

    mcDelta <- object@mcDelta * 100

    if(levels){outDelta <- outPost - outPre}
    else{outDelta <- (outPost/outPre - 1) * 100}


    isForeign <- as.numeric(mcDelta > 0)
    isForeign <- factor(isForeign,levels=0:1,labels=c(" ","*"))

    results <- data.frame(pricePre=pricePre,pricePost=pricePost,
                          priceDelta=priceDelta,outputPre=outPre,
                          outputPost=outPost,outputDelta=outDelta,
                          tariffPre=tariffPre,tariffPost=tariffPost)




    rownames(results) <- paste(isForeign,object@labels)

    sharesPost <- calcShares(object,FALSE,revenue)

    if(market){


      istaxed <- tariffPre > 0 | tariffPost>0

      thisgovrev <- thispsdelta <- thiscv <- NA_real_

      try(thiscv <- CV(object),silent = TRUE)


      thispsdelta  <- NA_real_

      try(thisgovrev <- sum(tariffPost * calcRevenues(object, preMerger = FALSE) - tariffPre * calcRevenues(object, preMerger = TRUE), na.rm=TRUE)/100)

      try(thispsdelta  <- tapply(calcProducerSurplus(object,preMerger=FALSE) - calcProducerSurplus(object,preMerger=TRUE), istaxed,sum),silent=TRUE)


      foreignshare <- outPost[istaxed]
      domesticshare <- outPost[!istaxed]


      results <- with(results, data.frame(
       # 'Pre-Tariff HHI' = as.integer(round(hhi(object,preMerger=TRUE))),
       #  'HHI Change' = as.integer(round(hhi(object,preMerger=FALSE) -  hhi(object,preMerger=TRUE))),
        'Domestic Firm Price Change (%)'= sum(priceDelta[!istaxed] * domesticshare, na.rm=TRUE) / sum(domesticshare),
        'Foreign Firm Price Change (%)'= sum(priceDelta[istaxed] * foreignshare, na.rm=TRUE) / sum(foreignshare),
        'Industry Price Change (%)' = calcPriceDelta(object, market=TRUE,levels=FALSE)*100,
        'Consumer Harm ($)' = thiscv,
        'Domestic Firm Benefit ($)' = thispsdelta[1],
        'Foreign Firm Harm ($)' = -thispsdelta[2],
        `Gov't Revenue ($)` = thisgovrev,
        'Net Domestic Harm ($)'= thiscv - thispsdelta[1]  - thisgovrev,
        'Net Total Harm ($)'= thiscv - thispsdelta[1] - thispsdelta[2] - thisgovrev,

        check.names=FALSE
      ))

      if(levels){colnames(results) <- gsub("%","$/unit",colnames(results))}


    }

    colnames(results)[colnames(results) %in% c("outputPre","outputPost")] <- sumlabels

    cat("\nMerger simulation results under '",class(object),"' demand:\n\n",sep="")

    options("width"=ifelse(market,25,100)) # this width ensures that everything gets printed on the same line
    print(round(results,digits),digits=digits, row.names=ifelse(market, FALSE, TRUE))
    options("width"=curWidth) #restore to current width



    if(!market){

      results <- cbind(isForeign, results)
      rownames(results) <- object@labels

      cat("\n\tNotes: '*' indicates foreign products.\n ")
      if(levels){cat("\tDeltas are level changes.\n")}
      else{cat("\tDeltas are percent changes.\n")}
      if(revenue){cat("\tOutput is based on revenues.\n")}
      else{cat("\tOutput is based on units sold.\n")}

    }



    cat("\n\n")


    if(parameters){

      cat("\nDemand Parameter Estimates:\n\n")
      if(is.list(object@slopes)){
        print(lapply(object@slopes,round,digits=digits))
      }
      else{
        print(round(object@slopes,digits))
      }
      cat("\n\n")

      if(.hasSlot(object,"intercepts")){

        cat("\nIntercepts:\n\n")
        print(round(object@intercepts,digits))
        cat("\n\n")

      }

      if(.hasSlot(object,"constraint") && object@constraint){cat("\nNote: (non-singleton) nesting parameters are constrained to be equal")}
      cat("\n\n")

    }


    return(invisible(results))

  })


#' @rdname summary-methods
#' @export
setMethod(
  f= "summary",
  signature= "QuotaBertrand",
  definition=function(object,revenue=FALSE,levels=FALSE, parameters = FALSE, market=FALSE,insideOnly = TRUE,digits=2){

    curWidth <-  getOption("width")


    pricePre   <-  object@pricePre
    pricePost  <-  object@pricePost

    quotaPre <- object@quotaPre *100
    quotaPost <- object@quotaPost *100

    if(grepl("aids",class(object),ignore.case=TRUE)){

      priceDelta <-  object@priceDelta
    }
    else{ priceDelta <- calcPriceDelta(object,levels=levels)}

    if(!levels) priceDelta <- priceDelta *100

    if(levels){
      outPre  <-  calcQuantities(object,preMerger=TRUE)
      outPost <-  calcQuantities(object,preMerger=FALSE)

      if(revenue){
        outPre <- pricePre*outPre
        outPost <- pricePost*outPost
      }

      sumlabels=paste("quantity",c("Pre","Post"),sep="")
    }

    else{

      outPre  <-  calcShares(object,preMerger=TRUE,revenue=revenue) * 100
      outPost <-  calcShares(object,preMerger=FALSE,revenue=revenue) * 100

      if(insideOnly){
        outPre <- outPre/sum(outPre)* 100
        outPost <- outPost/sum(outPost)* 100
      }

      sumlabels=paste("shares",c("Pre","Post"),sep="")
    }

    mcDelta <- object@mcDelta * 100

    if(levels){outDelta <- outPost - outPre}
    else{outDelta <- (outPost/outPre - 1) * 100}


    isForeign <- is.finite(quotaPre) | is.finite(quotaPost)
    isForeign <- factor(isForeign,levels=c(FALSE,TRUE),labels=c(" ","*"))

    results <- data.frame(pricePre=pricePre,pricePost=pricePost,
                          priceDelta=priceDelta,outputPre=outPre,
                          outputPost=outPost,outputDelta=outDelta,
                          quotaPre=quotaPre,quotaPost=quotaPost)




    rownames(results) <- paste(isForeign,object@labels)

    sharesPost <- calcShares(object,FALSE,revenue)

    if(market){


      istaxed <- is.finite(quotaPre) | is.finite(quotaPost)

      thisgovrev <- thispsdelta <- thiscv <- NA_real_

      try(thiscv <- CV(object),silent = TRUE)


      thispsdelta  <- NA_real_
      try(thispsdelta  <- tapply(calcProducerSurplus(object,preMerger=FALSE) - calcProducerSurplus(object,preMerger=TRUE), istaxed,sum),silent=TRUE)


      foreignshare <- outPost[istaxed]
      domesticshare <- outPost[!istaxed]


      results <- with(results, data.frame(
      #  'Pre-Quota HHI' = as.integer(round(hhi(object,preMerger=TRUE))),
      #  'HHI Change' = as.integer(round(hhi(object,preMerger=FALSE) -  hhi(object,preMerger=TRUE))),
        'Domestic Firm Price Change (%)'= sum(priceDelta[!istaxed] * domesticshare, na.rm=TRUE) / sum(domesticshare),
        'Foreign Firm Price Change (%)'= sum(priceDelta[istaxed] * foreignshare, na.rm=TRUE) / sum(foreignshare),
        'Industry Price Change (%)' = calcPriceDelta(object, market=TRUE)*100,
        'Consumer Harm ($)' = thiscv,
        'Domestic Firm Benefit ($)' = thispsdelta[1],
        'Foreign Firm Harm ($)' = -thispsdelta[2],
        `Gov't Revenue ($)` = 0,
        'Net Harm ($)'= thiscv - thispsdelta[1] - thispsdelta[2],

        check.names=FALSE
      ))

      if(levels){colnames(results) <- gsub("%","$/unit",colnames(results))}


    }

    colnames(results)[colnames(results) %in% c("outputPre","outputPost")] <- sumlabels

    cat("\nMerger simulation results under '",class(object),"' demand:\n\n",sep="")

    options("width"=ifelse(market,25,100)) # this width ensures that everything gets printed on the same line
    print(round(results,digits),digits=digits, row.names=ifelse(market, FALSE, TRUE))
    options("width"=curWidth) #restore to current width



    if(!market){

      results <- cbind(isForeign, results)
      rownames(results) <- object@labels

      cat("\n\tNotes: '*' indicates foreign products.\n ")
      if(levels){cat("\tDeltas are level changes.\n")}
      else{cat("\tDeltas are percent changes.\n")}
      if(revenue){cat("\tOutput is based on revenues.\n")}
      else{cat("\tOutput is based on units sold.\n")}

    }



    cat("\n\n")


    if(parameters){

      cat("\nDemand Parameter Estimates:\n\n")
      if(is.list(object@slopes)){
        print(lapply(object@slopes,round,digits=digits))
      }
      else{
        print(round(object@slopes,digits))
      }
      cat("\n\n")

      if(.hasSlot(object,"intercepts")){

        cat("\nIntercepts:\n\n")
        print(round(object@intercepts,digits))
        cat("\n\n")

      }

      if(.hasSlot(object,"constraint") && object@constraint){cat("\nNote: (non-singleton) nesting parameters are constrained to be equal")}
      cat("\n\n")

    }


    return(invisible(results))

  })


#' @rdname summary-methods
setMethod(
  f= "summary",
  signature= "TariffCournot",
  definition=function(object,market=FALSE,revenue=FALSE,levels=FALSE,parameters=FALSE,digits=2){

    if(market){nplants <- 1}
    else{ nplants <- nrow(object@quantities) }

    curWidth <-  getOption("width")
    curSci  <-  getOption("scipen")

    tariffPre <-  object@tariffPre * 100
    tariffPost <- object@tariffPost * 100

    pricePre   <-  object@pricePre
    pricePost  <-  object@pricePost
    priceDelta <- calcPriceDelta(object,levels=levels)
    if(!levels) priceDelta <- priceDelta *100

    if(levels){
      outPre  <-  object@quantityPre
      outPost <-  object@quantityPost
      sumlabels=paste("quantity",c("Pre","Post"),sep="")

      if(revenue){
        outPre <- t(pricePre*t(outPre))
        outPost <- t(pricePost*t(outPost))
        sumlabels=paste("revenue",c("Pre","Post"),sep="")
      }

    }

    else{

      outPre  <-  calcShares(object,preMerger=TRUE,revenue=revenue) * 100
      outPost <-  calcShares(object,preMerger=FALSE,revenue=revenue) * 100


      sumlabels=paste("shares",c("Pre","Post"),sep="")
    }



    if(market) {

      istaxed <- tariffPre > 0 | tariffPost > 0

      thisgovrev <- thispsdelta <- thiscv <- NA

      try(thiscv <- CV(object),silent = TRUE)

      try(thisgovrev <- sum(tariffPost * calcRevenues(object, preMerger = FALSE) - tariffPre * calcRevenues(object, preMerger = TRUE), na.rm=TRUE)/100)

      try(thispsdelta  <- tapply(calcProducerSurplus(object,preMerger=FALSE) - calcProducerSurplus(object,preMerger=TRUE), istaxed,sum),silent=TRUE)



      foreignshare <- outPost[istaxed]
      domesticshare <- outPost[!istaxed]

      results <- data.frame(
        # 'Current Tariff HHI' = as.integer(round(hhi(object,preMerger=TRUE))),
        # 'HHI Change' = as.integer(round(hhi(object,preMerger=FALSE) -  hhi(object,preMerger=TRUE))),
        'Industry Price Change (%)' = calcPriceDelta(object)*100,
        'Consumer Harm ($)' = thiscv,
        'Domestic Firm Benefit ($)' = thispsdelta[1],
        'Foreign Firm Harm ($)' = -thispsdelta[2],
        `Gov't Revenue ($)` = thisgovrev,
        'Net Domestic Harm ($)'= thiscv - thispsdelta[1]  - thisgovrev,
        'Net Total Harm ($)'= thiscv - thispsdelta[1] - thispsdelta[2] - thisgovrev,

        check.names=FALSE
      )


      }
    else{
      ids <- expand.grid(plant=object@labels[[1]], product=object@labels[[2]])



      out <- data.frame(product=ids$product,
                        plant=ids$plant,outPre=as.vector(t(outPre)),
                        outPost = as.vector(t(outPost)))

      out$isForeign <- as.numeric( tariffPre >0 | tariffPost >0 )
      out$isForeign <- factor(out$isForeign,levels=0:1,labels=c(" ","*"))
      out$tariffPre <- tariffPre
      out$tariffPost <- tariffPost

      if(levels){out$outDelta <- out$outPost - out$outPre}
      else{out$outDelta <- (out$outPost/out$outPre - 1) * 100}
      out$pricePre <- rep(pricePre,each=nplants)
      out$pricePost <- rep(pricePost,each=nplants)
      out$priceDelta <- rep(priceDelta, each=nplants)

      results <- out[, c("isForeign","product","plant", "pricePre","pricePost","priceDelta","outPre","outPost","outDelta", "tariffPre","tariffPost" )]

      colnames(results)[colnames(results) %in% c("outPre","outPost")] <- sumlabels
    }



    cat("\nTariff simulation results under '",class(object),"' demand:\n\n",sep="")

    options("width"=110) # this width ensures that everything gets printed on the same line
    options("scipen"=999) # this width ensures that everything gets printed on the same line
    print(format(results,digits=digits),row.names = FALSE)
    options("width"=curWidth) #restore to current width
    options("scipen"=curSci) #restore to current scientific notation

    if(!market){
    cat("\n\tNotes: '*' indicates foreign plants.\n ")
    if(levels){cat("\tDeltas are level changes.\n")}
    else{cat("\tDeltas are percent changes.\n")}
    if(revenue){cat("\tOutput is based on revenues.\n")}
    else{cat("\tOutput is based on units sold.\n")}
}

    cat("\n\n")


    if(parameters){

      cat("\nDemand Parameter Estimates:\n\n")

      print(round(object@slopes,digits=digits))

      cat("\n\n")

      if(.hasSlot(object,"intercepts")){

        cat("\nIntercepts:\n\n")
        print(round(object@intercepts,digits=digits))
        cat("\n\n")

      }
}

  return(invisible(results))

  })
