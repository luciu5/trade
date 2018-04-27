setMethod(
  f= "summary",
  signature= "TariffBertrand",
  definition=function(object,revenue=FALSE,shares=TRUE,levels=FALSE, parameters = TRUE, market=FALSE,insideOnly = TRUE,digits=2,...){

    curWidth <-  getOption("width")


    pricePre   <-  object@pricePre
    pricePost  <-  object@pricePost

    if(grepl("aids",class(object),ignore.case=TRUE)){

      priceDelta <-  object@priceDelta
    }
    else{ priceDelta <- calcPriceDelta(object,levels=levels)}

    if(!levels) priceDelta <- priceDelta *100

    if(!shares && !missPrices){
      outPre  <-  calcQuantities(object,preMerger=TRUE)
      outPost <-  calcQuantities(object,preMerger=FALSE)

      if(revenue){
        outPre <- pricePre*outPre
        outPost <- pricePost*outPost
      }

      sumlabels=paste("quantity",c("Pre","Post"),sep="")
    }

    else{
      if(!shares){warning("'shares' equals FALSE but 'calcQuantities' not defined. Reporting shares instead of quantities")}

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
                          outputPost=outPost,outputDelta=outDelta)



    results <- cbind(results,tariff=mcDelta)


    rownames(results) <- paste(isForeign,object@labels)

    sharesPost <- calcShares(object,FALSE,revenue)

    if(market){

      tariff <- mcDelta/100
      istaxed <- tariff > 0

      thisgovrev <- thispsdelta <- thiscv <- NA_real_

      try(thiscv <- CV(object),silent = TRUE)


      thispsdelta  <- NA_real_

      try(thisgovrev <- sum(tariff * calcRevenues(object, preMerger = FALSE), na.rm=TRUE))

      try(thispsdelta  <- tapply(calcProducerSurplus(object,preMerger=FALSE) - calcProducerSurplus(object,preMerger=TRUE), istaxed,sum),silent=TRUE)


      foreignshare <- outPost[istaxed]
      domesticshare <- outPost[!istaxed]


      results <- with(results, data.frame(
        'Pre-Tariff HHI' = as.integer(round(hhi(object,preMerger=TRUE))),
        'HHI Change' = as.integer(round(hhi(object,preMerger=FALSE) -  hhi(object,preMerger=TRUE))),
        'Domestic Firm Price Change (%)'= sum(priceDelta[!istaxed] * domesticshare, na.rm=TRUE) / sum(domesticshare),
        'Foreign Firm Price Change (%)'= sum(priceDelta[istaxed] * foreignshare, na.rm=TRUE) / sum(foreignshare),
        'Industry Price Change (%)' = sum(priceDelta * sharesPost/100,na.rm=TRUE),
        'Consumer Harm ($)' = thiscv,
        'Domestic Firm Benefit ($)' = thispsdelta[1],
        'Foreign Firm Harm ($)' = -thispsdelta[2],
        `Gov't Revenue ($)` = thisgovrev,
        'Net Harm ($)'= thiscv - thispsdelta[1] - thispsdelta[2] - thisgovrev,

        #'Estimated Market Elasticity' = thiselast,
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




setMethod(
  f= "summary",
  signature= "TariffCournot",
  definition=function(object,market=FALSE,revenue=FALSE,shares=FALSE,levels=FALSE,parameters=FALSE,digits=2,...){

    if(market){nplants <- 1}
    else{ nplants <- nrow(object@quantities) }

    curWidth <-  getOption("width")
    curSci  <-  getOption("scipen")

    pricePre   <-  object@pricePre
    pricePost  <-  object@pricePost
    priceDelta <- calcPriceDelta(object,levels=levels)
    if(!levels) priceDelta <- priceDelta *100

    if(!shares){
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
      if(!shares){warning("'shares' equals FALSE but 'calcQuantities' not defined. Reporting shares instead of quantities")}


      outPre  <-  calcShares(object,preMerger=TRUE,revenue=revenue) * 100
      outPost <-  calcShares(object,preMerger=FALSE,revenue=revenue) * 100


      sumlabels=paste("shares",c("Pre","Post"),sep="")
    }

    if(market){

      outPre <- colSums(outPre,na.rm=TRUE)
      outPost <- colSums(outPost,na.rm=TRUE)
      ids <- data.frame(plant = 1 ,product= object@labels[[2]])
    }


    else{

      ids <- expand.grid(plant=object@labels[[1]], product=object@labels[[2]])
    }


    out <- data.frame(product=ids$product,
                      plant=ids$plant,outPre=as.vector(t(outPre)),
                      outPost = as.vector(t(outPost)))

    if(market) {out$plant <- NULL}
    else{
      out$isForeign <- as.numeric(rowSums( abs(object@ownerPost - object@ownerPre))>0)
      out$isForeign <- factor(out$isForeign,levels=0:1,labels=c(" ","*"))
    }

    tariff <- object@mcDelta * 100

    if(levels){out$outDelta <- out$outPost - out$outPre}
    else{out$outDelta <- (out$outPost/out$outPre - 1) * 100}

    out$pricePre <- rep(pricePre,each=nplants)
    out$pricePost <- rep(pricePost,each=nplants)
    out$priceDelta <- rep(priceDelta, each=nplants)

    if(market){
      results <- out[,c("product","pricePre","pricePost","priceDelta","outPre","outPost","outDelta" )]
    }

    else{
      results <- out[, c("isForeign","product","plant", "pricePre","pricePost","priceDelta","outPre","outPost","outDelta" )]
    }

    colnames(results)[colnames(results) %in% c("outPre","outPost")] <- sumlabels

    if(!market && sum(abs(tariff))>0) results <- cbind(results,tariff=tariff)



    sharesPost <- calcShares(object,FALSE,revenue)

    cat("\nTariff simulation results under '",class(object),"' demand:\n\n",sep="")

    options("width"=100) # this width ensures that everything gets printed on the same line
    options("scipen"=999) # this width ensures that everything gets printed on the same line
    print(format(results,digits=digits),row.names = FALSE)
    options("width"=curWidth) #restore to current width
    options("scipen"=curSci) #restore to current scientific notation

    cat("\n\tNotes: '*' indicates foreign plants.\n ")
    if(levels){cat("\tDeltas are level changes.\n")}
    else{cat("\tDeltas are percent changes.\n")}
    if(revenue){cat("\tOutput is based on revenues.\n")}
    else{cat("\tOutput is based on units sold.\n")}


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
