require(shiny)
require(rhandsontable)


shinyServer(function(input, output, session) {


   nPossProds <- 10 #only allow 10 products by default

   msgCatcher <-

   function(expr)
   {
     W <- NULL
     E <- NULL

     w.handler <- function(w){ # warning handler
       W <<- append(W,conditionMessage(w))
       invokeRestart("muffleWarning")

     }
     e.handler <- function(e){ # error handler
       E <<- append(E, conditionMessage(e))
       NULL

     }
     list(value = withCallingHandlers(tryCatch(expr, error = e.handler),
                                      warning = w.handler),
          warning = W, error = E)
   }




   isOverID <-  function(supply, calcElast, inputData){




   provElast <- grepl('elasticity',calcElast)



   nMargins <- inputData[,grepl("Margins",colnames(inputData))]
   nMargins <- length(nMargins[!is.na(nMargins)])


   if(supply == "Cournot" &&
      ((provElast && nMargins > 0)  || (!provElast && nMargins >1) )){
     res <- paste(helpText(tags$b("Note:"), "some model parameters are over-identified. The tables above may be helpful in assessing model fit."))
   }

   else if(supply != "Cournot" &&
           ((provElast && nMargins > 1)  || (!provElast && nMargins >2) )){
     res <- paste(helpText(tags$b("Note:"),"some model parameters are over-identified. The tables above may be helpful in assessing model fit."))
   }
   else{
     res <- paste(helpText(tags$b("Note:"),"model parameters are just-identified. Inputted and fitted values should match."))

   }
   res
}


   genInputData <- function(nrows,type = c("Tariffs","Quotas")){
    # a function to generate default input data set for simulations

     type=match.arg(type)

      exampleData <- data.frame(
       Name = c("Prod1","Prod2","Prod3","Prod4"),
       Owner  = c("Firm1","Firm2","Firm3","Firm3"),
       'Prices \n($/unit)'    = rep(10,4),
       'Quantities'   =c(0.4,.3,.2,.1)*100,
       'Margins\n(p-c)/p' =c(0.25,NA,NA,NA),
       stringsAsFactors = FALSE,
       check.names=FALSE
     )



      exampleData <- exampleData[order(exampleData$`Quantities`, decreasing = TRUE),]
      rownames(exampleData) <- NULL

      if(type == "Tariffs"){
        fx <- data.frame('Current \nTariff \n(proportion)' = c(.05,.05,0,0),
                         'New \nTariff \n(proportion)' = c(.25,.25,0,0),
                         stringsAsFactors = FALSE,
                         check.names=FALSE)
      }
      else if (type == "Quotas"){
        fx <- data.frame('Current \nQuota \n(proportion)' = c(Inf,Inf,Inf,Inf),
                         'New \nQuota \n(proportion)' = c(.75,.75,Inf,Inf),
                         stringsAsFactors = FALSE,
                         check.names=FALSE)

      }


      exampleData <- cbind(exampleData, fx)

      inputData <- as.data.frame(matrix(NA_real_,nrow=max(nrows, nrow(exampleData)),ncol= ncol(exampleData)))
      colnames(inputData) <- colnames(exampleData)
      inputData[1:nrow(exampleData),] <- exampleData


     return(inputData)

   }




   gencode <- function(type){

     if(type == "Tariffs"){
       indata <-   values[["inputData"]]
     }
     else if(type == "Quotas"){
       indata <-   valuesQuota[["inputData"]]}

     indata <- indata[!is.na(indata$Name) & indata$Name != '',]

     cnames <- colnames(indata)
     cnames <- gsub("\n","",cnames)

     firstPrice <- which(!is.na(indata[,grep("price",cnames, ignore.case = TRUE)]))[1]

     argnames <- c("demand",
                   "owner",
                   "prices",
                   "quantities",
                   "margins",
                   "tariffPre",
                   "tariffPost"

     )


     argvalues   <- paste0(argnames," = simdata$`",cnames,"`")

     thisElast <- ifelse(grepl("elast",input$calcElast),
                         input$enterElast,
                         "NA_real_"
     )

     if(grepl("logit", input$demand, ignore.case =TRUE)){
       thisSize <- "sum(simdata$`Quantities`)"
     }
     else if(all(is.na(indata[,grepl("price",cnames, ignore.case = TRUE)]))){

       thisSize <-   "sum(simdata$`Revenues`)"
     }

     else{

       thisSize <- paste0("sum(simdata$`",grep("price",cnames, ignore.case = TRUE, value=TRUE),"`*simdata$`Quantities`)")


     }

     thisdemand <- gsub("\\s*\\(.*","",input$demand,perl=TRUE)

     argvalues[1] <- paste(c("demand = ", shQuote(thisdemand)), collapse = "")

     argvalues <- c(argvalues,
                    paste0("mktElast = ", thisElast,collapse = ""),
                    #paste0("insideSize = ",thisSize, collapse=""),
                    "labels = simdata$Name"

     )




     if(input$supply == "Cournot"){
       atrfun <- "cournot_tariff"
       argvalues[grep("prices", argvalues)] <- paste0(argvalues[grep("prices", argvalues)],"[",firstPrice,"]")
       argvalues[grep("quantities", argvalues)] <- "quantities = as.matrix(simdata$`Quantities`)"
       argvalues[grep("margins", argvalues)] <- paste0("margins = as.matrix(simdata$`",grep("Margin",cnames,value = TRUE),"`)")
       argvalues[grep("tariffPre", argvalues)] <- paste0("tariffPre = as.matrix(simdata$`",grep("Current Tariff",cnames,value = TRUE),"`)")
       argvalues[grep("tariffPost", argvalues)] <- paste0("tariffPost = as.matrix(simdata$`",grep("New Tariff",cnames,value = TRUE),"`)")
       argvalues[grep("labels", argvalues)] <- sprintf("labels = list(as.character(simdata$Name),as.character(simdata$Name[%d]))",firstPrice)
       argvalues <- argvalues[grep("insideSize", argvalues, invert = TRUE)]
     }
     else if( input$supply =="Bertrand"){atrfun <- "bertrand_tariff"}
     else{atrfun <- "auction2nd.logit.alm"
     argvalues <- argvalues[-1]
     argvalues[grep("quantities", argvalues)] <- "shares = simdata$`Quantities` / sum( simdata$`Quantities` ) "
     argvalues[grep("margins", argvalues)] <- paste0("margins = simdata$`",
                                                     grep("Margin",cnames,value = TRUE),
                                                     "` * ", "simdata$`", grep("Price", cnames, value=TRUE),"`")
     }

     atrfun <- paste0("simres <- ",atrfun,"(\n\t",paste0(argvalues,collapse = ",\n\t"),")",collapse = "\n")

     indata_code <- sapply(1:ncol(indata),
                           function(x){d <- indata[,x];
                           if(is.character(d)){d <- sprintf("'%s'", indata[,x])};
                           paste0('`',cnames[x],'`',"= c(",paste(d,collapse=","),")")}
     )
     indata_code <- paste0("simdata <- data.frame(\n\t", paste0(indata_code,collapse=",\n\t"),",\n check.names = FALSE,\n stringsAsFactors = FALSE\n)")

     if(type =="Quotas"){
       atrfun <- gsub("tariff","quota",atrfun)
       indata_code <- gsub("Tariff","Quota",indata_code)
     }

     thiscode <- c(
       "library(trade)",
       "\n\n ## Load Data:\n",
       indata_code,
       "\n\n ## Run Simulation: \n",
       atrfun,
       "\n\n ## Summary Tab Results:\n",
       "summary(simres, revenues = FALSE, levels = FALSE, market=TRUE)",
       "\n\n ## Details Tab Results:\n",
       "summary(simres, revenues = FALSE, levels = FALSE, market=FALSE)\n\n",
       "\n\n ## Elasticities Tab Results  (Pre-tariff Only):\n",
       "elast(simres, preMerger = TRUE, market=TRUE)\n elast(simres, preMerger = TRUE, market=FALSE)",
       "\n\n ## Diagnostics Tab Results:\n",
       "calcDiagnostics(simres)\n\n"
     )

     return(thiscode)





   }


   genShareOut <- function(sim){
   if( grepl("cournot",class(sim),ignore.case = TRUE)){return()}

   isCES <- grepl("ces",class(sim),ignore.case = TRUE)

   res <- data.frame('No-purchase\n Share (%)'= c(
     1 - sum(calcShares(sim, preMerger=TRUE,revenue=isCES)),
     1 - sum(calcShares(sim, preMerger=FALSE,revenue=isCES))
   )
   ,check.names = FALSE
   )*100

   res$'Revenues ($)' <- as.integer(round(c(calcRevenues(sim, preMerger=TRUE, market = TRUE ),
                                            calcRevenues(sim, preMerger=FALSE, market = TRUE )
   )))


   rownames(res) <- c("Current Tariff","New Tariff")

   if( grepl("aids",class(sim),ignore.case = TRUE)) res$'No-purchase\n Share (%)' <- NULL

   return(res)
   }

   gensum <- function(res, indata,type = c("Tariffs","Quotas")){
     #a function to generate summary stats for results tab


     type=match.arg(type)

     isCournot <- grepl("Cournot",class(res))
     isAuction <- grepl("Auction",class(res))
     isRevDemand <- grepl("ces|aids",class(res),ignore.case = TRUE)
     isLogit <- grepl("logit",class(res),ignore.case = TRUE)

     missPrices <- any(is.na(res@prices))

     inLevels <- FALSE

     if(isAuction && missPrices){inLevels = TRUE}

     indata <- indata[!is.na(indata$Name),]



     tariffPre <- indata[,grepl("Cur.*\\n(Tariff|Quota)",colnames(indata), perl= TRUE),drop=TRUE]
     tariffPost <- indata[,grepl("New.*\\n(Tariff|Quota)",colnames(indata), perl=TRUE),drop=TRUE]


     if(type == "Tariffs"){
     tariffPre[is.na(tariffPre)] <- 0
     tariffPost[is.na(tariffPost)] <- 0
     istaxed <- tariffPre > 0 | tariffPost > 0
     }
     else if (type=="Quotas"){
       ## set quota for unconstrained firms to be Infinite
       istaxed <- is.finite(tariffPre) | is.finite(tariffPost)
     }


     if(isCournot){

       capture.output( s <- summary(res, market = FALSE))
       theseshares <- drop(res@quantities/sum(res@quantities))

       totQuantPost <- sum(s$quantityPost,na.rm=TRUE)
       s$sharesPost <- s$quantityPost/totQuantPost*100


     }

     else{
       capture.output(s <- summary(res, revenue = isRevDemand & missPrices,levels = inLevels))
       theseshares <- calcShares(res, preMerger=TRUE, revenue=isRevDemand & missPrices)


       theseshares <- theseshares/sum(theseshares)

     }





     thisgovrev <- thispsdelta <- thiscv <- NA

     try(thiscv <- CV(res),silent = TRUE)

     if(type == "Quotas"){
       tariffPre[] <- 0
       tariffPost[] <- 0
     }

     try(thisgovrev <- sum(tariffPost * calcRevenues(res, preMerger = FALSE) - tariffPre * calcRevenues(res, preMerger = TRUE), na.rm=TRUE))


       try(thispsdelta  <- tapply(drop(calcProducerSurplus(res,preMerger=FALSE)*(1 - tariffPost)) - drop(calcProducerSurplus(res,preMerger=TRUE)*(1 - tariffPre)), istaxed,sum),silent=TRUE)




     foreignshare <- s$sharesPost[istaxed]
     domesticshare <- s$sharesPost[!istaxed]


     res <- with(s, data.frame(
           #'Current Tariff HHI' = as.integer(round(hhi(res,preMerger=TRUE))),
           #'HHI Change' = as.integer(round(hhi(res,preMerger=FALSE) -  hhi(res,preMerger=TRUE))),
           'Domestic Firm Price Change (%)'= sum(priceDelta[!istaxed] * domesticshare, na.rm=TRUE) / sum(domesticshare),
           'Foreign Firm Price Change (%)'= sum(priceDelta[istaxed] * foreignshare, na.rm=TRUE) / sum(foreignshare),
           'Industry Price Change (%)' = sum(priceDelta * sharesPost/100,na.rm=TRUE),
           'Consumer Harm ($)' = thiscv,
           'Domestic Firm Benefit ($)' = thispsdelta[1],
           'Foreign Firm Harm ($)' = -thispsdelta[2],
           `Gov't Revenue ($)` = thisgovrev,
           #'Net Domestic Harm ($)'= thiscv - thispsdelta[1]  - thisgovrev,
           'Net Total Harm ($)'= thiscv - thispsdelta[1] - thispsdelta[2] - thisgovrev,

           #'Estimated Market Elasticity' = thiselast,
           check.names=FALSE
     ))

     if(inLevels){
       colnames(res) <- gsub('(?<=Price Change\\s)\\(%\\)',"($/unit)",colnames(res), perl=TRUE)
     }

     return(res)
   }



   gendiag <- function(res,mktElast=FALSE){
     #a function to generate diagnostics data

    isCournot <- grepl("Cournot",class(res))

    if(isCournot){labels= res@labels[[1]]}
    else{labels=res@labels}

    obsPrices <- res@prices
    obsShares <- res@shares
    obsMargins <- res@margins
    obsElast <- res@mktElast

    #if(length(obsMargins[!is.na(obsMargins)]) < 2){return()}


    prePrices <- unname(drop(res@pricePre))
    preMargins <- drop(calcMargins(res, preMerger=TRUE))
    preShares <- drop(calcShares(res, preMerger=TRUE))
    preShares <- drop(preShares/sum(preShares))
    preElast <- elast(res, preMerger=TRUE, market=TRUE)

    if(!mktElast){

    res <- data.frame(
      "Inputted Prices"= obsPrices,
      "Fitted Prices" = prePrices,
      "Price Change (%)"= (1 - obsPrices/prePrices)*100,
      "Inputted Shares (%)" = obsShares*100,
      "Fitted Shares(%)"=preShares*100,
      "Share Change (%)"=(1 - obsShares/preShares)*100,
      "Inputted Margins" = obsMargins,
      "Fitted  Margins"=preMargins,
      "Margin Change (%)"= (1 - obsMargins/preMargins)*100,
      #'Market Elasticity'= 1 - obsElast/preElast,
      check.names = FALSE
    )

    #rmThese <- colSums(abs(res),na.rm=TRUE)



    if(isCournot)  res[-1,grepl('Prices',colnames(res))] <- NA

    #res <- res[,rmThese >1e-3,drop=FALSE]


    if(!isCournot) rownames(res) <- labels

    }

    else{ res <- data.frame(
                    'Inputted Elasticity' = obsElast,
                    'Fitted Elasticity' = preElast,
                     'Elasticity Change'= (1 - obsElast/preElast)*100,
                            check.names = FALSE)

    #if(res < 1e-3) res <- NULL
    }
    return(res)
   }

   runSims <- function(supply, demand, indata, mktElast, type = c("Tariffs","Quotas")){
     # a function to execute code from antitrust package based on ui inputs

     type <- match.arg(type)

     prices <- indata[,"Prices \n($/unit)"]
     margins <- indata$Margins

     tariffPre  <- indata[,grepl("Cur.*\\n(Tariff|Quota)",colnames(indata),perl=TRUE),drop=TRUE]
     tariffPost <- indata[,grepl("New.*\\n(Tariff|Quota)",colnames(indata),perl=TRUE),drop=TRUE]


     if(type == "Tariffs"){
       tariffPre[is.na(tariffPre)] <- 0
       tariffPost[is.na(tariffPost)] <- 0
     }
     else if(type == "Quotas"){
       ## set quota for unconstrained firms to be Inf
       tariffPre[is.na(tariffPre)] <- Inf
       tariffPost[is.na(tariffPost)] <- Inf
     }
     missPrices <- any(is.na(prices))

     shares_quantity <- shares_revenue <- indata$Output/sum(indata$Output, na.rm=TRUE)
     insideSize <- sum(indata[,"Output"], na.rm=TRUE)



     if(!missPrices){

       if(any(grepl("ces|aids",demand,perl=TRUE,ignore.case=TRUE))) insideSize <- sum(prices * indata[,"Output"], na.rm =TRUE)

       shares_revenue <- prices * shares_revenue / sum(prices * shares_revenue)
       if(supply == "2nd Score Auction") margins <- margins * prices # convert to level margins
     }

     firstMargin <- which(!is.na(margins))[1]
     firstPrice <- which(!is.na(prices))[1]


     ownerPre = model.matrix(~-1+indata$Owner)
     ownerPre = tcrossprod(ownerPre)
     ownerPost = ownerPre


     if(supply != "Cournot" && type == "Tariffs" ){
        ownerPost =ownerPost*(1-tariffPost)
        ownerPre =ownerPre*(1-tariffPre)
        }




if( type == "Tariffs"){
     switch(supply,
            Bertrand =
              switch(demand,
                     `logit (unknown elasticity)`= logit.alm(prices= prices,
                                                             shares= shares_quantity,
                                                             margins= margins,
                                                             ownerPre= ownerPre,
                                                             ownerPost= ownerPost,
                                                             insideSize = insideSize ,
                                                             mcDelta = indata$mcDelta, labels=indata$Name),
                     `aids (unknown elasticity)` = aids(prices= prices,
                               shares= shares_revenue,
                               margins= margins,
                               ownerPre= ownerPre,
                               ownerPost= ownerPost,
                               insideSize = insideSize ,
                               mcDelta = indata$mcDelta, labels=indata$Name),
                     `ces (unknown elasticity)`= ces.alm(prices= prices,
                                                         shares= shares_revenue,
                                                         margins= margins,
                                                         ownerPre= ownerPre,
                                                         ownerPost= ownerPost,
                                                         insideSize = insideSize ,
                                                         mcDelta = indata$mcDelta, labels=indata$Name),
                     linear=linear(prices= prices,
                                   quantities= indata[,"Output"],
                                   margins= margins,
                                   ownerPre= ownerPre,
                                   ownerPost= ownerPost,
                                   mcDelta = indata$mcDelta, labels=indata$Name),
                     aids=aids(prices= prices,
                               shares= shares_revenue,
                               margins= margins,
                               ownerPre= ownerPre,
                               ownerPost= ownerPost,
                               insideSize = insideSize ,
                               mcDelta = indata$mcDelta, labels=indata$Name, mktElast = mktElast),
                     logit= logit.alm(prices= prices,
                                      shares= shares_quantity,
                                      margins= margins,
                                      ownerPre= ownerPre,
                                      ownerPost= ownerPost,
                                      insideSize = insideSize ,
                                      mcDelta = indata$mcDelta, labels=indata$Name,  mktElast = mktElast ),
                     ces = ces.alm(prices= prices,
                                   shares= shares_revenue,
                                   margins= margins,
                                   ownerPre= ownerPre,
                                   ownerPost= ownerPost,
                                   insideSize = insideSize ,
                                   mcDelta = indata$mcDelta, labels=indata$Name,  mktElast = mktElast),
                     linear=linear(prices= prices,
                                   quantities= indata[,"Output"],
                                   margins= margins,
                                   ownerPre= ownerPre,
                                   ownerPost= ownerPost,
                                   mcDelta = indata$mcDelta, labels=indata$Name),
                     pcaids=pcaids(prices= prices,
                                   shares= shares_revenue,
                                   knownElast = -1/margins[firstMargin],
                                   knownElastIndex = firstMargin,
                                   mktElast = mktElast,
                                   insideSize = insideSize,
                                   ownerPre= ownerPre,
                                   ownerPost= ownerPost,
                                   mcDelta = indata$mcDelta, labels=indata$Name)
              ),
            Cournot =

              cournot(prices= prices[firstPrice],
                      demand = gsub("\\s+\\(.*","",demand,perl=TRUE),
                      cost= rep("linear", nrow(indata)),
                      quantities = as.matrix(indata[,"Output"]),
                      margins= as.matrix(margins),
                      ownerPre= ownerPre,
                      ownerPost= ownerPost,
                      mktElast = ifelse( grepl("unknown elasticity", demand),
                                         NA_real_, mktElast),
                      mcDelta = indata$mcDelta,
                      labels=list(as.character(indata$Name),indata$Name[firstPrice]))
            ,
            `2nd Score Auction`= switch(demand,
                                        `logit (unknown elasticity)` = auction2nd.logit.alm(prices= prices,
                                                                                            shares= shares_quantity,
                                                                                            margins= margins,
                                                                                            ownerPre= ownerPre,
                                                                                            ownerPost= ownerPost,
                                                                                            mcDelta = indata$mcDelta, labels=indata$Name),
                                        logit = auction2nd.logit.alm(prices= prices,
                                                                     shares= shares_quantity,
                                                                     margins= margins,
                                                                     ownerPre= ownerPre,
                                                                     ownerPost= ownerPost,
                                                                     insideSize = insideSize,
                                                                     mcDelta = indata$mcDelta, labels=indata$Name,
                                                                     mktElast = mktElast)

            )
     )
}

else if ( type == "Quotas"){


  switch(supply,
         Bertrand =
           switch(demand,
                  `logit (unknown elasticity)`= logit.cap.alm(prices= prices,
                                                          shares= shares_quantity,
                                                          margins= margins,
                                                          capacitiesPre = indata$Output*tariffPre,
                                                          capacitiesPost = indata$Output*tariffPost,
                                                          ownerPre= ownerPre,
                                                          ownerPost= ownerPost,
                                                          insideSize = insideSize ,
                                                          mcDelta = indata$mcDelta, labels=indata$Name),
                  logit= logit.cap.alm(prices= prices,
                                   shares= shares_quantity,
                                   margins= margins,
                                   capacitiesPre = indata$Output*tariffPre,
                                   capacitiesPost = indata$Output*tariffPost,
                                   ownerPre= ownerPre,
                                   ownerPost= ownerPost,
                                   insideSize = insideSize ,
                                   mcDelta = indata$mcDelta, labels=indata$Name,  mktElast = mktElast )
           ),
         Cournot =

           cournot(prices= prices[firstPrice],
                   demand = gsub("\\s+\\(.*","",demand,perl=TRUE),
                   cost= rep("linear", nrow(indata)),
                   quantities = as.matrix(indata[,"Output"]),
                   margins= as.matrix(margins),
                   ownerPre= ownerPre,
                   ownerPost= ownerPost,
                   mktElast = ifelse( grepl("unknown elasticity", demand),
                                      NA_real_, mktElast),
                   mcDelta = indata$mcDelta,
                   labels=list(as.character(indata$Name),indata$Name[firstPrice]))
  )


}
   }



   # ## create a reactive object that tracks which demand is being used
   # demand <- eventReactive(input$simulate, {
   #                                         ifelse(input$supply =="Bertrand",
   #
   #                                            ifelse(input$calcElast == "2 or more margins",input$demand_bert_alm,input$demand_bert),
   #                                                    ifelse(input$supply == "Cournot",
   #                                                   ifelse(input$calcElast == "1 or more margins",input$demand_cournot_alm, input$demand_cournot),
   #                                                   ifelse(input$calcElast == "2 or more margins",input$demand_2nd_alm, input$demand_2nd)))
   #                    })
   #

   ## create a reactive list of objects

   valuesQuota <-   reactiveValues(inputData = genInputData(nPossProds), sim =NULL, msg = NULL)
   values <-  reactiveValues(inputData = genInputData(nPossProds), sim =NULL, msg = NULL)



   ## initialize  inputData
 observeEvent(input$menu=="Tariffs" | input$addRows,{

                  values[["inputData"]] <- genInputData(nrow = input$addRows ,type = "Tariffs")}

 )

 observeEvent(input$menu == "Quotas" | input$addRowsQuota,{
     valuesQuota[["inputData"]]<- genInputData(nrow = input$addRowsQuota ,type = "Quotas" )

   })


   ## update reactive list whenever changes are made to input
    observe({

      supply <- input$supply
      demand <- gsub("\\s*(unknown elasticity)","",input$demand,perl=TRUE)

      provElast <- grepl('elasticity',input$calcElast)
      defElast <- ifelse(provElast, 1 , 2)

     if(supply == 'Cournot'){
       theseChoices <- c("market elasticity and 0 or more margins",
                         "1 or more margins"
       )


       demandChoices<- c("linear","log")

       }

    else{
      theseChoices <- c("market elasticity and 1 or more margins",
                        "2 or more margins"
      )
      updateRadioButtons(session = session, "calcElast", "Calibrate model parameters using:",
                         choices = theseChoices , selected = theseChoices[defElast])

      if(supply == 'Bertrand'){demandChoices<- c('logit','ces','aids')}
      else{demandChoices<- c('logit')}



    }

      if( !provElast ){ demandChoices <- paste(demandChoices,"(unknown elasticity)")}

      updateRadioButtons(session = session, "calcElast", "Calibrate model parameters using:",
                         choices = theseChoices , selected = theseChoices[defElast])

      provDemand <- grep(demand, demandChoices)
      provDemand <- ifelse(length(provDemand) > 0 , provDemand, 1)

      updateSelectInput(session=session, "demand", "Demand Specification:",
                        choices = demandChoices, selected = demandChoices[provDemand])


      if(!is.null(input$hot)){

        values[["inputData"]] = hot_to_r(input$hot)
      }



      if(!is.null(input$hotQuota)){

        valuesQuota[["inputData"]] = hot_to_r(input$hotQuota)
      }

    })


    ## display inputs
    output$hot <- renderRHandsontable({

      inputData <- values[["inputData"]]

      colnames(inputData) <- gsub("Quota","Tariff", colnames(inputData))

      prices <- inputData[,"Prices \n($/unit)"]
      output <- inputData[,grepl("Quantities|Revenue",colnames(inputData), perl=TRUE)]

      missPrices <- isTRUE(any(is.na(prices[ !is.na(output) ] ) ))


      if(input$supply == "2nd Score Auction"){ colnames(inputData) <- gsub("Tariff \n(proportion)","Tariff \n($/unit)", colnames(inputData))}
      else{colnames(inputData) <- gsub("Tariff \n($/unit)","Tariff \n(proportion)", colnames(inputData))}

      if(missPrices && input$supply =="2nd Score Auction"){colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n ($/unit)"}
      else{colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n (p-c)/p"}

      if (missPrices && any(grepl("ces|aids",input$demand, perl=TRUE), na.rm=TRUE)){colnames(inputData)[grepl("Quantities",colnames(inputData))] <- "Revenues"}
      else{{colnames(inputData)[grepl("Revenues",colnames(inputData))] <- "Quantities"}}




      if (!is.null(inputData))
        rhandsontable(inputData, stretchH = "all", contextMenu = FALSE ) %>% hot_col(col = 1:ncol(inputData), valign = "htMiddle") %>%
        hot_col(col = which (sapply(inputData,is.numeric)),halign = "htCenter" ) %>% hot_cols(columnSorting = TRUE)

    })


    output$hotQuota <- renderRHandsontable({

      inputData <- valuesQuota[["inputData"]]

      colnames(inputData) <- gsub("Tariff","Quota", colnames(inputData))

      prices <- inputData[,"Prices \n($/unit)"]
      output <- inputData[,grepl("Quantities|Revenue",colnames(inputData), perl=TRUE)]

      missPrices <- isTRUE(any(is.na(prices[ !is.na(output) ] ) ))

      if(input$supplyQuota == "2nd Score Auction"){ colnames(inputData) <- gsub("Quota \n(proportion)","Quota \n($/unit)", colnames(inputData))}
      else{colnames(inputData) <- gsub("Quota \n($/unit)","Quota \n(proportion)", colnames(inputData))}

      if(missPrices && input$supplyQuota =="2nd Score Auction"){colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n ($/unit)"}
      else{colnames(inputData)[grepl("Margins",colnames(inputData))] <- "Margins\n (p-c)/p"}

      if (missPrices && any(grepl("ces|aids",input$demandQuota, perl=TRUE), na.rm=TRUE)){colnames(inputData)[grepl("Quantities",colnames(inputData))] <- "Revenues"}
      else{{colnames(inputData)[grepl("Revenues",colnames(inputData))] <- "Quantities"}}





      if (!is.null(inputData))
        rhandsontable(inputData, stretchH = "all", contextMenu = FALSE ) %>% hot_col(col = 1:ncol(inputData), valign = "htMiddle") %>%
        hot_col(col = which (sapply(inputData,is.numeric)),halign = "htCenter" ) %>% hot_cols(columnSorting = TRUE)

    })




  ## simulate merger when the "simulate" button is clicked
   observeEvent(input$simulate | input$simulateQuota,{


     if(input$menu == "Tariffs"){
       values[["sim"]] <- values[["msg"]] <-  NULL
       updateTabsetPanel(session,inputId  = "inTabset", selected = "respanel")
       indata <- values[["inputData"]]
     }
     else if (input$menu == "Quotas"){

       valuesQuota[["sim"]] <- valuesQuota[["msg"]] <-  NULL
       updateTabsetPanel(session,inputId  = "inTabsetQuota", selected = "respanelQuota")
       indata <-   valuesQuota[["inputData"]]
     }

      isOutput <-  grepl("Quantities|Revenues",colnames(indata),perl = TRUE)


      colnames(indata)[ grepl("Margins",colnames(indata),perl = TRUE)] <- "Margins"

      colnames(indata)[isOutput] <- "Output"

      indata <- indata[!is.na(indata[,"Output"]),]


      indata$mcDelta <- 0

      if(input$menu == "Tariffs"){
         tariffPost <- indata[,grep('New.*\\n(Tariff)',colnames(indata), perl=TRUE), drop = TRUE]
         tariffPost[is.na(tariffPost)] <- 0

         tariffPre <- indata[,grep('Cur.*\\n(Tariff)',colnames(indata), perl = TRUE), drop= TRUE]
         tariffPre[is.na(tariffPre)] <- 0
         indata$mcDelta <-  (tariffPost - tariffPre)
         indata$mcDelta <-  indata$mcDelta/(1 - tariffPost)
      }


      indata$Owner <- factor(indata$Owner,levels=unique(indata$Owner) )




      thisSim <- msgCatcher(
                          runSims(supply = input$supply,demand = input$demand,
                                  indata = indata, mktElast = input$enterElast,
                                  type = input$menu)
      )



     thisSim$warning <- grep("are the same|INCREASE in marginal costs", thisSim$warning, value= TRUE, invert=TRUE, perl=TRUE)
     if(length(thisSim$warning) == 0){thisSim$warning = NULL}

     if(input$menu == "Tariffs"){
      values[["sim"]]<-  thisSim$value
      values[["msg"]]<-  list(error=thisSim$error,warning=thisSim$warning)
     }
     else if(input$menu == "Quotas"){
       valuesQuota[["sim"]]<-  thisSim$value
       valuesQuota[["msg"]]<-  list(error=thisSim$error,warning=thisSim$warning)
     }

     if(!is.null(thisSim$error) || !is.null(thisSim$warning)){

       if(input$menu == "Tariffs" ){
       updateTabsetPanel(session,inputId  = "inTabset", selected = "msgpanel")
       }

       else  if(input$menu == "Quotas" ){
         updateTabsetPanel(session,inputId  = "inTabsetQuota", selected = "msgpanelQuota")
       }

       }

    })


   ## identify whether model is over-identified
   output$overIDText <-   renderText({

     if(is.null(values[["inputData"]])){return()}

     isOverID(input$supply, input$calcElast, values[["InputData"]])
   })

   output$overIDTextQuota <-  renderText({

     if(is.null(valuesQuota[["inputData"]])){return()}

     isOverID(input$supplyQuota, input$calcElastQuota, valuesQuota[["InputData"]])
   })


   ## identify interface as Public Site or not
   output$urlTextQuota <-   output$urlText <-   renderText({

     thisurl <- session$clientData$url_hostname


     if(grepl("atrnet\\.gov", thisurl, perl=TRUE)){
       res <- paste(h4(span("Internal Server",style="color:blue")))
     }
     else if(grepl("^127", thisurl, perl=TRUE)){
       res <- paste(h4(span("Local Server",style="color:green")))
     }
     else{
       res <- paste(h4(span("Public Server",style="color:red")))
     }
    res

   })

    ## display summary results from gensum to results tab
    output$results <-

          renderTable({

            if(input$inTabset != "respanel" || input$simulate == 0|| is.null(values[["sim"]])){return()}

            isolate(inputData <- values[["inputData"]])

            gensum(values[["sim"]],inputData,type = input$menu)
          })

    output$resultsQuota <-

      renderTable({

        if(input$inTabsetQuota != "respanelQuota" || input$simulateQuota == 0|| is.null(valuesQuota[["sim"]])){return()}

        isolate(inputData <- valuesQuota[["inputData"]])

        gensum(valuesQuota[["sim"]],inputData,type = input$menu)
      })


  ## display summary values to details tab
    output$results_detailed <- renderTable({

      if(input$inTabset!= "detpanel" || input$simulate == 0  || is.null(values[["sim"]])){return()}

        if(input$supply == "Cournot"){

          res <- NULL
          capture.output(try(res <- summary(values[["sim"]], revenue= FALSE,market=FALSE),silent=TRUE))

          res$isParty <- factor(res$mcDelta >0, labels=c("","*"))

          res$product <- res$mcDelta <- NULL

          try(colnames(res) <- c("Foreign Firm","Name","Current Tariff Price","New Tariff Price", "Price Change (%)","Current Tariff Quantity","New Tariff Quantity", "Output Change (%)"),silent=TRUE)

          }

        else{

          isAuction <- grepl("Auction",class(values[["sim"]]))
          isRevDemand <- grepl("ces|aids",class(values[["sim"]]),ignore.case = TRUE)
          inLevels <- FALSE
          #isAIDS <- grepl("aids",class(values[["sim"]]),ignore.case = TRUE)
          missPrice <- any(is.na(values[["sim"]]@prices))
          if(isAuction && missPrice){inLevels = TRUE}

          capture.output(res <- summary(values[["sim"]], revenue=isRevDemand & missPrice, insideOnly=TRUE, levels=inLevels))
          res$Name <- rownames(res)
          res$isParty <- factor(res$mcDelta >0, labels=c("","*"))
          res$mcDelta <- NULL
          res <- res[,c(1, ncol(res), 2 : (ncol(res)  - 1))]

          thesenames <-  c("Foreign Firm","Name","Current Tariff Price","New Tariff Price", "Price Change (%)","Current Tariff Share (%)","New Tariff Share (%)", "Share Change (%)")


          colnames(res) <- thesenames




          if(inLevels){ colnames(res)[ colnames(res) == "Price Change (%)"] = "Price Change ($/unit)"}

          }


        res


      }, digits = 2)


    ## display summary values to details tab
    output$results_detailedQuota <- renderTable({

      if(input$inTabsetQuota != "detpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

      if(input$supplyQuota == "Cournot"){

        res <- NULL
        capture.output(try(res <- summary(valuesQuota[["sim"]], revenue= FALSE,market=FALSE),silent=TRUE))


        res$isParty <- factor(is.finite(valuesQuota[["sim"]]@capacitiesPre) | is.finite(valuesQuota[["sim"]]@capacitiesPost), labels=c("","*"))


        res$product <- res$mcDelta <- NULL

        try(colnames(res) <- c("Foreign Firm","Name","Current Quota Price","New Quota Price", "Price Change (%)","Current Quota Quantity","New Quota Quantity", "Output Change (%)"),silent=TRUE)

      }

      else{

        isAuction <- grepl("Auction",class(valuesQuota[["sim"]]))
        isRevDemand <- grepl("ces|aids",class(valuesQuota[["sim"]]),ignore.case = TRUE)
        inLevels <- FALSE
        #isAIDS <- grepl("aids",class(valuesQuota[["sim"]]),ignore.case = TRUE)
        missPrice <- any(is.na(valuesQuota[["sim"]]@prices))
        if(isAuction && missPrice){inLevels = TRUE}

        capture.output(res <- summary(valuesQuota[["sim"]], revenue=isRevDemand & missPrice, insideOnly=TRUE, levels=inLevels))
        res$Name <- rownames(res)

        res$isParty <- factor(is.finite(valuesQuota[["sim"]]@capacitiesPre) | is.finite(valuesQuota[["sim"]]@capacitiesPost), labels=c("","*"))


        res$mcDelta <- NULL
        res <- res[,c(1, ncol(res), 2 : (ncol(res)  - 1))]

        thesenames <-  c("Foreign Firm","Name","Current Quota Price","New Quota Price", "Price Change (%)","Current Quota Share (%)","New Quota Share (%)", "Share Change (%)")


        colnames(res) <- thesenames




        if(inLevels){ colnames(res)[ colnames(res) == "Price Change (%)"] = "Price Change ($/unit)"}

      }


      res


    }, digits = 2)




    output$results_shareOut <- renderTable({

      if(input$inTabset!= "detpanel" || input$simulate == 0  || is.null(values[["sim"]])){return()}

      genShareOut(values[["sim"]])

      }, rownames = TRUE, digits=1,align="c")


    output$results_shareOutQuota <- renderTable({

      if(input$inTabsetQuota != "detpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

      genShareOut(valuesQuota[["sim"]])

    }, rownames = TRUE, digits=1,align="c")


    ## display results to diagnostics tab
    output$results_diagnostics <- renderTable({

      if(input$inTabset!= "diagpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

      res <- gendiag(values[["sim"]])

      res

    }, digits = 0 ,rownames = TRUE,align="c")

    output$results_diagnosticsQuota <- renderTable({

      if(input$inTabsetQuota != "diagpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

      res <- gendiag(valuesQuota[["sim"]])

      res

    }, digits = 0 ,rownames = TRUE,align="c")


    ## display market elasticity gap to diagnostics tab
    output$results_diag_elast <- renderTable({

      if(input$inTabset!= "diagpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

      res <- gendiag(values[["sim"]], mktElast=TRUE)

      res

    }, digits =2,rownames = FALSE,align="c")

    output$results_diag_elastQuota <- renderTable({

      if(input$inTabsetQuota!= "diagpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

      res <- gendiag(valuesQuota[["sim"]], mktElast=TRUE)

      res

    }, digits =2,rownames = FALSE,align="c")




    ## display parameters to diagnostics tab
    output$parameters <- renderPrint({

      if(input$inTabset!= "diagpanel" || input$simulate == 0  || is.null(values[["sim"]])){return()}

      print(getParms(values[["sim"]],digits=2))


    })

    output$parametersQuota <- renderPrint({

      if(input$inTabsetQuota!= "diagpanelQuota" || input$simulateQuota == 0  || is.null(valuesQuota[["sim"]])){return()}

      print(getParms(valuesQuota[["sim"]],digits=2))


    })

    ## display elasticities to elasticity tab
    output$results_elast <- renderTable({

      if(input$inTabset!= "elastpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

      isCournot <- grepl("Cournot",class(values[["sim"]]))

       if(input$pre_elast == "Current Tariff"){ preMerger = TRUE}
        else{preMerger =FALSE}

      if(!isCournot && input$diversions){
        res <- diversion(values[["sim"]], preMerger=preMerger)
      }
      else{  res <- elast(values[["sim"]], preMerger=preMerger)}
        if(isCournot){colnames(res) <- "Elasticity"}

        res


    }, rownames = TRUE)

    output$results_elastQuota <- renderTable({

      if(input$inTabsetQuota != "elastpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

      isCournot <- grepl("Cournot",class(valuesQuota[["sim"]]))

      if(input$pre_elastQuota == "Current Quota"){ preMerger = TRUE}
      else{preMerger =FALSE}

      if(!isCournot && input$diversionsQuota){
        res <- diversion(valuesQuota[["sim"]], preMerger=preMerger)
      }
      else{  res <- elast(valuesQuota[["sim"]], preMerger=preMerger)}
      if(isCournot){colnames(res) <- "Elasticity"}

      res


    }, rownames = TRUE)


    output$results_mktelast <- renderTable({

      if(input$inTabset!= "elastpanel" || input$simulate == 0 || is.null(values[["sim"]])){return()}

      if(input$pre_elast == "Current Tariff"){ preTariff = TRUE}
      else{preTariff =FALSE}

      res <- as.matrix(elast(values[["sim"]], preMerger=preTariff, market = TRUE))
      colnames(res)= "Market"
      res

    }, rownames = FALSE)

    output$results_mktelastQuota <- renderTable({

      if(input$inTabsetQuota!= "elastpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["sim"]])){return()}

      if(input$pre_elast == "Current Quota"){ preQuota = TRUE}
      else{preQuota =FALSE}

      res <- as.matrix(elast(valuesQuota[["sim"]], preMerger=preQuota, market = TRUE))
      colnames(res)= "Market"
      res

    }, rownames = FALSE)


    ## display R code to code tab
    output$results_code <- renderPrint({

      if(input$inTabset!= "codepanel"){return()}


      thiscode <- gencode("Tariffs")

      cat(thiscode)

    })


    output$results_codeQuota <- renderPrint({

      if( input$inTabsetQuota!= "codepanelQuota"){return()}


      thiscode <- gencode("Quotas")

      cat(thiscode)

    })


    ## display messages to message tab
   output$warnings <- renderPrint({

      if(input$inTabset!= "msgpanel" || input$simulate == 0 || is.null(values[["msg"]])){return()}

      print(values[["msg"]]$warning)


   })

   output$warningsQuota <- renderPrint({

     if(input$inTabsetQuota!= "msgpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["msg"]])){return()}

     print(valuesQuota[["msg"]]$warning)


   })

   output$errors <- renderPrint({

     if(input$inTabset!= "msgpanel" || input$simulate == 0 || is.null(values[["msg"]]$error)){cat(return())}

     print(values[["msg"]]$error)


   })

   output$errorsQuota <- renderPrint({

     if(input$inTabsetQuota!= "msgpanelQuota" || input$simulateQuota == 0 || is.null(valuesQuota[["msg"]]$error)){cat(return())}

     print(valuesQuota[["msg"]]$error)


   })

   output$reference <- renderText({
   includeHTML(system.file('doc','Reference.html', package='trade'))
})

  })

