#' @title Tariff Simulation With User-Supplied Demand Parameters
#' @name Sim-Functions
#' @aliases sim
#' @description Simulates the price effects of an ad valorem tariff
#' with user-supplied demand parameters under the
#' assumption that all firms in the market are playing either a
#' differentiated products Bertrand pricing game, 2nd price auction, or bargaining game.
#' @description Let k denote the number of products produced by all firms below.
#' @param prices A length k vector of product prices.
#' @param supply A character string indicating how firms compete with one another. Valid
#' values are "moncom" (monopolistic competition), "bertrand" (Nash Bertrand),  "auction2nd"
#' (2nd score auction), or "bargaining".
#' @param demand A character string indicating the type of demand system
#'   to be used in the merger simulation. Supported demand systems are
#'   logit (\sQuote{Logit}) or ces (\sQuote{CES}).
#' @param demand.param  See Below.
#' @param owner EITHER a vector of length k whose values indicate which firm produced a product before the tariff
#' OR a k x k matrix of pre-merger ownership shares.
#' @param tariffPre  A vector of length k where each element equals the \strong{current} \emph{ad valorem} tariff
#' (expressed as a proportion of the consumer price) imposed on each product. Default is 0, which assumes no tariff.
#' @param tariffPost  A vector of length k where each element equals the \strong{new}  \emph{ad valorem} tariff
#' (expressed as a proportion of the consumer price) imposed on each product. Default is 0, which assumes no tariff.
#' @param bargpowerPre A length k vector of pre-merger bargaining power parameters. Values
#' must be between 0 (sellers have the power) and 1 (buyers the power). Ignored if \sQuote{supply} not equal
#' to "bargaining".
#' @param bargpowerPost A length k vector of post-merger bargaining power parameters. Values
#' must be between 0 (sellers have the power) and 1 (buyers the power). Default is \sQuote{bargpowerPre}.
#' Ignored if \sQuote{supply} not equal to "bargaining".
#' @param subset A vector of length k where each element equals TRUE if
#'   the product indexed by that element should be included in the
#'   post-merger simulation and FALSE if it should be excluded.Default is a
#'   length k vector of TRUE.
#' @param insideSize A length 1 vector equal to total units sold if \sQuote{demand} equals "logit", or total revenues if
#' \sQuote{demand} equals "ces".
#' @param priceStart A length k vector of starting values us
#' @param priceOutside A length 1 vector indicating the price of the
#'   outside good. This option only applies to the \sQuote{Logit} class and its child classes
#'   Default for \sQuote{Logit},\sQuote{LogitNests}, and \sQuote{LogitCap} is 0,
#'   and for \sQuote{CES} and \sQuote{CesNests} is 1.
#' @param priceStart A length k vector of starting values used to solve for
#'   equilibrium price. Default is the \sQuote{prices} vector for all values of
#'   demand except for \sQuote{AIDS}, which is set equal to a vector of 0s.
#' @param labels A k-length vector of labels. Default is \dQuote{Prod#}, where
#'   \sQuote{#} is a number between 1 and the length of \sQuote{prices}.
#' @param ... Additional options to feed to the
#'       optimizer used to solve for equilibrium prices.
#'
#' @details Using user-supplied demand parameters,
#' \code{sim} simulates the effects of a merger in a market where
#' firms are playing a differentiated products pricing game.
#'

#' If \sQuote{demand} equals \sQuote{Logit}  then
#' \sQuote{demand.param} must equal a list containing
#' \itemize{
#'   \item{alpha}{The price coefficient.}
#'   \item{meanval}{A length-k vector of mean valuations \sQuote{meanval}. If
#'     none of the values of \sQuote{meanval} are zero, an outside good is assumed
#'     to exist.}
#' }
#' If demand equals \sQuote{CES}  then
#' \sQuote{demand.param} must equal a list containing
#'
#' \itemize{
#'   \item{gamma}{ The price coefficient,}
#'   \item{alpha}{The coefficient on the numeraire good. May instead be
#'     calibrated using \sQuote{shareInside},}
#'   \item{meanval}{A length-k vector of mean valuations \sQuote{meanval}. If
#'     none of the values of \sQuote{meanval} are zero, an outside good is assumed
#'     to exist,}
#'   \item{shareInside}{ The budget share of all products in the
#'     market. Default is 1, meaning that all consumer wealth is spent on
#'     products in the market. May instead be specified using \sQuote{alpha}.}
#'
#' }
#'
#' @return \code{sim} returns an instance of the class specified by the
#' \sQuote{demand} argument.
#' @seealso The S4 class documentation for:  \code{\linkS4class{Logit}} and
#' \code{\linkS4class{CES}},
#' @author Charles Taragin \email{ctaragin@ftc.gov}
#'
#' @examples ## Calibration and simulation results from a merger between Budweiser and
#' ## Old Style. Note that the in the following model there is no outside
#' ## good; BUD's mean value has been normalized to zero.
#'
#' ## Source: Epstein/Rubenfeld 2004, pg 80
#'
#'
#' prodNames <- c("BUD","OLD STYLE","MILLER","MILLER-LITE","OTHER-LITE","OTHER-REG")
#' owner <-c("BUD","OLD STYLE","MILLER","MILLER","OTHER-LITE","OTHER-REG")
#' tariff <- c(0,0,0,0,.1,.1)
#'
#' price    <- c(.0441,.0328,.0409,.0396,.0387,.0497)
#'
#' # a list containing price coefficient and mean valuations
#' demand.param=list(alpha=-48.0457,
#'                   meanval=c(0,0.4149233,1.1899885,0.8252482,0.1460183,1.4865730)
#' )
#'
#' sim.logit <- sim(price,demand="logit",supply="bertrand", demand.param,
#'                  owner=owner,tariffPost=tariff,labels=prodNames)
#'
#'
#'
#' print(sim.logit)           # return predicted price change
#' summary(sim.logit)         # summarize merger simulation
#'
#' elast(sim.logit,TRUE)      # returns premerger elasticities
#' elast(sim.logit,FALSE)     # returns postmerger elasticities
#'
#' diversion(sim.logit,TRUE)  # return premerger diversion ratios
#' diversion(sim.logit,FALSE) # return postmerger diversion ratios
#'
#'
#' cmcr(sim.logit)            #calculate compensating marginal cost reduction
#' upp(sim.logit)             #calculate Upwards Pricing Pressure Index
#'
#' CV(sim.logit)              #calculate representative agent compensating variation
#'
#' @include bargaining_tariff.R
NULL


#'@rdname Sim-Functions
#'@export
sim <- function(prices,
                supply=c("moncom","bertrand","auction","bargaining"),
                demand=c("logit","ces"),
                demand.param,
                owner,
                tariffPre=rep(0,length(prices)),
                tariffPost,
                subset=rep(TRUE,length(prices)),
                insideSize=1,
                priceOutside,
                priceStart,
                bargpowerPre=rep(0.5,length(prices)),
                bargpowerPost=bargpowerPre,
                labels=paste("Prod",1:length(prices),sep=""),...){

  supply <- match.arg(supply)
  demand <- match.arg(demand)
  nprods <- length(prices)

  mcDelta= (tariffPost - tariffPre)/(1 - tariffPost)


  if(is.null(owner)){

    warning("'owner' is NULL. Assuming each product is owned by a single firm.")
    ownerPre <-  diag(nprods)

  }


  else if(!is.matrix(owner)){

    owner <- factor(owner, levels = unique(owner))
    owner = model.matrix(~-1+owner)
    owner = tcrossprod(owner)



  }

  if(missing(priceStart)){priceStart <- prices}


  ## Create placeholders values to fill required Class slots

  shares <- margins <- rep(1/nprods,nprods)


  ## general checks
  if(!is.list(demand.param)){stop("'demand.param' must be a list.")}

  ## Checks for discrete choice models
  if(demand %in% c("ces","logit")){

    if(!("meanval" %in% names(demand.param))){
      stop("'demand.param' does not contain 'meanval'.")
    }
    if(length(demand.param$meanval) != nprods || any(is.na(demand.param$meanval))){
      stop("'meanval' must be a length-k vector of product mean valuations. NAs not allowed.")
    }

    if(demand %in% c("logit")){


      ## An outside option is assumed to exist if all mean valuations are non-zero
      if(all(demand.param$meanval!=0)){
        normIndex <- NA
        #shares <- rep(1/(nprods+1),nprods)
      }
      else{
        normIndex <- which(demand.param$meanval==0)

        if(length(normIndex)>1){
          warning("multiple values of meanval are equal to zero. Normalizing with respect to the first product with zero mean value.")
          normIndex <- normIndex[1]
        }

      }

      if(!("alpha" %in% names(demand.param))   ||
         length(demand.param$alpha) != 1     ||
         isTRUE(demand.param$alpha>0)){
        stop("'demand.param' does not contain 'alpha' or 'alpha' is not a negative number.")
      }

      if(missing(priceOutside)){priceOutside <- 0}

    }


    else  if(demand %in% c("ces")){
      if(!("gamma" %in% names(demand.param))   ||
         length(demand.param$gamma) != 1     ||
         isTRUE(demand.param$gamma<0)){
        stop("'demand.param' does not contain 'gamma' or 'gamma' is not a positive number.")
      }


      ## uncover Numeraire Coefficients
      if((!("alpha" %in% names(demand.param) ) || is.null(demand.param$alpha)) &&
         !("shareInside" %in% names(demand.param))){
        warning("'demand.param' does not contain either 'alpha' or 'shareInside'. Setting alpha=NULL.")

        demand.param$alpha=NULL
      }

      else if("shareInside" %in% names(demand.param)){
        shareInside=demand.param$shareInside
        demand.param$shareInside <- NULL

        if(shareInside<1) {demand.param$alpha <- 1/shareInside -1}
        else{ demand.param$alpha <- NULL}


      }


      ## An outside option is assumed to exist if all mean valuations are non-zero
      if(all(demand.param$meanval!=1)){
        normIndex <- NA
        #shares <- rep(1/(nprods+1),nprods)
      }
      else{
        normIndex <- which(demand.param$meanval==1)

        if(length(normIndex)>1){
          warning("multiple values of meanval are equal to one. Normalizing with respect to the first product with  mean value equal to 1.")
          normIndex <- normIndex[1]
        }


      }


      if(missing(priceOutside)){priceOutside <- 1}
    }


  }






  ## Create constructors for each demand system specified in the 'demand' parameter


  result <-
    switch(demand,

         "logit" =switch(supply,
                         "bertrand" =
                           new("TariffLogit",prices=prices, shares=shares,
                               margins=margins,
                               ownerPre=owner*(1-tariffPre),
                               ownerPost=owner*(1-tariffPost),
                               mcDelta=mcDelta,
                               subset=subset,
                               priceOutside=priceOutside,
                               priceStart=priceStart,
                               shareInside= 1,
                               tariffPre=tariffPre,
                               tariffPost=tariffPost,
                               insideSize = insideSize,
                               normIndex=normIndex,
                               parmsStart=c(-2,0.1),
                               labels=labels),
                         "auction2nd" =
                           new("Tariff2ndLogit",prices=prices, shares=shares,
                               margins=margins,
                               ownerPre=owner,
                               ownerPost=owner,
                               mcDelta=mcDelta,
                               subset=subset,
                               priceOutside=priceOutside,
                               priceStart=priceStart,
                               shareInside= 1,
                               tariffPre=tariffPre,
                               tariffPost=tariffPost,
                               normIndex=normIndex,
                               insideSize = insideSize,
                               parmsStart=c(-2,0.1),
                               labels=labels),
                         "bargaining" =
                           new("TariffBargainingLogit",
                               prices=prices, shares=shares,
                               margins=margins,
                               ownerPre=owner*(1-tariffPre),
                               ownerPost=owner*(1-tariffPost),
                               mcDelta=mcDelta,
                               subset=subset,
                               priceOutside=priceOutside,
                               priceStart=priceStart,
                               shareInside= 1,
                               bargpowerPre=bargpowerPre,
                               bargpowerPost=bargpowerPost,
                               normIndex=normIndex,
                               tariffPre=tariffPre,
                               tariffPost=tariffPost,
                               insideSize = insideSize,
                               labels=labels),
                         "moncom" =
                           new("TariffMonComLogit",
                               prices=prices, shares=shares,
                               margins=margins,
                               ownerPre=owner,
                               ownerPost=owner,
                               mcDelta=mcDelta,
                               subset=subset,
                               priceOutside=priceOutside,
                               priceStart=priceStart,
                               shareInside= 1,
                               tariffPre=tariffPre,
                               tariffPost=tariffPost,
                               insideSize = insideSize,
                               normIndex=normIndex,
                               labels=labels)
         ),

         "ces" =  switch(supply,
                         "bertrand" =
                           new("TariffCES",prices=prices, shares=shares,
                               margins=margins,
                               ownerPre=owner*(1-tariffPre),
                               ownerPost=owner*(1-tariffPost),
                               mcDelta=mcDelta,
                               subset=subset,
                               priceOutside=priceOutside,
                               priceStart=priceStart,
                               shareInside= 1,
                               tariffPre=tariffPre,
                               tariffPost=tariffPost,
                               insideSize = insideSize,
                               normIndex=normIndex,
                               parmsStart=c(2,0.1),
                               labels=labels),
                         "moncom" =
                           new("TariffMonComCES",prices=prices, shares=shares,
                               margins=margins,
                               ownerPre=owner,
                               ownerPost=owner,
                               mcDelta=mcDelta,
                               subset=subset,
                               priceOutside=priceOutside,
                               priceStart=priceStart,
                               shareInside= 1,
                               tariffPre=tariffPre,
                               tariffPost=tariffPost,
                               insideSize = insideSize,
                               normIndex=normIndex,
                               labels=labels)
         )

  )



  result@slopes=demand.param




  ## Calculate marginal cost
  result@mcPre     <-  calcMC(result,TRUE)
  result@mcPost    <-  calcMC(result,FALSE)



  if(supply=="auction2nd"){result@mcDelta <- result@mcPre*mcDelta}
  else{result@mcDelta <- mcDelta}


  ## Solve Non-Linear System for Price Changes
  result@pricePre  <- calcPrices(result,TRUE,...)
  result@pricePost <- calcPrices(result,FALSE,subset=subset,...)

  if(demand=="logit"){result@mktSize <- insideSize/sum(calcShares(result))}
  else if ( demand =="ces"){result@mktSize <- insideSize*(1+result@slopes$alpha)}


  return(result)
}
