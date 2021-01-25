#'@title S4 classes to model tariffs
#'@import antitrust
#'@import methods
#'@import stats
#'@description Extend classes from the \pkg{antitrust} package to accomodate tariffs.
#'@slot tariffPre For TariffCournot, a matrix containing  \strong{current} plant-level (rows) AND product-level (columns) tariffs.
#'Default is a matrix of 0s.  For all other classes,  a vector containg \strong{current} product-level  tariffs.
#'\emph{ad valorem} taxes are expressed as a proportion of the consumer price.
#'Default is a vector of 0s.
#'@slot tariffPost a For TariffCournot, a matrix containing  \strong{new} plant-level (rows) AND product-level (columns) tariffs.
#'Default is a matrix of 0s.  For all other classes,   a vector containing \strong{new} product-level  tariffs.
#'\emph{ad valorem} taxes are expressed as a proportion of the consumer price.
#'Default is a vector of 0s.
#'@name Tariff-classes
NULL

#'@rdname Tariff-classes
#' @export
setClass("TariffCournot", contains = "Cournot",
         representation=representation(
                        tariffPre       = "matrix",
                        tariffPost       = "matrix"),
         validity = function(object){


           if(!isTRUE(all.equal(dim(object@tariffPre), dim(object@quantities))) ||
              !isTRUE(all.equal(dim(object@tariffPost), dim(object@quantities)))
           ){
           stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'quantities'")
           }
           if(isTRUE(all.equal(object@tariffPre,object@tariffPost))){
             stop("'tariffPre' and 'tariffPost' are equal")
           }

           if(any(is.na(object@tariffPre)) ||
              any(is.na(object@tariffPost)) ){
             stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
           }
         })


#'@rdname Tariff-classes
#' @export
setClass("Tariff2ndLogit", contains = "Auction2ndLogitALM",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         validity = function(object){

           if(isTRUE(all.equal(object@tariffPre,object@tariffPost))){
             stop("'tariffPre' and 'tariffPost' are equal")
           }

           if(!isTRUE(all.equal(length(object@tariffPre), length(object@shares))) ||
              !isTRUE(all.equal(length(object@tariffPost), length(object@shares)))
           ){
             stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'quantities'")
           }

           if(any(is.na(object@tariffPre)) ||
              any(is.na(object@tariffPost)) ){
             stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
           }

           if(any(object@prices<0 | is.na(object@prices),na.rm=TRUE)){
             stop("'prices' values must be non-missing and positive")}

         })

#'@rdname Tariff-classes
#' @export
setClass("TariffBargainingLogit", contains = "BargainingLogit",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         validity = function(object){

           if(isTRUE(all.equal(object@tariffPre,object@tariffPost))){
             stop("'tariffPre' and 'tariffPost' are equal")
           }

           if(!isTRUE(all.equal(length(object@tariffPre), length(object@shares))) ||
              !isTRUE(all.equal(length(object@tariffPost), length(object@shares)))
           ){
             stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'quantities'")
           }

           if(any(is.na(object@tariffPre)) ||
              any(is.na(object@tariffPost)) ){
             stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
           }

         })

#'@rdname Tariff-classes
#' @export
setClass("TariffLogit", contains = "LogitALM",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         validity = function(object){

           if(isTRUE(all.equal(object@tariffPre,object@tariffPost))){
             stop("'tariffPre' and 'tariffPost' are equal")
           }

           if(!isTRUE(all.equal(length(object@tariffPre), length(object@shares))) ||
              !isTRUE(all.equal(length(object@tariffPost), length(object@shares)))
           ){
             stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'quantities'")
           }

           if(any(is.na(object@tariffPre)) ||
              any(is.na(object@tariffPost)) ){
             stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
           }
         })

#'@rdname Tariff-classes
#' @export
setClass("TariffMonComLogit", contains = "Logit",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         prototype=list(
         control.slopes=list(reltol=.Machine$double.eps^0.25)
           ),
         validity = function(object){

           if(isTRUE(all.equal(object@tariffPre,object@tariffPost))){
             stop("'tariffPre' and 'tariffPost' are equal")
           }

           if(!isTRUE(all.equal(length(object@tariffPre), length(object@shares))) ||
              !isTRUE(all.equal(length(object@tariffPost), length(object@shares)))
           ){
             stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'quantities'")
           }

           if(any(is.na(object@tariffPre)) ||
              any(is.na(object@tariffPost)) ){
             stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
           }
         })

setClass("TariffMonComCES", contains = "CES",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         prototype=list(
           control.slopes=list(reltol=.Machine$double.eps^0.25)
         ),
         validity = function(object){

           if(isTRUE(all.equal(object@tariffPre,object@tariffPost))){
             stop("'tariffPre' and 'tariffPost' are equal")
           }

           if(!isTRUE(all.equal(length(object@tariffPre), length(object@shares))) ||
              !isTRUE(all.equal(length(object@tariffPost), length(object@shares)))
           ){
             stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'revenues'")
           }

           if(any(is.na(object@tariffPre)) ||
              any(is.na(object@tariffPost)) ){
             stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
           }
         })

#'@rdname Tariff-classes
#' @export
setClass("TariffCES", contains = "CESALM",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         validity = function(object){

            if(isTRUE(all.equal(object@tariffPre,object@tariffPost))){
             stop("'tariffPre' and 'tariffPost' are equal")
           }

           if(!isTRUE(all.equal(length(object@tariffPre), length(object@shares))) ||
              !isTRUE(all.equal(length(object@tariffPost), length(object@shares)))
           ){
             stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'quantities'")
           }
           if(any(is.na(object@tariffPre)) ||
              any(is.na(object@tariffPost)) ){
             stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
           }
         })

#'@rdname Tariff-classes
#' @export
setClass("TariffAIDS", contains = "AIDS",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         validity = function(object){

           if(isTRUE(all.equal(object@tariffPre,object@tariffPost))){
             stop("'tariffPre' and 'tariffPost' are equal")
           }

           if(!isTRUE(all.equal(length(object@tariffPre), length(object@shares))) ||
              !isTRUE(all.equal(length(object@tariffPost), length(object@shares)))
           ){
             stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'quantities'")
           }
           if(any(is.na(object@tariffPre)) ||
              any(is.na(object@tariffPost)) ){
             stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
           }
         })

#'@rdname Tariff-classes
#' @export
setClassUnion("TariffBertrand", c("TariffLogit", "TariffCES", "TariffAIDS","TariffMonComLogit","TariffMonComCES","Tariff2ndLogit","TariffBargainingLogit"))


