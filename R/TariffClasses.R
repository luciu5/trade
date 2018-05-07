#'@title S4 classes to model tariffs
#'@description Extend classes from the \code{\link{antitrust}} package to accomodate tariffs.
#'@slot tariffPre For TariffCournot, a matrix containing  \strong{current} plant-level (rows) AND product-level (columns) tariffs.
#'Default is a matrix of 0s.  For all other classes,  a vector containg \strong{current} product-level  tariffs.
#'\emph{ad valorem} taxes are expressed as a proportion of the consumer price.
#'Default is a vector of 0s.
#'@slot tariffPost a For TariffCournot, a matrix containing  \strong{new} plant-level (rows) AND product-level (columns) tariffs.
#'Default is a matrix of 0s.  For all other classes,   a vector containing \strong{new} product-level  tariffs.
#'\emph{ad valorem} taxes are expressed as a proportion of the consumer price.
#'Default is a vector of 0s.
#'@name Tariff-classes
#' @export
NULL

#'@rdname Tariff-classes
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
setClassUnion("TariffBertrand", c("TariffLogit", "TariffCES", "TariffAIDS"))


