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

.validTariffVector <- function(object) {
  if(!isTRUE(all.equal(length(object@tariffPre), length(object@shares))) ||
     !isTRUE(all.equal(length(object@tariffPost), length(object@shares)))
  ){
    stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'quantities'")
  }

  if(any(is.na(object@tariffPre)) ||
     any(is.na(object@tariffPost)) ){
    stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
  }

  TRUE
}

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

           .validTariffVector(object)
         })

#'@rdname Tariff-classes
#' @export
setClass("TariffLogitCournot", contains = "LogitCournot",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         validity = function(object){

           .validTariffVector(object)
         })

#'@rdname Tariff-classes
#' @export
setClass("TariffLogitCournotALM", contains = "LogitCournotALM",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         validity = function(object){

           .validTariffVector(object)
         })

# BLP demand uses the complete antitrust BLP conduct classes.  These light
# tariff wrappers add only the pre/post tariff state; the demand, conduct,
# cost-recovery, and equilibrium methods remain those supplied by antitrust.
# In particular, trade does not create a second BLP implementation.
setClass("TariffLogitBLP", contains = "LogitBLP",
         representation = representation(
           tariffPre = "numeric",
           tariffPost = "numeric"),
         validity = function(object) .validTariffVector(object))

setClass("TariffCournotBLP", contains = "CournotBLP",
         representation = representation(
           tariffPre = "numeric",
           tariffPost = "numeric"),
         validity = function(object) .validTariffVector(object))

setClass("TariffAuction2ndBLP", contains = "Auction2ndBLP",
         representation = representation(
           tariffPre = "numeric",
           tariffPost = "numeric"),
         validity = function(object) .validTariffVector(object))

setClass("TariffBargainingBLP", contains = "BargainingBLP",
         representation = representation(
           tariffPre = "numeric",
           tariffPost = "numeric"),
         validity = function(object) .validTariffVector(object))

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
setClass("TariffMonComCES", contains = "CES",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         prototype=list(
           control.slopes=list(reltol=.Machine$double.eps^0.25)
         ),
         validity = function(object){

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
setClass("TariffBargainingCES", contains = "BargainingCES",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"),
         validity = function(object){

           if(!isTRUE(all.equal(length(object@tariffPre), length(object@shares))) ||
              !isTRUE(all.equal(length(object@tariffPost), length(object@shares)))
           ){
             stop("'tariffPre' and 'tariffPost' must have the same dimensions as 'shares'")
           }

           if(any(is.na(object@tariffPre)) ||
              any(is.na(object@tariffPost)) ){
             stop("'tariffPre' and 'tariffPost' elements should be 0 rather than NA")
           }

         })

#'@rdname Tariff-classes
#' @export
setClassUnion("TariffBertrand", c("TariffLogit", "TariffCES", "TariffAIDS","TariffMonComLogit","TariffMonComCES","Tariff2ndLogit","TariffBargainingLogit","TariffBargainingCES", "TariffLogitBLP", "TariffCournotBLP", "TariffAuction2ndBLP", "TariffBargainingBLP"))

#'@rdname Tariff-classes
#' @export
setClassUnion("TariffLogitCournotModels", c("TariffLogitCournot", "TariffLogitCournotALM", "TariffCournotBLP"))
