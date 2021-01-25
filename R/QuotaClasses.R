#'@title S4 classes to model quotas
#'@description Extend classes from the \pkg{antitrust} package to accomodate quotas.
#'@slot quotaPre For QuotaCournot, a matrix containing  \strong{current} plant-level (rows) AND product-level (columns) quotas.
#'Default is a matrix of 0s.  For all other classes,  a vector containing \strong{current} product-level  quotas.
#'Quotas are expressed as a proportion of pre-merger output.
#'Default is a vector of Infs.
#'@slot quotaPost a For QuotaCournot, a matrix containing  \strong{new} plant-level (rows) AND product-level (columns) quotas.
#'Default is a matrix of Infs.  For all other classes,   a vector containing \strong{new} product-level  quotas.
#'quotas are expressed as a proportion of pre-merger output.
#'Default is a vector of Infss.
#'@name Quota-classes

NULL

#'@rdname Quota-classes
#' @export
setClass("QuotaCournot", contains = "Cournot",
         representation=representation(
                        quotaPre       = "matrix",
                        quotaPost       = "matrix"),
         validity = function(object){


           if(!isTRUE(all.equal(dim(object@quotaPre), dim(object@quantities))) ||
              !isTRUE(all.equal(dim(object@quotaPost), dim(object@quantities)))
           ){
           stop("'quotaPre' and 'quotaPost' must have the same dimensions as 'quantities'")
           }
           if(isTRUE(all.equal(object@quotaPre,object@quotaPost))){
             stop("'quotaPre' and 'quotaPost' are equal")
           }

           if(any(is.na(object@quotaPre)) ||
              any(is.na(object@quotaPost)) ){
             stop("'quotaPre' and 'quotaPost' elements should be Inf rather than NA")
           }
         })

#'@rdname Quota-classes
#' @export
setClass("QuotaLogit", contains = "LogitCapALM",
         representation=representation(
           quotaPre       = "numeric",
           quotaPost       = "numeric"),
         validity = function(object){

           if(isTRUE(all.equal(object@quotaPre,object@quotaPost))){
             stop("'quotaPre' and 'quotaPost' are equal")
           }

           if(!isTRUE(all.equal(length(object@quotaPre), length(object@shares))) ||
              !isTRUE(all.equal(length(object@quotaPost), length(object@shares)))
           ){
             stop("'quotaPre' and 'quotaPost' must have the same dimensions as 'quantities'")
           }

           if(any(is.na(object@quotaPre)) ||
              any(is.na(object@quotaPost)) ){
             stop("'quotaPre' and 'quotaPost' elements should be Inf rather than NA")
           }
         })



#'@rdname Quota-classes
#' @export
setClassUnion("QuotaBertrand", c("QuotaLogit"))


