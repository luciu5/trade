#'@title Initialize Methods
#'@description Initialize methods for the \code{TariffBertrand} and \code{TariffCournot} classes
#' @param .Object an instance of class \code{TariffBertrand} or \code{TariffCournot}
#' @param ... arguments to pass to initialize
#' @name initialize-methods
#'@include QuotaClasses.R
NULL
#' @rdname initialize-methods
#' @export
setMethod("initialize", "TariffBertrand", function(.Object, ...) {

  trapWarning <- function(expr){
    withCallingHandlers({expr}
                        , warning = function(w) {
                          if (grepl( "'ownerPost' and 'ownerPre' are the same|positive values of 'mcDelta' imply an INCREASE in marginal costs",conditionMessage(w),perl= TRUE)){
                            invokeRestart("muffleWarning")}


                        })
  }

  trapWarning(.Object <- callNextMethod())
  trapWarning(validObject(.Object))


  return(.Object)
})
#' @rdname initialize-methods
#' @export
setMethod("initialize", "QuotaBertrand", function(.Object, ...) {

  trapWarning <- function(expr){
    withCallingHandlers({expr}
                        , warning = function(w) {
                          if (grepl( "'ownerPost' and 'ownerPre' are the same|positive values of 'mcDelta' imply an INCREASE in marginal costs",conditionMessage(w),perl= TRUE)){
                            invokeRestart("muffleWarning")}


                        })
  }

  trapWarning(.Object <- callNextMethod())
  trapWarning(validObject(.Object))


  return(.Object)
})
#' @rdname initialize-methods
#' @export
setMethod("initialize", "TariffCournot", function(.Object, ...) {

  trapWarning <- function(expr){
    withCallingHandlers({expr}
                        , warning = function(w) {
                          if (grepl( "'ownerPost' and 'ownerPre' are the same|positive values of 'mcDelta' imply an INCREASE in marginal costs",conditionMessage(w), perl=TRUE)){
                            invokeRestart("muffleWarning")}


                        })
  }

  trapWarning(.Object <- callNextMethod())
  trapWarning(validObject(.Object))


  return(.Object)
})
