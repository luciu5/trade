#'@title Initialize Methods
#'@description Initialize methods for the \code{TariffBertrand} and \code{TariffCournot} classes
#' @name initialize-methods
#'@include TariffClasses.R
#' @export
NULL
#' @rdname initialize-methods
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
