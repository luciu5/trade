#'@title S4 classes to model tariffs
#'@description Extend classes from the \code{\link{antitrust}} package to accomodate tariffs.
#'@slot tariffPre a numeric vector containing  \strong{current} product-level (or in the case of Cournot, plant-level) tariffs.
#'Default is a vector of 0s.
#'@slot tariffPost a numeric vector containing \strong{new} product-level (or in the case of Cournot, plant-level) tariffs.
#'Default is a vector of 0s.
#'@name Tariff-classes
#' @export
NULL

#'@rdname Tariff-classes
setClass("TariffCournot", contains = "Cournot",
         representation=representation(
                        tariffPre       = "numeric",
                        tariffPost       = "numeric"))

#'@rdname Tariff-classes
setClass("TariffLogit", contains = "LogitALM",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"))

#'@rdname Tariff-classes
setClass("TariffCES", contains = "CESALM",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"))

#'@rdname Tariff-classes
setClass("TariffAIDS", contains = "AIDS",
         representation=representation(
           tariffPre       = "numeric",
           tariffPost       = "numeric"))

#'@rdname Tariff-classes
setClassUnion("TariffBertrand", c("TariffLogit", "TariffCES", "TariffAIDS"))


