% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ps-methods.R
\name{ps-methods}
\alias{ps-methods}
\alias{calcProducerSurplus,TariffBertrand-method}
\alias{calcProducerSurplus,TariffCournot-method}
\title{Methods To Calculate Producer Surplus}
\usage{
\S4method{calcProducerSurplus}{TariffBertrand}(object, preMerger = TRUE)

\S4method{calcProducerSurplus}{TariffCournot}(object, preMerger = TRUE)
}
\arguments{
\item{object}{an instance of class \code{TariffBertrand} or \code{TariffCournot}}

\item{preMerger}{when TRUE, calculates producer surplus under the existing tariff regime. When FALSE, calculates
tariffs under the new tariff regime. Default is TRUE.}
}
\value{
product-level (or in the case of Cournot, plant-level) producer surplus
}
\description{
Producer Surplus methods for the \code{TariffBertrand} and \code{TariffCournot} classes
}
