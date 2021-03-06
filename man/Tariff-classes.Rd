% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TariffClasses.R
\docType{class}
\name{Tariff-classes}
\alias{Tariff-classes}
\alias{TariffCournot-class}
\alias{Tariff2ndLogit-class}
\alias{TariffBargainingLogit-class}
\alias{TariffLogit-class}
\alias{TariffMonComLogit-class}
\alias{TariffCES-class}
\alias{TariffAIDS-class}
\alias{TariffBertrand-class}
\title{S4 classes to model tariffs}
\description{
Extend classes from the \pkg{antitrust} package to accomodate tariffs.
}
\section{Slots}{

\describe{
\item{\code{tariffPre}}{For TariffCournot, a matrix containing  \strong{current} plant-level (rows) AND product-level (columns) tariffs.
Default is a matrix of 0s.  For all other classes,  a vector containg \strong{current} product-level  tariffs.
\emph{ad valorem} taxes are expressed as a proportion of the consumer price.
Default is a vector of 0s.}

\item{\code{tariffPost}}{a For TariffCournot, a matrix containing  \strong{new} plant-level (rows) AND product-level (columns) tariffs.
Default is a matrix of 0s.  For all other classes,   a vector containing \strong{new} product-level  tariffs.
\emph{ad valorem} taxes are expressed as a proportion of the consumer price.
Default is a vector of 0s.}
}}

