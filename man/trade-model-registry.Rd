\name{trade-model-registry}
\alias{trade-model-registry}
\alias{model_spec}
\alias{supportedModels}
\title{Trade model specifications and supported-model registry}
\description{
  Normalize trade demand, conduct, variant, and policy names and inspect the
  complete economic model combinations implemented by \pkg{trade}.
}
\usage{
model_spec(demand, conduct, variant = "standard", policy = "tariff")

supportedModels()
}
\arguments{
  \item{demand}{A demand-system name, such as \code{"logit"} or
    \code{"ces"}.}
  \item{conduct}{A conduct name, such as \code{"bertrand"} or
    \code{"cournot"}.}
  \item{variant}{An implemented model variant.}
  \item{policy}{The policy family represented by the model, currently
    \code{"tariff"} or \code{"quota"}.}
}
\details{
  The registry describes complete implemented economic models. It does not
  mechanically combine arbitrary demand and conduct modules.
}
\value{
  \code{model_spec} returns a small normalized specification. A specification
  contains \code{demand}, \code{conduct}, \code{variant}, \code{policy}, and
  \code{id}. \code{supportedModels} returns a data frame with one row per
  registered model and its capabilities.
}
