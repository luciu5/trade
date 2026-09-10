\name{trade-architecture}
\alias{trade-architecture}
\alias{TradeFit-class}
\alias{TradeCounterfactualPath-class}
\alias{calibrate}
\alias{specify}
\alias{simulate,TradeFit-method}
\alias{simulate,TradeCounterfactualPath-method}
\title{Calibrate, specify, and simulate trade models}
\description{
  The trade architecture separates construction of a baseline structural
  model from policy counterfactual simulation.  Existing trade S4 objects
  remain the economic implementation and are returned by \code{simulate}.
  \code{simulate()} and \code{respecify()} are S4 generics owned by
  \pkg{antitrust}; trade supplies methods for its own \code{TradeFit} and
  \code{TradeCounterfactualPath} classes.
}
\usage{
calibrate(demand, conduct = NULL, variant = "standard",
  policy = "tariff", ...)

specify(demand, conduct = NULL, prices, parameters,
  variant = "standard", policy = "tariff", ...)

\S4method{simulate}{TradeFit}(object, tariffPost, quotaPost, subset,
  priceStart, bargpowerPost, isMax = FALSE, ...)

\S4method{simulate}{TradeCounterfactualPath}(object, tariffPost, quotaPost, subset,
  priceStart, bargpowerPost, isMax = FALSE, ...)
}
\arguments{
  \item{demand}{A demand-system name or a normalized model specification.}
  \item{conduct}{A conduct name when \code{demand} is not a specification.}
  \item{variant}{An implemented model variant, such as \code{"standard"} or
    \code{"alm"}.}
  \item{policy}{The baseline policy family, currently \code{"tariff"} or
    \code{"quota"}.}
  \item{prices}{A vector of observed product prices.}
  \item{parameters}{A named list of supplied structural parameters.}
  \item{object}{A \code{TradeFit} returned by \code{calibrate} or
    \code{specify}, or a \code{TradeCounterfactualPath} to resume from.}
  \item{tariffPost}{A post-policy ad valorem tariff vector.}
  \item{quotaPost}{A post-policy quota vector.}
  \item{subset}{A logical vector selecting products in the post-policy market.}
  \item{priceStart}{Optional post-policy price starting values.}
  \item{bargpowerPost}{Optional post-policy bargaining-power vector for
    bargaining models.}
  \item{isMax}{Whether to request the legacy local-maximum check where
    supported.}
  \item{...}{Baseline inputs, scenario inputs, or solver options accepted by
    the selected legacy implementation.}
}
\details{
  \code{calibrate} delegates observed-data calibration to a complete
  registered trade constructor. \code{specify} loads supplied structural
  parameters through the legacy parameterized path where available.
  \code{simulate} clones the fitted object, applies one policy scenario, and
  invokes the existing model-specific equilibrium methods. A fitted object
  can be simulated repeatedly.
}
\value{
  \code{calibrate} and \code{specify} return a \code{TradeFit}. \code{simulate}
  returns the existing trade S4 result class selected by the fitted model, or
  a \code{TradeCounterfactualPath} for a multi-step \code{Counterfactual}.
}
\section{TradeFit class}{
  A \code{TradeFit} contains the normalized model specification, the existing
  calibrated S4 model object, structural parameters, observed inputs, and
  calibration diagnostics.
}
\section{TradeCounterfactualPath class}{
  A \code{TradeCounterfactualPath} is a trade-owned subclass of antitrust's
  \code{CounterfactualPath} with no additional slots. It exists only so that
  \code{simulate()} dispatches to trade's tariff/quota step-resolution logic
  when resuming a path produced by simulating a \code{TradeFit}, without
  overriding the shared \code{CounterfactualPath} method that antitrust and
  other structural-fit families rely on for their own paths.
}
\seealso{\code{\link{model_spec}}, \code{\link{supportedModels}}}
