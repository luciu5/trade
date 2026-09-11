\name{trade-architecture}
\alias{trade-architecture}
\alias{TradeFit-class}
\alias{calibrate}
\alias{specify}
\alias{simulate,TradeFit-method}
\alias{simulate_steps,TradeFit-method}
\alias{validate_counterfactual,TradeFit-method}
\title{Calibrate, specify, and simulate trade models}
\description{
  The trade architecture separates construction of a baseline structural
  model from policy counterfactual simulation.  Existing trade S4 objects
  remain the economic implementation and are returned by \code{simulate}.
  \code{simulate()} and \code{respecify()} are S4 generics owned by
  \pkg{antitrust}; trade supplies methods for its own \code{TradeFit} class.
  Trade also supplies the corresponding \code{validate_counterfactual()}
  method so shared path resumption uses trade's model capabilities.
  Resuming a multi-step \code{CounterfactualPath} produced by a
  \code{TradeFit} is handled by antitrust's own shared \code{CounterfactualPath}
  method, which dispatches the new steps back to trade through
  \code{simulate_steps()}.
}
\usage{
calibrate(demand, conduct = NULL, variant = "standard",
  policy = "tariff", ...)

specify(demand, conduct = NULL, prices, parameters,
  variant = "standard", policy = "tariff", ...)

\S4method{simulate}{TradeFit}(object, tariffPost, quotaPost, subset,
  priceStart, bargpowerPost, isMax = FALSE, ...)

\S4method{simulate_steps}{TradeFit}(object, last_result, steps, ...)

\S4method{validate_counterfactual}{TradeFit}(object, counterfactual)
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
    \code{specify}.}
  \item{tariffPost}{A post-policy ad valorem tariff vector.}
  \item{quotaPost}{A post-policy quota vector.}
  \item{subset}{A logical vector selecting products in the post-policy market.}
  \item{priceStart}{Optional post-policy price starting values.}
  \item{bargpowerPost}{Optional post-policy bargaining-power vector for
    bargaining models.}
  \item{isMax}{Whether to request the legacy local-maximum check where
    supported.}
  \item{last_result}{The legacy trade S4 result the path last landed on.}
  \item{steps}{The ordered \code{CounterfactualStep} objects to solve next.}
  \item{counterfactual}{A \code{Counterfactual} to validate against the
    fitted trade model's registered capabilities.}
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
  a \code{CounterfactualPath} for a multi-step \code{Counterfactual}.
  \code{simulate_steps} returns a list with one element, \code{results}: a
  list of legacy trade S4 results, one per step, in order.
  \code{validate_counterfactual} returns the validated counterfactual
  invisibly or raises an error for unsupported fields.
}
\section{TradeFit class}{
  A \code{TradeFit} contains the normalized model specification, the existing
  calibrated S4 model object, structural parameters, observed inputs, and
  calibration diagnostics.
}
\seealso{\code{\link{model_spec}}, \code{\link{supportedModels}}}
