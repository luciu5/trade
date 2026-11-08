% TariffGameFit documentation
\name{TariffGameFit}
\alias{TariffGameFit}
\alias{TariffGameFit-class}
\alias{simulate,TariffGameFit-method}
\alias{simulate_steps,TariffGameFit-method}
\alias{validate_counterfactual,TariffGameFit-method}
\alias{respecify,TariffGameFit-method}
\alias{calcPrices,TariffGameFit-method}
\alias{calcShares,TariffGameFit-method}
\alias{calcQuantities,TariffGameFit-method}
\alias{calcMargins,TariffGameFit-method}
\alias{calcMC,TariffGameFit-method}
\alias{calcSlopes,TariffGameFit-method}
\alias{calcRevenues,TariffGameFit-method}
\alias{calcProducerSurplus,TariffGameFit-method}
\alias{CV,TariffGameFit-method}
\alias{summary,TariffGameFit-method}
\title{A fitted tariff reuse wrapper}
\description{
\code{TariffGameFit} preserves a fitted output-market game and records its
consumer-price tariff fractions, effective costs, recovered physical costs,
ownership, roles, and immutable source fit. Promotion does not recalibrate or
solve the baseline. It supports ordinary output Logit/CES fits and standard
output Logit second-score auction fits under the explicit tariff contract and
the four native coordination classes
\code{StackelbergLogit}, \code{StackelbergCES}, \code{CoreFringeLogit}, and
\code{CoreFringeCES}.
}
\usage{
\S4method{simulate}{TariffGameFit}(object, tariffPost = NULL,
  quotaPost = NULL, ownerPost = NULL, leadersPost = NULL,
  corePost = NULL, mcDelta = NULL, subset = NULL,
  priceStart = NULL, ...)

\S4method{simulate_steps}{TariffGameFit}(object, last_result, steps, ...)

\S4method{validate_counterfactual}{TariffGameFit}(object, counterfactual)
}
\arguments{
\item{object}{A \code{TariffGameFit}.}
\item{tariffPost}{A scalar or product vector of finite consumer-price tariff
fractions less than one, or a \code{Counterfactual}.}
\item{quotaPost}{Unsupported for this initial tariff wrapper.}
\item{ownerPost}{A post-policy product-to-firm vector.}
\item{leadersPost}{Post-policy Stackelberg leader firm IDs. Required for a
Stackelberg ownership change.}
\item{corePost}{Post-policy Core-Fringe core firm IDs. Required for a
Core-Fringe ownership change; an empty core is valid.}
\item{mcDelta}{A scalar, named product vector, or product vector of physical
proportional cost changes. If omitted, retain the current physical-cost shock;
explicit zero restores original baseline physical costs.}
\item{subset}{A logical, product-index, or product-label active-product
selection.}
\item{priceStart}{Optional positive post-policy price starting values.}
\item{last_result,steps}{The last solved wrapper and the next ordered steps
for the sequential extension point.}
\item{counterfactual}{A supported one-step or multi-step \code{Counterfactual}.}
\item{...}{Additional arguments; unsupported lifecycle and policy arguments
are rejected.}
}
\details{
Baseline and post costs must be finite and strictly positive in this initial
percentage-cost adapter. Zero or negative cost states fail explicitly; retain
those posterior draw identities when reporting simulation failures.
Post-policy effective costs are derived as
\code{cPre * (1 + mcDelta) / (1 - tariffPost)}. Scenarios always start from
the original fitted baseline, so repeated simulations do not compound tariff
adjustments. Actual ownership and revenue retention are kept separate.
Mixed post tariffs are supported except within native second-score bidding
portfolios. Baseline retention must be compatible with the fitted source.
Sequential \code{Counterfactual} paths instead advance each solved market
before applying the next step. Physical cost shocks compound, omitted tariffs
persist at their current absolute levels, and exits are cumulative. Paths can
be resumed using \code{simulate(path, counterfactual(...))}. An entrant's
physical cost is converted to effective cost exactly once. Tariff precedence
is a simultaneous tariff field, the entrant's tariff extra, then the fit's
\code{entrant_tariff} default. Entry is implemented only for supported ordinary
output models, and pre-entry output is zero. Native coordination paths require
explicit leader/core roles when ownership changes; native entry, quota policies
and re-estimation remain unsupported. \code{calcMC()} reports physical costs, while
\code{calcMargins(level = TRUE)} reports physical net unit profit and
\code{calcMargins(level = FALSE)} reports a net-revenue Lerner margin.
}
\value{
\code{simulate()} returns a \code{CounterfactualPath} for multiple steps. A
single step returns a new \code{TariffGameFit} containing the updated
underlying model and explicit tariff/physical-cost state. Accessor methods
return values for that stored game.
}
\seealso{\code{\link{as_trade_fit}}, \code{\link{tariff_accounts}},
\code{\link{tariff_welfare}}}
