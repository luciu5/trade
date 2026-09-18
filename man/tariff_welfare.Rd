% tariff_welfare documentation
\name{tariff_welfare}
\alias{tariff_welfare}
\title{Tariff welfare accounting}
\description{
Summarize physical producer surplus, importing-government revenue, and the
established antitrust consumer-variation statistic for a \code{TariffGameFit}.
}
\usage{
tariff_welfare(object)
}
\arguments{
\item{object}{A \code{TariffGameFit} with a stored post-policy state.}
}
\value{
A named list containing physical producer-surplus change,
importing-government revenue change, their sum, established antitrust
consumer variation, and the negative of that variation as monetary consumer
change. The list also records the CV sign convention and scopes: all included
producers and the importing government.}
\details{
The producer and government changes are physical accounting transfers and are
reported separately from consumer variation. Consumer variation uses the
established antitrust convention, in which a positive value is compensation
required for a price increase. No nationality is inferred and no CES-specific
quasilinear welfare measure is introduced.
}
\seealso{\code{\link{TariffGameFit}}, \code{\link{tariff_accounts}}}
