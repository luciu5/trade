% tariff_accounts documentation
\name{tariff_accounts}
\alias{tariff_accounts}
\title{Product tariff accounts}
\description{
Return product-level revenue, cost, producer-profit, and government-revenue
accounts for a \code{TariffGameFit}.
}
\usage{
tariff_accounts(object, preMerger = FALSE)
}
\arguments{
\item{object}{A \code{TariffGameFit}.}
\item{preMerger}{If \code{TRUE}, report the stored baseline; otherwise report
the current post-policy state.}
}
\value{
A data frame with product, firm, consumer price, quantity, tariff fraction,
gross revenue, net revenue, effective cost, physical cost,
effective unit markup, physical unit profit, net-revenue margin, producer
profit, and government revenue. Gross revenue is price times quantity;
government revenue is the tariff fraction times gross revenue.
}
\details{
Inactive products retain their stored price state but have zero monetary
accounts. Costs are reported in both effective and physical units so that
tariff revenue is not applied twice.
}
\seealso{\code{\link{TariffGameFit}}, \code{\link{tariff_welfare}}}
