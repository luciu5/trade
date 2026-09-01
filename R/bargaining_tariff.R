#'Tariff Simulation With A Nash Bargaining Game
#'
#' Simulate the effect of tariffs when firms play a Nash Bargaining game and consumer demand is Logit or CES.
#'
#' @param demand A character vector indicating which demand system to use. Currently allows logit (default) or ces.
#' @param prices  A length k vector product prices.
#' @param shares A length k vector of product shares. Values must be between 0 and 1.
#' @param margins A length k vector of product margins. All margins must be in \strong{levels} (not w.r.t to price), or NA.
#' @param owner Required. EITHER a vector of length k whose values indicate which firm produced a product before the tariff OR a k x k matrix of pre-tariff ownership shares.
#' @param diversions  A k x k matrix of diversion ratios with diagonal elements equal to -1. Default is missing, in which case diversion according to revenue share is assumed.
#' @param mktElast A negative number equal to the industry pre-merger price elasticity. Default is NA .
#' @param insideSize An integer equal to total pre-merger units sold.
#' If shares sum to one, this also equals the size of the market.
#' @param tariffPre  A vector of length k where each element equals the \strong{current} \emph{ad valorem} tariff
#' (expressed as a proportion of the consumer price) imposed on each product. Default is 0, which assumes no tariff.
#' @param tariffPost  A vector of length k where each element equals the \strong{new}  \emph{ad valorem} tariff
#' (expressed as a proportion of the consumer price) imposed on each product. Default is 0, which assumes no tariff.
#' @param bargpowerPre A length k vector of pre-tariff bargaining power parameters. Values
#' must be between 0 (sellers have the power) and 1 (buyers the power). NA values are allowed,
#' though must be calibrated from additional margin and share data. Default is 0.5.
#' @param bargpowerPost A length k vector of post-tariff bargaining power parameters. Values
#' must be between 0 (sellers have the power) and 1 (buyers the power). NA values are allowed,
#' though must be calibrated from additional margin and share data. Default is \sQuote{bargpowerPre}.
#' @param normIndex An integer equalling the index (position) of the
#' inside product whose mean valuation will be normalized to 1. Default
#' is 1, unless \sQuote{shares} sum to less than 1, in which case the default is
#' NA and an outside good is assumed to exist.
#' @param priceStart For aids, a vector of length k who elements equal to an initial guess of the proportional change in price caused by the merger.
#'  The default is to draw k random elements from a [0,1] uniform distribution. For ces and logit, the default is prices.
#' @param priceOutside price of the outside good. Equals 0 for logit and 1 for ces. Not used for aids.
#' @param control.slopes A list of  \code{\link[stats]{optim}}  control parameters passed to the calibration routine optimizer (typically the \code{calcSlopes} method).
#' @param control.equ A list of  \code{\link[BB]{BBsolve}} control parameters passed to the non-linear equation solver (typically the \code{calcPrices} method).
#' @param labels A k-length vector of labels.
#' @param ... Additional options to feed to the \code{\link[BB]{BBsolve}} optimizer used to solve for equilibrium prices.
#'
#' @details
#'
#' Let k denote the number of products produced by all firms.
#' Using price, and quantity, information for all products
#' in each market, as well as margin information for at least
#' one products in each market, \code{bargaining_tariff} is able to
#' recover the slopes and intercepts of a Logit or CES demand
#' system. These parameters are then used to simulate the price
#' effects of an \emph{ad valorem} tariff under the assumption that the firms are playing a
#' Nash Bargaining game.
#'
#' @seealso \code{\link{bertrand_tariff}} to simulate the effects of a tariff under a Bertrand pricing game and \code{\link{monopolistic_competition_tariff}} to simulate the effects of a tariff under monopolistic competition.
#'
#' @return \code{bargaining_tariff} returns an instance of class \code{\linkS4class{TariffBargainingLogit}} or \code{\linkS4class{TariffBargainingCES}}
#' @references Simon P. Anderson, Andre de Palma, Brent Kreider, Tax incidence in differentiated product oligopoly,
#' Journal of Public Economics, Volume 81, Issue 2, 2001, Pages 173-192.
#' @export

bargaining_tariff <- function(
  demand = c("logit", "ces"),
  prices, shares, margins,
  owner = NULL,
  mktElast = NA_real_,
  insideSize = NA_real_,
  diversions,
  tariffPre = rep(0, length(shares)),
  tariffPost = rep(0, length(shares)),
  bargpowerPre = rep(0.5, length(prices)),
  bargpowerPost = bargpowerPre,
  normIndex = ifelse(isTRUE(all.equal(sum(shares), 1, check.names = FALSE)), 1, NA),
  priceOutside = ifelse(demand == "logit", 0, 1),
  priceStart,
  control.slopes,
  control.equ,
  labels = paste("Prod", 1:length(shares), sep = ""),
  ...
) {
  demand <- match.arg(demand)
  nprods <- length(shares)

  if (missing(prices)) {
    prices <- rep(NA_real_, nprods)
  }

  subset <- rep(TRUE, nprods)

  tariffPre <- .normalize_tariff(tariffPre, nprods, "tariffPre")
  tariffPost <- .normalize_tariff(tariffPost, nprods, "tariffPost")

  owner <- .owner_to_matrix(owner, nprods,
                            "'owner' must be supplied as a length-k vector or k x k ownership matrix")

  ownerPost <- .apply_tariff_to_owner(owner, tariffPost)
  ownerPre <- .apply_tariff_to_owner(owner, tariffPre)

  mcDelta <- .tariff_mc_delta(tariffPre, tariffPost)

  shares_revenue <- shares_quantity <- shares

  if (all(!is.na(prices))) {
    shares_revenue <- prices * shares_quantity / sum(prices * shares_quantity)
  }

  if (missing(priceStart)) priceStart <- prices

  if (demand == "logit" && missing(diversions)) {
    diversions <- tcrossprod(1 / (1 - shares_quantity), shares_quantity)
    diag(diversions) <- -1
  } else if (demand == "ces" && missing(diversions)) {
    diversions <- tcrossprod(1 / (1 - shares_revenue), shares_revenue)
    diag(diversions) <- -1
  }

  result <- switch(demand,
    logit = new("TariffBargainingLogit",
      prices = prices,
      shares = shares_quantity,
      margins = margins,
      weights = rep(1, nprods),
      ownerPre = ownerPre,
      ownerPost = ownerPost,
      mktElast = mktElast,
      mcDelta = mcDelta,
      subset = subset,
      priceOutside = priceOutside,
      priceStart = priceStart,
      diversion = diversions,
      shareInside = sum(shares_quantity),
      bargpowerPre = bargpowerPre,
      bargpowerPost = bargpowerPost,
      normIndex = normIndex,
      tariffPre = tariffPre,
      tariffPost = tariffPost,
      insideSize = insideSize,
      labels = labels
    ),
    ces = new("TariffBargainingCES",
      prices = prices,
      shares = shares_revenue,
      margins = margins,
      weights = rep(1, nprods),
      ownerPre = ownerPre,
      ownerPost = ownerPost,
      mktElast = mktElast,
      mcDelta = mcDelta,
      subset = subset,
      priceOutside = priceOutside,
      priceStart = priceStart,
      diversion = diversions,
      shareInside = sum(shares_revenue),
      bargpowerPre = bargpowerPre,
      bargpowerPost = bargpowerPost,
      normIndex = normIndex,
      tariffPre = tariffPre,
      tariffPost = tariffPost,
      insideSize = insideSize,
      labels = labels
    )
  )

  if (!missing(control.slopes)) {
    result@control.slopes <- control.slopes
  }
  if (!missing(control.equ)) {
    result@control.equ <- control.equ
  }

  result@ownerPre <- ownerToMatrix(result, TRUE)
  result@ownerPost <- ownerToMatrix(result, FALSE)

  result <- calcSlopes(result)

  result@mcPre <- calcMC(result, TRUE)
  result@mcDelta <- result@mcPre * mcDelta
  result@mcPost <- calcMC(result, FALSE)

  result@pricePre <- calcPrices(result, preMerger = TRUE)
  result@pricePost <- calcPrices(result, preMerger = FALSE)

  return(result)
}
