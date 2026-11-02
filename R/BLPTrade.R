# BLP demand in trade tariff models.
#
# The BLP demand, contraction mapping, minimum-distance calibration, and
# conduct equations are implemented by antitrust.  Trade adds only the
# tariff-aware wrapper and translates tariff changes into the same post-state
# conventions used by its existing legacy constructors.
#' @importFrom utils modifyList
NULL

.trade_blp_class <- function(conduct) {
  switch(conduct,
    bertrand = "TariffLogitBLP",
    cournot = "TariffCournotBLP",
    auction2nd = "TariffAuction2ndBLP",
    bargaining = "TariffBargainingBLP",
    stop("unsupported trade BLP conduct: ", conduct)
  )
}

.trade_blp_tariff_model <- function(model, spec, tariffPre) {
  values <- lapply(methods::slotNames(model), function(name) {
    methods::slot(model, name)
  })
  names(values) <- methods::slotNames(model)
  values$tariffPre <- as.numeric(tariffPre)
  values$tariffPost <- as.numeric(tariffPre)
  result <- do.call(methods::new, c(list(Class = .trade_blp_class(spec$conduct)), values))
  result <- .trade_promotion_copy_attributes(model, result)
  result <- .set_tariff_retention(result)
  methods::validObject(result)
  result
}

.trade_blp_owner <- function(arguments, n) {
  if (!is.null(arguments$ownerPre) && !is.null(arguments$owner) &&
      !isTRUE(all.equal(arguments$ownerPre, arguments$owner))) {
    stop("supply either 'ownerPre' or 'owner' for trade BLP, not conflicting values for both")
  }
  owner <- if (!is.null(arguments$ownerPre)) arguments$ownerPre else arguments$owner
  .owner_to_matrix(owner, n,
                   "'ownerPre' (or legacy 'owner') must be supplied as a length-k vector or k x k ownership matrix")
}

.trade_blp_owner_for_tariff <- function(owner, tariff, conduct) {
  owner
}

.trade_blp_options <- c(
  "output", "priceOutside", "insideSize", "labels", "weights", "nDraws",
  "draws", "consDraws", "integrationPoints", "drawWeights", "integration", "integrationWeights",
  "contractionTol",
  "contractionMaxIter", "nNodes",
  "optimizer_control", "bargpowerPre", "s0"
)

.trade_blp_antitrust_options <- function(arguments, include_s0 = FALSE) {
  allowed <- if (isTRUE(include_s0)) .trade_blp_options else {
    setdiff(.trade_blp_options, "s0")
  }
  arguments[intersect(names(arguments), allowed)]
}

.trade_blp_fit <- function(antitrust_fit, spec, arguments, tariffPre,
                           conditions, route, provenance) {
  model <- .trade_blp_tariff_model(antitrust_fit@model, spec, tariffPre)
  stored <- arguments
  stored$owner <- if (!is.null(arguments$owner)) arguments$owner else {
    arguments$ownerPre
  }
  stored$ownerPre <- stored$owner
  stored$tariffPre <- tariffPre
  stored$tariffPost <- tariffPre
  fit <- .trade_fit(spec, model, stored, conditions, route = route,
                    calibration_args = provenance$calibration_args,
                    specification_args = provenance$specification_args)

  ## Keep all BLP diagnostics, including the fixed integration rule and
  ## identification profile, while adding the trade tariff translation.
  fit@diagnostics <- modifyList(antitrust_fit@diagnostics, fit@diagnostics)
  fit@diagnostics$legacy_class <- class(model)[1]
  fit@diagnostics$trade <- list(
    tariffPre = tariffPre,
    ownershipInput = stored$owner,
    ownership = model@ownerPre,
    revenueRetentionPre = antitrust::getRetention(model, TRUE)
  )
  fit@diagnostics$calibration_args <- provenance$calibration_args
  fit@diagnostics$specification_args <- provenance$specification_args
  fit@diagnostics$warnings <- unique(c(
    antitrust_fit@diagnostics$warnings,
    conditions$warnings
  ))
  fit@diagnostics$messages <- unique(c(
    antitrust_fit@diagnostics$messages,
    conditions$messages
  ))
  ## Preserve antitrust's direct BLP parameter names for lifecycle consumers
  ## while retaining the tariff wrapper's complete slope object for legacy
  ## callers.  The wrapper must not collapse alphaMean/sigma into an opaque
  ## nested list.
  fit@parameters <- antitrust_fit@parameters
  fit@parameters$slopes <- model@slopes
  if (.trade_has_slot(model, "bargpowerPre")) {
    fit@parameters$bargpowerPre <- model@bargpowerPre
  }
  fit
}

.trade_calibrate_blp <- function(spec, arguments, calibration_args) {
  prices <- arguments$prices
  shares <- arguments$shares
  margins <- arguments$margins
  if (is.null(prices) || is.null(shares) || is.null(margins)) {
    stop("trade BLP calibration requires 'prices', 'shares', and 'margins'")
  }
  if (is.null(arguments$s0)) {
    stop("trade BLP calibration requires the known outside share 's0'")
  }
  n <- length(prices)
  owner <- .trade_blp_owner(arguments, n)
  tariffPre <- .normalize_tariff(
    if (is.null(arguments$tariffPre)) rep(0, n) else arguments$tariffPre,
    n, "tariffPre"
  )
  owner_pre <- .trade_blp_owner_for_tariff(owner, tariffPre, spec$conduct)
  call <- c(
    list(
      demand = "blp", conduct = spec$conduct,
      prices = prices, shares = shares, margins = margins,
      ownerPre = owner_pre, s0 = arguments$s0,
      revenueRetentionPre = 1 - tariffPre
    ),
    .trade_blp_antitrust_options(arguments)
  )
  captured <- .trade_capture_conditions(do.call(antitrust::calibrate, call))
  if (inherits(captured$value, "try-error")) stop(captured$value)
  .trade_blp_fit(
    captured$value, spec, arguments, tariffPre, captured, "calibrate",
    list(calibration_args = calibration_args, specification_args = NULL)
  )
}

.trade_specify_blp <- function(spec, prices, parameters, arguments,
                               specification_args) {
  shares <- arguments$shares
  if (is.null(shares)) {
    stop("trade BLP parameter loading requires observed 'shares'")
  }
  n <- length(prices)
  owner <- .trade_blp_owner(arguments, n)
  tariffPre <- .normalize_tariff(
    if (is.null(arguments$tariffPre)) rep(0, n) else arguments$tariffPre,
    n, "tariffPre"
  )
  owner_pre <- .trade_blp_owner_for_tariff(owner, tariffPre, spec$conduct)
  ## Promote integration controls supplied through trade's `...` boundary
  ## into the antitrust parameter list as well. This keeps the controls
  ## attached to the portable BLP state even when downstream specify methods
  ## inspect only `parameters`.
  blp_parameters <- parameters
  for (name in intersect(names(arguments), c(
    "integrationPoints", "draws", "consDraws", "drawWeights",
    "integrationWeights", "integration", "nNodes", "nDraws"
  ))) {
    if (!is.null(arguments[[name]])) blp_parameters[[name]] <- arguments[[name]]
  }
  call <- c(
    list(
      demand = "blp", conduct = spec$conduct, prices = prices,
      parameters = blp_parameters, ownerPre = owner_pre, shares = shares,
      margins = arguments$margins, revenueRetentionPre = 1 - tariffPre
    ),
    .trade_blp_antitrust_options(arguments, include_s0 = TRUE)
  )
  captured <- .trade_capture_conditions(do.call(antitrust::specify, call))
  if (inherits(captured$value, "try-error")) stop(captured$value)
  .trade_blp_fit(
    captured$value, spec, c(arguments, list(prices = prices,
                                             shares = shares,
                                             parameters = parameters)),
    tariffPre, captured, "specify",
    list(calibration_args = NULL, specification_args = specification_args)
  )
}

.trade_recalculate_blp <- function(fit, tariffPost, subset, priceStart,
                                   bargpowerPost, isMax, arguments) {
  model <- fit@model
  n <- length(model@shares)
  tariffPost <- .normalize_tariff(tariffPost, n, "tariffPost")
  model@tariffPost <- tariffPost
  model@subset <- subset
  owner <- .trade_policy_owner(fit, model)
  owner <- .owner_to_matrix(owner, n,
                            "fitted trade BLP ownership is not valid")
  model@ownerPost <- .trade_blp_owner_for_tariff(
    owner, tariffPost, fit@spec$conduct
  )
  model <- .set_tariff_retention(model)
  ## Physical costs stay fixed; calcMC applies the retention ratio once.
  model@mcDelta <- rep(0, n)
  model@mcPost <- calcMC(model, preMerger = FALSE)
  if (!is.null(bargpowerPost) && fit@spec$conduct == "bargaining") {
    model@bargpowerPost <- bargpowerPost
  }
  if (!is.null(priceStart)) model@priceStart <- priceStart

  if (fit@spec$conduct == "auction2nd") {
    model@pricePost <- calcPrices(model, preMerger = FALSE)
  } else {
    price_arguments <- c(
      list(object = model, preMerger = FALSE, isMax = isMax),
      arguments
    )
    model@pricePost <- do.call(calcPrices, price_arguments)
  }
  model
}
