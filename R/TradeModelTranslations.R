# Demand translation adapter for TradeFit objects.
#
# Demand conversion is implemented by antitrust. Trade supplies only the
# adapter needed to present a tariff model as an AntitrustFit, then rebuilds
# the target tariff model with the translated demand primitives. Keeping the
# conversion in one package prevents the two transition graphs from drifting.

.trade_translation_market_elasticity <- function(model) {
  value <- try(elast(model, preMerger = TRUE, market = TRUE), silent = TRUE)
  if (inherits(value, "try-error")) NA_real_ else as.numeric(value)
}

.trade_translation_state <- function(fit) {
  model <- fit@model
  prices <- as.numeric(model@pricePre)
  quantities <- as.numeric(calcQuantities(model, preMerger = TRUE))
  qshares <- as.numeric(calcShares(model, preMerger = TRUE,
                                   revenue = FALSE))
  rshares <- as.numeric(calcShares(model, preMerger = TRUE,
                                   revenue = TRUE))
  if (any(!is.finite(quantities)) && is.numeric(fit@observed$quantities)) {
    quantities <- as.numeric(fit@observed$quantities)
  }
  if (any(!is.finite(quantities))) quantities <- qshares / sum(qshares)
  if (any(!is.finite(qshares))) qshares <- quantities / sum(quantities)
  revenues <- quantities * prices
  if (any(!is.finite(rshares))) rshares <- revenues / sum(revenues)
  if (any(!is.finite(c(prices, quantities, qshares, rshares, revenues)))) {
    stop("respecify() requires finite baseline prices, shares, and quantities")
  }
  list(
    model = model,
    prices = prices,
    quantities = quantities,
    quantity_shares = qshares,
    revenue_shares = rshares,
    revenues = revenues,
    has_outside_quantity = sum(qshares) < 1 - 1e-8,
    has_outside_revenue = sum(rshares) < 1 - 1e-8,
    price_outside = if (.trade_has_slot(model, "priceOutside")) {
      as.numeric(model@priceOutside)[1]
    } else {
      0
    },
    norm_index = if (.trade_has_slot(model, "normIndex")) {
      model@normIndex
    } else {
      NA_integer_
    },
    owner = .trade_raw_owner(fit, model),
    labels = if (.trade_has_slot(model, "labels")) model@labels else names(prices),
    elasticity = as.matrix(elast(model, preMerger = TRUE)),
    market_elasticity = .trade_translation_market_elasticity(model)
  )
}

.trade_antitrust_conduct <- function(conduct) {
  ## MonCom is now a first-class antitrust conduct.  Use that registered
  ## target for demand translation so the proxy carries the same conduct
  ## identity; trade still rebuilds the final target through its tariff-aware
  ## MonCom implementation.
  conduct
}

.trade_validate_translation_arguments <- function(target, supplied) {
  if (identical(target$demand, "ces") && !is.null(supplied$gamma) &&
      (!is.numeric(supplied$gamma) || length(supplied$gamma) != 1L ||
       !is.finite(supplied$gamma) || supplied$gamma <= 1)) {
    stop("target trade CES 'gamma' must be a finite scalar greater than 1 for the output-market CES path")
  }
  if (identical(target$demand, "logit") && !is.null(supplied$alpha) &&
      (!is.numeric(supplied$alpha) || length(supplied$alpha) != 1L ||
       !is.finite(supplied$alpha) || supplied$alpha >= 0)) {
    stop("target trade Logit 'alpha' must be a finite, negative scalar")
  }
}

.trade_antitrust_proxy <- function(fit, state) {
  source <- fit@spec
  antitrust_spec <- antitrust::model_spec(
    source$demand,
    .trade_antitrust_conduct(source$conduct),
    variant = source$variant
  )
  parameters <- fit@parameters
  if (is.list(parameters$slopes)) parameters <- parameters$slopes

  proxy_model <- fit@model
  if (.trade_has_slot(proxy_model, "shares")) {
    proxy_model@shares <- if (identical(source$demand, "ces")) {
      state$revenue_shares
    } else {
      state$quantity_shares
    }
  }

  observed <- fit@observed
  observed$ownerPre <- state$owner
  observed$prices <- state$prices
  observed$quantities <- state$quantities
  observed$shares <- if (identical(source$demand, "ces")) {
    state$revenue_shares
  } else {
    state$quantity_shares
  }
  if (is.null(observed$margins)) observed$margins <- NULL

  baseline <- fit@diagnostics$calibration_args
  if (!is.list(baseline)) baseline <- fit@diagnostics$specification_args
  if (!is.list(baseline)) baseline <- list()
  baseline[c(
    "demand", "conduct", "variant", "prices", "shares", "quantities",
    "ownerPre", "insideSize", "priceOutside", "labels"
  )] <- list(
    source$demand,
    .trade_antitrust_conduct(source$conduct),
    source$variant,
    state$prices,
    observed$shares,
    state$quantities,
    state$owner,
    sum(state$quantities),
    state$price_outside,
    state$labels
  )
  diagnostics <- fit@diagnostics
  diagnostics$calibration_args <- baseline

  methods::new(
    "AntitrustFit",
    spec = antitrust_spec,
    model = proxy_model,
    parameters = parameters,
    observed = observed,
    diagnostics = diagnostics
  )
}

.trade_antitrust_translation <- function(fit, target, supplied, state) {
  .trade_validate_translation_arguments(target, supplied)
  proxy <- .trade_antitrust_proxy(fit, state)
  target_conduct <- .trade_antitrust_conduct(target$conduct)
  demand_arguments <- supplied[intersect(
    names(supplied), c("alpha", "gamma", "nests", "sigma",
                       "bargpowerPre")
  )]
  translated <- do.call(
    antitrust::respecify,
    c(
      list(
        object = proxy,
        demand = target$demand,
        conduct = target_conduct,
        variant = target$variant
      ),
      demand_arguments
    )
  )
  parameters <- translated@parameters
  target_shares <- as.numeric(translated@observed$shares)
  if (identical(target$demand, "ces") && is.null(parameters$shareInside)) {
    ## `shareInside` is a CES normalization primitive. antitrust uses it in
    ## target construction but does not retain it in its public parameter
    ## list; recover the already translated observed share total, rather than
    ## estimating it from margins or an elasticity objective.
    parameters$shareInside <- sum(target_shares)
  }
  list(
    fit = translated,
    parameters = parameters,
    shares = target_shares,
    inside_size = if (identical(target$demand, "ces")) {
      sum(state$revenues)
    } else {
      sum(state$quantities)
    },
    price_outside = if (.trade_has_slot(translated@model, "priceOutside")) {
      as.numeric(translated@model@priceOutside)[1]
    } else {
      state$price_outside
    },
    mapping = translated@diagnostics$translation,
    antitrust_transition = translated@diagnostics$transition
  )
}

.trade_translation_target_args <- function(state, target, parameters,
                                           inside_size, price_outside,
                                           conduct_arguments = list()) {
  arguments <- list(
    demand = target$demand,
    conduct = target$conduct,
    variant = target$variant,
    prices = state$prices,
    parameters = parameters,
    owner = state$owner,
    insideSize = inside_size,
    priceOutside = price_outside,
    labels = state$labels
  )
  if (.trade_has_slot(state$model, "tariffPre")) {
    arguments$tariffPre <- state$model@tariffPre
  }
  if (.trade_has_slot(state$model, "quotaPre")) {
    arguments$quotaPre <- state$model@quotaPre
  }
  c(arguments, conduct_arguments)
}

.trade_translation_build <- function(state, target, parameters, inside_size,
                                     price_outside,
                                     conduct_arguments = list()) {
  result <- do.call(specify, .trade_translation_target_args(
    state, target, parameters, inside_size, price_outside,
    conduct_arguments
  ))
  if (target$demand == "ces" && .trade_has_slot(result@model, "mktSize")) {
    result@model@mktSize <- inside_size / sum(calcShares(
      result@model, preMerger = TRUE, revenue = TRUE
    ))
  }
  validObject(result@model)
  result
}

.translate_trade_demand <- function(fit, target, transition, supplied,
                                    conduct_arguments = list()) {
  state <- .trade_translation_state(fit)
  ## The antitrust adapter owns demand conversion, but its target constructor
  ## must also see conduct primitives carried by the trade target.  Merge them
  ## by name to avoid duplicate actual arguments when the caller supplied the
  ## same primitive explicitly.
  translation_supplied <- supplied
  if (length(conduct_arguments)) {
    translation_supplied[names(conduct_arguments)] <- conduct_arguments
  }
  translated <- .trade_antitrust_translation(
    fit, target, translation_supplied, state
  )
  target_fit <- .trade_translation_build(
    state, target, translated$parameters, translated$inside_size,
    translated$price_outside, conduct_arguments
  )
  target_e <- as.matrix(elast(target_fit@model, preMerger = TRUE))
  target_market_elasticity <- .trade_translation_market_elasticity(
    target_fit@model
  )
  target_shares <- as.numeric(calcShares(
    target_fit@model, preMerger = TRUE,
    revenue = identical(target$demand, "ces")
  ))
  target_q <- as.numeric(calcQuantities(target_fit@model, preMerger = TRUE))
  source_j <- state$elasticity * outer(state$quantities, 1 / state$prices)
  target_j <- target_e * outer(target_q, 1 / state$prices)
  e_diff <- target_e - state$elasticity
  j_diff <- target_j - source_j
  antitrust_diagnostics <- translated$fit@diagnostics$translation
  list(
    fit = target_fit,
    state = state,
    shares = translated$shares,
    parameters = translated$parameters,
    diagnostics = list(
      source_demand = fit@spec$demand,
      target_demand = target$demand,
      transition_kind = transition$kind,
      delegated_to = "antitrust::respecify",
      antitrust_transition = translated$antitrust_transition,
      baseline_price_discrepancy = max(
        abs(target_fit@model@pricePre - state$prices), na.rm = TRUE
      ),
      baseline_quantity_discrepancy = max(
        abs(target_q - state$quantities), na.rm = TRUE
      ),
      baseline_share_discrepancy = max(
        abs(target_shares - translated$shares), na.rm = TRUE
      ),
      required_arguments = transition$required_arguments,
      derived_parameters = translated$parameters,
      discarded_parameters = transition$discarded,
      target_parameter_validity = isTRUE(validObject(
        target_fit@model, test = TRUE
      )),
      source_elasticity = state$elasticity,
      target_elasticity = target_e,
      source_market_elasticity = state$market_elasticity,
      target_market_elasticity = target_market_elasticity,
      elasticity_discrepancy = e_diff,
      elasticity_rmse = sqrt(mean(e_diff^2, na.rm = TRUE)),
      maximum_absolute_elasticity_difference = max(abs(e_diff), na.rm = TRUE),
      source_jacobian = source_j,
      target_jacobian = target_j,
      jacobian_discrepancy = j_diff,
      jacobian_rmse = sqrt(mean(j_diff^2, na.rm = TRUE)),
      parameter_mapping = antitrust_diagnostics$parameter_mapping
    )
  )
}
