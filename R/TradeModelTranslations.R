# Deterministic demand translations for TradeFit objects.
#
# Trade currently has complete supplied-parameter paths for flat Logit and CES
# Bertrand and monopolistic-competition tariff models.  This file therefore
# implements only those registered transitions.  It does not introduce
# nested or Cournot parameter paths that the legacy package does not provide.

.trade_translation_market_elasticity <- function(model) {
  value <- try(elast(model, preMerger = TRUE, market = TRUE), silent = TRUE)
  if (inherits(value, "try-error")) NA_real_ else as.numeric(value)
}

.trade_translation_state <- function(fit) {
  model <- fit@model
  prices <- as.numeric(model@pricePre)
  quantities <- as.numeric(calcQuantities(model, preMerger = TRUE))
  qshares <- as.numeric(calcShares(model, preMerger = TRUE, revenue = FALSE))
  rshares <- as.numeric(calcShares(model, preMerger = TRUE, revenue = TRUE))
  if (!all(is.finite(quantities)) && is.numeric(fit@observed$quantities)) {
    quantities <- as.numeric(fit@observed$quantities)
  }
  if (!all(is.finite(quantities))) quantities <- qshares / sum(qshares)
  if (!all(is.finite(qshares))) qshares <- quantities / sum(quantities)
  revenues <- quantities * prices
  if (!all(is.finite(rshares))) rshares <- revenues / sum(revenues)
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
      model@priceOutside
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

.trade_translation_parameter <- function(fit, name) {
  slopes <- fit@parameters$slopes
  if (is.list(slopes) && !is.null(slopes[[name]])) {
    return(slopes[[name]])
  }
  if (.trade_has_slot(fit@model, "slopes") &&
      is.list(fit@model@slopes) && !is.null(fit@model@slopes[[name]])) {
    return(fit@model@slopes[[name]])
  }
  NULL
}

.trade_translation_validate_alpha <- function(alpha) {
  if (!is.numeric(alpha) || length(alpha) != 1L ||
      !is.finite(alpha) || alpha >= 0) {
    stop("target trade Logit 'alpha' must be a finite, negative scalar")
  }
  as.numeric(alpha)
}

.trade_translation_validate_gamma <- function(gamma) {
  if (!is.numeric(gamma) || length(gamma) != 1L ||
      !is.finite(gamma) || gamma <= 0) {
    stop("target trade CES 'gamma' must be a finite, positive scalar")
  }
  as.numeric(gamma)
}

.trade_translation_logit_meanval <- function(shares, prices, alpha,
                                             has_outside, price_outside,
                                             reference) {
  if (any(shares <= 0)) stop("demand translation requires strictly positive baseline shares")
  if (has_outside) {
    outside <- 1 - sum(shares)
    if (outside <= 0) stop("the target Logit model has no positive outside share")
    result <- log(shares / outside) - alpha * (prices - price_outside)
  } else {
    result <- log(shares / shares[reference]) -
      alpha * (prices - prices[reference])
    result[reference] <- 0
  }
  names(result) <- names(shares)
  result
}

.trade_translation_ces_meanval <- function(shares, prices, gamma,
                                           has_outside, price_outside,
                                           reference) {
  if (any(shares <= 0)) stop("demand translation requires strictly positive baseline shares")
  if (has_outside) {
    outside <- 1 - sum(shares)
    if (outside <= 0 || price_outside <= 0) {
      stop("the target CES model requires a positive outside-good price")
    }
    result <- (shares / outside) * (price_outside / prices)^(1 - gamma)
  } else {
    result <- (shares / shares[reference]) *
      (prices[reference] / prices)^(1 - gamma)
    result[reference] <- 1
  }
  names(result) <- names(shares)
  result
}

.trade_translation_reference <- function(state) {
  index <- as.integer(state$norm_index)[1]
  if (is.na(index) || index < 1L || index > length(state$prices)) 1L else index
}

.trade_translation_price_outside <- function(state, target, has_outside) {
  value <- as.numeric(state$price_outside)[1]
  if (target == "ces" && (!is.finite(value) || value <= 0)) return(1)
  if (!has_outside) return(if (is.finite(value) && value >= 0) value else 1)
  if (!is.finite(value) || value < 0) 0 else value
}

.trade_translation_target_args <- function(state, target, parameters,
                                           inside_size, price_outside) {
  list(
    demand = target,
    prices = state$prices,
    parameters = parameters,
    tariffPre = state$model@tariffPre,
    owner = state$owner,
    insideSize = inside_size,
    priceOutside = price_outside,
    labels = state$labels
  )
}

.trade_translation_build <- function(state, target, parameters, inside_size,
                                     price_outside) {
  result <- do.call(specify, .trade_translation_target_args(
    state, target, parameters, inside_size, price_outside
  ))
  if (target$demand == "ces" && .trade_has_slot(result@model, "mktSize")) {
    result@model@mktSize <- inside_size / sum(calcShares(
      result@model, preMerger = TRUE, revenue = TRUE
    ))
  }
  validObject(result@model)
  result
}

.translate_trade_demand <- function(fit, target, transition, supplied) {
  state <- .trade_translation_state(fit)
  source <- fit@spec$demand
  target_demand <- target$demand
  missing <- setdiff(transition$required_arguments, names(supplied))
  if (length(missing)) {
    stop("respecify() transition from '", source, "' to '", target_demand,
         "' requires explicit target primitive(s): ", paste(missing, collapse = ", "))
  }
  if (!all(c(source, target_demand) %in% c("logit", "ces"))) {
    stop("trade currently supports deterministic respecification only between flat Logit and CES")
  }

  to_ces <- target_demand == "ces"
  shares <- if (to_ces) state$revenue_shares else state$quantity_shares
  has_outside <- if (to_ces) state$has_outside_revenue else state$has_outside_quantity
  price_outside <- .trade_translation_price_outside(
    state, target_demand, has_outside
  )
  inside_size <- if (to_ces) sum(state$revenues) else sum(state$quantities)
  reference <- .trade_translation_reference(state)

  if (to_ces) {
    gamma <- if (source == "ces") {
      as.numeric(.trade_translation_parameter(fit, "gamma"))[1]
    } else {
      as.numeric(supplied$gamma)[1]
    }
    gamma <- .trade_translation_validate_gamma(gamma)
    meanval <- .trade_translation_ces_meanval(
      shares, state$prices, gamma, has_outside, price_outside, reference
    )
    parameters <- list(gamma = gamma, meanval = meanval,
                       shareInside = sum(shares))
    mapping <- list(formula = "mu_j proportional to revenue_share_j * price_j^(gamma - 1)")
  } else {
    alpha <- if (source == "logit") {
      as.numeric(.trade_translation_parameter(fit, "alpha"))[1]
    } else {
      as.numeric(supplied$alpha)[1]
    }
    alpha <- .trade_translation_validate_alpha(alpha)
    meanval <- .trade_translation_logit_meanval(
      shares, state$prices, alpha, has_outside, price_outside, reference
    )
    parameters <- list(alpha = alpha, meanval = meanval)
    mapping <- list(formula = "delta_j = log(quantity_share_j / reference_share) - alpha * price_difference")
  }

  target_fit <- .trade_translation_build(
    state, target, parameters, inside_size, price_outside
  )
  target_e <- as.matrix(elast(target_fit@model, preMerger = TRUE))
  target_market_elasticity <- .trade_translation_market_elasticity(target_fit@model)
  target_shares <- as.numeric(calcShares(
    target_fit@model, preMerger = TRUE, revenue = to_ces
  ))
  target_q <- as.numeric(calcQuantities(target_fit@model, preMerger = TRUE))
  source_j <- state$elasticity * outer(state$quantities, 1 / state$prices)
  target_j <- target_e * outer(target_q, 1 / state$prices)
  e_diff <- target_e - state$elasticity
  j_diff <- target_j - source_j
  list(
    fit = target_fit,
    state = state,
    shares = shares,
    parameters = parameters,
    diagnostics = list(
      source_demand = source,
      target_demand = target_demand,
      transition_kind = transition$kind,
      baseline_price_discrepancy = max(abs(target_fit@model@pricePre - state$prices), na.rm = TRUE),
      baseline_quantity_discrepancy = max(abs(target_q - state$quantities), na.rm = TRUE),
      baseline_share_discrepancy = max(abs(target_shares - shares), na.rm = TRUE),
      required_arguments = transition$required_arguments,
      derived_parameters = parameters,
      discarded_parameters = transition$discarded,
      target_parameter_validity = isTRUE(validObject(target_fit@model, test = TRUE)),
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
      parameter_mapping = mapping
    )
  )
}
