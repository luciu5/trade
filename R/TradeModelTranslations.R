# Local flat Logit/CES demand translations for TradeFit objects.
# Nested demand systems are not currently registered in trade; those
# transitions remain unavailable here until a complete trade model path exists.

.trade_translation_market_elasticity <- function(model) {
  value <- try(elast(model, preMerger = TRUE, market = TRUE), silent = TRUE)
  if (inherits(value, "try-error")) NA_real_ else as.numeric(value)
}

.trade_translation_state <- function(fit) {
  model <- fit@model
  prices <- as.numeric(model@pricePre)
  qshares <- as.numeric(calcShares(model, preMerger = TRUE, revenue = FALSE))
  rshares <- as.numeric(calcShares(model, preMerger = TRUE, revenue = TRUE))
  quantities <- as.numeric(calcQuantities(model, preMerger = TRUE))
  revenues <- as.numeric(calcRevenues(model, preMerger = TRUE))
  ## The historical parameterized CES path can leave mktSize as numeric(0)
  ## when shareInside equals one.  Recover the same inside-revenue accounting
  ## used by the calibrated CES path without changing the legacy constructor.
  if (identical(fit@spec$demand, "ces") && !length(revenues)) {
    revenues <- model@insideSize * rshares / sum(rshares)
    quantities <- revenues / prices
  }
  if (any(!is.finite(c(prices, qshares, rshares, quantities, revenues)))) {
    stop("respecify() requires finite baseline prices, shares, and quantities")
  }
  list(
    model = model,
    prices = prices,
    quantity_shares = qshares,
    revenue_shares = rshares,
    quantities = quantities,
    revenues = revenues,
    has_outside_quantity = sum(qshares) < 1 - 1e-8,
    has_outside_revenue = sum(rshares) < 1 - 1e-8,
    norm_index = if ("normIndex" %in% methods::slotNames(model)) model@normIndex else NA_integer_,
    owner = .trade_raw_owner(fit, model),
    labels = if ("labels" %in% methods::slotNames(model)) model@labels else names(prices),
    elasticity = as.matrix(elast(model, preMerger = TRUE)),
    market_elasticity = .trade_translation_market_elasticity(model)
  )
}

.trade_translation_reference <- function(state) {
  index <- as.integer(state$norm_index)[1]
  if (is.na(index) || index < 1L || index > length(state$prices)) 1L else index
}

.trade_translation_logit_meanval <- function(shares, prices, alpha,
                                             has_outside, reference) {
  if (any(shares <= 0)) stop("local demand translation requires positive shares")
  if (has_outside) {
    outside <- 1 - sum(shares)
    if (outside <= 0) stop("the target Logit model has no positive outside share")
    meanval <- log(shares / outside) - alpha * prices
  } else {
    meanval <- log(shares / shares[reference]) -
      alpha * (prices - prices[reference])
    meanval[reference] <- 0
  }
  names(meanval) <- names(shares)
  meanval
}

.trade_translation_ces_meanval <- function(shares, prices, gamma,
                                           has_outside, reference) {
  if (any(shares <= 0)) stop("local demand translation requires positive shares")
  if (has_outside) {
    outside <- 1 - sum(shares)
    if (outside <= 0) stop("the target CES model has no positive outside share")
    meanval <- (shares / outside) * prices^(gamma - 1)
  } else {
    meanval <- (shares / shares[reference]) *
      (prices[reference] / prices)^(1 - gamma)
    meanval[reference] <- 1
  }
  names(meanval) <- names(shares)
  meanval
}

.trade_translation_target_args <- function(state, target, parameters,
                                           shares, inside_size,
                                           price_outside, tariff_pre) {
  list(
    demand = target,
    prices = state$prices,
    parameters = parameters,
    tariffPre = tariff_pre,
    owner = state$owner,
    insideSize = inside_size,
    priceOutside = price_outside,
    labels = state$labels
  )
}

.trade_translation_build <- function(state, target, parameters, shares,
                                     inside_size, price_outside, tariff_pre) {
  args <- .trade_translation_target_args(
    state, target, parameters, shares, inside_size, price_outside, tariff_pre
  )
  result <- do.call(specify, args)
  if (identical(target$demand, "ces") && "mktSize" %in% methods::slotNames(result@model)) {
    ## Keep the target CES market-size slot usable for output methods.  This is
    ## the total revenue implied by the target inside revenue and share.
    result@model@mktSize <- inside_size / sum(shares)
  }
  result
}

.trade_translation_distance <- function(source_elasticity, target_fit) {
  target_elasticity <- as.matrix(elast(target_fit@model, preMerger = TRUE))
  difference <- target_elasticity - source_elasticity
  finite <- is.finite(difference)
  if (!any(finite)) return(Inf)
  mean(difference[finite]^2)
}

.translate_trade_demand <- function(fit, target) {
  state <- .trade_translation_state(fit)
  if (!all(c(fit@spec$demand, target$demand) %in% c("logit", "ces"))) {
    stop("trade currently supports local respecification only between flat Logit and CES")
  }
  to_ces <- identical(target$demand, "ces")
  shares <- if (to_ces) state$revenue_shares else state$quantity_shares
  inside_size <- if (to_ces) sum(state$revenues) else sum(state$quantities)
  has_outside <- if (to_ces) state$has_outside_revenue else state$has_outside_quantity
  price_outside <- if (to_ces) 1 else 0
  reference <- .trade_translation_reference(state)
  tariff_pre <- state$model@tariffPre

  if (to_ces) {
    objective_gamma <- function(gamma) {
      meanval <- try(.trade_translation_ces_meanval(
        shares, state$prices, gamma, has_outside, reference
      ), silent = TRUE)
      if (inherits(meanval, "try-error")) return(1e12)
      parameters <- list(gamma = gamma, meanval = meanval,
                         shareInside = sum(shares))
      candidate <- try(suppressWarnings(.trade_translation_build(
        state, target, parameters, shares, inside_size, price_outside,
        tariff_pre
      )), silent = TRUE)
      if (inherits(candidate, "try-error")) return(1e12)
      .trade_translation_distance(state$elasticity, candidate)
    }
    result <- stats::optimize(objective_gamma, c(1 + 1e-5, 100))
    gamma <- result$minimum
    meanval <- .trade_translation_ces_meanval(
      shares, state$prices, gamma, has_outside, reference
    )
    parameters <- list(gamma = gamma, meanval = meanval,
                       shareInside = sum(shares))
    mapping <- list(
      formula = "mu_j proportional to revenue_share_j * price_j^(gamma - 1)",
      optimizer = "stats::optimize over gamma"
    )
  } else {
    source_gamma <- state$model@slopes$gamma
    weighted_price <- sum(state$prices * shares) / sum(shares)
    objective_alpha <- function(alpha) {
      meanval <- try(.trade_translation_logit_meanval(
        shares, state$prices, alpha, has_outside, reference
      ), silent = TRUE)
      if (inherits(meanval, "try-error")) return(1e12)
      parameters <- list(alpha = alpha, meanval = meanval)
      candidate <- try(suppressWarnings(.trade_translation_build(
        state, target, parameters, shares, inside_size, price_outside,
        tariff_pre
      )), silent = TRUE)
      if (inherits(candidate, "try-error")) return(1e12)
      .trade_translation_distance(state$elasticity, candidate)
    }
    sign <- if (isTRUE(state$model@output)) -1 else 1
    initial <- sign * max(abs(source_gamma) / weighted_price, 1e-3)
    bounds <- if (sign < 0) c(-1e6, -1e-5) else c(1e-5, 1e6)
    result <- stats::optimize(objective_alpha, bounds,
                              tol = .Machine$double.eps^0.25)
    alpha <- result$minimum
    meanval <- .trade_translation_logit_meanval(
      shares, state$prices, alpha, has_outside, reference
    )
    parameters <- list(alpha = alpha, meanval = meanval)
    mapping <- list(
      formula = "meanval_j = log(quantity_share_j / reference_share) - alpha * price_difference",
      initial_alpha = initial,
      optimizer = "stats::optimize over alpha"
    )
  }

  target_fit <- .trade_translation_build(
    state, target, parameters, shares, inside_size, price_outside, tariff_pre
  )
  target_elasticity <- as.matrix(elast(target_fit@model, preMerger = TRUE))
  target_shares <- as.numeric(calcShares(target_fit@model, TRUE,
                                         revenue = to_ces))
  target_quantities <- as.numeric(calcQuantities(target_fit@model, TRUE))
  share_difference <- target_shares - shares
  quantity_difference <- target_quantities - state$quantities
  elasticity_difference <- target_elasticity - state$elasticity
  diagnostics <- list(
    source_demand = fit@spec$demand,
    target_demand = target$demand,
    transition_type = "local-demand-translation",
    baseline_share_discrepancy = max(abs(share_difference), na.rm = TRUE),
    baseline_quantity_discrepancy = max(abs(quantity_difference), na.rm = TRUE),
    elasticity_rmse = sqrt(mean(elasticity_difference^2, na.rm = TRUE)),
    maximum_absolute_elasticity_difference = max(abs(elasticity_difference), na.rm = TRUE),
    source_market_elasticity = state$market_elasticity,
    target_market_elasticity = .trade_translation_market_elasticity(target_fit@model),
    parameter_mapping = mapping,
    optimizer = list(convergence = TRUE, objective = result$objective),
    target_parameters = parameters
  )
  list(fit = target_fit, state = state, parameters = parameters,
       shares = shares, inside_size = inside_size, diagnostics = diagnostics)
}
