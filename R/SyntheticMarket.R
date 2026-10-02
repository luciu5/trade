## Trade-native synthetic markets. The neutral share/ownership design comes
## from antitrust, while policy calibration and equilibrium state remain in
## trade's registered model implementations.

.trade_synthetic_missing_anchors <- function(spec) {
    if (spec$variant == "alm") {
        return("a passive outside share and a validated tariff ALM equilibrium route")
    }
    if (spec$demand == "blp") {
        return("fixed heterogeneity dispersion and integration draws/weights")
    }
    if (spec$demand == "aids") {
        return("an AIDS slope/diversion structure satisfying adding-up and symmetry")
    }
    if (spec$conduct == "core_fringe") {
        return("core/fringe state and a validated game-specific equilibrium route")
    }
    if (spec$demand %in% c("linear", "loglin") &&
        spec$conduct == "cournot") {
        return("plant ownership, plant-level cost functions, and a relative demand shape")
    }
    if (spec$demand == "ces" && spec$conduct == "bargaining") {
        return("a validated tariff CES bargaining inversion; the existing observed anchors and default bargaining weight suffice economically")
    }
    if (spec$demand == "logit" && spec$conduct == "cournot") {
        return("a validated tariff Cournot quantity FOC inversion; the existing observed anchors suffice economically")
    }
    "a policy-specific allocation, demand, or conduct anchor"
}

.trade_synthetic_foc_solution <- function(shares, ownership) {
    derivative <- diag(shares) - tcrossprod(shares)
    G <- t(ownership * derivative)
    n <- length(shares)
    rank_tolerance <- sqrt(.Machine$double.eps)
    foc_rank <- qr(G, tol = rank_tolerance)$rank
    foc_condition_limit <- 1 / rank_tolerance
    foc_condition_number <- tryCatch(
        kappa(G, exact = TRUE),
        error = function(e) Inf
    )
    if (foc_rank < n || !is.finite(foc_condition_number) ||
        foc_condition_number > foc_condition_limit) {
        stop(
            "ownership-adjusted Logit FOC system is singular or ill-conditioned",
            " (rank = ", foc_rank, "/", n,
            ", condition number = ", format(foc_condition_number, digits = 6),
            ", limit = ", format(foc_condition_limit, digits = 6), ")"
        )
    }
    z <- tryCatch(solve(G, shares), error = function(e) e)
    if (inherits(z, "error") || any(!is.finite(z))) {
        stop("ownership-adjusted Logit FOC system is singular or non-finite: ",
             if (inherits(z, "error")) conditionMessage(z) else "non-finite solution")
    }
    list(
        G = G,
        z = z,
        rank = foc_rank,
        condition_number = foc_condition_number,
        condition_limit = foc_condition_limit
    )
}

.trade_synthetic_logit_parameters <- function(shares, costs, owner,
                                              reference_product, margin,
                                              tariff = rep(0, length(shares))) {
    n <- length(shares)
    owner_matrix <- .owner_to_matrix(
        owner, n,
        "'owner' must be supplied as a length-k vector or k x k ownership matrix"
    )
    if (length(tariff) != n || any(!is.finite(tariff)) ||
        any(tariff < 0) || any(tariff >= 1)) {
        stop("'tariffPre' must be a finite vector in [0, 1) when recovering logit parameters")
    }
    reference_price <- costs[reference_product] / (1 - margin)
    reference_effective_markup <- reference_price -
        costs[reference_product] / (1 - tariff[reference_product])
    if (!is.finite(reference_effective_markup) ||
        reference_effective_markup <= 0) {
        stop("the reference margin must exceed the reference tariff rate to imply a positive demand markup")
    }
    probe_prices <- rep(reference_price, n)
    ## With a nonzero tariff, trade's ALM convention scales ownership before
    ## applying the elasticity system. Ask the native TariffLogit method for
    ## the markups at alpha = -1, rather than reproducing that orientation
    ## here. The markup system is homogeneous in 1 / alpha.
    meanval_probe <- log(shares / shares[reference_product])
    meanval_probe[reference_product] <- 0
    foc_solution <- NULL
    if (any(tariff != 0)) {
        probe <- specify(
            demand = "logit", conduct = "bertrand", policy = "tariff",
            prices = probe_prices,
            parameters = list(alpha = -1, meanval = meanval_probe),
            owner = owner, priceOutside = reference_price,
            tariffPre = tariff
        )
        probe_markup <- as.numeric(calcMargins(
            probe@model, preMerger = TRUE, level = TRUE
        ))
        if (length(probe_markup) != n || !all(is.finite(probe_markup))) {
            stop("the native TariffLogit method produced non-finite probe markups")
        }
        alpha <- -unname(probe_markup[reference_product]) /
            reference_effective_markup
        markups <- probe_markup * (-1 / alpha)
        effective_owner <- probe@model@ownerPre
        foc <- rep(NA_real_, n)
    } else {
        effective_owner <- owner_matrix
        foc_solution <- .trade_synthetic_foc_solution(shares, effective_owner)
        G <- foc_solution$G
        z <- foc_solution$z
        alpha <- -unname(z[reference_product]) / reference_effective_markup
        markups <- -z / alpha
        foc <- unname(shares + G %*% (alpha * markups))
    }
    if (!is.finite(alpha) || alpha >= 0) {
        stop("the reference markup does not identify a finite negative Logit price coefficient")
    }
    prices <- costs / (1 - tariff) + markups
    if (any(!is.finite(prices)) || any(prices <= 0)) {
        stop("trade observed inputs imply invalid equilibrium consumer prices")
    }
    meanval <- log(shares / shares[reference_product]) -
        alpha * (prices - prices[reference_product])
    meanval[reference_product] <- 0
    list(alpha = alpha, meanval = meanval, prices = prices,
         markups = markups,
         ownership = effective_owner,
         foc = foc,
         foc_rank = if (!is.null(foc_solution)) foc_solution$rank else NA_integer_,
         foc_condition_number = if (!is.null(foc_solution)) {
             foc_solution$condition_number
         } else NA_real_,
         foc_condition_limit = if (!is.null(foc_solution)) {
             foc_solution$condition_limit
         } else NA_real_)
}

.trade_synthetic_other_logit_solution <- function(spec, shares, costs, owner,
                                                   ref, margin, tariff, dots) {
    n <- length(shares)
    if (spec$conduct == "bargaining") {
        power <- dots$bargpowerPre
        if (is.null(power)) power <- rep(0.5, n)
        if (!is.numeric(power) || length(power) != n ||
            any(!is.finite(power)) || any(power <= 0 | power >= 1)) {
            stop("trade bargaining observed mode requires an explicit all-product 'bargpowerPre' vector in (0, 1)")
        }
    }
    if (any(!is.finite(tariff)) || any(tariff < 0 | tariff >= 1)) {
        stop("'tariffPre' must contain finite rates in [0, 1)")
    }
    reference_price <- costs[ref] / (1 - margin)
    effective_reference_markup <- reference_price - costs[ref] / (1 - tariff[ref])
    if (!is.finite(effective_reference_markup) ||
        effective_reference_markup <= 0) {
        stop("reference margin must exceed the reference tariff rate")
    }
    probe <- do.call(specify, c(list(
        demand = "logit", conduct = spec$conduct, policy = "tariff",
        prices = rep(reference_price, n),
        parameters = list(alpha = -1, meanval = log(shares / shares[ref])),
        owner = owner, priceOutside = reference_price,
        tariffPre = tariff), dots))
    coefficient <- as.numeric(calcMargins(probe@model, TRUE, level = TRUE))
    if (length(coefficient) != n || any(!is.finite(coefficient)) ||
        any(coefficient <= 0)) {
        stop("native trade conduct method did not yield finite positive unit-slope markups")
    }
    alpha <- -coefficient[ref] / effective_reference_markup
    markups <- coefficient / (-alpha)
    prices <- costs / (1 - tariff) + markups
    meanval <- log(shares / shares[ref])
    if (spec$conduct != "auction2nd") {
        meanval <- meanval - alpha * (prices - prices[ref])
    }
    meanval[ref] <- 0
    list(alpha = alpha, meanval = meanval, prices = prices,
         markups = markups)
}

.trade_synthetic_ces_solution <- function(spec, shares, costs, owner,
                                          ref, margin, tariff) {
    n <- length(shares)
    if (length(tariff) != n || any(!is.finite(tariff)) ||
        any(tariff < 0 | tariff >= 1)) {
        stop("'tariffPre' must contain finite rates in [0, 1)")
    }
    reference_price <- costs[ref] / (1 - margin)
    demand_margin <- (margin - tariff[ref]) / (1 - tariff[ref])
    if (demand_margin <= 0) {
        stop("reference margin must exceed the reference tariff rate")
    }
    probe <- function(gamma) {
        fit <- specify("ces", spec$conduct, policy = "tariff",
            prices = rep(reference_price, n),
            parameters = list(gamma = gamma, alpha = 0,
                              meanval = shares / shares[ref]),
            owner = owner, priceOutside = reference_price,
            tariffPre = tariff)
        as.numeric(calcMargins(fit@model, TRUE))
    }
    ## The active reference product is its own firm. Its policy-adjusted
    ## demand margin therefore identifies curvature without a search bound.
    gamma <- if (spec$conduct == "moncom") {
        1 / demand_margin
    } else {
        (1 / demand_margin - shares[ref]) / (1 - shares[ref])
    }
    if (!is.finite(gamma) || gamma <= 1) {
        stop("tariff CES reference margin implies non-finite or inadmissible curvature")
    }
    all_margin <- probe(gamma)
    curvature_residual <- all_margin[ref] - demand_margin
    if (any(!is.finite(all_margin)) ||
        any(all_margin <= 0 | all_margin >= 1) ||
        !is.finite(curvature_residual) ||
        abs(curvature_residual) > 1e-8) {
        stop("tariff CES ownership and policy imply inadmissible demand margins")
    }
    prices <- costs / (1 - tariff) / (1 - all_margin)
    if (any(!is.finite(prices)) || any(prices <= 0)) {
        stop("tariff CES implies invalid consumer prices")
    }
    meanval <- (shares / shares[ref]) *
        (prices / prices[ref])^(gamma - 1)
    list(gamma = gamma, meanval = meanval, prices = prices,
         markups = prices * all_margin,
         curvature_residual = curvature_residual,
         curvature_status = "identified-closed-form",
         curvature_sensitivity = -1 /
             (demand_margin^2 * if (spec$conduct == "moncom") 1
              else (1 - shares[ref])))
}

.trade_synthetic_stackelberg <- function(spec, market, tariff, leaders,
                                          conduct, dots) {
    shares <- market$observed$unconditional_shares
    costs <- market$observed$costs
    owner <- as.character(market$products$firm_id)
    n <- length(shares)
    ref <- market$design$reference_product
    if (!is.numeric(tariff) || length(tariff) != n ||
        any(!is.finite(tariff)) || any(tariff < 0 | tariff >= 1)) {
        stop("Stackelberg observed mode requires finite tariff rates in [0, 1)")
    }
    ## Tariff reuse represents costs as c/(1-tau). The native game has the
    ## same baseline FOCs only if retention is constant within each firm.
    if (any(vapply(split(tariff, owner), function(x)
        any(abs(x - x[1L]) > 1e-10), logical(1)))) {
        stop("Stackelberg tariff reuse requires one baseline tariff rate per firm")
    }
    firms <- unique(owner)
    if (is.null(leaders)) {
        firm_shares <- vapply(firms, function(f) sum(shares[owner == f]),
                              numeric(1))
        leaders <- firms[order(-firm_shares, seq_along(firms))][seq_len(
            if (market$design$n_firms > 3L) 3L else 1L)]
    }
    if (!is.character(leaders) && !is.factor(leaders) &&
        !is.numeric(leaders)) {
        stop("'leadersPre' must contain firm IDs from the observed ownership design")
    }
    leaders <- as.character(leaders)
    if (!length(leaders) || anyNA(leaders) || anyDuplicated(leaders) ||
        any(!leaders %in% firms)) {
        stop("'leadersPre' must contain distinct firm IDs from the observed ownership design")
    }
    if (length(market$observed$passive_outside_share) != 1L ||
        market$observed$passive_outside_share <= 0) {
        stop("Stackelberg observed mode requires a positive passive outside share")
    }
    ref_price <- costs[ref] / (1 - market$observed$reference_margin)
    effective_costs <- costs / (1 - tariff)
    ref_markup <- ref_price - effective_costs[ref]
    if (!is.finite(ref_markup) || ref_markup <= 0) {
        stop("the reference margin must exceed the reference tariff rate to identify a positive Stackelberg demand markup")
    }
    if (spec$demand == "logit") {
        h <- utils::getFromNamespace(".sk_h", "coordination")(
            shares, owner, leaders, conduct, rep(TRUE, n))
        scale <- h[ref] / ref_markup
        if (!is.finite(scale) || scale <= 0) {
            stop("the reference margin does not identify a positive Stackelberg Logit price coefficient")
        }
        prices <- effective_costs + h / scale
        recovered <- list(alpha = -scale, leader_markup_coefficient = h,
                          curvature_status = "identified-closed-form")
    } else {
        multiplier <- utils::getFromNamespace(".ces_multipliers", "coordination")
        target <- ref_markup / ref_price
        gap <- function(log_excess) {
            gamma <- 1 + exp(log_excess)
            value <- try(multiplier(shares, owner, leaders, gamma, conduct,
                                    rep(TRUE, n))$margins[ref], silent = TRUE)
            if (inherits(value, "try-error") || !is.finite(value)) NA_real_
            else value - target
        }
        if (conduct == "bertrand" && !owner[ref] %in% leaders) {
            ## The follower margin is 1/[1+(gamma-1)(1-R_r)]. It identifies
            ## gamma without a numerical bound, even for a tiny margin.
            gamma <- 1 + (1 / target - 1) / (1 - shares[ref])
            curvature_status <- "identified-closed-form"
            curvature_sensitivity <- -1 /
                (target^2 * (1 - shares[ref]))
        } else {
            upper <- min(1e15, max(1e6, 100 / target))
            grid <- seq(log(1e-14), log(upper - 1), length.out = 301L)
            values <- vapply(grid, gap, numeric(1))
            exact <- which(is.finite(values) &
                abs(values) <= 64 * .Machine$double.eps * target)
            candidates <- which(is.finite(values[-length(values)]) &
                is.finite(values[-1L]) &
                values[-length(values)] * values[-1L] < 0)
            if (length(candidates) == 1L) {
                root <- stats::uniroot(gap, grid[candidates + 0:1],
                                       tol = 1e-11)
            } else if (length(exact) == 1L && !length(candidates)) {
                root <- list(root = grid[exact])
            } else if (!length(candidates) && !length(exact)) {
                stop("the Stackelberg CES reference margin has no admissible curvature within the numerical gamma search domain (1, ",
                     format(upper), "]")
            } else {
                stop("the Stackelberg CES reference margin does not uniquely identify admissible curvature gamma > 1")
            }
            gamma <- 1 + exp(root$root)
            curvature_status <- "identified-root"
            curvature_sensitivity <- NA_real_
        }
        if (!is.finite(gamma) || gamma <= 1) {
            stop("the Stackelberg CES reference margin implies inadmissible curvature gamma")
        }
        demand_margins <- multiplier(shares, owner, leaders, gamma,
            conduct, rep(TRUE, n))$margins
        if (any(!is.finite(demand_margins)) ||
            any(demand_margins <= 0 | demand_margins >= 1)) {
            stop("Stackelberg CES implies inadmissible product margins")
        }
        prices <- effective_costs / (1 - demand_margins)
        recovered <- list(gamma = gamma,
                          curvature_status = curvature_status,
                          curvature_residual = demand_margins[ref] - target,
                          curvature_sensitivity = curvature_sensitivity)
    }
    if (any(!is.finite(prices)) || any(prices <= 0)) {
        stop("Stackelberg observed inputs imply non-finite or non-positive equilibrium prices")
    }
    if (is.null(dots$control.equ)) {
        dots$control.equ <- list(implicitCheck = FALSE)
    }
    source <- do.call(coordination::stackelberg, c(list(
        prices = prices, shares = shares, margins = rep(NA_real_, n),
        ownerPre = owner, leadersPre = leaders, demand = spec$demand,
        conduct = conduct, insideSize = 1, priceOutside = ref_price,
        alpha = if (spec$demand == "logit") -recovered$alpha else NULL,
        gamma = if (spec$demand == "ces") recovered$gamma else NULL), dots))
    native_foc <- coordination::stackelberg_residuals(source)
    if (!is.finite(native_foc$maxNormalized) ||
        native_foc$maxNormalized > 1e-7 ||
        max(abs(source@pricePre - prices)) > 1e-6 ||
        max(abs(source@mcPre - effective_costs)) > 1e-6) {
        stop("native Stackelberg baseline failed equilibrium, price, or supplied-cost validation")
    }
    fit <- as_trade_fit(source, tariffPre = tariff,
        cost_basis = "effective", margin_basis = "net_revenue")
    recovered$leadersPre <- leaders
    recovered$stackelberg_conduct <- conduct
    recovered$native_foc_residual <- native_foc$maxNormalized
    recovered$solver_status <- source@diagnostics$solverStatus
    list(fit = fit, recovered = recovered)
}

.trade_synthetic_quota_fit <- function(spec, shares, prices, owner,
                                        policy_pre, reference_price,
                                        recovered, dots) {
    ## QuotaLogit currently has no supplied-parameter constructor. Use its
    ## native constructor as a typed container, then replace only the demand
    ## parameters with the full active-product FOC solution. The subsequent
    ## cost and price calculations remain the registered trade/antitrust
    ## methods.
    n <- length(shares)
    margins <- recovered$markups / prices
    quota_args <- c(
        list(demand = "logit", prices = prices, quantities = shares,
             margins = margins, owner = owner,
             quotaPre = policy_pre, quotaPost = policy_pre,
             priceOutside = reference_price,
             labels = paste0("Prod", seq_len(n))),
        dots
    )
    captured <- .trade_capture_conditions(do.call(bertrand_quota, quota_args))
    model <- captured$value
    model@slopes <- recovered[c("alpha", "meanval")]
    model@normIndex <- n
    model@priceOutside <- reference_price
    model@pricePre <- prices
    model@mcPre <- calcMC(model, preMerger = TRUE)
    model@mcPost <- calcMC(model, preMerger = FALSE)
    model@pricePost <- calcPrices(model, preMerger = FALSE,
                                  subset = rep(TRUE, n))
    arguments <- c(
        list(prices = prices, quantities = shares, margins = margins,
             owner = owner, quotaPre = policy_pre, quotaPost = policy_pre,
             priceOutside = reference_price, demand = "logit"),
        dots
    )
    .trade_fit(
        spec, model, arguments, captured, route = "synthetic_observed",
        calibration_args = arguments
    )
}

.trade_synthetic_complete_parameters <- function(spec, parameters, shares,
                                                 prices, reference_product) {
    if (!is.list(parameters) || is.null(names(parameters)) ||
        any(!nzchar(names(parameters)))) {
        stop("'parameters' must be a named list in primitives mode")
    }
    result <- parameters
    alpha <- result$alpha %||% result$alphaMean %||% result$alpha_mean
    if (spec$demand == "logit") {
        if (is.null(alpha) || length(alpha) != 1L || !is.finite(alpha) ||
            alpha >= 0) {
            stop("primitives mode requires a finite negative 'alpha' for logit")
        }
        result$alpha <- as.numeric(alpha)
        if (is.null(result$meanval)) {
            result$meanval <- log(shares / shares[reference_product]) -
                result$alpha * (prices - prices[reference_product])
            result$meanval[reference_product] <- 0
        }
        if (length(result$meanval) != length(shares) ||
            any(!is.finite(result$meanval))) {
            stop("'meanval' must be a finite vector with one element per product")
        }
        result$meanval <- result$meanval - result$meanval[reference_product]
    }
    if (spec$demand == "ces") {
        gamma <- result$gamma
        if (is.null(gamma) || length(gamma) != 1L || !is.finite(gamma)) {
            stop("primitives mode requires a finite 'gamma' for ces")
        }
        if (is.null(result$meanval)) {
            result$meanval <- (shares * prices^gamma) /
                (shares[reference_product] * prices[reference_product]^gamma)
            result$meanval[reference_product] <- 1
        }
        if (length(result$meanval) != length(shares) ||
            any(!is.finite(result$meanval)) ||
            result$meanval[reference_product] == 0) {
            stop("'meanval' must be finite and non-zero at the reference product")
        }
        result$meanval <- result$meanval / result$meanval[reference_product]
    }
    result
}

.trade_synthetic_parameter_error <- function(truth, recovered) {
    if (!length(truth) || !length(recovered)) return(list())
    result <- list()
    for (name in names(truth)) {
        value <- recovered[[name]]
        if (is.null(value) && is.list(recovered$slopes)) {
            value <- recovered$slopes[[name]]
        }
        if (!is.null(value) && is.numeric(truth[[name]]) &&
            is.numeric(value) && length(truth[[name]]) == length(value)) {
            result[[name]] <- unname(value - truth[[name]])
        }
    }
    result
}

.trade_synthetic_attach <- function(fit, market, mode, reference_markup,
                                    policy, policy_pre, parameter_truth,
                                    foc_diagnostics = list()) {
    if (mode == "observed") {
        prices <- as.numeric(fit@model@pricePre)
        costs <- market$observed$costs
        effective_costs <- as.numeric(fit@model@mcPre)
        retention <- if (policy == "tariff") 1 - policy_pre else rep(1, length(costs))
        physical_costs <- effective_costs * retention
        target_shares <- if (fit@spec$conduct == "stackelberg") {
            market$observed$unconditional_shares
        } else market$shares
        shares <- as.numeric(calcShares(fit@model, TRUE,
            revenue = fit@spec$demand == "ces"))
        margin_native <- as.numeric(calcMargins(fit@model, TRUE, level = TRUE))
        share_residual <- max(abs(shares - target_shares))
        cost_residual <- max(abs(physical_costs - costs))
        foc_residual <- max(abs(margin_native - (prices - effective_costs)))
        ref <- market$design$reference_product
        reference_residual <- (prices[ref] - costs[ref]) / prices[ref] -
            market$observed$reference_margin
        if (any(!is.finite(c(share_residual, cost_residual, foc_residual,
                             reference_residual))) ||
            max(share_residual, abs(reference_residual)) > 1e-8 ||
            max(cost_residual, foc_residual) > 1e-6) {
            stop("trade observed synthetic realization failed share, physical-cost, reference-margin, or FOC validation: ",
                 paste(signif(c(share_residual, cost_residual, foc_residual,
                                reference_residual), 4), collapse = ", "))
        }
        market$prices <- prices
        market$reference_price <- prices[ref]
        market$costs <- costs
        market$markups <- prices - costs
        market$products$price <- prices
        market$products$cost <- costs
        market$products$markup <- prices - costs
        market$products$margin <- (prices - costs) / prices
        market$observed$prices <- prices
        market$observed$reference_price <- prices[ref]
        market$design$reference_price <- prices[ref]
        market$diagnostics$equilibrium_status <- "verified"
        fit@observed$shares <- market$shares
        fit@observed$unconditional_shares <- target_shares
        fit@observed$passive_outside_share <-
            market$observed$passive_outside_share
        fit@observed$prices <- prices
        fit@observed$costs <- costs
        fit@observed$reference_margin <- market$observed$reference_margin
        fit@observed$policy_pre <- policy_pre
        fit@observed$synthetic_market <- market
        fit@diagnostics$synthetic_market <- market
        fit@diagnostics$synthetic_recovered <- fit@parameters
        fit@diagnostics$synthetic <- list(
            status = "completed", mode = mode, policy = policy,
            policy_pre = policy_pre, target_shares = target_shares,
            conditional_shares = market$shares,
            passive_outside_share = market$observed$passive_outside_share,
            supplied_costs = costs, effective_costs = effective_costs,
            solved_prices = prices, implied_markup = prices - costs,
            implied_margin = (prices - costs) / prices,
            recovered_parameters = fit@parameters,
            reference_margin_residual = reference_residual,
            share_residual = share_residual, cost_residual = cost_residual,
            foc_residual = foc_residual, equilibrium_status = "verified",
            equilibrium_check = TRUE,
            foc_rank = foc_diagnostics$foc_rank,
            foc_condition_number = foc_diagnostics$foc_condition_number,
            curvature_residual = foc_diagnostics$curvature_residual,
            curvature_status = foc_diagnostics$curvature_status,
            curvature_sensitivity =
                foc_diagnostics$curvature_sensitivity,
            leadersPre = foc_diagnostics$leadersPre,
            stackelberg_conduct = foc_diagnostics$stackelberg_conduct,
            native_foc_residual = foc_diagnostics$native_foc_residual,
            solver_status = foc_diagnostics$solver_status,
            identification_status = foc_diagnostics$curvature_status)
        return(fit)
    }
    fit@observed$shares <- market$shares
    fit@observed$quantities <- market$shares
    fit@observed$prices <- market$prices
    fit@observed$owner <- market$products$firm_id
    fit@observed$synthetic_market <- market
    fit@observed$reference_product <- market$design$reference_product
    fit@observed$reference_share <- market$design$outside_share
    fit@observed$reference_price <- market$design$reference_price
    fit@observed$outside_margin <- reference_markup
    fit@observed$markup_units <- "level price difference"
    fit@observed$policy_pre <- policy_pre
    fit@diagnostics$synthetic_market <- market
    fit@diagnostics$synthetic_truth <- parameter_truth
    fit@diagnostics$synthetic_recovered <- fit@parameters
    fit@diagnostics$synthetic_parameter_error <-
        .trade_synthetic_parameter_error(parameter_truth, fit@parameters)
    model <- fit@model
    n <- length(market$shares)
    implied_markup <- tryCatch(
        as.numeric(calcMargins(model, preMerger = TRUE, level = TRUE)),
        error = function(e) rep(NA_real_, n)
    )
    foc <- rep(NA_real_, n)
    foc_residual <- NA_real_
    if (identical(fit@spec$conduct, "bertrand") &&
        methods::is(model, "Logit") &&
        !methods::is(model, "CES") &&
        !methods::is(model, "LogitCournot") &&
        (!methods::is(model, "LogitCap") || methods::is(model, "QuotaLogit")) &&
        !methods::is(model, "LogitNests")) {
        alpha <- model@slopes$alpha
        if (length(alpha) == 1L && is.finite(alpha) &&
            nrow(model@ownerPre) == n && length(implied_markup) == n) {
            revenue <- calcShares(model, preMerger = TRUE, revenue = TRUE)
            elasticity <- elast(model, preMerger = TRUE)
            margin_prop <- implied_markup / as.numeric(model@pricePre)
            out_sign <- if (isTRUE(model@output)) -1 else 1
            foc_matrix <- t(elasticity) * model@ownerPre
            foc <- foc_matrix %*% (margin_prop * revenue) -
                out_sign * revenue * diag(model@ownerPre)
            foc_residual <- max(abs(foc))
        }
    }
    foc_supported <- is.finite(foc_residual)
    synthetic_diagnostics <- list(
        status = if (foc_supported) "completed" else "unavailable",
        mode = mode,
        policy = policy,
        policy_pre = policy_pre,
        reference_product = market$design$reference_product,
        reference_markup = reference_markup,
        markup_units = "level price difference",
        implied_markup = unname(implied_markup),
        foc = unname(foc),
        foc_residual = foc_residual,
        foc_tolerance = 1e-8,
        foc_status = if (foc_supported) "verified" else "unavailable",
        equilibrium_check = if (foc_supported) {
            foc_residual < 1e-8
        } else {
            NA
        }
    )
    if (length(foc_diagnostics)) {
        synthetic_diagnostics$foc_rank <- foc_diagnostics$foc_rank
        synthetic_diagnostics$foc_condition_number <-
            foc_diagnostics$foc_condition_number
        synthetic_diagnostics$foc_condition_limit <-
            foc_diagnostics$foc_condition_limit
    }
    fit@diagnostics$synthetic <- synthetic_diagnostics
    fit
}

#' Generate a model-consistent synthetic trade market
#'
#' `synthetic_market()` is the trade-native fake-market entry point. It draws
#' product shares and ownership through [antitrust::fake_market()], then uses
#' trade's selected policy/model implementation to calibrate the baseline.
#' Structural demand parameters can be difficult to choose directly. Observed
#' mode instead begins with shares, ownership, positive physical marginal
#' costs, a proportional reference margin, and policy state; it solves demand
#' scale and equilibrium consumer prices. This is an intuition experiment, not
#' an empirical data-generating process. For tariffs, physical cost `c` and
#' effective cost `c/(1-tariffPre)` are distinguished explicitly.
#' Tariff CES Bertrand and monopolistic-competition routes use revenue shares
#' and identify CES curvature from the reference margin after accounting for
#' the reference tariff.
#' Logit and CES Stackelberg routes additionally draw or accept a passive
#' outside share and solve native leader/follower equilibrium. They require
#' a common tariff rate within each firm because tariff reuse stores effective
#' costs `c/(1-tariffPre)`.
#'
#' The active reference product is a real product in the ownership map. The
#' `n_firms` argument counts inside firms and the reference firm is additional.
#' `n_products` may be a scalar shared by all inside firms or a vector with one
#' count per firm.
#' For now, a baseline must be either tariff-only or quota-only. Supplying both
#' `tariffPre` and `quotaPre` is rejected explicitly because simultaneous
#' pre-policies are not yet implemented by the trade model registry.
#'
#' @param demand Demand-system name, with `"logit"` as the default.
#' @param supply Supply/conduct name, with `"bertrand"` as the default.
#' @param mode Either `"observed"` or `"primitives"`.
#' @param policy Baseline policy family, `"tariff"` or `"quota"`.
#' @param n_firms Number of inside firms; the reference firm is additional.
#' @param n_products Number of products per inside firm. A scalar is recycled
#' across firms; a vector must have length `n_firms`.
#' @param dirichlet_alpha Positive product-level Dirichlet parameters, one per
#' inside product. If omitted, all shapes equal one.
#' @param outside_beta Positive Beta shape parameters for the reference share.
#' @param shares Optional complete all-product shares summing to one.
#' @param costs Optional complete positive physical marginal-cost vector.
#' @param cost_rule `"common"` or `"uniform"` observed cost design.
#' @param cost_level Positive common cost, default 80.
#' @param cost_range Positive endpoints for heterogeneous uniform costs.
#' @param reference_margin Proportional reference margin `(p_r-c_r)/p_r` in
#'   `(0,1)`. Its price is implied by `p_r=c_r/(1-reference_margin)`.
#' @param passive_outside_share For observed Stackelberg games, a positive
#'   passive outside share. `NULL` draws uniformly from
#'   `passive_outside_range`; supplied `shares` remain conditional on active
#'   products and are scaled by `1-passive_outside_share` for the native game.
#' @param passive_outside_range Endpoints for the passive outside-share draw.
#' @param leadersPre Optional Stackelberg leader firm IDs. By default the
#'   largest firm by aggregate active-product share leads when `n_firms <= 3`;
#'   the three largest lead when `n_firms > 3`. The reference firm is eligible.
#' @param stackelberg_conduct Underlying Stackelberg game, `"bertrand"` or
#'   `"cournot"`; default `"bertrand"`.
#' @param reference_price Positive reference price in primitives mode only.
#' @param prices Optional complete prices in primitives mode only.
#' @param outside_margin Retired observed price-first argument.
#' @param parameters Named model-specific primitives in primitives mode.
#' @param tariffPre Optional tariff-only pre-policy vector.
#' @param quotaPre Optional quota-only pre-policy vector.
#' @param seed Optional explicit integer seed.
#' @param ... Model arguments forwarded to [specify()] or, for Stackelberg,
#'   [coordination::stackelberg()]. Bargaining power `bargpowerPre` defaults
#'   to 0.5 per product and may be overridden.
#' @return A [TradeFit] directly accepted by [simulate()].
#' @examples
#' if (requireNamespace("coordination", quietly = TRUE)) {
#'   fit <- synthetic_market(
#'     supply = "stackelberg", n_firms = 2,
#'     shares = c(0.2, 0.3, 0.5), costs = c(60, 70, 80),
#'     reference_margin = 0.25, passive_outside_share = 0.2,
#'     tariffPre = c(0, 0.1, 0)
#'   )
#'   fit@diagnostics$synthetic$leadersPre
#' }
#' @export
synthetic_market <- function(
    demand = "logit", supply = "bertrand",
    mode = c("observed", "primitives"),
    policy = c("tariff", "quota"),
    n_firms = 3L, n_products = 1L,
    dirichlet_alpha = NULL,
    outside_beta = c(2, 8), shares = NULL,
    costs = NULL, cost_rule = c("common", "uniform"), cost_level = 80,
    cost_range = c(50, 100), reference_margin = NULL,
    reference_price = 100, prices = NULL, outside_margin = NULL,
    parameters = NULL,
    tariffPre = NULL, quotaPre = NULL, seed = NULL,
    passive_outside_share = NULL,
    passive_outside_range = c(0.1, 0.5),
    leadersPre = NULL, stackelberg_conduct = "bertrand", ...) {
    mode <- match.arg(mode)
    policy <- match.arg(policy)
    if (mode == "observed" && (!missing(reference_price) ||
        !is.null(prices) || !is.null(outside_margin))) {
        stop("observed mode uses costs and proportional 'reference_margin'; price-first arguments are retired")
    }
    if (mode == "primitives" &&
        (length(reference_price) != 1L || !is.numeric(reference_price) ||
        !is.finite(reference_price) || reference_price <= 0)) {
        stop("'reference_price' must be a single strictly positive number")
    }
    if (!is.numeric(n_firms) || length(n_firms) != 1L ||
        n_firms != as.integer(n_firms) || n_firms < 1L) {
        stop("'n_firms' must be a positive integer")
    }
    n_firms <- as.integer(n_firms)
    n_products_input <- as.numeric(n_products)
    if (length(n_products_input) != 1L &&
        length(n_products_input) != n_firms) {
        stop("'n_products' must be a positive integer scalar or a vector of length n_firms")
    }
    if (any(!is.finite(n_products_input)) ||
        any(n_products_input != as.integer(n_products_input)) ||
        any(n_products_input < 1)) {
        stop("'n_products' must contain positive integers")
    }
    n_products <- if (length(n_products_input) == 1L) {
        as.integer(n_products_input)
    } else {
        as.integer(n_products_input)
    }
    products_per_firm <- if (length(n_products_input) == 1L) {
        rep(as.integer(n_products_input), n_firms)
    } else {
        as.integer(n_products_input)
    }
    if (sum(products_per_firm) > 2147483646) {
        stop("the number of inside products is too large")
    }
    n <- as.integer(sum(products_per_firm) + 1L)
    if (!is.null(dirichlet_alpha) &&
        (!is.numeric(dirichlet_alpha) || length(dirichlet_alpha) != n - 1L ||
         any(!is.finite(dirichlet_alpha)) || any(dirichlet_alpha <= 0))) {
        stop("'dirichlet_alpha' must be a finite, strictly positive vector of length ",
             n - 1L)
    }
    if (!is.numeric(outside_beta) || length(outside_beta) != 2L ||
        any(!is.finite(outside_beta)) || any(outside_beta <= 0)) {
        stop("'outside_beta' must be a finite, strictly positive vector of length 2")
    }
    if (mode == "primitives" && is.null(parameters)) {
        stop("primitives mode requires a named 'parameters' list")
    }
    if (mode == "primitives" && !is.null(prices) &&
        (!is.numeric(prices) || length(prices) != n ||
         any(!is.finite(prices)) || any(prices <= 0) ||
         !isTRUE(all.equal(unname(prices[n]), unname(reference_price))))) {
        stop("'prices' must be a finite, strictly positive all-product vector whose reference price equals 'reference_price'")
    }
    if (!is.null(tariffPre) && !is.null(quotaPre)) {
        stop("simultaneous tariff and quota pre-policies are not supported; supply exactly one policy family")
    }
    if (policy == "tariff" && !is.null(quotaPre)) {
        stop("'quotaPre' cannot be supplied with policy = 'tariff'; simultaneous tariff and quota pre-policies are not supported")
    }
    if (policy == "quota" && !is.null(tariffPre)) {
        stop("'tariffPre' cannot be supplied with policy = 'quota'; simultaneous tariff and quota pre-policies are not supported")
    }
    if (policy == "quota" && mode == "primitives") {
        stop("primitives mode is not available for quota baselines until trade supports quota specify(); use mode = 'observed'")
    }
    dots <- list(...)
    duplicate <- intersect(names(dots), c("prices", "costs", "quantities", "margins",
                                           "owner", "parameters", "demand",
                                           "supply", "conduct", "policy",
                                           "tariffPre", "quotaPre", "priceOutside",
                                           "labels"))
    if (length(duplicate)) {
        stop("argument(s) supplied more than once: ",
             paste(duplicate, collapse = ", "))
    }

    spec <- model_spec(demand, supply, policy = policy)
    if (spec$conduct != "stackelberg" &&
        (!is.null(leadersPre) || !missing(stackelberg_conduct) ||
         !is.null(passive_outside_share) ||
         !missing(passive_outside_range))) {
        stop("leader and passive-outside inputs are only supported for observed Stackelberg games")
    }
    if (spec$conduct == "stackelberg" && mode != "observed") {
        stop("trade Stackelberg synthetic markets currently require mode = 'observed'")
    }
    if (spec$conduct == "stackelberg") {
        stackelberg_conduct <- match.arg(stackelberg_conduct,
                                         c("bertrand", "cournot"))
        duplicate_stack <- intersect(names(dots), c(
            "ownerPre", "leadersPre", "shares", "demand", "output",
            "insideSize", "normIndex", "priceOutside", "alpha", "gamma",
            "revenueRetentionPre", "revenueRetentionPost"))
        if (length(duplicate_stack)) {
            stop("Stackelberg observed arguments are fixed by the design: ",
                 paste(duplicate_stack, collapse = ", "))
        }
    }
    if (mode == "observed" && identical(
        .trade_registry_entry(spec)$observed_synthetic, "unsupported")) {
        stop("observed synthetic mode is underidentified or unsupported for ",
             spec$id, "; it requires ",
             .trade_synthetic_missing_anchors(spec))
    }
    design <- if (mode == "observed") {
        antitrust::fake_market(
            mode = "observed", n_firms = n_firms, n_products = n_products,
            dirichlet_alpha = dirichlet_alpha, outside_beta = outside_beta,
            shares = shares, costs = costs, cost_rule = cost_rule,
            cost_level = cost_level, cost_range = cost_range,
            reference_margin = reference_margin,
            passive_outside_share = if (spec$conduct == "stackelberg") {
                passive_outside_share
            } else 0,
            passive_outside_range = passive_outside_range, seed = seed)
    } else {
        antitrust::fake_market(
            mode = "primitives", n_firms = n_firms, n_products = n_products,
            dirichlet_alpha = dirichlet_alpha, outside_beta = outside_beta,
            shares = shares, prices = prices, price_level = reference_price,
            reference_price = reference_price, parameters = parameters,
            seed = seed)
    }
    shares <- design$shares
    prices <- design$prices
    owner <- design$products$firm_id
    ref <- design$design$reference_product
    markup <- NA_real_
    policy_pre <- if (policy == "tariff") {
        if (is.null(tariffPre)) rep(0, n) else tariffPre
    } else {
        if (is.null(quotaPre)) rep(Inf, n) else quotaPre
    }
    if (length(policy_pre) != n || anyNA(policy_pre)) {
        stop("the selected pre-policy vector must have one non-missing value per product")
    }
    if (policy == "quota" && any(is.finite(policy_pre) & policy_pre < 1)) {
        stop("finite pre-merger quotas below one would ration the supplied baseline shares; quota synthetic markets currently require non-binding quotas (values >= 1 or Inf)")
    }

    if (mode == "observed") {
        if (policy == "tariff" && spec$conduct == "stackelberg") {
            realized <- .trade_synthetic_stackelberg(
                spec, design, policy_pre, leadersPre,
                stackelberg_conduct, dots)
            prices <- as.numeric(realized$fit@model@pricePre)
            return(.trade_synthetic_attach(realized$fit, design, mode,
                prices[ref] - design$observed$costs[ref], policy,
                policy_pre, parameter_truth = list(),
                foc_diagnostics = realized$recovered))
        }
        if (policy == "tariff" && spec$demand == "ces" &&
            spec$conduct %in% c("bertrand", "moncom")) {
            recovered <- .trade_synthetic_ces_solution(
                spec, shares, design$observed$costs, owner, ref,
                design$observed$reference_margin, policy_pre)
            prices <- recovered$prices
            reference_price <- prices[ref]
            markup <- prices[ref] - design$observed$costs[ref]
            fit <- do.call(specify, c(list(
                demand = "ces", conduct = spec$conduct,
                policy = "tariff", prices = prices,
                parameters = c(recovered[c("gamma", "meanval")],
                               list(alpha = 0)),
                owner = owner, priceOutside = reference_price,
                tariffPre = policy_pre), dots))
            return(.trade_synthetic_attach(fit, design, mode, markup,
                policy, policy_pre, parameter_truth = list(),
                foc_diagnostics = recovered))
        }
        if (policy == "tariff" && spec$conduct != "bertrand") {
            if (spec$conduct == "bargaining" && is.null(dots$bargpowerPre)) {
                dots$bargpowerPre <- rep(0.5, n)
            }
            recovered <- .trade_synthetic_other_logit_solution(
                spec, shares, design$observed$costs, owner, ref,
                design$observed$reference_margin, policy_pre, dots)
            prices <- recovered$prices
            reference_price <- prices[ref]
            markup <- prices[ref] - design$observed$costs[ref]
            fit <- do.call(specify, c(list(
                demand = spec$demand, conduct = spec$conduct,
                variant = spec$variant, policy = spec$policy,
                prices = prices,
                parameters = recovered[c("alpha", "meanval")],
                owner = owner, priceOutside = reference_price,
                tariffPre = policy_pre), dots))
            return(.trade_synthetic_attach(
                fit, design, mode, markup, policy, policy_pre,
                parameter_truth = list(), foc_diagnostics = recovered))
        }
        ## TariffLogit is an ALM descendant in trade and its legacy calibrator
        ## identifies both alpha and a passive outside share. The synthetic
        ## reference product is active instead, so recover alpha from the full
        ## active-product FOC system and then use trade's supplied-parameter
        ## constructor. This keeps the economic inversion in trade.
        if (spec$demand == "logit" && spec$conduct == "bertrand" &&
            policy == "tariff") {
            recovered <- .trade_synthetic_logit_parameters(
                shares, design$observed$costs, owner, ref,
                design$observed$reference_margin,
                tariff = policy_pre
            )
            prices <- recovered$prices
            reference_price <- prices[ref]
            markup <- prices[ref] - design$observed$costs[ref]
            args <- c(
                list(demand = spec$demand, conduct = spec$conduct,
                     variant = spec$variant, policy = spec$policy,
                     prices = prices,
                     parameters = recovered[c("alpha", "meanval")],
                     owner = owner, priceOutside = reference_price,
                     tariffPre = policy_pre), dots
            )
            fit <- do.call(specify, args)
            fit@diagnostics$parameter_source <-
                "observed-reference-markup-foc"
            return(.trade_synthetic_attach(
                fit, design, mode, markup, policy, policy_pre,
                parameter_truth = list(), foc_diagnostics = recovered
            ))
        }
        if (spec$demand == "logit" && spec$conduct == "bertrand" &&
            policy == "quota") {
            recovered <- .trade_synthetic_logit_parameters(
                shares, design$observed$costs, owner, ref,
                design$observed$reference_margin
            )
            prices <- recovered$prices
            reference_price <- prices[ref]
            markup <- prices[ref] - design$observed$costs[ref]
            fit <- .trade_synthetic_quota_fit(
                spec, shares, prices, owner, policy_pre, reference_price,
                recovered, dots
            )
            fit@diagnostics$parameter_source <-
                "observed-reference-markup-foc"
            return(.trade_synthetic_attach(
                fit, design, mode, markup, policy, policy_pre,
                parameter_truth = list(), foc_diagnostics = recovered
            ))
        }
    }

    parameters <- .trade_synthetic_complete_parameters(
        spec, parameters, shares, prices, ref
    )
    args <- c(
        list(demand = spec$demand, conduct = spec$conduct,
             variant = spec$variant, policy = spec$policy,
             prices = prices, parameters = parameters, owner = owner,
             priceOutside = reference_price),
        stats::setNames(list(policy_pre), paste0(policy, "Pre")), dots
    )
    fit <- do.call(specify, args)
    .trade_synthetic_attach(
        fit, design, mode, NA_real_, policy, policy_pre, parameters
    )
}
