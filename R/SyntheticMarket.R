## Trade-native synthetic markets. The neutral share/ownership design comes
## from antitrust, while policy calibration and equilibrium state remain in
## trade's registered model implementations.

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

.trade_synthetic_logit_parameters <- function(shares, prices, owner,
                                              reference_product, markup,
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
            prices = prices,
            parameters = list(alpha = -1, meanval = meanval_probe),
            owner = owner, priceOutside = prices[reference_product],
            tariffPre = tariff
        )
        probe_markup <- as.numeric(calcMargins(
            probe@model, preMerger = TRUE, level = TRUE
        ))
        if (length(probe_markup) != n || !all(is.finite(probe_markup))) {
            stop("the native TariffLogit method produced non-finite probe markups")
        }
        alpha <- -unname(probe_markup[reference_product]) / markup
        markups <- probe_markup * (-1 / alpha)
        effective_owner <- probe@model@ownerPre
        foc <- rep(NA_real_, n)
    } else {
        effective_owner <- owner_matrix
        foc_solution <- .trade_synthetic_foc_solution(shares, effective_owner)
        G <- foc_solution$G
        z <- foc_solution$z
        alpha <- -unname(z[reference_product]) / markup
        markups <- -z / alpha
        foc <- unname(shares + G %*% (alpha * markups))
    }
    if (!is.finite(alpha) || alpha >= 0) {
        stop("the reference markup does not identify a finite negative Logit price coefficient")
    }
    meanval <- log(shares / shares[reference_product]) -
        alpha * (prices - prices[reference_product])
    meanval[reference_product] <- 0
    list(alpha = alpha, meanval = meanval, markups = markups,
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
    if (methods::is(model, "Logit") &&
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
    synthetic_diagnostics <- list(
        status = "completed",
        mode = mode,
        policy = policy,
        policy_pre = policy_pre,
        reference_product = market$design$reference_product,
        reference_markup = reference_markup,
        markup_units = "level price difference",
        implied_markup = unname(implied_markup),
        foc = unname(foc),
        foc_residual = foc_residual,
        foc_tolerance = 1e-8
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
#' trade's selected policy/model implementation to calibrate or specify the
#' baseline. Prices and margins are not independent random draws: the active
#' reference product supplies the positive price normalization and one level
#' markup, while trade recovers the remaining model state.
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
#' @param reference_price A positive level price for the reference product.
#' @param prices Optional complete positive price vector ending at
#'   `reference_price`.
#' @param outside_margin Optional level reference-product markup in observed
#'   mode. Otherwise the open numerical implementation of `U(0, 100)` is used.
#' @param parameters Named model-specific primitives in primitives mode.
#' @param tariffPre Optional tariff-only pre-policy vector.
#' @param quotaPre Optional quota-only pre-policy vector.
#' @param seed Optional explicit integer seed.
#' @param ... Additional arguments forwarded to [calibrate()] or [specify()].
#' @return A [TradeFit] directly accepted by [simulate()].
#' @export
synthetic_market <- function(
    demand = "logit", supply = "bertrand",
    mode = c("observed", "primitives"),
    policy = c("tariff", "quota"),
    n_firms = 3L, n_products = 1L,
    dirichlet_alpha = NULL,
    outside_beta = c(2, 8), reference_price = 100,
    prices = NULL, outside_margin = NULL, parameters = NULL,
    tariffPre = NULL, quotaPre = NULL, seed = NULL, ...) {
    mode <- match.arg(mode)
    policy <- match.arg(policy)
    if (length(reference_price) != 1L || !is.numeric(reference_price) ||
        !is.finite(reference_price) || reference_price <= 0) {
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
    if (!is.null(outside_margin) &&
        (length(outside_margin) != 1L || !is.numeric(outside_margin) ||
         !is.finite(outside_margin) || outside_margin <= 0 ||
         outside_margin >= 100)) {
        stop("'outside_margin' must lie strictly inside the level support (0, 100)")
    }
    if (mode == "primitives" && is.null(parameters)) {
        stop("primitives mode requires a named 'parameters' list")
    }
    if (!is.null(prices) &&
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
    duplicate <- intersect(names(dots), c("prices", "quantities", "margins",
                                           "owner", "parameters", "demand",
                                           "supply", "conduct", "policy",
                                           "tariffPre", "quotaPre", "priceOutside",
                                           "labels"))
    if (length(duplicate)) {
        stop("argument(s) supplied more than once: ",
             paste(duplicate, collapse = ", "))
    }

    spec <- model_spec(demand, supply, policy = policy)
    design <- antitrust::fake_market(
        mode = if (mode == "observed") "observed" else "primitives",
        n_firms = n_firms, n_products = n_products,
        dirichlet_alpha = dirichlet_alpha, outside_beta = outside_beta,
        prices = prices, price_level = reference_price,
        reference_price = reference_price,
        outside_margin = if (mode == "observed") outside_margin else NULL,
        parameters = if (mode == "primitives") parameters else list(),
        seed = seed
    )
    shares <- design$shares
    prices <- design$prices
    owner <- design$products$firm_id
    ref <- design$design$reference_product
    markup <- if (mode == "observed") design$observed$outside_margin else NA_real_
    if (mode == "observed" && markup >= reference_price) {
        stop("the reference markup implies a non-positive reference cost; use a larger 'reference_price' or supply a smaller 'outside_margin'")
    }
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

    level_conduct <- spec$conduct %in% c("auction2nd", "bargaining")
    if (mode == "observed") {
        ## TariffLogit is an ALM descendant in trade and its legacy calibrator
        ## identifies both alpha and a passive outside share. The synthetic
        ## reference product is active instead, so recover alpha from the full
        ## active-product FOC system and then use trade's supplied-parameter
        ## constructor. This keeps the economic inversion in trade.
        if (spec$demand == "logit" && spec$conduct == "bertrand" &&
            policy == "tariff") {
            recovered <- .trade_synthetic_logit_parameters(
                shares, prices, owner, ref, markup,
                tariff = policy_pre
            )
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
                shares, prices, owner, ref, markup
            )
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
        margins <- rep(NA_real_, n)
        margins[ref] <- if (level_conduct) markup else markup / reference_price
        args <- c(
            list(demand = spec$demand, conduct = spec$conduct,
                 variant = spec$variant, policy = spec$policy,
                 prices = prices, quantities = shares, margins = margins,
                 owner = owner),
            stats::setNames(list(policy_pre), paste0(policy, "Pre")), dots
        )
        fit <- do.call(calibrate, args)
        return(.trade_synthetic_attach(
            fit, design, mode, markup, policy, policy_pre, list()
        ))
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
