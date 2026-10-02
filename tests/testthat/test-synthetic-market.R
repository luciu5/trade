## Migration parity A: physical costs and policy FOCs survive trade realization.

test_that("tariff Logit preserves physical costs and solves its independent FOC", {
    s <- c(.2, .1, .15, .25, .3)
    costs <- c(40, 50, 60, 70, 80)
    tariff <- c(.05, .1, .08, .12, .1)
    fit <- synthetic_market(n_firms = 2, n_products = 2, shares = s,
        costs = costs, reference_margin = .3, tariffPre = tariff, seed = 7)
    market <- fit@diagnostics$synthetic_market
    prices <- market$prices
    alpha <- fit@model@slopes$alpha
    D <- diag(s) - tcrossprod(s)
    retention <- 1 - tariff
    foc <- retention * s + t(market$ownership * (alpha * D)) %*%
        (retention * prices - costs)
    expect_equal(market$costs, costs)
    expect_equal(unname(fit@model@mcPre) * retention, costs,
                 tolerance = 1e-8)
    expect_equal(unname(calcShares(fit@model, TRUE)), s,
                 tolerance = 1e-10)
    expect_lt(max(abs(foc)), 1e-9)
    expect_lt(abs((prices[5] - costs[5]) / prices[5] - .3), 1e-12)
    expect_true(all(is.finite(prices) & prices > 0))
})

test_that("nonbinding quota Logit retains costs and prices", {
    costs <- c(50, 60, 70, 80, 90)
    fit <- synthetic_market(policy = "quota", n_firms = 2,
        n_products = 2, shares = c(.2, .1, .15, .25, .3),
        costs = costs, reference_margin = .2,
        quotaPre = c(2, 2, 2, 2, Inf), seed = 7)
    market <- fit@diagnostics$synthetic_market
    expect_equal(market$costs, costs)
    expect_equal(unname(fit@model@mcPre), costs, tolerance = 1e-8)
    expect_lt(fit@diagnostics$synthetic$share_residual, 1e-9)
    expect_lt(fit@diagnostics$synthetic$foc_residual, 1e-9)
})

test_that("trade conduct alternatives obey native equations with tariffs", {
    s <- c(.2, .1, .15, .25, .3)
    costs <- c(50, 60, 70, 80, 90)
    for (conduct in c("moncom", "auction2nd", "bargaining")) {
        fit <- synthetic_market(supply = conduct, n_firms = 2,
            n_products = 2, shares = s, costs = costs,
            reference_margin = .3, tariffPre = rep(.1, 5), seed = 7)
        market <- fit@diagnostics$synthetic_market
        expect_equal(market$costs, costs, info = conduct)
        expect_lt(fit@diagnostics$synthetic$share_residual, 1e-9)
        expect_lt(fit@diagnostics$synthetic$foc_residual, 1e-9)
        expect_lt(fit@diagnostics$synthetic$cost_residual, 1e-9)
        if (conduct == "bargaining") {
            expect_equal(fit@model@bargpowerPre, rep(.5, 5))
        }
    }
})

test_that("tariff CES uses revenue shares and its own policy-weighted FOC", {
    s <- c(.2, .1, .15, .25, .3)
    costs <- c(40, 50, 60, 70, 80)
    tariff <- c(.02, .05, .03, .06, .1)
    for (conduct in c("bertrand", "moncom")) {
        fit <- synthetic_market(demand = "ces", supply = conduct,
            n_firms = 2, n_products = 2, shares = s, costs = costs,
            reference_margin = .25, tariffPre = tariff, seed = 7)
        market <- fit@diagnostics$synthetic_market
        prices <- market$prices
        gamma <- fit@model@slopes$gamma
        E <- matrix(rep((gamma - 1) * s, each = length(s)), length(s))
        diag(E) <- diag(E) - gamma
        owner <- if (conduct == "moncom") {
            E <- -gamma * diag(length(s))
            diag(length(s))
        } else market$ownership
        retention <- 1 - tariff
        effective_margin <- (prices - costs / retention) / prices
        foc <- retention * s +
            t(owner * E) %*% (retention * s * effective_margin)
        expect_lt(max(abs(foc)), 1e-8)
        expect_equal(unname(calcShares(fit@model, TRUE, revenue = TRUE)),
                     s, tolerance = 1e-9)
        expect_equal(unname(fit@model@mcPre) * retention, costs,
                     tolerance = 1e-8)
        expect_equal(market$costs, costs)
        expect_equal((prices[5] - costs[5]) / prices[5], .25,
                     tolerance = 1e-9)
        expect_gt(gamma, 1)
    }
})

test_that("Stackelberg Logit inverts a leader game with multiproduct costs and tariffs", {
    s <- c(.15, .1, .2, .25, .3)
    q <- .8 * s
    c <- c(50, 60, 70, 80, 90)
    tariff <- c(.05, .05, .1, .1, .02)
    fit <- synthetic_market(supply = "stackelberg", n_firms = 2,
        n_products = 2, shares = s, costs = c, tariffPre = tariff,
        reference_margin = .3, passive_outside_share = .2, seed = 7)
    owner <- c("1", "1", "2", "2", "3")
    firm_share <- vapply(unique(owner), function(f) sum(q[owner == f]),
                         numeric(1))
    ## Firm 2 has the greatest aggregate share. Its followers are firms 1, 3.
    expect_identical(fit@model@leadersPre, "2")
    B <- sum(firm_share[c(1, 3)]^2 /
             (1 - firm_share[c(1, 3)] + firm_share[c(1, 3)]^2))
    h <- c(rep(1 / (1 - firm_share[1]), 2),
           rep(1 / (1 - firm_share[2] / (1 - B)), 2),
           1 / (1 - firm_share[3]))
    p_ref <- c[5] / .7
    alpha_abs <- h[5] / (p_ref - c[5] / (1 - tariff[5]))
    prices_oracle <- c / (1 - tariff) + h / alpha_abs
    expect_equal(unname(fit@model@pricePre), unname(prices_oracle),
                 tolerance = 1e-8)
    expect_equal(unname(fit@model@mcPre) * (1 - tariff), c,
                 tolerance = 1e-8)
    expect_equal(unname(calcShares(fit@model, TRUE)), q,
                 tolerance = 1e-9)
    expect_lt(coordination::stackelberg_residuals(fit@model)$maxNormalized,
              1e-8)
    expect_lt(fit@diagnostics$synthetic$foc_residual, 1e-8)
    expect_identical(fit@diagnostics$synthetic$solver_status$pre,
                     "converged")
})

test_that("Stackelberg leader default selects three largest firms above three", {
    s <- c(.12, .13, .1, .15, .5)
    c <- c(50, 60, 70, 80, 90)
    for (demand in c("logit", "ces")) {
        for (conduct in c("bertrand", "cournot")) {
            fit <- synthetic_market(demand = demand,
                supply = "stackelberg", stackelberg_conduct = conduct,
                n_firms = 4, shares = s, costs = c,
                reference_margin = .3, passive_outside_share = .2,
                tariffPre = c(.05, .1, .02, .08, .1), seed = 7)
            expect_identical(fit@model@leadersPre, c("5", "4", "2"))
            expect_equal(unname(fit@model@mcPre) *
                         (1 - c(.05, .1, .02, .08, .1)), c,
                         tolerance = 1e-7)
            expect_equal(unname(calcShares(fit@model, TRUE,
                revenue = demand == "ces")), .8 * s,
                tolerance = 1e-8)
            expect_lt(coordination::stackelberg_residuals(
                fit@model)$maxNormalized, 1e-7)
            expect_lt(fit@diagnostics$synthetic$share_residual, 1e-8)
            expect_lt(fit@diagnostics$synthetic$foc_residual, 1e-7)
            expect_true(all(is.finite(fit@model@pricePre) &
                            fit@model@pricePre > 0))
        }
    }
})

test_that("Stackelberg CES follower anchor identifies leader and follower prices", {
    s <- c(.2, .3, .5)
    q <- .8 * s
    c <- c(50, 60, 90)
    gamma <- 4
    h <- gamma - 1
    ref_margin <- 1 / (1 + h * (1 - q[3]))
    fit <- synthetic_market(demand = "ces", supply = "stackelberg",
        n_firms = 2, shares = s, costs = c,
        reference_margin = ref_margin, passive_outside_share = .2,
        leadersPre = "1", seed = 7)
    af <- h * q[c(2, 3)] /
        (1 - q[c(2, 3)] + h * (1 - q[c(2, 3)] + q[c(2, 3)]^2))
    phi <- 1 / (1 - sum(q[c(2, 3)] * af))
    margins_oracle <- c(1 / (gamma - h * phi * q[1]),
        1 / (1 + h * (1 - q[2])), ref_margin)
    expect_equal(fit@model@slopes$gamma, gamma, tolerance = 1e-8)
    expect_equal(unname(fit@model@pricePre),
                 unname(c / (1 - margins_oracle)), tolerance = 1e-8)
    expect_lt(coordination::stackelberg_residuals(fit@model)$maxNormalized,
              1e-8)
})

test_that("Stackelberg CES identifies curvature beside a search grid point", {
    ## A near-grid root previously counted as both an exact root and a sign
    ## change and was incorrectly rejected as underidentified.
    grid <- seq(log(1e-14), log(1e6 - 1), length.out = 301L)
    gamma <- 1 + exp(grid[190] + 1e-13)
    q <- .8 * c(.2, .3, .5)
    h <- gamma - 1
    af <- h * q[1:2] /
        (1 - q[1:2] + h * (1 - q[1:2] + q[1:2]^2))
    phi <- 1 / (1 - sum(q[1:2] * af))
    reference_margin <- 1 / (gamma - h * phi * q[3])
    fit <- synthetic_market(demand = "ces", supply = "stackelberg",
        n_firms = 2, shares = c(.2, .3, .5),
        costs = c(60, 70, 80), reference_margin = reference_margin,
        passive_outside_share = .2, leadersPre = "3", seed = 7)
    expect_equal(fit@model@slopes$gamma, gamma, tolerance = 1e-8)
    expect_lt(abs(fit@diagnostics$synthetic$curvature_residual), 1e-10)
})

test_that("Stackelberg reference margin changes demand scale at fixed observables", {
    base <- list(supply = "stackelberg", n_firms = 2,
        shares = c(.2, .3, .5), costs = c(50, 60, 90),
        passive_outside_share = .2, seed = 7)
    low <- do.call(synthetic_market, c(base, list(reference_margin = .2)))
    high <- do.call(synthetic_market, c(base, list(reference_margin = .4)))
    expect_gt(high@model@pricePre[3], low@model@pricePre[3])
    expect_lt(abs(high@model@slopes$alpha),
              abs(low@model@slopes$alpha))
    ces_low <- do.call(synthetic_market, c(base,
        list(demand = "ces", reference_margin = .2)))
    ces_high <- do.call(synthetic_market, c(base,
        list(demand = "ces", reference_margin = .4)))
    expect_gt(ces_high@model@pricePre[3], ces_low@model@pricePre[3])
    expect_lt(ces_high@model@slopes$gamma,
              ces_low@model@slopes$gamma)
})

test_that("Stackelberg observed mode rejects unclosed or invalid games", {
    base <- list(supply = "stackelberg", n_firms = 2,
        n_products = 2, shares = c(.15, .1, .2, .25, .3),
        costs = c(50, 60, 70, 80, 90),
        reference_margin = .3, passive_outside_share = .2, seed = 7)
    expect_error(do.call(synthetic_market, c(base,
        list(tariffPre = c(.05, .1, .1, .1, .02)))),
        "one baseline tariff rate per firm")
    expect_error(do.call(synthetic_market, c(base,
        list(leadersPre = "unknown"))), "distinct firm IDs")
    expect_error(do.call(synthetic_market, modifyList(base,
        list(passive_outside_share = 0))), "positive passive outside")
    expect_error(do.call(synthetic_market, c(base,
        list(tariffPre = rep(.4, 5)))), "margin must exceed")
})

test_that("trade rejects inadmissible policy and unidentified combinations", {
    expect_error(synthetic_market(tariffPre = rep(.4, 3),
        n_firms = 2, costs = c(50, 60, 70), reference_margin = .2,
        seed = 1), "margin must exceed")
    expect_error(synthetic_market(policy = "quota", n_firms = 2,
        costs = c(50, 60, 70), quotaPre = c(.5, 2, Inf), seed = 1),
        "non-binding quotas")
    expect_error(synthetic_market(demand = "aids", supply = "bertrand",
        seed = 1), "underidentified or unsupported")
    expect_error(synthetic_market(prices = c(1, 2, 3, 4), seed = 1),
                 "observed mode uses")
})

test_that("trade primitives mode still uses supplied prices and parameters", {
    fit <- synthetic_market(mode = "primitives", n_firms = 2,
        parameters = list(alpha = -.05), reference_price = 100, seed = 7)
    expect_equal(fit@diagnostics$route, "specify")
    expect_equal(unname(fit@model@slopes$alpha), -.05)
})
