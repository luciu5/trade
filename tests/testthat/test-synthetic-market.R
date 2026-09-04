test_that("trade synthetic tariff markets use the active reference product", {
    fit <- synthetic_market(
        demand = "logit", supply = "bertrand", policy = "tariff",
        n_firms = 2, n_products = 2, reference_price = 100,
        outside_margin = 20, seed = 7
    )
    market <- fit@diagnostics$synthetic_market

    expect_s4_class(fit, "TradeFit")
    expect_equal(fit@spec$policy, "tariff")
    expect_equal(unname(calcShares(fit@model, TRUE)), market$shares,
                 tolerance = 1e-10)
    expect_lt(fit@diagnostics$synthetic$foc_residual, 1e-8)
    expect_equal(unname(fit@model@pricePre), market$prices, tolerance = 1e-10)
    expect_equal(fit@model@tariffPre, rep(0, 5))

    result <- simulate(fit, counterfactual(tariff = rep(.1, 5)))
    expect_s4_class(result, "TariffLogit")
})

test_that("trade synthetic quota markets retain the quota baseline", {
    fit <- synthetic_market(
        demand = "logit", supply = "bertrand", policy = "quota",
        n_firms = 2, n_products = 2, reference_price = 100,
        outside_margin = 20, quotaPre = c(2, 2, 2, 2, Inf), seed = 7
    )

    expect_s4_class(fit, "TradeFit")
    expect_equal(fit@spec$policy, "quota")
    expect_s4_class(fit@model, "QuotaLogit")
    expect_equal(fit@model@quotaPre, c(2, 2, 2, 2, Inf))
    expect_equal(unname(calcShares(fit@model, TRUE)),
                 fit@diagnostics$synthetic_market$shares, tolerance = 1e-10)
    expect_lt(fit@diagnostics$synthetic$foc_residual, 1e-8)
})

test_that("trade rejects simultaneous or mismatched pre-policies", {
    expect_error(
        synthetic_market(n_firms = 2, tariffPre = rep(0, 3),
                         quotaPre = rep(Inf, 3)),
        "simultaneous tariff and quota"
    )
    expect_error(
        synthetic_market(n_firms = 2, policy = "tariff",
                         quotaPre = rep(Inf, 3)),
        "quotaPre.*simultaneous tariff and quota"
    )
    expect_error(
        synthetic_market(n_firms = 2, policy = "quota",
                         tariffPre = rep(0, 3)),
        "tariffPre.*simultaneous tariff and quota"
    )
    expect_error(
        synthetic_market(n_firms = 2, policy = "quota",
                         quotaPre = c(.9, 1, Inf)),
        "non-binding quotas"
    )
})

test_that("trade primitives mode remains an explicit parameter path", {
    fit <- synthetic_market(
        demand = "logit", supply = "bertrand", policy = "tariff",
        mode = "primitives", n_firms = 2, parameters = list(alpha = -.05),
        reference_price = 100, seed = 7
    )

    expect_s4_class(fit, "TradeFit")
    expect_equal(fit@diagnostics$route, "specify")
    expect_equal(unname(fit@parameters$slopes$alpha), -.05)
    expect_equal(unname(fit@diagnostics$synthetic_truth$alpha), -.05)
    expect_equal(fit@diagnostics$synthetic_parameter_error$alpha, 0,
                 tolerance = 1e-12)
})
