trade_cf_data <- function() {
    list(
        prices = c(.0441, .0328, .0409, .0396, .0387, .0497),
        quantities = c(.066, .172, .253, .187, .099, .223) * 100,
        margins = c(.3830, .5515, .5421, .5557, .4453, .3769),
        owner = c("BUD", "OLD STYLE", "MILLER", "MILLER", "OTHER-LITE", "OTHER-REG")
    )
}

test_that("trade Counterfactual preserves legacy tariff simulations", {
    x <- trade_cf_data()
    fit <- calibrate("logit", "bertrand", prices = x$prices,
                     quantities = x$quantities, margins = x$margins,
                     owner = x$owner)
    tariff <- rep(.01, length(x$prices))
    old <- simulate(fit, tariffPost = tariff)
    cf <- counterfactual(tariff = tariff)
    new <- simulate(fit, cf)
    expect_equal(new@pricePost, old@pricePost, tolerance = 1e-8)
    expect_equal(attr(new, "counterfactual")$fields, cf)
    expect_equal(unname(fit@model@pricePre), unname(x$prices), tolerance = 1e-12)
})

test_that("an empty trade Counterfactual preserves the baseline prices", {
    x <- trade_cf_data()
    fit <- calibrate("logit", "bertrand", prices = x$prices,
                     quantities = x$quantities, margins = x$margins,
                     owner = x$owner)
    result <- simulate(fit, counterfactual())
    expect_equal(unname(result@pricePost), unname(fit@model@pricePre),
                 tolerance = 1e-8)
})

test_that("trade Counterfactual supports exit and validates capabilities", {
    x <- trade_cf_data()
    fit <- calibrate("logit", "bertrand", prices = x$prices,
                     quantities = x$quantities, margins = x$margins,
                     owner = x$owner)
    result <- simulate(fit, counterfactual(exit = 6))
    logical_result <- simulate(fit, counterfactual(
        exit = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)
    ))
    expect_true(inherits(result, "TariffLogit"))
    expect_true(inherits(logical_result, "TariffLogit"))
    expect_error(simulate(fit, counterfactual(quota = rep(1, 6))),
                 "does not support")
    expect_error(simulate(fit, counterfactual(tariff = rep(.01, 6)),
                          subset = rep(TRUE, 6)), "cannot combine")
})
